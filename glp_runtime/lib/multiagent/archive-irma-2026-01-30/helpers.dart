/// Helper Routines for irmaGLP Transactions
/// 
/// Implements abandon, request, export, and reactivate helpers
/// as specified in irmaGLP-spec.md Section 4.
/// 
/// Implementation notes:
/// - abandon() takes READER as parameter (not variable)
/// - Only readers can be abandoned
/// - export() creates relay via RelaySetup (callback-based)
///   Implements: export_reader(Y?, Z) :- Z = Y?.
library;

import 'dart:convert';
import 'dart:typed_data';

import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart'; // For GoalRef in reactivate
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

/// Information needed to set up a relay forwarding callback
class RelaySetup {
  /// The original reader (Y?) that we're waiting on
  final int originalReaderId;
  
  /// The relay writer (Z) that should be bound when Y? receives a value
  final int relayWriterId;
  
  /// The relay reader (Z?) that was exported to the recipient
  final int relayReaderId;
  
  RelaySetup({
    required this.originalReaderId,
    required this.relayWriterId,
    required this.relayReaderId,
  });
}

/// Helper routines for irmaGLP transactions
class IrmaHelpers {
  final String agentId;
  late final PayloadSerializer _serializer;
  
  IrmaHelpers(this.agentId) {
    _serializer = PayloadSerializer(agentId);
  }
  
  /// abandon(readerId) for agent p
  /// 
  /// Specification: irmaGLP-spec.md Section 4.1
  /// 
  /// CRITICAL: An agent can only abandon a READER, which causes its 
  /// dual writer to be abandoned at the remote agent.
  /// 
  /// When reader Y? becomes unreachable at agent p, this notifies other
  /// agents so they can clean up the paired writer Y.
  void abandon(
    int readerId,
    VariableTable vp,
    MessageQueue mp,
  ) {
    final readerKey = VarKey(readerId, true); // reader
    final entry = vp.lookup(readerKey);
    if (entry == null) {
      // Variable not in table, nothing to do
      return;
    }

    if (entry.role == VariableRole.importedReader && entry.creator != vp.agentId) {
      // Imported reader - notify creator using THEIR local ID
      // The creator needs to receive their creatorLocalId, not our local readerId
      final creatorWriterId = entry.creatorLocalId ?? readerId;
      final payload = _serializeAbandonMessage(creatorWriterId);
      mp.add(OutboundMessage(
        destination: entry.creator,
        type: MessageType.abandon,
        payload: payload,
      ));
      vp.remove(readerKey);
    }
    else if (entry.role == VariableRole.createdReader &&
             entry.creator == vp.agentId &&
             entry.requester != null) {
      // Created reader with requester - notify requester
      // We are the creator, so send our local readerId (which is the paired writer ID)
      final payload = _serializeAbandonMessage(readerId);
      mp.add(OutboundMessage(
        destination: entry.requester!,
        type: MessageType.abandon,
        payload: payload,
      ));
      vp.remove(readerKey);
    }
    else {
      // Local abandonment only - just remove from table
      vp.remove(readerKey);
    }
  }
  
  /// request(readerId) for agent p
  /// 
  /// Specification: irmaGLP-spec.md Section 4.2
  /// 
  /// Send read request for imported reader that hasn't been requested yet.
  /// Idempotent: request sent only once (state changes from null to creator).
  /// 
  /// CRITICAL: Uses creator's local ID (creatorLocalId) in the global ID format,
  /// not our local readerId. This ensures the creator can look it up in their V_p.
  void request(
    int readerId,
    String agentId,
    VariableTable vp,
    MessageQueue mp,
  ) {
    final readerKey = VarKey(readerId, true); // reader
    final entry = vp.lookup(readerKey);
    if (entry == null) {
      // Variable not in table
      return;
    }
    
    if (entry.role == VariableRole.importedReader &&
        entry.creator != agentId &&
        !entry.requestSent) {
      // Reader imported but not yet requested
      // Mark request sent
      vp.markRequestSent(readerKey);
      
      // Queue read request message using creator's ID namespace
      // This ensures the creator can look up the variable in their V_p
      final creatorSerializer = PayloadSerializer(entry.creator);
      final payload = creatorSerializer.createReadRequestPayload(
        entry.creatorLocalId,  // Use creator's original ID, not our local ID
        agentId,
      );
      mp.add(OutboundMessage(
        destination: entry.creator,
        type: MessageType.readRequest,
        payload: payload,
      ));
    }
    // If state != null, request already sent - idempotent, no action
  }
  
  /// reactivate(readerId, suspendedSet) for agent p
  /// 
  /// Specification: irmaGLP-spec.md Section 4.3
  /// 
  /// Find and reactivate all goals suspended on reader readerId.
  /// Returns set of goals to append to active queue.
  Set<GoalRef> reactivate(
    int readerId,
    Map<GoalRef, Set<int>> suspendedSet,
  ) {
    final reactivated = <GoalRef>{};
    
    // Find all goals blocked on this reader
    final toRemove = <GoalRef>[];
    for (final entry in suspendedSet.entries) {
      final goal = entry.key;
      final blockers = entry.value;
      
      if (blockers.contains(readerId)) {
        reactivated.add(goal);
        toRemove.add(goal);
      }
    }
    
    // Remove from suspended set
    for (final goal in toRemove) {
      suspendedSet.remove(goal);
    }
    
    return reactivated;
  }
  
  /// export(term, agentId, vp, activeQueue) for agent p
  ///
  /// Specification: irmaGLP-spec.md Section 4.3
  ///
  /// Update variable table when term is sent outside agent p.
  /// Creates relay variables for requested readers being re-exported.
  ///
  /// Returns modified term (with relay variables substituted if needed),
  /// relay setup info for establishing forwarding callbacks, and
  /// list of newly exported writers that need heap callbacks.
  ///
  /// [isReader] - Callback to check if address is a reader (use heap.isReader)
  ExportResult export(
    Term term,
    String agentId,
    VariableTable vp,
    List<RelaySetup> relaySetups, // Output: relay forwarding setups needed
    List<int> Function() allocateFreshPair, // Callback to allocate (writer, reader) pair
    bool Function(int addr) isReader, // Callback to check if addr is reader (from heap)
  ) {
    final newlyExportedWriters = <int>[];
    final modifiedTerm = _exportTermRecursive(
      term,
      agentId,
      vp,
      relaySetups,
      allocateFreshPair,
      isReader,
      newlyExportedWriters,
    );

    return ExportResult(modifiedTerm, relaySetups, newlyExportedWriters);
  }

  Term _exportTermRecursive(
    Term term,
    String agentId,
    VariableTable vp,
    List<RelaySetup> relaySetups,
    List<int> Function() allocateFreshPair,
    bool Function(int addr) isReader,
    List<int> newlyExportedWriters,
  ) {
    if (term is ConstTerm) {
      return term;
    } else if (term is VarRef) {
      // Per irmaGLP-spec.md Section 3.2.1: use raw addr, check isReader via heap
      final addr = term.addr;
      final isReaderVar = isReader(addr);
      final varKey = VarKey(addr, isReaderVar);
      final creator = _getCreator(addr, isReaderVar, agentId, vp);

      if (creator == agentId && !vp.contains(varKey)) {
        // Local variable being exported for first time
        final role = isReaderVar ? VariableRole.createdReader : VariableRole.createdWriter;
        vp.add(varKey, VariableEntry(
          varId: addr,
          isReader: isReaderVar,
          creator: agentId,
          role: role,
        ));
        // Track newly exported writers for callback registration
        // Per spec Section 5.2: when created writers are bound, send assignments
        if (!isReaderVar) {
          newlyExportedWriters.add(addr);
        }
        return term;
      }
      else if (creator != agentId) {
        // Non-local variable
        final entry = vp.lookup(varKey);

        if (entry == null || !entry.requestSent) {
          // Writer or non-requested reader - just remove
          vp.remove(varKey);
          return term;
        }
        else if (entry.role == VariableRole.importedReader &&
                 entry.requestSent) {
          // Requested reader - needs relay
          // Per spec Section 4.3: create fresh pair (Z, Z?), replace Y? with Z?
          // in exported term, add export_reader(Y?, Z) forwarding.
          //
          // Implementation: We use heap callbacks instead of GLP goals.
          // When Y? is bound, Z should be bound to the same value.

          // Callback allocates and returns [writerAddr, readerAddr]
          final pair = allocateFreshPair();
          final relayWriter = pair[0];  // Writer address
          final relayReader = pair[1];  // Reader address

          // Replace Y? with Z? in term - relayReader is the reader address
          final replacedTerm = VarRef(relayReader);

          // Record relay setup for callback registration
          // This implements: export_reader(Y?, Z) :- Z = Y?.
          relaySetups.add(RelaySetup(
            originalReaderId: addr,
            relayWriterId: relayWriter,
            relayReaderId: relayReader,
          ));

          // Add relay reader Z? to V_p as created reader
          final relayReaderKey = VarKey(relayReader, true);
          vp.add(relayReaderKey, VariableEntry(
            varId: relayReader,
            isReader: true,
            creator: agentId,
            role: VariableRole.createdReader,
          ));

          // Also register relay writer Z in V_p
          final relayWriterKey = VarKey(relayWriter, false);
          vp.add(relayWriterKey, VariableEntry(
            varId: relayWriter,
            isReader: false,
            creator: agentId,
            role: VariableRole.createdWriter,
          ));

          return replacedTerm;
        }
      }

      return term;
    } else if (term is StructTerm) {
      // Recursively export args
      final exportedArgs = term.args.map((arg) =>
        _exportTermRecursive(arg, agentId, vp, relaySetups, allocateFreshPair, isReader, newlyExportedWriters)
      ).toList();

      return StructTerm(term.functor, exportedArgs);
    }

    throw UnsupportedError('Cannot export term type: ${term.runtimeType}');
  }
  
  /// Get creator of a variable
  /// 
  /// If in V_p, use creator field. Otherwise assume created by current agent.
  String _getCreator(int varId, bool isReader, String agentId, VariableTable vp) {
    final key = VarKey(varId, isReader);
    final entry = vp.lookup(key);
    return entry?.creator ?? agentId;
  }
  
  // Serialization helpers for messages
  
  List<int> _serializeAbandonMessage(int writerId) {
    // Serialize writer ID
    final data = ByteData(4);
    data.setInt32(0, writerId, Endian.big);
    return data.buffer.asUint8List();
  }
  
  List<int> _serializeReadRequest(int readerId, String requester) {
    // Serialize reader ID + requester agent ID
    final builder = BytesBuilder();
    
    // Reader ID
    final data = ByteData(4);
    data.setInt32(0, readerId, Endian.big);
    builder.add(data.buffer.asUint8List());
    
    // Requester
    final requesterBytes = utf8.encode(requester);
    builder.addByte(requesterBytes.length);
    builder.add(requesterBytes);
    
    return builder.toBytes();
  }
}

/// Result of export operation
class ExportResult {
  /// Modified term (with relay variables if needed)
  final Term term;

  /// Relay setups for establishing forwarding callbacks
  ///
  /// Each entry specifies: when originalReaderId is bound,
  /// bind relayWriterId to the same value.
  /// This implements: export_reader(Y?, Z) :- Z = Y?.
  final List<RelaySetup> relaySetups;

  /// Writers that were newly exported (first time added to V_p)
  ///
  /// These writers need heap callbacks registered so that when they are
  /// bound locally, assignment messages can be sent to requesters.
  /// Per spec Section 5.2 Case 1: assignments are sent when created
  /// writers are bound and have requesters.
  final List<int> newlyExportedWriters;

  ExportResult(this.term, this.relaySetups, this.newlyExportedWriters);
}
