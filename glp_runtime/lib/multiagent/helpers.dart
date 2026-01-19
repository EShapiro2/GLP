/// Helper Routines for irmaGLP Transactions
/// 
/// Implements abandon, request, export, and reactivate helpers
/// as specified in irmaGLP-spec.md v1.1 Section 4.
/// 
/// CRITICAL CORRECTIONS:
/// - abandon() takes READER as parameter (not variable)
/// - Only readers can be abandoned
/// - export() creates relay with relay/2 goal (per paper)
library;

import 'dart:convert';
import 'dart:typed_data';

import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/variable_table.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/payload_serializer.dart';

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
    final entry = vp.lookup(readerId);
    if (entry == null) {
      // Variable not in table, nothing to do
      return;
    }
    
    // Compute paired writer ID (reader Y? → writer Y)
    // In implementation: same varId, just different isReader flag
    final writerId = readerId; // Paired writer has same ID
    
    if (entry.role == VariableRole.importedReader && entry.creator != vp.agentId) {
      // Imported variable - notify creator
      // Send WRITER in abandon message
      final payload = _serializeAbandonMessage(writerId);
      mp.add(OutboundMessage(
        destination: entry.creator,
        type: MessageType.abandon,
        payload: payload,
      ));
      vp.remove(readerId);
    } 
    else if (entry.role == VariableRole.createdReader && 
             entry.creator == vp.agentId && 
             entry.state != null) {
      // Created reader with requester - notify requester
      // Send WRITER in abandon message
      final requester = entry.state as String;
      final payload = _serializeAbandonMessage(writerId);
      mp.add(OutboundMessage(
        destination: requester,
        type: MessageType.abandon,
        payload: payload,
      ));
      vp.remove(readerId);
    } 
    else {
      // Local abandonment only - just remove from table
      vp.remove(readerId);
    }
  }
  
  /// request(readerId) for agent p
  /// 
  /// Specification: irmaGLP-spec.md Section 4.2
  /// 
  /// Send read request for imported reader that hasn't been requested yet.
  /// Idempotent: request sent only once (state changes from null to creator).
  void request(
    int readerId,
    String agentId,
    VariableTable vp,
    MessageQueue mp,
  ) {
    final entry = vp.lookup(readerId);
    if (entry == null) {
      // Variable not in table
      return;
    }
    
    if (entry.role == VariableRole.importedReader && 
        entry.creator != agentId && 
        entry.state == null) {
      // Reader imported but not yet requested
      // Update state to mark request sent
      vp.updateState(readerId, entry.creator);
      
      // Queue read request message
      final payload = _serializeReadRequest(readerId, agentId);
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
  /// Specification: irmaGLP-spec.md Section 4.4
  /// 
  /// Update variable table when term is sent outside agent p.
  /// Creates relay variables for requested readers being re-exported.
  /// 
  /// Returns modified term (with relay variables substituted if needed).
  ExportResult export(
    Term term,
    String agentId,
    VariableTable vp,
    List<GoalRef> relayGoals, // Output: forwarding goals to add
    List<int> Function(int, int) allocateFreshPair, // Callback to allocate (writer, reader) pair
  ) {
    final modifiedTerm = _exportTermRecursive(
      term, 
      agentId, 
      vp, 
      relayGoals,
      allocateFreshPair,
    );
    
    return ExportResult(modifiedTerm, relayGoals);
  }
  
  Term _exportTermRecursive(
    Term term,
    String agentId,
    VariableTable vp,
    List<GoalRef> relayGoals,
    List<int> Function(int, int) allocateFreshPair,
  ) {
    if (term is ConstTerm) {
      return term;
    } else if (term is VarRef) {
      final varId = term.varId;
      final creator = _getCreator(varId, agentId, vp);
      
      if (creator == agentId && !vp.contains(varId)) {
        // Local variable being exported for first time
        final role = term.isReader ? VariableRole.createdReader : VariableRole.createdWriter;
        vp.add(varId, VariableEntry(
          varId: varId,
          creator: agentId,
          role: role,
        ));
        return term;
      } 
      else if (creator != agentId) {
        // Non-local variable
        final entry = vp.lookup(varId);
        
        if (entry == null || entry.state == null) {
          // Writer or non-requested reader - just remove
          vp.remove(varId);
          return term;
        } 
        else if (entry.role == VariableRole.importedReader && 
                 entry.state == entry.creator) {
          // Requested reader - needs relay
          // Create fresh pair (Z, Z?)
          final pair = allocateFreshPair(0, 0); // Callback allocates and returns IDs
          final relayWriter = pair[0];
          final relayReader = pair[1];
          
          // Replace Y? with Z? in term
          final replacedTerm = VarRef(relayReader, isReader: true);
          
          // Add forwarding goal: relay(Y, Z)
          // This will be: relay(originalWriter, relayWriter)
          final forwardGoal = _createRelayGoal(varId, relayWriter);
          relayGoals.add(forwardGoal);
          
          // Add relay reader to V_p
          vp.add(relayReader, VariableEntry(
            varId: relayReader,
            creator: agentId,
            role: VariableRole.createdReader,
          ));
          
          return replacedTerm;
        }
      }
      
      return term;
    } else if (term is StructTerm) {
      // Recursively export args
      final exportedArgs = term.args.map((arg) => 
        _exportTermRecursive(arg, agentId, vp, relayGoals, allocateFreshPair)
      ).toList();
      
      return StructTerm(term.functor, exportedArgs);
    }
    
    throw UnsupportedError('Cannot export term type: ${term.runtimeType}');
  }
  
  /// Get creator of a variable
  /// 
  /// If in V_p, use creator field. Otherwise assume created by current agent.
  String _getCreator(int varId, String agentId, VariableTable vp) {
    final entry = vp.lookup(varId);
    return entry?.creator ?? agentId;
  }
  
  /// Create relay goal
  /// 
  /// Returns GoalRef for: relay(originalWriter, relayWriter)
  /// This will be looked up as relay/2 in the program.
  GoalRef _createRelayGoal(int originalWriter, int relayWriter) {
    // For now, return a placeholder GoalRef
    // The actual PC will be resolved when the goal is enqueued
    // The runtime will need to look up relay/2 predicate
    return GoalRef(0, 0); // PC will be set by runtime
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
  
  /// Forwarding goals to add to active queue
  final List<GoalRef> relayGoals;
  
  ExportResult(this.term, this.relayGoals);
}
