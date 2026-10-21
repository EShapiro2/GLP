/// madGLP Agent Context
///
/// Provides agent-level context for multiagent GLP communication.
/// Each agent has W_p (global writers table) and M_p (message queue).
///
/// Specification: IGLP app:madglp-spec (madGLP Specification).
library;

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/body_kernels.dart' show deepDeref;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/machine_state.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/global_send.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/imported_writer_records.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/wire/codec.dart'
    show wireMsgKindValue, wireMsgKindRequest, wireMsgKindAcknowledgement;
import 'package:glp_runtime/wire/artefact.dart' show ModuleRefusal;
import 'package:glp_runtime/wire/payload_codec.dart' show PayloadCodec;

/// Callback for delivering messages to other agents
typedef MessageDeliveryCallback = void Function(String destination, OutboundMessage message);

/// madGLP Agent Context
///
/// Wraps a GlpRuntime with W_p and M_p for multiagent operation.
/// Each agent has its own MadContext with unique agentId.
class MadContext {
  /// Unique identifier for this agent (e.g., "alice", "bob")
  final String agentId;

  /// Underlying GLP runtime
  final GlpRuntime runtime;

  /// Global writers table W_p: tracks writers awaiting incoming assignments
  final GlobalWritersTable wp;

  /// Imported-writer records U_p (madGLP Local State `(R_p, W_p, U_p, M_p)`):
  /// marks each writer localized from a writer global name as the sending end
  /// of that link, so re-exporting it forwards the original name instead of
  /// anchoring a new link here (Definition Globalize, case 4).
  final ImportedWriterRecords up;

  /// Message queue M_p: outbound messages to other agents
  final MessageQueue mp;

  /// Registry for pending global_send goals (watches readers, sends when known)
  final GlobalSendRegistry globalSendRegistry;

  /// Optional callback for message delivery (set by coordinator)
  MessageDeliveryCallback? onMessageReady;

  /// The agent's networking layer (set at boot). Backs the seam predicates
  /// (IGLP Definition "Seam Predicates"); `sign/3` and `signature/2` sign and
  /// verify under the runtime's person identity, not through the layer.
  GlpNetwork? network;

  /// Called after a place event the layer reports is delivered onto its
  /// place's stream ([deliverPlaceEvent]), so that the host runs: a place
  /// event is an event, and "On each event the agent binds what the event
  /// delivers, reduces until quiescent ... and then performs its Sends"
  /// (IGLP, Implementation Notes, "Event-driven execution").  Set by a host
  /// over a given network; the simulation reports no place events.
  void Function()? onPlaceEventDelivered;

  /// The canonical bytes e(T) of a ground term T, which a signature signs and
  /// a verifier checks (IGLP code-format-fragment.tex, "Signed and Hashed
  /// Content"). It throws on a variable, and the bytes are agent-independent,
  /// so they match across agents.
  List<int> canonicalSerialize(Term groundTerm) =>
      PayloadCodec.serializeAgentMessage(groundTerm);

  /// Optional trace sink for MAD infrastructure output.
  /// When set, MAD debug output goes through this callback instead of print().
  void Function(String)? traceSink;

  /// Hold table for early `_r(p, i)` assignments (IGLP app:in-networking,
  /// Early messages). An assignment `_r(p, i) := T` arriving before its
  /// `LocalizeEntry` exists — possible under any non-FIFO transport — is stored
  /// here keyed by (remoteAgent, remoteIndex) and delivered when `localize()`
  /// creates the matching entry. Only the `_r` case needs holding: `_w(p, i)`
  /// entries exist before the global name leaves the agent, and the serializer
  /// entry at index 0 is permanent.
  final Map<(String, int), ({Term value, String fromAgent})>
      _heldReaderAssignments = {};

  /// Pending reader-name values (spec §app:requests-acks): a sent value
  /// `_r(p, i) := T↑` is retained here, keyed by (p, i), until its
  /// acknowledgement arrives. An authorised request for the link addresses the
  /// pending value to the requester as well; the acknowledgement deletes it,
  /// closing the link.
  /// Writer-name and serializer values are fire-and-forget and never pend.
  ///
  /// nestedReaderNames are the reader names anchored at this agent that the
  /// value carries (allocated when it was globalized): on the acknowledgement,
  /// the acker is recorded as the holder of each of their links (madGLP
  /// Receive, Acknowledgement case) — the acker localized them.
  final Map<(String, int),
          ({
        List<int> payload,
        String destination,
        List<GlobalName> nestedReaderNames
      })> _pendingReaderValues = {};

  /// Reader names this agent forwarded on (Definition Globalize, forwarding
  /// case). After forwarding, the no-entry condition on an arriving value for
  /// the name means stale, not early: such arrivals are dropped, never held.
  /// Indices are never reused, so the record never misclassifies.
  final Set<(String, int)> _forwardedReaderNames = {};

  /// Reader names whose value this agent applied, removing the entry (madGLP
  /// Receive, Reader case: "The entry matching (p, i) is removed").  A value
  /// arriving for such a name afterwards is a repeat, and is dropped: "a value
  /// finds its entry removed" (Remark Fair Message Delivery), "A value ...
  /// matching no entry and no pending value is dropped" (§app:requests-acks).
  /// It is not early — an early assignment is one arriving "before the message
  /// whose localization creates its global writers table entry" (IGLP
  /// app:in-networking, Early messages) — and is not held.  Indices are never
  /// reused, so the record never misclassifies.
  final Set<(String, int)> _appliedReaderNames = {};

  /// Held incoming assignments (Definition Held Link, case 4): an assignment
  /// arriving at the anchor from an agent it did not export the name to is
  /// "retained keyed by its global name" and not applied, until
  /// `authorise_link/2` authorises it (then applied) or refuses (discarded).
  ///
  /// Distinct from [_heldReaderAssignments], which retains an assignment that
  /// arrived *early*, before its entry existed. That one is applied as soon as
  /// localization creates the entry and needs no answer; this one waits on the
  /// program. The paper points at the early hold only as the model for how a
  /// retained assignment is keyed.
  final Map<(String, int, bool), ({Term value, String fromAgent})>
      _heldAssignments = {};

  /// Held incoming requests (Definition Held Link, case 3): every request
  /// reaching the anchor is held — the agent the anchor exported the name to
  /// sends none, so a request always means the name was forwarded. The
  /// requesters are retained so authorisation can record each among the
  /// link's holders.  Per link, in order of arrival: the first is the one
  /// reported, and "a request from a further requester --- the reader
  /// forwarded again --- is held and reported in its turn, the anchor's
  /// program asked once for each party that comes to hold the end" (§Held
  /// Links), its turn coming when the program has answered the one before.
  final Map<(String, int), List<String>> _heldRequests = {};

  /// The (link, requester) pairs the anchor has recorded, by link: a requester
  /// is recorded when its request is held.  "A request repeating a (link,
  /// requester) pair already recorded --- the link held, authorised or closed
  /// --- is dropped" (§app:requests-acks).  A link's pairs are released when
  /// the link closes, after which every request for it is dropped as one "for
  /// a closed link" (madGLP Receive, Request case).
  final Map<(String, int), Set<String>> _requestPairs = {};

  /// Links reported and not yet answered, newest report per link. Keyed as
  /// `(anchor, index, isWriter)`; the value is the agent the name or traffic
  /// came from — the `S` of the `pending_link(G, S)` report.
  final Map<(String, int, bool), String> _reportedLinks = {};

  /// Send MAD trace output to traceSink if set, otherwise silent.
  void _trace(String msg) {
    if (traceSink != null) {
      traceSink!(msg);
    }
  }

  MadContext({
    required this.agentId,
    required this.runtime,
  })  : wp = GlobalWritersTable(agentId),
        up = ImportedWriterRecords(agentId),
        mp = MessageQueue(),
        globalSendRegistry = GlobalSendRegistry(agentId);

  // =========================================================================
  // Writer Binding Observation
  // =========================================================================

  /// Called when a writer is bound to a value
  ///
  /// Checks for global_send goals watching this writer's reader and fires
  /// them if found (IGLP Definition global_send Predicate).
  void onWriterBound(HeapCell writerId, Term value) {
    _trace('[MAD $agentId] onWriterBound: writerId=$writerId, value=$value');
    _fireGlobalSendGoalIfExists(writerId, value);
    _flushReports();
  }

  // =========================================================================
  // Message Queue Operations (Send Transaction)
  // =========================================================================

  /// Flush all queued messages via the onMessageReady callback
  ///
  /// Returns the number of messages flushed.
  int flushMessages() {
    if (onMessageReady == null) return 0;

    int count = 0;
    final destinations = List<String>.from(mp.destinations);
    _trace('[MAD $agentId] flushMessages: mp.totalLength=${mp.totalLength}, destinations=$destinations');

    for (final dest in destinations) {
      while (true) {
        final msg = mp.poll(dest);
        if (msg == null) break;
        _trace('[MAD $agentId] flushMessages: sending ${msg.type} to $dest');
        onMessageReady!(dest, msg);
        count++;
      }
    }
    _trace('[MAD $agentId] flushMessages: flushed $count messages');
    return count;
  }

  // =========================================================================
  // madGLP global_send Mechanism (Phase 3)
  // =========================================================================

  /// Fire global_send goal if one is watching this writer's reader
  ///
  /// When a writer is bound, its paired reader becomes "known". If there's
  /// a global_send goal watching that reader, fire it now.
  void _fireGlobalSendGoalIfExists(HeapCell writerAddr, Term bound) {
    // The value sent is the writer's value with every assignment made by then applied, a reader whose writer is assigned standing for its value, as '_send' sends it (body_kernels.dart, sendKernel).  cGLP Reduce communicates the restriction of $(\hat\sigma?)^\star$, the pair completion applied to a fixed point, to the readers of the goal's writers, so a goal writer's value carries the head writers' values (GLP-Spec glp.tex and IGLP concurrent-glp.tex, Definition "cGLP Transition System"), and the value of a Send is $T^\uparrow$ (IGLP madglp-spec.tex, Definition "madGLP Send Transaction").  Until 2026-10-21 the callback globalized such a reader as a variable of T, and p(X, f(X?)) :- ground(X?) | q(X?) with the goal p(a, Y) sent f(_r(alice, 3)) on Y's link and then a on a link of its own (ground_reader_at_reduction_test.dart).
    final value = deepDeref(runtime, bound);
    // Check if there's a global_send goal watching this writer's reader
    final result = globalSendRegistry.onWriterBound(
      writerAddr: writerAddr,
      value: value,
      table: wp,
      records: up,
      extractVariables: (val) {
        final vars = <TermVar>[];
        if (val is Term) {
          _extractTermVars(val, vars);
        }
        return vars;
      },
    );

    if (result == null) {
      return;
    }

    _trace('[MAD $agentId] global_send FIRED: ${result.globalName} -> ${result.destination}');

    // Definition Held Link, case 2: at the holder of a forwarded writer, the
    // value its global_send goal produces. The record carries the agent the
    // name came from, because the hold happens here, at binding, when the
    // message that brought the name is long gone.
    //
    // The record is removed only when the value is actually sent — which, when
    // held, is after authorisation — or when refusal releases it. Refusal needs
    // the record, so it must outlive the hold.
    final record = up.lookup(writerAddr);
    final holdValue = record != null && record.isForwarded;
    if (!holdValue && record != null) {
      up.remove(writerAddr);
      _trace('[MAD $agentId] imported-writer record for $writerAddr released '
          '— value sent');
    }

    // Record names forwarded by this globalization: arrivals for them are
    // stale from now on (Definition Globalize, case 3).
    _recordForwardedNames(result.globalizeResult);

    // Imported writers occurring in the value were forwarded under their
    // original names (Definition Globalize, case 4): drop their goals too.
    _dropForwardedWriterGoals(result.globalizeResult);

    // Globalize the value term: replace local VarRefs with GlobalNames
    // This is needed so the receiver can localize nested global names
    final globalizedValue = globalizeTermWithResult(
      result.value as Term,
      result.extractedVariables,
      result.globalizeResult,
    );

    // Queue the assignment message
    final payload =
        PayloadCodec.createGlobalSendPayload(result.globalName, globalizedValue);

    mp.add(OutboundMessage(
      destination: result.destination,
      type: MessageType.assignment,
      payload: payload,
      link: (
        result.globalName.agent,
        result.globalName.index,
        result.globalName.isWriter
      ),
      held: holdValue,
    ));
    if (holdValue) {
      _reportPendingLink(result.globalName, record!.sender);
    }

    // The requesters recorded among the link's holders before the value was
    // produced each receive it as well: "p cannot tell the current holder
    // from the past one: it sends the value to each, a past holder discards
    // it, and the current holder acknowledges" (§app:requests-acks).  Only a
    // reader-name link at its anchor has requesters.
    for (final requester in result.requesters) {
      if (requester == result.destination) continue;
      mp.add(OutboundMessage(
        destination: requester,
        type: MessageType.assignment,
        payload: payload,
      ));
      _trace('[MAD $agentId] value ${result.globalName} sent to requester '
          '$requester as well');
    }

    // A reader-name value pends until acknowledged (§app:requests-acks): a
    // request addresses it to the requester as well, the acknowledgement
    // releases it. Writer-name values are fire-and-forget — they always find
    // their fixed anchor entry.
    // The spawns' names are the value's self-anchored reader links, recorded
    // so the acknowledgement can name their holder.
    if (result.globalName.isReader && result.globalName.index > 0) {
      _pendingReaderValues[
              (result.globalName.agent, result.globalName.index)] =
          (
        payload: payload,
        destination: result.destination,
        nestedReaderNames: result.globalizeResult.spawns
            .map((s) => s.globalName)
            .toList(),
      );
      _trace('[MAD $agentId] value ${result.globalName} pending until ack');
    }

    // Register any new goals spawned for nested variables
    // Must set up both registry entry AND onBind callback (same as registerGlobalSendSpawns)
    for (final newGoal in result.newGoals) {
      _trace('[MAD $agentId] global_send spawned new goal: ${newGoal.globalName}');
      globalSendRegistry.register(newGoal);
      runtime.heap.onBind(newGoal.readerAddr, (Term value) {
        onWriterBound(newGoal.readerAddr, value);
      });
    }
  }

  /// Extract TermVars from a term for globalization
  ///
  /// Each TermVar carries both the writer and reader addresses of its pair,
  /// looked up via the heap's cross-pointers.  The variables are met in the
  /// order the recursion this replaces met them, depth first and left to
  /// right, which is the order Globalize allocates their indices in
  /// (Definition Globalize: "For each variable Y occurring in T ... allocate
  /// the next index i"); the walk keeps a stack of its own, as until
  /// 2026-10-02 it recursed once a structure argument, and a cold call
  /// carrying a long list overflowed the Dart stack.
  void _extractTermVars(Term term, List<TermVar> result) {
    final pending = <Term>[term];
    while (pending.isNotEmpty) {
      final t = pending.removeLast();
      if (t is VarRef) {
        final isReader = runtime.heap.isReader(t.addr);
        if (isReader) {
          final writerAddr = runtime.heap.tryWriterForReader(t.addr);
          result.add(TermVar.reader(t.addr, writerAddr: writerAddr ?? t.addr));
        } else {
          result.add(TermVar.writer(t.addr,
              readerAddr: runtime.heap.pairedReaderAddr(t.addr)));
        }
      } else if (t is StructTerm) {
        // The first argument is met next, so it goes on the stack last.
        for (var i = t.args.length - 1; i >= 0; i--) {
          pending.add(t.args[i]);
        }
      }
      // ConstTerm has no variables
    }
  }

  /// Register global_send goals from GlobalSendSpawn info
  ///
  /// Called after globalize() or localize() to register any spawned goals.
  /// Per spec Section 4: global_send(T, G, Q) fires when T (the reader) becomes
  /// known, i.e., when the paired writer is bound. We register both the goal in
  /// the GlobalSendRegistry and an onBind callback on the heap so that
  /// onWriterBound is called when the writer is assigned.
  void registerGlobalSendSpawns(List<GlobalSendSpawn> spawns) {
    for (final spawn in spawns) {
      globalSendRegistry.register(GlobalSendGoal.fromSpawn(spawn));
      runtime.heap.onBind(spawn.readerAddr, (Term value) {
        onWriterBound(spawn.readerAddr, value);
      });
      _trace('[MAD $agentId] registered global_send goal: ${spawn.globalName} -> ${spawn.destAgent}');
    }
  }

  /// Record the reader names a globalization forwarded (Definition Globalize,
  /// case 3): from now on a value arriving for such a name is stale
  /// at this agent and is dropped, not held.
  void _recordForwardedNames(GlobalizeResult result) {
    for (final gn in result.forwardedNames) {
      _forwardedReaderNames.add((gn.agent, gn.index));
      _trace('[MAD $agentId] forwarded $gn under its original anchor');
    }
  }

  /// Remove the `global_send` goal of every imported writer this globalization
  /// forwarded (Definition Globalize, case 4). `globalize` already removed the
  /// record; the goal is registered under the same writer address, so the two
  /// go together and this agent leaves the link — it neither holds the sending
  /// end nor relays the value, which travels to the anchor in one message.
  void _dropForwardedWriterGoals(GlobalizeResult result) {
    for (final rec in result.forwardedWriters) {
      globalSendRegistry.removeGoalFor(rec.writerAddr);
      // The goal's heap hook goes with it: this writer will never be bound
      // here, and nothing must fire on its behalf.
      runtime.heap.removeBindCallback(rec.writerAddr);
      _trace('[MAD $agentId] forwarded imported writer as _w(${rec.anchor}, '
          '${rec.index}) — record and goal removed, link left');
    }
  }

  /// Queue the requests a localization produced (Definition Localize, request
  /// rule): req(_r(p,i)) to the anchor p for each reader name received from a
  /// sender other than its anchor.
  /// Every such request is held (Definition Held Link, case 1): "one is created
  /// exactly when the name was forwarded", so its existence is already the
  /// proof that the other end is at an agent we did not exchange the name with.
  /// The request goes into M_p marked held and reported; Send skips it until
  /// `authorise_link/2` answers.
  void _queueLocalizeRequests(LocalizeResult result, String fromAgent) {
    for (final gn in result.requests) {
      mp.add(OutboundMessage(
        destination: gn.agent,
        type: MessageType.request,
        payload: PayloadCodec.createRequestPayload(gn),
        link: (gn.agent, gn.index, false),
        held: true,
      ));
      _trace('[MAD $agentId] queued req($gn) to ${gn.agent} — HELD');
      _reportPendingLink(gn, fromAgent);
    }
  }

  // =========================================================================
  // madGLP Receive Transaction (Phase 4)
  // =========================================================================

  /// Handle incoming madGLP assignment message
  ///
  /// Per spec Section 8.3, handles three cases:
  ///
  /// **Case `_w(p, 0) := [T | _w(p,0)]` (Serializer)**: Cold-call to our network input.
  /// **Case `_w(p, i) := T` with i > 0**: We globalized writer Y, find entry (Y, q) at index i.
  /// **Case `_r(p, i) := T`**: We localized _r(p,i), search for entry (Z_q, p, i).
  ///
  /// Both non-serializer cases: localize T, bind writer, register spawned goals, remove entry.
  /// Serializer case: extend network input stream, update entry (don't remove).
  ///
  /// [serial] is the serial a serializer message carries, which its sender
  /// allocated (Definition "madGLP Receive Transaction", Serializer case: "m
  /// ... under serial i from sender s"); a serializer message has one and no
  /// other message does (IGLP Code Format, "Messages").  [fromAgent] is the
  /// sender the layer authenticates.
  void handleMadAssignment({
    required GlobalName globalName,
    required Term value,
    required String fromAgent,
    int? serial,
  }) {
    _trace('[MAD $agentId] handleMadAssignment: $globalName := $value from '
        '$fromAgent${serial == null ? '' : ' under serial $serial'}');

    final isSerializer = globalName.isWriter && globalName.index == 0;
    if (isSerializer != (serial != null)) {
      throw ArgumentError(isSerializer
          ? 'a serializer message $globalName := ... carries a serial'
          : 'only a serializer message carries a serial, not $globalName');
    }

    if (isSerializer) {
      // Case _w(p, 0) := [T | _w(p,0)] - serializer (cold-call to network input)
      _handleSerializerAssignment(value, fromAgent, serial!);
    } else if (globalName.isWriter) {
      // Case _w(p, i) := T with i > 0 - we globalized this writer, direct lookup
      _handleWriterAssignment(globalName, value, fromAgent);
    } else {
      // Case _r(p, i) := T - we localized this, search for entry
      _handleReaderAssignment(globalName, value, fromAgent);
    }
  }

  /// Handle serializer assignment _w(p, 0) := [T | _w(p,0)]
  ///
  /// This is a cold-call to our network input stream, under [serial] from
  /// [fromAgent], the sender the layer authenticates.  Definition madGLP
  /// Receive Transaction, Serializer case: "The receiving agent is q, with
  /// entry (N_q, D_q) at index 0 in W_q.  If (s, i) ∈ D_q, m is a repeat: it is
  /// dropped and nothing changes.  Otherwise let N'_q be a fresh local writer,
  /// σ̂ = {N_q := [T_q↓ | N'_q?]} ... The entry at index 0 in W'_q becomes
  /// (N'_q, D_q ∪ {(s, i)})."
  ///
  /// A repeat is dropped before anything is localized, so it creates no entry,
  /// record, goal, request or report.
  void _handleSerializerAssignment(Term value, String fromAgent, int serial) {
    _trace('[MAD $agentId] _handleSerializerAssignment: cold-call from '
        '$fromAgent under serial $serial');

    // Get current serializer writer from index-0 entry
    final currentWriter = wp.serializerWriterAddr;
    if (currentWriter == null) {
      throw StateError('Serializer entry not initialized at index 0');
    }
    _trace('[MAD $agentId] _handleSerializerAssignment: current serializer writer=$currentWriter');

    if (wp.serializerHasAppended(fromAgent, serial)) {
      _trace('[MAD $agentId] _handleSerializerAssignment: ($fromAgent, '
          '$serial) appended before — a repeat, dropped');
      return;
    }

    // The value is [T | _w(p,0)] - extract the content T
    Term content;
    if (value is StructTerm && value.functor == '.' && value.args.length == 2) {
      content = value.args[0];  // Head is the actual content
      // The tail (value.args[1]) is the serializer variable _w(p,0), ignored
      _trace('[MAD $agentId] _handleSerializerAssignment: extracted content from list cell');
    } else {
      // If not wrapped in list cell, use the value directly (for compatibility)
      content = value;
      _trace('[MAD $agentId] _handleSerializerAssignment: using value directly (no list wrapper)');
    }

    // Localize the content: replace global names with local variables
    // Per spec Section 8.3: "Localize T↑ by q to get T_q↓"
    final globalNames = extractGlobalNames(content);
    if (globalNames.isNotEmpty) {
      _trace('[MAD $agentId] _handleSerializerAssignment: found ${globalNames.length} global names to localize');
      final localizeResult = localize(
        globalNames: globalNames,
        localAgent: agentId,
        fromAgent: fromAgent,
        table: wp,
        records: up,
        freshAddrAllocator: () => runtime.heap.allocateVariable(),
      );

      // Register spawned goals from localization (_w cases; the records were
      // added by localize alongside them)
      registerGlobalSendSpawns(localizeResult.spawns);

      // Issue 7: deliver any `_r` assignment that arrived before these entries.
      _deliverHeldReaderAssignments(globalNames);

      // Request rule: forwarded names teach their anchors the new holder.
      _queueLocalizeRequests(localizeResult, fromAgent);

      // Replace global names with local variables in content
      content = localizeTermWithResult(content, globalNames, localizeResult);
      _trace('[MAD $agentId] _handleSerializerAssignment: localized content = $content');
    }

    _appendToNetworkInput(content);
    wp.recordSerializerAppend(fromAgent, serial);
  }

  /// Append [content] to this agent's network input stream through the index-0
  /// serializer entry (Definition Index-0 Serializer): bind the current
  /// serializer writer to `[content | N'?]`, repoint the permanent entry at the
  /// fresh writer, and reactivate whatever was suspended on the stream.
  ///
  /// Both a localized cold-call and a `pending_link` report arrive this way —
  /// the report is "appended to p's network input stream through the index-0
  /// serializer entry" (Definition Held Link).
  void _appendToNetworkInput(Term content) {
    final currentWriter = wp.serializerWriterAddr;
    if (currentWriter == null) {
      throw StateError(
        'Serializer entry not initialized at index 0 for agent $agentId: '
        'the network input stream is where cold-calls and pending_link reports '
        'are delivered, and boot must create the entry before either can occur',
      );
    }

    // Allocate fresh writer for stream continuation
    final (freshWriter, freshReader) = runtime.heap.allocateVariable();
    _trace('[MAD $agentId] _appendToNetworkInput: fresh stream continuation ($freshWriter,$freshReader)');

    // Build the list cell [content | freshReader] and extend the stream
    final listCell = StructTerm('.', [content, VarRef(freshReader)]);
    final activations = runtime.heap.bindVariable(currentWriter, listCell);
    _trace('[MAD $agentId] _appendToNetworkInput: bound stream, ${activations.length} activations');

    // Update the serializer entry to point to the fresh writer (permanent entry)
    wp.updateSerializerWriter(freshWriter);
    _trace('[MAD $agentId] _appendToNetworkInput: updated serializer entry to writer=$freshWriter');

    // Reactivate suspended goals waiting on the network input stream
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }
  }

  // ===========================================================================
  // DECLARED PLACES (Definition Seam Predicates)
  // ===========================================================================
  //
  // The declared places and their streams are the runtime's bookkeeping,
  // [GlpRuntime.placeStreams], kept whether a layer is bound or none is
  // (runtime/place_streams.dart); here the layer is told and its reports are
  // put on the streams.

  /// Whether [place] is declared at this agent.
  bool hasDeclaredPlace(String place) => runtime.placeStreams.isDeclared(place);

  /// Names of the places declared at this agent.
  Iterable<String> get declaredPlaces => runtime.placeStreams.places;

  /// Declare [place] with radius [radiusMetres], its event stream growing from
  /// [streamWriterAddr] — the writer of the `E` of `place_declare(P, R, E)`.
  ///
  /// A further declaration of the same place supersedes the earlier one, whose
  /// stream is closed (Definition Seam Predicates). The stream exists before the
  /// layer call, so an event cannot arrive before there is a stream to carry it.
  ///
  /// Where the layer refuses, the declaration stands and its stream carries
  /// `unobservable`.
  void declarePlace(String place, double radiusMetres, HeapCell streamWriterAddr) {
    final network = this.network;
    if (network == null) {
      throw StateError('no GlpNetwork bound to agent $agentId');
    }

    // A further declaration supersedes the earlier one, closing its stream.
    final declaration =
        runtime.placeStreams.declare(place, streamWriterAddr);
    _installPlaceEventHandler(network);
    _trace('[MAD $agentId] declarePlace($place, $radiusMetres): stream '
        'writer=$streamWriterAddr');

    // The layer answers asynchronously; the stream exists either way. On a
    // refusal E receives `unobservable`, the declaration stands, and
    // `observable` follows if the layer later begins reporting (Definition Seam
    // Predicates). A refusal is the layer watching nothing, which is what
    // `unobservable` says; the stream is not closed, since a refusal is neither
    // of the two closing cases.
    network.declarePlace(place, radiusMetres).then(
      (declared) {
        if (declared) return;
        _trace('[MAD $agentId] declarePlace($place): refused by the platform '
            '— unobservable');
        // Answers can arrive after place_remove or after a superseding
        // declaration, whose stream is the successor's, not this one's.
        if (!runtime.placeStreams.isStanding(place, declaration)) return;
        deliverPlaceEvent(place, PlaceEvent.unobservable);
      },
      onError: (Object e) => _trace(
          '[MAD $agentId] declarePlace($place): layer failed: $e'),
    );
  }

  /// End the declaration of [place], closing its stream. Does nothing where
  /// [place] is not declared (Definition Seam Predicates).
  void removePlace(String place) {
    final network = this.network;
    if (network == null) {
      throw StateError('no GlpNetwork bound to agent $agentId');
    }
    if (!runtime.placeStreams.close(place)) return;
    _trace('[MAD $agentId] place $place: stream closed');
    network.removePlace(place).catchError((Object e) =>
        _trace('[MAD $agentId] removePlace($place): layer failed: $e'));
    _trace('[MAD $agentId] removePlace($place)');
  }

  /// Route the layer's place events onto the declared places' streams. Installed
  /// once per network; the layer reports observability on the stream of every
  /// standing declaration, so each report arrives here already named.
  void _installPlaceEventHandler(GlpNetwork network) {
    if (identical(_placeEventHandlerNetwork, network)) return;
    _placeEventHandlerNetwork = network;
    network.onPlaceEvent = deliverPlaceEvent;
  }

  GlpNetwork? _placeEventHandlerNetwork;

  /// Deliver [event] on [place]'s stream, serializer-fashion (Definition Seam
  /// Predicates): bind the current tail writer to `[event | E'?]`, retain the
  /// fresh writer as the new tail, and reactivate whatever suspended on the
  /// stream. One declaration yields one stream however many events follow.
  ///
  /// An event for a place that is not declared here is dropped.
  void deliverPlaceEvent(String place, PlaceEvent event) {
    if (!runtime.placeStreams.deliver(place, ConstTerm(event.name))) {
      _trace('[MAD $agentId] place event $event for undeclared $place — dropped');
      return;
    }
    _trace('[MAD $agentId] place $place: $event');
    onPlaceEventDelivered?.call();
  }

  /// The ground presentation of a global name (§Global Variable Names): the
  /// 2-ary structures `'_r'(P, I)` and `'_w'(P, I)`, functor giving polarity,
  /// first argument the anchor. This is the form a program sees — not the wire
  /// form, which stands in the place of a variable.
  static Term groundGlobalName(GlobalName gn) => StructTerm(
      gn.isWriter ? '_w' : '_r',
      [ConstTerm(gn.agent), ConstTerm(gn.index)]);

  /// Reports produced by the transaction in progress, appended to the network
  /// input stream when it completes (see [_reportPendingLink]).
  final List<Term> _deferredReports = [];

  /// Report a held link (Definition Held Link): append `pending_link(G, S)` to
  /// this agent's network input stream, within the transaction that would
  /// otherwise have acted. [sender] is the agent the name or the traffic came
  /// from — not the anchor, where they differ.
  ///
  /// The report is queued and appended when the transaction completes, per
  /// Definition Held Link: "The report is appended last in that transaction,
  /// after anything the transaction itself appends, so a link is reported only
  /// once the term carrying it can be read." A cold-call carrying a held link
  /// therefore reaches the program first, and the report about it second.
  void _reportPendingLink(GlobalName gn, String sender) {
    _reportedLinks[(gn.agent, gn.index, gn.isWriter)] = sender;
    _deferredReports.add(StructTerm(
        'pending_link', [groundGlobalName(gn), ConstTerm(sender)]));
    _trace('[MAD $agentId] HELD $gn from $sender — reporting pending_link');
  }

  /// Append the transaction's reports to the network input stream, in the order
  /// they were produced. Idempotent when there are none.
  void _flushReports() {
    if (_deferredReports.isEmpty) return;
    final reports = List<Term>.from(_deferredReports);
    _deferredReports.clear();
    for (final r in reports) {
      _appendToNetworkInput(r);
    }
  }

  /// Whether the link `(anchor, index, isWriter)` has been reported and not yet
  /// answered.
  bool hasReportedLink(String anchor, int index, bool isWriter) =>
      _reportedLinks.containsKey((anchor, index, isWriter));

  /// Number of links reported and awaiting an answer.
  int get reportedLinkCount => _reportedLinks.length;

  /// Every link reported and not yet answered, with the sender each was
  /// reported against — the `(G, S)` of its `pending_link` report.
  Iterable<(GlobalName, String)> get reportedLinks => _reportedLinks.entries
      .map((e) => (
            e.key.$3
                ? GlobalName.writer(e.key.$1, e.key.$2)
                : GlobalName.reader(e.key.$1, e.key.$2),
            e.value
          ));

  /// Handle _w(p, i) := T assignment with i > 0 (we globalized writer Y)
  ///
  /// We are agent p, the link's anchor. We globalized writer Y, creating
  /// GlobalizeEntry (Y, q) at index i. The holder's `global_send` goal fires
  /// and sends `_w(p, i) := T` to us; the lookup is by index alone.
  ///
  /// Spec (madGLP Receive, Writer case): "The receiving agent is p, with entry
  /// (X, q) at index i in W_p; **the sender need not be q, the name having
  /// been forwarded** (Definition Globalize)." So [fromAgent] is not matched
  /// against the entry's `remoteAgent` — it serves only to localize the
  /// nested global names the value carries. The entry's `remoteAgent` records
  /// who the writer was first exported to; after a forwarding it is no longer
  /// the only possible sender. A forwarded writer needs no request and gets no
  /// acknowledgement: `_w(p, i)` names its destination, so one message
  /// suffices however far the writer travelled.
  ///
  /// [authorised] is set when `authorise_link/2` releases an assignment this
  /// agent held under case 4; the hold test is then skipped, the answer already
  /// given.
  void _handleWriterAssignment(GlobalName globalName, Term value, String fromAgent,
      {bool authorised = false}) {
    final entry = wp.lookupByIndex(globalName.index);

    if (entry == null) {
      // Stale drop (spec §app:requests-acks): a value matching no entry is
      // dropped. A writer-name value always finds its fixed anchor entry, so
      // a miss is provably stale — indices are never reused.
      _trace('[MAD $agentId] _handleWriterAssignment: no entry for $globalName'
          ' — stale, dropped');
      return;
    }

    // An assignment for a link already held is dropped, not held or reported
    // again: "A request from a requester already recorded for the link, and
    // an assignment for a link already holding one, are dropped, so each party
    // is reported once" (§Held Links) — an assignment delivered twice among
    // them (Remark "Fair Message Delivery").
    final linkKey = (globalName.agent, globalName.index, true);
    if (!authorised && _heldAssignments.containsKey(linkKey)) {
      _trace('[MAD $agentId] _handleWriterAssignment: $globalName already '
          'held — assignment dropped');
      return;
    }

    // Definition Held Link, case 4: at the anchor, an assignment arriving from
    // an agent it did not export the name to. The test is structural — the
    // entry's `q` against the sender — and consults nothing outside the agent.
    // This is why `q` is kept in the entry `(X, q)`.
    if (entry.remoteAgent != fromAgent && !authorised) {
      _heldAssignments[linkKey] = (value: value, fromAgent: fromAgent);
      _reportPendingLink(globalName, fromAgent);
      return;
    }

    final writerAddr = entry.writerAddr;
    _trace('[MAD $agentId] _handleWriterAssignment: globalize-writer entry, writerAddr=$writerAddr');

    // Localize the value: replace global names with local variables
    // Per spec Section 8.3: "Localize T↑ by p to get T_p↓"
    Term localizedValue = value;
    final globalNames = extractGlobalNames(value);
    if (globalNames.isNotEmpty) {
      _trace('[MAD $agentId] _handleWriterAssignment: localizing ${globalNames.length} nested global names');
      final localizeResult = localize(
        globalNames: globalNames,
        localAgent: agentId,
        fromAgent: fromAgent,
        table: wp,
        records: up,
        freshAddrAllocator: () => runtime.heap.allocateVariable(),
      );
      registerGlobalSendSpawns(localizeResult.spawns);
      _deliverHeldReaderAssignments(globalNames);
      _queueLocalizeRequests(localizeResult, fromAgent);
      localizedValue = localizeTermWithResult(value, globalNames, localizeResult);
      _trace('[MAD $agentId] _handleWriterAssignment: localized value = $localizedValue');
    }

    // Bind the writer with localized value
    final activations = runtime.heap.bindVariable(writerAddr, localizedValue);
    _trace('[MAD $agentId] _handleWriterAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    wp.removeGlobalizeEntry(globalName.index);
    _trace('[MAD $agentId] _handleWriterAssignment: entry removed');
  }

  /// Handle _r(p, i) := T assignment (we localized _r(p, i))
  ///
  /// We are agent q. We localized _r(p, i), creating LocalizeEntry (Z_q, p, i).
  /// Now p's gs fires and sends _r(p, i) := T to us. Search by (p, i).
  /// Per spec Section 8.3: "Agent q searches for entry (X_q, p, i)."
  void _handleReaderAssignment(GlobalName globalName, Term value, String fromAgent) {
    final entry = wp.findByRemote(globalName.agent, globalName.index);
    if (entry == null) {
      // After forwarding, no-entry means stale (spec §app:requests-acks): the
      // reader moved on, and the anchor's redirected value will reach its
      // current holder. Drop — holding is never needed for a forwarded name.
      if (_forwardedReaderNames
          .contains((globalName.agent, globalName.index))) {
        _trace('[MAD $agentId] _handleReaderAssignment: $globalName was '
            'forwarded — stale value dropped');
        return;
      }
      // A repeat of a value already applied here finds its entry removed, and
      // is dropped (Remark Fair Message Delivery).
      if (_appliedReaderNames.contains((globalName.agent, globalName.index))) {
        _trace('[MAD $agentId] _handleReaderAssignment: $globalName was '
            'applied before — a repeat, dropped');
        return;
      }
      // Issue 7 / spec §8.3 (Early Messages): the assignment arrived before its
      // LocalizeEntry exists (possible under any non-FIFO transport). Hold it,
      // keyed by (remoteAgent, remoteIndex); localize() delivers it when it
      // creates the matching entry. Do not throw — the message is not dropped.
      _heldReaderAssignments[(globalName.agent, globalName.index)] =
          (value: value, fromAgent: fromAgent);
      _trace('[MAD $agentId] _handleReaderAssignment: no entry for $globalName '
          'yet — held pending localize');
      return;
    }

    _trace('[MAD $agentId] _handleReaderAssignment: localize-reader entry, writerAddr=${entry.writerAddr}');

    // Localize the value: replace global names with local variables
    // Per spec Section 8.3: "Localize T↑ by q to get T_q↓"
    Term localizedValue = value;
    final globalNames = extractGlobalNames(value);
    if (globalNames.isNotEmpty) {
      _trace('[MAD $agentId] _handleReaderAssignment: localizing ${globalNames.length} nested global names');
      final localizeResult = localize(
        globalNames: globalNames,
        localAgent: agentId,
        fromAgent: fromAgent,
        table: wp,
        records: up,
        freshAddrAllocator: () => runtime.heap.allocateVariable(),
      );
      registerGlobalSendSpawns(localizeResult.spawns);
      _deliverHeldReaderAssignments(globalNames);
      _queueLocalizeRequests(localizeResult, fromAgent);
      localizedValue = localizeTermWithResult(value, globalNames, localizeResult);
      _trace('[MAD $agentId] _handleReaderAssignment: localized value = $localizedValue');
    }

    // Bind the writer with localized value
    final activations = runtime.heap.bindVariable(entry.writerAddr, localizedValue);
    _trace('[MAD $agentId] _handleReaderAssignment: bound writer, ${activations.length} activations');

    // Reactivate suspended goals
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }

    // Remove the entry
    wp.removeLocalizeEntry(globalName.agent, globalName.index);
    _appliedReaderNames.add((globalName.agent, globalName.index));
    _trace('[MAD $agentId] _handleReaderAssignment: entry removed');

    // Acknowledge to the value's sender (madGLP Receive, Reader case): the
    // acknowledgement releases the pending value at the sender and closes the
    // link. Only reader-name values are acknowledged.
    mp.add(OutboundMessage(
      destination: fromAgent,
      type: MessageType.acknowledgement,
      payload: PayloadCodec.createAckPayload(globalName),
    ));
    _trace('[MAD $agentId] _handleReaderAssignment: queued ack($globalName) '
        'to $fromAgent');
  }

  /// Deliver any held `_r` assignments whose `LocalizeEntry` was just created.
  ///
  /// Issue 7 / spec §8.3: after `localize()` creates LocalizeEntries for the
  /// given global names, check the hold table for each `_r(p, i)` name and
  /// process any assignment that arrived before the entry existed. Delivering
  /// re-enters `_handleReaderAssignment`, which now finds the entry; nested
  /// held assignments terminate because each delivery removes its hold-table
  /// entry.
  void _deliverHeldReaderAssignments(List<GlobalName> globalNames) {
    for (final gn in globalNames) {
      if (gn.isWriter) continue; // only `_r` names create LocalizeEntries
      final held = _heldReaderAssignments.remove((gn.agent, gn.index));
      if (held == null) continue;
      _trace('[MAD $agentId] delivering held assignment for $gn from '
          '${held.fromAgent}');
      _handleReaderAssignment(gn, held.value, held.fromAgent);
    }
  }

  /// Handle madGLP assignment with nested global names
  ///
  /// Extended version that handles nested global names in the value term,
  /// creating LocalizeEntries and spawning global_send goals as needed.
  void handleMadAssignmentWithGlobalNames({
    required GlobalName globalName,
    required Term value,
    required List<GlobalName> nestedGlobalNames,
    required String fromAgent,
  }) {
    _trace('[MAD $agentId] handleMadAssignmentWithGlobalNames: $globalName with ${nestedGlobalNames.length} nested names');

    // Localize the nested global names
    final localizeResult = localize(
      globalNames: nestedGlobalNames,
      localAgent: agentId,
      fromAgent: fromAgent,
      table: wp,
      records: up,
      freshAddrAllocator: () => runtime.heap.allocateVariable(),
    );

    // Register spawned goals from localization
    registerGlobalSendSpawns(localizeResult.spawns);

    // Issue 7: deliver any `_r` assignment that arrived before these entries.
    _deliverHeldReaderAssignments(nestedGlobalNames);

    // Request rule: forwarded names teach their anchors the new holder.
    _queueLocalizeRequests(localizeResult, fromAgent);

    // Now handle the main assignment
    handleMadAssignment(
      globalName: globalName,
      value: value,
      fromAgent: fromAgent,
    );
  }

  // =========================================================================
  // madGLP Receive Transaction — Request and Acknowledgement
  // =========================================================================

  /// Handle an incoming madGLP payload: dispatch on the message kind byte
  /// (code-format v2 — 0 value, 1 request, 2 acknowledgement).
  ///
  /// This is the single entry point for the receive path: both the isolate
  /// runner and the app runtime hand the opaque payload bytes here.
  ///
  /// A value message carrying a module whose certificate does not verify is
  /// refused at receipt, with the reason (GLP #3 Cowork, 2026-10-02 20:58 UTC:
  /// "a received module whose certificate does not verify is not a Module
  /// value and the message carrying it is refused at receipt, with the reason,
  /// as IGLP's loader refuses at adoption; nothing is delivered as text").
  /// The refusal is the payload's decoding, before any Receive: "A payload is
  /// one assignment message in the canonical encoding" (IGLP
  /// app:in-networking, "Payloads"), a module constant decodes "to the Module
  /// constant" (§cf-terms, constant tag 6), and a module that is no Module
  /// value leaves the payload no message, so no Receive transaction
  /// (Definition "madGLP Receive Transaction") takes it.  Nothing is assigned,
  /// no entry is consumed, nothing is acknowledged, held or reported, and the
  /// runtime prints the refusal and its reason, as it prints a kernel's abort.
  /// The sender is told nothing, IGLP having no message for it: a reader-name
  /// value stays pending there, unacknowledged (Definition "madGLP Local
  /// State").
  void handleIncomingPayload({
    required List<int> payload,
    required String fromAgent,
  }) {
    if (payload.isEmpty) {
      _trace('[MAD $agentId] handleIncomingPayload: empty payload dropped');
      return;
    }
    switch (payload[0]) {
      case wireMsgKindValue:
        final ({GlobalName name, Term value, int? serial}) received;
        try {
          received = PayloadCodec.deserializeValuePayload(payload);
        } on ModuleRefusal catch (refusal) {
          print('[REFUSED] $agentId: a message from $fromAgent is refused at '
              'receipt: ${refusal.message}');
          return;
        }
        handleMadAssignment(
            globalName: received.name,
            value: received.value,
            fromAgent: fromAgent,
            serial: received.serial);
      case wireMsgKindRequest:
        handleRequest(
            PayloadCodec.decodeRequestOrAckPayload(payload), fromAgent);
      case wireMsgKindAcknowledgement:
        handleAck(PayloadCodec.decodeRequestOrAckPayload(payload), fromAgent);
      default:
        throw FormatException(
            'unknown madGLP message kind: ${payload[0]}');
    }
    // The Receive transaction is complete: its held-link reports go on the
    // network input stream now, after whatever the transaction delivered.
    _flushReports();
  }

  /// Handle req(_r(p, i)) — we are the anchor p (madGLP Receive, Request
  /// case): the request is held; on authorisation the requester is recorded
  /// among the link's holders and a pending value is addressed to it as well.
  /// A request repeating a recorded (link, requester) pair, and a request for
  /// a closed link, are dropped.
  void handleRequest(GlobalName globalName, String fromAgent) {
    _trace('[MAD $agentId] handleRequest: req($globalName) from $fromAgent');
    final key = (globalName.agent, globalName.index);

    // "A request repeating a (link, requester) pair already recorded --- the
    // link held, authorised or closed --- is dropped" (§app:requests-acks); "A
    // request from a requester already recorded for the link ... [is]
    // dropped, so each party is reported once" (§Held Links) — a request
    // delivered twice among them (Remark "Fair Message Delivery").  A closed
    // link's pairs are released, and the closed case falls to the test below.
    if (_requestPairs[key]?.contains(fromAgent) ?? false) {
      _trace('[MAD $agentId] handleRequest: ($globalName, $fromAgent) '
          'recorded before — request dropped');
      return;
    }

    // A request matching no live link is stale and dropped as before, without
    // troubling the program: "A request for a closed link is dropped."
    if (!_readerLinkIsLive(globalName)) {
      _trace('[MAD $agentId] handleRequest: $globalName matches nothing — '
          'stale request dropped');
      return;
    }

    // Definition Held Link, case 3: at the anchor, a request. Every request is
    // held — the agent the anchor exported the name to sends none, so a request
    // is proof the name was forwarded. Nothing is redirected and no holder is
    // recorded until `authorise_link/2` answers.  A further requester's
    // request is held behind the one reported, and reported in its turn.
    (_requestPairs[key] ??= <String>{}).add(fromAgent);
    final queue = _heldRequests[key] ??= <String>[];
    queue.add(fromAgent);
    if (queue.length == 1) {
      _reportPendingLink(globalName, fromAgent);
    } else {
      _trace('[MAD $agentId] handleRequest: req($globalName) from '
          '$fromAgent held behind ${queue.length - 1} — reported in its turn');
    }
  }

  /// Whether the reader-name link [globalName] anchored here is live: its
  /// value not yet produced (the `global_send` goal waits) or produced and
  /// pending.  Otherwise the link is closed — acknowledged or refused.
  bool _readerLinkIsLive(GlobalName globalName) =>
      _pendingReaderValues
          .containsKey((globalName.agent, globalName.index)) ||
      globalSendRegistry.hasGoalForLink(globalName);

  /// After the program answered the held request reported for the link
  /// [globalName], report the next requester held for it, in its turn (§Held
  /// Links).  A held request whose link has closed meanwhile is dropped
  /// unreported: "A request for a closed link is dropped" (madGLP Receive,
  /// Request case).
  void _reportNextHeldRequest(GlobalName globalName) {
    final key = (globalName.agent, globalName.index);
    final queue = _heldRequests[key];
    if (queue == null) return;
    if (queue.isNotEmpty && _readerLinkIsLive(globalName)) {
      _reportPendingLink(globalName, queue.first);
      return;
    }
    for (final requester in queue) {
      _trace('[MAD $agentId] held req($globalName) from $requester: the link '
          'closed — dropped');
    }
    _heldRequests.remove(key);
  }

  /// Release the request records of the reader-name link [globalName], now
  /// closed: its recorded (link, requester) pairs, and the requests held
  /// behind the one reported, which are for a closed link.
  void _releaseRequestRecords(GlobalName globalName, {bool held = false}) {
    final key = (globalName.agent, globalName.index);
    _requestPairs.remove(key);
    if (held) _heldRequests.remove(key);
  }

  /// Perform the request the anchor held, once authorised (madGLP Receive,
  /// Request case): record the requester among the link's holders, and address
  /// a pending value to it as well, unsent.
  void _applyRequest(GlobalName globalName, String fromAgent) {
    final key = (globalName.agent, globalName.index);
    final pending = _pendingReaderValues[key];
    if (pending != null) {
      // Address the pending value to the requester as well: "if the value is
      // pending, p addresses it to s as well" (§app:requests-acks). The copy
      // sent to a past holder is dropped there as stale; the value stays
      // pending until the current holder's acknowledgement arrives.
      _pendingReaderValues[key] = (
        payload: pending.payload,
        destination: fromAgent,
        nestedReaderNames: pending.nestedReaderNames,
      );
      mp.add(OutboundMessage(
        destination: fromAgent,
        type: MessageType.assignment,
        payload: pending.payload,
      ));
      _trace('[MAD $agentId] handleRequest: pending $globalName addressed '
          'to $fromAgent as well');
      return;
    }

    // Value not yet produced: record the requester among the link's holders —
    // per-link runtime state consulted at send time, when the value goes to
    // each holder recorded (§app:requests-acks).
    if (globalSendRegistry.recordRequester(globalName, fromAgent)) {
      _trace('[MAD $agentId] handleRequest: $fromAgent recorded among the '
          'holders of $globalName');
      return;
    }

    // No pending value, no goal: the link is closed — stale request, dropped.
    _trace('[MAD $agentId] handleRequest: $globalName matches nothing — '
        'stale request dropped');
  }

  /// Handle ack(_r(p, i)) from agent s — we sent the value (madGLP Receive,
  /// Acknowledgement case): record s as the holder of every link whose reader
  /// name anchored here occurs in the acknowledged value — s localized them —
  /// re-addressing any pending value of such a link to s; then remove the
  /// acknowledged value: the link is closed. An acknowledgement for a closed
  /// link is dropped.
  void handleAck(GlobalName globalName, String fromAgent) {
    final key = (globalName.agent, globalName.index);
    final removed = _pendingReaderValues.remove(key);
    if (removed == null) {
      _trace('[MAD $agentId] handleAck: ack($globalName) matches no pending '
          'value — stale, dropped');
      return;
    }
    for (final nested in removed.nestedReaderNames) {
      _recordHolder(nested, fromAgent);
    }
    // The link is closed: every request for it is now dropped as one for a
    // closed link, so its recorded (link, requester) pairs are released.
    _releaseRequestRecords(globalName);
    _trace('[MAD $agentId] handleAck: pending $globalName released — link '
        'closed');
  }

  /// Record [holder] as the holder of the link [globalName] (per-link runtime
  /// state): a not-yet-fired global_send goal has its destination updated; a
  /// pending value addressed elsewhere is re-addressed to [holder]. A closed
  /// link needs nothing.
  void _recordHolder(GlobalName globalName, String holder) {
    if (globalSendRegistry.redirectGoal(globalName, holder)) {
      _trace('[MAD $agentId] holder of $globalName recorded as $holder');
      return;
    }
    final key = (globalName.agent, globalName.index);
    final pending = _pendingReaderValues[key];
    if (pending != null && pending.destination != holder) {
      _pendingReaderValues[key] = (
        payload: pending.payload,
        destination: holder,
        nestedReaderNames: pending.nestedReaderNames,
      );
      mp.add(OutboundMessage(
        destination: holder,
        type: MessageType.assignment,
        payload: pending.payload,
      ));
      _trace('[MAD $agentId] pending $globalName re-addressed to holder '
          '$holder');
    }
  }

  // =========================================================================
  // Held Links — authorise_link/2
  // =========================================================================

  /// Answer a reported link (Definition authorise\_link Predicate).
  ///
  /// On [authorise], release what was held: a held outgoing message becomes
  /// unsent, a retained assignment is applied, a retained request is performed.
  /// On refuse, drop the link: the held message is removed from M_p, a retained
  /// assignment is discarded, and the link's entry, record, and `global_send`
  /// goal at this agent are released.
  ///
  /// A refusal is accepted in every case — the program refuses for reasons the
  /// runtime does not see. Returns false only when the link was never reported,
  /// so the caller can tell an answer from a mis-addressed one.
  ///
  /// A held request answered, the next requester held for the same link is
  /// reported in its turn (§Held Links), within this transaction.
  bool answerLink(GlobalName gn, bool authorise) {
    final answered = _answerLink(gn, authorise);
    if (answered && !gn.isWriter) _reportNextHeldRequest(gn);
    _flushReports();
    return answered;
  }

  bool _answerLink(GlobalName gn, bool authorise) {
    final linkKey = (gn.agent, gn.index, gn.isWriter);
    if (_reportedLinks.remove(linkKey) == null) {
      _trace('[MAD $agentId] answerLink: $gn was not reported — ignored');
      return false;
    }
    _trace('[MAD $agentId] answerLink: $gn '
        '${authorise ? "AUTHORISED" : "REFUSED"}');

    final heldMessage = mp.findHeld(gn.agent, gn.index, gn.isWriter);
    final heldAssignment = _heldAssignments.remove(linkKey);
    // The held request reported for the link is the first held for it.
    final heldQueue = gn.isWriter ? null : _heldRequests[(gn.agent, gn.index)];
    final heldRequester = (heldQueue == null || heldQueue.isEmpty)
        ? null
        : heldQueue.removeAt(0);

    if (authorise) {
      // A held outgoing message becomes unsent — the same message, released.
      if (heldMessage != null) {
        heldMessage.held = false;
        // Case 2: the value now goes, so its record has served its purpose.
        final rec = up.findByLink(gn.agent, gn.index);
        if (rec != null) up.remove(rec.writerAddr);
        _trace('[MAD $agentId] answerLink: held message for $gn released');
      }
      // A retained assignment is applied, by the ordinary receive path.
      if (heldAssignment != null) {
        _handleWriterAssignment(
            gn, heldAssignment.value, heldAssignment.fromAgent,
            authorised: true);
      }
      // A retained request is performed.
      if (heldRequester != null) {
        _applyRequest(gn, heldRequester);
      }
      return true;
    }

    // Refuse: drop the link.
    if (heldMessage != null) {
      mp.remove(heldMessage);
      _trace('[MAD $agentId] answerLink: held message for $gn removed');
    }
    _dropLink(gn);
    return true;
  }

  /// Release a refused link's entry, record, and `global_send` goal at this
  /// agent, and any pending value addressed by it.
  void _dropLink(GlobalName gn) {
    if (gn.isWriter) {
      wp.removeGlobalizeEntry(gn.index);
    } else {
      wp.removeLocalizeEntry(gn.agent, gn.index);
      _pendingReaderValues.remove((gn.agent, gn.index));
      // The link is closed: its recorded pairs go, and the requests still
      // held for it are for a closed link, dropped.
      _releaseRequestRecords(gn, held: true);
    }
    final rec = up.findByLink(gn.agent, gn.index);
    if (rec != null) {
      up.remove(rec.writerAddr);
      globalSendRegistry.removeGoalFor(rec.writerAddr);
      runtime.heap.removeBindCallback(rec.writerAddr);
    }
    final goal = globalSendRegistry.removeGoalForLink(gn);
    if (goal != null) {
      runtime.heap.removeBindCallback(goal.readerAddr);
    }
    _trace('[MAD $agentId] _dropLink: $gn released — entry, record, goal');
  }

  /// Number of pending (sent, unacknowledged) reader-name values.
  int get pendingReaderValueCount => _pendingReaderValues.length;

  /// Number of assignments retained because their link is held.
  int get heldAssignmentCount => _heldAssignments.length;

  /// Number of requests retained because their link is held: the one reported
  /// for each link and those held behind it.
  int get heldRequestCount =>
      _heldRequests.values.fold(0, (n, q) => n + q.length);

  /// Number of (link, requester) pairs recorded for links still live.
  int get recordedRequestPairCount =>
      _requestPairs.values.fold(0, (n, s) => n + s.length);

  /// Number of reader-name assignments retained because they arrived early,
  /// before the entry their localization creates (IGLP app:in-networking,
  /// Early messages).
  int get earlyAssignmentCount => _heldReaderAssignments.length;

  /// Whether the value for reader name (anchorAgent, index) is still pending.
  bool hasPendingReaderValue(String anchorAgent, int index) =>
      _pendingReaderValues.containsKey((anchorAgent, index));

  // =========================================================================
  // Export/Import Operations (for Flutter app compatibility)
  // =========================================================================

  /// Export a term for sending to another agent
  ///
  /// Extracts variables and registers them in W_p as needed.
  /// For madGLP, this prepares the term for serialization by setting up
  /// onBind callbacks for writers so assignments can be routed.
  void exportTerm(Term term) {
    final vars = <TermVar>[];
    _extractTermVars(term, vars);

    // Register onBind callbacks for any writers in the term
    for (final v in vars) {
      if (v.isWriter) {
        // Set up callback so when this writer is bound, we can route the assignment
        runtime.heap.onBind(v.addr, (Term value) {
          onWriterBound(v.addr, value);
        });
        _trace('[MAD $agentId] exportTerm: registered callback for writer ${v.addr}');
      }
    }
  }

  /// Process suspension - in madGLP, blocking readers don't trigger immediate requests
  ///
  /// The madGLP model is push-based: assignments are sent when writers are bound,
  /// not when readers request. However, for compatibility with the UI, we log
  /// blocking readers.
  void processSuspension(Set<HeapCell> blockingReaders) {
    // In madGLP, suspension means we're waiting for assignments to arrive
    // The push model means we don't send read requests - we just wait
    for (final readerId in blockingReaders) {
      _trace('[MAD $agentId] processSuspension: waiting for assignment to reader $readerId');
    }
    // No explicit request messages needed in madGLP push model
  }

  // =========================================================================
  // Unified Send ('_send' builtin implementation)
  // =========================================================================

  /// Unified send operation per spec Section 11.5
  ///
  /// The `'_send'(T, G, Q)` builtin behavior depends on whether G is a serializer
  /// address (index 0) or a normal global name (index > 0):
  ///
  /// **Case G = `_w(q, 0)` (Serializer)**:
  /// 1. Globalizes term T for remote agent Q
  /// 2. Allocates a serial i from the agent's global-name counter, one per
  ///    message, so that (this agent, i) is unique for the whole run as every
  ///    global name is (Remark "Cold-Call via Serializer", item 1)
  /// 3. Adds message `(_w(q,0) := [T↑ | _w(q,0)], Q)` under serial i to M_p —
  ///    content wrapped in list cell
  ///
  /// **Case G = `_w(p, i)` or `_r(p, i)` with i > 0 (Normal)**:
  /// 1. Globalizes term T for remote agent Q
  /// 2. Adds message `(G := T↑, Q)` to M_p — content sent directly
  void send(Term term, bool isWriter, String gnAgent, int gnIndex, String destAgent) {
    _trace('[MAD $agentId] send: term=$term, isWriter=$isWriter, gnAgent=$gnAgent, gnIndex=$gnIndex, dest=$destAgent');

    // Extract variables from the term for globalization
    final vars = <TermVar>[];
    _extractTermVars(term, vars);
    _trace('[MAD $agentId] send: found ${vars.length} variables in term');

    // Globalize the term for the destination agent
    // This creates entries for writers and spawns global_send goals for readers
    final globalizeResult = globalize(
      variables: vars,
      localAgent: agentId,
      remoteAgent: destAgent,
      table: wp,
      records: up,
    );

    // Register the spawned global_send goals for readers
    registerGlobalSendSpawns(globalizeResult.spawns);

    // Record names forwarded by this globalization (stale-drop bookkeeping).
    _recordForwardedNames(globalizeResult);

    // Imported writers in the term were forwarded under their original names
    // (Definition Globalize, case 4): drop their goals — this agent leaves
    // those links, so the value goes from the new holder to the anchor direct.
    _dropForwardedWriterGoals(globalizeResult);

    // For globalize-writer entries: NO onBind is registered here.
    // Per spec Section 5.1: when Y is a writer, p creates an entry (Y, q) and
    // waits for the assignment to arrive. The global_send goal is spawned at q
    // (by localize), not at p. Agent p does not send anything for _w entries.

    // Transform the term to use global names
    final globalizedTerm = globalizeTermWithResult(term, vars, globalizeResult);
    _trace('[MAD $agentId] send: globalized term = $globalizedTerm');

    // Build the global name structure
    final globalName = isWriter
        ? GlobalName.writer(gnAgent, gnIndex)
        : GlobalName.reader(gnAgent, gnIndex);

    // Create payload based on serializer (index 0) vs normal (index > 0)
    List<int> payload;
    if (isWriter && gnIndex == 0) {
      // Serializer case: wrap in list cell [T↑ | _w(q,0)], under a serial
      // this agent allocates from its global-name counter — after T's own
      // names, which keep the indices they had before cold calls carried one.
      final serial = wp.allocateIndex();
      _trace('[MAD $agentId] send: serializer case, wrapping in list cell, '
          'serial $serial');
      payload = PayloadCodec.createSerializerPayload(globalName, globalizedTerm,
          serial: serial);
    } else {
      // Normal case: send directly
      _trace('[MAD $agentId] send: normal case, sending directly');
      payload = PayloadCodec.createGlobalSendPayload(globalName, globalizedTerm);
    }

    // Queue the message for delivery
    mp.add(OutboundMessage(
      destination: destAgent,
      type: MessageType.assignment,
      payload: payload,
    ));

    // A reader-name value pends until acknowledged (§app:requests-acks).
    if (!isWriter && gnIndex > 0) {
      _pendingReaderValues[(gnAgent, gnIndex)] = (
        payload: payload,
        destination: destAgent,
        nestedReaderNames:
            globalizeResult.spawns.map((s) => s.globalName).toList(),
      );
      _trace('[MAD $agentId] send: value ${globalName} pending until ack');
    }

    _trace('[MAD $agentId] send: queued message to $destAgent, mp.totalLength=${mp.totalLength}');
    _flushReports();
  }
}
