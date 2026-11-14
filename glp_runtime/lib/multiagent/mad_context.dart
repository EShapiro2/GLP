/// madGLP Agent Context
///
/// Provides agent-level context for multiagent GLP communication.
/// Each agent has W_p (global writers table) and M_p (message queue).
///
/// Specification: IGLP app:madglp-spec (madGLP Specification).
library;

import 'dart:async';

import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/body_kernels.dart'
    show deepDeref, networkKey, publicAddressKey, reachableKey;
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;
import 'package:glp_runtime/runtime/saved_state.dart';
import 'package:glp_runtime/runtime/seam_declarations.dart';
import 'package:glp_runtime/multiagent/message_queue.dart';
import 'package:glp_runtime/multiagent/global_send.dart';
import 'package:glp_runtime/multiagent/global_writers_table.dart';
import 'package:glp_runtime/multiagent/imported_writer_records.dart';
import 'package:glp_runtime/multiagent/mad_helpers.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/multiagent/simulation_network.dart'
    show SimulationNetworkClient;
import 'package:glp_runtime/wire/codec.dart'
    show
        wireMsgKindValue,
        wireMsgKindRequest,
        wireMsgKindAcknowledgement,
        wireMsgKindGone;
import 'package:glp_runtime/wire/artefact.dart' show ModuleRefusal;
import 'package:glp_runtime/wire/payload_codec.dart'
    show GlobalNameRefusal, PayloadCodec;

/// Callback for delivering messages to other agents: it hands [message] to the layer and returns the layer's answer, a future that completes once the layer has saved the message for delivery and fails where the layer refuses it, being unable to save it (GNA, Section 2.9: send "completes once the message is saved for delivery ..., and fails if the message cannot be saved").  A callback that returns no future is taken as accepting the message.
typedef MessageDeliveryCallback = FutureOr<void> Function(
    String destination, OutboundMessage message);

/// A pending reader-name value ([MadContext._pendingReaderValues]):
/// [holderEpoch] is the epoch this end records of the holder, from the first
/// request or acknowledgement from it, null until one arrives.
typedef _PendingReaderValue = ({
  List<int> payload,
  String destination,
  List<GlobalName> nestedReaderNames,
  int? holderEpoch
});

/// madGLP Agent Context
///
/// Wraps a GlpRuntime with W_p and M_p for multiagent operation.
/// Each agent has its own MadContext with unique agentId.
class MadContext {
  /// Unique identifier for this agent (e.g., "alice", "bob")
  final String agentId;

  /// The epoch e_p of this agent (IGLP db0a117, Definition "Epoch": "an
  /// integer e_p >= 1 of its local state, 1 at the first start of p"); a
  /// restart after a crash opens the next ([crashRestart]).  Every global
  /// name the agent allocates carries it, and every message it sends.
  int get epoch => _epoch;
  int _epoch = 1;

  /// The known epochs E_p: "a partial function from agents to epochs"
  /// (Definition "madGLP Local State"), the epoch each message from an agent
  /// carried last, "which is never below the epoch E_q(p) held before"
  /// ([learnEpoch]).
  final Map<String, int> _knownEpochs = {};

  /// The epoch known of [agent], or null where no message from it has been
  /// applied.
  int? knownEpoch(String agent) => _knownEpochs[agent];

  /// The link ends this agent retired, by the name of each (Definition
  /// "Retire"): at a restart after a crash, on learning a peer's new epoch,
  /// and on a gone.  Traffic for one is answered gone(G), "it reaches an end
  /// the agent lost" (IGLP db0a117, "Requests and Acknowledgements"), where
  /// traffic for a name the agent never held an end of, or holds one of
  /// still, is early or applied.
  final Set<GlobalName> _retiredEnds = {};

  /// Whether this agent retired its end of the link named [name].
  bool hasRetired(GlobalName name) => _retiredEnds.contains(name);

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

  /// The seam's resolution of a destination to the one agent it names, set
  /// by a simulation's binding: "a destination, in a global name or in a
  /// message, is a key or a readable name standing for one, and the seam
  /// resolves both to the one agent" (IGLP 4f84612, Implementation Notes,
  /// "Simulation realisation").  [send] resolves the agent of the global name
  /// and the destination by it before Globalize, so that the link records,
  /// the message queue and the held-link test each name the agent once,
  /// however the program named it.  A name it does not resolve is kept as
  /// the program gave it.  Over a given network it is not set: there the
  /// destination is the key ([AgentRuntime]).
  String? Function(String name)? resolveAgent;

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

  /// Where the layer's place events go, where set: to the host, which
  /// delivers each in an event cycle of its own by [deliverPlaceEvent], after
  /// the cycles before it (IGLP, Implementation Notes, "Event-driven
  /// execution").  [declaration] is the declaration a
  /// refusal answers, null for an event; it is checked when the event is
  /// delivered.  Where it is not set, an event is delivered as it arrives.
  void Function(String place, PlaceEvent event, int? declaration)?
      placeEventSink;

  /// Where the presences of a nearby declaration go, where set: to the host,
  /// which delivers each in an event cycle of its own by [deliverPresence],
  /// as it does a place event ([placeEventSink]).  [declaration] is the
  /// nearby declaration on [underlay] that stood when the layer reported it,
  /// and is checked when the presence is delivered.  Where it is not set, a
  /// presence is delivered as the layer reports it.
  void Function(ProximityUnderlay underlay, Term presence, int declaration)?
      presenceSink;

  /// The number of presences delivered onto a nearby stream, so that a host
  /// delivering them as they arrive can tell that a run is to follow.
  int presencesDelivered = 0;

  /// Where the reaches of the reachable declaration go, where set: to the
  /// host, which delivers each in an event cycle of its own by
  /// [deliverReach], as it does a presence ([presenceSink]).  [declaration] is
  /// the reachable declaration that stood when the layer reported the
  /// session, and is checked when the reach is delivered.  Where it is not
  /// set, a reach is delivered as the layer reports it.
  void Function(Term reach, int declaration)? reachSink;

  /// Where set, the host's: a change of the agent's own public address the
  /// layer reports, `onConnectivityStatusChanged`, delivered as an event of
  /// the agent's own, whose cycle hands it to [deliverPublicAddress], as it
  /// does a reach ([reachSink]).  [declaration] is the public-address
  /// declaration that stood when the layer reported the change.  Where it is
  /// not set, the change is delivered as the layer reports it.
  void Function(Term address, int declaration)? publicAddressSink;

  /// Where set, the host's: a change of the local network the layer reports,
  /// `onNetworkChanged`, delivered as an event of the agent's own, whose
  /// cycle hands it to [deliverNetwork], as [publicAddressSink].
  void Function(Term network, int declaration)? networkSink;

  /// The number of reaches delivered onto the reachable stream, so that a
  /// host delivering them as they arrive can tell that a run is to follow.
  int reachesDelivered = 0;

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
  final Map<GlobalName, ({Term value, String fromAgent})>
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
  final Map<GlobalName, _PendingReaderValue> _pendingReaderValues = {};

  /// Reader names this agent forwarded on (Definition Globalize, forwarding
  /// case). After forwarding, the no-entry condition on an arriving value for
  /// the name means stale, not early: such arrivals are dropped, never held.
  /// Indices are never reused, so the record never misclassifies.
  final Set<GlobalName> _forwardedReaderNames = {};

  /// Reader names whose value this agent applied, removing the entry (madGLP
  /// Receive, Reader case: "The entry matching (p, i) is removed").  A value
  /// arriving for such a name afterwards is a repeat, and is dropped: "a value
  /// finds its entry removed" (Remark Fair Message Delivery), "A value ...
  /// matching no entry and no pending value is dropped" (§app:requests-acks).
  /// It is not early — an early assignment is one arriving "before the message
  /// whose localization creates its global writers table entry" (IGLP
  /// app:in-networking, Early messages) — and is not held.  Indices are never
  /// reused, so the record never misclassifies.
  final Set<GlobalName> _appliedReaderNames = {};

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
  final Map<GlobalName, ({Term value, String fromAgent})> _heldAssignments =
      {};

  /// Held incoming requests (Definition Held Link, case 3): every request
  /// reaching the anchor is held — the agent the anchor exported the name to
  /// sends none, so a request always means the name was forwarded. The
  /// requesters are retained so authorisation can record each among the
  /// link's holders.  Per link, in order of arrival: the first is the one
  /// reported, and "a request from a further requester --- the reader
  /// forwarded again --- is held and reported in its turn, the anchor's
  /// program asked once for each party that comes to hold the end" (§Held
  /// Links), its turn coming when the program has answered the one before.
  /// Each held request is its requester with the epoch its request carried,
  /// which the goal or the pending value records of the holder once the
  /// request is authorised (IGLP db0a117: "the epoch carried by the first
  /// request, acknowledgement or value from the holder").
  final Map<GlobalName, List<(String, int)>> _heldRequests = {};

  /// The (link, requester) pairs the anchor has recorded, by link: a requester
  /// is recorded when its request is held.  "A request repeating a (link,
  /// requester) pair already recorded --- the link held, authorised or closed
  /// --- is dropped" (§app:requests-acks).  A link's pairs are released when
  /// the link closes, after which every request for it is dropped as one "for
  /// a closed link" (madGLP Receive, Request case).
  final Map<GlobalName, Set<String>> _requestPairs = {};

  /// Links reported and not yet answered, newest report per link. Keyed as
  /// `(anchor, index, isWriter)`; the value is the agent the name or traffic
  /// came from — the `S` of the `pending_link(G, S)` report.
  final Map<GlobalName, String> _reportedLinks = {};

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

  /// The layer's answers still to come to the messages [flushMessages] handed it.
  final Set<Future<void>> _unanswered = {};

  /// Hand every sendable message of M_p to the layer by [onMessageReady], the Send transactions (Definition madGLP Send); the number handed over.  The runtime waits for the layer's answer to each ([answered]): "the layer refuses to send a message it cannot save, and the runtime waits for its answer and, on a refusal, keeps the message and sends it again later" (IGLP b014a9e, Implementation Notes, "Networking Seam", The contract).  A refused message is put back in M_p undelivered, and the next flush hands it over again, after the messages it took this time.
  int flushMessages() {
    if (onMessageReady == null) return 0;

    int count = 0;
    final refused = <OutboundMessage>[];
    final destinations = List<String>.from(mp.destinations);
    _trace('[MAD $agentId] flushMessages: mp.totalLength=${mp.totalLength}, destinations=$destinations');

    for (final dest in destinations) {
      while (true) {
        final msg = mp.poll(dest);
        if (msg == null) break;
        _trace('[MAD $agentId] flushMessages: sending ${msg.type} to $dest');
        count++;
        final FutureOr<void> answer;
        try {
          answer = onMessageReady!(dest, msg);
        } catch (e) {
          _trace('[MAD $agentId] flushMessages: the layer refused the '
              '${msg.type} to $dest: $e; it is kept');
          refused.add(msg);
          continue;
        }
        if (answer is Future<void>) {
          late final Future<void> waiting;
          waiting = answer.then((_) {}, onError: (Object e) {
            _trace('[MAD $agentId] flushMessages: the layer refused the '
                '${msg.type} to $dest: $e; it is kept');
            mp.add(msg);
          }).whenComplete(() => _unanswered.remove(waiting));
          _unanswered.add(waiting);
        }
      }
    }
    refused.forEach(mp.add);
    _trace('[MAD $agentId] flushMessages: flushed $count messages');
    return count;
  }

  /// Completes once the layer has answered every message handed to it by [flushMessages], each refused one being back in M_p by then.
  Future<void> answered() async {
    while (_unanswered.isNotEmpty) {
      await Future.wait(List.of(_unanswered));
    }
  }

  // =========================================================================
  // madGLP global_send Mechanism
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
      localEpoch: _epoch,
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

    // Queue the assignment message, in this agent's epoch
    final payload = PayloadCodec.createGlobalSendPayload(
        result.globalName, globalizedValue,
        senderEpoch: _epoch);

    mp.add(OutboundMessage(
      destination: result.destination,
      type: MessageType.assignment,
      payload: payload,
      link: result.globalName,
      held: holdValue,
    ));
    if (holdValue) {
      _reportPendingLink(result.globalName, record.sender);
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
        link: result.globalName,
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
      _pendingReaderValues[result.globalName] = (
        payload: payload,
        destination: result.destination,
        nestedReaderNames: result.globalizeResult.spawns
            .map((s) => s.globalName)
            .toList(),
        holderEpoch: result.otherEpoch,
      );
      _trace('[MAD $agentId] value ${result.globalName} pending until ack');
    }

    // Register any new goals spawned for nested variables
    // Must set up both registry entry AND onBind callback (same as registerGlobalSendSpawns)
    for (final newGoal in result.newGoals) {
      _trace('[MAD $agentId] global_send spawned new goal: ${newGoal.globalName}');
      globalSendRegistry.register(newGoal);
      _watch(newGoal.readerAddr);
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
  /// Per IGLP Definition global_send Predicate: global_send(T, G, Q) fires
  /// when T (the reader) becomes
  /// known, i.e., when the paired writer is bound. We register both the goal in
  /// the GlobalSendRegistry and an onBind callback on the heap so that
  /// onWriterBound is called when the writer is assigned.
  void registerGlobalSendSpawns(List<GlobalSendSpawn> spawns) {
    for (final spawn in spawns) {
      globalSendRegistry.register(GlobalSendGoal.fromSpawn(spawn));
      _watch(spawn.readerAddr);
      _trace('[MAD $agentId] registered global_send goal: ${spawn.globalName} -> ${spawn.destAgent}');
    }
  }

  /// The heap callback of the `global_send` goal registered under [key]: when
  /// the writer is assigned, [onWriterBound] fires the goal.  Tagged with
  /// [key], from which [restoreState] makes it again for a restarted agent.
  void _watch(HeapCell key) =>
      runtime.heap.onBind(key, (Term value) => onWriterBound(key, value),
          tag: key);

  /// Record the reader names a globalization forwarded (Definition Globalize,
  /// case 3): from now on a value arriving for such a name is stale
  /// at this agent and is dropped, not held.
  void _recordForwardedNames(GlobalizeResult result) {
    for (final gn in result.forwardedNames) {
      _forwardedReaderNames.add(gn);
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
      _trace('[MAD $agentId] forwarded imported writer as ${rec.name} — '
          'record and goal removed, link left');
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
        payload: PayloadCodec.createRequestPayload(gn, senderEpoch: _epoch),
        link: gn,
        held: true,
      ));
      _trace('[MAD $agentId] queued req($gn) to ${gn.agent} — HELD');
      _reportPendingLink(gn, fromAgent);
    }
  }

  // =========================================================================
  // madGLP Receive Transaction
  // =========================================================================

  /// Handle incoming madGLP assignment message
  ///
  /// Per IGLP Definition madGLP Receive Transaction, handles three cases:
  ///
  /// **Case `_w(p, 0) := [T | _w(p,0)]` (Serializer)**: Cold-call to our network input.
  /// **Case `_w(p, i) := T` with i > 0**: We globalized writer Y, find entry (Y, q) at index i.
  /// **Case `_r(p, i) := T`**: We localized _r(p,i), search for entry (Z_q, p, i).
  ///
  /// Both non-serializer cases: localize T, bind writer, register spawned goals, remove entry.
  /// Serializer case: extend network input stream, update entry (don't remove).
  ///
  /// [serial] is the serial a serializer message carries, which its sender
  /// allocated in its epoch [senderEpoch] (Definition "madGLP Send
  /// Transaction", Serializer case: "under serial i of p's epoch e"); a
  /// serializer message has one and no other message does (IGLP Code Format,
  /// "Messages").  [fromAgent] is the sender the layer authenticates, and
  /// [senderEpoch] the epoch the message carries, the sender's when it sent
  /// it; a message handed here by a host and not by [handleIncomingPayload]
  /// is taken as of epoch 1.
  void handleMadAssignment({
    required GlobalName globalName,
    required Term value,
    required String fromAgent,
    int? serial,
    int senderEpoch = 1,
  }) {
    _trace('[MAD $agentId] handleMadAssignment: $globalName := $value from '
        '$fromAgent in epoch $senderEpoch'
        '${serial == null ? '' : ' under serial $serial'}');

    final isSerializer = globalName.isSerializer;
    if (isSerializer != (serial != null)) {
      throw ArgumentError(isSerializer
          ? 'a serializer message $globalName := ... carries a serial'
          : 'only a serializer message carries a serial, not $globalName');
    }

    if (isSerializer) {
      // Case _w(p, 0) := [T | _w(p,0)] - serializer (cold-call to network input)
      _handleSerializerAssignment(value, fromAgent, senderEpoch, serial!);
    } else if (globalName.isWriter) {
      // Case _w(p, i) := T with i > 0 - we globalized this writer, direct lookup
      _handleWriterAssignment(globalName, value, fromAgent,
          senderEpoch: senderEpoch);
    } else {
      // Case _r(p, i) := T - we localized this, search for entry
      _handleReaderAssignment(globalName, value, fromAgent);
    }
  }

  /// Handle serializer assignment _w(p, 0) := [T | _w(p,0)]
  ///
  /// This is a cold-call to our network input stream, under [serial] of the
  /// epoch [senderEpoch] of [fromAgent], the sender the layer authenticates.
  /// IGLP db0a117, Definition madGLP Send Transaction, Serializer case: "q
  /// holds the entry (N_q, D_q) at index 0 in W_q.  If (p, e, i) ∈ D_q, m is a
  /// repeat: nothing changes at q.  Otherwise let N'_q be a fresh local
  /// writer, σ̂ = {N_q := [T_q↓ | N'_q?]} ... The entry at index 0 in W'_q
  /// becomes (N'_q, D_q ∪ {(p, e, i)})."  A serializer message of an earlier
  /// epoch of the sender is a repeat only by its (sender, epoch, serial): the
  /// sender's serials begin again in each epoch.
  ///
  /// A repeat is dropped before anything is localized, so it creates no entry,
  /// record, goal, request or report.
  void _handleSerializerAssignment(
      Term value, String fromAgent, int senderEpoch, int serial) {
    _trace('[MAD $agentId] _handleSerializerAssignment: cold-call from '
        '$fromAgent under serial $serial of epoch $senderEpoch');

    // Get current serializer writer from index-0 entry
    final currentWriter = wp.serializerWriterAddr;
    if (currentWriter == null) {
      throw StateError('Serializer entry not initialized at index 0');
    }
    _trace('[MAD $agentId] _handleSerializerAssignment: current serializer writer=$currentWriter');

    if (wp.serializerHasAppended(fromAgent, senderEpoch, serial)) {
      _trace('[MAD $agentId] _handleSerializerAssignment: ($fromAgent, '
          '$senderEpoch, $serial) appended before — a repeat, dropped');
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
    // Per IGLP, madGLP Receive Transaction: "Localize T↑ by q to get T_q↓"
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
    wp.recordSerializerAppend(fromAgent, senderEpoch, serial);
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

    // Build the list cell [content | freshReader] and extend the stream,
    // reactivating the goals suspended on it
    final listCell = StructTerm('.', [content, VarRef(freshReader)]);
    final activations = _bindAndReactivate(currentWriter, listCell);
    _trace('[MAD $agentId] _appendToNetworkInput: bound stream, $activations activations');

    // Update the serializer entry to point to the fresh writer (permanent entry)
    wp.updateSerializerWriter(freshWriter);
    _trace('[MAD $agentId] _appendToNetworkInput: updated serializer entry to writer=$freshWriter');
  }

  /// Bind [writer] to [value] and reactivate the goals the binding wakes; the
  /// number of them.
  int _bindAndReactivate(HeapCell writer, Term value) {
    final activations = runtime.heap.bindVariable(writer, value);
    for (final act in activations) {
      runtime.enqueueReactivatedGoal(act);
    }
    return activations.length;
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

  /// Declare [place] with radius [radiusMetres], its event stream growing from
  /// [streamWriterAddr] — the writer of the `E` of `declare(place(P, R, E))`.
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

    _askLayerForPlace(network, place, null, radiusMetres, declaration);
  }

  /// Give [network] the place [place] of radius [radiusMetres] about
  /// [centre] --- the one the store kept for it, at a restart; null at the
  /// program's declaration, the device's location at the time of the call
  /// --- for the standing declaration of number [declaration]; keep the centre
  /// the layer answers with ([keepPlaceCentre]), and put a refusal on that
  /// declaration's stream.
  void _askLayerForPlace(GlpNetwork network, String place,
      PlaceCentre? centre, double radiusMetres, int declaration) {
    // The layer answers asynchronously; the stream exists either way. On a
    // refusal E receives `unobservable`, the declaration stands, and
    // `observable` follows if the layer later begins reporting (Definition Seam
    // Predicates). A refusal is the layer watching nothing, which is what
    // `unobservable` says; the stream is not closed, since a refusal is neither
    // of the two closing cases.
    network.declarePlace(place, centre, radiusMetres).then(
      (registered) {
        // Answers can arrive after remove(place(P)) or after a superseding
        // declaration, whose stream is the successor's, not this one's: the
        // declaration answered goes with the answer, and is checked when it
        // is delivered or kept.
        if (registered == null) {
          _trace('[MAD $agentId] declarePlace($place): refused by the '
              'platform — unobservable');
          _placeEvent(place, PlaceEvent.unobservable, declaration);
          return;
        }
        _trace('[MAD $agentId] declarePlace($place): registered about '
            '$registered');
        final sink = placeCentreSink;
        if (sink != null) {
          sink(place, registered, declaration);
        } else {
          keepPlaceCentre(place, registered, declaration);
        }
      },
      onError: (Object e) => _trace(
          '[MAD $agentId] declarePlace($place): layer failed: $e'),
    );
  }

  /// Where set, the host's: the centre the layer registered a place about,
  /// in answer to the declaration of the given number, delivered as an event
  /// of the agent's own, whose cycle keeps it with the place's standing
  /// declaration and saves the state ([AgentRuntime]); where null, it is kept
  /// at once.
  void Function(String place, PlaceCentre centre, int declaration)?
      placeCentreSink;

  /// Keep [centre], the one the layer registered [place] about in answer to
  /// its declaration of number [declaration], with the place's standing
  /// declaration, so that a restart declares the place again about it (IGLP
  /// 96cd44a, Implementation Notes, "Persistence": the store keeps "the
  /// places with the centre each was declared about").  Dropped where that
  /// declaration no longer stands, the place removed or declared again
  /// since.  Returns whether the record changed.
  bool keepPlaceCentre(String place, PlaceCentre centre, int declaration) {
    if (!runtime.placeStreams.isStanding(place, declaration)) return false;
    return runtime.seamDeclarations
        .placeCentre(place, centre.latitude, centre.longitude);
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
    network.onPlaceEvent = (place, event) => _placeEvent(place, event, null);
  }

  /// A place event the layer reports, or a refusal answering [declaration]:
  /// to the host's [placeEventSink] where it is set, delivered at once where
  /// it is not.
  void _placeEvent(String place, PlaceEvent event, int? declaration) {
    final sink = placeEventSink;
    if (sink != null) {
      sink(place, event, declaration);
    } else {
      deliverPlaceEvent(place, event, declaration: declaration);
    }
  }

  /// Make to the layer every declaration the restored state holds standing,
  /// in the order they were made, before any goal of the agent is reduced:
  /// "The layer starts empty at every launch, and at a restart the runtime
  /// makes every declaration the store holds to it again before any goal of
  /// the agent is reduced" (IGLP 0c77e47, Implementation Notes,
  /// "Persistence"); the program does not declare again.  A peer, an
  /// address, a rendezvous server and a trust level go to the layer function
  /// behind each; a place goes to [GlpNetwork.declarePlace] for its standing
  /// declaration, about the centre the store kept for it, where the layer
  /// had answered with one before the state was saved, and about the device's
  /// location otherwise ("each place about the centre the store holds for
  /// it", IGLP 96cd44a), its stream a refusal is put on, as [declarePlace]
  /// does.
  /// A nearby declaration calls no layer function: its stream is fed by what
  /// the new layer reports; a public-address or network declaration calls
  /// none either, and its standing stream is given the new layer's present
  /// value (the GLP paper f265fec, "Networking seam").  A layer function the layer does not have is
  /// traced and the rest are made.  Does nothing where no layer is bound.
  /// Returns the declarations made.
  List<StandingDeclaration> declareStandingToLayer() {
    final network = this.network;
    if (network == null) return const [];
    final made = <StandingDeclaration>[];
    for (final d in runtime.seamDeclarations.standing.toList()) {
      final a = d.args;
      try {
        switch (d.kind) {
          case SeamDeclarationKind.peer:
            network.putKnownPeer(PubKey.fromHex(a[0]));
          case SeamDeclarationKind.address:
            network.putPeerAddress(PubKey.fromHex(a[0]), a[1]);
          case SeamDeclarationKind.rendezvous:
            network.putRendezvousServer(PubKey.fromHex(a[0]), a[1]);
          case SeamDeclarationKind.trust:
            network.setTrustLevel(ProximityUnderlay.values.byName(a[0]),
                TrustLevel.values.byName(a[1]));
          case SeamDeclarationKind.place:
            final declaration = runtime.placeStreams.standing(a[0]);
            if (declaration == null) continue;
            _installPlaceEventHandler(network);
            final centre = a.length >= 4
                ? PlaceCentre(
                    latitude: double.parse(a[2]),
                    longitude: double.parse(a[3]))
                : null;
            _askLayerForPlace(
                network, a[0], centre, double.parse(a[1]), declaration);
          case SeamDeclarationKind.publicAddress:
            // No layer function: the standing stream, restored with the
            // state, is given the new layer's present address, as a
            // declaration begins (the GLP paper f265fec, "Networking seam").
            if (runtime.publicAddressStreams.standing(publicAddressKey) ==
                null) {
              continue;
            }
            runtime.publicAddressStreams.deliver(
                publicAddressKey, addressTerm(presentPublicAddress()));
          case SeamDeclarationKind.network:
            if (runtime.networkStreams.standing(networkKey) == null) continue;
            runtime.networkStreams
                .deliver(networkKey, networkTerm(presentNetwork()));
        }
        made.add(d);
        _trace('[MAD $agentId] restart: $d made to the layer again');
      } on UnsupportedError catch (e) {
        _trace('[MAD $agentId] restart: $d not made to the layer: '
            '${e.message}');
      }
    }
    return made;
  }

  /// Route the layer's place events onto the streams of the places a
  /// restarted agent declared, as [declarePlace] routes them.
  void resumePlaceEvents() {
    final network = this.network;
    if (network == null || runtime.placeStreams.places.isEmpty) return;
    _installPlaceEventHandler(network);
  }

  GlpNetwork? _placeEventHandlerNetwork;

  /// What the layer reported with `onPeerConnected(pk, underlay,
  /// application?)` (GNA api.tex, Section 2.3), by peer key hex: for each
  /// underlay over which the peer is connected and not since reported
  /// disconnected, the attested application, null for an unattested session.
  /// Each map is in order of report, the latest last.  Backs `peer_app/2`
  /// (GLP-Spec appendix-guards, "Networking seam", 5fab8ac; IGLP, Definition
  /// "Seam Predicates", 4a334c2).  A session is the layer's and is proved
  /// again after a restart ("A peer that restarts proves itself again before
  /// it is reachable", GNA, Section 2.6), so the record is not part of the
  /// agent's saved state.
  final Map<String, Map<Underlay, Application?>> _peerSessions = {};

  /// The layer reported [pk] connected over [underlay], with the attested
  /// [application], null where the session is unattested.  Called by the host
  /// that binds the network, from the layer's `onPeerConnected`.
  void peerConnected(PubKey pk, Underlay underlay, Application? application) {
    final sessions = _peerSessions.putIfAbsent(pk.hex, () => {});
    sessions.remove(underlay); // the latest report last
    sessions[underlay] = application;
    if (underlay == Underlay.wan && !_wanSessions.containsKey(pk.hex)) {
      _wanSessions[pk.hex] = pk;
      _reach(_presenceTerm('connected', pk));
    }
  }

  /// The layer reported [pk] no longer reachable over [underlay].  Called by
  /// the host that binds the network, from the layer's `onPeerDisconnected`.
  void peerDisconnected(PubKey pk, Underlay underlay) {
    if (underlay == Underlay.wan && _wanSessions.remove(pk.hex) != null) {
      _reach(_presenceTerm('lost', pk));
    }
    final sessions = _peerSessions[pk.hex];
    if (sessions == null) return;
    sessions.remove(underlay);
    if (sessions.isEmpty) _peerSessions.remove(pk.hex);
  }

  // ===========================================================================
  // AGENTS NEARBY (Definition Seam Predicates: nearby(U, S))
  // ===========================================================================
  //
  // GLP-Spec 210cee1, appendix-guards, "Networking seam": "nearby(U, S)
  // assigns S a stream of the agents nearby over the proximity underlay U:
  // seen(K) when the layer discovers on U an agent of key K that it
  // recognises or, under open trust, contacts, and gone(K) when that agent can
  // no longer be reached over U.  A further declaration on the same underlay
  // supersedes the earlier one and closes its stream, and remove(nearby(U))
  // ends the declaration and closes its stream."  IGLP b20951d, after
  // Definition "Seam Predicates": a presence is bridged below the madGLP
  // layer, the stream fed serializer-fashion and closed as a place's.
  //
  // The layer discovers a nearby agent, recognised or, under open trust,
  // contacted, by onPeerDiscovered(pk) on that underlay's sub-object, and
  // reports it no longer discovered there, its presence no longer observed
  // and no session held on the underlay, by onPeerLost(pk) (GNA cfbfeac,
  // proximity.tex, "Concepts and Principles", Discovery; system-predicates
  // .tex: "onPeerDiscovered and onPeerLost behind the stream of
  // declare(nearby(Underlay, Events))").  A session's connection and
  // disconnection are peer_app/2's, and no presence: until 2026-11-08 a
  // connection over a proximity underlay was a seen, and a disconnection a
  // gone.  The runtime keeps, per proximity underlay, the agents so reported
  // nearby and not since lost, so a declaration is given seen for every
  // agent nearby when it is made.  What the layer reports is the layer's and is reported again after
  // a restart, so the record is not part of the agent's saved state; the
  // declarations and their streams are ([GlpRuntime.nearbyStreams]).

  /// The agents the layer has reported nearby over each proximity underlay
  /// and not since gone, by key hex, in order of report.
  final Map<ProximityUnderlay, Map<String, PubKey>> _nearby = {
    for (final u in ProximityUnderlay.values) u: <String, PubKey>{},
  };

  /// The layer reported [pk] discovered over the proximity underlay
  /// [underlay], `onPeerDiscovered(pk)` on its sub-object.  Called by the host
  /// that binds the network.
  void peerDiscovered(PubKey pk, ProximityUnderlay underlay) =>
      _appeared(pk, underlay);

  /// The layer reported [pk] lost over the proximity underlay [underlay],
  /// `onPeerLost(pk)` on its sub-object.  Called by the host that binds the
  /// network.
  void peerLost(PubKey pk, ProximityUnderlay underlay) =>
      _vanished(pk, underlay);

  /// Whether the layer has reported [pk] nearby over [underlay] and not since
  /// gone.
  bool isNearby(PubKey pk, ProximityUnderlay underlay) =>
      _nearby[underlay]!.containsKey(pk.hex);

  void _appeared(PubKey pk, ProximityUnderlay underlay) {
    final nearby = _nearby[underlay]!;
    if (nearby.containsKey(pk.hex)) return; // already seen and not gone
    nearby[pk.hex] = pk;
    _presence(underlay, _presenceTerm('seen', pk));
  }

  void _vanished(PubKey pk, ProximityUnderlay underlay) {
    if (_nearby[underlay]!.remove(pk.hex) == null) return; // never seen
    _presence(underlay, _presenceTerm('gone', pk));
  }

  /// `seen(K)`, `gone(K)`, `connected(K)` or `lost(K)`, K the key of [pk] as a program names the agent:
  /// its key, 64 lowercase hex characters, over a real network, and in a
  /// simulation the readable name standing for it, as the seam names the
  /// sender of a message (IGLP, Remark "Agent Names"; Implementation Notes,
  /// "Simulation realisation": "a readable name stands for a key").
  Term _presenceTerm(String functor, PubKey pk) {
    final network = this.network;
    final name = network is SimulationNetworkClient
        ? network.directory.idOf(pk) ?? pk.hex
        : pk.hex;
    return StructTerm(functor, [ConstTerm(name)]);
  }

  /// A presence the layer reported on [underlay]: to the host's
  /// [presenceSink] where it is set, delivered at once where it is not; where
  /// no nearby declaration on [underlay] stands, nothing.
  void _presence(ProximityUnderlay underlay, Term presence) {
    final declaration = runtime.nearbyStreams.standing(underlay.name);
    if (declaration == null) return;
    final sink = presenceSink;
    if (sink != null) {
      sink(underlay, presence, declaration);
    } else {
      deliverPresence(underlay, presence, declaration: declaration);
    }
  }

  /// Declare a watch on the agents nearby over [underlay], its stream growing
  /// from [streamWriterAddr], the writer of the `S` of
  /// `declare(nearby(U, S))`; a declaration on [underlay] already standing is
  /// superseded and its stream closed.  The stream is given `seen(K)` at once
  /// for every agent the layer has reported nearby over [underlay] and not
  /// since gone.
  void declareNearby(ProximityUnderlay underlay, HeapCell streamWriterAddr) {
    runtime.nearbyStreams.declare(underlay.name, streamWriterAddr);
    _trace('[MAD $agentId] declare nearby over ${underlay.name}: stream '
        'writer=$streamWriterAddr');
    for (final pk in _nearby[underlay]!.values) {
      runtime.nearbyStreams.deliver(underlay.name, _presenceTerm('seen', pk));
    }
  }

  /// Deliver [presence] on [underlay]'s nearby stream, serializer-fashion; a
  /// presence answering a [declaration] that no longer stands, removed or
  /// superseded, is dropped.
  void deliverPresence(ProximityUnderlay underlay, Term presence,
      {required int declaration}) {
    if (!runtime.nearbyStreams.isStanding(underlay.name, declaration)) {
      _trace('[MAD $agentId] presence $presence over ${underlay.name} for a '
          'nearby declaration that no longer stands --- dropped');
      return;
    }
    runtime.nearbyStreams.deliver(underlay.name, presence);
    presencesDelivered++;
    _trace('[MAD $agentId] nearby over ${underlay.name}: $presence');
  }

  // ===========================================================================
  // AGENTS REACHABLE (Definition Seam Predicates: reachable(S))
  // ===========================================================================
  //
  // GLP-Spec cb9df43, appendix-guards, "Networking seam": "reachable(S)
  // assigns S a stream of the agents the networking layer connects to over
  // the WAN: connected(K) when the layer has a session with the agent of key
  // K over the WAN, beginning with every agent it has one with when it is
  // declared, and lost(K) when that session ends.  A further declaration
  // supersedes the earlier one and closes its stream, and remove(reachable)
  // ends the declaration and closes its stream."  IGLP 0c77e47, after
  // Definition "Seam Predicates": a reach is bridged below the madGLP layer,
  // the stream fed serializer-fashion and closed as a place's.
  //
  // The layer reports a session over the WAN by onPeerConnected(pk, wan,
  // application?) and its end by onPeerDisconnected(pk, wan) (GNA api.tex,
  // Section 2.3).  The runtime keeps the agents with which the layer has a
  // session over the WAN, so a declaration is given connected for every one
  // when it is made.  What the layer reports is the layer's and is reported
  // again after a restart, so the record is not part of the agent's saved
  // state; the declaration and its stream are ([GlpRuntime.reachableStreams]).
  // In the simulation the router reports the PAN throughout, so the stream
  // carries nothing (IGLP 0c77e47, "Simulation realisation").

  /// The agents with which the layer has a session over the WAN, as
  /// reported and not since ended, by key hex, in order of report.
  final Map<String, PubKey> _wanSessions = {};

  /// A reach the layer reported: to the host's [reachSink] where it is set,
  /// delivered at once where it is not; where no reachable declaration
  /// stands, nothing.
  void _reach(Term reach) {
    final declaration = runtime.reachableStreams.standing(reachableKey);
    if (declaration == null) return;
    final sink = reachSink;
    if (sink != null) {
      sink(reach, declaration);
    } else {
      deliverReach(reach, declaration: declaration);
    }
  }

  /// Declare a watch on the agents reachable over the WAN, its stream growing
  /// from [streamWriterAddr], the writer of the `S` of
  /// `declare(reachable(S))`; a declaration already standing is superseded
  /// and its stream closed.  The stream is given `connected(K)` at once for
  /// every agent with which the layer has a session over the WAN.
  void declareReachable(HeapCell streamWriterAddr) {
    runtime.reachableStreams.declare(reachableKey, streamWriterAddr);
    _trace('[MAD $agentId] declare reachable: stream writer=$streamWriterAddr');
    for (final pk in _wanSessions.values) {
      runtime.reachableStreams
          .deliver(reachableKey, _presenceTerm('connected', pk));
    }
  }

  /// Deliver [reach] on the reachable stream, serializer-fashion; a reach
  /// answering a [declaration] that no longer stands, removed or superseded,
  /// is dropped.
  void deliverReach(Term reach, {required int declaration}) {
    if (!runtime.reachableStreams.isStanding(reachableKey, declaration)) {
      _trace('[MAD $agentId] reach $reach for a reachable declaration that '
          'no longer stands --- dropped');
      return;
    }
    runtime.reachableStreams.deliver(reachableKey, reach);
    reachesDelivered++;
    _trace('[MAD $agentId] reachable: $reach');
  }

  // ===========================================================================
  // THE AGENT'S OWN PUBLIC ADDRESS AND LOCAL NETWORK (Definition Seam
  // Predicates: public_address(S), network(S))
  // ===========================================================================
  //
  // The GLP paper f265fec, appendix-guards, "Networking seam": "public_address(S)
  // assigns S a stream of the agent's own public address, beginning with
  // address(A), A the ip:port the layer has learnt from a rendezvous server,
  // or none where it knows none, when it is declared, and then the address at
  // each change, none where it is lost. ... network(S) assigns S a stream of
  // the local network the agent is attached to, beginning with attached(N), N
  // an opaque fingerprint of that network, or none where it is attached to
  // none or the platform makes the fingerprint unreadable, when it is
  // declared, and then the network at each change ... For each, a further
  // declaration supersedes the earlier one and closes its stream, and the
  // removal ends the declaration and closes its stream.  The address and the
  // network change independently of each other."  IGLP 07a2b00, Definition
  // "Seam Predicates": declare(public_address(S?)) and declare(network(S?)).
  //
  // The layer gives the present address by getPublicAddress() and each change
  // by onConnectivityStatusChanged(old, new), the present network by
  // networkIdentity() and each change by onNetworkChanged(old, new) (GNA
  // ip.tex, Connectivity and Address; Local Network Identity); the host
  // routes the two callbacks to [publicAddressChanged] and
  // [networkChanged].  A layer that does not provide a function, the
  // simulation's, gives none (IGLP 07a2b00, "Simulation realisation").  The
  // streams are the runtime's bookkeeping ([GlpRuntime.publicAddressStreams],
  // [GlpRuntime.networkStreams]), fed and closed as a place's; what the layer
  // reports is the layer's, so a restart gives a standing stream the new
  // layer's present value ([declareStandingToLayer]).

  /// `address(A)` for the address [a], or `none` where the layer knows none.
  static Term addressTerm(String? a) =>
      a == null ? ConstTerm('none') : StructTerm('address', [ConstTerm(a)]);

  /// `attached(N)` for the network fingerprint [n], or `none` where the agent
  /// is attached to none or the fingerprint is unreadable.
  static Term networkTerm(String? n) =>
      n == null ? ConstTerm('none') : StructTerm('attached', [ConstTerm(n)]);

  /// The agent's public address as the layer knows it now; null where it
  /// knows none, or does not provide the function.
  String? presentPublicAddress() {
    try {
      return network?.getPublicAddress();
    } on UnsupportedError {
      return null;
    }
  }

  /// The local network the layer is attached to now, by its fingerprint;
  /// null where it is attached to none, or does not provide the function.
  String? presentNetwork() {
    try {
      return network?.networkIdentity();
    } on UnsupportedError {
      return null;
    }
  }

  /// Declare a watch on the agent's own public address, its stream growing
  /// from [streamWriterAddr], the writer of the `S` of
  /// `declare(public_address(S))`; a declaration already standing is
  /// superseded and its stream closed.  The stream is given the present
  /// address at once.
  void declarePublicAddress(HeapCell streamWriterAddr) {
    runtime.publicAddressStreams.declare(publicAddressKey, streamWriterAddr);
    final present = addressTerm(presentPublicAddress());
    _trace('[MAD $agentId] declare public_address: stream '
        'writer=$streamWriterAddr, present $present');
    runtime.publicAddressStreams.deliver(publicAddressKey, present);
  }

  /// Declare a watch on the local network the agent is attached to, its
  /// stream growing from [streamWriterAddr], the writer of the `S` of
  /// `declare(network(S))`; a declaration already standing is superseded and
  /// its stream closed.  The stream is given the present network at once.
  void declareNetwork(HeapCell streamWriterAddr) {
    runtime.networkStreams.declare(networkKey, streamWriterAddr);
    final present = networkTerm(presentNetwork());
    _trace('[MAD $agentId] declare network: stream writer=$streamWriterAddr, '
        'present $present');
    runtime.networkStreams.deliver(networkKey, present);
  }

  /// The layer reported the agent's public address changed to [newAddress],
  /// null where it is lost, `onConnectivityStatusChanged(old, new)`.  Called
  /// by the host that binds the network.
  void publicAddressChanged(String? newAddress) {
    final declaration =
        runtime.publicAddressStreams.standing(publicAddressKey);
    if (declaration == null) return;
    final address = addressTerm(newAddress);
    final sink = publicAddressSink;
    if (sink != null) {
      sink(address, declaration);
    } else {
      deliverPublicAddress(address, declaration: declaration);
    }
  }

  /// The layer reported the local network changed to [newId], null where the
  /// agent is attached to none, `onNetworkChanged(old, new)`.  Called by the
  /// host that binds the network.
  void networkChanged(String? newId) {
    final declaration = runtime.networkStreams.standing(networkKey);
    if (declaration == null) return;
    final net = networkTerm(newId);
    final sink = networkSink;
    if (sink != null) {
      sink(net, declaration);
    } else {
      deliverNetwork(net, declaration: declaration);
    }
  }

  /// Deliver [address] on the public-address stream, serializer-fashion; one
  /// answering a [declaration] that no longer stands, removed or superseded,
  /// is dropped.
  void deliverPublicAddress(Term address, {required int declaration}) {
    if (!runtime.publicAddressStreams
        .isStanding(publicAddressKey, declaration)) {
      _trace('[MAD $agentId] public address $address for a declaration that '
          'no longer stands --- dropped');
      return;
    }
    runtime.publicAddressStreams.deliver(publicAddressKey, address);
    _trace('[MAD $agentId] public_address: $address');
  }

  /// Deliver [net] on the network stream, serializer-fashion; one answering
  /// a [declaration] that no longer stands, removed or superseded, is
  /// dropped.
  void deliverNetwork(Term net, {required int declaration}) {
    if (!runtime.networkStreams.isStanding(networkKey, declaration)) {
      _trace('[MAD $agentId] network $net for a declaration that no longer '
          'stands --- dropped');
      return;
    }
    runtime.networkStreams.deliver(networkKey, net);
    _trace('[MAD $agentId] network: $net');
  }

  /// The layer's session with [pk] as reported, or null where it has none:
  /// its attested application, null where the session is unattested.  Where
  /// the peer is connected over more than one underlay, the application
  /// reported last.
  ({Application? application})? peerSession(PubKey pk) {
    final sessions = _peerSessions[pk.hex];
    if (sessions == null || sessions.isEmpty) return null;
    return (application: sessions.values.last);
  }

  /// Deliver [event] on [place]'s stream, serializer-fashion (Definition Seam
  /// Predicates): bind the current tail writer to `[event | E'?]`, retain the
  /// fresh writer as the new tail, and reactivate whatever suspended on the
  /// stream. One declaration yields one stream however many events follow.
  ///
  /// An event for a place that is not declared here is dropped, and so is a
  /// refusal answering a [declaration] that no longer stands.
  void deliverPlaceEvent(String place, PlaceEvent event, {int? declaration}) {
    if (declaration != null &&
        !runtime.placeStreams.isStanding(place, declaration)) {
      _trace('[MAD $agentId] refusal of $place answers a declaration that '
          'no longer stands — dropped');
      return;
    }
    if (!runtime.placeStreams.deliver(place, ConstTerm(event.name))) {
      _trace('[MAD $agentId] place event $event for undeclared $place — dropped');
      return;
    }
    _trace('[MAD $agentId] place $place: $event');
    onPlaceEventDelivered?.call();
  }

  /// The ground presentation of a global name (IGLP cec4682, "Global
  /// Variable Names"): the 3-ary structures `'_r'(P, E, I)` and `'_w'(P, E,
  /// I)`, functor giving polarity, first argument the anchor, second its
  /// epoch, third the index.  This is the form a program sees — not the wire
  /// form, which stands in the place of a variable.
  static Term groundGlobalName(GlobalName gn) => gn.toStruct();

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
    _reportedLinks[gn] = sender;
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
  bool hasReportedLink(GlobalName link) => _reportedLinks.containsKey(link);

  /// Number of links reported and awaiting an answer.
  int get reportedLinkCount => _reportedLinks.length;

  /// Every link reported and not yet answered, with the sender each was
  /// reported against — the `(G, S)` of its `pending_link` report.
  Iterable<(GlobalName, String)> get reportedLinks =>
      _reportedLinks.entries.map((e) => (e.key, e.value));

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
  /// given.  [senderEpoch] is the epoch the message carried, which the entry
  /// records of the holder ("the epoch carried by the first request,
  /// acknowledgement or value from the holder", IGLP db0a117).
  ///
  /// A value naming an epoch of this agent before its current one is stale
  /// and dropped; one naming an end this agent lost is answered gone
  /// ([_lostEnd]); otherwise a miss is a repeat, the value applied before,
  /// and is dropped (IGLP db0a117, "Requests and Acknowledgements").
  void _handleWriterAssignment(GlobalName globalName, Term value, String fromAgent,
      {bool authorised = false, int? senderEpoch}) {
    if (globalName.epoch < _epoch) {
      _trace('[MAD $agentId] _handleWriterAssignment: $globalName names '
          'epoch ${globalName.epoch}, before this agent\'s $_epoch — stale, '
          'dropped');
      return;
    }
    final entry = wp.lookupByIndex(globalName.index);

    if (entry == null) {
      if (_lostEnd(globalName, fromAgent)) return;
      // Stale drop (spec §app:requests-acks): a value matching no entry is
      // dropped. A writer-name value always finds its fixed anchor entry, so
      // a miss is provably stale — names are never reused.
      _trace('[MAD $agentId] _handleWriterAssignment: no entry for $globalName'
          ' — stale, dropped');
      return;
    }
    if (senderEpoch != null) entry.holderEpoch ??= senderEpoch;

    // An assignment for a link already held is dropped, not held or reported
    // again: "A request from a requester already recorded for the link, and
    // an assignment for a link already holding one, are dropped, so each party
    // is reported once" (§Held Links) — an assignment delivered twice among
    // them (Remark "Fair Message Delivery").
    if (!authorised && _heldAssignments.containsKey(globalName)) {
      _trace('[MAD $agentId] _handleWriterAssignment: $globalName already '
          'held — assignment dropped');
      return;
    }

    // Definition Held Link, case 4: at the anchor, an assignment arriving from
    // an agent it did not export the name to. The test is structural — the
    // entry's `q` against the sender — and consults nothing outside the agent.
    // This is why `q` is kept in the entry `(X, q)`.
    if (entry.remoteAgent != fromAgent && !authorised) {
      _heldAssignments[globalName] = (value: value, fromAgent: fromAgent);
      _reportPendingLink(globalName, fromAgent);
      return;
    }

    final writerAddr = entry.writerAddr;
    _trace('[MAD $agentId] _handleWriterAssignment: globalize-writer entry, writerAddr=$writerAddr');

    // Localize the value: replace global names with local variables
    // Per IGLP, madGLP Receive Transaction: "Localize T↑ by p to get T_p↓"
    final localizedValue =
        _localizeNested(value, fromAgent, '_handleWriterAssignment');

    // Bind the writer with localized value, reactivating suspended goals
    final activations = _bindAndReactivate(writerAddr, localizedValue);
    _trace('[MAD $agentId] _handleWriterAssignment: bound writer, $activations activations');

    // Remove the entry
    wp.removeGlobalizeEntry(globalName.index);
    _trace('[MAD $agentId] _handleWriterAssignment: entry removed');
  }

  /// Handle _r(p, i) := T assignment (we localized _r(p, i))
  ///
  /// We are agent q. We localized _r(p, i), creating LocalizeEntry (Z_q, p, i).
  /// Now p's gs fires and sends _r(p, i) := T to us. Search by (p, i).
  /// Per IGLP, madGLP Receive Transaction, Reader case: "Agent q searches for
  /// entry (X_q, p, i)."
  void _handleReaderAssignment(GlobalName globalName, Term value, String fromAgent) {
    final entry = wp.findByRemote(globalName);
    if (entry == null) {
      // After forwarding, no-entry means stale (spec §app:requests-acks): the
      // reader moved on, and the anchor's redirected value will reach its
      // current holder. Drop — holding is never needed for a forwarded name.
      if (_forwardedReaderNames.contains(globalName)) {
        _trace('[MAD $agentId] _handleReaderAssignment: $globalName was '
            'forwarded — stale value dropped');
        return;
      }
      // A repeat of a value already applied here finds its entry removed, and
      // is dropped (Remark Fair Message Delivery).
      if (_appliedReaderNames.contains(globalName)) {
        _trace('[MAD $agentId] _handleReaderAssignment: $globalName was '
            'applied before — a repeat, dropped');
        return;
      }
      // An end this agent lost: answered gone (IGLP db0a117).
      if (_lostEnd(globalName, fromAgent)) return;
      // Issue 7 / IGLP app:in-networking, Early messages: the assignment arrived before its
      // LocalizeEntry exists (possible under any non-FIFO transport). Hold it,
      // keyed by its name; localize() delivers it when it creates the
      // matching entry. Do not throw — the message is not dropped.
      _heldReaderAssignments[globalName] =
          (value: value, fromAgent: fromAgent);
      _trace('[MAD $agentId] _handleReaderAssignment: no entry for $globalName '
          'yet — held pending localize');
      return;
    }

    _trace('[MAD $agentId] _handleReaderAssignment: localize-reader entry, writerAddr=${entry.writerAddr}');

    // Localize the value: replace global names with local variables
    // Per IGLP, madGLP Receive Transaction: "Localize T↑ by q to get T_q↓"
    final localizedValue =
        _localizeNested(value, fromAgent, '_handleReaderAssignment');

    // Bind the writer with localized value, reactivating suspended goals
    final activations = _bindAndReactivate(entry.writerAddr, localizedValue);
    _trace('[MAD $agentId] _handleReaderAssignment: bound writer, $activations activations');

    // Remove the entry
    wp.removeLocalizeEntry(globalName);
    _appliedReaderNames.add(globalName);
    _trace('[MAD $agentId] _handleReaderAssignment: entry removed');

    // Acknowledge to the value's sender (madGLP Receive, Reader case): the
    // acknowledgement releases the pending value at the sender and closes the
    // link. Only reader-name values are acknowledged.
    mp.add(OutboundMessage(
      destination: fromAgent,
      type: MessageType.acknowledgement,
      payload: PayloadCodec.createAckPayload(globalName, senderEpoch: _epoch),
      link: globalName,
    ));
    _trace('[MAD $agentId] _handleReaderAssignment: queued ack($globalName) '
        'to $fromAgent');
  }

  /// The value [value] of an assignment from [fromAgent] with its nested
  /// global names localized (Definition Localize): the entries and records
  /// made and the spawned goals registered, an assignment that arrived early
  /// for any of the names delivered, the requests the forwarded names call
  /// for queued, and the names replaced by the fresh variables.  [handler]
  /// names the Receive case, for the trace.  A value carrying no global name
  /// is returned as it is.
  Term _localizeNested(Term value, String fromAgent, String handler) {
    final globalNames = extractGlobalNames(value);
    if (globalNames.isEmpty) return value;
    _trace('[MAD $agentId] $handler: localizing ${globalNames.length} nested global names');
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
    final localizedValue =
        localizeTermWithResult(value, globalNames, localizeResult);
    _trace('[MAD $agentId] $handler: localized value = $localizedValue');
    return localizedValue;
  }

  /// Deliver any held `_r` assignments whose `LocalizeEntry` was just created.
  ///
  /// Issue 7 / IGLP app:in-networking, Early messages: after `localize()` creates LocalizeEntries for the
  /// given global names, check the hold table for each `_r(p, i)` name and
  /// process any assignment that arrived before the entry existed. Delivering
  /// re-enters `_handleReaderAssignment`, which now finds the entry; nested
  /// held assignments terminate because each delivery removes its hold-table
  /// entry.
  void _deliverHeldReaderAssignments(List<GlobalName> globalNames) {
    for (final gn in globalNames) {
      if (gn.isWriter) continue; // only `_r` names create LocalizeEntries
      final held = _heldReaderAssignments.remove(gn);
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
    int senderEpoch = 1,
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
      senderEpoch: senderEpoch,
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
  ///
  /// A message --- a value, a request or an acknowledgement --- carrying a
  /// global name whose agent is not an atom is refused at receipt in the same
  /// way, the refusal naming the name: "An agent is an atom and an index is a
  /// non-negative integer, and the reader refuses a global name whose agent or
  /// index is anything else" (IGLP code-format-fragment.tex, Terms, tag 2).
  /// The name is a tag-2 variable, or the name the message is addressed to;
  /// a structure `'_w'(P, I)` the message carries is a program's term and
  /// arrives as the structure (IGLP madglp-spec.tex:17, at IGLP 1e01638).
  ///
  /// Every message carries the epoch of its sender, which this agent learns
  /// before the message is applied, retiring every link end whose other end
  /// is at the sender with a recorded epoch below it ([learnEpoch]; IGLP
  /// db0a117, Definition "madGLP Send Transaction": "In every case E'_q(p) is
  /// the epoch m carries, and q retires every link end whose other end is at
  /// p and whose recorded epoch of p is below it").  A fourth kind, gone(G),
  /// retires this agent's end of the link G ([handleGone]).
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
        final ({GlobalName name, Term value, int? serial, int senderEpoch})
            received;
        try {
          received = PayloadCodec.deserializeValuePayload(payload);
        } on ModuleRefusal catch (refusal) {
          print('[REFUSED] $agentId: a message from $fromAgent is refused at '
              'receipt: ${refusal.message}');
          return;
        } on GlobalNameRefusal catch (refusal) {
          print('[REFUSED] $agentId: a message from $fromAgent is refused at '
              'receipt: ${refusal.message}');
          return;
        }
        learnEpoch(fromAgent, received.senderEpoch);
        handleMadAssignment(
            globalName: received.name,
            value: received.value,
            fromAgent: fromAgent,
            serial: received.serial,
            senderEpoch: received.senderEpoch);
      case wireMsgKindRequest || wireMsgKindAcknowledgement || wireMsgKindGone:
        final ({int kind, GlobalName name, int senderEpoch}) link;
        try {
          link = PayloadCodec.decodeLinkPayload(payload);
        } on GlobalNameRefusal catch (refusal) {
          print('[REFUSED] $agentId: a message from $fromAgent is refused at '
              'receipt: ${refusal.message}');
          return;
        }
        learnEpoch(fromAgent, link.senderEpoch);
        switch (link.kind) {
          case wireMsgKindRequest:
            handleRequest(link.name, fromAgent, senderEpoch: link.senderEpoch);
          case wireMsgKindAcknowledgement:
            handleAck(link.name, fromAgent, senderEpoch: link.senderEpoch);
          default:
            handleGone(link.name, fromAgent);
        }
      default:
        throw FormatException(
            'unknown madGLP message kind: ${payload[0]}');
    }
    // The Receive transaction is complete: its held-link reports go on the
    // network input stream now, after whatever the transaction delivered.
    _flushReports();
  }

  // =========================================================================
  // Epochs: learning a peer's, retiring link ends, the restart after a crash
  // (IGLP db0a117, Definitions "Epoch", "madGLP Local State", "Retire" and
  // "Restart after a Crash")
  // =========================================================================

  /// A message from [agent] carrying the epoch [epoch] is applied: E_p(agent)
  /// becomes [epoch], "which is never below the epoch E_q(p) held before",
  /// and where it rose this agent retires every link end whose other end is
  /// at [agent] and whose recorded epoch of [agent] is below it (IGLP
  /// db0a117, Definition "madGLP Send Transaction", the sentence after the
  /// list): [agent] crashed and restarted, and every link between the two
  /// ended.  Returns the names of the ends retired.
  List<GlobalName> learnEpoch(String agent, int epoch) {
    final known = _knownEpochs[agent];
    if (known != null && epoch <= known) return const [];
    _knownEpochs[agent] = epoch;
    if (known == null) {
      // The first message from the agent: nothing recorded of it is below.
      _trace('[MAD $agentId] epoch of $agent learnt: $epoch');
    } else {
      _trace('[MAD $agentId] epoch of $agent rose from $known to $epoch: '
          'the ends at it are retired');
    }
    final retired = <GlobalName>[];
    final entries = wp.retireEndsAt(agent, epoch);
    for (final (index, _) in entries.globalize) {
      retired.add(GlobalName.writer(agentId, index, epoch: _epoch));
    }
    for (final e in entries.localize) {
      retired.add(e.name);
    }
    for (final rec in up.retireAt(agent, epoch)) {
      globalSendRegistry.removeGoalFor(rec.writerAddr);
      runtime.heap.removeBindCallback(rec.writerAddr);
      retired.add(rec.name);
    }
    for (final goal in globalSendRegistry.retireWhere((g) =>
        g.globalName.isReader &&
        g.globalName.agent == agentId &&
        g.destination == agent &&
        g.otherEpoch != null &&
        g.otherEpoch! < epoch)) {
      runtime.heap.removeBindCallback(goal.readerAddr);
      retired.add(goal.globalName);
    }
    for (final MapEntry(key: name, value: pending)
        in _pendingReaderValues.entries.toList()) {
      final h = pending.holderEpoch;
      if (pending.destination == agent && h != null && h < epoch) {
        _pendingReaderValues.remove(name);
        retired.add(name);
      }
    }
    for (final name in retired) {
      _endRetired(name);
    }
    return retired;
  }

  /// A `gone(G)` from [fromAgent]: this agent retires its end of the link
  /// [name], where it has one (IGLP db0a117, Definition "madGLP Send
  /// Transaction", Gone: "q retires its end of the link G, where it has one:
  /// the entry, or the record and the global_send goal, and the messages of
  /// the link in M_q.  Nothing else changes at q").
  void handleGone(GlobalName name, String fromAgent) {
    _trace('[MAD $agentId] gone($name) from $fromAgent');
    retireEnd(name);
  }

  /// Retire this agent's end of the link named [name], where it has one
  /// (Definition "Retire"): the entry, or the record and the goal, or the
  /// goal and the pending value of a reader name anchored here; the messages
  /// of the link in M_p; and what the agent held or recorded of the link.
  /// Returns whether an end was held.  The paired local variable stays in
  /// the resolvent, widowed: "the writer of a removed entry is never
  /// assigned, and the reader of a removed goal is held by no link".
  bool retireEnd(GlobalName name) {
    var held = false;
    if (name.agent == agentId) {
      if (name.epoch == _epoch) {
        if (name.isWriter) {
          if (wp.lookupByIndex(name.index) != null && name.index > 0) {
            wp.removeGlobalizeEntry(name.index);
            held = true;
          }
        } else {
          final goal = globalSendRegistry.removeGoalForLink(name);
          if (goal != null) {
            runtime.heap.removeBindCallback(goal.readerAddr);
            held = true;
          }
          if (_pendingReaderValues.remove(name) != null) held = true;
        }
      }
    } else if (name.isReader) {
      if (wp.findByRemote(name) != null) {
        wp.removeLocalizeEntry(name);
        held = true;
      }
    } else {
      final rec = up.findByLink(name);
      if (rec != null) {
        up.remove(rec.writerAddr);
        globalSendRegistry.removeGoalFor(rec.writerAddr);
        runtime.heap.removeBindCallback(rec.writerAddr);
        held = true;
      }
    }
    _endRetired(name);
    return held;
  }

  /// What goes with an end retired: the messages of its link, and what was
  /// held, recorded or reported of it; and the name is recorded as lost, so
  /// that traffic for it is answered gone ([_lostEnd]).
  void _endRetired(GlobalName name) {
    final removed = mp.removeOfLink(name);
    _heldReaderAssignments.remove(name);
    _heldAssignments.remove(name);
    _heldRequests.remove(name);
    _requestPairs.remove(name);
    _reportedLinks.remove(name);
    _retiredEnds.add(name);
    _trace('[MAD $agentId] end $name retired'
        '${removed == 0 ? '' : ', $removed messages of it removed'}');
  }

  /// Traffic from [fromAgent] for the end [name]: where this agent lost it,
  /// retired at a crash, on learning the other agent's epoch or on a gone,
  /// the agent answers gone([name]) to the sender, "which retires its own
  /// end of the link" (IGLP db0a117, "Requests and Acknowledgements"), and
  /// true is returned; false where the agent never had the end, or has it.
  bool _lostEnd(GlobalName name, String fromAgent) {
    if (!_retiredEnds.contains(name)) return false;
    _trace('[MAD $agentId] traffic for the lost end $name from $fromAgent: '
        'answered gone');
    mp.add(OutboundMessage(
      destination: fromAgent,
      type: MessageType.gone,
      payload: PayloadCodec.createGonePayload(name, senderEpoch: _epoch),
    ));
    return true;
  }

  /// The restart after a crash (IGLP db0a117, Definition "Restart after a
  /// Crash": the local state at the restart "is its local state at its last
  /// suspension ... with e_p incremented by one and every link end of p
  /// retired but the serializer entry"): the next epoch is opened, every
  /// entry but the serializer's, every record, every global_send goal and
  /// every pending value is retired, with the messages of their links, what
  /// was held, recorded or reported of them, and the early assignments; the
  /// cold calls left in M_p are sent in the new epoch ("the epoch its sender
  /// is in when it sends it").  The known epochs stand.  Returns the names of
  /// the ends retired.
  List<GlobalName> crashRestart() {
    _epoch++;
    _trace('[MAD $agentId] restart after a crash: epoch $_epoch, every link '
        'end retired');
    final retired = <GlobalName>[];
    final entries = wp.retireAllButSerializer();
    for (final (index, _) in entries.globalize) {
      retired.add(GlobalName.writer(agentId, index, epoch: _epoch - 1));
    }
    for (final e in entries.localize) {
      retired.add(e.name);
    }
    for (final rec in up.retireAll()) {
      globalSendRegistry.removeGoalFor(rec.writerAddr);
      runtime.heap.removeBindCallback(rec.writerAddr);
      retired.add(rec.name);
    }
    for (final goal in globalSendRegistry.retireWhere((_) => true)) {
      runtime.heap.removeBindCallback(goal.readerAddr);
      retired.add(goal.globalName);
    }
    retired.addAll(_pendingReaderValues.keys);
    _pendingReaderValues.clear();
    for (final name in retired) {
      _endRetired(name);
    }
    // What remains held or reported is of ends this agent never had.
    _retiredEnds.addAll(_heldReaderAssignments.keys);
    _heldReaderAssignments.clear();
    _heldAssignments.clear();
    _heldRequests.clear();
    _requestPairs.clear();
    _reportedLinks.clear();
    for (final m in mp.all.toList()) {
      if (m.link != null) mp.remove(m);
    }
    mp.withSenderEpoch(_epoch);
    return retired;
  }

  /// Handle req(_r(p, i)) — we are the anchor p (madGLP Receive, Request
  /// case): the request is held; on authorisation the requester is recorded
  /// among the link's holders and a pending value is addressed to it as well.
  /// A request repeating a recorded (link, requester) pair, and a request for
  /// a closed link, are dropped.
  void handleRequest(GlobalName globalName, String fromAgent,
      {int senderEpoch = 1}) {
    _trace('[MAD $agentId] handleRequest: req($globalName) from $fromAgent '
        'in epoch $senderEpoch');
    final key = globalName;
    if (globalName.agent == agentId && globalName.epoch < _epoch) {
      _trace('[MAD $agentId] handleRequest: $globalName names epoch '
          '${globalName.epoch}, before this agent\'s $_epoch — stale, '
          'dropped');
      return;
    }

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
    // troubling the program: "A request for a closed link is dropped."  One
    // for an end this agent lost is answered gone.
    if (!_readerLinkIsLive(globalName)) {
      if (_lostEnd(globalName, fromAgent)) return;
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
    final queue = _heldRequests[key] ??= <(String, int)>[];
    queue.add((fromAgent, senderEpoch));
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
      _pendingReaderValues.containsKey(globalName) ||
      globalSendRegistry.hasGoalForLink(globalName);

  /// After the program answered the held request reported for the link
  /// [globalName], report the next requester held for it, in its turn (§Held
  /// Links).  A held request whose link has closed meanwhile is dropped
  /// unreported: "A request for a closed link is dropped" (madGLP Receive,
  /// Request case).
  void _reportNextHeldRequest(GlobalName globalName) {
    final queue = _heldRequests[globalName];
    if (queue == null) return;
    if (queue.isNotEmpty && _readerLinkIsLive(globalName)) {
      _reportPendingLink(globalName, queue.first.$1);
      return;
    }
    for (final (requester, _) in queue) {
      _trace('[MAD $agentId] held req($globalName) from $requester: the link '
          'closed — dropped');
    }
    _heldRequests.remove(globalName);
  }

  /// Release the request records of the reader-name link [globalName], now
  /// closed: its recorded (link, requester) pairs, and the requests held
  /// behind the one reported, which are for a closed link.
  void _releaseRequestRecords(GlobalName globalName, {bool held = false}) {
    _requestPairs.remove(globalName);
    if (held) _heldRequests.remove(globalName);
  }

  /// Perform the request the anchor held, once authorised (madGLP Receive,
  /// Request case): record the requester among the link's holders, with the
  /// epoch [epoch] its request carried as the epoch the end records of it,
  /// and address a pending value to it as well, unsent.
  void _applyRequest(GlobalName globalName, String fromAgent, int epoch) {
    final pending = _pendingReaderValues[globalName];
    if (pending != null) {
      // Address the pending value to the requester as well: "if the value is
      // pending, p addresses it to s as well" (§app:requests-acks). The copy
      // sent to a past holder is dropped there as stale; the value stays
      // pending until the current holder's acknowledgement arrives.
      _addressPendingTo(globalName, pending, fromAgent, epoch);
      _trace('[MAD $agentId] handleRequest: pending $globalName addressed '
          'to $fromAgent as well');
      return;
    }

    // Value not yet produced: record the requester among the link's holders —
    // per-link runtime state consulted at send time, when the value goes to
    // each holder recorded (§app:requests-acks).
    if (globalSendRegistry.recordRequester(globalName, fromAgent,
        epoch: epoch)) {
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
  void handleAck(GlobalName globalName, String fromAgent,
      {int senderEpoch = 1}) {
    if (globalName.agent == agentId && globalName.epoch < _epoch) {
      _trace('[MAD $agentId] handleAck: $globalName names epoch '
          '${globalName.epoch}, before this agent\'s $_epoch — stale, '
          'dropped');
      return;
    }
    final removed = _pendingReaderValues.remove(globalName);
    if (removed == null) {
      if (_lostEnd(globalName, fromAgent)) return;
      _trace('[MAD $agentId] handleAck: ack($globalName) matches no pending '
          'value — stale, dropped');
      return;
    }
    for (final nested in removed.nestedReaderNames) {
      _recordHolder(nested, fromAgent, senderEpoch);
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
  void _recordHolder(GlobalName globalName, String holder, int epoch) {
    if (globalSendRegistry.recordHolder(globalName, holder, epoch: epoch)) {
      _trace('[MAD $agentId] holder of $globalName recorded as $holder');
      return;
    }
    final pending = _pendingReaderValues[globalName];
    if (pending != null && pending.destination != holder) {
      _addressPendingTo(globalName, pending, holder, epoch);
      _trace('[MAD $agentId] pending $globalName re-addressed to holder '
          '$holder');
    }
  }

  /// Address the pending value [pending] of the reader-name link [key] to
  /// [holder], whose epoch [epoch] the end records: it is recorded as the
  /// value's destination, and the value is queued to it, unsent.
  void _addressPendingTo(GlobalName key, _PendingReaderValue pending,
      String holder, int epoch) {
    _pendingReaderValues[key] = (
      payload: pending.payload,
      destination: holder,
      nestedReaderNames: pending.nestedReaderNames,
      holderEpoch: epoch,
    );
    mp.add(OutboundMessage(
      destination: holder,
      type: MessageType.assignment,
      payload: pending.payload,
      link: key,
    ));
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
    // The program answers with the name the report carried, epoch and all
    // (IGLP cec4682, "Global Variable Names").
    final answered = _answerLink(gn, authorise);
    if (answered && !gn.isWriter) _reportNextHeldRequest(gn);
    _flushReports();
    return answered;
  }

  bool _answerLink(GlobalName gn, bool authorise) {
    if (_reportedLinks.remove(gn) == null) {
      _trace('[MAD $agentId] answerLink: $gn was not reported — ignored');
      return false;
    }
    _trace('[MAD $agentId] answerLink: $gn '
        '${authorise ? "AUTHORISED" : "REFUSED"}');

    final heldMessage = mp.findHeld(gn);
    final heldAssignment = _heldAssignments.remove(gn);
    // The held request reported for the link is the first held for it.
    final heldQueue = gn.isWriter ? null : _heldRequests[gn];
    final heldRequester = (heldQueue == null || heldQueue.isEmpty)
        ? null
        : heldQueue.removeAt(0);

    if (authorise) {
      // A held outgoing message becomes unsent — the same message, released.
      if (heldMessage != null) {
        heldMessage.held = false;
        // Case 2: the value now goes, so its record has served its purpose.
        final rec = up.findByLink(gn);
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
        _applyRequest(gn, heldRequester.$1, heldRequester.$2);
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
      if (gn.agent == agentId) wp.removeGlobalizeEntry(gn.index);
    } else {
      wp.removeLocalizeEntry(gn);
      _pendingReaderValues.remove(gn);
      // The link is closed: its recorded pairs go, and the requests still
      // held for it are for a closed link, dropped.
      _releaseRequestRecords(gn, held: true);
    }
    final rec = up.findByLink(gn);
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

  // =========================================================================
  // The agent's saved state (IGLP, Definition "Restart"; Implementation
  // Notes, "Persistence")
  // =========================================================================

  /// Write what this context holds into the agent's saved state: the global
  /// writers table W_p, the imported-writer records U_p, the outgoing messages
  /// M_p with their held marks and the pending values, the `global_send` goals
  /// and the heap callbacks that fire them, and the records Implementation
  /// Notes adds to the local state --- the early and held assignments, the
  /// applied and forwarded reader names, the held requests and recorded
  /// requesters, and the links reported and not yet answered.  Written at the
  /// end of an event cycle, when no transaction is in progress.
  void saveState(StateWriter w, {bool scanning = false}) {
    if (!scanning && _deferredReports.isNotEmpty) {
      throw StateError('the state is saved inside a transaction, with '
          '${_deferredReports.length} reports not yet appended');
    }
    w.uint(_epoch);
    w.uint(_knownEpochs.length);
    for (final MapEntry(key: agent, value: e) in _knownEpochs.entries) {
      w.str(agent);
      w.uint(e);
    }
    w.uint(_retiredEnds.length);
    for (final name in _retiredEnds) {
      name.save(w);
    }
    wp.saveState(w);
    up.saveState(w);
    mp.saveState(w);
    globalSendRegistry.saveState(w);

    final callbacks = runtime.heap.bindCallbackTags.toList();
    w.uint(callbacks.length);
    for (final (at, tag) in callbacks) {
      if (tag == null) {
        if (scanning) {
          w.cell(at);
          continue;
        }
        throw StateError('a heap callback on cell ${at.id} was registered '
            'with no tag, and cannot be made again for a restarted agent');
      }
      w.cell(at);
      w.cell(tag);
    }

    w.uint(_heldReaderAssignments.length);
    for (final MapEntry(key: name, value: held)
        in _heldReaderAssignments.entries) {
      name.save(w);
      w.term(held.value);
      w.str(held.fromAgent);
    }
    w.uint(_pendingReaderValues.length);
    for (final MapEntry(key: name, value: pending)
        in _pendingReaderValues.entries) {
      name.save(w);
      w.bytes(pending.payload);
      w.str(pending.destination);
      w.boolean(pending.holderEpoch != null);
      if (pending.holderEpoch != null) w.uint(pending.holderEpoch!);
      w.uint(pending.nestedReaderNames.length);
      for (final gn in pending.nestedReaderNames) {
        gn.save(w);
      }
    }
    _saveNames(w, _forwardedReaderNames);
    _saveNames(w, _appliedReaderNames);
    w.uint(_heldAssignments.length);
    for (final MapEntry(key: name, value: held) in _heldAssignments.entries) {
      name.save(w);
      w.term(held.value);
      w.str(held.fromAgent);
    }
    w.uint(_heldRequests.length);
    for (final MapEntry(key: name, value: requesters)
        in _heldRequests.entries) {
      name.save(w);
      w.uint(requesters.length);
      for (final (q, e) in requesters) {
        w.str(q);
        w.uint(e);
      }
    }
    w.uint(_requestPairs.length);
    for (final MapEntry(key: name, value: requesters)
        in _requestPairs.entries) {
      name.save(w);
      w.uint(requesters.length);
      for (final q in requesters) {
        w.str(q);
      }
    }
    w.uint(_reportedLinks.length);
    for (final MapEntry(key: name, value: sender) in _reportedLinks.entries) {
      name.save(w);
      w.str(sender);
    }
  }

  static void _saveNames(StateWriter w, Set<GlobalName> names) {
    w.uint(names.length);
    for (final name in names) {
      name.save(w);
    }
  }

  static Set<GlobalName> _restoreNames(StateReader r) => {
        for (var i = r.uint(); i > 0; i--) GlobalName.restore(r),
      };

  /// Read back what [saveState] wrote, into a context fresh from
  /// construction, and make again the heap callbacks of the `global_send`
  /// goals, each on the cell it sat on.
  void restoreState(StateReader r) {
    _epoch = r.uint();
    _knownEpochs.clear();
    for (var i = r.uint(); i > 0; i--) {
      final agent = r.str();
      _knownEpochs[agent] = r.uint();
    }
    _retiredEnds
      ..clear()
      ..addAll(_restoreNames(r));
    wp.restoreState(r);
    up.restoreState(r);
    mp.restoreState(r);
    globalSendRegistry.restoreState(r);

    for (var i = r.uint(); i > 0; i--) {
      final at = r.cell();
      final key = r.cell();
      runtime.heap.restoreBindCallback(
          at, (Term value) => onWriterBound(key, value), key);
    }

    _heldReaderAssignments.clear();
    for (var i = r.uint(); i > 0; i--) {
      final name = GlobalName.restore(r);
      final value = r.term();
      _heldReaderAssignments[name] = (value: value, fromAgent: r.str());
    }
    _pendingReaderValues.clear();
    for (var i = r.uint(); i > 0; i--) {
      final name = GlobalName.restore(r);
      final payload = r.bytes();
      final destination = r.str();
      final holderEpoch = r.boolean() ? r.uint() : null;
      final nested = [for (var j = r.uint(); j > 0; j--) GlobalName.restore(r)];
      _pendingReaderValues[name] = (
        payload: payload,
        destination: destination,
        nestedReaderNames: nested,
        holderEpoch: holderEpoch,
      );
    }
    _forwardedReaderNames
      ..clear()
      ..addAll(_restoreNames(r));
    _appliedReaderNames
      ..clear()
      ..addAll(_restoreNames(r));
    _heldAssignments.clear();
    for (var i = r.uint(); i > 0; i--) {
      final name = GlobalName.restore(r);
      final value = r.term();
      _heldAssignments[name] = (value: value, fromAgent: r.str());
    }
    _heldRequests.clear();
    for (var i = r.uint(); i > 0; i--) {
      final name = GlobalName.restore(r);
      _heldRequests[name] = [
        for (var j = r.uint(); j > 0; j--) (r.str(), r.uint())
      ];
    }
    _requestPairs.clear();
    for (var i = r.uint(); i > 0; i--) {
      final name = GlobalName.restore(r);
      _requestPairs[name] = {for (var j = r.uint(); j > 0; j--) r.str()};
    }
    _reportedLinks.clear();
    for (var i = r.uint(); i > 0; i--) {
      final name = GlobalName.restore(r);
      _reportedLinks[name] = r.str();
    }
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

  /// Whether the value for the reader name `_r(anchorAgent, epoch, index)`
  /// is still pending, [epoch] this agent's current one where not given.
  bool hasPendingReaderValue(String anchorAgent, int index, {int? epoch}) =>
      _pendingReaderValues.containsKey(
          GlobalName.reader(anchorAgent, index, epoch: epoch ?? _epoch));

  // =========================================================================
  // Unified Send ('_send' builtin implementation)
  // =========================================================================

  /// Unified send operation per IGLP, Definition "madGLP Send Transaction"
  ///
  /// The `'_send'(T, G, Q)` builtin behavior depends on whether G is a serializer
  /// address (index 0) or a normal global name (index > 0):
  ///
  /// G is the program's name of the link, `_w(p, e, i)` or `_r(p, e, i)`,
  /// its agent [gnAgent], its epoch [gnEpoch] and its index [gnIndex] (IGLP
  /// cec4682, "Global Variable Names": the program's name is the 3-ary
  /// structure).
  ///
  /// **Case G = `_w(q, 0, 0)` (Serializer)**:
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
  void send(Term term, bool isWriter, String gnAgent, int gnEpoch, int gnIndex,
      String destAgent) {
    // The global name's agent and the destination, each a key or a readable
    // name standing for one, resolved to the one agent ([resolveAgent]).
    final resolve = resolveAgent;
    if (resolve != null) {
      gnAgent = resolve(gnAgent) ?? gnAgent;
      destAgent = resolve(destAgent) ?? destAgent;
    }
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
      localEpoch: _epoch,
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
    // Per IGLP Definition Globalize, writer case: when Y is a writer, p
    // creates an entry (Y, q) and
    // waits for the assignment to arrive. The global_send goal is spawned at q
    // (by localize), not at p. Agent p does not send anything for _w entries.

    // Transform the term to use global names
    final globalizedTerm = globalizeTermWithResult(term, vars, globalizeResult);
    _trace('[MAD $agentId] send: globalized term = $globalizedTerm');

    // The global name as the program gave it, epoch included.
    final globalName = isWriter
        ? GlobalName.writer(gnAgent, gnIndex, epoch: gnEpoch)
        : GlobalName.reader(gnAgent, gnIndex, epoch: gnEpoch);

    // Create payload based on serializer (index 0) vs normal (index > 0)
    List<int> payload;
    if (globalName.isSerializer) {
      // Serializer case: wrap in list cell [T↑ | _w(q,0)], under a serial
      // this agent allocates from its global-name counter in its epoch —
      // after T's own names, which keep the indices they had before cold
      // calls carried one.
      final serial = wp.allocateIndex();
      _trace('[MAD $agentId] send: serializer case, wrapping in list cell, '
          'serial $serial of epoch $_epoch');
      payload = PayloadCodec.createSerializerPayload(globalName, globalizedTerm,
          serial: serial, senderEpoch: _epoch);
    } else {
      // Normal case: send directly
      _trace('[MAD $agentId] send: normal case, sending directly');
      payload = PayloadCodec.createGlobalSendPayload(globalName, globalizedTerm,
          senderEpoch: _epoch);
    }

    // Queue the message for delivery
    mp.add(OutboundMessage(
      destination: destAgent,
      type: MessageType.assignment,
      payload: payload,
      link: globalName.isSerializer ? null : globalName,
    ));

    // A reader-name value pends until acknowledged (§app:requests-acks).
    if (!isWriter && gnIndex > 0) {
      _pendingReaderValues[globalName] = (
        payload: payload,
        destination: destAgent,
        nestedReaderNames:
            globalizeResult.spawns.map((s) => s.globalName).toList(),
        holderEpoch: null,
      );
      _trace('[MAD $agentId] send: value ${globalName} pending until ack');
    }

    _trace('[MAD $agentId] send: queued message to $destAgent, mp.totalLength=${mp.totalLength}');
    _flushReports();
  }
}
