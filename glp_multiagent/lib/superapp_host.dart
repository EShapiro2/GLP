/// The Grassroots App on a phone: one agent, its person live, its network the
/// layer it is given.
///
/// The runtime spawns the agent with every agent's initial goal,
/// `agent(ch(_?,_),ch(_?,_))`, which `programs/social/graph/core`'s agent/2
/// answers (GSA paper, Appendix "The Operating System", "Boot": "The runtime
/// posts to the Grassroots App the initial goal of every agent ... the
/// person's first act names the entry, with the person's key and the keys of
/// the persons the person knows, none at a first start"; IGLP, Implementation
/// Notes, "Boot": "a deployed agent is spawned by its application with the
/// same initial goal and its key made the same way, the person being the
/// person.  At the agent's first start the application sends on the person
/// channel the act that starts its program, with the agent's own key and the
/// keys of the peers the person knows, none at a first start"; GSA paper,
/// Appendix "The Operating System", "Boot", 963fcbf: "and the rendezvous
/// servers the app is configured with, each by key and address").  The act
/// is `superapp(Me, [], Servers)`, Servers the list of
/// `rendezvous(Key, Address)` the host is given ([rendezvousServers]), which
/// core's agent/2 starts as superapp/5: the social graph with the operating
/// system's Nearby and its invitation links around it (place.glp,
/// os_phone/7; links.glp) and home, the Grassroots App's own person
/// interface, on the person channel.  Another entry, `superapp_ui`, the
/// demonstration in one heap, has no network and takes `superapp_ui(Me, [])`.
///
/// The agent runs over the [GlpNetwork] it is given ([AgentRuntime]; IGLP,
/// Implementation Notes, "Networking Seam", One interface: "transport
/// integration is a backend swap"): on a phone GNA's glue over the layer, PAN,
/// LAN and WAN (phone_network.dart); in a test a network in one process.  It runs
/// in the isolate that holds the layer, since the layer's transports are the
/// platform's and answer in the isolate that registered them.
///
/// The agent's local state survives the termination of the app, kept by the
/// store the app gives it ([stateStore]; IGLP, Definition "Restart" and
/// Implementation Notes, "Persistence": "the runtime keeps it by a store the
/// host gives it ... and a restart resumes from the state the store holds").
/// At a restart the app sends no first act: the agent goes on from the state
/// the store holds, and the runtime grants refresh in the person's place, on
/// which the dispatcher has every construct draw again what it last drew, so
/// the screen is drawn from the restored state (IGLP 7a25ff8, the paragraph
/// after Definition "Restart"; vGLP d433f71, Section 6.3).  Until 2026-11-08
/// the app gave none, and every launch was a first start.  An update of the
/// app is a restart under a new version of the program: the state is restored
/// under it when every procedure a goal of the resolvent calls is defined in
/// it with the same type identity, and otherwise the update is refused and
/// the agent restarts under the version it terminated under, which the store
/// keeps (IGLP 61f3283, madglp-spec.tex, Termination and Restart;
/// Implementation Notes, "Persistence"), the host holding the refusal
/// ([updateRefusal]) for the app to say so.
///
/// The person sees what the program draws and nothing else (GSA paper,
/// Appendix "The Operating System", "The person"): each term the program sends
/// the person by send_to_user/1 is handed whole to the [UiRuntime], whose
/// bridge draws the constructs of home's questions as the default widget of
/// each question's type and the agent's notices as the thread of construct 0;
/// the person's grants on them go back to the agent as the person's acts.
library;

import 'dart:async';
import 'dart:typed_data';

import 'package:glp_runtime/multiagent/agent_runtime.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:glp_runtime/multiagent/glp_network.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

import 'isolate_protocol.dart' show runtimeTermOf, screenTermOf;
import 'ui_runtime/manifest.dart';
import 'ui_runtime/runtime.dart';
import 'ui_runtime/term.dart' show GTerm;

/// The entry the person's first act names on a phone: superapp/5 of
/// `programs/social/graph/core` (core's CoreAct
/// `superapp(Key, List(Key), List(RvServer))`).
const String superappEntry = 'superapp';

/// One agent of the Grassroots App, its person live, over a given network.
class SuperAppHost {
  /// The agent's handle in the log; over the network the agent is named by
  /// its key ([AgentRuntime.name]).
  final String agentId;

  /// The program the agent runs, `programs/social/graph/core`.
  final String program;

  /// The root `programs/self.glp`.
  final String rootSelfGlpPath;

  /// The network the agent runs over.
  final GlpNetwork network;

  /// The agent's key pair, the layer's ([AgentRuntime.keyPair]).
  final ({PubKey pub, Uint8List priv}) keyPair;

  /// The entry the person's first act names ([superappEntry]).
  final String entry;

  /// The rendezvous servers the app is configured with, each by its key, 64
  /// lowercase hex characters, and its address, `ip:port`, which the first
  /// act of [superappEntry] names (GSA paper, Appendix "The Operating
  /// System", "Boot").
  final List<({String key, String address})> rendezvousServers;

  /// The person's first act at the agent's first start, given the agent's
  /// key; null, and it is the act that starts the Grassroots App,
  /// `superapp(Me, [])`.  A scripted build's is its scripted person's,
  /// `sg_init(Me, Mode, Ks)` (scripted_person.dart).
  final rt.Term Function(String key)? firstAct;

  /// The image of the program's display declarations.
  final Manifest manifest;

  /// The person's screen: the bridge, and what the agent tells the person.
  /// The person's grants on it go to the agent, each an act of the person.
  late final UiRuntime ui = UiRuntime(
      manifest: manifest,
      onSend: (cmd) {
        onAct?.call(cmd);
        act(runtimeTermOf(cmd));
      });

  /// Each act of the person on the screen, as the bridge sends it, before it
  /// is handed to the agent; null, and none is told.  A debuggable build
  /// reports it (lib/debuglog/app_reporting.dart).
  void Function(GTerm cmd)? onAct;

  /// Each term the agent sends its person, as [UiRuntime.handle] is handed
  /// it ([AgentRuntime.onUserOutput]); null, and none is told.  A debuggable
  /// build reports it (lib/debuglog/app_reporting.dart).
  void Function(rt.Term term)? onUserOutput;

  /// The store that keeps the agent's local state across the termination of
  /// the app; null, and every start is a first start.
  final AgentStateStore? stateStore;

  /// The agent.
  final AgentRuntime agent;

  /// The agent's transcript, line by line ([AgentRuntime.onOutput]).
  void Function(String line)? onOutput;

  /// The agent's trace ([AgentRuntime.onLog]); null, and the agent runs
  /// untraced.
  void Function(String tag, String message)? onLog;

  /// Where the start refused an update of the app, the refusal: the agent
  /// runs the version it terminated under, which the store keeps (IGLP
  /// 61f3283, Termination and Restart).  Null where none was refused.
  UpdateRefusal? get updateRefusal => agent.updateRefusal;

  /// The person's acts not yet handed to the agent, in order: an act is an
  /// event of the agent, and its events are taken one at a time.
  Future<void> _acts = Future.value();

  SuperAppHost({
    required this.agentId,
    required this.program,
    required this.rootSelfGlpPath,
    required this.network,
    required this.keyPair,
    required this.manifest,
    this.entry = superappEntry,
    this.rendezvousServers = const [],
    this.firstAct,
    this.stateStore,
  }) : agent = AgentRuntime(
          agentId: agentId,
          program: program,
          rootSelfGlpPath: rootSelfGlpPath,
          keyPair: keyPair,
          network: network,
          stateStore: stateStore,
        );

  /// The agent's key, as 64 lowercase hex characters: its name.
  String get key => keyPair.pub.hex;

  /// Start the agent: compile the program, post the initial goal, and, at
  /// the agent's first start, send on the person channel the act that starts
  /// the Grassroots App, `superapp(Me, [], Servers)`; at a restart, go on from the
  /// state the [stateStore] holds, the runtime granting refresh in the
  /// person's place ([AgentRuntime.initialize]).  The person's grants go to
  /// the agent as they come, each an act of the person.  A restart under a
  /// new version of the program restores the state under it or refuses the
  /// update ([updateRefusal]).  Throws where the saved state cannot be
  /// restored: another agent's, or saved under code that is neither the
  /// program's compilation nor the program the store keeps (AgentRuntime's
  /// restore).
  Future<void> start() async {
    agent.onUserOutput = (term) {
      onUserOutput?.call(term);
      ui.handle(screenTermOf(term));
    };
    agent.onOutput = (line) => onOutput?.call(line);
    final log = onLog;
    if (log != null) agent.onLog = log;
    await agent.initialize();
    if (agent.firstStart) {
      await act(firstAct?.call(key) ??
          rt.StructTerm(entry, [
            rt.ConstTerm(key),
            rt.ConstTerm(rt.nil),
            if (entry == superappEntry) _servers(),
          ]));
    }
  }

  /// The person's act [term], a ground term, handed to the agent after the
  /// acts before it.
  Future<void> act(rt.Term term) =>
      _acts = _acts.then((_) => agent.injectUserInput(term));

  /// Completes once every act of the person so far has been taken by the
  /// agent, each its event cycle complete.
  Future<void> get idle => _acts;

  void dispose() => agent.dispose();

  /// [rendezvousServers] as the list term the first act carries, each
  /// `rendezvous(Key, Address)`.
  rt.Term _servers() {
    rt.Term list = rt.ConstTerm(rt.nil);
    for (final s in rendezvousServers.reversed) {
      list = rt.StructTerm('.', [
        rt.StructTerm('rendezvous', [rt.ConstTerm(s.key), rt.ConstTerm(s.address)]),
        list,
      ]);
    }
    return list;
  }
}
