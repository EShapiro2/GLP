/// The Grassroots App on a phone: the person's own agent over the real
/// network, Bluetooth, the local network and the Internet (GSA paper, Section 2 and
/// Appendix "The Operating System").
///
/// The app makes or reads the agent's key pair and instantiates the
/// networking layer with it, PAN, LAN and WAN (phone_network.dart); the agent runs
/// `programs/social/graph/core` from every agent's initial goal, and at its
/// first start the app sends on the person channel the act that starts the
/// Grassroots App, `superapp(Me, [])` (superapp_host.dart; GSA paper,
/// Appendix, "Boot"; IGLP, Implementation Notes, "Boot").  The person sees
/// home, the Grassroots App's own person interface, as the bridge draws the
/// constructs of its questions, and the thread of what the agent tells them:
/// opening the place (GPE:1), the persons nearby they come to know by an
/// encounter (GPE:2), and befriending (GSG:1) by picking a person nearby on
/// home's card for them, no key typed.
///
/// A debuggable build, built with the Dart define `GLP_DEBUGLOG=<password>`,
/// reports everything the app does to the log service
/// (GLP/phones/docs/debugging.md, "The phone side"; lib/debuglog/); a build
/// without it reaches none of that code, [debugLogOn] being a constant.
///
/// A scripted build runs the same app with a scripted person in place of the
/// person on the screen ([GrassrootsApp.script]; main_scripted.dart).
///
/// An invitation link the person tapped, which the phone hands the app, is
/// the person's act: the app sends on the person channel the act that opens
/// the link, as if the person had entered its text (IGLP, Implementation
/// Notes, "Boot"; invitation_link.dart).  A scripted build's person is its
/// script, and it takes no such act.
///
/// The demonstration with scripted peers in one heap, `superapp_ui/4`, is
/// `main_superapp_ui.dart`.
library;

import 'app_identity.dart' show installedApp;
import 'package:flutter/material.dart';
import 'package:glp_runtime/multiagent/agent_state_store.dart';
import 'package:path_provider/path_provider.dart';

import 'debuglog/app_reporting.dart';
import 'glp_sources.dart';
import 'invitation_link.dart';
import 'manifests/superapp_ui.dart';
import 'phone_network.dart';
import 'scripted_person.dart';
import 'superapp_host.dart';
import 'ui_runtime/agent_surface.dart';
import 'ui_runtime/look.dart';

/// The persona's name in the seed of the agent's key pair, made once at the
/// first start (IGLP, Implementation Notes, "Boot").
const String _persona = 'person';

void main() {
  WidgetsFlutterBinding.ensureInitialized();
  // Before runApp, so that the addresses the phone hands the app reach the
  // links before the app's navigator (invitation_link.dart, [InvitationLinks.listen]).
  runApp(GrassrootsApp(links: InvitationLinks.listen()));
}

class GrassrootsApp extends StatelessWidget {
  const GrassrootsApp({super.key, this.script, this.links});

  /// A scripted build's person, made when the app starts, where a fault in
  /// the build's defines is the app's to report; null, and the person is on
  /// the screen.
  final ScriptedPerson Function()? script;

  /// The invitation links the phone hands the app, each sent as the person's
  /// act that opens it; null, and none is taken.
  final InvitationLinks? links;

  @override
  Widget build(BuildContext context) => MaterialApp(
        title: 'Grassroots',
        debugShowCheckedModeBanner: false,
        theme: ThemeData(useMaterial3: true),
        home: GrassrootsScreen(script: script, links: links),
      );
}

class GrassrootsScreen extends StatefulWidget {
  const GrassrootsScreen({super.key, this.script, this.links});

  /// [GrassrootsApp.script].
  final ScriptedPerson Function()? script;

  /// [GrassrootsApp.links].
  final InvitationLinks? links;
  @override
  State<GrassrootsScreen> createState() => _GrassrootsScreenState();
}

class _GrassrootsScreenState extends State<GrassrootsScreen> {
  PhoneNetwork? _net;
  SuperAppHost? _host;
  String _status = 'Starting…';

  /// The reporter and its reader of the screen, in a debuggable build; null
  /// otherwise.  Every use of them is behind the constant [debugLogOn], so
  /// that a build without `GLP_DEBUGLOG` keeps none of the reporter's code.
  DebugReporter? _reporter;
  ScreenReporter? _screen;
  AppLifecycleListener? _lifecycle;

  /// The app's going to the background, on which the agent's state is saved
  /// (IGLP d70ec2e, Implementation Notes, "Persistence": the runtime keeps
  /// the state "saving it when the application goes to the background, which
  /// the platform announces before it may kill the application, and at no
  /// other time"): `AppLifecycleState.paused`.
  AppLifecycleListener? _background;

  /// The person's screen, which a debuggable build reads as text.
  final GlobalKey _surface = GlobalKey();

  /// The look of the Grassroots App's contract, once read; null until then.
  ContractLook? _look;

  /// The app's own line, reported in a debuggable build.
  void _setStatus(String status) {
    _status = status;
    if (debugLogOn) _reporter?.event('app', 'status', {'line': status});
  }

  /// An exception the app catches, reported in a debuggable build.
  void _caught(Object e, StackTrace stack) {
    if (debugLogOn) {
      _reporter?.event('app', 'exception',
          {'error': e.toString(), 'stack': stack.toString()});
    }
  }

  /// In a debuggable build, the screen read as text once the frame being
  /// built is drawn.
  void _screenChanged() {
    if (debugLogOn) _screen?.afterFrame(() => _surface.currentContext!);
  }

  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addPostFrameCallback((_) => _start());
  }

  @override
  void dispose() {
    _background?.dispose();
    _host?.dispose();
    _net?.dispose();
    if (debugLogOn) {
      _lifecycle?.dispose();
      _reporter?.close();
    }
    super.dispose();
  }

  Future<void> _start() async {
    if (debugLogOn) {
      final reporter = await startDebugReporter();
      if (reporter != null) {
        _reporter = reporter;
        _screen = ScreenReporter(reporter);
        _lifecycle = reportLifecycle(reporter);
      }
    }
    try {
      final make = widget.script;
      final script = make?.call();
      if (debugLogOn && script != null) {
        _reporter?.event('app', 'script',
            {'mode': script.mode, 'known': script.known});
      }
      final glp = await resolveGlpPaths();
      // The look the person's screen is drawn in, its contract's
      // ([superappContract]); a fault in reading it leaves the screen in the
      // plain theme and is reported, and the agent starts all the same.
      try {
        _look = (await Looks.read(glp.rootSelfGlp)).of(superappContract);
      } catch (e, stack) {
        _caught(e, stack);
      }
      final reporter = debugLogOn ? _reporter : null;
      final net = await PhoneNetwork.open(
          persona: _persona,
          middleware: debugLogOn && reporter != null
              ? [layerActions(reporter)]
              : const []);
      _net = net;
      if (debugLogOn) reporter?.setPhone(net.key.hex);
      // The agent's local state is kept across the termination of the app by
      // a store in the app's own directory, which the person does not see
      // (IGLP, Implementation Notes, "Persistence": "a store the host gives
      // it"): a launch after the first goes on from it.  A scripted build
      // keeps its agent's state apart, one store for each scripted person.
      final support = await getApplicationSupportDirectory();
      final kept = script == null ? _persona : '$_persona-${script.mode}';
      final store = FileAgentStateStore('${support.path}/agents/$kept.state');
      // The app installed, as the platform names it, which the runtime keeps
      // with the program and self_app/2 assigns (app_identity.dart).
      final installed = await installedApp();
      final host = SuperAppHost(
        agentId: _persona,
        program: glp.coreDir,
        rootSelfGlpPath: glp.rootSelfGlp,
        network: debugLogOn && reporter != null
            ? ReportingGlpNetwork(net.network, reporter)
            : net.network,
        manifest: superappManifest,
        services: readServices(glp.coreDir),
        firstAct: script?.firstAct,
        stateStore: store,
        installedApp: installed,
      );
      if (debugLogOn) {
        final screen = _screen;
        if (reporter != null && screen != null) {
          reportHost(reporter, host, screen);
        }
      }
      host.ui.onChange = () {
        // A link the phone handed the app is sent once the request form is
        // on the screen (invitation_link.dart).
        widget.links?.deliver(host.ui.bridge);
        if (mounted) setState(() {});
        _screenChanged();
      };
      host.onOutput = (line) {
        if (debugLogOn) reporter?.runtimeOutput(line);
        if (line.startsWith('[ERROR]') && mounted) {
          setState(() => _setStatus(line));
        }
      };
      setState(() {
        _host = host;
        _setStatus(net.transports
            ? 'Ready'
            : 'Ready, with neither Bluetooth nor the local network');
      });
      // The agent's state is saved when the app goes to the background, and
      // at no other time (IGLP d70ec2e, Implementation Notes, "Persistence";
      // [SuperAppHost.save]); a crash loses what the agent did since.
      _background = AppLifecycleListener(onStateChange: (state) {
        if (state == AppLifecycleState.paused) {
          host.save().catchError((Object e, StackTrace stack) {
            _caught(e, stack);
            return false;
          });
        } else if (state == AppLifecycleState.resumed) {
          // The suspension ends: the agent takes events again, and the
          // running mark is set (IGLP db0a117, Persistence, When).
          host.resume().catchError((Object e, StackTrace stack) {
            _caught(e, stack);
          });
        }
      });
      try {
        await host.start();
      } finally {
        // The layer attests the version of the app whose program the agent
        // runs, which the runtime decided at its start (IGLP, Implementation
        // Notes, "The contract"); no session was attested before it.
        await net.attestation.give(host.agent.runningApp);
      }
      if (mounted) setState(() {});
      _screenChanged();
    } catch (e, stack) {
      _caught(e, stack);
      if (mounted) setState(() => _setStatus('Error: $e'));
    }
  }

  @override
  Widget build(BuildContext context) {
    final host = _host;
    // The person sees what the program draws (GSA paper, Appendix "The
    // Operating System", "The person"); the app shows its own line only
    // while the agent is not running, or where the layer or the agent fails.
    return Scaffold(
      body: SafeArea(
        child: host == null
            ? Center(
                child: Text(_status,
                    style: const TextStyle(color: Colors.grey)))
            : Column(
                children: [
                  if (_status != 'Ready')
                    Padding(
                      padding: const EdgeInsets.symmetric(
                          horizontal: 12, vertical: 4),
                      child: Text(_status,
                          style: const TextStyle(
                              fontSize: 11, color: Colors.grey)),
                    ),
                  Expanded(
                    child: KeyedSubtree(
                      key: _surface,
                      child: _look == null
                          ? AgentSurface(agentId: _persona, runtime: host.ui)
                          : Theme(
                              data: _look!.theme(),
                              child: AgentSurface(
                                  agentId: _persona,
                                  runtime: host.ui,
                                  icon: _look!.icon)),
                    ),
                  ),
                ],
              ),
      ),
    );
  }
}
