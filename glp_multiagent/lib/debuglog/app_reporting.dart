/// The reporter wired into the Grassroots App (main_superapp.dart): what it
/// reports at launch, and where it reads each event of
/// GLP/phones/docs/debugging.md, "The phone side", item 3, with no change to
/// the agent or the layer --- the host's callbacks for the runtime
/// ([reportHost]), the glue's callbacks for the layer ([ReportingGlpNetwork]),
/// a middleware on the store the app gives the layer for the actions it
/// reduces ([layerActions]), the two assignable `debugPrint`s the layer's
/// diagnostics are printed by ([reportLayerDiagnostics]), and the bridge's
/// screen, read as text after each change ([ScreenReporter]).
library;

import 'dart:io' show Directory;

import 'package:flutter/foundation.dart' as flutter show debugPrint;
import 'package:flutter/widgets.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:grassroots_networking/grassroots_networking.dart' show AppState;
import 'package:grassroots_networking_core/grassroots_networking_core.dart'
    as core show debugPrint;
import 'package:path_provider/path_provider.dart';
import 'package:redux/redux.dart';

import '../isolate_protocol.dart' show screenTermOf;
import '../superapp_host.dart';
import '../ui_runtime/term.dart';
import 'device.dart';
import 'reporter.dart';
import 'screen_text.dart';

export 'reporter.dart' show DebugReporter, debugLogOn;
export 'reporting_network.dart' show ReportingGlpNetwork;

/// The reporter of this launch, in a debuggable build, its buffer in the
/// app's support directory, the launch reported and the core's diagnostics
/// and the framework's caught errors reported from now on; null in a build
/// without `GLP_DEBUGLOG`, and then nothing is opened and nothing is sent.
Future<DebugReporter?> startDebugReporter() async {
  if (!debugLogOn) return null;
  final syslog = flutter.debugPrint;
  try {
    final support = await getApplicationSupportDirectory();
    final reporter = await DebugReporter.open(
      dir: Directory('${support.path}/debuglog'),
      password: debugLogPassword,
      syslog: (m) => syslog(m),
    );
    reportLaunch(reporter, await buildCommit(), deviceFacts());
    reportLayerDiagnostics(reporter);
    final framework = FlutterError.onError;
    FlutterError.onError = (details) {
      reporter.event(Source.app, 'exception', {
        'error': details.exceptionAsString(),
        'stack': details.stack?.toString(),
      });
      framework?.call(details);
    };
    return reporter;
  } on Object catch (e) {
    syslog('debuglog: the reporter is not started: $e');
    return null;
  }
}

/// The launch's first events: its run, the build and the device (item 2);
/// the agent's key follows once the app has it ([DebugReporter.setPhone]).
void reportLaunch(
    DebugReporter reporter, String commit, Map<String, Object?> device) {
  reporter.event(Source.app, 'launch', {'run': reporter.run});
  reporter.event(Source.app, 'build', {'commit': commit});
  reporter.event(Source.app, 'device', device);
}

/// The layer's own diagnostics, every `debugPrint` line of networking/core,
/// of the Flutter package networking/lib (`[lan]`, `[ble]`, `[local-host]`)
/// and of the glue networking/glue (debugging.md, "The phone side", item 3),
/// reported, and still printed as before.  The core prints by its own
/// assignable `debugPrint` (networking/core, compat.dart), the Flutter
/// package by Flutter's, and the glue, which prints nothing today, would
/// print by one of the two; each line is reported with the one it was
/// printed by, `core` or `flutter`.  Flutter's `debugPrint` is the whole
/// app's, so a line of the framework or of the app comes with the layer's.
/// The reporter's own failures go to its [DebugReporter.syslog], which
/// [startDebugReporter] takes as Flutter's `debugPrint` was before this, so
/// that none is reported.  Returns what puts the two back as they were.
void Function() reportLayerDiagnostics(DebugReporter reporter) {
  final coreWas = core.debugPrint;
  final flutterWas = flutter.debugPrint;
  core.debugPrint = (String? message, {int? wrapWidth}) {
    coreWas(message, wrapWidth: wrapWidth);
    reporter.event(Source.layer, 'diagnostic',
        {'printer': 'core', 'line': message ?? ''});
  };
  flutter.debugPrint = (String? message, {int? wrapWidth}) {
    flutterWas(message, wrapWidth: wrapWidth);
    reporter.event(Source.layer, 'diagnostic',
        {'printer': 'flutter', 'line': message ?? ''});
  };
  return () {
    core.debugPrint = coreWas;
    flutter.debugPrint = flutterWas;
  };
}

/// The store's middleware that reports every action the layer's store
/// reduces, by its type and, where the action prints more than its type, its
/// print, and hands it on unchanged.
Middleware<AppState> layerActions(DebugReporter reporter) =>
    (Store<AppState> store, dynamic action, NextDispatcher next) {
      final type = action.runtimeType.toString();
      final printed = action.toString();
      reporter.event(Source.layer, 'action', {
        'action': type,
        if (printed != "Instance of '$type'") 'detail': printed,
      });
      next(action);
    };

/// The runtime's and the person's events of [host]: the agent's trace
/// (`onLog`), each term it sends its person (`onUserOutput`), and each act
/// the person takes on the screen.  The agent's transcript (`onOutput`) is
/// the app's own callback, which hands each line to
/// [DebugReporter.runtimeOutput].
void reportHost(DebugReporter reporter, SuperAppHost host, ScreenReporter screen) {
  host.onLog = (tag, message) {
    reporter.event(Source.runtime, 'log', {'tag': tag, 'message': message});
  };
  host.onUserOutput = (rt.Term term) {
    reporter.event(
        Source.runtime, 'userOutput', {'term': formatTerm(screenTermOf(term))});
  };
  host.onAct = screen.act;
}

/// The screen as the bridge draws it, reported whenever its text changes,
/// and the person's acts on it.
class ScreenReporter {
  final DebugReporter reporter;

  ScreenReporter(this.reporter);

  String? _last;
  Map<int?, List<String>> _texts = const {};

  /// Read the screen under [root] and report it where its text differs from
  /// the last reported.
  void screen(Element root) {
    final constructs = screenText(root);
    final json = [for (final c in constructs) c.toJson()];
    final key = json.toString();
    if (key == _last) return;
    _last = key;
    _texts = {for (final c in constructs) c.id: c.texts};
    reporter.event(Source.screen, 'screen', {'constructs': json});
  }

  /// Read the screen under [context]'s element after the frame being built,
  /// once the bridge has redrawn.
  void afterFrame(BuildContext Function() context) {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      try {
        final c = context();
        if (c is Element && c.mounted) screen(c);
      } on Object catch (e) {
        reporter.syslog('debuglog: the screen is not read: $e');
      }
    });
  }

  /// The person's act [grant], as the bridge sends it: `input(Id, P, R)`,
  /// the answer R at the question at P of construct Id, reported with the
  /// texts that construct showed when the person acted; any other act, as it
  /// is.
  void act(GTerm grant) {
    int? id;
    if (grant is GStruct &&
        grant.functor == 'input' &&
        grant.args.length == 3 &&
        grant.args[0] is GInt) {
      id = (grant.args[0] as GInt).value;
    }
    reporter.event(Source.screen, 'act', {
      'act': formatTerm(grant),
      if (id != null) 'construct': id,
      if (id != null) 'texts': _texts[id],
    });
  }
}
