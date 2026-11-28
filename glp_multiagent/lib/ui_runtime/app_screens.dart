/// The screens of the mini-apps installed on the super-app (EShapiro2/GLP#181).
///
/// The super-app gives each mini-app the person installs a screen of its own:
/// the person's end of its person channel is the person's
/// (`programs/social/graph/core/self.glp`, app_screens/4), and its draws reach
/// the person among home's, each tagged `app(App, D)` with the name the
/// mini-app is installed under.  [AppScreens] takes those terms off what the
/// agent sends its person, keeps one [UiRuntime] for each mini-app, and sends
/// the person's grants on a mini-app's screen back tagged `app(App, G)`, which
/// the super-app routes to that mini-app alone.  Home's own draws are not its.
library;

import 'package:flutter/foundation.dart';

import 'bridge.dart';
import 'manifest.dart';
import 'runtime.dart';
import 'term.dart';

/// The constructor of a term tagged for a mini-app's screen.
const String appTag = 'app';

/// The screens of the installed mini-apps, by the names they are installed under.
class AppScreens extends ChangeNotifier {
  /// Sends one act of the person to the agent, as the super-app's screen sends
  /// its grants.
  final void Function(GTerm act) send;

  final Map<String, UiRuntime> _screens = {};

  AppScreens({required this.send});

  /// The names of the mini-apps with a screen, in the order their screens opened.
  List<String> get apps => List.unmodifiable(_screens.keys);

  /// The screen of the mini-app installed as [app], or null.
  UiRuntime? screen(String app) => _screens[app];

  /// Takes one term the agent sent its person: where it is tagged for a
  /// mini-app's screen, `app(App, D)`, hands `D` to that screen, opening it
  /// at its first term, and returns true; every other term is home's.
  bool handle(GTerm term) {
    final (ctor, args) = ctorArgs(term);
    if (ctor != appTag || args.length != 2) return false;
    final name = _name(args[0]);
    if (name == null) return false;
    final ui = _screens.putIfAbsent(name, () => _open(name));
    ui.handle(args[1]);
    notifyListeners();
    return true;
  }

  UiRuntime _open(String name) {
    late final UiRuntime ui;
    ui = UiRuntime(
      manifest: Manifest(title: name, panels: [Panel(id: name, name: name, constructs: true)], activity: const []),
      onSend: (grant) => send(GStruct(appTag, [GAtom(name), grant])),
    );
    ui.onChange = notifyListeners;
    return ui;
  }

  /// How many of the questions on [app]'s screen wait for the person: the
  /// approval questions open on it, each another person's act that waits on
  /// the person's consent.
  int waiting(String app) {
    final ui = _screens[app];
    if (ui == null) return 0;
    var n = 0;
    for (final c in ui.bridge.constructs) {
      final (f, args) = ctorArgs(c.view);
      if (f == 'approval' && (args.length == 2 || args.length == 3) && questionAt(args.last) != null) n++;
    }
    return n;
  }

  static String? _name(GTerm t) => switch (t) {
        GAtom(:final name) => name,
        GString(:final value) => value,
        _ => null,
      };
}
