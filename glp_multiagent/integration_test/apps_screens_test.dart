/// The screens of GSC's three apps and of GAI's app (`programs/apps/<app>/screens/`), and of CSSN's mini-app (`programs/cssn/figures/`), taken on the iOS Simulator: the scenario of `test/apps_screens_test.dart`, [appsScreens], played on the simulator's screen, one person's phone on it at a time, and at each screen the host asked to take the simulator's screenshot, which it frames as the phone and writes under the screen's name (`tool/sim_shot.py`).
///
/// Run by `tool/sim_screens.sh`, which boots the simulator, sets its status bar, serves the screenshots and passes the run what it needs: `GLP_PROGRAMS`, the checkout's `programs/`, which the simulator's app reads from the Mac's filesystem as the headless test reads it; `GLP_SHOTS`, the host's screenshot service; and `FLUTTER_ROOT`, whose fonts the screens are set in.
library;

import 'package:integration_test/integration_test.dart';

import '../test/apps_screens_test.dart' show Screens, appsScreens;

void main() {
  IntegrationTestWidgetsFlutterBinding.ensureInitialized();
  const programs = String.fromEnvironment('GLP_PROGRAMS');
  const shots = String.fromEnvironment('GLP_SHOTS');
  if (programs.isEmpty || shots.isEmpty) {
    throw StateError('run by tool/sim_screens.sh, which passes '
        '--dart-define=GLP_PROGRAMS=... and --dart-define=GLP_SHOTS=...');
  }
  appsScreens(Screens.simulator(programs, Uri.parse(shots)));
}
