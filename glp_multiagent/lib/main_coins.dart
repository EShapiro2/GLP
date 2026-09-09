/// The currency mini-app on a live person's screen — one phone, one person,
/// the interface derived from the program.
///
/// `programs/coins` at `coins_ui/3`: alice's execution of Currencies' mini-app
/// with its mediator, and bob's, scripted, over one conversation. Nothing on
/// this screen is written here — [coinsManifest] is the image of the display
/// declarations of `programs/coins/currency/coins_agent.vglp`, and the shell
/// is [runVglpApp], which carries any compiled vGLP program.
library;

import 'manifests/coins_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Coins',
      person: 'alice',
      directory: (glp) => glp.coinsDir,
      goalLabel: 'coins_ui/3',
      manifest: coinsManifest,
      friends: const ['bob'],
    ));
