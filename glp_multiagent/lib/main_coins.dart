/// The currency mini-app on a live person's screen — one phone, one person,
/// the interface derived from the program.
///
/// `programs/currencies/coins` at `coins_ui/3`, the bridge entry of
/// `programs/currencies/coins/play_ui.glp`: alice's execution of Currencies'
/// mini-app on the person channel of vGLP's Definition "Person Channel,
/// Person Writer, GLP with Persons, Grant", and bob's, scripted, over one
/// conversation.  Nothing on this screen is written here: the bridge draws
/// the constructs the compiled program's dispatcher draws, each the default
/// widget of its interactive type, and returns alice's input as grants;
/// [coinsManifest] views what the agent writes to her; and the shell is
/// [runVglpApp], which carries any compiled vGLP program.
library;

import 'manifests/coins_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Coins',
      person: 'alice',
      directory: (glp) => glp.coinsDir,
      goalLabel: 'coins_ui/3',
      manifest: coinsManifest,
    ));
