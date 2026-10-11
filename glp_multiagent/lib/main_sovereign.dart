/// The sovereign mini-app on a live person's screen — one phone, one person,
/// the interface derived from the program.
///
/// `programs/currencies/sovereign` at `sovereign_ui/3`: the central bank's
/// execution of the denominated bond agent on the person channel of vGLP's
/// Definition "Person Channel, Person Writer, GLP with Persons, Grant", and
/// its two counterparties' --- Diana the community bank and Frank the
/// household, scripted --- over the three conversations of the sovereign
/// market cut to them.  Nothing on this screen is written here: the bridge
/// draws the constructs the compiled program's dispatcher draws, each the
/// default widget of its interactive type, and returns the central bank's
/// input as grants; [sovereignManifest] views what the agent writes to it;
/// and the shell is [runVglpApp], which carries any compiled vGLP program.
///
/// `sovereign_ui/3` is the program's bridge entry, Currencies' own in
/// `programs/currencies/sovereign/play_ui.glp` and exported from that
/// directory's `self.glp`: the entry the Flutter agent runtime starts, whose
/// three arguments are (Id, UserIn, NetIn) — the same contract `coins_ui/3`
/// meets in `programs/currencies/coins/play_ui.glp`. Its one clause is for
/// `cb`, so the central bank is the person this phone boots, and its calendar
/// is empty until the app attaches a clock.
library;

import 'manifests/sovereign_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Sovereign',
      person: 'cb',
      directory: (glp) => glp.sovereignDir,
      goalLabel: 'sovereign_ui/3',
      manifest: sovereignManifest,
    ));
