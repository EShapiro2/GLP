/// The sovereign mini-app on a live person's screen — one phone, one person,
/// the interface derived from the program.
///
/// `programs/currencies/sovereign` at `sovereign_ui/3`: the central bank's
/// execution of the denominated bond agent with its mediator, and its two
/// counterparties' — Diana the community bank and Frank the household,
/// scripted — over the three conversations of the sovereign market cut to
/// them. Nothing on this screen is written here — [sovereignManifest] is the
/// image of the display declarations of
/// `programs/currencies/sovereign/denominated/sovereign_agent.vglp`, and the
/// shell is [runVglpApp], which carries any compiled vGLP program.
///
/// `sovereign_ui/3` is the program's live-person harness, Currencies' own in
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
      friends: const ['diana', 'frank'],
    ));
