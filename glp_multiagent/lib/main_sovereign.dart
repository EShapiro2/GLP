/// The sovereign mini-app on a live person's screen — one phone, one person,
/// the interface derived from the program.
///
/// `programs/currencies/sovereign` at `sovereign_ui/3`: alice's execution of
/// the denominated bond agent with its mediator, and her counterparties',
/// scripted, over their conversations. Nothing on this screen is written here
/// — [sovereignManifest] is the image of the display declarations of
/// `programs/currencies/sovereign/denominated/sovereign_agent.vglp`, and the
/// shell is [runVglpApp], which carries any compiled vGLP program.
///
/// `sovereign_ui/3` is the program's live-person harness, the entry the
/// Flutter agent runtime starts, whose three arguments are (Id, UserIn,
/// NetIn) — the same contract `coins_ui/3` meets in
/// `programs/currencies/coins/play_ui.glp`. It is Currencies' source and is
/// not on disc yet: `programs/currencies/sovereign/self.glp` exports
/// `village/0`, `sovereign_village/0` and `credit_line/0` and no live-person
/// entry, and `miniapp#sovereign/4` takes a calendar stream besides, which
/// such a harness has to drive.
library;

import 'manifests/sovereign_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Sovereign',
      person: 'alice',
      directory: (glp) => glp.sovereignDir,
      goalLabel: 'sovereign_ui/3',
      manifest: sovereignManifest,
      friends: const ['bob'],
    ));
