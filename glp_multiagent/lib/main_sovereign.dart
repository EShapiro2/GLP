/// The sovereign mini-app on a live person's screen — one phone, one person,
/// the interface derived from the program.
///
/// `programs/currencies/sovereign`: the central bank's
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
/// The agent is started from agent(ch(_?,_),ch(_?,_)), every agent's initial
/// goal (GLP-Spec maglp.tex, Definition "Multiagent GLP"; IGLP,
/// Implementation Notes, "Boot"), which the program's agent/2, Currencies',
/// answers.  Until 2026-11-03 the runtime started it at `sovereign_ui/3`,
/// Currencies' bridge entry in `programs/currencies/sovereign/play_ui.glp`,
/// whose three arguments were (Id, UserIn, NetIn).
library;

import 'manifests/sovereign_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Sovereign',
      person: 'cb',
      directory: (glp) => glp.sovereignDir,
      manifest: sovereignManifest,
    ));
