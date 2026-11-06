/// The Grassroots Super-App on a live person's screen — the person's own acts
/// on their super-app, the interface derived from the program.
///
/// `programs/social/graph/core`, until 2026-11-03 at `superapp_ui/3`, the
/// bridge entry of `programs/social/graph/core/home_ui.glp`: alice's social-graph agent with
/// the person interface of `home.vglp` on the person channel of vGLP's
/// Definition "Person Channel, Person Writer, GLP with Persons, Grant", and a
/// scripted bob across a crossbar so that Connect and Invite have someone to
/// reach.  Nothing on this screen is written here: the bridge draws the
/// constructs the compiled program's dispatcher draws, each the default
/// widget of its interactive type, and returns alice's input as grants;
/// [superappManifest] views what the super-app tells her; and the shell is
/// [runVglpApp].
///
/// The agent is started from agent(ch(_?,_),ch(_?,_)), every agent's initial
/// goal (GLP-Spec maglp.tex, Definition "Multiagent GLP"; IGLP,
/// Implementation Notes, "Boot"), which the program's agent/2, its
/// owner's, answers; until 2026-11-03 the runtime started it at `superapp_ui/3`.
/// At the agent's first start the application sends on the person channel the
/// act that starts the super-app, `superapp_ui(Me, [])`, Me the agent's key
/// and no peer yet, later peers coming by acquaintance (IGLP b014a9e,
/// Implementation Notes, "Boot"); core's agent/2 starts superapp_ui/4 on it,
/// and the person's acts after it are the grants of the super-app's person
/// channel.
library;

import 'manifests/superapp_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Apps',
      person: 'alice',
      directory: (glp) => glp.coreDir,
      manifest: superappManifest,
      entry: 'superapp_ui',
    ));
