/// The Grassroots Super-App as a demonstration: `superapp_ui/4` of
/// `programs/social/graph/core/home_ui.glp`, the person live and her peers
/// scripted in one heap, so that Connect and Invite have someone to reach
/// with no other phone.  Run it with `flutter run -t
/// lib/main_superapp_ui.dart`; the app's default, `main.dart`, is the
/// Grassroots App on the real network (`main_superapp.dart`).
///
/// alice's social-graph agent with the person interface of `home.vglp` on the
/// person channel of vGLP's Definition "Person Channel, Person Writer, GLP
/// with Persons, Grant", and a scripted bob across a crossbar.  Nothing on
/// this screen is written here: the bridge draws the constructs the compiled
/// program's dispatcher draws, each the default widget of its interactive
/// type, and returns alice's input as grants; [superappManifest] views what
/// the super-app tells her; and the shell is [runVglpApp].
///
/// The agent is started from agent(ch(_?,_),ch(_?,_)), every agent's initial
/// goal (GLP-Spec maglp.tex, Definition "Multiagent GLP"; IGLP,
/// Implementation Notes, "Boot"), which the program's agent/2, its owner's,
/// answers.  At the agent's first start the application sends on the person
/// channel the act that starts the demonstration, `superapp_ui(Me, [])`, Me
/// the agent's key and no peer yet (IGLP b014a9e, Implementation Notes,
/// "Boot"); core's agent/2 starts superapp_ui/4 on it, and the person's acts
/// after it are the grants of the super-app's person channel.
library;

import 'manifests/superapp_ui.dart';
import 'ui_runtime/look.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Apps',
      contract: superappContract,
      person: 'alice',
      directory: (glp) => glp.coreDir,
      manifest: superappManifest,
      entry: 'superapp_ui',
    ));
