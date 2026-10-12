/// The Grassroots Super-App on a live person's screen — the person's own acts
/// on their super-app, the interface derived from the program.
///
/// `programs/social/graph/core` at `superapp_ui/3`, the bridge entry of
/// `programs/social/graph/core/home_ui.glp`: alice's social-graph agent with
/// the person interface of `home.vglp` on the person channel of vGLP's
/// Definition "Person Channel, Person Writer, GLP with Persons, Grant", and a
/// scripted bob across a crossbar so that Connect and Invite have someone to
/// reach.  Nothing on this screen is written here: the bridge draws the
/// constructs the compiled program's dispatcher draws, each the default
/// widget of its interactive type, and returns alice's input as grants;
/// [superappManifest] views what the super-app tells her; and the shell is
/// [runVglpApp].
library;

import 'manifests/superapp_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Apps',
      person: 'alice',
      directory: (glp) => glp.coreDir,
      goalLabel: 'superapp_ui/3',
      manifest: superappManifest,
    ));
