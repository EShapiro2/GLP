/// The Grassroots Super-App on a live person's screen — the person's own acts
/// on their super-app, derived from the program.
///
/// `programs/social/graph/core` at `superapp_ui/3`: alice's social-graph agent
/// with the person interface of `home.vglp` and its mediator, and a scripted
/// bob across a crossbar so that Connect and Invite have someone to reach.
/// Nothing on this screen is written here — [superappManifest] is the image of
/// the display declarations of `home.vglp`, and the shell is [runVglpApp].
library;

import 'manifests/superapp_ui.dart';
import 'vglp_app.dart';

void main() => runVglpApp(VglpProgram(
      title: 'Apps',
      person: 'alice',
      directory: (glp) => glp.coreDir,
      goalLabel: 'superapp_ui/3',
      manifest: superappManifest,
      friends: const ['bob'],
    ));
