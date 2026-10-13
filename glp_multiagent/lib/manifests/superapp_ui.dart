/// The Grassroots Super-App's own screen: the panel the Dart bridge draws the
/// constructs of `programs/social/graph/core/home.vglp` in, the output thread
/// among them.
///
/// The program runs by its bridge entry, `superapp_ui(alice, Grants, _)`
/// (`programs/social/graph/core/home_ui.glp`, `(Id, UserIn, NetIn)`), which
/// gives the super-app's person interface the person channel `ch(Grants?,
/// Draws)` of vGLP's Definition "Person Channel, Person Writer, GLP with
/// Persons, Grant" and sends the draws to the person by `send_to_user/1`, as
/// `superapp/3` does (GSG Section 5.1).  Its constructs are therefore the
/// bridge's (vGLP, sections/elicitation.tex, the paragraph "The
/// implementation"): the dispatcher draws `draw(Id, W, V)` and withdraws
/// `withdraw(Id)`, and the person grants `input(Id, P, R)`.  The source
/// declares no display and no widget, so each construct is the default widget
/// of its interactive type (Definition "Widget Declaration, Default Widget"),
/// drawn by the construct family: `home`'s question, `Stream(Request)?`, an
/// input box of a menu of the four forms of `Request ::= trust(CompilerKey) ;
/// install(AppName, Artefact) ; invite(AppName, Friend) ; connect(Friend)`,
/// each field labelled by the name of its declared type, a `Key` a peer
/// field, kept open, one request per submission (Remark "Persistence"); and
/// the question of `respond_friend`, `FriendRequest ::= from(Friend,
/// Decision?)` in writer mode, a card showing the friend who asks to connect
/// with the buttons yes and no, withdrawn once answered.  Nothing of the
/// deployed mediator's vocabulary --- its forms, its cards and their answers
/// --- is named here: none of it reaches this program.
///
/// The program's output to its person reaches the person only as the
/// thread of construct 0, its elements drawn by the widget of the type the
/// program declares by the reserved name Output (`Output ::= Notice`, `programs/social/graph/core/self.glp`, each
/// notice what the agent tells the person with the pending value of a
/// befriending dropped (GSG, 2026-10-13 20:16 UTC)): "A vGLP program
/// declares the type of its output to its person by defining the type
/// Output, a reserved name; the dispatcher draws that output as a thread
/// whose elements are drawn by the widget of Output" (vGLP 169b008,
/// Definition "Canonical Compilation"; vGLP, 2026-10-13 19:25 UTC, items 2
/// and 3).  No view here names a message or a type.
library;

import '../ui_runtime/manifest.dart';

/// Panel `apps`: the constructs the bridge holds open, the output thread
/// among them.
const _appsPanel = Panel(
  id: 'apps',
  name: 'apps',
  constructs: true,
);

/// The Grassroots Super-App's manifest.
const Manifest superappManifest = Manifest(
  title: 'Apps',
  panels: [_appsPanel],
  activity: [],
);
