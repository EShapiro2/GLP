/// The Grassroots Super-App's own screen: the panel the Dart bridge draws the
/// constructs of `programs/social/graph/core/home.vglp` in, and the views of
/// what the super-app tells its person.
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
/// What is named here is what the super-app tells its person, `msg(agent,
/// person, C)` with `C` an `AgentContent` (`programs/social/graph/self.glp`),
/// which `superapp/3` sends onto the program's end of the person channel and
/// the dispatcher merges with the draws: the friends the person connects
/// with, the friends view, and every other message a line of the screen,
/// shown by its default display.
library;

import '../ui_runtime/manifest.dart';

/// Panel `apps`: the constructs the bridge holds open, then the friends
/// view, then the screen, which takes every message of the super-app's the
/// friends view leaves.
const _appsPanel = Panel(
  id: 'apps',
  name: 'apps',
  views: [
    ScreenView(
      pattern: 'msg(agent, person, connected(Friend))',
      content: 'Friend',
      kind: ViewKind.friends,
      label: 'Friends',
      store: 'friends',
    ),
    ScreenView(
      pattern: 'msg(agent, person, S)',
      content: 'S',
      kind: ViewKind.list,
      label: 'Screen',
      store: 'screen',
    ),
  ],
);

/// The Grassroots Super-App's manifest.
const Manifest superappManifest = Manifest(
  title: 'Apps',
  panels: [_appsPanel],
  activity: [],
);
