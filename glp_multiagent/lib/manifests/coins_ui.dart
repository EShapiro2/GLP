/// The currency mini-app's screen: the panel the Dart bridge draws the
/// constructs of `programs/currencies/coins/currency/coins_agent.vglp` in,
/// and the views of what the agent writes to its person.
///
/// The program runs by its bridge entry, `coins_ui(alice, Grants, _)`
/// (`programs/currencies/coins/play_ui.glp`, `(Id, UserIn, NetIn)`), which
/// gives the mini-app the person channel `ch(Grants?, Draws)` of vGLP's
/// Definition "Person Channel, Person Writer, GLP with Persons, Grant" and
/// sends the draws to the person by `send_to_user/1`.  Its constructs are
/// therefore the bridge's (vGLP, sections/elicitation.tex, the paragraph "The
/// implementation": "the bridge draws each construct's widget and returns
/// the person's raw input as grants"): the dispatcher draws `draw(Id, W, V)`
/// and withdraws `withdraw(Id)`, and the person grants `input(Id, P, R)`.
/// The source declares no display and no widget, so each construct is the
/// default widget of its interactive type (Definition "Widget Declaration,
/// Default Widget"), drawn by the construct family: the agent's question,
/// `Stream(Request)?`, an input box of a menu of the four forms of `Request
/// ::= mint(Integer) ; swap(Peer, Lot, Lot) ; pay(Peer, Integer) ;
/// redeem(Peer, Peer)`, kept open, one request per submission; and the swap
/// responder's question, `Card ::= card(Offer, YesNo?)` in writer mode, a
/// card showing the offer with the buttons yes and no, withdrawn once
/// answered.  Nothing of the deployed mediator's card vocabulary --- its
/// forms, its cards and their answers --- is named here: none of it reaches
/// this program.
///
/// The agent's own output to its person, `msg(agent, person, C)` with `C` a
/// `ScreenContent`, is of the type the program declares by the reserved name
/// `Output ::= ScreenMsg` (`programs/currencies/coins/self.glp`), and reaches
/// the person only as the thread of construct 0, its elements drawn by
/// Output's widget: "A vGLP program declares the type of its output to its
/// person by defining the type Output, a reserved name; the dispatcher draws
/// that output as a thread whose elements are drawn by the widget of Output"
/// (vGLP 169b008, Definition "Canonical Compilation"; vGLP, 2026-10-13 19:25
/// UTC, items 2 and 3).  No view here names a message or a type.
library;

import '../ui_runtime/manifest.dart';

/// Panel `coins`: the constructs the bridge holds open, the output thread
/// among them.
const _coinsPanel = Panel(
  id: 'coins',
  name: 'coins',
  constructs: true,
);

/// The currency mini-app's manifest.
const Manifest coinsManifest = Manifest(
  title: 'Coins',
  panels: [_coinsPanel],
  activity: [],
);
