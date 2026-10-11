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
/// What is named here is the agent's own output to its person, `msg(agent,
/// person, C)` with `C` a `ScreenContent` (`programs/currencies/coins/
/// self.glp`), which the dispatcher merges with the draws: "the lists the
/// program writes --- the friends, the chat threads, the balances --- which
/// the runtime renders as GLP renders every message to the person" (vGLP,
/// sections/ui-primitives.tex).  The holdings, tallied by issuer after every
/// change, are the balances view, each tally replacing the last; every other
/// message is a line of the screen, shown by its default display.
library;

import '../ui_runtime/manifest.dart';

/// Panel `coins`: the constructs the bridge holds open, then the balances
/// view, then the screen, which takes every message of the agent's the
/// balances view leaves.
const _coinsPanel = Panel(
  id: 'coins',
  name: 'coins',
  views: [
    ScreenView(
      pattern: 'msg(agent, person, holdings(Lots))',
      content: 'Lots',
      kind: ViewKind.balances,
      label: 'Balances',
      store: 'balances',
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

/// The currency mini-app's manifest.
const Manifest coinsManifest = Manifest(
  title: 'Coins',
  panels: [_coinsPanel],
  activity: [],
);
