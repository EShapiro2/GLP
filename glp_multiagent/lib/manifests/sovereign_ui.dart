/// The sovereign mini-app's screen: the panel the Dart bridge draws the
/// constructs of `programs/currencies/sovereign/denominated/
/// sovereign_agent.vglp` in, and the views of what the agent writes to its
/// person.
///
/// The program runs by its bridge entry, `sovereign_ui(cb, Grants, _)`
/// (`programs/currencies/sovereign/play_ui.glp`, `(Id, UserIn, NetIn)`),
/// which gives the central bank's mini-app the person channel `ch(Grants?,
/// Draws)` of vGLP's Definition "Person Channel, Person Writer, GLP with
/// Persons, Grant" and sends the draws to the person by `send_to_user/1`.
/// Its constructs are therefore the bridge's (vGLP, sections/elicitation.tex,
/// the paragraph "The implementation"): the dispatcher draws `draw(Id, W, V)`
/// and withdraws `withdraw(Id)`, and the person grants `input(Id, P, R)`.
/// The source declares no display and no widget, so each construct is the
/// default widget of its interactive type (Definition "Widget Declaration,
/// Default Widget"), drawn by the construct family: the agent's question,
/// `Stream(Request)?`, an input box of a menu of the twelve forms of
/// `Request`, one per transaction of SGC's Definition 2.2, kept open, one
/// request per submission; and the two responders' questions, `SwapCard ::=
/// card(SwapOffer, YesNo?)` and `TransferCard ::= card(TransferOffer,
/// YesNo?)`, each in writer mode, a card showing the offer with the buttons
/// yes and no, withdrawn once answered.  Nothing of the deployed mediator's
/// card vocabulary --- its forms, its cards and their answers --- is named
/// here: none of it reaches this program.
///
/// What is named here is the agent's own output to its person, `msg(agent,
/// person, C)`, which the dispatcher merges with the draws: "the lists the
/// program writes --- the friends, the chat threads, the balances --- which
/// the runtime renders as GLP renders every message to the person" (vGLP,
/// sections/ui-primitives.tex).  The holdings, tallied by lot after every
/// change, are the balances view, each tally replacing the last; every other
/// message is a line of the screen, shown by its default display.
library;

import '../ui_runtime/manifest.dart';

/// Panel `sovereign`: the constructs the bridge holds open, then the balances
/// view, then the screen, which takes every message of the agent's the
/// balances view leaves.
const _sovereignPanel = Panel(
  id: 'sovereign',
  name: 'sovereign',
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

/// The sovereign mini-app's manifest.
const Manifest sovereignManifest = Manifest(
  title: 'Sovereign',
  panels: [_sovereignPanel],
  activity: [],
);
