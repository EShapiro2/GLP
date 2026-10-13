/// The sovereign mini-app's screen: the panel the Dart bridge draws the
/// constructs of `programs/currencies/sovereign/denominated/
/// sovereign_agent.vglp` in, the output thread among them.
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
/// The program's output to its person reaches the person only as the
/// thread of construct 0, its elements drawn by the widget of the type the
/// program declares by the reserved name Output (`Output ::= ScreenMsg`, `programs/currencies/sovereign/self.glp`): "A vGLP program
/// declares the type of its output to its person by defining the type
/// Output, a reserved name; the dispatcher draws that output as a thread
/// whose elements are drawn by the widget of Output" (vGLP 169b008,
/// Definition "Canonical Compilation"; vGLP, 2026-10-13 19:25 UTC, items 2
/// and 3).  No view here names a message or a type.
library;

import '../ui_runtime/manifest.dart';

/// Panel `sovereign`: the constructs the bridge holds open, the output
/// thread among them.
const _sovereignPanel = Panel(
  id: 'sovereign',
  name: 'sovereign',
  constructs: true,
);

/// The sovereign mini-app's manifest.
const Manifest sovereignManifest = Manifest(
  title: 'Sovereign',
  panels: [_sovereignPanel],
  activity: [],
);
