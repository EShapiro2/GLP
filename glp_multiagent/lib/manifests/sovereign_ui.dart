/// The sovereign mini-app's screen, the image of the display declarations of
/// `programs/currencies/sovereign/denominated/sovereign_agent.vglp`.
///
/// Nothing here is invented. Every element is derived from one of the twelve
/// `panel(sovereign)` declarations under vGLP's Definitions "Display
/// Declaration", "Manifest" and "Canonical Compilation", by the derivation
/// vGLP wrote out in
/// `/Grassroots/vGLP/docs/sovereign-manifest-derivation.md`. Seven persistent
/// compose forms, two transient cards and one view:
///
/// ```
/// display agent *(F, K, D) : panel(sovereign), label("Mint"),
///     field(F, text), field(K, number), field(D, date), persistent.
/// display agent *(Q, F, U, D, K, V, D1, K1) : panel(sovereign), label("Swap"),
///     field(Q, peer), field(F, text), field(U, peer), field(D, date),
///     field(K, number), field(V, peer), field(D1, date), field(K1, number),
///     persistent.
/// display agent *(Q, F, D, K) : panel(sovereign), label("Pay"), ... persistent.
/// display agent *(Q, F, D, R, D1) : panel(sovereign), label("Redeem"), ... persistent.
/// display agent *(E, F, U, D, K) : panel(sovereign), label("Deposit"), ... persistent.
/// display agent *(Q, F, U, D, K) : panel(sovereign), label("Release"), ... persistent.
/// display agent *(P, F, U, D, K) : panel(sovereign), label("Return"), ... persistent.
/// display respond_swap *(yes, From?, Want?, Offered?) : panel(sovereign),
///     label("Accept"), transient.
/// display respond_transfer *(yes, Kind?, From?, Bonds?) : panel(sovereign),
///     label("Accept"), transient.
/// display msg(agent, person, holdings(Lots)) : panel(sovereign), view(balances).
/// ```
///
/// The clause names and answer functors are the compilation's, and the source
/// and its compilation on disc carry them since Currencies' re-emission of
/// `sovereign_agent.vglp` under vGLP's ruling of 2026-09-18: the hand-written
/// decline siblings are out of both responders — where the transaction a
/// clause implements has an else-effect, the clause's else-branch is its
/// image, no decline sibling is written beside it, and an else-branch has no
/// display declaration of its own — and the compilation numbers a procedure's
/// volition-guarded clauses in textual order. So the accept clause of each
/// responder is `respond_swap_1` and `respond_transfer_1`, each with
/// `Reply_C ::= then(Xs_C) ; else`, and its card carries one Accept and the
/// decline the runtime derives from [InboxDesc.elseBranch]; the seven request
/// clauses of `agent` are `agent_1` to `agent_7` in the order they are
/// written, Mint to Return.
///
/// One thing this manifest takes from the ruling rather than from the source
/// as it stands on disc today: **the field labels.** The declarations on disc
/// use the two-argument `field(X, W)`, under which each label is the writer's
/// name and the Swap form reads Q, F, U, D, K, V, D1, K1. The three-argument
/// `field(X, W, L)` is the remedy, and the strings below are Currencies', read
/// off the derivation's list of what each position is.
library;

import '../ui_runtime/manifest.dart';
import '../ui_runtime/term.dart';

/// The seven request clauses of `agent`, perpetually pending: each a compose
/// form bound to the standing card of its clause. Their contexts are the
/// constants `ctx_agent_1` to `ctx_agent_7`, so they show no content.
///
/// One denomination variable runs through every clause, so no act of this
/// program moves bonds of two denominations: a form with two lots, Swap, has
/// one denomination field.
const _mint = CommandDesc(
  ctor: '',
  clause: 'agent_1',
  answerCtor: 'xs_agent_1',
  label: 'Mint',
  args: [
    FieldDesc('F', FieldType.text, 'Denomination'),
    FieldDesc('K', FieldType.number, 'Number of bonds'),
    FieldDesc('D', FieldType.date, 'Maturity'),
  ],
);

const _swap = CommandDesc(
  ctor: '',
  clause: 'agent_2',
  answerCtor: 'xs_agent_2',
  label: 'Swap',
  args: [
    FieldDesc('Q', FieldType.peer, 'Counterparty'),
    FieldDesc('F', FieldType.text, 'Denomination'),
    FieldDesc('U', FieldType.peer, 'Issuer of the bonds given'),
    FieldDesc('D', FieldType.date, 'Maturity of the bonds given'),
    FieldDesc('K', FieldType.number, 'Number of the bonds given'),
    FieldDesc('V', FieldType.peer, 'Issuer of the bonds wanted'),
    FieldDesc('D1', FieldType.date, 'Maturity of the bonds wanted'),
    FieldDesc('K1', FieldType.number, 'Number of the bonds wanted'),
  ],
);

const _pay = CommandDesc(
  ctor: '',
  clause: 'agent_3',
  answerCtor: 'xs_agent_3',
  label: 'Pay',
  args: [
    FieldDesc('Q', FieldType.peer, 'Payee'),
    FieldDesc('F', FieldType.text, 'Denomination'),
    FieldDesc('D', FieldType.date, 'Maturity'),
    FieldDesc('K', FieldType.number, 'Number'),
  ],
);

const _redeem = CommandDesc(
  ctor: '',
  clause: 'agent_4',
  answerCtor: 'xs_agent_4',
  label: 'Redeem',
  args: [
    FieldDesc('Q', FieldType.peer, 'Issuer presented to'),
    FieldDesc('F', FieldType.text, 'Denomination'),
    FieldDesc('D', FieldType.date, 'Maturity of the coin presented'),
    FieldDesc('R', FieldType.peer, 'Issuer wanted'),
    FieldDesc('D1', FieldType.date, 'Maturity wanted'),
  ],
);

const _deposit = CommandDesc(
  ctor: '',
  clause: 'agent_5',
  answerCtor: 'xs_agent_5',
  label: 'Deposit',
  args: [
    FieldDesc('E', FieldType.peer, 'Escrow agent'),
    FieldDesc('F', FieldType.text, 'Denomination'),
    FieldDesc('U', FieldType.peer, 'Issuer'),
    FieldDesc('D', FieldType.date, 'Maturity'),
    FieldDesc('K', FieldType.number, 'Number'),
  ],
);

const _release = CommandDesc(
  ctor: '',
  clause: 'agent_6',
  answerCtor: 'xs_agent_6',
  label: 'Release',
  args: [
    FieldDesc('Q', FieldType.peer, 'Beneficiary'),
    FieldDesc('F', FieldType.text, 'Denomination'),
    FieldDesc('U', FieldType.peer, 'Issuer'),
    FieldDesc('D', FieldType.date, 'Maturity'),
    FieldDesc('K', FieldType.number, 'Number'),
  ],
);

const _return = CommandDesc(
  ctor: '',
  clause: 'agent_7',
  answerCtor: 'xs_agent_7',
  label: 'Return',
  args: [
    FieldDesc('P', FieldType.peer, 'Depositor'),
    FieldDesc('F', FieldType.text, 'Denomination'),
    FieldDesc('U', FieldType.peer, 'Issuer'),
    FieldDesc('D', FieldType.date, 'Maturity'),
    FieldDesc('K', FieldType.number, 'Number'),
  ],
);

/// The swap responder's one transient clause, its context
/// `ctx_respond_swap_1(From, Want, Offered)`. Its question has one position,
/// the ground `yes`, which the Accept button carries; `From`, `Want` and
/// `Offered` are context and carry no field.
///
/// The clause's reply type is `Reply_respond_swap_1 ::= then(Xs) ; else`, so
/// the card carries a decline: the runtime derives it from [elseBranch], and
/// the person's tap grants `decline(ReqId)`, which selects the else-branch —
/// the clause's own refusal, and the image of the transaction's else-effect.
const _respondSwap = InboxDesc(
  clauses: ['respond_swap_1'],
  elseBranch: ['respond_swap_1'],
  args: ['From', 'Want', 'Offered'],
  itemKey: 'From',
  title: '',
  answers: [
    AnswerDesc(
      label: 'Accept',
      cmdCtor: '',
      clause: 'respond_swap_1',
      answerCtor: 'xs_respond_swap_1',
      fill: [ConstFill(GAtom('yes'))],
    ),
  ],
);

/// The transfer responder — the receiver's guard of a deposit, a release or a
/// return — its context `ctx_respond_transfer_1(Kind, From, Bonds)`, the kind
/// what the card shows. One Accept and a decline, as the swap card.
const _respondTransfer = InboxDesc(
  clauses: ['respond_transfer_1'],
  elseBranch: ['respond_transfer_1'],
  args: ['Kind', 'From', 'Bonds'],
  itemKey: 'From',
  title: '',
  answers: [
    AnswerDesc(
      label: 'Accept',
      cmdCtor: '',
      clause: 'respond_transfer_1',
      answerCtor: 'xs_respond_transfer_1',
      fill: [ConstFill(GAtom('yes'))],
    ),
  ],
);

/// Panel `sovereign`: the seven forms, the two cards, the balances view, and
/// the default display — a list in the program's panel — which takes the
/// twenty-seven other alternatives of `ScreenContent`.
const _sovereignPanel = Panel(
  id: 'sovereign',
  name: 'sovereign',
  commands: [_mint, _swap, _pay, _redeem, _deposit, _release, _return],
  inbox: [_respondSwap, _respondTransfer],
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
