/// Per-app UI manifest schema (paper §7.4).
///
/// A manifest is declarative data — no Dart logic. It maps a mediator's
/// `UserCmd`/`UserNotify` constructors to the three generic surfaces: outbox
/// forms, inbox cards, and activity rules. The generic runtime in this package
/// renders strictly from a manifest and names no app-specific constructor; all
/// app specifics (GSG, bonds, child-safe) live in manifest instances elsewhere.
library;

import 'term.dart';

/// Input widget kind for an outbox form field.
enum FieldType { person, text, integer }

/// One argument of an outbox command form.
class FieldDesc {
  final String name;
  final FieldType type;
  final String label;
  const FieldDesc(this.name, this.type, this.label);
}

/// An outbox form: a free `UserCmd` (no escrowed `ReqId`) the user composes.
/// Submitting builds the ground term `ctor(arg, ...)` from the field values.
class CommandDesc {
  final String ctor;
  final List<FieldDesc> args;
  final String label;
  const CommandDesc({required this.ctor, required this.args, required this.label});
}

/// How to fill one argument of an answering command.
sealed class Fill {
  const Fill();
}

/// Copy the value of a named field from the card's notify term. This also
/// covers the request id (a `req(N)` field) — no special id concept is needed,
/// which is why answers that omit the id (e.g. `reject_intro/1`) just don't
/// list that field.
class FromField extends Fill {
  final String field;
  const FromField(this.field);
}

/// A constant argument fixed by the manifest (e.g. `yes`/`no`).
class ConstFill extends Fill {
  final GTerm value;
  const ConstFill(this.value);
}

/// A value the user picks from a named activity list at answer time
/// (e.g. child-safe `picker(myChildren)`). Not used by GSG v1.
class PickerFill extends Fill {
  final String list;
  final FieldType type;
  const PickerFill(this.list, this.type);
}

/// One button on an inbox card: an answering `UserCmd`.
class AnswerDesc {
  final String label;
  final String cmdCtor;
  final List<Fill> fill;
  const AnswerDesc({required this.label, required this.cmdCtor, required this.fill});

  bool get needsPicker => fill.any((f) => f is PickerFill);
}

/// An inbox card: one `ReqId`-bearing `UserNotify`. [args] names the notify's
/// positional arguments so [title]/[subtitle] templates and [answers] can refer
/// to them by name (e.g. `{from}`).
class InboxDesc {
  final String notifyCtor;
  final List<String> args;
  final String title;
  final String? subtitle;
  final List<AnswerDesc> answers;
  const InboxDesc({
    required this.notifyCtor,
    required this.args,
    required this.title,
    this.subtitle,
    required this.answers,
  });
}

/// Structural effect of an all-ground `UserNotify` on the activity store.
sealed class Effect {
  const Effect();
}

/// Append the named field's value to a list.
class AppendTo extends Effect {
  final String list;
  final String field;
  const AppendTo(this.list, this.field);
}

/// Remove entries equal to the named field's value from a list.
class RemoveFrom extends Effect {
  final String list;
  final String field;
  const RemoveFrom(this.list, this.field);
}

/// Set a single keyed value to the named field's value.
class SetValue extends Effect {
  final String key;
  final String field;
  const SetValue(this.key, this.field);
}

/// Append the [valueField] value to a thread keyed by the [keyField] value.
class ExtendThread extends Effect {
  final String thread;
  final String keyField;
  final String valueField;
  const ExtendThread(this.thread, this.keyField, this.valueField);
}

/// Ensure a conversation exists in [thread] for the peer in [peerField] (an
/// empty thread). Used so becoming friends opens a chat even before any message.
class OpenChat extends Effect {
  final String thread;
  final String peerField;
  const OpenChat(this.thread, this.peerField);
}

/// Append a directional chat message to the conversation [thread] keyed by the
/// peer in [peerField], text in [textField]. [outgoing] marks the person's own
/// message (right bubble) vs a received one (left bubble); [tickField] optionally
/// names a delivery-status field (e.g. `sent`/`delivered`) for the tick mark.
/// This is the messaging realisation of §7.4's "extends a conversation".
class PushChat extends Effect {
  final String thread;
  final String peerField;
  final String textField;
  final bool outgoing;
  final String? tickField;
  const PushChat(this.thread, this.peerField, this.textField,
      {this.outgoing = false, this.tickField});
}

/// Set a balance entry in a holdings store: `holdings[store][owner][coin]`
/// becomes the [amountField] value. This is the keyed-balance view the coins
/// app needs and the chat/list/value views do not provide — a two-level map
/// (a person, the coins they hold) updated by a wholly-ground `balance_report`.
class SetBalance extends Effect {
  final String store;
  final String ownerField;
  final String coinField;
  final String amountField;
  const SetBalance(this.store, this.ownerField, this.coinField, this.amountField);
}

/// An activity rule: one all-ground `UserNotify` that lands in its target
/// surface (paper §7.4). [effect] mutates the rendered state — `connected` adds
/// a friend, `unfriended` removes one, `received` extends a conversation. There
/// is no separate "Activity" screen; "activity" is the rule's name. A rule may
/// have no effect (a recognised notify with nothing to render, e.g. a refused
/// offer that simply leaves no friend).
class ActivityDesc {
  final String notifyCtor;
  final List<String> args;
  final Effect? effect;
  const ActivityDesc({
    required this.notifyCtor,
    required this.args,
    this.effect,
  });
}

/// Declares that a thread store key is a set of conversations to render as a
/// chat list + drill-down conversation (the messaging surface). [sendCtor] is
/// the outbox command that sends a message; the open conversation supplies its
/// peer, the input its text — so the input builds `sendCtor(peer, text)`.
class ChatView {
  final String threadKey;
  final String label;
  final String sendCtor;
  const ChatView(
      {required this.threadKey, required this.label, required this.sendCtor});
}

/// Optional wallet surface. When set, the state screen lists the person
/// ([selfKey]) and the friends in [friendsList]; tapping one drills down to that
/// person's coin holdings (from `store.holdings[storeKey]`) plus the actions
/// available against them — [selfActions] on the person's own tile (mint),
/// [friendActions] on a friend's tile (pay, redeem, propose swap). A friend
/// action's [friendField] is prefilled with the open friend, so the person
/// points at a friend's actual coins (needed for redeem and swap). This is the
/// holdings analogue of [ChatView]: a list with drill-down, but to balances and
/// actions rather than a conversation.
class WalletView {
  final String storeKey;
  final String label;
  final String selfKey;
  final String friendsList;
  final String friendField;
  final List<CommandDesc> selfActions;
  final List<CommandDesc> friendActions;
  const WalletView({
    required this.storeKey,
    required this.label,
    required this.selfKey,
    required this.friendsList,
    required this.friendField,
    required this.selfActions,
    required this.friendActions,
  });
}

/// Display kind for a declared piece of activity state.
enum StateKind { list, value, thread }

/// A piece of activity state the surface should render (e.g. the Friends list).
/// Declaring it lets an empty section appear before the first notify arrives,
/// and keeps the surface from hardcoding any app-specific store key.
class StateView {
  final String key;
  final String label;
  final StateKind kind;
  const StateView(this.key, this.label, this.kind);
}

/// A complete per-app UI contract.
class Manifest {
  final String title;
  final List<CommandDesc> commands;
  final List<InboxDesc> inbox;
  final List<ActivityDesc> activity;
  final List<StateView> state;

  /// Optional messaging surface. When set, the state screen is a chat list and
  /// tapping a conversation opens it (the GrassApp surface). When null, the
  /// state screen is a plain list (the GSG Friends surface).
  final ChatView? chat;

  /// Optional wallet surface (coins app). When set, the state screen is the
  /// wallet — friends list drilling down to holdings and actions. Mutually
  /// exclusive with [chat] in practice.
  final WalletView? wallet;

  /// Label for the state/outbox screen's bottom-nav tab (e.g. 'Friends',
  /// 'Chats'). Defaults to 'Friends'.
  final String stateTabLabel;

  const Manifest({
    required this.title,
    required this.commands,
    required this.inbox,
    required this.activity,
    this.state = const [],
    this.chat,
    this.wallet,
    this.stateTabLabel = 'Friends',
  });

  /// The inbox descriptor matching constructor [ctor] of arity [arity], if any.
  InboxDesc? inboxMatch(String ctor, int arity) {
    for (final d in inbox) {
      if (d.notifyCtor == ctor && d.args.length == arity) return d;
    }
    return null;
  }

  /// The activity descriptor matching constructor [ctor] of arity [arity].
  ActivityDesc? activityMatch(String ctor, int arity) {
    for (final d in activity) {
      if (d.notifyCtor == ctor && d.args.length == arity) return d;
    }
    return null;
  }
}
