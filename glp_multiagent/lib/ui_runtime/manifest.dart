/// Per-app UI manifest schema (paper §6, the two constructs and the screen).
///
/// A manifest is declarative data — no Dart logic. It maps a mediator's
/// `UserCmd`/`UserNotify` constructors to the two constructs and the screen:
/// compose forms (the Request-shaped volition-guarded clauses), inbox cards
/// (the Respond-shaped pending reductions), and activity rules (the screen —
/// platform state flowing on the notify stream). The generic runtime in this
/// package renders strictly from a manifest and names no app-specific
/// constructor; all app specifics (GSG, bonds, child-safe) live in manifest
/// instances elsewhere.
library;

import 'term.dart';

/// Input widget kind for a compose-form field.
enum FieldType { person, text, integer }

/// One field of a compose form — one person input of the clause.
class FieldDesc {
  final String name;
  final FieldType type;
  final String label;
  const FieldDesc(this.name, this.type, this.label);
}

/// A compose form: a Request-shaped volition-guarded clause, its fields the
/// clause's person inputs — a free `UserCmd` (no escrowed `ReqId`).
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

  /// After this answer, open the card's [InboxDesc.itemKey] item (drill into its
  /// conversation). Set on a group invitation's Accept so joining opens the
  /// group; left false for answers that leave no item to open.
  final bool opensItem;
  const AnswerDesc(
      {required this.label,
      required this.cmdCtor,
      required this.fill,
      this.opensItem = false});

  bool get needsPicker => fill.any((f) => f is PickerFill);
}

/// An inbox card: a Respond-shaped pending volition-guarded reduction — one
/// `ReqId`-bearing `UserNotify`, its [answers] the sibling clauses. [args]
/// names the notify's positional arguments so [title]/[subtitle] templates
/// and [answers] can refer to them by name (e.g. `{from}`).
///
/// [itemKey] names the argument that pins the card to a row in its panel's view
/// — the offering person for a friend offer (`from`), the proposing friend for a
/// swap (`from`), the group for an invitation. The panel badges that row and
/// opens the card from it (WhatsApp-style per-item alert), rather than listing
/// the card in a separate inbox screen.
class InboxDesc {
  final String notifyCtor;
  final List<String> args;
  final String itemKey;
  final String title;
  final String? subtitle;
  final List<AnswerDesc> answers;
  const InboxDesc({
    required this.notifyCtor,
    required this.args,
    required this.itemKey,
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

/// Show a transient notice (snackbar) — for acknowledgement notifies that leave
/// no lasting state, e.g. a completed or failed swap. [template] may reference
/// the notify's fields, e.g. `Swap with {who} completed`.
class Toast extends Effect {
  final String template;
  const Toast(this.template);
}

/// Open (create, empty) a group conversation keyed by the group in [keyField]
/// — the compound `GroupId` term. The group's chat is keyed by that term (its
/// formatted string), so a group joined shows an empty conversation before any
/// message. The group analogue of [OpenChat].
class OpenGroup extends Effect {
  final String thread;
  final String keyField;
  const OpenGroup(this.thread, this.keyField);
}

/// Append an author-labeled message to the group conversation [thread] keyed by
/// the group in [keyField] (a compound `GroupId`), the author in [authorField],
/// text in [textField]. Unlike [PushChat] (one-to-one, direction only), a group
/// message carries its author; the renderer draws the person's own posts on the
/// right and others' on the left with the author's name.
class PushGroupChat extends Effect {
  final String thread;
  final String keyField;
  final String authorField;
  final String textField;
  const PushGroupChat(
      this.thread, this.keyField, this.authorField, this.textField);
}

/// Remove the group conversation keyed by the group in [keyField] — a member
/// left it or was removed. The group analogue of [RemoveFrom] for a thread.
class CloseGroup extends Effect {
  final String thread;
  final String keyField;
  const CloseGroup(this.thread, this.keyField);
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
/// surface(s) — the screen, the classic stream I/O of concurrent logic
/// programming (paper §6). [effects] mutate the rendered state — `connected`
/// both adds a friend to the Friends panel and opens the conversation in the
/// Chats panel, `unfriended` removes one, `received` extends a conversation.
/// There is no separate "Activity" screen; "activity" is the rule's name. A rule
/// may have no effect (a recognised notify with nothing to render, e.g. a
/// refused offer that simply leaves no friend). A notify lands in more than one
/// panel by listing one effect per panel it touches.
class ActivityDesc {
  final String notifyCtor;
  final List<String> args;
  final List<Effect> effects;
  const ActivityDesc({
    required this.notifyCtor,
    required this.args,
    this.effects = const [],
  });
}

/// Declares that a thread store key is a set of conversations to render as a
/// chat list + drill-down conversation (the messaging surface). [sendCtor] is
/// the compose command that sends a message; the open conversation supplies
/// its peer, the input its text — so the input builds `sendCtor(peer, text)`.
class ChatView {
  final String threadKey;
  final String label;
  final String sendCtor;
  const ChatView(
      {required this.threadKey, required this.label, required this.sendCtor});
}

/// A group-chat surface (paper §7.2 Social Network): the groups the person
/// belongs to, each a `GroupId`-keyed multi-party conversation. Its rows are the
/// person's groups (thread keys under [threadKey]) plus any pending group
/// invitation (a card alerting on the group). Opening a group shows its
/// author-labeled messages, an input that posts via [sendCtor]
/// (`send_group(GroupId, text)`), and its per-group [actions] (invite a friend,
/// leave) whose group argument is prefilled with the open group. Unlike
/// [ChatView] this is keyed by a compound term and its bubbles name their
/// author, which the one-to-one chat surface does not carry.
class GroupChatView {
  final String threadKey;
  final String label;
  final String sendCtor;

  /// The `GroupId`-typed field the [sendCtor] and each action expects first,
  /// prefilled from the open group (its argument, not typed by the person).
  final String groupField;
  final List<CommandDesc> actions;
  const GroupChatView({
    required this.threadKey,
    required this.label,
    required this.sendCtor,
    required this.groupField,
    this.actions = const [],
  });
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

  /// Optional explicit friends list key. When empty, the wallet's people are
  /// derived from holdings owners and chat peers (so a friend appears as soon
  /// as a connection forms or a balance is reported).
  final String friendsList;
  final String friendField;
  final List<CommandDesc> selfActions;
  final List<CommandDesc> friendActions;
  const WalletView({
    required this.storeKey,
    required this.label,
    required this.selfKey,
    required this.friendField,
    required this.selfActions,
    required this.friendActions,
    this.friendsList = '',
  });
}

/// A plain friends-list surface (the social-graph panel): the established
/// friends in [listKey], each a row. Pending friend offers badge a row for the
/// offering person even before the friendship is made — first contact is gated
/// here, so the offer appears as an alerting row in this panel.
class FriendsView {
  final String listKey;
  final String label;
  const FriendsView({required this.listKey, required this.label});
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

/// One platform's panel (paper §7): a name (app-bar title + bottom-nav tab
/// label), exactly one state view — a friends list, a wallet, or a chat
/// list — its own compose [commands] (the panel's "+"), and its own [inbox]
/// of asks, each surfaced as a per-item alert on its view's rows.
/// A new platform is a new panel; the panels stack without touching one another.
class Panel {
  /// Stable identity for routing a card to this panel (e.g. 'friends').
  final String id;

  /// The platform's name — the app-bar title when this panel is active and its
  /// bottom-nav tab label.
  final String name;

  // Exactly one of the four views is set; its kind selects the panel's icon.
  final FriendsView? friends;
  final WalletView? wallet;
  final ChatView? chat;
  final GroupChatView? groups;

  /// The panel's compose commands (its "+"); empty when the view composes its
  /// own actions (the wallet drills down to per-friend actions instead).
  final List<CommandDesc> commands;

  /// The panel's inbox cards, each pinned to a row by its [InboxDesc.itemKey].
  final List<InboxDesc> inbox;

  const Panel({
    required this.id,
    required this.name,
    this.friends,
    this.wallet,
    this.chat,
    this.groups,
    this.commands = const [],
    this.inbox = const [],
  });
}

/// A complete per-app UI contract: GrassApp is one app of several panels, one
/// per platform (paper §7). The bottom bar is the panels; the active panel's
/// name heads the app bar. Activity rules and declared state are shared across
/// panels — one notify may land in more than one panel (e.g. `connected`) — so
/// they live here and the panels render views over the one activity store.
class Manifest {
  final String title;
  final List<Panel> panels;
  final List<ActivityDesc> activity;
  final List<StateView> state;

  const Manifest({
    required this.title,
    required this.panels,
    required this.activity,
    this.state = const [],
  });

  /// The panel and inbox descriptor whose card matches constructor [ctor] of
  /// arity [arity], if any.
  (Panel, InboxDesc)? inboxMatch(String ctor, int arity) {
    for (final p in panels) {
      for (final d in p.inbox) {
        if (d.notifyCtor == ctor && d.args.length == arity) return (p, d);
      }
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
