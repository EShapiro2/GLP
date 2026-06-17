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
  const Manifest({
    required this.title,
    required this.commands,
    required this.inbox,
    required this.activity,
    this.state = const [],
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
