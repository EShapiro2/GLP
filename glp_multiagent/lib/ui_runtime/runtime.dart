/// Generic UI runtime (paper §7.4).
///
/// Holds the transport (send a ground `UserCmd`, receive ground `UserNotify`),
/// an inbox of cards, and an activity store. It renders nothing itself; it
/// turns notify text into structured state and turns user actions into ground
/// command text. It names no app-specific constructor — everything specific
/// comes from the [Manifest]. If a constructor name ever needs special-casing
/// here, that logic belongs in the schema instead.
library;

import 'manifest.dart';
import 'term.dart';

/// A live inbox card: the panel it belongs to, a matched notify descriptor, and
/// its bound field values. [itemKey] is the row in the panel's view it pins to
/// (its alert badges that row).
class InboxCard {
  final int id;
  final Panel panel;
  final InboxDesc desc;
  final Map<String, GTerm> fields;
  InboxCard(this.id, this.panel, this.desc, this.fields);

  /// The row key this card alerts on — the formatted value of its [itemKey]
  /// field (the offering person, the proposing friend, the invited group).
  String get itemKey => formatTerm(fields[desc.itemKey]!);
}

/// Structured state built from all-ground notifies — the state the outbox
/// leaves (friends list, balances, threads). Activity rules land here; there is
/// no separate activity feed (paper §7.4).
class ActivityStore {
  final Map<String, List<GTerm>> lists = {};
  final Map<String, GTerm> values = {};
  final Map<String, Map<String, List<GTerm>>> threads = {};

  /// Keyed-balance store: `holdings[storeKey][owner][coin]` is an amount. The
  /// coins app's wallet — a person and the coins they hold.
  final Map<String, Map<String, Map<String, GTerm>>> holdings = {};
}

/// The per-agent UI runtime.
class UiRuntime {
  final Manifest manifest;

  /// Transport: send a formatted ground `UserCmd` across the Dart/GLP boundary.
  final void Function(String cmdText) onSend;

  /// Called whenever observable state changes, so the surface can rebuild.
  void Function()? onChange;

  /// Called with a transient notice (snackbar text) from a [Toast] effect.
  void Function(String message)? onNotice;

  final ActivityStore store = ActivityStore();
  final List<InboxCard> inbox = [];

  /// Raw boundary lines, kept for debugging/trace.
  final List<String> raw = [];

  int _cardSeq = 0;

  UiRuntime({required this.manifest, required this.onSend}) {
    for (final v in manifest.state) {
      if (v.kind == StateKind.list) {
        store.lists.putIfAbsent(v.key, () => <GTerm>[]);
      } else if (v.kind == StateKind.thread) {
        store.threads.putIfAbsent(v.key, () => <String, List<GTerm>>{});
      }
    }
  }

  /// Feed one boundary line from `AgentOutput`. Lines that are not a single
  /// well-formed notify term (init/help/prompt lines) are ignored.
  void handleLine(String line) {
    raw.add(line);
    final term = tryParseTerm(line.trim());
    if (term == null) return;
    final (ctor, args) = ctorArgs(term);
    if (ctor.isEmpty) return;

    final ib = manifest.inboxMatch(ctor, args.length);
    if (ib != null) {
      final (panel, desc) = ib;
      inbox.add(InboxCard(_cardSeq++, panel, desc, _bind(desc.args, args)));
      onChange?.call();
      return;
    }

    final ac = manifest.activityMatch(ctor, args.length);
    if (ac != null) {
      _applyActivity(ac, _bind(ac.args, args));
      onChange?.call();
      return;
    }
    // Not a notify this manifest knows — ignore (e.g. a command echoed in help).
  }

  /// Submit a composed outbox command.
  void submitCommand(CommandDesc cmd, Map<String, GTerm> values) {
    final term = cmd.args.isEmpty
        ? GAtom(cmd.ctor)
        : GStruct(cmd.ctor, [for (final f in cmd.args) values[f.name]!]);
    onSend(formatTerm(term));
    onChange?.call();
  }

  /// Answer an inbox card with one of its answers. [picks] supplies any
  /// `PickerFill` values (unused by GSG v1). The card is consumed.
  void answerCard(InboxCard card, AnswerDesc answer,
      {Map<String, GTerm> picks = const {}}) {
    final filled = <GTerm>[];
    for (final f in answer.fill) {
      switch (f) {
        case FromField(:final field):
          filled.add(card.fields[field]!);
        case ConstFill(:final value):
          filled.add(value);
        case PickerFill(:final list):
          filled.add(picks[list]!);
      }
    }
    final term = filled.isEmpty ? GAtom(answer.cmdCtor) : GStruct(answer.cmdCtor, filled);
    onSend(formatTerm(term));
    inbox.removeWhere((c) => c.id == card.id);
    onChange?.call();
  }

  // ---------------------------------------------------------------------------

  Map<String, GTerm> _bind(List<String> names, List<GTerm> args) {
    final m = <String, GTerm>{};
    for (var i = 0; i < names.length; i++) {
      m[names[i]] = args[i];
    }
    return m;
  }

  void _applyActivity(ActivityDesc desc, Map<String, GTerm> fields) {
    for (final effect in desc.effects) {
      switch (effect) {
        case AppendTo(:final list, :final field):
          final l = store.lists.putIfAbsent(list, () => <GTerm>[]);
          final v = fields[field]!;
          if (!l.any((e) => formatTerm(e) == formatTerm(v))) l.add(v);
        case RemoveFrom(:final list, :final field):
          final v = fields[field]!;
          store.lists[list]?.removeWhere((e) => formatTerm(e) == formatTerm(v));
        case SetValue(:final key, :final field):
          store.values[key] = fields[field]!;
        case SetBalance(
            store: final storeKey,
            :final ownerField,
            :final coinField,
            :final amountField
          ):
          final h = store.holdings.putIfAbsent(storeKey, () => {});
          final owner = formatTerm(fields[ownerField]!);
          final coin = formatTerm(fields[coinField]!);
          h.putIfAbsent(owner, () => {})[coin] = fields[amountField]!;
        case Toast(:final template):
          onNotice?.call(renderTemplate(template, fields));
        case ExtendThread(:final thread, :final keyField, :final valueField):
          final t = store.threads.putIfAbsent(thread, () => <String, List<GTerm>>{});
          final k = formatTerm(fields[keyField]!);
          t.putIfAbsent(k, () => <GTerm>[]).add(fields[valueField]!);
        case OpenChat(:final thread, :final peerField):
          store.threads
              .putIfAbsent(thread, () => <String, List<GTerm>>{})
              .putIfAbsent(formatTerm(fields[peerField]!), () => <GTerm>[]);
        case PushChat(
            :final thread,
            :final peerField,
            :final textField,
            :final outgoing,
            :final tickField
          ):
          final t =
              store.threads.putIfAbsent(thread, () => <String, List<GTerm>>{});
          final k = formatTerm(fields[peerField]!);
          // Encode direction (and optional tick) in the stored term so the
          // renderer can draw left/right bubbles with delivery marks.
          final dir = outgoing ? 'out' : 'in';
          final tick = tickField == null ? null : fields[tickField];
          final msg = GStruct(dir, [
            fields[textField]!,
            if (tick != null) tick,
          ]);
          t.putIfAbsent(k, () => <GTerm>[]).add(msg);
      }
    }
  }

  /// Append the person's own outgoing message to a conversation immediately
  /// (optimistic echo), then send the command. Used by the chat input.
  void sendChat(ChatView chat, String peer, String text) {
    final t = store.threads.putIfAbsent(chat.threadKey, () => <String, List<GTerm>>{});
    t.putIfAbsent(peer, () => <GTerm>[]).add(GStruct('out', [GAtom(text), GAtom('sent')]));
    onSend(formatTerm(GStruct(chat.sendCtor, [GAtom(peer), GAtom(text)])));
    onChange?.call();
  }
}

/// Substitute `{name}` placeholders in a template with formatted field values.
String renderTemplate(String template, Map<String, GTerm> fields) {
  return template.replaceAllMapped(RegExp(r'\{(\w+)\}'), (m) {
    final t = fields[m.group(1)];
    return t == null ? m.group(0)! : formatTerm(t);
  });
}
