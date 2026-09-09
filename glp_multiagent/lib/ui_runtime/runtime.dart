/// Generic UI runtime (paper §6): the two constructs and the screen over one
/// mediator boundary.
///
/// Holds the transport (send a ground `UserCmd`, receive ground `UserNotify`),
/// an inbox of cards, and an activity store — the screen. It renders nothing
/// itself; it turns notify text into structured state and turns the person's
/// taps into ground command text. It names no app-specific constructor —
/// everything specific comes from the [Manifest]. If a constructor name ever
/// needs special-casing here, that logic belongs in the schema instead.
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

  /// The open asks this card carries, clause to its own `ReqId` — a compiled
  /// vGLP program's card only. Sibling clauses with an equal context are drawn
  /// as one card, so a card may carry several, and each of the card's buttons
  /// answers the ask of its own clause. Empty for a hand-written mediator's
  /// card, whose id (where it has one) is an ordinary field.
  final Map<String, GTerm> asks;

  InboxCard(this.id, this.panel, this.desc, this.fields,
      {Map<String, GTerm>? asks})
      : asks = asks ?? <String, GTerm>{};

  /// The row key this card alerts on — the formatted value of its [itemKey]
  /// field (the offering person, the proposing friend, the invited group).
  String get itemKey => formatTerm(fields[desc.itemKey]!);

  /// The answers whose ask is still open — the buttons this card actually
  /// offers. A sibling whose ask the agent has aborted is no longer offered.
  List<AnswerDesc> get liveAnswers => [
        for (final a in desc.answers)
          if (a.clause == null || asks.containsKey(a.clause)) a
      ];
}

/// Structured state built from all-ground notifies — the screen (friends
/// list, balances, threads). Activity rules land here; there is no separate
/// activity feed.
class ActivityStore {
  final Map<String, List<GTerm>> lists = {};
  final Map<String, GTerm> values = {};
  final Map<String, Map<String, List<GTerm>>> threads = {};

  /// Keyed-balance store: `holdings[storeKey][owner][coin]` is an amount. The
  /// coins app's wallet — a person and the coins they hold.
  final Map<String, Map<String, Map<String, GTerm>>> holdings = {};

  /// Bonds locked in escrow: `escrow[storeKey][counterparty]` is the term
  /// `esc(coin, maturity, amount, release)`. Kept apart from [holdings] because
  /// escrowed bonds are precisely those the holder no longer holds.
  final Map<String, Map<String, GTerm>> escrow = {};

  /// A declared balances view: `balances[viewStore][key]` is an amount. A
  /// snapshot REPLACES what the view held — the program tallies its whole
  /// state after every change, so a key no longer reported is no longer held.
  final Map<String, Map<String, GTerm>> balances = {};
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

  /// Holdings keys set since each owner's last balance snapshot, so a sync can
  /// drop the ones that vanished (a coin spent to zero). Keyed by owner.
  final Map<String, Set<String>> _holdingsTouched = {};

  /// Raw boundary lines, kept for debugging/trace.
  final List<String> raw = [];

  int _cardSeq = 0;

  /// The standing card of each persistent clause: its `ReqId`, which the
  /// clause's compose form answers. A fresh card of a clause replaces the one
  /// before it — the agent closes its open cards and poses them again at every
  /// reduction — so a form always grants against the ask that is open now.
  final Map<String, GTerm> standing = {};

  /// The parsed patterns of the manifest's declared views, by source text.
  final Map<String, GPattern> _viewPatterns = {};

  UiRuntime({required this.manifest, required this.onSend}) {
    for (final v in manifest.state) {
      if (v.kind == StateKind.list) {
        store.lists.putIfAbsent(v.key, () => <GTerm>[]);
      } else if (v.kind == StateKind.thread) {
        store.threads.putIfAbsent(v.key, () => <String, List<GTerm>>{});
      }
    }
    for (final p in manifest.panels) {
      for (final v in p.views) {
        final pat = GPattern.parse(v.pattern);
        if (pat != null) _viewPatterns[v.pattern] = pat;
        if (v.kind == ViewKind.list) {
          store.lists.putIfAbsent(v.store, () => <GTerm>[]);
        }
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

    // The person channel of a compiled vGLP program (Definition "Canonical
    // Compilation") carries one vocabulary whatever the program: the
    // mediator's cards and their closing, the agent's screen messages, and,
    // from the person, answers. A card is selected by its CLAUSE, not by
    // constructor and arity — `card`/3 is one constructor for every clause of
    // the program — so these two come first and consume what they recognise.
    if (ctor == cardCtor && args.length == 3 && _handleCard(args)) return;
    if (ctor == closedCtor && args.length == 1 && _handleClosed(args[0])) return;

    // A notify may retire pending cards as well as land as a card or an
    // activity of its own — an escrow expiring both removes its cancel offer
    // and announces the release — so dismissal is applied first and does not
    // consume the notify.
    _applyDismissals(ctor, args);

    // A notify may open a card AND change the screen — an escrow deposit offers
    // the cancel choice and puts the locked bonds in the wallet — so a card
    // does not consume the notify either.
    final ib = manifest.inboxMatch(ctor, args.length);
    if (ib != null) {
      final (panel, desc) = ib;
      inbox.add(InboxCard(_cardSeq++, panel, desc, _bind(desc.args, args)));
      onChange?.call();
    }

    final ac = manifest.activityMatch(ctor, args.length);
    if (ac != null) {
      _applyActivity(ac, _bind(ac.args, args));
      onChange?.call();
      return;
    }
    // A screen message the manifest views, by the pattern of its display
    // declaration — including the default display, which is a declared pattern
    // like any other and, being last, takes what the others leave.
    if (_applyViews(term)) {
      onChange?.call();
      return;
    }
    // Not a notify this manifest knows — ignore (e.g. a command echoed in help).
  }

  // === The compiled vGLP person channel =====================================

  /// A card of a compiled vGLP program: `card(C, ctx_C(y1, ..., yj), req(N))`.
  ///
  /// A persistent clause's card is the standing card its compose form answers.
  /// A transient clause's card is an inbox card, its context destructured into
  /// the descriptor's named fields; a card whose family and context are those
  /// of a card already open JOINS it, contributing its own ask, so sibling
  /// clauses offering a choice over one context are one card with a button per
  /// ask. Returns false for a clause the manifest does not name.
  bool _handleCard(List<GTerm> args) {
    final c = args[0];
    if (c is! GAtom) return false;
    final clause = c.name;
    final reqId = args[2];

    if (manifest.standingForm(clause) != null) {
      standing[clause] = reqId;
      onChange?.call();
      return true;
    }

    final match = manifest.clauseCard(clause);
    if (match == null) return false;
    final (panel, desc) = match;
    final (_, ctx) = ctorArgs(args[1]);
    final fields = _bind(desc.args, ctx);
    for (final open in inbox) {
      if (!identical(open.desc, desc)) continue;
      if (!_sameContext(open.fields, fields)) continue;
      open.asks[clause] = reqId;
      onChange?.call();
      return true;
    }
    inbox.add(InboxCard(_cardSeq++, panel, desc, fields, asks: {clause: reqId}));
    onChange?.call();
    return true;
  }

  /// `closed(req(N))`: the mediator retired that ask — the machine answered on
  /// the deadline, or another clause reduced the goal and aborted it. The ask
  /// goes from whatever holds it; a card left with no ask is gone.
  bool _handleClosed(GTerm reqId) {
    final key = formatTerm(reqId);
    var changed = false;
    standing.removeWhere((_, v) {
      final hit = formatTerm(v) == key;
      changed = changed || hit;
      return hit;
    });
    for (final card in [...inbox]) {
      if (!card.asks.values.any((v) => formatTerm(v) == key)) continue;
      card.asks.removeWhere((_, v) => formatTerm(v) == key);
      changed = true;
      if (card.asks.isEmpty) inbox.removeWhere((c) => c.id == card.id);
    }
    if (changed) onChange?.call();
    return changed;
  }

  /// Whether two cards' contexts are equal — what makes sibling clauses one
  /// card.
  static bool _sameContext(Map<String, GTerm> a, Map<String, GTerm> b) {
    if (a.length != b.length) return false;
    for (final e in a.entries) {
      final other = b[e.key];
      if (other == null || formatTerm(other) != formatTerm(e.value)) return false;
    }
    return true;
  }

  /// Land a screen message in the first declared view whose pattern it
  /// matches. No constructor of the program is named here: the display
  /// declaration's pattern is what selects the view.
  bool _applyViews(GTerm term) {
    for (final p in manifest.panels) {
      for (final v in p.views) {
        final pattern = _viewPatterns[v.pattern];
        if (pattern == null) continue;
        final bound = pattern.match(term);
        if (bound == null) continue;
        final content = bound[v.content];
        if (content == null) continue;
        switch (v.kind) {
          case ViewKind.balances:
            // A tally of pairs `f(Key, Amount)`, replacing what was held.
            final rows = <String, GTerm>{};
            if (content is GList) {
              for (final item in content.items) {
                final (_, itemArgs) = ctorArgs(item);
                if (itemArgs.length == 2) {
                  rows[formatTerm(itemArgs[0])] = itemArgs[1];
                }
              }
            }
            store.balances[v.store] = rows;
          case ViewKind.list:
            store.lists.putIfAbsent(v.store, () => <GTerm>[]).add(content);
          case ViewKind.thread:
            // A pair `f(Key, Entry)`: the entry extends the conversation Key.
            final (_, itemArgs) = ctorArgs(content);
            if (itemArgs.length == 2) {
              store.threads
                  .putIfAbsent(v.store, () => <String, List<GTerm>>{})
                  .putIfAbsent(formatTerm(itemArgs[0]), () => <GTerm>[])
                  .add(itemArgs[1]);
            }
        }
        return true;
      }
    }
    return false;
  }

  /// Submit a compose form — the person's tap grants the Request-shaped
  /// clause, the field values its person inputs.
  ///
  /// A compiled vGLP program's form grants `answer(Id, xs_C(v1, ..., vi))` for
  /// the ReqId of its clause's standing card, and that ask is then consumed:
  /// the goal poses the next, and the mediator's fresh card takes its place.
  /// Where no card of the clause stands there is nothing to grant, and the
  /// submission does nothing.
  void submitCommand(CommandDesc cmd, Map<String, GTerm> values) {
    if (cmd.isStanding) {
      final reqId = standing[cmd.clause];
      if (reqId == null) return;
      onSend(formatTerm(GStruct(answerCtor, [
        reqId,
        _answer(cmd.answerCtor!, [for (final f in cmd.args) values[f.name]!]),
      ])));
      standing.remove(cmd.clause);
      onChange?.call();
      return;
    }
    final term = cmd.args.isEmpty
        ? GAtom(cmd.ctor)
        : GStruct(cmd.ctor, [for (final f in cmd.args) values[f.name]!]);
    onSend(formatTerm(term));
    onChange?.call();
  }

  /// Answer an inbox card with one of its answers — the person's tap grants
  /// the chosen sibling clause; the volition is consumed by the reduction it
  /// authorises, so the card is consumed. [picks] supplies any `PickerFill`
  /// values (unused by GSG v1).
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
    if (answer.clause != null) {
      // The button of a compiled vGLP program's card answers ITS OWN ReqId:
      // sibling clauses drawn as one card are two entries of the pending
      // table, and the one not answered is aborted by the goal's reduction.
      final reqId = card.asks[answer.clause];
      if (reqId == null) return;
      onSend(formatTerm(
          GStruct(answerCtor, [reqId, _answer(answer.answerCtor!, filled)])));
    } else {
      final term =
          filled.isEmpty ? GAtom(answer.cmdCtor) : GStruct(answer.cmdCtor, filled);
      onSend(formatTerm(term));
    }
    inbox.removeWhere((c) => c.id == card.id);
    onChange?.call();
  }

  /// The answer term `xs_C(t1, ..., ti)` of a clause, or the bare `xs_C` where
  /// its question has no positions.
  static GTerm _answer(String ctor, List<GTerm> args) =>
      args.isEmpty ? GAtom(ctor) : GStruct(ctor, args);

  // ---------------------------------------------------------------------------

  /// Remove cards this notify retires: for each pending card, any of its
  /// [InboxDesc.dismissedBy] entries matching this notify's constructor and
  /// arity, whose named item equals the card's own item.
  void _applyDismissals(String ctor, List<GTerm> args) {
    if (inbox.isEmpty) return;
    inbox.removeWhere((card) {
      for (final d in card.desc.dismissedBy) {
        if (d.notifyCtor != ctor || d.args.length != args.length) continue;
        final i = d.args.indexOf(d.itemKey);
        if (i < 0) continue;
        if (formatTerm(args[i]) == card.itemKey) return true;
      }
      return false;
    });
  }

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
            :final amountField,
            :final maturityField
          ):
          final h = store.holdings.putIfAbsent(storeKey, () => {});
          final owner = formatTerm(fields[ownerField]!);
          final coin = formatTerm(fields[coinField]!);
          // With a maturity the key is `issuer@maturity`: one issuer may be held
          // at several maturities at once, and those must stay distinct rows.
          final key = maturityField == null
              ? coin
              : '$coin@${formatTerm(fields[maturityField]!)}';
          h.putIfAbsent(owner, () => {})[key] = fields[amountField]!;
          _holdingsTouched.putIfAbsent(owner, () => <String>{}).add(key);
        case SyncBalances(store: final storeKey, :final ownerField):
          // Prune to the just-reported snapshot: drop any of the owner's
          // holdings not set since the previous sync — how a coin spent to zero
          // leaves the wallet (it is simply no longer reported).
          final owner = formatTerm(fields[ownerField]!);
          final touched = _holdingsTouched[owner] ?? const <String>{};
          store.holdings[storeKey]?[owner]
              ?.removeWhere((k, _) => !touched.contains(k));
          _holdingsTouched.remove(owner);
        case AddEscrow(
            store: final storeKey,
            :final whoField,
            :final coinField,
            :final maturityField,
            :final amountField,
            :final releaseField
          ):
          final e = store.escrow.putIfAbsent(storeKey, () => {});
          e[formatTerm(fields[whoField]!)] = GStruct('esc', [
            fields[coinField]!,
            fields[maturityField]!,
            fields[amountField]!,
            if (releaseField != null) fields[releaseField]!,
          ]);
        case RemoveEscrow(store: final storeKey, :final whoField):
          store.escrow[storeKey]?.remove(formatTerm(fields[whoField]!));
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
        case OpenGroup(:final thread, :final keyField):
          store.threads
              .putIfAbsent(thread, () => <String, List<GTerm>>{})
              .putIfAbsent(formatTerm(fields[keyField]!), () => <GTerm>[]);
        case PushGroupChat(
            :final thread,
            :final keyField,
            :final authorField,
            :final textField
          ):
          final t =
              store.threads.putIfAbsent(thread, () => <String, List<GTerm>>{});
          final k = formatTerm(fields[keyField]!);
          // Store author + text; the renderer draws the person's own posts on
          // the right (author == self) and others' on the left, labeled.
          final msg = GStruct('grp', [fields[authorField]!, fields[textField]!]);
          t.putIfAbsent(k, () => <GTerm>[]).add(msg);
        case CloseGroup(:final thread, :final keyField):
          store.threads[thread]?.remove(formatTerm(fields[keyField]!));
      }
    }
  }

  /// Post to a group: build `sendCtor(GroupId, text)` from the open group's
  /// `GroupId` term (recovered from its thread key) and the input text. No
  /// optimistic echo — the message appears when its `group_received` round-trips
  /// through the hub, so the person's own post and others' arrive by one path.
  void sendGroup(GroupChatView view, GTerm groupId, String text) {
    final atom = chatAtom(text);
    onSend(formatTerm(GStruct(view.sendCtor, [groupId, GAtom(atom)])));
    onChange?.call();
  }

  /// Append the person's own outgoing message to a conversation immediately
  /// (optimistic echo), then send the command. Used by the chat input.
  void sendChat(ChatView chat, String peer, String text) {
    final atom = chatAtom(text);
    final t = store.threads.putIfAbsent(chat.threadKey, () => <String, List<GTerm>>{});
    t.putIfAbsent(peer, () => <GTerm>[]).add(GStruct('out', [GAtom(atom), GAtom('sent')]));
    onSend(formatTerm(GStruct(chat.sendCtor, [GAtom(peer), GAtom(atom)])));
    onChange?.call();
  }
}

/// The constructors of the compiled vGLP person channel, fixed by vGLP's
/// Definition "Canonical Compilation" and therefore the same for every
/// compiled program — they are the compilation's, not any application's.
const String cardCtor = 'card';
const String closedCtor = 'closed';
const String answerCtor = 'answer';

/// Free text as a GLP constant the boundary round-trips: the `_output` kernel
/// prints atoms unquoted, so a chat text must be a plain lowercase atom —
/// lowercase, word characters joined by `_`, never variable-shaped. The chat
/// renderer shows `_` as a space.
String chatAtom(String raw) {
  var s = raw.trim().toLowerCase().replaceAll(RegExp(r'[^a-z0-9]+'), '_');
  s = s.replaceAll(RegExp(r'^_+|_+$'), '');
  if (s.isEmpty) return 'msg';
  if (!RegExp(r'^[a-z]').hasMatch(s)) return 'm_$s';
  return s;
}

/// Substitute `{name}` placeholders in a template with formatted field values.
String renderTemplate(String template, Map<String, GTerm> fields) {
  return template.replaceAllMapped(RegExp(r'\{(\w+)\}'), (m) {
    final t = fields[m.group(1)];
    return t == null ? m.group(0)! : formatTerm(t);
  });
}
