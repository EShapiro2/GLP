/// The named widgets of the grassroots app's construct family: the Dart widgets a widget declaration `T =::= W` may name (vGLP, sections/elicitation.tex, Definition "Widget Declaration": "In the grassroots app, W names a Dart widget: the construct family draws it, and its type fixes the terms it forms").
///
/// A named widget is drawn over its construct's view, as the draw `draw(Id, named(W, D), V)` carries it, and takes the person's input through the grant of the question the view marks `input(P)` (bridge.dart).  The draw carries the name `W`, the default widget `D` of the type, and the view `V`: "It is drawn with the default widget of its type beside its name, from which it reads the type's constructors" (vGLP 92e6d3b, Section 6.2, the paragraph after Proposition "Default Widgets"; Section 6.3, the type `Widget`).  The construct family hands it `D` with the view, and the last value drawn on construct 0: "A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person" (vGLP e924f73, Section 6.2, the paragraph after Proposition "Default Widgets"; construct_family.dart, `_named`).  It grants the raw input the default widget of the type grants, so that the construct process forms the same answers from it (Definition "Construct Process").
///
/// Three named widgets are here.  [lastState] and [socialMenu], below, draw the apps of GSC's contracts in their tabs, GSNP's items and the notices of attempts that close without committing among them.  The first is [approvalSentence], over the approval question of every canonical CVA platform vGLP's compiler prints (programs/vglp/cva/compiler; vGLP, Section 5.4, "The compilation"): `Approval ::= approval(Attempt, YesNo?)`, `Attempt ::= attempt(Peer, Integer, Integer, Class)`, `YesNo ::= yes ; no`, each class an act schema applied to its arguments, each argument a `Peer` or a `Speech`, `Speech ::= speech(Utterance, Peer)`, `Utterance ::= utterance(String) ; Speech`.  It reads the class off the view and draws one sentence for it and two buttons labelled for it, in the words of GAI's task for the apps of GSC's contracts (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 3, its sentences as corrected at 11:35 UTC): `befriend(A, Me)`, "Alice asks to be your friend.", Accept and Decline; `join(A, Me, n)`, "Alice asks you to join the group friends.", Join and Decline.  A class the task gives no words for is drawn by a sentence built from its schema and its persons, each speech act of the class under it, and the buttons Yes and No.  A person is shown by name, their key with its first letter capitalised (the same task, its preamble).
library;

import 'package:flutter/material.dart';

import 'bridge.dart';
import 'construct_family.dart';
import 'tab_screen.dart';
import 'term.dart';

/// The name a widget declaration gives [approvalSentence]: `Approval =::= approval_sentence.`
const String approvalSentenceName = 'approval_sentence';

/// The key of the button of construct [id] granting the constant [c] at the question at [at].
ValueKey<String> answerKey(int id, String c, List<int> at) =>
    ValueKey('$id:answer:$c:${pathText(at)}');

/// The key of the sentence of construct [id].
ValueKey<String> sentenceKey(int id) => ValueKey('$id:sentence');

/// A person shown by name: their key with its first letter capitalised, `alice` as Alice.
String personName(GTerm p) {
  final s = displayText(p);
  return s.isEmpty ? s : '${s[0].toUpperCase()}${s.substring(1)}';
}

/// The wording of an approval question, read off its view `approval(attempt(I, N, K, C), Q)`: the sentence, the speech acts of the class drawn under it, and the labels of the buttons granting `yes` and `no`.
class ApprovalWording {
  /// The sentence, as "Alice asks to be your friend."
  final String sentence;

  /// The speech acts drawn under the sentence, in order: each its text and its byline, "by Alice", the person asked as `you`.
  final List<({String text, String byline})> speech;

  /// The labels of the buttons granting `yes` and `no`.
  final String yes, no;

  const ApprovalWording(this.sentence, this.speech, this.yes, this.no);

  /// The wording of [attempt], `attempt(I, N, K, C)`, or null where it is not one: the words of GAI's task where it words the class `C`, and a sentence built from `C` otherwise.
  static ApprovalWording? of(GTerm attempt) {
    final (af, xs) = ctorArgs(attempt);
    if (af != 'attempt' || xs.length != 4) return null;
    final (schema, args) = ctorArgs(xs[3]);
    if (schema.isEmpty) return null;
    return _worded(xs[0], schema, args) ?? _built(xs[0], schema, args);
  }

  /// The classes GAI's task words, the initiator `I` their first argument `A` and the person asked `Me` their second, a person other than `I`: `befriend(A, Me)`, "Alice asks to be your friend.", Accept and Decline; and `join(A, Me, n)`, `n` the group's name, a speech act, "Alice asks you to join the group friends.", its text the group's name, Join and Decline.  Null for every other class.
  static ApprovalWording? _worded(
      GTerm initiator, String schema, List<GTerm> args) {
    final i = formatTerm(initiator);
    if (args.length < 2 || formatTerm(args[0]) != i) return null;
    final me = args[1];
    if (SpeechAct.of(me) != null || formatTerm(me) == i) return null;
    final a = personName(initiator);
    switch ((schema, args.length)) {
      case ('befriend', 2):
        return ApprovalWording(
            '$a asks to be your friend.', const [], 'Accept', 'Decline');
      case ('join', 3):
        final group = SpeechAct.of(args[2]);
        if (group == null) return null;
        return ApprovalWording(
            '$a asks you to join the group ${group.text}.',
            const [],
            'Join',
            'Decline');
    }
    return null;
  }

  /// A class GAI's task does not word: "Alice asks to", the schema of the class, its underscores as spaces, and the class's other arguments that are persons, in order, the person asked as `you`; each speech act of the class under the sentence; and the buttons Yes and No.
  ///
  /// The person asked is the one person among the arguments of the class other than the initiator, where there is exactly one: the approval question is the one a Start raises (vGLP, Section 5.4, "The compilation"), at a participant of the attempt other than its initiator, and the view does not name the agent it is asked at.  Where the class names two persons or more besides the initiator, each is named and none is `you`.
  static ApprovalWording _built(
      GTerm initiator, String schema, List<GTerm> args) {
    final i = formatTerm(initiator);
    final persons = [for (final a in args) if (SpeechAct.of(a) == null) a];
    final others = {
      for (final p in persons)
        if (formatTerm(p) != i) formatTerm(p)
    };
    final asked = others.length == 1 ? others.single : null;
    String name(GTerm p) => formatTerm(p) == asked ? 'you' : personName(p);
    final objects = <String>[];
    var initiatorNamed = false;
    for (final p in persons) {
      if (!initiatorNamed && formatTerm(p) == i) {
        initiatorNamed = true;
        continue;
      }
      objects.add(name(p));
    }
    return ApprovalWording(
      '${personName(initiator)} asks to ${schema.replaceAll('_', ' ')}'
      '${objects.isEmpty ? '' : ' ${objects.join(', ')}'}.',
      [
        for (final a in args)
          if (SpeechAct.of(a) case final s?)
            (
              text: s.text,
              byline: [
                'by ${name(s.signers.first)}',
                for (final v in s.signers.skip(1)) 'via ${name(v)}',
              ].join(', '),
            ),
      ],
      'Yes',
      'No',
    );
  }
}

/// A speech act, `speech(U, P)`: its text, the utterance at its core, and its signers, the one who signed the utterance and each who signed it again after, in order (GSC, Section 3: "A speech act is an utterance or a speech act, signed by a person").
class SpeechAct {
  final String text;
  final List<GTerm> signers;
  const SpeechAct(this.text, this.signers);

  /// The speech act [t], or null where it is not one.
  static SpeechAct? of(GTerm t) {
    final (f, xs) = ctorArgs(t);
    if (f != 'speech' || xs.length != 2) return null;
    final (uf, us) = ctorArgs(xs[0]);
    if (uf == 'utterance' && us.length == 1) {
      return SpeechAct(displayText(us[0]), [xs[1]]);
    }
    final inner = SpeechAct.of(xs[0]);
    if (inner == null) return null;
    return SpeechAct(inner.text, [...inner.signers, xs[1]]);
  }
}

/// `approval_sentence`: the approval question of a canonical CVA platform drawn as one sentence for the attempt's class and two buttons labelled for it, granting `yes` and `no` at the question's position, `[2]`, as the default widget of `YesNo?`, `buttons([yes, no])`, grants them ([ApprovalWording]).  It is drawn by `named(approval_sentence, D)`, `D` the default widget of `Approval`, whose constructors it reads off the view, the type being one for every canonical CVA platform.  Once the question is answered, the label of the button that grants the answer is shown in place of the buttons.  A view that is not an approval question of that type is drawn as such, named, and takes no input: nothing stands in for it.
Widget approvalSentence(
    BuildContext context,
    OpenConstruct construct,
    GTerm dflt,
    GTerm view,
    List<int> at,
    void Function(GTerm path, GTerm raw) grant,
    GTerm? output) {
  final (f, args) = ctorArgs(view);
  if (f != 'approval' || args.length != 2) {
    return _cannotDraw(approvalSentenceName, view);
  }
  final p = questionAt(args[1]);
  final pending = pendingAt(args[0]) != null;
  final wording = pending ? null : ApprovalWording.of(args[0]);
  if (!pending && wording == null) {
    return _cannotDraw(approvalSentenceName, view);
  }
  final theme = Theme.of(context);
  final small = TextStyle(fontSize: 12, color: Colors.grey.shade600);
  final pAt = p == null ? null : [for (final i in p.items) (i as GInt).value];
  final buttons = [
    ('yes', wording?.yes ?? 'Yes'),
    ('no', wording?.no ?? 'No'),
  ];
  final answer = formatTerm(args[1]);
  final answered = [
    for (final (c, label) in buttons)
      if (c == answer) label,
    displayText(args[1]),
  ].first;
  return Column(
    crossAxisAlignment: CrossAxisAlignment.start,
    children: [
      Text(wording?.sentence ?? '…',
          key: sentenceKey(construct.id),
          style: theme.textTheme.titleMedium),
      for (final s in wording?.speech ?? const <({String text, String byline})>[])
        Container(
          margin: const EdgeInsets.only(top: 8),
          padding: const EdgeInsets.only(left: 10),
          decoration: BoxDecoration(
              border: Border(
                  left: BorderSide(color: Colors.grey.shade400, width: 2))),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(s.text, style: theme.textTheme.bodyLarge),
              Text(s.byline, style: small),
            ],
          ),
        ),
      if (p != null && pAt != null) ...[
        const SizedBox(height: 14),
        Row(children: [
          for (final (c, label) in buttons) ...[
            ElevatedButton(
              key: answerKey(construct.id, c, pAt),
              onPressed: () => grant(p, GAtom(c)),
              child: Text(label),
            ),
            const SizedBox(width: 12),
          ],
        ]),
      ] else
        // The question answered: the label of the button granting the answer, and no input taken.
        Padding(
          padding: const EdgeInsets.only(top: 8),
          child: Text(answered),
        ),
    ],
  );
}

/// A view a named widget cannot draw: named, taking no input.
Widget _cannotDraw(String widget, GTerm view) => Text(
    '$widget cannot draw ${formatTerm(view)}',
    style: TextStyle(color: Colors.red.shade700));

// === The apps of GSC's contracts: last_state and social_menu =================

/// The name a widget declaration gives [lastState]: `Stream(Output) =::= last_state.`, which vGLP's compiler prints into every canonical platform (programs/vglp/cva/compiler/text.glp).
const String lastStateName = 'last_state';

/// The name a widget declaration gives [socialMenu]: `Menu =::= social_menu.` (programs/apps/gsg, gsn, gsnp and gai, `self.vglp`).
const String socialMenuName = 'social_menu';

/// The key of the control or the row [what] that [lastState] or [socialMenu] draws for construct [id].
ValueKey<String> socialKey(int id, String what) =>
    ValueKey('$id:social:$what');

/// A group among the keys of [socialKey]: its leader and its name, as their terms.
String groupKey(GTerm leader, GTerm name) =>
    '${formatTerm(leader)}:${formatTerm(name)}';

/// The view of the agent of a canonical platform, as it writes it on its output to its person: "On the program's output to its person ... the agent goal writes its view view(s, K), the machine state s of its app state and the persons K it knows, when it starts and whenever either changes; and, when an attempt its person proposed closes without committing, a notice: refused([t]_j, q), q the participant whose vote no closed it, or yielded([t]_j), where it yielded to an attempt with priority over it.  The compilation defines Output as the type of the views and the notices, and declares for the type Stream(Output) of the output a widget that draws the last view written and the notices written after it" (vGLP f5f1644, Section 5.4, "The compilation").  A machine state is a list of atoms, each as its contract writes it (programs/gsc/GSG.glp, GSN.glp and GSNP.glp), in the view as the platform prints its type `Atom` (programs/apps/<app>/platform.vglp): `identity` and `friend(Q)`; in GSN also `received(X, P)`, `member(C)`, `listed(Q, N)`, `closed(N)`, `posted(X, N)`, `delivered(X, Q, N)` and `received_in(X, A, N)`; in GSNP `item(X)` and `authored(X)` in place of `received` and `received_in`; each `X` a speech act and each `N`, a group's name, a speech act.  `C` is the community name of a group, `<A>.N` in the contract, of type `CName ::= community(Peer) ; dot(CName, Speech)`: "a name term <u>.y1...yn is the community name of the person bound to u followed by the speech acts bound to y1, ..., yn, as <a>.n names the group n led by a" (vGLP c649c0d, Section 5.4, "Act schemas as local specifications"), so a member atom holds `dot(community(A), N)`, the group `N` led by `A`.
class SocialState {
  /// The machine state, its atoms.
  final List<GTerm> atoms;

  /// The persons the agent knows.
  final List<GTerm> known;

  /// The notices written after the last view, in the order written: those the widget draws.
  final List<SocialNotice> notices;

  /// Every notice written on construct 0, in the order written, those before the last view among them.
  final List<SocialNotice> written;

  const SocialState(this.atoms, this.known,
      [this.notices = const [], this.written = const []]);

  /// The last view written on construct 0, and the notices written after it, read off [output], the value drawn on it: the elements of the stream so far, each written whole a view, `view(S, K)`, or a notice ([SocialNotice]).  The first of the pair is false where [output] is not a stream of views and notices; the second is null where no view is written whole yet.
  static (bool, SocialState?) read(GTerm? output) {
    if (output is! GList) return (false, null);
    List<GTerm>? atoms, known;
    var notices = <SocialNotice>[];
    final written = <SocialNotice>[];
    for (final e in output.items) {
      if (!_whole(e)) continue;
      final (f, xs) = ctorArgs(e);
      if (f == 'view' && xs.length == 2 && xs[0] is GList && xs[1] is GList) {
        atoms = (xs[0] as GList).items;
        known = (xs[1] as GList).items;
        // A notice is drawn until the next view is written (vGLP f5f1644).
        notices = <SocialNotice>[];
        continue;
      }
      final n = SocialNotice.of(e);
      if (n == null) return (false, null);
      notices.add(n);
      written.add(n);
    }
    if (atoms == null || known == null) return (true, null);
    return (true, SocialState(atoms, known, notices, written));
  }

  /// Whether the program has written [t] whole: no position in it is marked pending.
  static bool _whole(GTerm t) =>
      pendingAt(t) == null &&
      switch (t) {
        GStruct(:final args) => args.every(_whole),
        GList(:final items) => items.every(_whole),
        _ => true,
      };

  /// The arguments of each atom of predicate [p] of arity [n], in the order of the state.
  List<List<GTerm>> _args(String p, int n) => [
        for (final a in atoms)
          if (ctorArgs(a) case (final f, final xs) when f == p && xs.length == n)
            xs
      ];

  static bool _same(GTerm a, GTerm b) => formatTerm(a) == formatTerm(b);

  /// Whether the person's identity is established (GSG:1).
  bool get identity =>
      atoms.any((a) => a is GAtom && a.name == 'identity');

  /// The person's friends (GSG:2).
  List<GTerm> get friends => [for (final x in _args('friend', 1)) x[0]];

  bool isFriend(GTerm q) => friends.any((f) => _same(f, q));

  /// The messages the person received from [p], `received(X, p)` (GSN:1).
  List<GTerm> received(GTerm p) => [
        for (final x in _args('received', 2))
          if (_same(x[1], p)) x[0]
      ];

  /// Whether the person holds a message received from a friend.
  bool get chats => _args('received', 2).isNotEmpty;

  /// The items the person holds, `item(X)` (GSNP:1 and GSNP:2).
  List<GTerm> get items => [for (final x in _args('item', 1)) x[0]];

  /// Whether the person authored the item [x], `authored(x)`, which sign adds with `item(x)` (GSNP:1; `sign(p? ; x)`, programs/gsc/GSNP.glp).
  bool isAuthored(GTerm x) => _args('authored', 1).any((a) => _same(a[0], x));

  /// The agent an item came from: its last signer, the friend who sent it in a message, forwarded it or delivered it in a group, or, for an item the person authored, the person (GSC, Section 3.3: "item(x) in the state of Alice is a speech act x Alice holds, signed by its author or by the agent who forwarded it to Alice, the author and every earlier forwarder signed inside it").
  static GTerm? sender(GTerm x) => SpeechAct.of(x)?.signers.last;

  /// The principal of the AI agent [r] that the person holds recorded, `agent_of(r, p)`, which `agent_befriend` and `agents_befriend` add at the agent the AI agent befriends (GAI:2; programs/gai/GAI.glp; GAI's paper, Table "The local state of an agent p": "pairs (r,q) of an AI agent r and its principal q that p holds recorded"); null where the person holds no such record of [r].
  GTerm? principalOf(GTerm r) => [
        for (final x in _args('agent_of', 2))
          if (_same(x[0], r)) x[1]
      ].firstOrNull;

  /// The agent [p] by name, and, where the person holds [p] recorded as an AI agent, its principal: "Alf, an AI agent of Alice" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC: "Author: Alf, an AI agent of Alice").
  String agentName(GTerm p) {
    final principal = principalOf(p);
    return principal == null
        ? personName(p)
        : '${personName(p)}, an AI agent of ${personName(principal)}';
  }

  /// The group a member atom holds, `member(dot(community(A), N))`: its leader `A` and its name `N`; null where the atom holds another community name.
  static (GTerm, GTerm)? _group(GTerm c) {
    final (d, xs) = ctorArgs(c);
    if (d != 'dot' || xs.length != 2 || SpeechAct.of(xs[1]) == null) {
      return null;
    }
    final (b, ys) = ctorArgs(xs[0]);
    if (b != 'community' || ys.length != 1) return null;
    return (ys[0], xs[1]);
  }

  /// The groups the person is a member of, `member(dot(community(A), N))`, in the order of the state, each with its leader `A` and its name `N`, its members and its messages (GSN:2 to GSN:5).  The leader's state lists the members, `listed(Q, N)` (`join(a?, q? ; n)` adds `listed(q, n)` at `a`, programs/gsc/GSN.glp), and holds the messages posted in it, `posted(X, N)`; so for a group [me] leads, its members are those the state lists in it and its messages those posted in it, and for any other group, no member is drawn.  A member's messages are those delivered to them in it, `received_in(X, A, N)`.  Each message once.
  List<SocialGroup> groups({GTerm? me}) => [
        for (final m in _args('member', 1))
          if (_group(m[0]) case (final a, final n))
            SocialGroup(
              a,
              n,
              [
                if (me != null && _same(a, me))
                  for (final x in _args('listed', 2))
                    if (_same(x[1], n)) x[0]
              ],
              _messagesIn(a, n, me != null && _same(a, me)),
            )
      ];

  /// Whether the person is a member of a group.
  bool get inGroup => _args('member', 1).any((m) => _group(m[0]) != null);

  List<GTerm> _messagesIn(GTerm a, GTerm n, bool leads) {
    final seen = <String>{};
    return [
      for (final x in _args('received_in', 3))
        if (_same(x[1], a) && _same(x[2], n) && seen.add(formatTerm(x[0])))
          x[0],
      if (leads)
        for (final x in _args('posted', 2))
          if (_same(x[1], n) && seen.add(formatTerm(x[0]))) x[0],
    ];
  }
}

/// A group the person is a member of: its leader, its name, a speech act, the members drawn in it, those the person's state lists in it where they lead it, and its messages.
class SocialGroup {
  final GTerm leader, name;
  final List<GTerm> members, messages;
  const SocialGroup(this.leader, this.name, this.members, this.messages);

  /// The group's name as the person reads it, the text of the speech act.
  String get title => SpeechAct.of(name)?.text ?? displayText(name);
}

/// The classes of a group (GSN:2 to GSN:5, programs/gsc/GSN.glp and GSNP.glp): an approval question or a notice of one of them concerns the Groups tab, and every other the Friends tab.
const Set<String> groupClasses = {
  'create',
  'join',
  'leave',
  'close',
  'drop',
  'post_own',
  'post',
  'post_forward',
  'deliver',
  'deliver_own',
};

/// A notice the agent writes on its output when an attempt its person proposed closes without committing: `refused(P, Q)`, `Q` the participant whose vote no closed it, or `yielded(P)`, where it yielded to an attempt with priority over it, `P` the part, a term of its class (vGLP c44769f, Section 5.4, "The compilation").  It is drawn as one sentence in the tab it concerns, in plain words, worded for the classes GAI's tasks name, befriend, join, message and forward (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 and 13:31 UTC; the sentence "Bob declined your friendship" approved by Udi, vGLP #15 Cowork to Code, 2026-10-19 13:34 UTC), and by a sentence built from its class for every other.
class SocialNotice {
  /// The part of the attempt, a term of its class: `befriend(alice, bob)`.
  final GTerm part;

  /// The participant whose vote no closed it, for `refused(P, Q)`; null for `yielded(P)`.
  final GTerm? by;

  const SocialNotice(this.part, this.by);

  /// The notice [t], or null where it is not one.
  static SocialNotice? of(GTerm t) {
    final (f, xs) = ctorArgs(t);
    if (f == 'refused' && xs.length == 2 && ctorArgs(xs[0]).$1.isNotEmpty) {
      return SocialNotice(xs[0], xs[1]);
    }
    if (f == 'yielded' && xs.length == 1 && ctorArgs(xs[0]).$1.isNotEmpty) {
      return SocialNotice(xs[0], null);
    }
    return null;
  }

  /// The schema of the part's class.
  String get schema => ctorArgs(part).$1;

  /// Whether the notice concerns the Groups tab.
  bool get concernsGroup => groupClasses.contains(schema);

  /// The notice in plain words, [me] the person, who proposed the attempt, where known:
  ///
  ///   - `befriend(Me, Q)`: "Bob declined your friendship." or "Your request to befriend Bob did not go through.";
  ///   - `join(Me, Q, n)`: "Bob declined to join the group friends." or "Your invitation to Bob to join the group friends did not go through.";
  ///   - `message(Me, Q, x)`: "Your message to Carol was not delivered.";
  ///   - `forward(Me, Q, x)`: "\"Flood on Main Street\" was not forwarded to Fay.";
  ///   - any other: "Bob said no to your request to unfriend Bob." or "Your request to unfriend Bob did not go through.", the schema, its underscores as spaces, and the persons of the class other than the person; for a group created, closed or left, "the group" and its name in place of the persons; and for any other class of a group, "in the group" and its name after them.
  String sentence(GTerm? me) {
    final (f, xs) = ctorArgs(part);
    final by = this.by;
    String text(GTerm x) => SpeechAct.of(x)?.text ?? displayText(x);
    switch ((f, xs.length)) {
      case ('befriend', 2):
        return by != null
            ? '${personName(by)} declined your friendship.'
            : 'Your request to befriend ${personName(xs[1])} did not go '
                'through.';
      case ('join', 3):
        return by != null
            ? '${personName(by)} declined to join the group ${text(xs[2])}.'
            : 'Your invitation to ${personName(xs[1])} to join the group '
                '${text(xs[2])} did not go through.';
      case ('message', 3):
        return 'Your message to ${personName(xs[1])} was not delivered.';
      case ('forward', 3):
        return '"${text(xs[2])}" was not forwarded to ${personName(xs[1])}.';
    }
    // Every other class: the persons of the class other than the person,
    // who proposed it; where the person is not known, the first, the
    // proposer of every class GAI's tasks name.
    final persons = [for (final a in xs) if (SpeechAct.of(a) == null) a];
    var dropped = false;
    final others = <String>[];
    for (final p in persons) {
      final mine = me != null
          ? formatTerm(p) == formatTerm(me)
          : !dropped;
      if (mine && !dropped) {
        dropped = true;
        continue;
      }
      others.add(personName(p));
    }
    // A group created, closed or left is the object of the request; in
    // every other class of a group the group is where it is made.  The
    // group's name is the last argument of each (programs/gsc/GSNP.glp).
    final ofGroup = const {'create', 'close', 'leave'}.contains(f);
    final group = groupClasses.contains(f) && xs.isNotEmpty
        ? '${ofGroup ? ' the group' : ' in the group'} ${text(xs.last)}'
        : '';
    final objects = ofGroup || others.isEmpty ? '' : ' ${others.join(', ')}';
    final request = 'your request to ${f.replaceAll('_', ' ')}$objects$group';
    return by != null
        ? '${personName(by)} said no to $request.'
        : '${request[0].toUpperCase()}${request.substring(1)} did not go '
            'through.';
  }
}

/// The requests a request question offers, read off the default widget `D` of its type, `Menu ::= menu(Key, List(Key), Request?)`: "It is drawn with the default widget of its type beside its name, from which it reads the type's constructors, so that a menu declared once lists the alternatives of the request type of each program it is declared for" (vGLP, Section 6.2, the paragraph after Proposition "Default Widgets").  The alternatives of the request type are `propose(P)`, `P` a term of Proposal naming a guarded part of a class, and `known(Q)` (vGLP, Section 5.4, "The compilation").
class RequestOffers {
  /// The alternatives of Proposal, each its functor and arity, `befriend/2`.
  final Set<String> proposals;

  /// Whether the request type has `known(Q)`.
  final bool known;

  const RequestOffers(this.proposals, this.known);

  bool has(String f, int n) => proposals.contains('$f/$n');

  /// The requests [dflt], the default widget of Menu, offers: its form `menu`, whose third field is the menu of the request type's alternatives, the form `propose` of one field, the menu of the forms of Proposal, and the form `known`.
  static RequestOffers of(GTerm dflt) {
    final proposals = <String>{};
    var known = false;
    final (k, ws) = ctorArgs(_bare(dflt));
    if (k == 'form' && ws.length == 2 && ws[1] is GList) {
      final fields = (ws[1] as GList).items;
      if (fields.length == 3) {
        for (final alt in _alternatives(_fieldWidget(fields[2]))) {
          final (ak, aws) = ctorArgs(alt);
          if (ak != 'form' || aws.length != 2 || aws[1] is! GList) continue;
          final f = ctorArgs(aws[0]).$1;
          final afs = (aws[1] as GList).items;
          if (f == 'known' && afs.length == 1) known = true;
          if (f == 'propose' && afs.length == 1) {
            for (final p in _alternatives(_fieldWidget(afs[0]))) {
              final (pk, pws) = ctorArgs(p);
              if (pk == 'form' && pws.length == 2 && pws[1] is GList) {
                proposals.add(
                    '${ctorArgs(pws[0]).$1}/${(pws[1] as GList).items.length}');
              }
            }
          }
        }
      }
    }
    return RequestOffers(proposals, known);
  }

  /// [w] with a declaration's name and a recursive type's binding taken off its top.
  static GTerm _bare(GTerm w) {
    final named = namedWidget(w);
    if (named != null) return _bare(named.$2);
    final (k, ws) = ctorArgs(w);
    if (k == recCtor && ws.length == 2) return _bare(ws[1]);
    return w;
  }

  /// The widget of a form's entry, `field(L, W)` or `question(L, W)`.
  static GTerm _fieldWidget(GTerm e) {
    final (_, es) = ctorArgs(e);
    return es.length == 2 ? _bare(es[1]) : e;
  }

  /// The alternatives a widget offers: the items of a menu, or the widget itself.
  static List<GTerm> _alternatives(GTerm w) {
    final (k, ws) = ctorArgs(w);
    if (k == 'menu' && ws.length == 1 && ws[0] is GList) {
      return [for (final a in (ws[0] as GList).items) _bare(a)];
    }
    return [w];
  }
}

/// What the request question asks, as [socialMenu] reads its view `menu(Me, Known, input(P))`: the agent's own key, the path of the question, null once it is answered, the requests its type offers, and the grant of the person's raw input at the question.  The persons known are read off the last view on construct 0 ([SocialState.known]), which the agent writes whenever they change, where the request question shows those known when it was asked.
class SocialAsk {
  final GTerm me;
  final GList? at;
  final RequestOffers offers;
  final void Function(GTerm path, GTerm raw) grant;
  const SocialAsk(this.me, this.at, this.offers, this.grant);

  /// Whether the question is open and its type offers [f] of arity [n].
  bool can(String f, int n) => at != null && offers.has(f, n);

  /// Answer the question with the request `propose(P)`, as the default widget of the request type grants it: the form `propose` holding the form of `P`.
  void propose(GTerm p) => grant(at!, GStruct('propose', [p]));

  /// A speech act of the text [text], signed by the person: "speech(utterance(S), P) is the text S by P" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 4), as the default widget of Speech forms it from its fields, a text field and a peer field.
  GTerm say(String text) => GStruct('speech', [
        GStruct('utterance', [GString(text)]),
        me,
      ]);
}

/// `last_state`: construct 0 of a canonical platform's app, drawn as the last view its agent wrote, its machine state and the persons it knows, "and the person's screen shows the last written" (vGLP b7e3517, Section 5.4, "The compilation"), laid out in the tab it is drawn in (tab_screen.dart; vGLP, Remark "Construct Families"), as GAI's task words it (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 4): in the Friends tab the person's friends and, in GSN, the chat with each, its messages; in the Groups tab the groups with their leader and their messages, and, in a group the person leads, its members, whom the leader's state lists ([SocialState.groups]); each message its text with its author's name under it; in GSNP each item in the chat with the friend it came from, with its author and those who forwarded it ([_SocialTabState._item]); and the notices, each one sentence at the top of the tab it concerns ([SocialNotice]), "a widget that draws the last view written and the notices written after it" (vGLP f5f1644), so that a notice is drawn until the next view is written.  It takes no input.  Drawn outside a screen of tabs, it draws the Friends tab and, where the person is in a group, the Groups tab under it.
Widget lastState(
    BuildContext context,
    OpenConstruct construct,
    GTerm dflt,
    GTerm view,
    List<int> at,
    void Function(GTerm path, GTerm raw) grant,
    GTerm? output) {
  final (ok, state) = SocialState.read(view);
  if (!ok) return _cannotDraw(lastStateName, view);
  final place = TabPlace.maybeOf(context);
  return SocialTab(
    key: socialKey(construct.id, 'tab:${place?.tab.name ?? 'all'}'),
    id: construct.id,
    state: state,
    place: place,
  );
}

/// `social_menu`: the request question of a canonical platform of GSC's contracts, `Menu ::= menu(Key, List(Key), Request?)`, drawn with the last view on construct 0, the machine state and the persons known, in the tab it is drawn in, as GAI's task words it (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, items 1 and 2).  In the Friends tab: the person's own name at the top; before their identity is established, the button Join, proposing `establish(Me)`; after it, the persons known, one row each, a friend with the button End friendship, proposing `unfriend(Me, Q)`, and any other with Befriend, proposing `befriend(Me, Q)`; at the bottom the field Add a person by key with Add, answering `known(Q)`.  Where the request type offers `message(Me, Q, X)`, tapping a friend opens the chat with them, its messages and a text field with Send, proposing `message(Me, Q, x)`, x the speech act of the text typed signed by the person.  Where it offers `create(Me, N)`, the Groups tab: the field New group with Create, proposing `create(Me, n)`; each group the person is in, its name, its leader and its messages, and, in a group they lead, its members; in a group the person leads, Add member, a picker of their friends not in it, proposing `join(Me, Q, n)`, and Close group, proposing `close(Me, n)`; in any group, a text field with Send, proposing `post_own(Me, x, n)` where they lead it and `post(Me, A, x, n)` otherwise; and in a group they do not lead, Leave group, proposing `leave(A, Me, n)`.  Each control is drawn where the request type offers its request, read off `D` ([RequestOffers]), and while the question is open; each grants `propose(P)` or `known(Q)` at the question's position, `[3]`, as the default widget of the request type grants it, so the construct process forms the same answers from it.  No term, functor, type name or Submit button is drawn.  The persons known, and who is a friend, are read off the last view, which the agent writes whenever either changes (vGLP b7e3517), and not off the request question, which shows the persons known when it was asked.  Where construct 0 holds no stream of views, it is drawn as such, named, and takes no input.
///
/// In GSNP (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, items 1 to 3): an item the person holds is drawn in the chat with the friend it came from, its last signer, its text, "Author: Dan" and, where it was forwarded, "via Eve, via Carol"; under one the person did not author, Forward, which opens a picker of their friends and, once one is picked, the warranty question of GSNP:2 with Yes, proposing `forward(Me, Q, x)`, and No; the chat's Send signs the text, proposing `sign(Me, x)`, and holds `message(Me, Q, x)` for the next request question, proposed once the view holds `authored(x)` ([_SocialTabState._chat]).  An item the person authored is drawn in the chat it was sent from, and an item from a person who is not a friend, or authored and sent from no chat on this screen, under Messages at the bottom of the Friends tab.  In GAI's app (programs/apps/gai), whose request type offers `sign(Me, X)` and `forward(Me, Q, X)` and no message, an item is drawn as in GSNP, and an author the person holds recorded as an AI agent, `agent_of(r, p)`, is named with its principal, "Author: Alf, an AI agent of Alice" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, its last paragraph).  A post carries an item, which the person signs before posting it (GSNP:1; `post_own(a? ; x, n)` requires `item(x)` at `a`, programs/gsc/GSNP.glp), so the text typed in a group forms no post: where the request type offers `sign(Me, X)`, no text field and Send are drawn in a group.  The notices are drawn as `last_state` draws them.
Widget socialMenu(
    BuildContext context,
    OpenConstruct construct,
    GTerm dflt,
    GTerm view,
    List<int> at,
    void Function(GTerm path, GTerm raw) grant,
    GTerm? output) {
  final (f, args) = ctorArgs(view);
  if (f != 'menu' || args.length != 3 || pendingAt(args[0]) != null) {
    return _cannotDraw(socialMenuName, view);
  }
  final (ok, state) = SocialState.read(output);
  if (!ok) {
    return Text(
        '$socialMenuName cannot draw ${formatTerm(view)} without a view on '
        'construct 0',
        style: TextStyle(color: Colors.red.shade700));
  }
  final place = TabPlace.maybeOf(context);
  return SocialTab(
    key: socialKey(construct.id, 'tab:${place?.tab.name ?? 'all'}'),
    id: construct.id,
    state: state,
    place: place,
    ask: SocialAsk(args[0], questionAt(args[2]), RequestOffers.of(dflt), grant),
  );
}

/// The tabs of the apps of GSC's contracts, drawn from the last view written, with the controls of the request question where [ask] is given ([socialMenu]) and without them where it is not ([lastState]).
class SocialTab extends StatefulWidget {
  /// The construct drawn: its identifier, in the keys of its controls.
  final int id;

  /// The last view written; null where none is written whole yet.
  final SocialState? state;

  /// The tab drawn and the chat open in it, from the screen of tabs; null outside one.
  final TabPlace? place;

  /// The request question; null for construct 0.
  final SocialAsk? ask;

  const SocialTab(
      {super.key,
      required this.id,
      required this.state,
      required this.place,
      this.ask});

  @override
  State<SocialTab> createState() => _SocialTabState();
}

class _SocialTabState extends State<SocialTab> {
  /// The text of each field, by its key.
  final Map<String, TextEditingController> _texts = {};

  @override
  void dispose() {
    for (final c in _texts.values) {
      c.dispose();
    }
    super.dispose();
  }

  TextEditingController _text(String k) =>
      _texts.putIfAbsent(k, TextEditingController.new);

  /// The text typed into the field [k], trimmed.
  String _typed(String k) => _text(k).text.trim();

  int get _id => widget.id;
  SocialAsk? get _ask => widget.ask;

  /// The person's own key, as the request question shows it, or as the screen of tabs holds it from the last one drawn; null where neither shows it.
  GTerm? get _me => _ask?.me ?? widget.place?.me;

  /// An agent identifier, as a peer field takes it: letters, digits and `_` (construct_family.dart, `_identifier`).
  static final RegExp _identifier = RegExp(r'^[A-Za-z0-9_]+$');

  /// The item whose Forward is open, its picker or its warranty question, by its term as written; null where none is.
  String? _forwarding;

  /// The friend picked in the open Forward; null while the picker is drawn.
  GTerm? _forwardTo;

  @override
  Widget build(BuildContext context) {
    final s = widget.state;
    if (s == null) {
      return Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
        if (_ask != null) _name(context, _ask!.me),
        Text('…', style: TextStyle(color: Colors.grey.shade600)),
      ]);
    }
    _sendHeld(s);
    return switch (widget.place?.tab) {
      ScreenTab.friends => _friends(context, s),
      ScreenTab.groups => _groups(context, s),
      null => Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            _friends(context, s),
            if (s.inGroup ||
                s.notices.any((n) => n.concernsGroup) ||
                (_ask?.offers.has('create', 2) ?? false))
              ...[const Divider(height: 32), _groups(context, s)],
          ],
        ),
    };
  }

  /// The second request of a Send in GSNP's chat: once the item the person signed is in the view as authored, `authored(x)`, the message that carries it, `message(Me, Q, x)`, proposed on the open request question, a later one than the question that took the sign ([TabPlace.hold]).  It is proposed after the frame, once, by whichever tab draws the open question first.
  void _sendHeld(SocialState s) {
    final ask = _ask;
    final place = widget.place;
    if (ask == null || ask.at == null || place == null) return;
    for (final h in place.held) {
      if (h.askedOn == _id || !s.isAuthored(h.item)) continue;
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (!mounted || !place.release(h.item)) return;
        ask.propose(GStruct('message', [ask.me, h.friend, h.item]));
      });
    }
  }

  /// The notices of the tab [t], each one sentence, the latest first ([SocialNotice]).
  List<Widget> _notices(SocialState s, ScreenTab t) {
    final me = _me;
    return [
      for (var i = s.notices.length - 1; i >= 0; i--)
        if ((t == ScreenTab.groups) == s.notices[i].concernsGroup)
          Container(
            key: socialKey(_id, 'notice:$i'),
            width: double.infinity,
            margin: const EdgeInsets.only(bottom: 8),
            padding: const EdgeInsets.fromLTRB(12, 8, 12, 8),
            decoration: BoxDecoration(
              color: Colors.amber.shade50,
              border: Border.all(color: Colors.amber.shade200),
              borderRadius: BorderRadius.circular(10),
            ),
            child: Row(children: [
              Icon(Icons.info_outline, size: 18, color: Colors.amber.shade900),
              const SizedBox(width: 8),
              Expanded(child: Text(s.notices[i].sentence(me))),
            ]),
          ),
    ];
  }

  Widget _name(BuildContext context, GTerm me) => Padding(
        padding: const EdgeInsets.only(bottom: 8),
        child: Text(personName(me),
            key: socialKey(_id, 'name'),
            style: Theme.of(context)
                .textTheme
                .titleLarge
                ?.copyWith(fontWeight: FontWeight.bold)),
      );

  // --- The Friends tab -------------------------------------------------------

  /// The friend in whose chat the item [x] is drawn, or null where it is drawn under Messages: the friend it came from ([SocialState.sender]), or, for an item the person authored, the friend they sent it to from a chat on this screen ([TabPlace.destination]); a chat being opened on a screen of tabs only.
  GTerm? _chatOf(SocialState s, GTerm x) {
    final place = widget.place;
    if (place == null) return null;
    final q = s.isAuthored(x) ? place.destination(x) : SocialState.sender(x);
    return q != null && s.isFriend(q) ? q : null;
  }

  /// What the chat with [q] holds: the messages the person received from them, in GSN, and the items drawn in it, in GSNP ([_chatOf]), in the order of the state.
  List<GTerm> _inChat(SocialState s, GTerm q) => [
        ...s.received(q),
        for (final x in s.items)
          if (_chatOf(s, x) case final c? when formatTerm(c) == formatTerm(q))
            x,
      ];

  Widget _friends(BuildContext context, SocialState s) {
    final ask = _ask;
    final chat = widget.place?.chat;
    if (chat != null) return _chat(context, s, chat);
    final elsewhere = [
      for (final x in s.items)
        if (_chatOf(s, x) == null) x
    ];
    final chats = (ask?.offers.has('message', 3) ?? false) ||
        s.chats ||
        elsewhere.length < s.items.length;
    final open = widget.place?.openChat;
    // The persons known and the friends, as the last view shows them; the
    // person's own key not among them.
    final persons = <GTerm>[];
    final seen = <String>{if (ask != null) formatTerm(ask.me)};
    for (final q in [...s.known, ...s.friends]) {
      if (seen.add(formatTerm(q))) persons.add(q);
    }
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        if (ask != null) _name(context, ask.me),
        ..._notices(s, ScreenTab.friends),
        if (!s.identity) ...[
          if (ask != null && ask.can('establish', 1))
            ElevatedButton(
              key: socialKey(_id, 'join'),
              onPressed: () => ask.propose(GStruct('establish', [ask.me])),
              child: const Text('Join'),
            ),
        ] else
          for (final q in ask == null ? s.friends : persons)
            _person(context, s, q,
                onTap: chats && open != null && s.isFriend(q)
                    ? () => open(q)
                    : null),
        if (ask != null && ask.at != null && ask.offers.known) ...[
          const SizedBox(height: 16),
          _entry(
            field: 'add-field',
            label: 'Add a person by key',
            button: 'add',
            buttonText: 'Add',
            valid: (t) => _identifier.hasMatch(t),
            onPressed: (t) => ask.grant(ask.at!, GStruct('known', [GAtom(t)])),
          ),
        ],
        if (elsewhere.isNotEmpty) ...[
          const SizedBox(height: 16),
          Text('Messages',
              key: socialKey(_id, 'items'),
              style: Theme.of(context).textTheme.titleMedium),
          for (final x in elsewhere) _item(context, s, x),
        ],
      ],
    );
  }

  /// A person's row: their initial and name, and, for the request question, the button that befriends them or ends the friendship; tapping a friend opens the chat with them where [onTap] is given.
  Widget _person(BuildContext context, SocialState s, GTerm q,
      {VoidCallback? onTap}) {
    final ask = _ask;
    final k = formatTerm(q);
    final friend = s.isFriend(q);
    final last = _inChat(s, q);
    Widget? button;
    if (ask != null && friend && ask.can('unfriend', 2)) {
      button = OutlinedButton(
        key: socialKey(_id, 'unfriend:$k'),
        onPressed: () => ask.propose(GStruct('unfriend', [ask.me, q])),
        child: const Text('End friendship'),
      );
    } else if (ask != null && !friend && ask.can('befriend', 2)) {
      button = ElevatedButton(
        key: socialKey(_id, 'befriend:$k'),
        onPressed: () => ask.propose(GStruct('befriend', [ask.me, q])),
        child: const Text('Befriend'),
      );
    }
    return ListTile(
      key: socialKey(_id, 'person:$k'),
      contentPadding: EdgeInsets.zero,
      leading: CircleAvatar(
        radius: 16,
        backgroundColor: Colors.green.shade100,
        child: Text(personName(q).isEmpty ? '' : personName(q)[0],
            style: const TextStyle(fontSize: 13)),
      ),
      title: Text(personName(q)),
      subtitle: onTap != null && last.isNotEmpty
          ? Text(SpeechAct.of(last.last)?.text ?? displayText(last.last),
              maxLines: 1, overflow: TextOverflow.ellipsis)
          : null,
      trailing: button,
      onTap: onTap,
    );
  }

  /// The chat with the friend [q]: in GSN the messages the person received from them, and in GSNP the items drawn in it ([_chatOf]), each with Forward where the person did not author it ([_item]); and, for the request question, a text field with Send.  In GSN Send proposes `message(Me, Q, x)`, x the speech act of the text typed signed by the person.  In GSNP, where the request type offers `sign(Me, X)`, the person signs the content they create (GSNP:1) and a message carries an item they authored (`message(p?, q ; x)` requires `authored(x)` at `p`, programs/gsc/GSNP.glp), so Send proposes `sign(Me, x)` on the open request question and holds `message(Me, Q, x)` for a later one, proposed once the view holds `authored(x)` (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 2; [TabPlace.hold]).
  Widget _chat(BuildContext context, SocialState s, GTerm q) {
    final ask = _ask;
    final place = widget.place;
    final k = formatTerm(q);
    final signs = ask != null && ask.offers.has('sign', 2) && place != null;
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Row(children: [
          InkResponse(
            key: socialKey(_id, 'back'),
            onTap: () => widget.place?.openChat(null),
            child: const Padding(
              padding: EdgeInsets.fromLTRB(0, 8, 12, 8),
              child: Icon(Icons.arrow_back),
            ),
          ),
          Text(personName(q),
              key: socialKey(_id, 'chat:$k'),
              style: Theme.of(context)
                  .textTheme
                  .titleLarge
                  ?.copyWith(fontWeight: FontWeight.bold)),
        ]),
        const SizedBox(height: 4),
        ..._notices(s, ScreenTab.friends),
        for (final x in s.received(q)) _message(context, x),
        for (final x in _inChat(s, q).skip(s.received(q).length))
          _item(context, s, x),
        if (ask != null && ask.can('message', 3) &&
            (!ask.offers.has('sign', 2) || signs)) ...[
          const SizedBox(height: 12),
          _entry(
            field: 'chat-field:$k',
            label: 'Message',
            button: 'send:$k',
            buttonText: 'Send',
            valid: (t) => t.isNotEmpty,
            onPressed: (t) {
              final x = ask.say(t);
              if (signs) {
                place.hold(x, q, _id, s.written.length);
                ask.propose(GStruct('sign', [ask.me, x]));
              } else {
                ask.propose(GStruct('message', [ask.me, q, x]));
              }
            },
          ),
        ],
      ],
    );
  }

  /// An item of GSNP (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 1): its text, then "Author: Dan", its first signer, then, where it was forwarded, "via Eve, via Carol", each who signed it after them, in order; under an item the person did not author, for the request question, the button Forward; under one they authored, nothing.  In GAI an author the person holds recorded as an AI agent is named with its principal, "Author: Alf, an AI agent of Alice" (the same task, its last paragraph; [SocialState.agentName]).
  Widget _item(BuildContext context, SocialState s, GTerm x) {
    final ask = _ask;
    final a = SpeechAct.of(x);
    final k = formatTerm(x);
    final small = TextStyle(fontSize: 12, color: Colors.grey.shade700);
    final forwards = ask != null &&
        a != null &&
        !s.isAuthored(x) &&
        ask.can('forward', 3) &&
        s.friends.isNotEmpty;
    return Column(
      key: socialKey(_id, 'item:$k'),
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Container(
          margin: const EdgeInsets.symmetric(vertical: 4),
          padding: const EdgeInsets.fromLTRB(12, 8, 12, 8),
          decoration: BoxDecoration(
            color: Colors.green.shade50,
            borderRadius: BorderRadius.circular(12),
          ),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(a?.text ?? displayText(x),
                  style: Theme.of(context).textTheme.bodyLarge),
              if (a != null) ...[
                const SizedBox(height: 2),
                Text('Author: ${s.agentName(a.signers.first)}', style: small),
                if (a.signers.length > 1)
                  Text(
                      [
                        for (final v in a.signers.skip(1))
                          'via ${personName(v)}'
                      ].join(', '),
                      style: small),
              ],
            ],
          ),
        ),
        if (forwards) ..._forward(context, s, x, a),
      ],
    );
  }

  /// Forward under the item [x], and, once tapped, a picker of the person's friends; picking Fay, the warranty question of GSNP:2, "Forward to Fay?  You warrant that you know Carol personally.", Carol the friend the item came from, with Yes, which proposes `forward(Me, fay, x)`, and No, which closes it (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 3).  The question is the widget's, the person's input before they submit it, and not an approval question of the platform: "the warranty in GSNP:2 and GSNP:3 describe no act and bind the agents" (GSC, Section 2.3).  Forward closes the picker again.
  List<Widget> _forward(
      BuildContext context, SocialState s, GTerm x, SpeechAct a) {
    final ask = _ask!;
    final k = formatTerm(x);
    final open = _forwarding == k;
    final to = open ? _forwardTo : null;
    void close() => setState(() {
          _forwarding = null;
          _forwardTo = null;
        });
    return [
      OutlinedButton.icon(
        key: socialKey(_id, 'forward:$k'),
        onPressed: () => open
            ? close()
            : setState(() {
                _forwarding = k;
                _forwardTo = null;
              }),
        icon: const Icon(Icons.forward, size: 18),
        label: const Text('Forward'),
      ),
      if (open && to == null)
        Padding(
          padding: const EdgeInsets.only(top: 6),
          child: Wrap(
            key: socialKey(_id, 'forward-picker:$k'),
            spacing: 8,
            runSpacing: 4,
            crossAxisAlignment: WrapCrossAlignment.center,
            children: [
              Text('Forward to',
                  style: TextStyle(fontSize: 13, color: Colors.grey.shade700)),
              for (final f in s.friends)
                ActionChip(
                  key: socialKey(_id, 'forward-to:$k:${formatTerm(f)}'),
                  label: Text(personName(f)),
                  onPressed: () => setState(() => _forwardTo = f),
                ),
            ],
          ),
        ),
      if (to != null)
        Container(
          key: socialKey(_id, 'warranty:$k'),
          width: double.infinity,
          margin: const EdgeInsets.only(top: 8),
          padding: const EdgeInsets.fromLTRB(12, 10, 12, 10),
          decoration: BoxDecoration(
            border: Border.all(color: Colors.grey.shade300),
            borderRadius: BorderRadius.circular(10),
          ),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text('Forward to ${personName(to)}?',
                  style: Theme.of(context).textTheme.titleMedium),
              const SizedBox(height: 4),
              Text('You warrant that you know ${personName(a.signers.last)} '
                  'personally.'),
              const SizedBox(height: 12),
              Row(children: [
                ElevatedButton(
                  key: socialKey(_id, 'forward-yes:$k'),
                  onPressed: () {
                    ask.propose(GStruct('forward', [ask.me, to, x]));
                    close();
                  },
                  child: const Text('Yes'),
                ),
                const SizedBox(width: 12),
                ElevatedButton(
                  key: socialKey(_id, 'forward-no:$k'),
                  onPressed: close,
                  child: const Text('No'),
                ),
              ]),
            ],
          ),
        ),
    ];
  }

  // --- The Groups tab --------------------------------------------------------

  Widget _groups(BuildContext context, SocialState s) {
    final ask = _ask;
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        ..._notices(s, ScreenTab.groups),
        if (ask != null && s.identity && ask.can('create', 2)) ...[
          _entry(
            field: 'group-field',
            label: 'New group',
            button: 'create',
            buttonText: 'Create',
            valid: (t) => t.isNotEmpty,
            onPressed: (t) =>
                ask.propose(GStruct('create', [ask.me, ask.say(t)])),
          ),
          const SizedBox(height: 12),
        ],
        for (final g in s.groups(me: _me)) _group(context, s, g),
      ],
    );
  }

  /// A group the person is in: its name, its leader, its messages, and, where they lead it, the members their state lists in it ([SocialState.groups]); and, for the request question, its controls.
  Widget _group(BuildContext context, SocialState s, SocialGroup g) {
    final ask = _ask;
    final k = groupKey(g.leader, g.name);
    final small = TextStyle(fontSize: 13, color: Colors.grey.shade700);
    final leads = ask != null && formatTerm(ask.me) == formatTerm(g.leader);
    final listed = {for (final m in g.members) formatTerm(m)};
    final candidates = [
      for (final f in s.friends)
        if (!listed.contains(formatTerm(f))) f
    ];
    // In GSNP a post carries an item the person signed (GSNP:1), which a
    // text typed here is not ([socialMenu]).
    final posts = ask != null &&
        !ask.offers.has('sign', 2) &&
        (leads ? ask.can('post_own', 3) : ask.can('post', 4));
    return Container(
      key: socialKey(_id, 'group:$k'),
      width: double.infinity,
      margin: const EdgeInsets.only(bottom: 12),
      padding: const EdgeInsets.fromLTRB(12, 10, 12, 10),
      decoration: BoxDecoration(
        border: Border.all(color: Colors.grey.shade300),
        borderRadius: BorderRadius.circular(10),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(g.title,
              style: Theme.of(context)
                  .textTheme
                  .titleMedium
                  ?.copyWith(fontWeight: FontWeight.bold)),
          Text('Led by ${personName(g.leader)}', style: small),
          if (g.members.isNotEmpty)
            Text('Members: ${g.members.map(personName).join(', ')}',
                style: small),
          const SizedBox(height: 4),
          for (final x in g.messages) _message(context, x),
          if (posts) ...[
            const SizedBox(height: 8),
            _entry(
              field: 'post-field:$k',
              label: 'Message',
              button: 'post:$k',
              buttonText: 'Send',
              valid: (t) => t.isNotEmpty,
              onPressed: (t) => ask.propose(leads
                  ? GStruct('post_own', [ask.me, ask.say(t), g.name])
                  : GStruct('post', [ask.me, g.leader, ask.say(t), g.name])),
            ),
          ],
          if (leads && ask.can('join', 3) && candidates.isNotEmpty) ...[
            const SizedBox(height: 8),
            Wrap(
              key: socialKey(_id, 'add-member:$k'),
              spacing: 8,
              runSpacing: 4,
              crossAxisAlignment: WrapCrossAlignment.center,
              children: [
                Text('Add member', style: small),
                for (final q in candidates)
                  ActionChip(
                    key: socialKey(_id, 'join:$k:${formatTerm(q)}'),
                    label: Text(personName(q)),
                    onPressed: () =>
                        ask.propose(GStruct('join', [ask.me, q, g.name])),
                  ),
              ],
            ),
          ],
          if (leads && ask.can('close', 2)) ...[
            const SizedBox(height: 8),
            OutlinedButton(
              key: socialKey(_id, 'close:$k'),
              onPressed: () =>
                  ask.propose(GStruct('close', [ask.me, g.name])),
              child: const Text('Close group'),
            ),
          ],
          if (ask != null && !leads && ask.can('leave', 3)) ...[
            const SizedBox(height: 8),
            OutlinedButton(
              key: socialKey(_id, 'leave:$k'),
              onPressed: () =>
                  ask.propose(GStruct('leave', [g.leader, ask.me, g.name])),
              child: const Text('Leave group'),
            ),
          ],
        ],
      ),
    );
  }

  // --- Pieces ----------------------------------------------------------------

  /// A message: its text, and its author's name under it, and each who forwarded it after them (GAI's task, item 4: "A message is its text with its author's name under it"; [SpeechAct]).
  Widget _message(BuildContext context, GTerm x) {
    final a = SpeechAct.of(x);
    final byline = a == null
        ? ''
        : [
            personName(a.signers.first),
            for (final v in a.signers.skip(1)) 'via ${personName(v)}',
          ].join(', ');
    return Container(
      key: socialKey(_id, 'message:${formatTerm(x)}'),
      margin: const EdgeInsets.symmetric(vertical: 4),
      padding: const EdgeInsets.fromLTRB(12, 8, 12, 8),
      decoration: BoxDecoration(
        color: Colors.green.shade50,
        borderRadius: BorderRadius.circular(12),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(a?.text ?? displayText(x),
              style: Theme.of(context).textTheme.bodyLarge),
          if (byline.isNotEmpty)
            Text(byline,
                style: TextStyle(fontSize: 12, color: Colors.grey.shade700)),
        ],
      ),
    );
  }

  /// A text field labelled [label] and the button [buttonText] beside it, enabled once the text typed is [valid], which calls [onPressed] with the text and clears the field.
  Widget _entry({
    required String field,
    required String label,
    required String button,
    required String buttonText,
    required bool Function(String) valid,
    required void Function(String) onPressed,
  }) {
    final t = _typed(field);
    return Row(
      children: [
        Expanded(
          child: TextField(
            key: socialKey(_id, field),
            controller: _text(field),
            decoration: InputDecoration(labelText: label, isDense: true),
            onChanged: (_) => setState(() {}),
          ),
        ),
        const SizedBox(width: 8),
        ElevatedButton(
          key: socialKey(_id, button),
          onPressed: valid(t)
              ? () {
                  onPressed(t);
                  setState(() => _text(field).clear());
                }
              : null,
          child: Text(buttonText),
        ),
      ],
    );
  }
}
