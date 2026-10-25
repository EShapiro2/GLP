/// The named widgets of the grassroots app's construct family: the Dart widgets a widget declaration `T =::= W` may name (vGLP, sections/elicitation.tex, Definition "Widget Declaration": "In the grassroots app, W names a Dart widget: the construct family draws it, and its type fixes the terms it forms").
///
/// A named widget is drawn over its construct's view, as the draw `draw(Id, named(W, D), V)` carries it, and takes the person's input through the grant of the question the view marks `input(P)` (bridge.dart).  The draw carries the name `W`, the default widget `D` of the type, and the view `V`: "It is drawn with the default widget of its type beside its name, from which it reads the type's constructors" (vGLP 92e6d3b, Section 6.2, the paragraph after Proposition "Default Widgets"; Section 6.3, the type `Widget`).  The construct family hands it `D` with the view, and the last value drawn on construct 0: "A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person" (vGLP e924f73, Section 6.2, the paragraph after Proposition "Default Widgets"; construct_family.dart, `_named`).  It grants the raw input the default widget of the type grants, so that the construct process forms the same answers from it (Definition "Construct Process").
///
/// On a platform compiled from a contract with pragmas, every word these widgets draw is the contract's, the program's, or one of the platform's four, yes, no, declined and unable: the view names the lists the app shows, the approval question carries the question the contract puts to the person, and the widget term of the request type the labels of its requests and fields and where the button of each is drawn, `act(place(...), W)` ([ActInfo]) (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"; `_approvalWorded`, `_SocialTabState._worded`).  No word of a contract is held there, and where a control is drawn is read off the widget term and the view, not off a schema's or a predicate's name.  The words held below the worded path are those of the apps drawn before their contracts carried pragmas, until no app is drawn so.
///
/// Three named widgets are here.  [lastState] and [socialMenu], below, draw the apps of GSC's contracts in their tabs, GSNP's items and the notices of attempts that close without committing among them.  The first is [approvalSentence], over the approval question of every canonical CVA platform vGLP's compiler prints (programs/vglp/cva/compiler; vGLP, Section 5.4, "The compilation"): `Approval ::= approval(Attempt, YesNo?)`, `Attempt ::= attempt(Peer, Integer, Integer, Class, Names)`, the attempt (i, k, pi, [t]_j, r) naming its request r, `Names ::= none ; request(Rho, Integer)` (vGLP f0acbc1, Definition "Attempt"), `YesNo ::= yes ; no`, each class an act schema applied to its arguments, each argument a `Peer` or a `Speech`, `Speech ::= speech(Utterance, Peer)`, `Utterance ::= utterance(String) ; Speech`.  It reads the class off the view and draws one sentence for it and two buttons labelled for it, in the words of GAI's task for the apps of GSC's contracts (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 3, its sentences as corrected at 11:35 UTC): `befriend(A, Me)`, "Alice asks to be your friend.", Accept and Decline; `join(A, Me, n)`, "Alice asks you to join the group friends.", Join and Decline; and, in GAI's app, `agent_befriend(R, Q, P)`, in the words of GAI's task for its figure (Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, (b) and (c)), at the principal "Alf asks to befriend Bob." and "Approve?", and at the other party "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme", read from the record of Alf that party holds, and "Befriend Alf?", with Yes and No; and `agent_meet(R, Q, P, Y)` likewise, "Alf asks to meet Bob." and "Approve?" at the principal, and "Alf, an AI agent of Alice, asks to meet you.", "Developer: Acme" and "Meet Alf?" at the other party.  And, in GC's app, `swap(A, Me, U, V)`, in the words of Currencies' task for its screens (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 18:10 UTC, task 2, (b)), "Alice asks to swap an Alice-coin for a Bob-coin.", a coin drawn by its issuer's name as GC writes it, "Alice-coin" ([coinName]), with Yes and No, the answers of the platform's `YesNo`: the task takes the compiler's words where they differ from its own, Accept and Decline.  A class the task gives no words for is drawn by a sentence built from its schema and its persons, each speech act of the class under it, and the buttons Yes and No.  A person is shown by name, their key with its underscores as spaces and each word capitalised (the same task, its preamble, which capitalised the first letter; GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 1: "The developer's key is acme_ltd, drawn \"Acme Ltd\"").
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

/// A person shown by name: their key, its underscores as spaces and each word capitalised, `alice` as Alice and `acme_ltd` as Acme Ltd (GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 1).
String personName(GTerm p) => [
      for (final w in displayText(p).split('_'))
        if (w.isNotEmpty) '${w[0].toUpperCase()}${w.substring(1)}'
    ].join(' ');

/// A coin issued by [u], `coin(u)`, by its issuer's name: "Alice-coin" (GC, programs/currencies/coins/GC.txt: "Alice-coin, a grassroots coin issued by Alice"), its hyphen the non-breaking hyphen, U+2011, so that a line is not broken inside it.
String coinName(GTerm u) => '${personName(u)}\u2011coin';

/// [coinName] with its indefinite article: "an Alice-coin", "a Bob-coin".
String aCoin(GTerm u) {
  final n = coinName(u);
  return '${'AEIOU'.contains(n[0]) ? 'an' : 'a'} $n';
}

/// The wording of an approval question, read off its view `approval(attempt(I, N, K, C, R), Q)`: the sentence, the speech acts of the class drawn under it, and the labels of the buttons granting `yes` and `no`.
class ApprovalWording {
  /// The sentence, as "Alice asks to be your friend."
  final String sentence;

  /// The speech acts drawn under the sentence, in order: each its text and its byline, "by Alice", the person asked as `you`.
  final List<({String text, String byline})> speech;

  /// The lines drawn under the sentence and its speech acts, as "Developer: Acme".
  final List<String> notes;

  /// The question drawn above the buttons, as "Befriend Alf?" or "Approve?"; null where none is.
  final String? question;

  /// The labels of the buttons granting `yes` and `no`.
  final String yes, no;

  const ApprovalWording(this.sentence, this.speech, this.yes, this.no,
      {this.notes = const [], this.question});

  /// The wording of [attempt], `attempt(I, N, K, C, R)`, or null where it is not one: the words of GAI's tasks where they word the class `C`, and a sentence built from `C` otherwise.  [state] is the last view the agent asked wrote on construct 0, with which the widget is drawn ("A declared widget is drawn also with the last value drawn on construct 0", vGLP e924f73, Section 6.2), where it tells whom the question is asked; null where no view is drawn.  [classes] are the classes the platform's `Class` offers, each its functor and arity, `meet/2`, read off the default widget of `Approval` ([classesOf]); null where not read.  CSSN's app is compiled from its contract with pragmas, so its words are the contract's, drawn by the worded path ([_approvalWorded]), and none of its words is written here.  GKK's meet and introduce are worded in GKK's words in every app ([_worded]).
  static ApprovalWording? of(GTerm attempt,
      [SocialState? state, Set<String>? classes]) {
    final (af, xs) = ctorArgs(attempt);
    if (af != 'attempt' || xs.length != 5) return null;
    final (schema, args) = ctorArgs(xs[3]);
    if (schema.isEmpty) return null;
    return _worded(xs[0], schema, args, state) ?? _built(xs[0], schema, args);
  }

  /// The classes the platform's `Class` offers, each its functor and arity, `meet/2`, read off [dflt], the default widget of `Approval ::= approval(Attempt, YesNo?)`: its form `approval`, whose first field is the form `attempt`, whose fourth is the menu of the forms of `Class` (vGLP f0acbc1, Definition "Attempt").  Empty where [dflt] is not of that type.
  static Set<String> classesOf(GTerm dflt) {
    final classes = <String>{};
    final (k, ws) = ctorArgs(RequestOffers._bare(dflt));
    if (k != 'form' || ws.length != 2 || ws[1] is! GList) return classes;
    final fields = (ws[1] as GList).items;
    if (fields.length != 2) return classes;
    final (ak, aws) = ctorArgs(RequestOffers._fieldWidget(fields[0]));
    if (ak != 'form' || aws.length != 2 || aws[1] is! GList) return classes;
    final afs = (aws[1] as GList).items;
    if (afs.length != 5) return classes;
    for (final c in RequestOffers._alternatives(
        RequestOffers._fieldWidget(afs[3]))) {
      final (ck, cws) = ctorArgs(c);
      if (ck == 'form' && cws.length == 2 && cws[1] is GList) {
        classes.add('${ctorArgs(cws[0]).$1}/${(cws[1] as GList).items.length}');
      }
    }
    return classes;
  }

  /// The classes GAI's tasks word, the initiator `I` their first argument `A` and the person asked `Me` their second, a person other than `I`: `befriend(A, Me)`, "Alice asks to be your friend.", Accept and Decline; `join(A, Me, n)`, `n` the group's name, a speech act, "Alice asks you to join the group friends.", its text the group's name, Join and Decline; and `agent_befriend(R, Q, P)` ([_agentBefriend]) and `agent_meet(R, Q, P, Y)` ([_agentMeet]).  And GKK's, which every contract of GSC's extends (programs/gsc/GKK.glp; GSC #5 Cowork, Code_inbox.md, 2026-10-20 17:53 UTC): `meet(A, Me)`, GKK:2, "Alice asks to meet you in person.", Yes and No; and `introduce(A, Me, C)`, GKK:3, `C` the person introduced, "Alice introduces Carol to you.", Yes and No.  And GC's swap, `swap(A, Me, U, V)`, GC:2, `A` giving a `U`-coin and taking a `V`-coin, in the words of Currencies' task (Code_inbox.md, Currencies #11 Cowork, 2026-10-20 18:10 UTC, task 2, (b)): "Alice asks to swap an Alice-coin for a Bob-coin.", Yes and No.  Null for every other class.
  static ApprovalWording? _worded(GTerm initiator, String schema,
      List<GTerm> args, SocialState? state) {
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
      case ('agent_befriend', 3):
        return _agentBefriend(args, state);
      case ('agent_meet', 4):
        return _agentMeet(args, state);
      case ('meet', 2):
        return ApprovalWording(
            '$a asks to meet you in person.', const [], 'Yes', 'No');
      case ('introduce', 3):
        final c = args[2];
        if (SpeechAct.of(c) != null || formatTerm(c) == formatTerm(me)) {
          return null;
        }
        return ApprovalWording('$a introduces ${personName(c)} to you.',
            const [], 'Yes', 'No');
      // GC:2, `swap(p?, q? ; u, v)` (programs/currencies/coins/GC.glp): at
      // `p` it deletes `coin(u)` and adds `coin(v)`, so the initiator gives a
      // `u`-coin and takes a `v`-coin.
      case ('swap', 4):
        final (u, v) = (args[2], args[3]);
        if (SpeechAct.of(u) != null || SpeechAct.of(v) != null) return null;
        return ApprovalWording(
            '$a asks to swap ${aCoin(u)} for ${aCoin(v)}.', const [], 'Yes', 'No');
    }
    return null;
  }

  /// `agent_befriend(R, Q, P)`, the AI agent `R` and `Q`, who know each other, entering a friendship by an act of both and of its principal `P` (GnW:2; `agent_befriend(r?, q?, p?)`, programs/gai/GAI.glp at GLP 9d736804), asked at `Q` and at `P`, in the words of GAI's task (Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, (b) and (c)).  At the principal, whose last view holds `principal_of(R)` (`enter` adds it at `p`): "Alf asks to befriend Bob.", "Approve?", Yes and No.  At `Q`, who holds `R` recorded, `agent_of(R, P, Y)`, since `agent_befriend` requires that `R` knows `Q`, and every agent an AI agent knows, other than its principal, came to know it by `agent_meet` or `agents_meet`, which record it there (GAI's paper, Section 3.3, part 2): "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme", the developer named by the attestation of that record ([SocialState.developerOf]), "Befriend Alf?", Yes and No.  Null where no view is drawn, so that whom it is asked cannot be told, or where the view holds no record of `R` naming `P` its principal.
  static ApprovalWording? _agentBefriend(
      List<GTerm> args, SocialState? state) {
    final (r, q, p) = (args[0], args[1], args[2]);
    if (state == null || SpeechAct.of(p) != null) return null;
    final agent = personName(r);
    if (state.isPrincipalOf(r)) {
      return ApprovalWording(
          '$agent asks to befriend ${personName(q)}.', const [], 'Yes', 'No',
          question: 'Approve?');
    }
    final principal = state.principalOf(r);
    final developer = state.developerOf(r);
    if (principal == null ||
        developer == null ||
        formatTerm(principal) != formatTerm(p)) {
      return null;
    }
    return ApprovalWording(
        '$agent, an AI agent of ${personName(p)}, asks to befriend you.',
        const [],
        'Yes',
        'No',
        notes: ['Developer: ${personName(developer)}'],
        question: 'Befriend $agent?');
  }

  /// `agent_meet(R, Q, P, Y)`, the AI agent `R` coming to know `Q` by an act of both and of its principal `P`, presenting its attestation `Y`, `Q` recording `R`, `P` and `Y` (GnW:2, GAI:2; `agent_meet(r?, q?, p? ; y)`, programs/gai/GAI.glp at GLP 88d85368), asked at `Q` and at `P`, in words drawn by analogy with [_agentBefriend]'s (Code #12's brief for GAI #6's task, Code_inbox.md, 2026-10-20 20:12 UTC, item 4), for GAI's confirmation.  At the principal, whose last view holds `principal_of(R)`: "Alf asks to meet Bob.", "Approve?", Yes and No.  At `Q`, who sees the AI agent's principal and its developer at the meeting: "Alf, an AI agent of Alice, asks to meet you.", "Developer: Acme Ltd", the developer named by the attestation ([SocialState.developerIn]), "Meet Alf?", Yes and No.  Null where no view is drawn, so that whom it is asked cannot be told, or `Y` is no speech act.
  static ApprovalWording? _agentMeet(List<GTerm> args, SocialState? state) {
    final (r, q, p, y) = (args[0], args[1], args[2], args[3]);
    final developer = SocialState.developerIn(y);
    if (state == null || developer == null || SpeechAct.of(p) != null) {
      return null;
    }
    final agent = personName(r);
    if (state.isPrincipalOf(r)) {
      return ApprovalWording(
          '$agent asks to meet ${personName(q)}.', const [], 'Yes', 'No',
          question: 'Approve?');
    }
    return ApprovalWording(
        '$agent, an AI agent of ${personName(p)}, asks to meet you.',
        const [],
        'Yes',
        'No',
        notes: ['Developer: ${personName(developer)}'],
        question: 'Meet $agent?');
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

/// `approval_sentence`: the approval question of a canonical CVA platform drawn as one sentence for the attempt's class and two buttons labelled for it, granting `yes` and `no` at the question's position, `[2]`, as the default widget of `YesNo?`, `buttons([yes, no])`, grants them ([ApprovalWording]); where GAI's task words the class so, with lines under the sentence and a question above the buttons, read with the last view on construct 0 ([output]), with which a declared widget is drawn (vGLP e924f73, Section 6.2).  It is drawn by `named(approval_sentence, D)`, `D` the default widget of `Approval`, whose constructors it reads off the view, the type being one for every canonical CVA platform.  Once the question is answered, the label of the button that grants the answer is shown in place of the buttons.  A view that is not an approval question of that type is drawn as such, named, and takes no input: nothing stands in for it.
Widget approvalSentence(
    BuildContext context,
    OpenConstruct construct,
    GTerm dflt,
    GTerm view,
    List<int> at,
    void Function(GTerm path, GTerm raw) grant,
    GTerm? output) {
  final (f, args) = ctorArgs(view);
  if (f == 'approval' && args.length == 3) {
    return _approvalWorded(context, construct, dflt, args, grant);
  }
  if (f != 'approval' || args.length != 2) {
    return _cannotDraw(approvalSentenceName, view);
  }
  final p = questionAt(args[1]);
  final pending = pendingAt(args[0]) != null;
  final (_, state) = SocialState.read(output);
  final wording = pending
      ? null
      : ApprovalWording.of(args[0], state, ApprovalWording.classesOf(dflt));
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
      for (final n in wording?.notes ?? const <String>[])
        Padding(
          padding: const EdgeInsets.only(top: 4),
          child: Text(n, style: small),
        ),
      if (wording?.question case final q?)
        Padding(
          padding: const EdgeInsets.only(top: 12),
          child: Text(q, style: theme.textTheme.bodyLarge),
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

/// The approval question of a platform compiled from a contract with pragmas, `approval(Attempt, Ws, YesNo?)`: the sentence the contract puts to the person, `Ws`, which the platform writes, each word `text(S)` drawn as it is and `person(P)` by the person's name (vGLP 1126f4f, Section 6.2, "The words a platform shows": "the platform writes it into the approval question, and its widget draws it as it draws any output of the program"); under it the text of each speech act of the attempt's class, the program's output; and the two answers, each drawn by the label the widget term of `YesNo` carries, or by its constant where it carries none ("A construct family draws the label it is given, and the name of the type where it is given none").  Where the contract puts no sentence to the person, `[]`, the class is drawn as the program writes it, its schema and its persons by name.  No word of a contract is held here.
Widget _approvalWorded(BuildContext context, OpenConstruct construct,
    GTerm dflt, List<GTerm> args, void Function(GTerm path, GTerm raw) grant) {
  final p = questionAt(args[2]);
  final pending = pendingAt(args[0]) != null || pendingAt(args[1]) != null;
  final theme = Theme.of(context);
  final words = args[1] is GList ? (args[1] as GList).items : const <GTerm>[];
  // After the sentence, each value of the binding it does not name, by the name of its field, named(F, P) a person and valued(F, K) a number, one to a line (vGLP 110caf4, Section 5.4, "The contract's words": "and shows after it each value of the binding the question does not name, by the name of its field").
  final said = [
    for (final w in words)
      switch (ctorArgs(w)) {
        ('text', [final t]) => displayText(t),
        ('person', [final q]) => personName(q),
        ('named', [_, _]) || ('valued', [_, _]) => '',
        _ => displayText(w),
      }
  ].join();
  final values = [
    for (final w in words)
      switch (ctorArgs(w)) {
        ('named', [final f, final q]) => '${displayText(f)}: ${personName(q)}',
        ('valued', [final f, final k]) => '${displayText(f)}: ${displayText(k)}',
        _ => null,
      }
  ].nonNulls.toList();
  final (af, axs) = ctorArgs(args[0]);
  // The question Start raises asks of an attempt, attempt(I, N, K, C, R), of its class C; the one an arriving hello asks, hello(Q, N), of the meeting N (vGLP, Section 5.4, "The compilation").
  final cls = af == 'attempt' && axs.length == 5
      ? axs[3]
      : af == 'hello' && axs.length == 2
          ? axs[1]
          : null;
  final (cf, cxs) = cls == null ? ('', const <GTerm>[]) : ctorArgs(cls);
  final sentence = pending
      ? '\u2026'
      : said.isNotEmpty
          ? said
          : [
              cf,
              for (final x in cxs)
                if (SpeechAct.of(x) == null) personName(x)
            ].join(' ');
  // A speech act drawn with its signers, innermost first, by name (vGLP 377aa37, Section 6.2, "The words a platform shows").
  final speech = [
    for (final x in cxs)
      if (SpeechAct.of(x) case final a?)
        '${a.text}\n${[for (final q in a.signers) personName(q)].join(', ')}'
  ];
  final buttons = _answers(dflt);
  final pAt = p == null ? null : [for (final i in p.items) (i as GInt).value];
  final answer = formatTerm(args[2]);
  final answered = [
    for (final (shows, c) in buttons)
      if (formatTerm(c) == answer) shows,
    displayText(args[2]),
  ].first;
  return Column(
    crossAxisAlignment: CrossAxisAlignment.start,
    children: [
      Text(sentence,
          key: sentenceKey(construct.id), style: theme.textTheme.titleMedium),
      if (!pending)
        for (final v in values)
          Padding(
            padding: const EdgeInsets.only(top: 4),
            child: Text(v, style: theme.textTheme.bodyLarge),
          ),
      for (final t in speech)
        Container(
          margin: const EdgeInsets.only(top: 8),
          padding: const EdgeInsets.only(left: 10),
          decoration: BoxDecoration(
              border: Border(
                  left: BorderSide(color: Colors.grey.shade400, width: 2))),
          child: Text(t, style: theme.textTheme.bodyLarge),
        ),
      if (p != null && pAt != null) ...[
        const SizedBox(height: 14),
        Row(children: [
          for (final (shows, c) in buttons) ...[
            ElevatedButton(
              key: answerKey(construct.id, displayText(c), pAt),
              onPressed: () => grant(p, c),
              child: Text(shows),
            ),
            const SizedBox(width: 12),
          ],
        ]),
      ] else
        Padding(
          padding: const EdgeInsets.only(top: 8),
          child: Text(answered),
        ),
    ],
  );
}

/// The answers of an approval question, read off [dflt], the default widget of its type, `form(approval, [..., field(L, buttons([C1, C2]))])`: each what its button shows, the label its widget term carries or its constant, and the constant it grants; `yes` and `no` where [dflt] gives none.
List<(String, GTerm)> _answers(GTerm dflt) {
  final (k, ws) = ctorArgs(RequestOffers._bare(dflt));
  if (k == 'form' && ws.length == 2 && ws[1] is GList) {
    final fs = (ws[1] as GList).items;
    if (fs.isNotEmpty) {
      final (bk, bws) = ctorArgs(RequestOffers._fieldWidget(fs.last));
      if (bk == 'buttons' && bws.length == 1 && bws[0] is GList) {
        return [for (final b in (bws[0] as GList).items) buttonOf(b)];
      }
    }
  }
  return const [('yes', GAtom('yes')), ('no', GAtom('no'))];
}

/// A view a named widget cannot draw: named, taking no input.
Widget _cannotDraw(String widget, GTerm view) => Text(
    '$widget cannot draw ${formatTerm(view)}',
    style: TextStyle(color: Colors.red.shade700));

// === The apps of GSC's contracts: last_state and social_menu =================

/// The name a widget declaration gives [lastState]: `Stream(Output) =::= last_state.`, which vGLP's compiler prints into every canonical platform (programs/vglp/cva/compiler/text.glp).
const String lastStateName = 'last_state';

/// The name a widget declaration gives [socialMenu]: `Menu =::= social_menu.`, which vGLP's compiler prints into every canonical platform, with the labels of a contract's pragmas where it carries them (programs/vglp/cva/compiler/README.md).
const String socialMenuName = 'social_menu';

/// The key of the control or the row [what] that [lastState] or [socialMenu] draws for construct [id].
ValueKey<String> socialKey(int id, String what) =>
    ValueKey('$id:social:$what');

/// A group among the keys of [socialKey]: its leader and its name, as their terms.
String groupKey(GTerm leader, GTerm name) =>
    '${formatTerm(leader)}:${formatTerm(name)}';

/// The view of the agent of a canonical platform, as it writes it on its output to its person: "On the program's output to its person ... the agent goal writes its view view(s, K), the machine state s of its app state and the persons K it knows, when it starts and whenever either changes; and, when an approval its person made, by a request or by an answer yes, ends by a refusal, a notice refused([t]_j, q, d): [t]_j the part of the refused attempt, q its refuser and d the reason, from the abort outcome, or from the vote where the agent's own vote no refused it.  The compilation defines Output as the type of the views and the notices, and declares for the type Stream(Output) of the output a widget that draws the last view written and the notices written after it" (vGLP f0acbc1, Section 5.4, "The compilation"; [SocialNotice]).  A machine state is a list of atoms, each as its contract writes it (programs/gsc/GSG.glp, GSN.glp and GSNP.glp), in the view as the platform prints its type `Atom` (programs/apps/<app>/platform.vglp): `identity` and `friend(Q)`; in GSN also `received(X, P)`, `member(C)`, `listed(Q, N)`, `closed(N)`, `posted(X, N)`, `delivered(X, Q, N)` and `received_in(X, A, N)`; in GSNP `item(X)` and `authored(X)` in place of `received` and `received_in`; each `X` a speech act and each `N`, a group's name, a speech act.  `C` is the community name of a group, `<A>.N` in the contract, of type `CName ::= community(Peer) ; dot(CName, Speech)`: "a name term <u>.y1...yn is the community name of the person bound to u followed by the speech acts bound to y1, ..., yn, as <a>.n names the group n led by a" (vGLP c649c0d, Section 5.4, "Act schemas as local specifications"), so a member atom holds `dot(community(A), N)`, the group `N` led by `A`.
class SocialState {
  /// The machine state, its atoms.
  final List<GTerm> atoms;

  /// The persons the agent knows.
  final List<GTerm> known;

  /// The notices written after the last view, in the order written: those the widget draws.
  final List<SocialNotice> notices;

  /// Every notice written on construct 0, in the order written, those before the last view among them.
  final List<SocialNotice> written;

  /// The lists the app shows, where the platform is compiled from a contract with pragmas: each its name and the predicate whose atoms in the machine state it lists, as the view carries them, `view(S, K, [tab(Name, P), ...])` (vGLP 1126f4f, Section 5.4, "The contract's words": "a list's name into the view the agent writes"); null where the view carries none, `view(S, K)`.
  final List<({String name, String predicate, bool counted})>? lists;

  /// The approvals the person made that are pending, as the view carries them, `view(S, K, A)` or `view(S, K, A, Ts)`: each by its request name (vGLP e286276, Section 5.4, "The compilation": "the approvals A its person made that are pending, each by its request name, a request's own and an answer's that of its attempt's part").
  final List<GTerm> pending;

  const SocialState(this.atoms, this.known,
      [this.notices = const [],
      this.written = const [],
      this.lists,
      this.pending = const []]);

  /// The last view written on construct 0, and the notices written after it, read off [output], the value drawn on it: the elements of the stream so far, each written whole a view, `view(S, K)`, or a notice ([SocialNotice]).  The first of the pair is false where [output] is not a stream of views and notices; the second is null where no view is written whole yet.
  static (bool, SocialState?) read(GTerm? output) {
    if (output is! GList) return (false, null);
    List<GTerm>? atoms, known;
    var pending = <GTerm>[];
    List<({String name, String predicate, bool counted})>? lists;
    var notices = <SocialNotice>[];
    final written = <SocialNotice>[];
    for (final e in output.items) {
      if (!_whole(e)) continue;
      final (f, xs) = ctorArgs(e);
      if (f == 'view' &&
          (xs.length == 2 ||
              (xs.length == 3 && xs[2] is GList) ||
              (xs.length == 4 && xs[2] is GList && xs[3] is GList)) &&
          xs[0] is GList &&
          xs[1] is GList) {
        atoms = (xs[0] as GList).items;
        known = (xs[1] as GList).items;
        // view(S, K, A), and, with pragmas, view(S, K, A, Ts): the approvals pending and the lists (vGLP, Section 5.4, "The compilation" and "The contract's words").
        pending = xs.length >= 3 ? (xs[2] as GList).items : <GTerm>[];
        lists = xs.length == 4
            ? [
                for (final t in (xs[3] as GList).items)
                  if (ctorArgs(t) case (final tf, final ts)
                      when tf == 'tab' && (ts.length == 2 || ts.length == 3))
                    (
                      name: displayText(ts[0]),
                      predicate: displayText(ts[1]),
                      // tab(Name, P, counted): the contract adds or deletes P with a multiplicity, and a row shows its count (vGLP 110caf4, Section 5.4, "The contract's words").
                      counted: ts.length == 3 && displayText(ts[2]) == 'counted'
                    ),
              ]
            : null;
        // A notice is drawn until the next view is written: "which the
        // screen shows until the next view" (vGLP f0acbc1, Section 5.4, "The
        // compilation", the short version's).
        notices = <SocialNotice>[];
        continue;
      }
      final n = SocialNotice.of(e);
      if (n == null) return (false, null);
      notices.add(n);
      written.add(n);
    }
    if (atoms == null || known == null) return (true, null);
    return (true, SocialState(atoms, known, notices, written, lists, pending));
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

  /// The person's friends (GSG:1).
  List<GTerm> get friends => [for (final x in _args('friend', 1)) x[0]];

  /// The person's contacts, the persons they know, `knows(Q)`: met in person (GKK:2) or introduced to them (GKK:3) (programs/gsc/GKK.glp: "knows(q) in the state of p says that p holds q's public key").
  List<GTerm> get contacts => [for (final x in _args('knows', 1)) x[0]];

  /// Whether the person knows [q], `knows(q)`.
  bool isContact(GTerm q) => contacts.any((k) => _same(k, q));

  /// The issuers of the coins the person holds, one for each coin, `coin(U)`, in the order of the state (GC; programs/currencies/coins/GC.glp: "coin(u) in the state of p is a coin issued by u and held by p").
  List<GTerm> get coins => [for (final x in _args('coin', 1)) x[0]];

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

  /// Whether the person authored the item [x], `authored(x)`, which sign adds with `item(x)` (GSNP:1; `sign(p? ; x)`, programs/gai/GSNP.glp).
  bool isAuthored(GTerm x) => _args('authored', 1).any((a) => _same(a[0], x));

  /// The agent an item came from: its last signer, the friend who sent it in a message, forwarded it or delivered it in a group, or, for an item the person authored, the person (GSC, Section 3.3: "item(x) in the state of Alice is a speech act x Alice holds, signed by its author or by the agent who forwarded it to Alice, the author and every earlier forwarder signed inside it").
  static GTerm? sender(GTerm x) => SpeechAct.of(x)?.signers.last;

  /// The principal of the AI agent [r] that the person holds recorded, `agent_of(r, p, y)`, with its attestation `y`, which `agent_meet` and `agents_meet` add at the agent the AI agent comes to know (GnW:2, GAI:2; programs/gai/GAI.glp at GLP 88d85368; GAI's paper, Table "The local state of an agent p": "triples (r,q,y) of an AI agent r, its principal q and its attestation y that p holds recorded"); null where the person holds no such record of [r].
  GTerm? principalOf(GTerm r) => [
        for (final x in _args('agent_of', 3))
          if (_same(x[0], r)) x[1]
      ].firstOrNull;

  /// The agent [p] by name, and, where the person holds [p] recorded as an AI agent, its principal: "Alf, an AI agent of Alice" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC: "Author: Alf, an AI agent of Alice").
  String agentName(GTerm p) {
    final principal = principalOf(p);
    return principal == null
        ? personName(p)
        : '${personName(p)}, an AI agent of ${personName(principal)}';
  }

  /// The attestation of the AI agent [r] that the person holds recorded, `y` of `agent_of(r, p, y)`; null where they hold no such record of [r].
  GTerm? attestationOf(GTerm r) => [
        for (final x in _args('agent_of', 3))
          if (_same(x[0], r)) x[2]
      ].firstOrNull;

  /// The developer an attestation [y] names: the first signer of the code it carries.  "The developer of an AI agent is the person who signed its code" (GAI:1), and the attestation is `sgn(x, p)`, `x` the code signed by its developer, signed over by the principal (`enter(p?, r? ; x)`, programs/gai/GAI.glp at GLP 5744daec), so its developer is its first signer (GAI #5 Cowork, Code_inbox.md, 2026-10-19 20:36 UTC, item 5: "the signer of the code item being the developer").  Null where [y] is no speech act.
  static GTerm? developerIn(GTerm y) => SpeechAct.of(y)?.signers.first;

  /// The developer of the AI agent [r] that the person holds recorded, read from its attestation ([developerIn]); null where they hold no record of [r].
  GTerm? developerOf(GTerm r) {
    final y = attestationOf(r);
    return y == null ? null : developerIn(y);
  }

  /// Whether the person is an AI agent, `agent` (`enter` adds it at the AI agent, programs/gai/GAI.glp).
  bool get isAgent => atoms.any((a) => a is GAtom && a.name == 'agent');

  /// The person's AI agents, `principal_of(R)`, which `enter` adds at the principal (Wards:1, GAI:2), in the order of the state.
  List<GTerm> get aiAgents => [for (final x in _args('principal_of', 1)) x[0]];

  /// Whether the person is the principal of [r], `principal_of(r)`.
  bool isPrincipalOf(GTerm r) => aiAgents.any((a) => _same(a, r));

  /// Whether the person's view shows [q] an AI agent: their own, `principal_of(q)`, or one they hold recorded, `agent_of(q, p, y)`.
  bool isKnownAgent(GTerm q) => isPrincipalOf(q) || principalOf(q) != null;

  /// Whether the person retired their AI agent [r], `retired_of(r)`, which `retire(p?, r)` adds at `p` (GAI:4).
  bool hasRetired(GTerm r) =>
      _args('retired_of', 1).any((x) => _same(x[0], r));

  /// Whether the person holds the item [x], `item(x)`.
  bool holdsItem(GTerm x) => items.any((i) => _same(i, x));

  /// The person's children, `parenting(R)`, which `enter` and `enter_2` add at a parent (CSSN:1, Wards:1; programs/cssn/CSSN.glp), in the order of the state.
  List<GTerm> get children => [for (final x in _args('parenting', 1)) x[0]];

  /// The person's parents, `parent(P)`, which `enter` and `enter_2` add at the child.
  List<GTerm> get parents => [for (final x in _args('parent', 1)) x[0]];

  /// Whether the person is a child, `child`, which `enter` and `enter_2` add at the child and a declaration of age deletes (CSSN:1, CSSN:3).
  bool get isChild => atoms.any((a) => a is GAtom && a.name == 'child');

  /// Whether the person is [q]'s parent, `parenting(q)`.
  bool isParentOf(GTerm q) => children.any((c) => _same(c, q));

  /// Whether the person is [q]'s child, `parent(q)`.
  bool isChildOf(GTerm q) => parents.any((p) => _same(p, q));

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

/// The classes of a group (GSN:2 to GSN:5, programs/gsc/GSN.glp and GSNP.glp), and CSSN's of a child's membership, `child_join`, `child_join_2`, `child_leave` and `child_close` (GnW:2 and GnW:3, programs/cssn/CSSN.glp), which "belong in Groups, as join, leave and close do" (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 14:58 UTC): an approval question or a notice of one of them concerns the Groups tab, and every other the Friends tab.
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

/// The classes of GC's coins, `mint`, `swap`, `pay` and `redeem` (GC:1 to GC:4, programs/currencies/coins/GC.glp): an approval question or a notice of one of them concerns the Coins tab.
const Set<String> coinClasses = {'mint', 'swap', 'pay', 'redeem'};

/// A notice the agent writes on its output when an approval its person made, by a request or by an answer yes, ends by a refusal: `refused(P, Q, D)`, `P` the part of the refused attempt, a term of its class, `Q` its refuser, and `D` the reason, `declined`, or `unable(Lacks, Has)`, the atoms the schema requires at the refuser that its state lacks and those it forbids that its state has, as the contract writes them: "a notice refused([t]_j, q, d): [t]_j the part of the refused attempt, q its refuser and d the reason, from the abort outcome, or from the vote where the agent's own vote no refused it"; "A vote unable of a compiled program carries the atoms the schema requires at the voter that its state lacks, and those it forbids that its state has, and the notice shows them" (vGLP f0acbc1, Section 5.4, "The compilation" and "Act schemas"; the types `Output ::= view(List(Atom), List(Key)) ; refused(Class, Key, Reason)` and `Reason ::= declined ; unable(List(Atom), List(Atom))`, programs/apps/<app>/platform.vglp and programs/cssn/platform.vglp).  "When a refusal ends an approval, the person who made it is told who refused the transaction and why, declined or unable" (Section 5.4, "Triggers"): the person told is any guard whose approval the refusal ends, the one who requested the transaction or one who answered yes, so the sentence does not take the person to have proposed it ([sentence]).  It is drawn as one sentence in the tab it concerns.
class SocialNotice {
  /// The request name of the part of the refused attempt, its schema-binding: `befriend(alice, bob)` (vGLP e286276, Section 5.4, "The compilation": "a notice refused(N, q, d): N the request name of the part of the refused attempt"); on a platform printed before it, the part's term.
  final GTerm part;

  /// The refuser, the participant whose vote no refused the attempt.
  final GTerm by;

  /// Whether the reason is `declined`, the refuser's person having answered no; otherwise it is `unable(Lacks, Has)`.
  final bool declined;

  /// For `unable(Lacks, Has)`: the atoms the schema requires at the refuser that its state lacks, and those it forbids that its state has.
  final List<GTerm> lacks, has;

  /// The reason, as the program writes it: `declined` or `unable(Lacks, Has)`.
  final GTerm? reason;

  const SocialNotice(this.part, this.by,
      {this.declined = false,
      this.lacks = const [],
      this.has = const [],
      this.reason});

  /// The notice [t], `refused(P, Q, D)`, or null where it is not one.
  static SocialNotice? of(GTerm t) {
    final (f, xs) = ctorArgs(t);
    if (f != 'refused' || xs.length != 3 || ctorArgs(xs[0]).$1.isEmpty) {
      return null;
    }
    final d = xs[2];
    if (d is GAtom && d.name == 'declined') {
      return SocialNotice(xs[0], xs[1], declined: true, reason: d);
    }
    // A request dropped where the agent can carry out no part it names: refused(N, p, unable) (vGLP, Section 5.4, "The compilation").
    if (d is GAtom && d.name == 'unable') {
      return SocialNotice(xs[0], xs[1], reason: d);
    }
    final (u, us) = ctorArgs(d);
    if (u == 'unable' && us.length == 2 && us[0] is GList && us[1] is GList) {
      return SocialNotice(xs[0], xs[1],
          lacks: (us[0] as GList).items,
          has: (us[1] as GList).items,
          reason: d);
    }
    return null;
  }

  /// The schema of the part's class.
  String get schema => ctorArgs(part).$1;

  /// The functor of a term: a part's schema.
  static String _functor(GTerm t) => ctorArgs(t).$1;

  /// Whether the notice concerns the Groups tab.
  bool get concernsGroup => groupClasses.contains(schema);

  /// Whether the notice concerns the Coins tab.
  bool get concernsCoin => coinClasses.contains(schema);

  /// The tab the notice is drawn in: Groups for a class of a group, Coins for a class of the coins, and Friends for every other.
  ScreenTab get tab => concernsGroup
      ? ScreenTab.groups
      : concernsCoin
          ? ScreenTab.coins
          : ScreenTab.friends;

  static bool _same(GTerm a, GTerm b) => formatTerm(a) == formatTerm(b);

  /// The text of a speech act, or the term as the person reads it.
  static String _text(GTerm x) => SpeechAct.of(x)?.text ?? displayText(x);

  /// Whether [t] is a person: no speech act and no list.
  static bool _person(GTerm t) => t is! GList && SpeechAct.of(t) == null;

  /// [ws] joined as English joins them: "Alice", "Alice and Bob", "Alice, Bob and Carol".
  static String _and(List<String> ws) => ws.length < 2
      ? ws.join()
      : '${ws.take(ws.length - 1).join(', ')} and ${ws.last}';

  /// The notice in plain words, [me] the person told, where known: who refused the transaction and why (vGLP f0acbc1, Section 5.4, "Triggers"), the transaction named as the person reads it ([_act]), the person as "you".
  ///
  ///   - `declined`: "Bob declined your friendship.", "Dave declined the friendship of Carol and Dave."; a joiner declining a group, "Bob declined to join the group friends." (GAI #3 Cowork, Code_inbox.md, 2026-10-19 10:45 UTC; "Bob declined your friendship" approved by Udi, vGLP #15 Cowork to Code, 2026-10-19 13:34 UTC);
  ///   - `unable(Lacks, Has)`: the refuser's machine refused it, and the atoms lacked and held in words, each said of the refuser ([_words]): "Bob's machine refused your friendship: Bob has no identity.";
  ///   - the refuser the person: "Your machine refused your message to Carol: you and Carol are not friends.", the person's own machine having refused it at once, as a Propose that refuses or a guard's vote no does (Definitions "Proposing" and "Answering and Voting").
  String sentence(GTerm? me) {
    bool isMe(GTerm q) => me != null && _same(q, me);
    final act = _act(isMe);
    if (declined) {
      final (f, xs) = ctorArgs(part);
      if (f == 'join' &&
          xs.length >= 3 &&
          _same(xs[1], by) &&
          !isMe(by)) {
        return '${personName(by)} declined to join the group '
            '${_text(xs.last)}.';
      }
      return isMe(by)
          ? 'You declined $act.'
          : '${personName(by)} declined $act.';
    }
    final reasons = [
      for (final a in lacks) _words(a, false, isMe),
      for (final a in has) _words(a, true, isMe),
    ];
    final head =
        '${isMe(by) ? 'Your' : "${personName(by)}'s"} machine refused $act';
    return reasons.isEmpty ? '$head.' : '$head: ${reasons.join('; ')}.';
  }

  /// The transaction the part names, as the person reads it, a noun phrase, the person as "you" and "your": a friendship, "your friendship with Bob", or "your friendship" where the other is the refuser, "the friendship of Carol and Dave" where the person is neither; its end, "the end of your friendship with Bob"; a group's acts, "Bob's joining the group friends", "the closing of the group friends"; "your message to Carol"; CSSN's, "Alice's entering Carol as their child", "your entering Dave as your child, with Grace", "Bob's declaring Dave of age", and a child's meeting as a meeting, "the meeting of Carol and Dave"; and, for a class not named here, "the act" and its schema and persons, "the act agent unfriend of Alf, Bob and Alice".
  String _act(bool Function(GTerm) isMe) {
    final (f, xs) = ctorArgs(part);
    String who(GTerm q) => isMe(q) ? 'you' : personName(q);
    String whose(GTerm q) => isMe(q) ? 'your' : "${personName(q)}'s";
    String friendship(GTerm a, GTerm b) {
      for (final (x, y) in [(a, b), (b, a)]) {
        if (isMe(x)) {
          return _same(y, by)
              ? 'your friendship'
              : 'your friendship with ${personName(y)}';
        }
      }
      return 'the friendship of ${personName(a)} and ${personName(b)}';
    }

    bool persons(List<int> at) =>
        at.every((i) => i < xs.length && _person(xs[i]));
    final group =
        xs.isNotEmpty ? 'the group ${_text(xs.last)}' : 'the group';
    switch (f) {
      case 'befriend' || 'agent_befriend' || 'agents_befriend'
          when persons([0, 1]):
        return friendship(xs[0], xs[1]);
      case 'unfriend' || 'unfriend_p' || 'unfriend_q' || 'agent_unfriend'
          when persons([0, 1]):
        return 'the end of ${friendship(xs[0], xs[1])}';
      case 'join' when xs.length >= 3 && persons([1]):
        return '${whose(xs[1])} joining $group';
      case 'leave' when xs.length >= 3 && persons([1]):
        return '${whose(xs[1])} leaving $group';
      case 'close':
        return 'the closing of $group';
      case 'create':
        return 'the creation of $group';
      case 'drop' when xs.length == 3 && persons([1]):
        return 'the dropping of ${who(xs[1])} from $group';
      case 'post' || 'post_own' || 'post_forward' when xs.length >= 3:
        return 'the post "${_text(xs[xs.length - 2])}" in $group';
      case 'deliver' || 'deliver_own' when xs.length == 4 && persons([1]):
        return 'the delivery of "${_text(xs[2])}" to ${who(xs[1])} in $group';
      case 'message' when xs.length == 3 && persons([0, 1]):
        return '${whose(xs[0])} message to ${who(xs[1])}';
      case 'forward' when xs.length == 3 && persons([1]):
        return 'the forwarding of "${_text(xs[2])}" to ${who(xs[1])}';
      case 'sign' when xs.length == 2:
        return 'the signing of "${_text(xs[1])}"';
      case 'establish' when xs.length == 1 && persons([0]):
        return 'the establishing of ${whose(xs[0])} identity';
      // GAI's agent_meet and agents_meet: the meeting of the two named first.
      case 'meet' || 'agent_meet' || 'agents_meet'
          when persons([0, 1]):
        for (final (x, y) in [(xs[0], xs[1]), (xs[1], xs[0])]) {
          if (isMe(x)) {
            return _same(y, by)
                ? 'your meeting'
                : 'your meeting with ${personName(y)}';
          }
        }
        return 'the meeting of ${personName(xs[0])} and ${personName(xs[1])}';
      case 'introduce' when xs.length == 3 && persons([0, 1, 2]):
        return '${whose(xs[0])} introduction of ${who(xs[2])} to '
            '${who(xs[1])}';
      case 'enter' when xs.length == 3 && persons([0, 1]):
        return 'the creation of ${whose(xs[0])} AI agent '
            '${personName(xs[1])}';
      case 'retire' when xs.length == 2 && persons([0, 1]):
        return '${whose(xs[0])} retiring ${who(xs[1])}';
    }
    final named = [
      for (final x in xs)
        if (_person(x)) who(x)
    ];
    return 'the act ${f.replaceAll('_', ' ')}'
        '${named.isEmpty ? '' : ' of ${_and(named)}'}';
  }

  /// The atom [atom] in words, said of the refuser, the person as "you": held where [held], the schema forbidding it, and lacked otherwise, the schema requiring it.  `identity`, "Bob has no identity"; `friend(Q)`, "Bob and Carol are not friends"; `child`, "Bob is a child"; `parent(Q)`, "Alice is not Carol's parent"; `parenting(R)`, "Alice is Carol's parent"; `coparent(Q)`, "Bob and Grace are parents of a child together"; `coparented`, "Dave has two parents"; `retired`; `agent`, "Alf is an AI agent"; `principal_of(R)` and `principal(P)`; `member(C)`, "Bob is not a member of the group friends"; `listed(Q, N)`, "Bob is not listed in Alice's group friends"; `closed(N)`; `item(X)` and `authored(X)`, a speech act by its text; `retired_of(R)`; and any other atom by its predicate and its arguments, "Bob holds received "Hi" Alice".
  String _words(GTerm atom, bool held, bool Function(GTerm) isMe) {
    final v = by;
    final you = isMe(v);
    final s = you ? 'you' : personName(v);
    final be = you ? 'are' : 'is';
    final have = you ? 'have' : 'has';
    final hold = you ? 'hold' : 'holds';
    final doNot = you ? 'do not' : 'does not';
    final not = held ? '' : ' not';
    String who(GTerm q) => isMe(q) ? 'you' : personName(q);
    String whose(GTerm q) => isMe(q) ? 'your' : "${personName(q)}'s";
    String isOf(GTerm q) => isMe(q) ? 'are' : 'is';
    String say(GTerm x) => x is GList
        ? _and([for (final y in x.items) say(y)])
        : SpeechAct.of(x) != null
            ? '"${_text(x)}"'
            : ctorArgs(x).$2.isEmpty && x is GAtom
                ? who(x)
                : displayText(x);
    final (f, xs) = ctorArgs(atom);
    switch ((f, xs.length)) {
      case ('identity', 0):
        return held ? '$s $have an identity' : '$s $have no identity';
      case ('retired', 0):
        return '$s $be$not retired';
      case ('agent', 0):
        return '$s $be$not an AI agent';
      case ('friend', 1):
        return '$s and ${who(xs[0])} are$not friends';
      case ('knows', 1):
        return held
            ? '$s ${you ? 'know' : 'knows'} ${who(xs[0])}'
            : '$s $doNot know ${who(xs[0])}';
      case ('principal_of', 1):
        return '$s $be$not ${whose(xs[0])} principal';
      case ('principal', 1):
        return '${who(xs[0])} ${isOf(xs[0])}$not ${whose(v)} principal';
      case ('member', 1):
        if (SocialState._group(xs[0]) case (_, final n)) {
          return '$s $be$not a member of the group ${_text(n)}';
        }
      case ('listed', 2):
        return '${who(xs[0])} ${isOf(xs[0])}$not listed in ${whose(v)} group '
            '${_text(xs[1])}';
      case ('closed', 1):
        return '${whose(v)} group ${_text(xs[0])} is$not closed';
      case ('item', 1):
        return held
            ? '$s $hold ${say(xs[0])}'
            : '$s $doNot hold ${say(xs[0])}';
      case ('authored', 1):
        return held
            ? '$s authored ${say(xs[0])}'
            : '$s did not author ${say(xs[0])}';
      case ('retired_of', 1):
        return held
            ? '$s retired ${who(xs[0])}'
            : '$s did not retire ${who(xs[0])}';
    }
    final words = [f.replaceAll('_', ' '), for (final x in xs) say(x)].join(' ');
    return held ? '$s $hold $words' : '$s $doNot hold $words';
  }
}

/// The requests a request question offers, read off the default widget `D` of its type, `Menu ::= menu(Key, List(Key), Request?)`: "It is drawn with the default widget of its type beside its name, from which it reads the type's constructors, so that a menu declared once lists the alternatives of the request type of each program it is declared for" (vGLP, Section 6.2, the paragraph after Proposition "Default Widgets").  The alternatives of the request type are `propose(P)`, `P` a term of Proposal naming a guarded part of a class, and `known(Q)` (vGLP, Section 5.4, "The compilation").
/// Where the button of a request is drawn, as its widget term says, `act(place(T, M, Ps, Gs, Is, Ks, Ws), W)` (vGLP 932c710, Section 6.2, "The words a platform shows": "The button of an act that names, beside the person proposing it, a person, a group or an item is drawn beside each one the view lists, on whichever tab lists it, the row filling the act's variable, and the button of an act that names none on the tab of its contract"; bridge.dart, [actCtor]): the [tab] of its contract; the place [me] of the person proposing it; the places [persons] a person the view lists fills; the [groups] a group it lists fills, each the place of its leader and that of its name; the [items] a row of a list fills, each the place of its speech act and the predicate the list lists; the places [keys] a field for a key fills where the peer is not listed; and the text of its request form, [words], each a string or the place of a person.  Places count from 1.
class ActInfo {
  final String tab;
  final int me;
  final List<int> persons;
  final List<(int, int)> groups;
  final List<(int, String)> items;
  final List<int> keys;
  final List<Object> words;
  /// The atoms of the proposer's state that fill the places no row fills, each its predicate and the place of each argument, 0 where it is none: GAI's agent_of(r, p, y) at the principal, its attestation y (GAI.glp at a336b30d).
  final List<(String, List<int>)> held;
  /// The precondition of the act at the proposer's role, each atom it requires or deletes there, and each it forbids, [forbids] true, by its predicate and each argument: the place [int] it is at, a group by the places of its leader and its name, `(int, int)`, or null, any (vGLP feda043, Section 6.2, "The words a platform shows": "where the viewer's state holds the atoms the act requires at the viewer's role and lacks those it forbids there"; `req(P, As)` and `forb(P, As)`, programs/self.glp).
  final List<({bool forbids, String pred, List<Object?> args})> conds;
  /// The atoms of the viewer's state from which the schemas prove that the person at a place holds an atom the act forbids there, `unless(P, As)` (vGLP 893d4ea, Section 6.2: "the viewer holding the first shows that x holds the second, and an act that forbids the second at x's role is not drawn beside x").
  final List<({String pred, List<Object?> args})> unless;
  /// The atoms of the viewer's state from which the schemas prove that the person at a place holds an atom its role requires, each with the atom it proves, [proves], `proved(Q, Ws, P, As)`: the place offers only a person for whom, for each such atom, the viewer holds an atom matching one of its entries (vGLP 3ec07a2, Section 6.2, "The words a platform shows": "where the place's role requires an atom that the schemas prove of a person from the viewer's records, as above, only a person so proved is offered").
  final List<({String proves, String pred, List<Object?> args})> proved;

  const ActInfo(this.tab, this.me, this.persons, this.groups, this.items,
      this.keys, this.words,
      [this.held = const [],
      this.conds = const [],
      this.unless = const [],
      this.proved = const []]);

  /// The places the atoms of [held] fill.
  Set<int> get heldPlaces => {
        for (final (_, ks) in held)
          for (final k in ks)
            if (k > 0) k
      };

  /// The place term [p], `place(T, M, Ps, Gs, Is, Ks, Ws)`; null where it is not one.
  static ActInfo? of(GTerm p) {
    final (f, xs0) = ctorArgs(p);
    if (f != 'place' || xs0.length != 7 || xs0[4] is! GList) return null;
    final its = <(int, String)>[];
    final held = <(String, List<int>)>[];
    final conds = <({bool forbids, String pred, List<Object?> args})>[];
    final unless = <({String pred, List<Object?> args})>[];
    final proved = <({String proves, String pred, List<Object?> args})>[];
    for (final it in (xs0[4] as GList).items) {
      final (itf, itxs) = ctorArgs(it);
      if (itf == 'proved' &&
          itxs.length == 4 &&
          itxs[1] is GList &&
          itxs[3] is GList) {
        final args = <Object?>[];
        for (final a in (itxs[3] as GList).items) {
          final (af, axs) = ctorArgs(a);
          if (af == 'at' && axs.length == 1 && axs[0] is GInt) {
            args.add((axs[0] as GInt).value);
          } else if (af == 'any' && axs.isEmpty) {
            args.add(null);
          } else {
            return null;
          }
        }
        final ws = [for (final w in (itxs[1] as GList).items) displayText(w)];
        proved.add((
          proves: '${displayText(itxs[0])}(${ws.join(',')})',
          pred: displayText(itxs[2]),
          args: args
        ));
      } else if ((itf == 'req' || itf == 'forb' || itf == 'unless') &&
          itxs.length == 2 &&
          itxs[1] is GList) {
        final args = <Object?>[];
        for (final a in (itxs[1] as GList).items) {
          final (af, axs) = ctorArgs(a);
          if (af == 'at' && axs.length == 1 && axs[0] is GInt) {
            args.add((axs[0] as GInt).value);
          } else if (af == 'grp' &&
              axs.length == 2 &&
              axs[0] is GInt &&
              axs[1] is GInt) {
            args.add(((axs[0] as GInt).value, (axs[1] as GInt).value));
          } else if (af == 'any' && axs.isEmpty) {
            args.add(null);
          } else {
            return null;
          }
        }
        if (itf == 'unless') {
          unless.add((pred: displayText(itxs[0]), args: args));
        } else {
          conds.add((forbids: itf == 'forb', pred: displayText(itxs[0]), args: args));
        }
      } else if (itf == 'item' && itxs.length == 2 && itxs[0] is GInt) {
        its.add(((itxs[0] as GInt).value, displayText(itxs[1])));
      } else if (itf == 'atom' && itxs.length == 2 && itxs[1] is GList) {
        final ks = [
          for (final k in (itxs[1] as GList).items) k is GInt ? k.value : -1
        ];
        if (ks.contains(-1)) return null;
        held.add((displayText(itxs[0]), ks));
      } else {
        return null;
      }
    }
    final xs = [...xs0.take(4), ...xs0.skip(5)];
    int? n(GTerm t) => t is GInt ? t.value : null;
    List<int>? ints(GTerm t) {
      if (t is! GList) return null;
      final r = [for (final x in t.items) n(x)];
      return r.contains(null) ? null : [for (final i in r) i!];
    }
    final ps = ints(xs[2]), ks = ints(xs[4]), m = n(xs[1]);
    if (ps == null || ks == null || m == null || xs[3] is! GList ||
        xs[5] is! GList) {
      return null;
    }
    final gs = <(int, int)>[];
    for (final g in (xs[3] as GList).items) {
      final (gf, gxs) = ctorArgs(g);
      if (gf != 'group' || gxs.length != 2) return null;
      final j = n(gxs[0]), k = n(gxs[1]);
      if (j == null || k == null) return null;
      gs.add((j, k));
    }
    final ws = <Object>[];
    for (final w in (xs[5] as GList).items) {
      final (wf, wxs) = ctorArgs(w);
      if (wf == 'say' && wxs.length == 1) {
        ws.add(displayText(wxs[0]));
      } else if (wf == 'at' && wxs.length == 1 && n(wxs[0]) != null) {
        ws.add(n(wxs[0])!);
      } else {
        return null;
      }
    }
    return ActInfo(
        displayText(xs[0]), m, ps, gs, its, ks, ws, held, conds, unless, proved);
  }

  /// Whether the viewer's state [atoms] proves of [x], at the place [k], each atom the act requires at that place's role that the schemas prove from the viewer's records ([proved]): for each, the viewer holds an atom matching one of its entries with [x] at [k].
  bool provedAt(List<GTerm> atoms, int k, GTerm x) {
    final need = <String, List<({String pred, List<Object?> args})>>{};
    for (final e in proved) {
      if (e.args.contains(k)) {
        (need[e.proves] ??= []).add((pred: e.pred, args: e.args));
      }
    }
    bool matches(GTerm a, String pred, List<Object?> args) {
      final (f, xs) = ctorArgs(a);
      if (f != pred || xs.length != args.length) return false;
      for (var j = 0; j < args.length; j++) {
        if (args[j] == k && formatTerm(xs[j]) != formatTerm(x)) return false;
      }
      return true;
    }

    return need.values.every((es) =>
        es.any((e) => atoms.any((a) => matches(a, e.pred, e.args))));
  }

  /// Whether the act names, beside the person proposing it, a person, a group or an item.
  bool get namesOne =>
      persons.isNotEmpty || groups.isNotEmpty || items.isNotEmpty;

  /// The places an item fills.
  Set<int> get itemPlaces => {for (final (k, _) in items) k};

  /// How many persons, groups and items the act names beside the person proposing it: its persons, its items, each person a field names by key, [kinds] `peer`, and each group once, but those a held atom of the proposer's state fills (vGLP feda043, Section 6.2: "The button of an act that names, beside the person proposing it, one person, group or item ...; an act that names two or more is a form at the top of the tab of its contract").
  int namedCount(List<String> kinds) {
    final gp = {for (final (j, k) in groups) ...[j, k]};
    // A held atom fills the places no row and no field fills, as GAI's attestation; a place it names that a person or an item fills stays named.
    final hp = heldPlaces.difference({...persons, ...itemPlaces, ...keys});
    final ps = <int>{
      ...persons,
      ...itemPlaces,
      ...keys,
      for (var i = 1; i <= kinds.length; i++)
        if (kinds[i - 1] == 'peer') i,
    }..removeAll({me, ...gp, ...hp});
    return ps.length + groups.length;
  }

  /// Whether the state [atoms] meets the act's precondition at the proposer's role under the binding [at], its places filled: each atom it requires or deletes there held, a place not filled matching any argument, and each it forbids whose places are all filled lacking (vGLP feda043, Section 6.2).
  bool holds(List<GTerm> atoms, Map<int, GTerm> at) {
    bool matches(GTerm a, String pred, List<Object?> args) {
      final (f, xs) = ctorArgs(a);
      if (f != pred || xs.length != args.length) return false;
      for (var j = 0; j < args.length; j++) {
        final x = args[j];
        if (x is int) {
          final v = at[x];
          if (v != null && formatTerm(v) != formatTerm(xs[j])) return false;
        } else if (x is (int, int)) {
          final l = at[x.$1], n = at[x.$2];
          if (l == null || n == null) continue;
          final g = SocialState._group(xs[j]);
          if (g == null ||
              formatTerm(g.$1) != formatTerm(l) ||
              formatTerm(g.$2) != formatTerm(n)) {
            return false;
          }
        }
      }
      return true;
    }

    for (final u in unless) {
      final bound = u.args.every((x) => x is! int || at.containsKey(x)) &&
          u.args.any((x) => x is int);
      if (bound && atoms.any((a) => matches(a, u.pred, u.args))) return false;
    }
    for (final c in conds) {
      if (c.forbids) {
        final bound = c.args.every((x) => x is int
            ? at.containsKey(x)
            : x is (int, int)
                ? at.containsKey(x.$1) && at.containsKey(x.$2)
                : false);
        if (bound && atoms.any((a) => matches(a, c.pred, c.args))) return false;
      } else if (!atoms.any((a) => matches(a, c.pred, c.args))) {
        return false;
      }
    }
    return true;
  }

  /// Whether the state [atoms] of the viewer [me] meets the act's own precondition at the proposer's role: holds the atoms the act requires there that name no other role or variable, and lacks those it forbids there that name none (vGLP 897d1ce, Section 6.2, "The words a platform shows": "such a form or button is drawn only where the viewer's state holds, at some guarding role of the act, the atoms the act requires there that name no other role or variable, and lacks those it forbids there that name none").
  bool ownHolds(List<GTerm> atoms, GTerm me) {
    final own = [
      for (final c in conds)
        if (c.args.every((x) => x is int && x == this.me)) c
    ];
    return ActInfo(tab, this.me, persons, groups, items, keys, words, held, own)
        .holds(atoms, {this.me: me});
  }

  /// Whether the act requires or deletes, at the proposer's role, an atom of [pred] naming a place of [places] (vGLP feda043, Section 6.2: "beside a row of a tab only where the act requires or deletes, at the viewer's role, the atom of that tab's predicate naming the row's item").
  bool requiresOf(String pred, Set<int> places) => conds.any((c) =>
      !c.forbids &&
      c.pred == pred &&
      c.args.any((x) => x is int
          ? places.contains(x)
          : x is (int, int) && places.contains(x.$2)));
}

class RequestOffers {
  /// The alternatives of Proposal, each its functor and arity, `befriend/2`.
  final Set<String> proposals;

  /// Whether the request type has `known(Q)`.
  final bool known;

  /// The label the widget declaration gives each alternative of Proposal, by its functor and arity, where it gives one: the button of the contract's pragma, "Befriend" (vGLP 1126f4f, Section 6.2, "The words a platform shows": "a widget term carries a label beside each constructor"; programs/vglp/cva/compiler/README.md).
  final Map<String, String> labels;

  /// The names of the fields of each alternative of Proposal, by its functor and arity, in order: the name the declaration gives the field, the contract's in angle brackets, or the name of its declared type ("A construct family draws the label it is given, and the name of the type where it is given none").
  final Map<String, List<String>> fields;

  /// The label of `known(Q)`, or null where none is given, and the name of its field.
  final String? knownLabel;
  final String knownField;

  /// Where the button of each alternative of Proposal is drawn, by its functor and arity, where the widget term says ([ActInfo]; `act(P, W)`, bridge.dart).
  final Map<String, ActInfo> acts;

  /// Every place the button of each alternative of Proposal is drawn at, one for each of its guarding roles, the first that of [acts] (vGLP 110caf4, Section 6.2: "the button of an act is drawn at each of its guarding roles, beside each row that fills its variable"; nested `act(P, W)`, bridge.dart).
  final Map<String, List<ActInfo>> allActs;

  /// The kind of the field of each place of each alternative of Proposal, by its functor and arity, in order, read off its widget: `peer`, `number`, `text`, `speech` or `other`.
  final Map<String, List<String>> kinds;

  const RequestOffers(this.proposals, this.known,
      {this.labels = const {},
      this.fields = const {},
      this.knownLabel,
      this.knownField = 'Peer',
      this.acts = const {},
      this.allActs = const {},
      this.kinds = const {}});

  /// The alternative of Proposal of functor [f], its key `f/n`; null where there is none.
  String? keyOf(String f) {
    for (final k in proposals) {
      if (k.substring(0, k.lastIndexOf('/')) == f) return k;
    }
    return null;
  }

  /// The tab the request of functor [f] is drawn on: the tab of its contract; null where the widget term does not say.
  String? tabOf(String f) {
    final k = keyOf(f);
    return k == null ? null : acts[k]?.tab;
  }

  /// What the request of functor [f] is called: its label, or [f] where none is given.
  String labelOf(String f) {
    final k = keyOf(f);
    return k == null ? f : labels[k] ?? f;
  }

  bool has(String f, int n) => proposals.contains('$f/$n');

  /// What the request `f/n` is called: its label, or its functor where none is given.
  String label(String f, int n) => labels['$f/$n'] ?? f;

  /// The name of the field [i], from 1, of the request `f/n`.
  String field(String f, int n, int i) {
    final fs = fields['$f/$n'];
    return fs != null && i >= 1 && i <= fs.length ? fs[i - 1] : '';
  }

  /// What `known(Q)` is called: its label, or its functor.
  String get knownName => knownLabel ?? 'known';

  /// Whether the declaration labels any request: the platform is compiled from a contract with pragmas.
  bool get worded => labels.isNotEmpty;

  /// The requests [dflt], the default widget of Menu, offers: its form `menu`, whose third field is the menu of the request type's alternatives, the form `propose` of one field, the menu of the forms of Proposal, and the form `known`; each with the label its declaration gives it, `label(S, form(F, ...))`, and its fields' names.
  static RequestOffers of(GTerm dflt) {
    final proposals = <String>{};
    final labels = <String, String>{};
    final fields = <String, List<String>>{};
    final acts = <String, ActInfo>{};
    final allActs = <String, List<ActInfo>>{};
    final kinds = <String, List<String>>{};
    var known = false;
    String? knownLabel;
    var knownField = 'Peer';
    List<String> names(GList es) => [
          for (final e in es.items)
            if (ctorArgs(e).$2.isNotEmpty) displayText(ctorArgs(e).$2[0])
        ];
    final (k, ws) = ctorArgs(_bare(dflt));
    if (k == 'form' && ws.length == 2 && ws[1] is GList) {
      final fs = (ws[1] as GList).items;
      if (fs.length == 3) {
        for (final raw in _items(_fieldWidget(fs[2]))) {
          final altLabel = labelledWidget(raw)?.$1;
          final (ak, aws) = ctorArgs(_bare(raw));
          if (ak != 'form' || aws.length != 2 || aws[1] is! GList) continue;
          final f = ctorArgs(aws[0]).$1;
          final afs = aws[1] as GList;
          if (f == 'known' && afs.items.length == 1) {
            known = true;
            knownLabel = altLabel;
            knownField = names(afs).firstOrNull ?? knownField;
          }
          if (f == 'propose' && afs.items.length == 1) {
            for (final praw in _items(_fieldWidget(afs.items[0]))) {
              final pl = labelledWidget(praw)?.$1;
              final (pk, pws) = ctorArgs(_bare(praw));
              if (pk == 'form' && pws.length == 2 && pws[1] is GList) {
                final key =
                    '${ctorArgs(pws[0]).$1}/${(pws[1] as GList).items.length}';
                proposals.add(key);
                if (pl != null) labels[key] = pl;
                fields[key] = names(pws[1] as GList);
                kinds[key] = [
                  for (final e in (pws[1] as GList).items) _kind(e)
                ];
                // The places of its guarding roles, nested, the first outermost.
                for (var cur = actWidget(praw); cur != null; cur = actWidget(cur.$2)) {
                  final a = ActInfo.of(cur.$1);
                  if (a == null) continue;
                  acts.putIfAbsent(key, () => a);
                  (allActs[key] ??= []).add(a);
                }
              }
            }
          }
        }
      }
    }
    return RequestOffers(proposals, known,
        labels: labels,
        fields: fields,
        knownLabel: knownLabel,
        knownField: knownField,
        acts: acts,
        allActs: allActs,
        kinds: kinds);
  }

  /// The kind of a form's field, read off its widget: a peer field, a number field, a text field, the form of a speech act, or any other.
  static String _kind(GTerm entry) {
    final w = _fieldWidget(entry);
    final (k, ws) = ctorArgs(w);
    if (k == 'peer' && ws.isEmpty) return 'peer';
    if (k == 'number' && ws.isEmpty) return 'number';
    if (k == 'text' && ws.isEmpty) return 'text';
    if (k == 'form' && ws.isNotEmpty && ctorArgs(ws[0]).$1 == 'speech') {
      return 'speech';
    }
    return 'other';
  }

  /// The items of a menu as written, each with its label, or the widget itself.
  static List<GTerm> _items(GTerm w) {
    final (k, ws) = ctorArgs(w);
    if (k == 'menu' && ws.length == 1 && ws[0] is GList) {
      return (ws[0] as GList).items;
    }
    return [w];
  }

  /// [w] with a declaration's name, a request's place and a recursive type's binding taken off its top.
  static GTerm _bare(GTerm w) {
    final named = namedWidget(w);
    if (named != null) return _bare(named.$2);
    final act = actWidget(w);
    if (act != null) return _bare(act.$2);
    final labelled = labelledWidget(w);
    if (labelled != null) return _bare(labelled.$2);
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

/// `last_state`: construct 0 of a canonical platform's app, drawn as the last view its agent wrote, its machine state and the persons it knows, "and the person's screen shows the last written" (vGLP b7e3517, Section 5.4, "The compilation"), laid out in the tab it is drawn in (tab_screen.dart; vGLP, Remark "Construct Families"), as GAI's task words it (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 4): in the Friends tab the person's friends and, in GSN, the chat with each, its messages; in the Groups tab the groups with their leader and their messages, and, in a group the person leads, its members, whom the leader's state lists ([SocialState.groups]); each message its text with its author's name under it; in GSNP each item in the chat with the friend it came from, with its author and those who forwarded it ([_SocialTabState._item]); and the notices, each one sentence at the top of the tab it concerns ([SocialNotice]), "a widget that draws the last view written and the notices written after it" (vGLP f0acbc1, Section 5.4, "The compilation"), so that a notice is drawn until the next view is written.  It takes no input.  Drawn outside a screen of tabs, it draws the Friends tab and, where the person is in a group, the Groups tab under it.
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

/// `social_menu`: the request question of a canonical platform of GSC's contracts, `Menu ::= menu(Key, List(Key), Request?)`, drawn with the last view on construct 0, the machine state and the persons known, in the tab it is drawn in, as GAI's task words it (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, items 1 and 2).  In the Friends tab: the person's own name at the top; before their identity is established, the button Join, proposing `establish(Me)`; after it, the persons known, one row each, a friend with the button End friendship, proposing `unfriend(Me, Q)`, and any other with Befriend, proposing `befriend(Me, Q)`; at the bottom the field Add a person by key with Add, answering `known(Q)`, except on a child's screen in CSSN's app, its state holding `child`: "A child's menu offers ... neither meeting nor adding a person by key", the child's agent discovering only its parents' agents, at the setup of the child's phone, before the entering (CSSN, Section 4.6.2; Appendix C.4; CSSN #9 Cowork, Code_inbox.md, 2026-10-20 22:04 UTC, item 2).  Where the request type offers `meet(Me, Q)`, GKK:2 (programs/gsc/GKK.glp, which every contract of GSC's extends), the rows are the persons known and the person's contacts, those the state knows, `knows(Q)`; a person known who is not a contact is drawn with Meet, proposing `meet(Me, Q)`, and Befriend is drawn on a contact's row, befriend requiring that the two know each other (GSG:1); and where it offers `introduce(Me, Q, C)`, GKK:3, the person composes "Introduce Carol to Bob." under the rows ([_SocialTabState._composers]).  Where the request type offers `message(Me, Q, X)`, tapping a friend opens the chat with them, its messages and a text field with Send, proposing `message(Me, Q, x)`, x the speech act of the text typed signed by the person.  Where it offers `create(Me, N)`, the Groups tab: the field New group with Create, proposing `create(Me, n)`; each group the person is in, its name, its leader and its messages, and, in a group they lead, its members; in a group the person leads, Add member, a picker of their friends not in it, proposing `join(Me, Q, n)`, and Close group, proposing `close(Me, n)`; in any group, a text field with Send, proposing `post_own(Me, x, n)` where they lead it and `post(Me, A, x, n)` otherwise; and in a group they do not lead, Leave group, proposing `leave(A, Me, n)`.  Each control is drawn where the request type offers its request, read off `D` ([RequestOffers]), and while the question is open; each grants `propose(P)` or `known(Q)` at the question's position, `[3]`, as the default widget of the request type grants it, so the construct process forms the same answers from it.  No term, functor, type name or Submit button is drawn.  The persons known, and who is a friend, are read off the last view, which the agent writes whenever either changes (vGLP b7e3517), and not off the request question, which shows the persons known when it was asked.  Where construct 0 holds no stream of views, it is drawn as such, named, and takes no input.
///
/// In GSNP (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, items 1 to 3): an item the person holds is drawn in the chat with the friend it came from, its last signer, its text, "Author: Dan" and, where it was forwarded, "via Eve, via Carol"; under one the person did not author, Forward, which opens a picker of their friends and, once one is picked, the warranty question of GSNP:2 with Yes, proposing `forward(Me, Q, x)`, and No; the chat's Send signs the text, proposing `sign(Me, x)`, and holds `message(Me, Q, x)` for the next request question, proposed once the view holds `authored(x)` ([_SocialTabState._chat]).  An item the person authored is drawn in the chat it was sent from, and an item from a person who is not a friend, or authored and sent from no chat on this screen, under Messages at the bottom of the Friends tab.  In GAI's app (programs/apps/gai), whose request type offers `sign(Me, X)`, `message(Me, Q, X)` and `forward(Me, Q, X)` as GSNP's does, an item is drawn as in GSNP, and an author the person holds recorded as an AI agent, `agent_of(r, p, y)`, is named with its principal, "Author: Alf, an AI agent of Alice" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, its last paragraph), with its developer under it, "Developer: Acme", as in its row in Friends; and, where the request type offers `enter(Me, R, X)`, the AI agents tab: the form Create an AI agent, taking the name and the code and proposing `sign(Me, k)` and then `enter(Me, r, k)`, and the person's AI agents, each with Retire, proposing `retire(Me, R)` (GAI #5 Cowork, 2026-10-19 20:36 UTC, (a) and item 5; [_SocialTabState._agents]).  A post carries an item, which the person signs before posting it (GSNP:1; `post_own(a? ; x, n)` requires `item(x)` at `a`, programs/gai/GSNP.glp), so the text typed in a group forms no post: where the request type offers `sign(Me, X)`, no text field and Send are drawn in a group.  The notices are drawn as `last_state` draws them.
///
/// In GC's app (programs/apps/gc), whose request type offers `mint(Me)`, the Coins tab: Mint, the person's holdings, each coin by its issuer's name, "Alice-coin", with Pay, and the swap composed as a sentence, "Swap an Alice-coin for a Bob-coin with Bob." ([_SocialTabState._coinsTab]).
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

  /// The slot of a composed request whose picker is open, `kind:i`; null where none is ([_composer]).
  String? _picking;

  /// The person chosen into each slot of the composed requests, by `kind:i`.
  final Map<String, GTerm> _chosen = {};

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
    if (s.lists != null) return _worded(context, s);
    return switch (widget.place?.tab) {
      ScreenTab.friends => _friends(context, s),
      ScreenTab.groups => _groups(context, s),
      ScreenTab.agents => _agents(context, s),
      ScreenTab.coins => _coinsTab(context, s),
      null => Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            _friends(context, s),
            if (s.inGroup ||
                s.notices.any((n) => n.concernsGroup) ||
                (_ask?.offers.has('create', 2) ?? false))
              ...[const Divider(height: 32), _groups(context, s)],
            if (s.aiAgents.isNotEmpty || _creates(s))
              ...[const Divider(height: 32), _agents(context, s)],
            if (s.coins.isNotEmpty ||
                s.notices.any((n) => n.concernsCoin) ||
                (_ask?.offers.has('mint', 1) ?? false))
              ...[const Divider(height: 32), _coinsTab(context, s, name: false)],
          ],
        ),
    };
  }

  /// The second request of a Send in GSNP's chat: once the item the person signed is in the view as authored, `authored(x)`, the message that carries it, `message(Me, Q, x)`, proposed on the open request question, a later one than the question that took the sign ([TabPlace.hold]).  Likewise the second request of Create in GAI's AI agents tab: once the view holds the code the person signed, `item(k)`, the creation of the AI agent, `enter(Me, r, k)` ([TabPlace.holdEnter]; GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 2: "Create proposes sign(Me, k) and then, once her view holds item(k), enter(Me, r, k)").  Each is proposed after the frame, once, by whichever tab draws the open question first, and one on each request question.
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
      return;
    }
    for (final h in place.heldEnters) {
      if (h.askedOn == _id || !s.holdsItem(h.item)) continue;
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (!mounted || !(place.releaseEnter?.call(h.item) ?? false)) return;
        ask.propose(GStruct('enter', [ask.me, h.agent, h.item]));
      });
      return;
    }
  }

  /// The notices of the tab [t], each one sentence, the latest first ([SocialNotice], [SocialNotice.tab]).
  List<Widget> _notices(SocialState s, ScreenTab t) {
    final me = _me;
    return [
      for (var i = s.notices.length - 1; i >= 0; i--)
        if (s.notices[i].tab == t)
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
    // The persons known, the contacts and the friends, as the last view
    // shows them; the person's own key not among them.
    var persons = <GTerm>[];
    final seen = <String>{if (ask != null) formatTerm(ask.me)};
    for (final q in [...s.known, ...s.contacts, ...s.friends]) {
      if (seen.add(formatTerm(q))) persons.add(q);
    }
    // With no request question open, the contacts and the friends.
    final listed = <GTerm>[];
    final seenListed = <String>{};
    for (final q in [...s.contacts, ...s.friends]) {
      if (seenListed.add(formatTerm(q))) listed.add(q);
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
          for (final q in ask == null ? listed : persons)
            _person(context, s, q,
                onTap: chats && open != null && s.isFriend(q)
                    ? () => open(q)
                    : null),
        if (ask != null && s.identity) ..._composers(context, s, ask),
        if (ask != null &&
            ask.at != null &&
            ask.offers.known) ...[
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

  /// A person's row: their initial and name, under it, for an AI agent the person holds recorded, `agent_of(r, p, y)`, its principal, "an AI agent of Alice" ([SocialState.principalOf]), and under that its developer, "Developer: Acme" ([SocialState.developerOf]; GAI #5 Cowork, Code_inbox.md, 2026-10-19 20:36 UTC, item 5: "in Alf's entry in Bob's Friends tab"; GAI #5 Cowork, Code_inbox.md, 2026-10-20 08:01 UTC: Alf listed with "an AI agent of Alice" and "Developer: Acme Ltd", read from agent_of(alf, alice, y); GAI's paper, Section 3.1: "every friend of an AI agent sees its principal and its developer"), and, for the request question, the button that befriends them or ends the friendship; tapping a friend opens the chat with them where [onTap] is given.  End friendship proposes `unfriend(Me, Q)`, or, where the request type offers `unfriend_p(Me, Q)` in its place, that: in GAI `unfriend` "carries no ? and stands for one schema per role, guarded at that role, unfriend_p and unfriend_q" (programs/gai/GAI.glp), and the person ending the friendship takes the guarded first role, `unfriend_p(p?, q)`, whether or not the friend is an AI agent that is retired (Code_inbox.md, GAI #4 Cowork, 2026-10-19 16:57 UTC, item 4).  The row of the person's own AI agent, `principal_of(Q)`, offers no End friendship: `unfriend_p(p?, q)` forbids `principal_of(q)` at `p` (programs/gai/GAI.glp), and only Retire, in the AI agents tab, ends that friendship ("The friendship of a ward and their guardian is ended only by the guardian, and it then ends their guardianship", Wards:3; "A principal retires their AI agent by ending their friendship with it", GAI:4).  Nor does the row of an AI agent the person's view shows ([SocialState.isKnownAgent]), or an open approval question names ([TabPlace.agentsAsked]), offer Meet or Befriend: `meet(p?, q?)` and `befriend(p?, q?)` forbid `agent` at both parties (programs/gai/GAI.glp at GLP 88d85368), and `agent_meet` names Alf as an AI agent of Alice before Bob's view records it.
  Widget _person(BuildContext context, SocialState s, GTerm q,
      {String? relation, VoidCallback? onTap}) {
    final ask = _ask;
    final k = formatTerm(q);
    final friend = s.isFriend(q);
    final last = _inChat(s, q);
    Widget? button;
    final unfriend = ask == null
        ? null
        : ask.can('unfriend', 2)
            ? 'unfriend'
            : ask.can('unfriend_p', 2)
                ? 'unfriend_p'
                : null;
    // In CSSN `unfriend(p?, q)` forbids `parent(q)` and `parenting(q)` at `p`
    // (programs/cssn/CSSN.glp): a parent and their child end their
    // friendship by other acts.
    if (ask != null &&
        friend &&
        unfriend != null &&
        !s.isPrincipalOf(q) &&
        !s.isParentOf(q) &&
        !s.isChildOf(q)) {
      button = OutlinedButton(
        key: socialKey(_id, 'unfriend:$k'),
        onPressed: () => ask.propose(GStruct(unfriend, [ask.me, q])),
        child: const Text('End friendship'),
      );
    } else if (ask != null &&
        !friend &&
        ask.can('meet', 2) &&
        !s.isContact(q) &&
        !s.isChild &&
        !s.isKnownAgent(q) &&
        !(widget.place?.agentsAsked.contains(k) ?? false)) {
      // GKK:2: a person known by key and not yet met is met in person, an
      // act of both, before anything that requires knowing them.  In CSSN
      // `meet(p?, q?)` forbids `child` at both roles (CSSN:2), so a child's
      // rows offer no Meet.  In GAI `meet(p?, q?)` forbids `agent` at both
      // parties (programs/gai/GAI.glp at GLP 88d85368): an AI agent the view
      // shows, or an open approval question names, is met by `agent_meet`,
      // with its principal.
      button = ElevatedButton(
        key: socialKey(_id, 'meet:$k'),
        onPressed: () => ask.propose(GStruct('meet', [ask.me, q])),
        child: const Text('Meet'),
      );
    } else if (ask != null &&
        !friend &&
        ask.can('befriend', 2) &&
        !s.isChild &&
        // befriend requiring that the two know each other (GSG:1), where
        // the request type offers meeting.
        (!ask.can('meet', 2) || s.isContact(q)) &&
        !s.isKnownAgent(q) &&
        !(widget.place?.agentsAsked.contains(k) ?? false)) {
      button = ElevatedButton(
        key: socialKey(_id, 'befriend:$k'),
        onPressed: () => ask.propose(GStruct('befriend', [ask.me, q])),
        child: const Text('Befriend'),
      );
    }
    final lastText = onTap != null && last.isNotEmpty
        ? Text(SpeechAct.of(last.last)?.text ?? displayText(last.last),
            maxLines: 1, overflow: TextOverflow.ellipsis)
        : null;
    final principal = s.principalOf(q);
    final developer = s.developerOf(q);
    return ListTile(
      key: socialKey(_id, 'person:$k'),
      contentPadding: EdgeInsets.zero,
      leading: CircleAvatar(
        radius: 16,
        backgroundColor: Colors.green.shade100,
        child: Text(personName(q).isEmpty ? '' : personName(q)[0],
            style: const TextStyle(fontSize: 13)),
      ),
      title: Text(
          relation == null ? personName(q) : '${personName(q)}, $relation'),
      subtitle: principal == null && developer == null
          ? lastText
          : Column(
              mainAxisSize: MainAxisSize.min,
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                if (principal != null)
                  Text('an AI agent of ${personName(principal)}'),
                if (developer != null)
                  Text('Developer: ${personName(developer)}'),
                if (lastText != null) lastText,
              ],
            ),
      trailing: button,
      onTap: onTap,
    );
  }

  /// The chat with the friend [q]: in GSN the messages the person received from them, and in GSNP the items drawn in it ([_chatOf]), each with Forward where the person did not author it ([_item]); and, for the request question, a text field with Send.  In GSN Send proposes `message(Me, Q, x)`, x the speech act of the text typed signed by the person.  In GSNP, where the request type offers `sign(Me, X)`, the person signs the content they create (GSNP:1) and a message carries an item they authored (`message(p?, q ; x)` requires `authored(x)` at `p`, programs/gai/GSNP.glp), so Send proposes `sign(Me, x)` on the open request question and holds `message(Me, Q, x)` for a later one, proposed once the view holds `authored(x)` (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, item 2; [TabPlace.hold]).
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

  /// An item of GSNP (GAI #3 Cowork, 2026-10-19 13:31 UTC, item 1): its text, then "Author: Dan", its first signer, then, where it was forwarded, "via Eve, via Carol", each who signed it after them, in order; under an item the person did not author, for the request question, the button Forward; under one they authored, nothing.  In GAI an author the person holds recorded as an AI agent is named with its principal, "Author: Alf, an AI agent of Alice" (the same task, its last paragraph; [SocialState.agentName]), and under that line its developer, "Developer: Acme", read from the attestation the person holds recorded with it ([SocialState.developerOf]; GAI:2: "Every act of the AI agent names its principal and its developer"; GAI #5 Cowork, Code_inbox.md, 2026-10-19 20:36 UTC, item 5).
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
                if (s.developerOf(a.signers.first) case final d?)
                  Text('Developer: ${personName(d)}', style: small),
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

  /// The requests composed, each a sentence the person completes by choosing a person into each of its places: in every app whose contract extends GKK, where the request type offers `introduce(P, Q, C)`, "Introduce Carol to Bob." with the button Introduce, `C` a contact of the person's and `Q` another the agent knows, proposing `introduce(Me, Q, C)` (GKK:3; `introduce(p?, q? ; c)`, programs/gsc/GKK.glp); once sent, "You asked to introduce Carol to Bob." (the words drawn by Code for GSC's confirmation, GSC #5 Cowork, Code_inbox.md, 2026-10-20 17:53 UTC).  CSSN's app is compiled from its contract with pragmas, and its requests are drawn by the worded path in the contract's words.
  List<Widget> _composers(BuildContext context, SocialState s, SocialAsk ask) {
    final me = ask.me;
    bool same(GTerm a, GTerm b) => formatTerm(a) == formatTerm(b);
    final known = <GTerm>[];
    final seen = <String>{formatTerm(me)};
    for (final q in [...s.known, ...s.friends]) {
      if (seen.add(formatTerm(q))) known.add(q);
    }
    String n(GTerm q) => personName(q);
    // GKK:3: the person introduces a contact c to a contact q they can reach,
    // one the agent knows, introduce(Me, q, c) in the order of its head,
    // introduce(p?, q? ; c) (programs/gsc/GKK.glp).
    final contacts = [
      for (final q in s.contacts)
        if (!same(q, me)) q
    ];
    final c0 = _chosen['introduce:0'];
    return [
      if (ask.offers.has('introduce', 3))
        _composer(
          context,
          ask,
          kind: 'introduce',
          composing: contacts.length >= 2,
          slots: [
            contacts,
            [
              for (final q in contacts)
                if (s.known.any((k) => same(k, q)) &&
                    (c0 == null || !same(q, c0)))
                  q
            ],
          ],
          pieces: ['Introduce ', 0, ' to ', 1, '.'],
          button: 'Introduce',
          request: (c) => GStruct('introduce', [me, c[1], c[0]]),
          sent: (c) => 'You asked to introduce ${n(c[0])} to ${n(c[1])}.',
        ),
    ];
  }

  /// What a place of a composed request is drawn as: a group, `group(A, N)`, by its name; a coin, `coin(U)`, by its issuer's name, "Alice-coin" ([coinName]); a person by name.
  static String _placeName(GTerm q) {
    final (f, xs) = ctorArgs(q);
    if (f == 'group' && xs.length == 2) {
      return SpeechAct.of(xs[1])?.text ?? displayText(xs[1]);
    }
    if (f == 'coin' && xs.length == 1) return coinName(xs[0]);
    return personName(q);
  }

  /// What a place of a composed request is drawn as in its sentence: a coin with its indefinite article, "an Alice-coin" ([aCoin]); any other as [_placeName] draws it.
  static String _slotName(GTerm q) {
    final (f, xs) = ctorArgs(q);
    if (f == 'coin' && xs.length == 1) return aCoin(xs[0]);
    return _placeName(q);
  }

  /// One composed request [kind]: what the person sent of it, each one line; and, where [composing], its sentence of [pieces], each a text or the index of a place, a person, or a group by its name ([_placeName]), chosen into it from [slots] by tapping it, which opens a picker of the persons or groups it takes, "…" where none is chosen; and the button [button], Submit unless given, enabled once every place holds a person and the question is open, which proposes [request] of the persons chosen and holds [sent] of them on the screen.  Nothing is drawn where nothing is sent and the person cannot compose it.
  Widget _composer(
    BuildContext context,
    SocialAsk ask, {
    required String kind,
    required bool composing,
    required List<List<GTerm>> slots,
    required List<Object> pieces,
    String button = 'Submit',
    required GTerm Function(List<GTerm>) request,
    required String Function(List<GTerm>)? sent,
  }) {
    final place = widget.place;
    final lines = [
      for (final (k, line) in place?.sent ?? const <(String, String)>[])
        if (k == kind) line
    ];
    if (!composing && lines.isEmpty) return const SizedBox.shrink();
    final theme = Theme.of(context);
    final body = theme.textTheme.bodyLarge;
    // A choice no longer among the persons its place takes is dropped.
    for (var i = 0; i < slots.length; i++) {
      final c = _chosen['$kind:$i'];
      if (c != null &&
          !slots[i].any((q) => formatTerm(q) == formatTerm(c))) {
        _chosen.remove('$kind:$i');
      }
    }
    final chosen = [for (var i = 0; i < slots.length; i++) _chosen['$kind:$i']];
    final ready = ask.at != null && chosen.every((c) => c != null);
    final open = _picking;
    final openAt = open != null && open.startsWith('$kind:')
        ? int.tryParse(open.substring(kind.length + 1))
        : null;
    InlineSpan slot(int i) => WidgetSpan(
          alignment: PlaceholderAlignment.baseline,
          baseline: TextBaseline.alphabetic,
          child: InkWell(
            key: socialKey(_id, 'slot:$kind:$i'),
            onTap: () => setState(
                () => _picking = _picking == '$kind:$i' ? null : '$kind:$i'),
            child: Text(
              chosen[i] == null ? '…' : _slotName(chosen[i]!),
              style: body?.copyWith(
                color: theme.colorScheme.primary,
                decoration: TextDecoration.underline,
                decorationColor: theme.colorScheme.primary,
              ),
            ),
          ),
        );
    return Container(
      key: socialKey(_id, 'compose:$kind'),
      width: double.infinity,
      margin: const EdgeInsets.only(top: 8, bottom: 4),
      padding: const EdgeInsets.fromLTRB(12, 10, 12, 10),
      decoration: BoxDecoration(
        border: Border.all(color: Colors.grey.shade300),
        borderRadius: BorderRadius.circular(10),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          for (var j = 0; j < lines.length; j++)
            Padding(
              padding: EdgeInsets.only(bottom: composing ? 10 : 0),
              child: Text(lines[j],
                  key: socialKey(_id, 'sent:$kind:$j'), style: body),
            ),
          if (composing) ...[
            Text.rich(
              TextSpan(children: [
                for (final piece in pieces)
                  piece is int ? slot(piece) : TextSpan(text: '$piece'),
              ]),
              key: socialKey(_id, 'sentence:$kind'),
              style: body,
            ),
            if (openAt != null && openAt < slots.length)
              Padding(
                padding: const EdgeInsets.only(top: 6),
                child: Wrap(
                  key: socialKey(_id, 'pick:$kind:$openAt'),
                  spacing: 8,
                  runSpacing: 4,
                  children: [
                    for (final q in slots[openAt])
                      ActionChip(
                        key: socialKey(
                            _id, 'pick:$kind:$openAt:${formatTerm(q)}'),
                        label: Text(_placeName(q)),
                        onPressed: () => setState(() {
                          _chosen['$kind:$openAt'] = q;
                          _picking = null;
                        }),
                      ),
                  ],
                ),
              ),
            const SizedBox(height: 10),
            ElevatedButton(
              key: socialKey(_id, 'submit:$kind'),
              onPressed: ready
                  ? () {
                      final c = [for (final x in chosen) x!];
                      if (sent != null) place?.addSent?.call(kind, sent(c));
                      ask.propose(request(c));
                      setState(() {
                        for (var i = 0; i < slots.length; i++) {
                          _chosen.remove('$kind:$i');
                        }
                        _picking = null;
                      });
                    }
                  : null,
              child: Text(button),
            ),
          ],
        ],
      ),
    );
  }

  // --- The apps of contracts with pragmas -----------------------------------

  /// The tab of a platform compiled from a contract with pragmas, every word on it the contract's or one of the platform's four, yes, no, declined and unable (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"; Section 8): the list the tab draws, [TabPlace.list], one of those the view names, its name the tab's; on a screen with no tabs every list, one under another.  A tab holds, in order: the notices of the acts drawn on it; the approvals the person made that are pending, of those acts, each its button and the persons it names; the buttons of the acts that name, beside the person proposing them, no person and no group, drawn on the tab of their contract, each with the fields the person fills, and a meeting's button with a field for the peer's key; and the rows of the list, each an atom of its predicate in the state, its item the first of its arguments that is a person, a group or a speech act, showing the atoms of the state naming the item, and beside it the button of each act that names a person, where the item is a person, or a group, where it is a group ([_wordedRow]).  Where the tab is is the request's widget term's, `act(place(T, ...), W)` ([ActInfo]); no word of a contract is held here.
  Widget _worded(BuildContext context, SocialState s) {
    final lists = s.lists!;
    final pred = widget.place?.list;
    if (pred != null) {
      final l = lists.firstWhere((l) => l.predicate == pred,
          orElse: () => lists.first);
      return _wordedList(context, s, l,
          first: lists.isNotEmpty && lists.first.predicate == l.predicate);
    }
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        for (var i = 0; i < lists.length; i++) ...[
          if (i > 0) const Divider(height: 32),
          _wordedList(context, s, lists[i], first: i == 0),
        ],
      ],
    );
  }

  Widget _wordedList(
      BuildContext context, SocialState s, ({String name, String predicate, bool counted}) l,
      {required bool first}) {
    final ask = _ask;
    final o = ask?.offers;
    final lists = s.lists ?? const [];
    final home = l.name;
    // An act whose widget term names no tab, or one the view lists not, is drawn on the first.
    bool onThis(String f) {
      final t = o?.tabOf(f);
      final known = t != null && lists.any((x) => x.name == t);
      return known ? t == home : first;
    }

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        if (first && ask != null) _name(context, ask.me),
        for (var i = s.notices.length - 1; i >= 0; i--)
          if (onThis(SocialNotice._functor(s.notices[i].part)))
            _wordedNotice(s.notices[i], i, o),
        for (final a in s.pending)
          if (onThis(_pendingFunctor(a))) _wordedPending(a, o),
        if (ask != null && o != null)
          ..._topActs(context, s, ask, o, l.predicate, onThis),
        for (final r in _rows(s, l.predicate))
          _wordedRow(context, s, r,
              counted: l.counted,
              alone: _rows(s, l.predicate)
                      .where((x) =>
                          x.item != null &&
                          r.item != null &&
                          formatTerm(x.item!) == formatTerm(r.item!))
                      .length <=
                  1),
      ],
    );
  }

  /// The acts drawn at the top of the tab of their contract (vGLP feda043, Section 6.2, "The words a platform shows": "an act that names two or more is a form at the top of the tab of its contract, its places picked from the rows; a button that covers several schemas is drawn once; and the button of an act that names none, or a meeting with a peer not listed, is drawn on the tab of its contract"): a meeting with its field for the peer's key; an act that names none, as its form; and the button of each act that names two or more, one for each label, which opens its form, its places picked.  The person proposes at the guarding role whose precondition their state meets, one requiring an atom of the tab's predicate [pred] first, and the first guarding role where none is met.
  List<Widget> _topActs(BuildContext context, SocialState s, SocialAsk ask,
      RequestOffers o, String pred, bool Function(String) onThis) {
    final me = ask.me;
    final out = <Widget>[];
    for (final k in o.proposals) {
      final first = o.acts[k];
      if (first == null) continue;
      final f = k.substring(0, k.lastIndexOf('/'));
      final n = int.parse(k.substring(k.lastIndexOf('/') + 1));
      if (!onThis(f)) continue;
      final all = o.allActs[k] ?? [first];
      // Drawn only where the viewer's state meets the act's own precondition at some guarding role (vGLP 897d1ce, Section 6.2).
      final owns = [for (final a in all) if (a.ownHolds(s.atoms, me)) a];
      if (owns.isEmpty) continue;
      if (first.keys.isNotEmpty && n == 2) {
        out.add(_keyForm(context, s, ask, o, k, first));
        continue;
      }
      // A form only at a guarding role where each of its places has someone to pick (vGLP 22:54 UTC, Section 6.2); an act that names none is not a form.
      final cands = [
        for (final a in owns)
          if (a.namedCount(o.kinds[k] ?? const []) == 0 || _placesPickable(s, a, me)) a
      ];
      if (cands.isEmpty) continue;
      final others = {for (var i = 1; i <= n; i++) i};
      final pick = cands
              .where((a) => a.requiresOf(pred, {...others}..remove(a.me)))
              .firstOrNull ??
          cands.first;
      // An act whose entry no row is, drawn on the tab of its contract; an act that names an entry is drawn in that entry's row, and no form is drawn at the top of a tab (vGLP 516a0a2, Section 6.2: "The button of an act that names no entry, and of a meeting with a peer not listed, is drawn on the tab of its contract").
      final c = pick.namedCount(o.kinds[k] ?? const []);
      final rowable = (s.lists ?? const []).any(
          (l) => pick.requiresOf(l.predicate, {...others}..remove(pick.me)));
      if (c == 0 || !rowable) {
        out.add(_actForm(context, s, ask, o, k, pick, null));
      }
    }
    return out;
  }

  /// The functor of an approval pending, its request name (vGLP e286276, Section 5.4, "The compilation": "each by its request name, a request's own and an answer's that of its attempt's part").
  static String _pendingFunctor(GTerm a) => ctorArgs(a).$1;

  /// A term's words as [_named] draws them, a speech act with its signers, innermost first, by name (vGLP 377aa37, Section 6.2, "The words a platform shows": "a speech act, in a row, a question or a field, is drawn with its signers, innermost first, by the names the viewer's state holds for them").
  static String _signed(GTerm x) {
    final sa = SpeechAct.of(x);
    if (sa == null) return _named(x);
    return '${sa.text} (${[for (final p in sa.signers) personName(p)].join(', ')})';
  }

  /// A term's words: a person by name, a speech act by its text, a group by its name and its leader's name, and any other term as the program writes it.
  static String _named(GTerm x) {
    final sa = SpeechAct.of(x);
    if (sa != null) return sa.text;
    final g = SocialState._group(x);
    if (g != null) {
      return '${SpeechAct.of(g.$2)?.text ?? displayText(g.$2)}, '
          '${personName(g.$1)}';
    }
    return x is GAtom ? personName(x) : displayText(x);
  }

  /// An atom as the program writes it, its persons by name.
  static String _atomText(GTerm a) {
    final (f, xs) = ctorArgs(a);
    if (xs.isEmpty) return f;
    return '$f(${[for (final x in xs) _named(x)].join(', ')})';
  }

  /// A notice, one line: the refuser, the reason, declined or unable, and the act's button; where the reason is unable, the atoms the schema requires at the refuser that its state lacks and those it forbids that its state has (vGLP, Section 6.2, "The words a platform shows": "a notice as the refuser, the reason and the act's button --- "Bob declined Befriend" ... --- with, where the reason is unable, the atoms the schema requires at the refuser that its state lacks and those it forbids that its state has").
  Widget _wordedNotice(SocialNotice n, int i, RequestOffers? o) {
    final f = SocialNotice._functor(n.part);
    final reason = n.declined ? 'declined' : 'unable';
    final atoms = [for (final a in [...n.lacks, ...n.has]) _atomText(a)];
    final text = '${personName(n.by)} $reason ${o?.labelOf(f) ?? f}'
        '${atoms.isEmpty ? '' : ': ${atoms.join(', ')}'}';
    return Container(
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
        Expanded(child: Text(text)),
      ]),
    );
  }

  /// An approval pending, one line: its act's button and the persons and the speech acts it names but the person (vGLP, Section 5.4, "Triggers": "the person sees their pending approvals in the view").
  Widget _wordedPending(GTerm a, RequestOffers? o) {
    final (f, xs) = ctorArgs(a);
    final me = _me;
    final named = [
      for (final x in xs)
        if (x is! GList && !(me != null && formatTerm(x) == formatTerm(me)))
          _signed(x)
    ];
    return Padding(
      key: socialKey(_id, 'pending:${formatTerm(a)}'),
      padding: const EdgeInsets.only(bottom: 6),
      child: Row(children: [
        Icon(Icons.hourglass_empty, size: 16, color: Colors.grey.shade600),
        const SizedBox(width: 6),
        Expanded(
          child: Text([o?.labelOf(f) ?? f, ...named].join(' '),
              style: TextStyle(color: Colors.grey.shade700)),
        ),
      ]),
    );
  }

  /// The rows of the list of [pred]: each atom of [pred] in the state, once, with the count of its copies, in the order of the state; its item the first of its arguments that is a person, a group or a speech act, an item of GSNP's `item(x)` its speech act (vGLP #20, Code_inbox.md, 2026-10-23 23:08 UTC, item 9: "an item's row its author and the agents that forwarded it").
  List<({GTerm atom, int count, GTerm? item})> _rows(SocialState s, String pred) {
    final rows = <({GTerm atom, int count, GTerm? item})>[];
    final at = <String, int>{};
    for (final a in s.atoms) {
      final (f, xs) = ctorArgs(a);
      if (f != pred) continue;
      final k = formatTerm(a);
      final i = at[k];
      if (i != null) {
        rows[i] = (atom: rows[i].atom, count: rows[i].count + 1, item: rows[i].item);
        continue;
      }
      GTerm? item;
      for (final x in xs) {
        if (x is GAtom ||
            SocialState._group(x) != null ||
            SpeechAct.of(x) != null) {
          item = x;
          break;
        }
      }
      at[k] = rows.length;
      rows.add((atom: a, count: 1, item: item));
    }
    return rows;
  }

  /// Whether the atom [a] names the item [item]: a person among its arguments; a group by its name, or by its community name, among them.
  static bool _naming(GTerm a, GTerm item) {
    final g = SocialState._group(item);
    final want = formatTerm(g == null ? item : g.$2);
    final whole = formatTerm(item);
    final (_, xs) = ctorArgs(a);
    return xs.any((x) => formatTerm(x) == want || formatTerm(x) == whole);
  }

  /// A row of a list: its item by name, the count of its copies where its list is counted, the contract adding or deleting its predicate with a multiplicity (vGLP 110caf4, Section 5.4, "The contract's words"), the atoms of the state naming the item, each by its other arguments (vGLP, Section 5.4, "The contract's words": "a row shows the atoms of the state naming its item, the view carrying the whole state"), and the buttons of the acts that name a person or a group beside the item ([_rowActs]).  A speech act is drawn by its text, and under it its signers by name, its author and then each agent that forwarded it, in order (GSC, Section 3: "A speech act is an utterance or a speech act, signed by a person"; vGLP #20, Code_inbox.md, 2026-10-23 23:08 UTC, item 9).
  Widget _wordedRow(BuildContext context, SocialState s,
      ({GTerm atom, int count, GTerm? item}) r,
      {bool alone = true, bool counted = false}) {
    final item = r.item;
    // A row is keyed by its item, or by its atom where another row of the list has the same item, as two bonds of one issuer of different maturities.
    final k = formatTerm(alone ? (item ?? r.atom) : r.atom);
    final title = item == null
        ? _atomText(r.atom)
        : '${_named(item)}${counted ? '  ${r.count}' : ''}';
    final shows = <String>[];
    final signed = item == null ? null : SpeechAct.of(item);
    if (signed != null) {
      shows.add([for (final p in signed.signers) personName(p)].join(', '));
    }
    if (item != null) {
      final whole = formatTerm(item);
      final g = SocialState._group(item);
      final part = g == null ? whole : formatTerm(g.$2);
      // The row's own atom by its other arguments, as a bond's maturity, it being an atom of the state naming the item.
      final own = [
        for (final x in ctorArgs(r.atom).$2)
          if (formatTerm(x) != whole && formatTerm(x) != part) _signed(x)
      ];
      if (own.isNotEmpty) shows.add(own.join(', '));
      for (final a in s.atoms) {
        if (formatTerm(a) == formatTerm(r.atom) || !_naming(a, item)) continue;
        final (_, xs) = ctorArgs(a);
        final rest = [
          for (final x in xs)
            if (formatTerm(x) != whole && formatTerm(x) != part) _signed(x)
        ];
        if (rest.isNotEmpty) shows.add(rest.join(', '));
      }
    }
    final small = TextStyle(fontSize: 13, color: Colors.grey.shade700);
    return Container(
      key: socialKey(_id, 'row:$k'),
      width: double.infinity,
      margin: const EdgeInsets.only(bottom: 8),
      padding: const EdgeInsets.fromLTRB(12, 8, 12, 8),
      decoration: BoxDecoration(
        border: Border.all(color: Colors.grey.shade300),
        borderRadius: BorderRadius.circular(10),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Row(children: [
            CircleAvatar(
              radius: 14,
              backgroundColor: Colors.green.shade100,
              child: Text(title.isEmpty ? '' : title[0],
                  style: const TextStyle(fontSize: 12)),
            ),
            const SizedBox(width: 8),
            Expanded(
              child: Text(title,
                  style: Theme.of(context)
                      .textTheme
                      .titleSmall
                      ?.copyWith(fontWeight: FontWeight.bold)),
            ),
          ]),
          for (final x in shows)
            Padding(
              padding: const EdgeInsets.only(top: 4, left: 36),
              child: Text(x, style: small),
            ),
          if (item != null)
            ..._rowActs(context, s, item, k, ctorArgs(r.atom).$1),
        ],
      ),
    );
  }

  /// The act open in a row's composer, `f/n|item`; null where none is.
  String? _openAct;

  /// The places of the act [a] a row fills, by its item [item] of the list of [pred] (vGLP 932c710, Section 6.2, "The words a platform shows": "The button of an act that names, beside the person proposing it, a person, a group or an item is drawn beside each one the view lists, on whichever tab lists it, the row filling the act's variable"): a group its leader's place and its name's; a speech act the place of an item the act names on that list; a person the act's first place of a person.
  static Map<int, GTerm> _fill(ActInfo a, GTerm? item, String? pred) {
    if (item == null) return const {};
    final g = SocialState._group(item);
    if (g != null) {
      if (a.groups.isEmpty) return const {};
      final (j, k) = a.groups.first;
      return {j: g.$1, k: g.$2};
    }
    for (final (k, p) in a.items) {
      if (p == pred) return {k: item};
    }
    if (SpeechAct.of(item) != null) return const {};
    if (item is GAtom && a.persons.isNotEmpty) return {a.persons.first: item};
    return const {};
  }

  /// The buttons beside a row's item: of each act that names, beside the person proposing it, a person, where the item is a person and not the person themselves, the item filling the first place of a person; a group, where the item is a group whose leader the act names at a place other than the proposer's, or one the person leads; or an item of the row's list, [pred], the row's speech act filling it; and under them the composer of the act open, its fields and pickers.
  List<Widget> _rowActs(
      BuildContext context, SocialState s, GTerm item, String k, String pred) {
    final ask = _ask;
    final o = ask?.offers;
    if (ask == null || o == null || ask.at == null) return const [];
    final me = ask.me;
    final g = SocialState._group(item);
    final mine = formatTerm(item) == formatTerm(me);
    final speech = SpeechAct.of(item) != null;
    final acts = <(String, ActInfo)>[];
    for (final key in o.proposals) {
      final kinds = o.kinds[key] ?? const <String>[];
      // An act whose entry the row is, drawn in the row with its fields beside it, where the act requires or deletes, at the proposer's role, the atom of this tab's predicate naming the row's item, where the viewer's state meets its precondition there, and where each field that takes a person has someone to offer (vGLP 516a0a2, Section 6.2, "The words a platform shows": "A button is drawn in the row of a tab's entry, whose person takes a role of the act, with the act's fields beside it ...; the button is drawn only where each such field has someone to offer").
      for (final a in o.allActs[key] ?? const <ActInfo>[]) {
        if (a.namedCount(kinds) < 1 || mine) continue;
        if (g != null && a.groups.isEmpty) continue;
        if (g == null && speech && a.items.every((x) => x.$2 != pred)) continue;
        if (g == null && !speech && item is! GAtom) continue;
        final fill = _fill(a, item, pred);
        // A row whose entry fills the proposer's own place is the proposer's own, as the group a leader leads (GSN:3, Join in the row of the group).
        if (fill.isEmpty ||
            (fill.containsKey(a.me) && formatTerm(fill[a.me]!) != formatTerm(me))) {
          continue;
        }
        if (!a.requiresOf(pred, fill.keys.toSet())) continue;
        if (!a.holds(s.atoms, {a.me: me, ...fill})) continue;
        if (!_placesPickable(s, a, me, filled: fill)) continue;
        acts.add((key, a));
        break;
      }
    }
    if (acts.isEmpty) return const [];
    // The schemas of one act share its button (GSC's ruling, Code_inbox.md, 2026-10-24 08:06 UTC; programs/vglp/cva/compiler/README.md, "The forms the papers leave open", 2): acts of one label drawn at the same places are one button, its composer the union of their fields ([_sharedForm]).
    final groups = _shared(o, acts);
    final open = [
      for (final m in groups)
        if (_openAct == '${_groupKey(o, m)}|$k') m
    ];
    return [
      const SizedBox(height: 6),
      Wrap(spacing: 8, runSpacing: 4, children: [
        for (final m in groups)
          OutlinedButton(
            key: socialKey(_id, 'act:${_groupName(o, m)}:$k'),
            onPressed: () {
              final r = m.length == 1
                  ? _request(s, o, m.single.$1, m.single.$2, me,
                      _fill(m.single.$2, item, pred), const {})
                  : null;
              if (r != null) {
                ask.propose(r);
              } else {
                final gk = _groupKey(o, m);
                setState(() => _openAct = _openAct == '$gk|$k' ? null : '$gk|$k');
              }
            },
            child: Text(o.label(m.first.$1.substring(0, m.first.$1.lastIndexOf('/')),
                int.parse(m.first.$1.substring(m.first.$1.lastIndexOf('/') + 1)))),
          ),
      ]),
      for (final m in open)
        m.length == 1
            ? _actForm(context, s, ask, o, m.single.$1, m.single.$2, item,
                pred: pred)
            : _sharedForm(context, s, ask, o, m, item, pred: pred),
    ];
  }

  /// The acts [acts] in groups, in order, each the acts of one label drawn at the same places: the schemas of one act, which share its button.
  static List<List<(String, ActInfo)>> _shared(
      RequestOffers o, List<(String, ActInfo)> acts) {
    final groups = <List<(String, ActInfo)>>[];
    final at = <String, int>{};
    for (final (key, a) in acts) {
      final f = key.substring(0, key.lastIndexOf('/'));
      final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
      final label = o.labels[key];
      // A button that covers several schemas is drawn once (vGLP feda043, Section 6.2).
      final g = label == null ? null : '$label|${a.tab}';
      final i = g == null ? null : at[g];
      if (i == null) {
        if (g != null) at[g] = groups.length;
        groups.add([(key, a)]);
      } else {
        groups[i].add((key, a));
      }
      assert(n >= 0 && f.isNotEmpty);
    }
    return groups;
  }

  /// The key of a group of acts in [_openAct]: its one act's, or the shared button's.
  static String _groupKey(RequestOffers o, List<(String, ActInfo)> m) =>
      m.length == 1 ? m.single.$1 : 'shared:${_groupName(o, m)}';

  /// The name of a group of acts in a control's key: its one act's functor, or, for a shared button, the button's text in lower case, each space an underscore, the name its schemas bear before their suffix (README.md, ibid.).
  static String _groupName(RequestOffers o, List<(String, ActInfo)> m) {
    final f = m.first.$1.substring(0, m.first.$1.lastIndexOf('/'));
    if (m.length == 1) return f;
    final l = o.labels[m.first.$1] ?? f;
    return l.toLowerCase().replaceAll(RegExp(r' +'), '_');
  }

  /// The composer of a shared button, the acts [m] of one label: a field for each name among the fields of its acts, each once, in the order of the pragma's fields; on its button, the act whose fields are those the person filled, each of its fields filled and every other left empty (GSC's ruling, ibid.: "an inapplicable field left empty"), proposed as [_actForm] proposes its act; the button disabled where no act is so filled.
  Widget _sharedForm(BuildContext context, SocialState s, SocialAsk ask,
      RequestOffers o, List<(String, ActInfo)> m, GTerm? item, {String? pred}) {
    final name = _groupName(o, m);
    final ik = item == null ? '' : formatTerm(item);
    final composer = '$name:$ik';
    final me = ask.me;
    // Each act's places the person fills, by the name of their field.
    final named = <String, Map<String, int>>{};
    final union = <String>[];
    for (final (key, a) in m) {
      final f = key.substring(0, key.lastIndexOf('/'));
      final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
      final filled = <int>{a.me, ..._fill(a, item, pred).keys};
      final places = <String, int>{};
      // Each name not yet held placed after the one before it in this act, as the pragma's fields are the union of the schemas' variables in head order (README.md, ibid.).
      var after = -1;
      for (var i = 1; i <= n; i++) {
        if (filled.contains(i) || a.persons.contains(i)) continue;
        final fn = o.field(f, n, i);
        places[fn] = i;
        final at = union.indexOf(fn);
        if (at >= 0) {
          after = at;
        } else {
          union.insert(after + 1, fn);
          after++;
        }
      }
      named[key] = places;
    }
    String typedOf(String fn) =>
        _form('field:$composer:${union.indexOf(fn) + 1}').text.trim();
    GTerm? chosen() {
      for (final (key, a) in m) {
        final places = named[key]!;
        final fits = union.every((fn) =>
            places.containsKey(fn) ? typedOf(fn).isNotEmpty : typedOf(fn).isEmpty);
        if (!fits) continue;
        final r = _request(s, o, key, a, me, _fill(a, item, pred),
            {for (final MapEntry(key: fn, value: i) in places.entries) i: typedOf(fn)},
            composer: composer);
        if (r != null) return r;
      }
      return null;
    }

    final r = chosen();
    final first = m.first.$1;
    final f0 = first.substring(0, first.lastIndexOf('/'));
    final n0 = int.parse(first.substring(first.lastIndexOf('/') + 1));
    return Container(
      key: socialKey(_id, 'form:$composer'),
      margin: const EdgeInsets.only(top: 6, bottom: 6),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          for (var j = 1; j <= union.length; j++)
            TextField(
              key: socialKey(_id, 'field:$composer:$j'),
              controller: _form('field:$composer:$j'),
              decoration:
                  InputDecoration(labelText: union[j - 1], isDense: true),
              keyboardType: TextInputType.text,
              onChanged: (_) => setState(() {}),
            ),
          const SizedBox(height: 6),
          ElevatedButton(
            key: socialKey(_id, 'send:$composer'),
            onPressed: r == null
                ? null
                : () {
                    ask.propose(r);
                    setState(() {
                      for (var j = 1; j <= union.length; j++) {
                        _form('field:$composer:$j').clear();
                      }
                      _openAct = null;
                    });
                  },
            child: Text(o.label(f0, n0)),
          ),
        ],
      ),
    );
  }

  /// Whether each person place of the form of [a], but one a field names by key or a held atom fills, has someone to pick (vGLP, Section 6.2, Udi's approval of 2026-10-24 22:54 UTC: "each place of a form is picked from the rows of the tab whose predicate the act requires at the viewer's role naming that place, or, where it requires none, from the persons the viewer knows ...; a form is drawn only where each of its places has someone to pick").
  bool _placesPickable(SocialState s, ActInfo a, GTerm me,
      {Map<int, GTerm> filled = const {}}) {
    final hp = a.heldPlaces.difference({...a.persons, ...a.itemPlaces, ...a.keys});
    final taken = {for (final x in filled.values) formatTerm(x)};
    for (final k in a.persons) {
      if (k == a.me || a.keys.contains(k) || hp.contains(k) || filled.containsKey(k)) continue;
      final preds = [for (final (kk, p) in a.items) if (kk == k) p];
      // Where the place's role requires an atom the schemas prove from the viewer's records, only a person so proved (vGLP 3ec07a2, Section 6.2).
      // Distinct roles are bound to distinct persons: a field offers no person of the act's other places (vGLP 516a0a2, Section 6.2).
      final some = preds.isEmpty
          ? _knownPickable(s, me).any((x) =>
              !taken.contains(formatTerm(x)) && a.provedAt(s.atoms, k, x))
          : preds.any((p) => _pickableItems(s, p, person: true, me: me).any(
              (x) => !taken.contains(formatTerm(x)) && a.provedAt(s.atoms, k, x)));
      if (!some) return false;
    }
    // A group a field takes offers the groups the viewer may pick, and the button is drawn only where it has one to offer (vGLP 516a0a2, Section 6.2: "the button is drawn only where each such field has someone to offer").
    for (final (j, k) in a.groups) {
      if (filled.containsKey(k)) continue;
      if (_pickableGroups(s, me, j, a.me).isEmpty) return false;
    }
    return true;
  }

  /// Whether [p] is picked at a place of the composer [composer] other than [i], distinct roles being bound to distinct persons: "A field that takes a person offers the persons the viewer knows ... other than the viewer and the persons of the act's other places, since distinct roles are bound to distinct persons" (vGLP 516a0a2, Section 6.2).
  bool _pickedElsewhere(String composer, int i, GTerm p, List<int> places) {
    for (final j in places) {
      if (j == i) continue;
      final q = _picked['$composer:$j'];
      if (q != null && formatTerm(q) == formatTerm(p)) return true;
    }
    return false;
  }

  /// The persons the viewer's agent knows, but the viewer, each once: whom a place of a form is picked from where the act requires, at the viewer's role, no atom of a tab's predicate naming it (ibid.).
  List<GTerm> _knownPickable(SocialState s, GTerm me) {
    final seen = <String>{formatTerm(me)};
    return [for (final p in s.known) if (seen.add(formatTerm(p))) p];
  }

  /// The groups the person may pick for the group [j]: those the view lists, `member(G)`, whose leader is the person where the act names its leader at the proposer's place [me], and another where it does not.
  List<GTerm> _pickableGroups(SocialState s, GTerm me, int j, int place) => [
        for (final m in s._args('member', 1))
          if (SocialState._group(m[0]) case (final a, _))
            if ((formatTerm(a) == formatTerm(me)) == (j == place)) m[0]
      ];

  /// The items the person may pick for an item of the list of [pred]: at a place of a person, the persons of its rows, other than the person [me] and the row's item [item] filling the composer, so that a row of that list fills it (vGLP, Section 6.2; compiler README, item 6, `item(K, P)`: "a person of another role ... at place K ... so that a row of that list fills it"); at any other place, the speech acts of its rows; each once.
  List<GTerm> _pickableItems(SocialState s, String pred,
      {bool person = false, GTerm? me, GTerm? item}) {
    final seen = <String>{
      if (person && me != null) formatTerm(me),
      if (person && item != null) formatTerm(item),
    };
    return [
      for (final r in _rows(s, pred))
        if (r.item case final x?
            when (person ? x is GAtom : SpeechAct.of(x) != null) &&
                seen.add(formatTerm(x)))
          x
    ];
  }

  /// The person, the group or the item picked for place [i] of the composer [c].
  final Map<String, GTerm> _picked = {};

  /// The request the act [key] proposes, its proposer the person, the places a row fills, [fill], each other person, group and item picked, and each field read from what the person typed, [typed]; null where a place is not yet filled.
  GTerm? _request(SocialState s, RequestOffers o, String key, ActInfo a,
      GTerm me, Map<int, GTerm> fill, Map<int, String> typed,
      {String composer = ''}) {
    final f = key.substring(0, key.lastIndexOf('/'));
    final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
    final kinds = o.kinds[key] ?? const [];
    final args = List<GTerm?>.filled(n, null);
    args[a.me - 1] = me;
    for (final e in fill.entries) {
      if (e.key >= 1 && e.key <= n) args[e.key - 1] = e.value;
    }
    for (final (j, k) in a.groups) {
      if (args[k - 1] != null) continue;
      final gp = _picked['$composer:g$k'];
      final gg = gp == null ? null : SocialState._group(gp);
      if (gg == null) return null;
      args[j - 1] ??= gg.$1;
      args[k - 1] = gg.$2;
    }
    _derive(s, a, args);
    final its = a.itemPlaces;
    for (var i = 1; i <= n; i++) {
      if (args[i - 1] != null) continue;
      if ((a.persons.contains(i) && !a.keys.contains(i)) || its.contains(i)) {
        args[i - 1] = _picked['$composer:$i'];
        continue;
      }
      final t = typed[i];
      if (t == null || t.isEmpty) return null;
      final kind = a.keys.contains(i)
          ? 'peer'
          : i - 1 < kinds.length
              ? kinds[i - 1]
              : 'other';
      switch (kind) {
        case 'number':
          final v = int.tryParse(t);
          if (v == null) return null;
          args[i - 1] = GInt(v);
        case 'peer':
          if (!_identifier.hasMatch(t)) return null;
          args[i - 1] = GAtom(t);
        case 'speech':
          args[i - 1] = GStruct('speech', [
            GStruct('utterance', [GString(t)]),
            me,
          ]);
        default:
          args[i - 1] = GString(t);
      }
    }
    if (args.contains(null)) return null;
    return GStruct(f, [for (final x in args) x!]);
  }

  /// The places the atoms of [a]'s held patterns fill: for each, the first atom of the state of its predicate agreeing with [args] at the places filled, its arguments filling the rest.
  static void _derive(SocialState s, ActInfo a, List<GTerm?> args) {
    for (final (p, ks) in a.held) {
      if (ks.every((k) => k == 0 || args[k - 1] != null)) continue;
      for (final at in s.atoms) {
        final (f, xs) = ctorArgs(at);
        if (f != p || xs.length != ks.length) continue;
        var ok = true;
        for (var j = 0; j < ks.length && ok; j++) {
          final k = ks[j];
          if (k > 0 && args[k - 1] != null) {
            ok = formatTerm(args[k - 1]!) == formatTerm(xs[j]);
          }
        }
        if (!ok) continue;
        for (var j = 0; j < ks.length; j++) {
          if (ks[j] > 0) args[ks[j] - 1] ??= xs[j];
        }
        break;
      }
    }
  }

  /// The composer of the act [key]: the text of its request form where it has one, a picker for each person, group and item place the row does not fill, a field for each place the person fills, named as its widget term names it, and its button; for an act that names no person, group or item, drawn on its contract's tab.
  Widget _actForm(BuildContext context, SocialState s, SocialAsk ask,
      RequestOffers o, String key, ActInfo a, GTerm? item,
      {String? pred, String? composerName, List<(String, ActInfo)>? alts}) {
    final f = key.substring(0, key.lastIndexOf('/'));
    final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
    final ik = item == null ? '' : formatTerm(item);
    final composer = '${composerName ?? f}:$ik';
    final me = ask.me;
    final fill = _fill(a, item, pred);
    final filled = <int>{a.me, ...fill.keys};
    final picks = [
      for (final i in a.persons)
        if (!filled.contains(i) &&
            !a.keys.contains(i) &&
            // A person place that is an item of a list is picked once, from that list's rows, among the item pickers.
            !a.itemPlaces.contains(i))
          i
    ];
    final groupPicks = [
      for (final (j, k) in a.groups)
        if (!filled.contains(k)) (j, k)
    ];
    final itemPicks = [
      for (final (k, p) in a.items)
        if (!filled.contains(k)) (k, p)
    ];
    final groupPlaces = {for (final (j, k) in a.groups) ...[j, k]};
    // The places the proposer's state fills from a held atom, once the row has filled the rest.
    {
      final args = List<GTerm?>.filled(n, null);
      args[a.me - 1] = me;
      for (final e in fill.entries) {
        args[e.key - 1] = e.value;
      }
      _derive(s, a, args);
      for (var i = 1; i <= n; i++) {
        if (args[i - 1] != null) filled.add(i);
      }
    }
    final fields = [
      for (var i = 1; i <= n; i++)
        if (!filled.contains(i) &&
            (!a.persons.contains(i) || a.keys.contains(i)) &&
            !groupPlaces.contains(i) &&
            !a.itemPlaces.contains(i))
          i
    ];
    Map<int, String> typed() => {
          for (final i in fields) i: _form('field:$composer:$i').text.trim()
        };
    final at = <int, GTerm>{a.me: me, ...fill};
    for (final i in [for (final i in picks) i, for (final (k, _) in itemPicks) k]) {
      if (_picked['$composer:$i'] case final p?) at[i] = p;
    }
    final words = [
      for (final w in a.words)
        if (w is int) (at[w] == null ? '' : personName(at[w]!)) else w as String
    ].join();
    // The atoms naming another role are checked as the person picks the places, as for a row (vGLP 897d1ce, Section 6.2): the request is proposed only where the viewer's state meets the act's precondition at their role under the places filled.
    bool fits(ActInfo a2, GTerm? r) {
      if (r == null) return false;
      final xs = ctorArgs(r).$2;
      return a2.holds(s.atoms, {for (var i = 0; i < xs.length; i++) i + 1: xs[i]});
    }

    GTerm? chosen() {
      if (alts == null) {
        final r = _request(s, o, key, a, me, fill, typed(), composer: composer);
        return fits(a, r) ? r : null;
      }
      // The places picked in the form, each of which the schema proposed must have.
      final pickedAt = {
        for (final i in [
          ...picks,
          for (final (k, _) in itemPicks) k,
          for (final (_, k) in groupPicks) k,
        ])
          if (_picked['$composer:$i'] != null ||
              _picked['$composer:g$i'] != null)
            i
      };
      for (final (k2, a2) in alts) {
        final own = <int>{
          ...a2.persons,
          ...a2.itemPlaces,
          for (final (_, k) in a2.groups) k
        }..remove(a2.me);
        if (!pickedAt.every(own.contains)) continue;
        final r = _request(s, o, k2, a2, me, fill, typed(), composer: composer);
        if (fits(a2, r)) return r;
      }
      return null;
    }

    final r = chosen();
    final small = TextStyle(fontSize: 13, color: Colors.grey.shade700);
    bool picked(String k, GTerm x) =>
        _picked[k] != null && formatTerm(_picked[k]!) == formatTerm(x);
    return Container(
      key: socialKey(_id, 'form:$composer'),
      margin: const EdgeInsets.only(top: 6, bottom: 6),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          if (words.isNotEmpty)
            Padding(
              padding: const EdgeInsets.only(bottom: 4),
              child: Text(words),
            ),
          for (final i in picks)
            Wrap(spacing: 6, runSpacing: 4, children: [
              Text(o.field(f, n, i), style: small),
              for (final p in _knownPickable(s, me))
                if ((item == null || formatTerm(p) != formatTerm(item)) &&
                    !_pickedElsewhere(composer, i, p, picks) &&
                    a.provedAt(s.atoms, i, p))
                  ChoiceChip(
                    key: socialKey(_id, 'pick:$composer:$i:${formatTerm(p)}'),
                    label: Text(personName(p)),
                    selected: picked('$composer:$i', p),
                    onSelected: (_) =>
                        setState(() => _picked['$composer:$i'] = p),
                  ),
            ]),
          for (final (j, k) in groupPicks)
            Wrap(spacing: 6, runSpacing: 4, children: [
              Text(o.field(f, n, k), style: small),
              for (final gt in _pickableGroups(s, me, j, a.me))
                ChoiceChip(
                  key: socialKey(_id, 'pick:$composer:g$k:${formatTerm(gt)}'),
                  label: Text(_named(gt)),
                  selected: picked('$composer:g$k', gt),
                  onSelected: (_) =>
                      setState(() => _picked['$composer:g$k'] = gt),
                ),
            ]),
          for (final (k, p) in itemPicks)
            Wrap(spacing: 6, runSpacing: 4, children: [
              Text(o.field(f, n, k), style: small),
              for (final x in _pickableItems(s, p,
                  person: a.persons.contains(k), me: me, item: item))
                if (!a.persons.contains(k) || a.provedAt(s.atoms, k, x))
                ChoiceChip(
                  key: socialKey(_id, 'pick:$composer:$k:${formatTerm(x)}'),
                  label: Text(_signed(x)),
                  selected: picked('$composer:$k', x),
                  onSelected: (_) =>
                      setState(() => _picked['$composer:$k'] = x),
                ),
            ]),
          for (final i in fields)
            TextField(
              key: socialKey(_id, 'field:$composer:$i'),
              controller: _form('field:$composer:$i'),
              decoration: InputDecoration(
                  labelText: o.field(f, n, i), isDense: true),
              keyboardType: (o.kinds[key] ?? const []).length >= i &&
                      o.kinds[key]![i - 1] == 'number'
                  ? TextInputType.number
                  : TextInputType.text,
              onChanged: (_) => setState(() {}),
            ),
          const SizedBox(height: 6),
          ElevatedButton(
            key: socialKey(_id, 'send:$composer'),
            onPressed: r == null
                ? null
                : () {
                    ask.propose(r);
                    setState(() {
                      for (final i in fields) {
                        _form('field:$composer:$i').clear();
                      }
                      for (final i in picks) {
                        _picked.remove('$composer:$i');
                      }
                      for (final (_, k) in groupPicks) {
                        _picked.remove('$composer:g$k');
                      }
                      for (final (k, _) in itemPicks) {
                        _picked.remove('$composer:$k');
                      }
                      _openAct = null;
                    });
                  },
            child: Text(o.label(f, n)),
          ),
        ],
      ),
    );
  }

  /// A meeting's button with a field for the peer's key, where the peer is not listed (vGLP, Section 6.2: "Meet with a peer not listed takes a field for the key"), named as the widget term names that place.
  Widget _keyForm(BuildContext context, SocialState s, SocialAsk ask,
      RequestOffers o, String key, ActInfo a) {
    final f = key.substring(0, key.lastIndexOf('/'));
    final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
    final kp = a.keys.first;
    return Padding(
      padding: const EdgeInsets.only(bottom: 8),
      child: _entry(
        field: 'key:$f',
        label: o.field(f, n, kp),
        button: 'key-send:$f',
        buttonText: o.label(f, n),
        valid: (t) =>
            _identifier.hasMatch(t) && t != displayText(ask.me),
        onPressed: (t) {
          final args = <GTerm>[
            for (var i = 1; i <= n; i++)
              i == a.me
                  ? ask.me
                  : i == kp
                      ? GAtom(t)
                      : GAtom('')
          ];
          ask.propose(GStruct(f, args));
        },
      ),
    );
  }

  // --- The AI agents tab -----------------------------------------------------

  /// The text of the form's field [k]: the screen's, held across the request questions the agent asks one after another ([TabPlace.field]), or this tab's outside a screen of tabs.
  TextEditingController _form(String k) => widget.place?.field?.call(k) ?? _text(k);

  /// Whether the form Create an AI agent is drawn: the request question is open and offers `sign(Me, X)` and `enter(Me, R, X)`, the person's identity is established, they are no AI agent (`enter(p?, r? ; x)` forbids `agent` at `p`, programs/gai/GAI.glp), and the screen of tabs holds the creation for its second request ([TabPlace.holdEnter]).
  bool _creates(SocialState s) {
    final ask = _ask;
    return ask != null &&
        ask.can('sign', 2) &&
        ask.can('enter', 3) &&
        s.identity &&
        !s.isAgent &&
        widget.place?.holdEnter != null;
  }

  /// The AI agents tab of GAI's app (GAI's paper, Figure "The GAI app", its first screen; Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, (a)): the form Create an AI agent, where it is drawn ([_create]), and the person's AI agents, `principal_of(R)`, each with Retire ([_aiAgent]).
  Widget _agents(BuildContext context, SocialState s) {
    final ask = _ask;
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        if (ask != null && _creates(s)) _create(context, ask, s),
        for (final r in s.aiAgents) _aiAgent(context, s, r),
      ],
    );
  }

  /// The form Create an AI agent, which takes the AI agent's name and its code (GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 2: "The form takes Name and the code"): the field Name, the key of the AI agent, which the person chooses ("A person may act to create a ward ... and establish the ward's identity by choosing the ward's key pair", Wards:1); the code, its text and the key of its developer, the fields of the default widget of Speech, a text field and a peer field, forming the code signed by its developer, `speech(utterance(C), D)`; under them "Code signed by Acme Ltd" and "You attest that Alf runs it." ("A person who creates an AI agent attests that it runs code signed by its developer", GAI:1); and Create.  Create proposes `sign(Me, k)`, `k` the code signed over by the person (`sign(p? ; x)` adds `item(x)` at `p`), and holds `enter(Me, r, k)` for a later request question, proposed once the view holds `item(k)` (`enter(p?, r? ; x)` requires it at `p`, programs/gai/GAI.glp; [_sendHeld]).  Create is enabled once the three are typed, the name is not the person's own key nor one of their AI agents, and no creation of it is held.  What the person typed stays across the request questions.
  Widget _create(BuildContext context, SocialAsk ask, SocialState s) {
    final place = widget.place!;
    final name = _form('agent-name').text.trim();
    final code = _form('agent-code').text.trim();
    final dev = _form('agent-developer').text.trim();
    final named = _identifier.hasMatch(name) && name != formatTerm(ask.me);
    final developed = _identifier.hasMatch(dev);
    final valid = named &&
        developed &&
        code.isNotEmpty &&
        !s.isPrincipalOf(GAtom(name)) &&
        !place.heldEnters.any((h) => formatTerm(h.agent) == name);
    Widget field(String k, String label) => TextField(
          key: socialKey(_id, k),
          controller: _form(k),
          decoration: InputDecoration(labelText: label, isDense: true),
          onChanged: (_) => setState(() {}),
        );
    return Container(
      key: socialKey(_id, 'create-agent-form'),
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
          Text('Create an AI agent',
              style: Theme.of(context)
                  .textTheme
                  .titleMedium
                  ?.copyWith(fontWeight: FontWeight.bold)),
          field('agent-name', 'Name'),
          field('agent-code', 'Code'),
          field('agent-developer', 'Developer'),
          const SizedBox(height: 10),
          Text(
              'Code signed by '
              '${developed ? personName(GAtom(dev)) : 'its developer'}',
              key: socialKey(_id, 'code-signed')),
          const SizedBox(height: 4),
          Text(
              'You attest that '
              '${named ? personName(GAtom(name)) : 'the AI agent'} runs it.',
              key: socialKey(_id, 'attest')),
          const SizedBox(height: 10),
          ElevatedButton(
            key: socialKey(_id, 'create-agent'),
            onPressed: valid
                ? () {
                    final k = GStruct('speech', [
                      GStruct('speech', [
                        GStruct('utterance', [GString(code)]),
                        GAtom(dev),
                      ]),
                      ask.me,
                    ]);
                    place.holdEnter!(k, GAtom(name), _id, s.written.length);
                    ask.propose(GStruct('sign', [ask.me, k]));
                  }
                : null,
            child: const Text('Create'),
          ),
        ],
      ),
    );
  }

  /// One of the person's AI agents: its initial and name, and, for the request question, Retire, proposing `retire(Me, R)` (GAI:4, "A principal retires their AI agent by ending their friendship with it"; `retire(p?, r)`, programs/gai/GAI.glp); once retired, `retired_of(R)`, which `retire` forbids, "Retired" in its place.
  Widget _aiAgent(BuildContext context, SocialState s, GTerm r) {
    final ask = _ask;
    final k = formatTerm(r);
    final Widget? trailing = s.hasRetired(r)
        ? Text('Retired', style: TextStyle(color: Colors.grey.shade600))
        : ask != null && ask.can('retire', 2)
            ? OutlinedButton(
                key: socialKey(_id, 'retire:$k'),
                onPressed: () => ask.propose(GStruct('retire', [ask.me, r])),
                child: const Text('Retire'),
              )
            : null;
    return ListTile(
      key: socialKey(_id, 'agent:$k'),
      contentPadding: EdgeInsets.zero,
      leading: CircleAvatar(
        radius: 16,
        backgroundColor: Colors.green.shade100,
        child: Text(personName(r).isEmpty ? '' : personName(r)[0],
            style: const TextStyle(fontSize: 13)),
      ),
      title: Text(personName(r)),
      trailing: trailing,
    );
  }

  // --- The Coins tab ---------------------------------------------------------

  /// The Coins tab of GC's app (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 18:10 UTC, task 2), its words the compiler's, the names of GC's schemas, with a coin drawn by its issuer's name as GC writes it, "Alice-coin" ([coinName]): the person's own name, where [name]; the notices of the coins' classes; for the request question, Mint, proposing `mint(Me)` (GC:1; `mint(p?)`, programs/currencies/coins/GC.glp); the person's holdings, the coins their state holds, `coin(U)`, one row for each issuer, "Alice-coin", or "2 Alice-coins" where they hold two ([_coin]), and "No coins" where they hold none; the swap, composed as the sentence "Swap an Alice-coin for a Bob-coin with Bob." and the button Swap, `U` the issuer of a coin the person holds, `V` a person they know, or themselves, other than `U`, and `Q` a person they know, proposing `swap(Me, Q, U, V)` (GC:2; `swap(p?, q? ; u, v)`, which at `p` deletes `coin(u)` and adds `coin(v)` and requires `knows(q)`), and once sent "You asked to swap an Alice-coin for a Bob-coin with Bob." ([_composer]); and each payment the person asked for, "You asked to pay a Bob-coin to Bob." ([_coin]).  The sentences once sent are Code's, made by analogy with "You asked to introduce Carol to Bob.".  Redemption, `redeem(Me, Q, R)` (GC:4), has no control here: the screens of Currencies' task do not redeem.
  Widget _coinsTab(BuildContext context, SocialState s, {bool name = true}) {
    final ask = _ask;
    final me = _me;
    bool same(GTerm a, GTerm b) => formatTerm(a) == formatTerm(b);
    // The coins held, by issuer, in the order of the state.
    final issuers = <GTerm>[];
    final counts = <String, int>{};
    for (final u in s.coins) {
      final k = formatTerm(u);
      if (!counts.containsKey(k)) issuers.add(u);
      counts[k] = (counts[k] ?? 0) + 1;
    }
    final contacts = [
      for (final q in s.contacts)
        if (me == null || !same(q, me)) q
    ];
    GTerm coin(GTerm u) => GStruct('coin', [u]);
    GTerm issuer(GTerm c) => ctorArgs(c).$2[0];
    final u0 = _chosen['swap:0'];
    final paid = [
      for (final (k, line) in widget.place?.sent ?? const <(String, String)>[])
        if (k == 'pay') line
    ];
    final body = Theme.of(context).textTheme.bodyLarge;
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        if (name && ask != null) _name(context, ask.me),
        ..._notices(s, ScreenTab.coins),
        if (ask != null && s.identity && ask.can('mint', 1)) ...[
          ElevatedButton(
            key: socialKey(_id, 'mint'),
            onPressed: () => ask.propose(GStruct('mint', [ask.me])),
            child: const Text('Mint'),
          ),
          const SizedBox(height: 4),
        ],
        if (issuers.isEmpty)
          Padding(
            padding: const EdgeInsets.symmetric(vertical: 12),
            child: Text('No coins',
                key: socialKey(_id, 'no-coins'),
                style: body?.copyWith(color: Colors.grey.shade600)),
          )
        else
          for (final u in issuers) _coin(context, s, u, counts[formatTerm(u)]!),
        if (ask != null && s.identity && ask.offers.has('swap', 4))
          _composer(
            context,
            ask,
            kind: 'swap',
            composing: issuers.isNotEmpty && contacts.isNotEmpty,
            slots: [
              [for (final u in issuers) coin(u)],
              [
                for (final v in [...contacts, if (me != null) me])
                  if (u0 == null || !same(coin(v), u0)) coin(v)
              ],
              contacts,
            ],
            pieces: ['Swap ', 0, ' for ', 1, ' with ', 2, '.'],
            button: 'Swap',
            request: (c) =>
                GStruct('swap', [ask.me, c[2], issuer(c[0]), issuer(c[1])]),
            sent: (c) => 'You asked to swap ${aCoin(issuer(c[0]))} for '
                '${aCoin(issuer(c[1]))} with ${personName(c[2])}.',
          ),
        if (paid.isNotEmpty)
          Container(
            key: socialKey(_id, 'paid'),
            width: double.infinity,
            margin: const EdgeInsets.only(top: 8, bottom: 4),
            padding: const EdgeInsets.fromLTRB(12, 10, 12, 10),
            decoration: BoxDecoration(
              border: Border.all(color: Colors.grey.shade300),
              borderRadius: BorderRadius.circular(10),
            ),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                for (var j = 0; j < paid.length; j++)
                  Padding(
                    padding: EdgeInsets.only(top: j == 0 ? 0 : 10),
                    child: Text(paid[j],
                        key: socialKey(_id, 'sent:pay:$j'), style: body),
                  ),
              ],
            ),
          ),
      ],
    );
  }

  /// The person's coins of the issuer [u], [count] of them: the issuer's initial on a coin, and "Alice-coin", or "2 Alice-coins"; and, for the request question, where the issuer is a person the person knows other than themselves, Pay, proposing `pay(Me, U)`, a `U`-coin paid to `U` (GC:3; `pay(p?, q)` requires `knows(q)` and `coin(q)` at `p`, programs/currencies/coins/GC.glp), the screen holding the line "You asked to pay a Bob-coin to Bob." ([TabPlace.addSent]).
  Widget _coin(BuildContext context, SocialState s, GTerm u, int count) {
    final ask = _ask;
    final me = _me;
    final k = formatTerm(u);
    final pays = ask != null &&
        ask.can('pay', 2) &&
        s.isContact(u) &&
        (me == null || formatTerm(me) != k);
    final initial = personName(u).isEmpty ? '' : personName(u)[0];
    return ListTile(
      key: socialKey(_id, 'coin:$k'),
      contentPadding: EdgeInsets.zero,
      leading: CircleAvatar(
        radius: 16,
        backgroundColor: Colors.amber.shade200,
        child: Text(initial, style: const TextStyle(fontSize: 13)),
      ),
      title: Text(count == 1 ? coinName(u) : '$count ${coinName(u)}s'),
      trailing: ask != null && pays
          ? ElevatedButton(
              key: socialKey(_id, 'pay:$k'),
              onPressed: () {
                widget.place?.addSent
                    ?.call('pay', 'You asked to pay ${aCoin(u)} to ${personName(u)}.');
                ask.propose(GStruct('pay', [ask.me, u]));
              },
              child: const Text('Pay'),
            )
          : null,
    );
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

  /// A group the person is in: its name, its leader, its messages, and, where they lead it, the members their state lists in it ([SocialState.groups]); and, for the request question, its controls.  In CSSN's app, a group the person leads invites a child in the words CSSN gave (CSSN #9 Cowork, Code_inbox.md, 2026-10-20 19:44 UTC, item 4), each with Submit: an adult leader "Invite Carol to kids_chat, with Alice.", `S` a friend not listed in it and `P` another person the agent knows, the child's parent, proposing `child_join(Me, S, P, n)` (GnW:2, CSSN:6; `child_join(a?, s?, p? ; n)`, programs/cssn/CSSN.glp), beside Add member; and a child leader, whom `join(a?, q? ; n)` forbids as leader (CSSN:6), in place of Add member, "Invite Dave to kids_chat, with Alice and Bob.", `S` a friend not listed in it, `P` their own parent and `Q` another person the agent knows, the other child's parent, proposing `child_join_2(Me, S, P, Q, n)` (`child_join_2(r?, s?, p?, q? ; n)`).  Once sent, "You invited Carol to kids_chat, with Alice." and "You invited Dave to kids_chat, with Alice and Bob.", Code's, made by analogy with "You entered Carol as your child.", for CSSN's word.
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
    bool same(GTerm a, GTerm b) => formatTerm(a) == formatTerm(b);
    String n(GTerm q) => personName(q);
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
          if (leads &&
              ask.can('join', 3) &&
              candidates.isNotEmpty) ...[
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
