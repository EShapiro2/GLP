/// The named widgets of the grassroots app's construct family: the Dart widgets a widget declaration `T =::= W` may name (vGLP, sections/elicitation.tex, Definition "Widget Declaration": "In the grassroots app, W names a Dart widget: the construct family draws it, and its type fixes the terms it forms").
///
/// A named widget is drawn over its construct's view, as the draw `draw(Id, named(W, D), V)` carries it, and takes the person's input through the grant of the question the view marks `input(P)` (bridge.dart).  The draw carries the name `W`, the default widget `D` of the type, and the view `V`: "It is drawn with the default widget of its type beside its name, from which it reads the type's constructors" (vGLP 92e6d3b, Section 6.2, the paragraph after Proposition "Default Widgets"; Section 6.3, the type `Widget`).  The construct family hands it `D` with the view, and the last value drawn on construct 0: "A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person" (vGLP e924f73, Section 6.2, the paragraph after Proposition "Default Widgets"; construct_family.dart, `_named`).  It grants the raw input the default widget of the type grants, so that the construct process forms the same answers from it (Definition "Construct Process").
///
/// On a platform compiled from a contract with pragmas, every word these widgets draw is the contract's, the program's, or one of the platform's four, yes, no, declined and unable: the view names the lists the app shows, the approval question carries the question the contract puts to the person, and the widget term of the request type the labels of its requests and fields and the declared widget, its buttons ([RequestOffers]) (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"; `_approvalWorded`, `_SocialTabState._worded`).  No word of a contract is held there, and where a control is drawn is read off the declaration and the view, not off a schema's or a predicate's name.  The words held below the worded path are those of the apps drawn before their contracts carried pragmas, until no app is drawn so.
///
/// Three named widgets are here.  [lastState] and [socialMenu], below, draw the tabs the view names, the notices of attempts that close without committing among them.  The first is [approvalSentence], over the approval question of every canonical CVA platform vGLP's compiler prints (programs/vglp/cva/compiler; vGLP, Section 5.4, "The compilation"): `Approval ::= approval(Attempt, YesNo?)`, `Attempt ::= attempt(Peer, Integer, Integer, Class, Names)`, the attempt (i, k, pi, [t]_j, r) naming its request r, `Names ::= none ; request(Rho, Integer)` (vGLP f0acbc1, Definition "Attempt"), `YesNo ::= yes ; no`, each class an act schema applied to its arguments, each argument a `Peer` or a `Speech`, `Speech ::= speech(Utterance, Peer)`, `Utterance ::= utterance(String) ; Speech`.  It reads the class off the view and draws one sentence for it and two buttons labelled for it, in the words of GAI's task for the apps of GSC's contracts (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 3, its sentences as corrected at 11:35 UTC): `befriend(A, Me)`, "Alice asks to be your friend.", Accept and Decline; `join(A, Me, n)`, "Alice asks you to join the group friends.", Join and Decline; and, in GAI's app, `agent_befriend(R, Q, P)`, in the words of GAI's task for its figure (Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, (b) and (c)), at the principal "Alf asks to befriend Bob." and "Approve?", and at the other party "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme", read from the record of Alf that party holds, and "Befriend Alf?", with Yes and No; and `agent_meet(R, Q, P, Y)` likewise, "Alf asks to meet Bob." and "Approve?" at the principal, and "Alf, an AI agent of Alice, asks to meet you.", "Developer: Acme" and "Meet Alf?" at the other party.  And, in GC's app, `swap(A, Me, U, V)`, in the words of Currencies' task for its screens (Currencies #11 Cowork, Code_inbox.md, 2026-10-20 18:10 UTC, task 2, (b)), "Alice asks to swap an Alice-coin for a Bob-coin.", a coin drawn by its issuer's name as GC writes it, "Alice-coin" ([coinName]), with Yes and No, the answers of the platform's `YesNo`: the task takes the compiler's words where they differ from its own, Accept and Decline.  A class the task gives no words for is drawn by a sentence built from its schema and its persons, each speech act of the class under it, and the buttons Yes and No.  A person is shown by name, their key with its underscores as spaces and each word capitalised (the same task, its preamble, which capitalised the first letter; GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 1: "The developer's key is acme_ltd, drawn \"Acme Ltd\"").
library;

import 'package:flutter/material.dart';
import 'package:glp_runtime/vglp/canonical.dart'
    show ActCondition, DeclaredAct, DeclaredButton, DeclaredField, DeclaredWidget;

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
    return _approvalWorded(context, construct, dflt, args, grant, output);
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

/// A line of an atom the proposer holds, as the tab that lists it draws it, with the name of that tab ([_SocialTabState._wordedAtom]): its first argument by name, with its count [k] where the tab is counted, then its other arguments; the viewer's last view [s] names the tabs.
String heldLine(GTerm atom, int k, SocialState? s) {
  final (f, xs) = ctorArgs(atom);
  final tab = s?.lists.where((l) => l.predicate == f).firstOrNull;
  final title = xs.isEmpty
      ? f
      : [
          '${_SocialTabState._named(xs.first)}'
              '${tab?.counted ?? false ? '  $k' : ''}',
          for (final y in xs.skip(1)) _SocialTabState._signed(y),
        ].join(', ');
  return tab == null ? title : '${tab.name}: $title';
}

/// The approval question of a platform compiled from a contract with pragmas, `approval(Attempt, Ws, YesNo?)`: the sentence the contract puts to the person, `Ws`, which the platform writes, each word `text(S)` drawn as it is and `person(P)` by the person's name (vGLP 1126f4f, Section 6.2, "The words a platform shows": "the platform writes it into the approval question, and its widget draws it as it draws any output of the program"); under it the text of each speech act of the attempt's class, the program's output; and the two answers, each drawn by the label the widget term of `YesNo` carries, or by its constant where it carries none ("A construct family draws the label it is given, and the name of the type where it is given none").  Where the contract puts no sentence to the person, `[]`, the class is drawn as the program writes it, its schema and its persons by name.  No word of a contract is held here.
Widget _approvalWorded(BuildContext context, OpenConstruct construct,
    GTerm dflt, List<GTerm> args, void Function(GTerm path, GTerm raw) grant,
    GTerm? output) {
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
        ('named', [_, _]) || ('valued', [_, _]) || ('held', [_, _]) => '',
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
  // After the values, the atoms the act requires of the proposer whose predicates the tabs list, held(A, K), as the tabs draw them (vGLP e22e2ad, Section 5.4, "The contract's words": "as the tabs draw them --- in a swap, the coins the proposer gives"; Section 6.2: "An approval question is drawn as its question, with the values and the proposer's atoms after it"), each beside the name of its tab.
  final (_, seen) = SocialState.read(output);
  final held = [
    for (final w in words)
      if (ctorArgs(w) case ('held', [final a, final k]))
        heldLine(a, k is GInt ? k.value : 1, seen)
  ];
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
        for (final v in [...values, ...held])
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

/// The name a widget declaration gives [socialMenu]: `Request =::= social_menu(...)`, which vGLP's compiler prints into every canonical platform for its request type (programs/vglp/cva/compiler/text.glp at GLP 7d857bcd), with the labels of a contract's pragmas where it carries them (programs/vglp/cva/compiler/README.md).
const String socialMenuName = 'social_menu';

/// The key of the control or the atom [what] that [lastState] or [socialMenu] draws for construct [id].
ValueKey<String> socialKey(int id, String what) =>
    ValueKey('$id:social:$what');

/// The view of the agent of a canonical platform, as it writes it on its output to its person: "On the program's output to its person (Section 5.3) the agent goal writes its view view(k, s, K, A), its own key k, the machine state s of its app state, the persons K it knows and the approvals A its person made that are pending, each by its request name, a request's own and an answer's that of its attempt's part, when it starts and whenever any changes; and, when an approval its person made, by a request or by an answer yes, ends by a refusal, a notice refused(N, q, d)" (vGLP 12f629d, Section 5.4, "The compilation"; [SocialNotice]); and the compiler carries "the tab's name and its predicate into the view the agent writes" (vGLP 12f629d, Section 5.4, "The contract's words"), as the view's last argument, so that the view is `view(Me, S, K, A, Ts)`, `Output ::= view(Key, List(Atom), List(Key), List(Approved), List(Tab))` (programs/vglp/cva/compiler/text.glp at GLP 7d857bcd).  A machine state is a list of atoms, each as its contract writes it, in the view as the platform prints its type `Atom` (programs/apps/<app>/platform.vglp): `identity` and `friend(Q)`; in GSN also `received(X, P)`, `member(C)`, `listed(Q, N)`, `closed(N)`, `posted(X, N)`, `delivered(X, Q, N)` and `received_in(X, A, N)`; in GSNP `item(X)` and `authored(X)` in place of `received` and `received_in`; each `X` a speech act and each `N`, a group's name, a speech act.  `C` is the community name of a group, `<A>.N` in the contract, of type `CName ::= community(Peer) ; dot(CName, Speech)`: "a name term <u>.y1...yn is the community name of the person bound to u followed by the speech acts bound to y1, ..., yn, as <a>.n names the group n led by a" (vGLP, Section 5.4, "Speech-act schemas as local specifications"), so a member atom holds `dot(community(A), N)`, the group `N` led by `A`.
class SocialState {
  /// The agent's own key, the view's first argument, `k` (vGLP 12f629d, Section 5.4, "The compilation": "its own key k").
  final GTerm me;

  /// The machine state, its atoms.
  final List<GTerm> atoms;

  /// The persons the agent knows.
  final List<GTerm> known;

  /// The notices written after the last view, in the order written: those the widget draws.
  final List<SocialNotice> notices;

  /// The lists the app shows, the view's last argument, `Ts`: each its name and the predicate whose atoms in the machine state it lists, `tab(Name, P)`, or `tab(Name, P, counted)` (vGLP 12f629d, Section 5.4, "The contract's words": "the tab's name and its predicate into the view the agent writes").
  final List<({String name, String predicate, bool counted})> lists;

  /// The approvals the person made that are pending, the view's `A`: each by its request name (vGLP 12f629d, Section 5.4, "The compilation": "the approvals A its person made that are pending, each by its request name, a request's own and an answer's that of its attempt's part").
  final List<GTerm> pending;

  const SocialState(this.me, this.atoms, this.known, this.lists,
      [this.notices = const [], this.pending = const []]);

  /// The last view written on construct 0, and the notices written after it, read off [output], the value drawn on it: the elements of the stream so far, each written whole a view, `view(Me, S, K, A, Ts)`, or a notice ([SocialNotice]).  The first of the pair is false where [output] is not a stream of views and notices; the second is null where no view is written whole yet.
  static (bool, SocialState?) read(GTerm? output) {
    if (output is! GList) return (false, null);
    GTerm? me;
    List<GTerm>? atoms, known;
    var pending = <GTerm>[];
    List<({String name, String predicate, bool counted})>? lists;
    var notices = <SocialNotice>[];
    for (final e in output.items) {
      if (!_whole(e)) continue;
      final (f, xs) = ctorArgs(e);
      if (f == 'view' &&
          xs.length == 5 &&
          xs[1] is GList &&
          xs[2] is GList &&
          xs[3] is GList &&
          xs[4] is GList) {
        // view(Me, S, K, A, Ts): the agent's own key, the machine state, the persons known, the approvals pending and the tabs (vGLP 12f629d, Section 5.4, "The compilation" and "The contract's words").
        me = xs[0];
        atoms = (xs[1] as GList).items;
        known = (xs[2] as GList).items;
        pending = (xs[3] as GList).items;
        lists = [
          for (final t in (xs[4] as GList).items)
            if (ctorArgs(t) case (final tf, final ts)
                when tf == 'tab' && (ts.length == 2 || ts.length == 3))
              (
                name: displayText(ts[0]),
                predicate: displayText(ts[1]),
                // tab(Name, P, counted): the contract adds or deletes P with a multiplicity, and each atom shows its count (vGLP, Section 5.4, "The contract's words").
                counted: ts.length == 3 && displayText(ts[2]) == 'counted'
              ),
        ];
        // A notice is drawn until the next view is written: "which the
        // screen shows until the next view" (vGLP f0acbc1, Section 5.4, "The
        // compilation", the short version's).
        notices = <SocialNotice>[];
        continue;
      }
      final n = SocialNotice.of(e);
      if (n == null) return (false, null);
      notices.add(n);
    }
    if (me == null || atoms == null || known == null || lists == null) {
      return (true, null);
    }
    return (true,
        SocialState(me, atoms, known, lists, notices, pending));
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

  /// The principal of the AI agent [r] that the person holds recorded, `agent_of(r, p, y)`, with its attestation `y`, which `agent_meet` and `agents_meet` add at the agent the AI agent comes to know (GnW:2, GAI:2; programs/gai/GAI.glp at GLP 88d85368; GAI's paper, Table "The local state of an agent p": "triples (r,q,y) of an AI agent r, its principal q and its attestation y that p holds recorded"); null where the person holds no such record of [r].
  GTerm? principalOf(GTerm r) => [
        for (final x in _args('agent_of', 3))
          if (_same(x[0], r)) x[1]
      ].firstOrNull;

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

  /// The person's AI agents, `principal_of(R)`, which `enter` adds at the principal (Wards:1, GAI:2), in the order of the state.
  List<GTerm> get aiAgents => [for (final x in _args('principal_of', 1)) x[0]];

  /// Whether the person is the principal of [r], `principal_of(r)`.
  bool isPrincipalOf(GTerm r) => aiAgents.any((a) => _same(a, r));

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
}

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

/// An act of a button as the request widget reads it, the declaration's `act(F, M, Roles, Pre, Binds, Fields, Same, Text)` (programs/vglp/cva/compiler/README.md, 6): "It draws one button for each way the viewer's state, in the last view written, satisfies the act at the proposer's role" (vGLP, Section 6.2, "The words a platform shows").  A place of the request is the `I`-th argument of `F(...)`; a value of a place is held by its number.
class ActInfo {
  final DeclaredAct act;
  const ActInfo(this.act);

  String get schema => act.schema;

  /// The place of the proposer, the viewer.
  int get me => act.proposer;

  List<DeclaredField> get fields => act.fields;

  /// The text of its request form, each word a string or the place of a person: "A question line naming the proposer is text of the request form, shown with the button" (vGLP, Section 5.4, "The contract's words").
  List<Object> get words => act.text;

  Iterable<ActCondition> _of(String kind) =>
      act.pre.where((c) => c.kind == kind);

  /// Whether the places [i] and [j] may hold one person, `same(K, J)` (GSC 316d875, Section 2; vGLP, Section 6.2: "but for two the clause says are possibly one person, which one person may fill").
  bool mayShare(int i, int j) =>
      [...act.pre, ...act.same].any((c) =>
          c.kind == 'same' &&
          ((c.args[0] == i && c.args[1] == j) ||
              (c.args[0] == j && c.args[1] == i)));

  /// Whether [at] binds distinct roles to distinct persons, but for two the clause says are possibly one person: "every binding of the variables of the atoms the schema requires there that a schema-binding makes, distinct roles bound to distinct persons" (vGLP e22e2ad, Section 5.4, "The contract's words").  An agent variable is bound by no such rule, so Introduce with Carol = Bob is a way, and Remove of herself is none (vGLP #23, Code_inbox.md, 2026-11-06 12:00 UTC, 1).
  bool distinct(Map<int, GTerm> at) {
    final rs = [
      for (final r in act.roles)
        if (at.containsKey(r)) r
    ];
    for (var i = 0; i < rs.length; i++) {
      for (var j = i + 1; j < rs.length; j++) {
        if (!mayShare(rs[i], rs[j]) &&
            formatTerm(at[rs[i]]!) == formatTerm(at[rs[j]]!)) {
          return false;
        }
      }
    }
    return true;
  }

  /// [at] with the places of the entry [c] the atom [a] fills, an entry's place holding one value; null where [a] does not match [c] under [at].  `any` matches every argument.
  static Map<int, GTerm>? matched(GTerm a, ActCondition c, Map<int, GTerm> at) {
    final (f, xs) = ctorArgs(a);
    if (f != c.pred || xs.length != c.args.length) return null;
    final out = {...at};
    bool put(int k, GTerm v) {
      final held = out[k];
      if (held == null) out[k] = v;
      return held == null || formatTerm(held) == formatTerm(v);
    }

    for (var j = 0; j < xs.length; j++) {
      final x = c.args[j];
      if (x is int) {
        if (!put(x, xs[j])) return null;
      } else if (x is (int, int)) {
        final g = SocialState._group(xs[j]);
        if (g == null || !put(x.$1, g.$1) || !put(x.$2, g.$2)) return null;
      }
    }
    return out;
  }

  /// Whether [at] determines the atom [c]: it gives every place of it, and no argument is `any`.
  static bool _determined(ActCondition c, Map<int, GTerm> at) =>
      c.args.every((x) => x is int
          ? at.containsKey(x)
          : x is (int, int) && at.containsKey(x.$1) && at.containsKey(x.$2));

  /// Whether the state [atoms] lacks what [at] forbids: each atom the schema forbids at the proposer's role that [at] determines ("lacks the atoms the schema forbids there that the binding determines", vGLP, Section 5.4, "The contract's words"), and each `unless(P, As)` whose person [at] gives ("an act that forbids the second at x's role is not offered with x", Section 6.2).
  bool lacks(List<GTerm> atoms, Map<int, GTerm> at) {
    for (final c in _of('forb')) {
      if (_determined(c, at) && atoms.any((a) => matched(a, c, at) != null)) {
        return false;
      }
    }
    for (final u in _of('unless')) {
      final named = u.args.whereType<int>();
      if (named.isNotEmpty &&
          named.every(at.containsKey) &&
          atoms.any((a) => matched(a, u, at) != null)) {
        return false;
      }
    }
    return true;
  }

  /// The values the way [at] binds, which identify it: "ways that differ only in a variable local to the role being one" (vGLP, Section 5.4, "The contract's words").
  String valuesOf(Map<int, GTerm> at) =>
      [for (final k in act.binds) formatTerm(at[k]!)].join(',');

  /// Every way the state [atoms] of the viewer [viewer] satisfies the act at the proposer's role, each a binding of the places of the atoms it requires, the proposer's place the viewer's: "every binding of the variables of the atoms the schema requires there that a schema-binding makes, distinct roles bound to distinct persons, under which the state holds those atoms and lacks the atoms the schema forbids there that the binding determines" (vGLP e22e2ad, Section 5.4, "The contract's words").  Ways that differ only in a variable local to the role are one, by [valuesOf].
  List<Map<int, GTerm>> ways(List<GTerm> atoms, GTerm viewer) {
    final reqs = _of('req').toList();
    final out = <String, Map<int, GTerm>>{};
    void from(int i, Map<int, GTerm> at) {
      if (i == reqs.length) {
        if (lacks(atoms, at) && distinct(at)) {
          out.putIfAbsent(valuesOf(at), () => at);
        }
        return;
      }
      for (final a in atoms) {
        final next = matched(a, reqs[i], at);
        if (next != null) from(i + 1, next);
      }
    }

    from(0, {me: viewer});
    return out.values.toList();
  }

  /// The places an entry names.
  static Set<int> placesOf(ActCondition c) => {
        for (final x in c.args)
          if (x is int) x else if (x is (int, int)) ...[x.$1, x.$2]
      };

  /// The atoms that bind the ways of the act, as the family draws them: of each predicate the first atom it requires that binds a place other than the proposer's.  A way is drawn beside the atom it binds there, so Introduce, `knows(q)` and `knows(c)`, beside each person known, with a picker of the others (vGLP, Section 6.2: "How the buttons of one act are arranged, each beside the atoms that bind it or as one button with a picker, is the construct family's"; vGLP #23, Code_inbox.md, 2026-11-05 13:33 UTC, 1).
  List<ActCondition> get entries {
    final seen = <String>{};
    return [
      for (final c in _of('req'))
        if ((placesOf(c)..remove(me)).isNotEmpty && seen.add(c.pred)) c
    ];
  }

  /// Whether the viewer's state [atoms] proves of [x], at the place [k], each atom the act requires at that place's role that the schemas prove from the viewer's records, `proved(Q, Ws, P, As)`: for each, the viewer holds an atom matching one of its entries with [x] at [k] (vGLP, Section 6.2: "only a person so proved").
  bool provedAt(List<GTerm> atoms, int k, GTerm x) {
    final need = <String, List<ActCondition>>{};
    for (final e in _of('proved')) {
      if (e.args.contains(k)) {
        (need['${e.proves}(${e.provesArgs.join(',')})'] ??= []).add(e);
      }
    }
    return need.values.every((es) => es
        .any((e) => atoms.any((a) => matched(a, e, {k: x}) != null)));
  }

  /// The persons the person field at the place [k] offers under [at]: "the persons the viewer knows, since the proposer's machine must know every participant, other than the viewer and the persons in the act's other roles, since distinct roles are bound to distinct persons, but for two the clause says are possibly one person, which one person may fill, and, where that person's role requires an atom that the schemas prove of a person from the viewer's records, as above, only a person so proved" (vGLP, Section 6.2), and none the act is not offered with ([lacks]).
  List<GTerm> offered(SocialState s, GTerm viewer, int k, Map<int, GTerm> at) {
    final seen = <String>{};
    return [
      for (final x in [...s.known, if (mayShare(k, me)) viewer])
        if (seen.add(formatTerm(x)) &&
            distinct({...at, k: x}) &&
            provedAt(s.atoms, k, x) &&
            lacks(s.atoms, {...at, k: x}))
          x
    ];
  }

  /// Whether each person field has someone to offer under [at]: "the button is drawn only where each such field has someone to offer" (ibid.).
  bool offers(SocialState s, GTerm viewer, Map<int, GTerm> at) => fields.every(
      (f) => f.type != 'person' || offered(s, viewer, f.place, at).isNotEmpty);
}

/// The requests a request question offers, read off the default widget `D` of its type, the request type `Request` (vGLP 12f629d, Section 5.4, "The compilation"): "It is drawn with the default widget of its type beside its name, from which it reads the type's constructors, so that a menu declared once lists the alternatives of the request type of each program it is declared for" (vGLP, Section 6.2, the paragraph after Proposition "Default Widgets").  The request type has the alternative `propose(P)`, `P` a term of Proposal naming a guarded part of a class (vGLP 40f8444, Section 5.4, "The compilation": "of a type with the alternative propose(N) for the request name N of each guarded part").
class RequestOffers {
  /// The alternatives of Proposal, each its functor and arity, `befriend/2`.
  final Set<String> proposals;

  /// The label the widget declaration gives each alternative of Proposal, by its functor and arity, where it gives one: the button of the contract's pragma, "Befriend" (vGLP 1126f4f, Section 6.2, "The words a platform shows": "a widget term carries a label beside each constructor"; programs/vglp/cva/compiler/README.md).
  final Map<String, String> labels;

  /// The names of the fields of each alternative of Proposal, by its functor and arity, in order: the name the declaration gives the field, the contract's in angle brackets, or the name of its declared type ("A construct family draws the label it is given, and the name of the type where it is given none").
  final Map<String, List<String>> fields;

  /// The buttons the declaration gives, each its label and its acts, in the order of the pragmas: "for its request type, a menu of the buttons and the fields of the contract's acts" (vGLP, Section 6.2, "The words a platform shows").
  final List<DeclaredButton> buttons;

  /// The kind of the field of each place of each alternative of Proposal, by its functor and arity, in order, read off its widget: `peer`, `number`, `text`, `speech` or `other`.
  final Map<String, List<String>> kinds;

  const RequestOffers(this.proposals,
      {this.labels = const {},
      this.fields = const {},
      this.buttons = const [],
      this.kinds = const {}});

  /// The alternative of Proposal of functor [f], its key `f/n`; null where there is none.
  String? keyOf(String f) {
    for (final k in proposals) {
      if (k.substring(0, k.lastIndexOf('/')) == f) return k;
    }
    return null;
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

  /// The requests [dflt], the default widget of the request type `Request`, offers: the request type's alternatives, the form `propose` of one field, the menu of the forms of Proposal, and the form `known` where the type has it, a menu of them or, of one alternative, its form (vGLP 12f629d, Section 5.4, "The compilation": the request question "of the request type Request?, takes one request, of a type with the alternative propose(N) for the request name N of each guarded part"; Definition "Default Widget"); each with the label its declaration gives it, `label(S, form(F, ...))`, and its fields' names; and the buttons of the declaration, which [w], the widget of the question, `named(W, D)`, carries ([declaredText]).
  static RequestOffers of(GTerm w) {
    final dflt = namedWidget(w)?.$2 ?? w;
    final text = declaredText(w);
    final buttons =
        text == null ? const <DeclaredButton>[] : DeclaredWidget.parse(text).buttons;
    final proposals = <String>{};
    final labels = <String, String>{};
    final fields = <String, List<String>>{};
    final kinds = <String, List<String>>{};
    List<String> names(GList es) => [
          for (final e in es.items)
            if (ctorArgs(e).$2.isNotEmpty) displayText(ctorArgs(e).$2[0])
        ];
    for (final raw in _items(_bare(dflt))) {
      final (ak, aws) = ctorArgs(_bare(raw));
      if (ak != 'form' || aws.length != 2 || aws[1] is! GList) continue;
      final f = ctorArgs(aws[0]).$1;
      final afs = aws[1] as GList;
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
          }
        }
      }
    }
    return RequestOffers(proposals,
        labels: labels,
        fields: fields,
        buttons: buttons,
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

  /// [w] with a declaration's name, a label and a recursive type's binding taken off its top.
  static GTerm _bare(GTerm w) {
    final named = namedWidget(w);
    if (named != null) return _bare(named.$2);
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

/// What the request question asks, as [socialMenu] reads it: the agent's own key, read off the last view on construct 0 ([SocialState.me]), the path of the question, `P` of its view `input(P)`, null once it is answered, the requests its type offers, and the grant of the person's raw input at the question.  The request question is bare, of the request type `Request?`, taking one request (vGLP 12f629d, Section 5.4, "The compilation"), so it shows neither the key nor the persons known: both are read off the last view, "A declared widget is drawn also with the last value drawn on construct 0" (vGLP 12f629d, Section 6.2).
class SocialAsk {
  final GTerm me;
  final GList? at;
  final RequestOffers offers;
  final void Function(GTerm path, GTerm raw) grant;

  /// The person's refresh, a grant addressed to no construct ([PersonRefresh]); null where the screen gives none.
  final VoidCallback? onRefresh;
  const SocialAsk(this.me, this.at, this.offers, this.grant, [this.onRefresh]);

  /// Whether the question is open and its type offers [f] of arity [n].
  bool can(String f, int n) => at != null && offers.has(f, n);

  /// Answer the question with the request `propose(P)`, as the default widget of the request type grants it: the form `propose` holding the form of `P`.
  void propose(GTerm p) => grant(at!, GStruct('propose', [p]));

  /// Grant `refresh` on the person channel, addressed to no construct: the request type has no alternative refresh, and on the grant the dispatcher has every construct draw again what it last drew (vGLP d433f71, Sections 5.4 and 6.3).
  void refresh() => onRefresh?.call();

  /// A speech act of the text [text], signed by the person: "speech(utterance(S), P) is the text S by P" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 4), as the default widget of Speech forms it from its fields, a text field and a peer field.
  GTerm say(String text) => GStruct('speech', [
        GStruct('utterance', [GString(text)]),
        me,
      ]);
}

/// `last_state`: construct 0 of a canonical platform's app, drawn as the last view its agent wrote, "and the person's screen shows the last written" (vGLP b7e3517, Section 5.4, "The compilation"), in the tab it is drawn in, the tabs being those the view names (vGLP, Section 5.4, "The contract's words": "the tab's name and its predicate into the view the agent writes"; tab_screen.dart), with the notices, "a widget that draws the last view written and the notices written after it" (vGLP f0acbc1, Section 5.4, "The compilation"), so that a notice is drawn until the next view is written ([SocialNotice], [_SocialTabState._worded]).  It takes no input.  Drawn outside a screen of tabs, it draws every list the view names, one under another.
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
    key: socialKey(construct.id, 'tab:${place?.list ?? 'all'}'),
    id: construct.id,
    state: state,
    place: place,
  );
}

/// `social_menu`: the request question of a canonical platform, of the request type `Request?`, `(Request?)*request(...)` (vGLP 12f629d, Section 5.4, "The compilation"; programs/vglp/cva/compiler/text.glp at GLP 7d857bcd), drawn with the last view on construct 0, the agent's own key, the machine state, the persons known and the tabs (Section 6.2: "A declared widget is drawn also with the last value drawn on construct 0"), in the tab it is drawn in, as `last_state` draws it, with a button for each way the viewer's state satisfies an act of its declaration, and the act's fields ([ActInfo], [RequestOffers]; Section 6.2, "The words a platform shows").  Each control is drawn while the question is open, and grants `propose(P)` at the question's position, as the default widget of the request type grants it, so the construct process forms the same answers from it.  Where construct 0 holds no stream of views, it is drawn as such, named, and takes no input; where it holds no view yet, the question is drawn waiting and takes no input, its key not yet written.
Widget socialMenu(
    BuildContext context,
    OpenConstruct construct,
    GTerm dflt,
    GTerm view,
    List<int> at,
    void Function(GTerm path, GTerm raw) grant,
    GTerm? output) {
  final (ok, state) = SocialState.read(output);
  if (!ok) {
    return Text(
        '$socialMenuName cannot draw ${formatTerm(view)} without a view on '
        'construct 0',
        style: TextStyle(color: Colors.red.shade700));
  }
  final place = TabPlace.maybeOf(context);
  return SocialTab(
    key: socialKey(construct.id, 'tab:${place?.list ?? 'all'}'),
    id: construct.id,
    state: state,
    place: place,
    ask: state == null
        ? null
        : SocialAsk(
            state.me, questionAt(view), RequestOffers.of(construct.widget), grant,
            PersonRefresh.maybeOf(context)?.refresh),
  );
}

/// A tab the view names, drawn from the last view written, with the controls of the request question where [ask] is given ([socialMenu]) and without them where it is not ([lastState]).
class SocialTab extends StatefulWidget {
  /// The construct drawn: its identifier, in the keys of its controls.
  final int id;

  /// The last view written; null where none is written whole yet.
  final SocialState? state;

  /// The tab drawn, from the screen of tabs; null outside one.
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

  int get _id => widget.id;
  SocialAsk? get _ask => widget.ask;

  /// The person's own key, as the request question shows it, or as the screen of tabs holds it from the last one drawn; null where neither shows it.
  GTerm? get _me => _ask?.me ?? widget.place?.me;

  /// An agent identifier, as a peer field takes it: letters, digits and `_` (construct_family.dart, `_identifier`).
  static final RegExp _identifier = RegExp(r'^[A-Za-z0-9_]+$');

  @override
  Widget build(BuildContext context) {
    final s = widget.state;
    if (s == null) {
      return Column(crossAxisAlignment: CrossAxisAlignment.start, children: [
        Text('…', style: TextStyle(color: Colors.grey.shade600)),
      ]);
    }
    return _worded(context, s);
  }

  /// The person's name, and the refresh, "a gesture or an icon, as the construct family chooses, with no word" (vGLP d433f71, Section 6.2): an icon, where the screen gives the person's refresh ([PersonRefresh]), granting refresh whether or not the request question is open.
  Widget _name(BuildContext context, GTerm me, {VoidCallback? refresh}) =>
      Padding(
        padding: const EdgeInsets.only(bottom: 8),
        child: Row(children: [
          Expanded(
            child: Text(personName(me),
                key: socialKey(_id, 'name'),
                style: Theme.of(context)
                    .textTheme
                    .titleLarge
                    ?.copyWith(fontWeight: FontWeight.bold)),
          ),
          if (refresh != null)
            IconButton(
              key: socialKey(_id, 'refresh'),
              icon: const Icon(Icons.refresh),
              onPressed: refresh,
            ),
        ]),
      );

  // --- The apps of contracts with pragmas -----------------------------------

  /// The tab of a platform compiled from a contract with pragmas, every word on it the contract's or one of the platform's four, yes, no, declined and unable (vGLP, Section 5.4, "The contract's words"; Section 6.2, "The words a platform shows"; Section 8): the list the tab draws, [TabPlace.list], one of those the view names, its name the tab's; on a screen with no tabs every list, one under another.  A tab holds the atoms of its predicate, each with its count, and beside each the buttons of the ways it binds; the first holds above them the notices, the approvals the person made that are pending, and the buttons of the ways no atom of a list binds.  No word of a contract is held here.
  Widget _worded(BuildContext context, SocialState s) {
    final lists = s.lists;
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
    // The ways of each button, each act's where each person field has someone to offer; none once the question is answered.
    final ways = [
      if (ask != null && o != null && ask.at != null)
        for (final b in o.buttons) (b, _waysOf(s, ask.me, b)),
    ];
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        if (first && ask != null)
          _name(context, ask.me, refresh: ask.onRefresh == null ? null : ask.refresh),
        if (first)
          for (var i = s.notices.length - 1; i >= 0; i--)
            _wordedNotice(s.notices[i], i, o),
        if (first)
          for (final a in s.pending) _wordedPending(a, o),
        if (first && ask != null && o != null)
          ..._topActs(context, s, ask, o, ways),
        for (final e in _listed(s, l.predicate))
          _wordedAtom(context, s, e, l.predicate, ways, counted: l.counted),
      ],
    );
  }

  /// The ways the viewer's state satisfies the acts of the button [b], in the order of its acts, each act's where each person field has someone to offer (vGLP, Section 6.2).
  static List<_Way> _waysOf(SocialState s, GTerm me, DeclaredButton b) => [
        for (final d in b.acts)
          if (ActInfo(d) case final a)
            for (final at in a.ways(s.atoms, me))
              if (a.offers(s, me, at)) (act: a, at: at, rest: d.binds),
      ];

  /// The ways of [ways] that bind the same values, each once: "A button that names several acts is drawn once for the ways of its acts that bind the same values" (vGLP, Section 6.2), each list the ways of one value, in order.
  static List<List<_Way>> _choices(List<_Way> ways) {
    final out = <String, List<_Way>>{};
    for (final w in ways) {
      (out[_valuesKey(w)] ??= []).add(w);
    }
    return out.values.toList();
  }

  /// The values a choice of a button offers: of the places [_Way.rest], those no atom beside it binds.
  static String _valuesKey(_Way w) =>
      [for (final k in w.rest) formatTerm(w.at[k]!)].join(',');

  /// The acts of the first tab: of each button, the ways no atom of a list binds, as one form, with a picker of those ways where they are several.  The way that binds nothing, as Establish's, is drawn here; so are those bound by an atom no tab lists, as GSN's Remove by `listed`.
  List<Widget> _topActs(BuildContext context, SocialState s, SocialAsk ask,
      RequestOffers o, List<(DeclaredButton, List<_Way>)> ways) {
    final listed = {for (final l in s.lists) l.predicate};
    return [
      for (final (b, ws) in ways)
        if ([
          for (final w in ws)
            if (!w.act.entries.any((e) => listed.contains(e.pred))) w
        ] case final top when top.isNotEmpty)
          _offerForm(context, s, ask, o, b, _choices(top), ''),
    ];
  }

  /// A term's words as [_named] draws them, a speech act with its signers, innermost first, by name (vGLP 377aa37, Section 6.2, "The words a platform shows": "A speech act, in a tab, a question or a field, is drawn with its signers, innermost first, by the names the viewer's state holds for them").
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

  /// The atoms of the predicate [pred] in the state, each once with the count of its copies, in the order of the state: "The tab lists the atoms of that predicate in the state, each with its count where the contract adds or deletes the predicate with a multiplicity" (vGLP, Section 5.4, "The contract's words").
  static List<({GTerm atom, int count})> _listed(SocialState s, String pred) {
    final out = <({GTerm atom, int count})>[];
    final at = <String, int>{};
    for (final a in s.atoms) {
      if (ctorArgs(a).$1 != pred) continue;
      final k = formatTerm(a);
      final i = at[k];
      if (i == null) {
        at[k] = out.length;
        out.add((atom: a, count: 1));
      } else {
        out[i] = (atom: out[i].atom, count: out[i].count + 1);
      }
    }
    return out;
  }

  /// An atom's key in the keys of its controls: its one argument, or the atom.
  static String _atomKey(GTerm a) {
    final xs = ctorArgs(a).$2;
    return xs.length == 1 ? formatTerm(xs.single) : formatTerm(a);
  }

  /// An atom of a tab's list: its first argument by name, with its count where its list is counted, and under it the speech act's signers and the atom's other arguments; beside it the buttons of the ways it binds ([_atomActs]).
  Widget _wordedAtom(
      BuildContext context,
      SocialState s,
      ({GTerm atom, int count}) e,
      String pred,
      List<(DeclaredButton, List<_Way>)> ways,
      {bool counted = false}) {
    final k = _atomKey(e.atom);
    final xs = ctorArgs(e.atom).$2;
    final title = xs.isEmpty
        ? _atomText(e.atom)
        : '${_named(xs.first)}${counted ? '  ${e.count}' : ''}';
    final signed = xs.isEmpty ? null : SpeechAct.of(xs.first);
    final shows = [
      if (signed != null)
        [for (final p in signed.signers) personName(p)].join(', '),
      if (xs.length > 1) [for (final y in xs.skip(1)) _signed(y)].join(', '),
    ];
    final small = TextStyle(fontSize: 13, color: Colors.grey.shade700);
    return Container(
      key: socialKey(_id, 'atom:$k'),
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
          ..._atomActs(context, s, e.atom, k, pred, ways),
        ],
      ),
    );
  }

  /// The form open beside an atom, `name|key`; null where none is.
  String? _openAct;

  /// The buttons beside the atom [atom] of the list of [pred]: of each button, the ways [atom] binds, one button for them, and a tap on it either making the request of its one way, where nothing is left to fill, or opening the form with the picker of its ways and the act's fields (vGLP, Section 6.2: "each beside the atoms that bind it or as one button with a picker").
  List<Widget> _atomActs(BuildContext context, SocialState s, GTerm atom,
      String k, String pred, List<(DeclaredButton, List<_Way>)> ways) {
    final ask = _ask;
    final o = ask?.offers;
    if (ask == null || o == null) return const [];
    final here = <(DeclaredButton, List<List<_Way>>)>[];
    for (final (b, ws) in ways) {
      final beside = <_Way>[];
      for (final w in ws) {
        for (final e in w.act.entries) {
          if (e.pred != pred || ActInfo.matched(atom, e, w.at) == null) continue;
          final bound = ActInfo.placesOf(e);
          beside.add((
            act: w.act,
            at: w.at,
            rest: [
              for (final p in w.act.act.binds)
                if (!bound.contains(p)) p
            ]
          ));
        }
      }
      if (beside.isNotEmpty) here.add((b, _choices(beside)));
    }
    if (here.isEmpty) return const [];
    return [
      const SizedBox(height: 6),
      Wrap(spacing: 8, runSpacing: 4, children: [
        for (final (b, cs) in here)
          OutlinedButton(
            key: socialKey(_id, 'act:${_buttonName(b)}:$k'),
            onPressed: () {
              final open = '${_buttonName(b)}|$k';
              if (_nothingToFill(cs)) {
                final w = cs.single.first;
                final r = _request(s, o, w, const {}, ask);
                if (r != null) ask.propose(r);
              } else {
                setState(() => _openAct = _openAct == open ? null : open);
              }
            },
            child: Text(b.label),
          ),
      ]),
      for (final (b, cs) in here)
        if (_openAct == '${_buttonName(b)}|$k')
          _offerForm(context, s, ask, o, b, cs, k),
    ];
  }

  /// Whether the button of the ways [cs] makes its request at a tap: one value, which the atom beside it shows whole, no field and no text.
  static bool _nothingToFill(List<List<_Way>> cs) =>
      cs.length == 1 &&
      cs.single.every((w) =>
          w.rest.isEmpty && w.act.fields.isEmpty && w.act.words.isEmpty);

  /// The name of a button in a control's key: its one act's schema, or the button's text in lower case, each space an underscore, the name its schemas bear before their suffix (programs/vglp/cva/compiler/README.md, 2).
  static String _buttonName(DeclaredButton b) => b.acts.length == 1
      ? b.acts.single.schema
      : b.label.toLowerCase().replaceAll(RegExp(r' +'), '_');

  /// The person picked for a place of the composer [c], and the value chosen, by its key, of the ways of a button with a picker.
  final Map<String, GTerm> _picked = {};
  final Map<String, String> _chosen = {};

  /// The value the field [f] of the form holds: a person picked, a number, a key, an agent or a text; null where it is not yet given.
  GTerm? _fieldValue(SocialState s, RequestOffers o, ActInfo a, DeclaredField f,
      String field, String pick, GTerm me) {
    if (f.type == 'person') return _picked[pick];
    final t = _form(field).text.trim();
    if (t.isEmpty) return null;
    switch (f.type) {
      case 'number':
        final v = int.tryParse(t);
        return v == null ? null : GInt(v);
      case 'key':
        // "The field of an acquaintance takes the key of a person the viewer does not know" (vGLP, Section 6.2).
        if (!_identifier.hasMatch(t) ||
            t == displayText(me) ||
            s.known.any((p) => displayText(p) == t)) {
          return null;
        }
        return GAtom(t);
      case 'agent':
        return _identifier.hasMatch(t) ? GAtom(t) : null;
    }
    final key = o.keyOf(a.schema);
    final kinds = key == null ? const <String>[] : o.kinds[key] ?? const [];
    if (f.place <= kinds.length && kinds[f.place - 1] == 'speech') {
      return GStruct('speech', [
        GStruct('utterance', [GString(t)]),
        me,
      ]);
    }
    return GString(t);
  }

  /// The request of the way [w] with the fields' values [given], "The request it makes carries the values of the way" (GLP #12, Code_inbox.md, 2026-11-06 07:06 UTC), or null where a place is not filled or the act is not offered with those values ([ActInfo.lacks]) (programs/vglp/cva/compiler/README.md, 6: "Every place of the request is M, in Binds or a field's").
  GTerm? _request(SocialState s, RequestOffers o, _Way w,
      Map<int, GTerm> given, SocialAsk ask) {
    if (ask.at == null) return null;
    final at = {...w.at, ...given};
    final key = o.keyOf(w.act.schema);
    if (key == null) return null;
    final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
    final args = [for (var i = 1; i <= n; i++) at[i]];
    if (args.contains(null) ||
        !w.act.lacks(s.atoms, at) ||
        !w.act.distinct(at)) {
      return null;
    }
    return GStruct(w.act.schema, [for (final x in args) x!]);
  }

  /// The form of the button [b] for the ways [cs], each list the ways of one value: its text, the values no atom beside it shows, "with the values that way binds" (vGLP, Section 6.2), a picker of them where they are several, its fields, and the button, enabled once the form makes a request.  The fields are those of the ways of the value chosen, each name once, each name not yet held placed after the one before it in its own act, "the fields of a line name these variables, of the acts its button names together, in the schemas' order" (vGLP, Section 5.4, "The contract's words"); the request is that of the first way whose fields are those the person filled.
  Widget _offerForm(BuildContext context, SocialState s, SocialAsk ask,
      RequestOffers o, DeclaredButton b, List<List<_Way>> cs, String k) {
    final composer = '${_buttonName(b)}:$k';
    final me = ask.me;
    final small = TextStyle(fontSize: 13, color: Colors.grey.shade700);
    final chosenKey = _chosen[composer];
    final chosen = cs.length == 1
        ? cs.single
        : cs.where((c) => _valuesKey(c.first) == chosenKey).firstOrNull;
    final shown = chosen ?? [for (final c in cs) ...c];
    final union = <String>[];
    final kept = <String, (ActInfo, DeclaredField)>{};
    for (final w in shown) {
      var after = -1;
      for (final f in w.act.fields) {
        kept.putIfAbsent(f.label, () => (w.act, f));
        final at = union.indexOf(f.label);
        if (at >= 0) {
          after = at;
        } else {
          union.insert(++after, f.label);
        }
      }
    }
    int number(String l) =>
        b.acts.length == 1 ? kept[l]!.$2.place : union.indexOf(l) + 1;
    String fieldKey(String l) => 'field:$composer:${number(l)}';
    String pickKey(String l) => '$composer:${number(l)}';
    bool given(String l) => kept[l]!.$2.type == 'person'
        ? _picked[pickKey(l)] != null
        : _form(fieldKey(l)).text.trim().isNotEmpty;
    GTerm? request() {
      for (final w in chosen ?? const <_Way>[]) {
        final ls = {for (final f in w.act.fields) f.label};
        if (!union.every((l) => ls.contains(l) == given(l))) continue;
        final values = <int, GTerm>{};
        for (final f in w.act.fields) {
          final v = _fieldValue(s, o, w.act, f, fieldKey(f.label),
              pickKey(f.label), me);
          if (v != null) values[f.place] = v;
        }
        final r = _request(s, o, w, values, ask);
        if (r != null) return r;
      }
      return null;
    }

    // The persons the other person fields of the first way hold, for what a field offers and for the text.
    final first = shown.first;
    final at = <int, GTerm>{
      ...first.at,
      for (final f in first.act.fields)
        if (f.type == 'person')
          if (_picked[pickKey(f.label)] case final p?) f.place: p
    };
    final words = [
      for (final w in first.act.words)
        if (w is int) (at[w] == null ? '' : personName(at[w]!)) else w as String
    ].join();
    final r = request();
    final rest = first.rest;
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
          if (rest.isNotEmpty)
            Wrap(spacing: 6, runSpacing: 4, children: [
              // A picker labelled by the name of its type, as a field given no label is (vGLP #23, Code_inbox.md, 2026-11-05 13:33 UTC, 1; Section 6.2, "The words a platform shows").
              Text(_placeName(o, first.act, rest.first), style: small),
              for (final c in cs)
                ChoiceChip(
                  key: socialKey(_id, 'pick:$composer:${_valuesKey(c.first)}'),
                  label: Text([
                    for (final p in c.first.rest) _signed(c.first.at[p]!)
                  ].join(', ')),
                  selected: cs.length == 1 || chosenKey == _valuesKey(c.first),
                  onSelected: (_) => setState(
                      () => _chosen[composer] = _valuesKey(c.first)),
                ),
            ]),
          for (final l in union)
            if (kept[l]!.$2.type == 'person')
              Wrap(spacing: 6, runSpacing: 4, children: [
                Text(l, style: small),
                for (final p in kept[l]!.$1.offered(s, me, kept[l]!.$2.place,
                    {...at}..remove(kept[l]!.$2.place)))
                  ChoiceChip(
                    key: socialKey(_id, 'pick:${pickKey(l)}:${formatTerm(p)}'),
                    label: Text(personName(p)),
                    selected: _picked[pickKey(l)] != null &&
                        formatTerm(_picked[pickKey(l)]!) == formatTerm(p),
                    onSelected: (_) =>
                        setState(() => _picked[pickKey(l)] = p),
                  ),
              ])
            else ...[
              TextField(
                key: socialKey(_id, fieldKey(l)),
                controller: _form(fieldKey(l)),
                decoration: InputDecoration(labelText: l, isDense: true),
                keyboardType: kept[l]!.$2.type == 'number'
                    ? TextInputType.number
                    : TextInputType.text,
                onChanged: (_) => setState(() {}),
              ),
              // An agent variable, "by key or picked among the persons the viewer knows", the viewer among them, a person in no role (programs/vglp/cva/compiler/README.md, 6; vGLP, Section 5.4, "The contract's words": "A field is typed, a person by key or picked among the persons the viewer knows").
              if (kept[l]!.$2.type == 'agent')
                Wrap(spacing: 6, runSpacing: 4, children: [
                  for (final p in {
                    for (final x in [me, ...s.known]) formatTerm(x): x
                  }.values)
                    ActionChip(
                      key: socialKey(_id, 'pick:${pickKey(l)}:${formatTerm(p)}'),
                      label: Text(personName(p)),
                      onPressed: () => setState(
                          () => _form(fieldKey(l)).text = displayText(p)),
                    ),
                ]),
            ],
          const SizedBox(height: 6),
          ElevatedButton(
            key: socialKey(_id, 'send:$composer'),
            onPressed: r == null
                ? null
                : () {
                    ask.propose(r);
                    setState(() {
                      for (final l in union) {
                        _form(fieldKey(l)).clear();
                        _picked.remove(pickKey(l));
                      }
                      _chosen.remove(composer);
                      _openAct = null;
                    });
                  },
            child: Text(b.label),
          ),
        ],
      ),
    );
  }

  /// The name of the place [i] of the request of [a], as the default widget names it.
  static String _placeName(RequestOffers o, ActInfo a, int i) {
    final key = o.keyOf(a.schema);
    if (key == null) return '';
    final n = int.parse(key.substring(key.lastIndexOf('/') + 1));
    return o.field(a.schema, n, i);
  }

  /// The text of the form's field [k]: the screen's, held across the request questions the agent asks one after another ([TabPlace.field]), or this tab's outside a screen of tabs.
  TextEditingController _form(String k) => widget.place?.field?.call(k) ?? _text(k);
}

/// A way an act is offered: the act, the values [at] of its places, and the places [rest] of its binding no atom beside the button binds.
typedef _Way = ({ActInfo act, Map<int, GTerm> at, List<int> rest});
