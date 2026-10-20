/// The named widgets of the grassroots app's construct family: the Dart widgets a widget declaration `T =::= W` may name (vGLP, sections/elicitation.tex, Definition "Widget Declaration": "In the grassroots app, W names a Dart widget: the construct family draws it, and its type fixes the terms it forms").
///
/// A named widget is drawn over its construct's view, as the draw `draw(Id, named(W, D), V)` carries it, and takes the person's input through the grant of the question the view marks `input(P)` (bridge.dart).  The draw carries the name `W`, the default widget `D` of the type, and the view `V`: "It is drawn with the default widget of its type beside its name, from which it reads the type's constructors" (vGLP 92e6d3b, Section 6.2, the paragraph after Proposition "Default Widgets"; Section 6.3, the type `Widget`).  The construct family hands it `D` with the view, and the last value drawn on construct 0: "A declared widget is drawn also with the last value drawn on construct 0, and is drawn again when that value changes, so that a request form can show the state the program last wrote to its person" (vGLP e924f73, Section 6.2, the paragraph after Proposition "Default Widgets"; construct_family.dart, `_named`).  It grants the raw input the default widget of the type grants, so that the construct process forms the same answers from it (Definition "Construct Process").
///
/// Three named widgets are here.  [lastState] and [socialMenu], below, draw the apps of GSC's contracts in their tabs, GSNP's items and the notices of attempts that close without committing among them, and, declared likewise by CSSN's app (programs/cssn/self.vglp), its persons each with what the state says of them and its composed requests.  The first is [approvalSentence], over the approval question of every canonical CVA platform vGLP's compiler prints (programs/vglp/cva/compiler; vGLP, Section 5.4, "The compilation"): `Approval ::= approval(Attempt, YesNo?)`, `Attempt ::= attempt(Peer, Integer, Integer, Class, Names)`, the attempt (i, k, pi, [t]_j, r) naming its request r, `Names ::= none ; request(Rho, Integer)` (vGLP f0acbc1, Definition "Attempt"), `YesNo ::= yes ; no`, each class an act schema applied to its arguments, each argument a `Peer` or a `Speech`, `Speech ::= speech(Utterance, Peer)`, `Utterance ::= utterance(String) ; Speech`.  It reads the class off the view and draws one sentence for it and two buttons labelled for it, in the words of GAI's task for the apps of GSC's contracts (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, item 3, its sentences as corrected at 11:35 UTC): `befriend(A, Me)`, "Alice asks to be your friend.", Accept and Decline; `join(A, Me, n)`, "Alice asks you to join the group friends.", Join and Decline; and, in GAI's app, `agent_befriend(R, Q, P, Y)`, in the words of GAI's task for its figure (Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, (b) and (c)), at the principal "Alf asks to befriend Bob." and "Approve?", and at the other party "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme" and "Befriend Alf?", with Yes and No; and `agent_meet(R, Q, P, Y)` likewise, "Alf asks to meet Bob." and "Approve?" at the principal, and "Alf, an AI agent of Alice, asks to meet you.", "Developer: Acme" and "Meet Alf?" at the other party.  And, in CSSN's app, `child_befriend(R, S, P, Q)`, in the words Udi gave for CSSN's Figure 3 (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10 UTC, (3) and (4)), at a parent "Alice asks that their child Carol and your child Dave befriend." and "Approve?", and at a child "Alice asks that you and Dave befriend, with Dave's parent Bob." and "Befriend Dave?", or, asked by the other child's parent, "Alice asks that you and their child Carol befriend." and "Befriend Carol?", with Yes and No (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, items 1 and 5); and `enter(P, R)` and `enter_2(P, Q, R)`, at the child "Alice enters you as their child." and at the other parent "Bob asks to enter Dave as your child, with you.", each with "Approve?", Yes and No (the same, item 4).  A class the task gives no words for is drawn by a sentence built from its schema and its persons, each speech act of the class under it, and the buttons Yes and No.  A person is shown by name, their key with its underscores as spaces and each word capitalised (the same task, its preamble, which capitalised the first letter; GAI #5 Cowork, Code_inbox.md, 2026-10-19 21:09 UTC, item 1: "The developer's key is acme_ltd, drawn \"Acme Ltd\"").
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

  /// The wording of [attempt], `attempt(I, N, K, C, R)`, or null where it is not one: the words of GAI's tasks where they word the class `C`, and a sentence built from `C` otherwise.  [state] is the last view the agent asked wrote on construct 0, with which the widget is drawn ("A declared widget is drawn also with the last value drawn on construct 0", vGLP e924f73, Section 6.2), where it tells whom the question is asked; null where no view is drawn.
  static ApprovalWording? of(GTerm attempt, [SocialState? state]) {
    final (af, xs) = ctorArgs(attempt);
    if (af != 'attempt' || xs.length != 5) return null;
    final (schema, args) = ctorArgs(xs[3]);
    if (schema.isEmpty) return null;
    if (schema == 'child_befriend' && args.length == 4) {
      return _childBefriend(xs[0], args, state) ?? _built(xs[0], schema, args);
    }
    if ((schema == 'enter' && args.length == 2) ||
        (schema == 'enter_2' && args.length == 3)) {
      return _enter(xs[0], args, state) ?? _built(xs[0], schema, args);
    }
    return _worded(xs[0], schema, args, state) ?? _built(xs[0], schema, args);
  }

  /// `child_befriend(R, S, P, Q)`, the children `R` and `S` befriending by an act of both and of a parent of each, `P` of `R` and `Q` of `S`, `P` a friend of `Q` (CSSN:2, Wards:2; `child_befriend(r?, s?, p?, q?)`, programs/cssn/CSSN.glp), in the words Udi gave for CSSN's Figure 3 (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10 UTC, (3) and (4)), with the initiator "their", the paper's pronoun for a person, and the approval of the child whose parent did not ask (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, items 1 and 5), whom it is asked read off the last view.  At a parent, whose view holds `parenting` of one of the two children, the initiator being the other child's parent: "Alice asks that their child Carol and your child Dave befriend.", "Approve?", Yes and No.  At a child, whose view holds `child` and `parent` of the parent of one of the two: where the initiator is the other child's parent, "Alice asks that you and their child Carol befriend.", "Befriend Carol?"; otherwise "Alice asks that you and Dave befriend, with Dave's parent Bob.", "Befriend Dave?", the other child and their parent named; with Yes and No.  Null where no view is drawn or it tells neither, so that the sentence is built from the class.
  static ApprovalWording? _childBefriend(
      GTerm initiator, List<GTerm> args, SocialState? state) {
    if (state == null || args.any((a) => SpeechAct.of(a) != null)) return null;
    final (r, s, p, q) = (args[0], args[1], args[2], args[3]);
    bool same(GTerm a, GTerm b) => formatTerm(a) == formatTerm(b);
    final i = personName(initiator);
    // At a parent: their child, the other child, and the other child's parent, who asks.
    for (final (mine, other, otherParent) in [(s, r, p), (r, s, q)]) {
      if (state.isParentOf(mine) &&
          !state.isParentOf(other) &&
          same(initiator, otherParent)) {
        return ApprovalWording(
            '$i asks that their child ${personName(other)} and your child '
            '${personName(mine)} befriend.',
            const [],
            'Yes',
            'No',
            question: 'Approve?');
      }
    }
    // At a child: their parent, the other child, and the other child's parent.
    if (state.isChild && !same(p, q)) {
      for (final (myParent, other, otherParent) in [(p, s, q), (q, r, p)]) {
        if (state.isChildOf(myParent)) {
          final o = personName(other);
          return ApprovalWording(
              same(initiator, otherParent)
                  ? '$i asks that you and their child $o befriend.'
                  : '$i asks that you and $o befriend, with $o\'s parent '
                      '${personName(otherParent)}.',
              const [],
              'Yes',
              'No',
              question: 'Befriend $o?');
        }
      }
    }
    return null;
  }

  /// `enter(P, R)` and `enter_2(P, Q, R)`, a parent `P`, or two parents `P` and `Q` jointly, entering the child `R` (CSSN:1, Wards:1; `enter(p?, r?)` and `enter_2(p?, q?, r?)`, programs/cssn/CSSN.glp), proposed by `P`, in the words CSSN gave (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, item 4), whom it is asked read off the last view.  At the child, whose view holds no identity: "Alice enters you as their child."; at the other parent of `enter_2`, whose view holds their identity: "Bob asks to enter Dave as your child, with you."; each with "Approve?", Yes and No.  Null where no view is drawn, the initiator is not `P`, or an argument is a speech act, so that the sentence is built from the class.
  static ApprovalWording? _enter(
      GTerm initiator, List<GTerm> args, SocialState? state) {
    if (state == null ||
        args.any((a) => SpeechAct.of(a) != null) ||
        formatTerm(initiator) != formatTerm(args[0])) {
      return null;
    }
    final i = personName(initiator);
    final sentence = state.identity && args.length == 3
        ? '$i asks to enter ${personName(args[2])} as your child, with you.'
        : '$i enters you as their child.';
    return ApprovalWording(sentence, const [], 'Yes', 'No',
        question: 'Approve?');
  }

  /// The classes GAI's tasks word, the initiator `I` their first argument `A` and the person asked `Me` their second, a person other than `I`: `befriend(A, Me)`, "Alice asks to be your friend.", Accept and Decline; `join(A, Me, n)`, `n` the group's name, a speech act, "Alice asks you to join the group friends.", its text the group's name, Join and Decline; and `agent_befriend(R, Q, P, Y)` ([_agentBefriend]) and `agent_meet(R, Q, P, Y)` ([_agentMeet]).  And GKK's, which every contract of GSC's extends (programs/gsc/GKK.glp; GSC #5 Cowork, Code_inbox.md, 2026-10-20 17:53 UTC): `meet(A, Me)`, GKK:2, "Alice asks to meet you in person.", Yes and No; and `introduce(A, Me, C)`, GKK:3, `C` the person introduced, "Alice introduces Carol to you.", Yes and No.  Null for every other class.
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
      case ('agent_befriend', 4):
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
    }
    return null;
  }

  /// `agent_befriend(R, Q, P, Y)`, the AI agent `R` entering a friendship with `Q` by an act of both and of its principal `P`, presenting its attestation `Y` (Wards:2, GAI:2; `agent_befriend(r?, q?, p? ; y)`, programs/gai/GAI.glp), asked at `Q` and at `P`, in the words of GAI's task (Code_inbox.md, GAI #5 Cowork, 2026-10-19 20:36 UTC, (b) and (c)).  At the principal, whose last view holds `principal_of(R)` (`enter` adds it at `p`): "Alf asks to befriend Bob.", "Approve?", Yes and No.  At `Q`: "Alf, an AI agent of Alice, asks to befriend you.", "Developer: Acme", the developer named by the attestation ([SocialState.developerIn]), "Befriend Alf?", Yes and No.  Null where no view is drawn, so that whom it is asked cannot be told, or `Y` is no speech act.
  static ApprovalWording? _agentBefriend(
      List<GTerm> args, SocialState? state) {
    final (r, q, p, y) = (args[0], args[1], args[2], args[3]);
    final developer = SocialState.developerIn(y);
    if (state == null || developer == null || SpeechAct.of(p) != null) {
      return null;
    }
    final agent = personName(r);
    if (state.isPrincipalOf(r)) {
      return ApprovalWording(
          '$agent asks to befriend ${personName(q)}.', const [], 'Yes', 'No',
          question: 'Approve?');
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
  if (f != 'approval' || args.length != 2) {
    return _cannotDraw(approvalSentenceName, view);
  }
  final p = questionAt(args[1]);
  final pending = pendingAt(args[0]) != null;
  final (_, state) = SocialState.read(output);
  final wording = pending ? null : ApprovalWording.of(args[0], state);
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

  /// The person's friends (GSG:1).
  List<GTerm> get friends => [for (final x in _args('friend', 1)) x[0]];

  /// The person's contacts, the persons they know, `knows(Q)`: met in person (GKK:2) or introduced to them (GKK:3) (programs/gsc/GKK.glp: "knows(q) in the state of p says that p holds q's public key").
  List<GTerm> get contacts => [for (final x in _args('knows', 1)) x[0]];

  /// Whether the person knows [q], `knows(q)`.
  bool isContact(GTerm q) => contacts.any((k) => _same(k, q));

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

  /// Whether the view holds a relation of a parent and their child, so that [relationOf] is drawn by each person's name.
  bool get hasWards => isChild || children.isNotEmpty || parents.isNotEmpty;

  /// What the person's state says of [q], in the words Udi gave for CSSN's Figure 3 (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10 UTC, (1) and (5): "Carol, your child", "Bob, your friend", "Alice, your parent"): "your child" where it holds `parenting(q)`, "your parent" where it holds `parent(q)`, and otherwise "your friend" where it holds `friend(q)`; null where it says none of these.
  String? relationOf(GTerm q) => isParentOf(q)
      ? 'your child'
      : isChildOf(q)
          ? 'your parent'
          : isFriend(q)
              ? 'your friend'
              : null;

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
  'child_join',
  'child_join_2',
  'child_leave',
  'child_close',
};

/// A notice the agent writes on its output when an approval its person made, by a request or by an answer yes, ends by a refusal: `refused(P, Q, D)`, `P` the part of the refused attempt, a term of its class, `Q` its refuser, and `D` the reason, `declined`, or `unable(Lacks, Has)`, the atoms the schema requires at the refuser that its state lacks and those it forbids that its state has, as the contract writes them: "a notice refused([t]_j, q, d): [t]_j the part of the refused attempt, q its refuser and d the reason, from the abort outcome, or from the vote where the agent's own vote no refused it"; "A vote unable of a compiled program carries the atoms the schema requires at the voter that its state lacks, and those it forbids that its state has, and the notice shows them" (vGLP f0acbc1, Section 5.4, "The compilation" and "Act schemas"; the types `Output ::= view(List(Atom), List(Key)) ; refused(Class, Key, Reason)` and `Reason ::= declined ; unable(List(Atom), List(Atom))`, programs/apps/<app>/platform.vglp and programs/cssn/platform.vglp).  "When a refusal ends an approval, the person who made it is told who refused the transaction and why, declined or unable" (Section 5.4, "Triggers"): the person told is any guard whose approval the refusal ends, the one who requested the transaction or one who answered yes, so the sentence does not take the person to have proposed it ([sentence]).  It is drawn as one sentence in the tab it concerns.
class SocialNotice {
  /// The part of the refused attempt, a term of its class: `befriend(alice, bob)`.
  final GTerm part;

  /// The refuser, the participant whose vote no refused the attempt.
  final GTerm by;

  /// Whether the reason is `declined`, the refuser's person having answered no; otherwise it is `unable(Lacks, Has)`.
  final bool declined;

  /// For `unable(Lacks, Has)`: the atoms the schema requires at the refuser that its state lacks, and those it forbids that its state has.
  final List<GTerm> lacks, has;

  const SocialNotice(this.part, this.by,
      {this.declined = false, this.lacks = const [], this.has = const []});

  /// The notice [t], `refused(P, Q, D)`, or null where it is not one.
  static SocialNotice? of(GTerm t) {
    final (f, xs) = ctorArgs(t);
    if (f != 'refused' || xs.length != 3 || ctorArgs(xs[0]).$1.isEmpty) {
      return null;
    }
    final d = xs[2];
    if (d is GAtom && d.name == 'declined') {
      return SocialNotice(xs[0], xs[1], declined: true);
    }
    final (u, us) = ctorArgs(d);
    if (u == 'unable' && us.length == 2 && us[0] is GList && us[1] is GList) {
      return SocialNotice(xs[0], xs[1],
          lacks: (us[0] as GList).items, has: (us[1] as GList).items);
    }
    return null;
  }

  /// The schema of the part's class.
  String get schema => ctorArgs(part).$1;

  /// Whether the notice concerns the Groups tab.
  bool get concernsGroup => groupClasses.contains(schema);

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
      if (const {'join', 'child_join', 'child_join_2'}.contains(f) &&
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

  /// The transaction the part names, as the person reads it, a noun phrase, the person as "you" and "your": a friendship, "your friendship with Bob", or "your friendship" where the other is the refuser, "the friendship of Carol and Dave" where the person is neither; its end, "the end of your friendship with Bob"; a group's acts, "Bob's joining the group friends", "the closing of the group friends"; "your message to Carol"; CSSN's, "Alice's entering Carol as their child", "your entering Dave as your child, with Grace", "Bob's declaring Dave of age"; and, for a class not named here, "the act" and its schema and persons, "the act agent unfriend of Alf, Bob and Alice".
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
      case 'befriend' || 'child_befriend' || 'child_adult_befriend' ||
            'agent_befriend' || 'agents_befriend'
          when persons([0, 1]):
        return friendship(xs[0], xs[1]);
      case 'parent_unfriend_2' when xs.length == 3 && persons([0, 2]):
        return 'the end of ${friendship(xs[0], xs[2])}';
      case 'unfriend' || 'unfriend_p' || 'unfriend_q' || 'child_unfriend' ||
            'parent_unfriend' || 'agent_unfriend'
          when persons([0, 1]):
        return 'the end of ${friendship(xs[0], xs[1])}';
      case 'join' || 'child_join' || 'child_join_2'
          when xs.length >= 3 && persons([1]):
        return '${whose(xs[1])} joining $group';
      case 'leave' || 'child_leave' when xs.length >= 3 && persons([1]):
        return '${whose(xs[1])} leaving $group';
      case 'close' || 'child_close':
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
      case 'meet' || 'agent_meet' || 'agents_meet' when persons([0, 1]):
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
      case 'enter' when xs.length == 2 && persons([0, 1]):
        return '${whose(xs[0])} entering ${who(xs[1])} as '
            '${isMe(xs[0]) ? 'your' : 'their'} child';
      case 'enter' when xs.length == 3 && persons([0, 1]):
        return 'the creation of ${whose(xs[0])} AI agent '
            '${personName(xs[1])}';
      case 'enter_2' when xs.length == 3 && persons([0, 1, 2]):
        return '${whose(xs[0])} entering ${who(xs[2])} as '
            '${isMe(xs[0]) || isMe(xs[1]) ? 'your' : 'their'} child, with '
            '${who(xs[1])}';
      case 'of_age' || 'of_age_2' when xs.length >= 2 && persons([0]):
        return '${whose(xs[0])} declaring ${who(xs.last)} of age';
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
      case ('child', 0):
        return '$s $be$not a child';
      case ('agent', 0):
        return '$s $be$not an AI agent';
      case ('coparented', 0):
        return held ? '$s $have two parents' : '$s $doNot have two parents';
      case ('friend', 1):
        return '$s and ${who(xs[0])} are$not friends';
      case ('knows', 1):
        return held
            ? '$s ${you ? 'know' : 'knows'} ${who(xs[0])}'
            : '$s $doNot know ${who(xs[0])}';
      case ('parent', 1):
        return '${who(xs[0])} ${isOf(xs[0])}$not ${whose(v)} parent';
      case ('parenting', 1):
        return '$s $be$not ${whose(xs[0])} parent';
      case ('coparent', 1):
        return '$s and ${who(xs[0])} are$not parents of a child together';
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

/// `social_menu`: the request question of a canonical platform of GSC's contracts, `Menu ::= menu(Key, List(Key), Request?)`, drawn with the last view on construct 0, the machine state and the persons known, in the tab it is drawn in, as GAI's task words it (Code_inbox.md, GAI #3 Cowork, 2026-10-19 10:45 UTC, items 1 and 2).  In the Friends tab: the person's own name at the top; before their identity is established, the button Join, proposing `establish(Me)`; after it, the persons known, one row each, a friend with the button End friendship, proposing `unfriend(Me, Q)`, and any other with Befriend, proposing `befriend(Me, Q)`; at the bottom the field Add a person by key with Add, answering `known(Q)`.  Where the request type offers `meet(Me, Q)`, GKK:2 (programs/gsc/GKK.glp, which every contract of GSC's extends), the rows are the persons known and the person's contacts, those the state knows, `knows(Q)`; a person known who is not a contact is drawn with Meet, proposing `meet(Me, Q)`, and Befriend is drawn on a contact's row, befriend requiring that the two know each other (GSG:1); and where it offers `introduce(Me, Q, C)`, GKK:3, the person composes "Introduce Carol to Bob." under the rows ([_SocialTabState._composers]).  Where the request type offers `message(Me, Q, X)`, tapping a friend opens the chat with them, its messages and a text field with Send, proposing `message(Me, Q, x)`, x the speech act of the text typed signed by the person.  Where it offers `create(Me, N)`, the Groups tab: the field New group with Create, proposing `create(Me, n)`; each group the person is in, its name, its leader and its messages, and, in a group they lead, its members; in a group the person leads, Add member, a picker of their friends not in it, proposing `join(Me, Q, n)`, and Close group, proposing `close(Me, n)`; in any group, a text field with Send, proposing `post_own(Me, x, n)` where they lead it and `post(Me, A, x, n)` otherwise; and in a group they do not lead, Leave group, proposing `leave(A, Me, n)`.  Each control is drawn where the request type offers its request, read off `D` ([RequestOffers]), and while the question is open; each grants `propose(P)` or `known(Q)` at the question's position, `[3]`, as the default widget of the request type grants it, so the construct process forms the same answers from it.  No term, functor, type name or Submit button is drawn.  The persons known, and who is a friend, are read off the last view, which the agent writes whenever either changes (vGLP b7e3517), and not off the request question, which shows the persons known when it was asked.  Where construct 0 holds no stream of views, it is drawn as such, named, and takes no input.
///
/// In GSNP (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, items 1 to 3): an item the person holds is drawn in the chat with the friend it came from, its last signer, its text, "Author: Dan" and, where it was forwarded, "via Eve, via Carol"; under one the person did not author, Forward, which opens a picker of their friends and, once one is picked, the warranty question of GSNP:2 with Yes, proposing `forward(Me, Q, x)`, and No; the chat's Send signs the text, proposing `sign(Me, x)`, and holds `message(Me, Q, x)` for the next request question, proposed once the view holds `authored(x)` ([_SocialTabState._chat]).  An item the person authored is drawn in the chat it was sent from, and an item from a person who is not a friend, or authored and sent from no chat on this screen, under Messages at the bottom of the Friends tab.  In GAI's app (programs/apps/gai), whose request type offers `sign(Me, X)`, `message(Me, Q, X)` and `forward(Me, Q, X)` as GSNP's does, an item is drawn as in GSNP, and an author the person holds recorded as an AI agent, `agent_of(r, p, y)`, is named with its principal, "Author: Alf, an AI agent of Alice" (Code_inbox.md, GAI #3 Cowork, 2026-10-19 13:31 UTC, its last paragraph), with its developer under it, "Developer: Acme", as in its row in Friends; and, where the request type offers `enter(Me, R, X)`, the AI agents tab: the form Create an AI agent, taking the name and the code and proposing `sign(Me, k)` and then `enter(Me, r, k)`, and the person's AI agents, each with Retire, proposing `retire(Me, R)` (GAI #5 Cowork, 2026-10-19 20:36 UTC, (a) and item 5; [_SocialTabState._agents]).  A post carries an item, which the person signs before posting it (GSNP:1; `post_own(a? ; x, n)` requires `item(x)` at `a`, programs/gai/GSNP.glp), so the text typed in a group forms no post: where the request type offers `sign(Me, X)`, no text field and Send are drawn in a group.  The notices are drawn as `last_state` draws them.
///
/// In CSSN's app (programs/cssn), whose request type offers `child_befriend(R, S, P, Q)`, each person is drawn with what the state says of them, "Carol, your child", "Alice, your parent", "Bob, your friend" ([SocialState.relationOf]), the person's parents first, then their children, their friends and the others; the row of a parent or a child offers no End friendship, `unfriend(p?, q)` forbidding `parent(q)` and `parenting(q)` at `p`, and a child is offered no Befriend, `befriend(p?, q?)` forbidding `child` at both (programs/cssn/CSSN.glp); and under the persons are the requests the person composes ([_SocialTabState._composers]).
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
    return switch (widget.place?.tab) {
      ScreenTab.friends => _friends(context, s),
      ScreenTab.groups => _groups(context, s),
      ScreenTab.agents => _agents(context, s),
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
    // In CSSN's app, each with what the state says of them, the person's
    // parents first, then their children, their friends and the others.
    final relations = _relations(s);
    if (relations) {
      int rank(GTerm q) => s.isChildOf(q)
          ? 0
          : s.isParentOf(q)
              ? 1
              : s.isFriend(q)
                  ? 2
                  : 3;
      persons = [
        for (var k = 0; k < 4; k++)
          for (final q in persons)
            if (rank(q) == k) q
      ];
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
                relation: relations ? s.relationOf(q) : null,
                onTap: chats && open != null && s.isFriend(q)
                    ? () => open(q)
                    : null),
        if (ask != null && s.identity) ..._composers(context, s, ask),
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
        !s.isKnownAgent(q) &&
        !(widget.place?.agentsAsked.contains(k) ?? false)) {
      // GKK:2: a person known by key and not yet met is met in person, an
      // act of both, before anything that requires knowing them.  In GAI
      // `meet(p?, q?)` forbids `agent` at both parties (programs/gai/GAI.glp
      // at GLP 88d85368): an AI agent the view shows, or an open approval
      // question names, is met by `agent_meet`, with its principal.
      button = ElevatedButton(
        key: socialKey(_id, 'meet:$k'),
        onPressed: () => ask.propose(GStruct('meet', [ask.me, q])),
        child: const Text('Meet'),
      );
    } else if (ask != null &&
        !friend &&
        ask.can('befriend', 2) &&
        !s.isChild &&
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

  // --- CSSN's requests ---------------------------------------------------------

  /// Whether each person is drawn with what the state says of them ([SocialState.relationOf]): in CSSN's app, whose request type offers `child_befriend(R, S, P, Q)`, and, for construct 0 drawn with no request question, where the view holds a relation of a parent and their child.
  bool _relations(SocialState s) =>
      _ask?.offers.has('child_befriend', 4) ?? s.hasWards;

  /// The requests composed, each a sentence the person completes by choosing a person into each of its places, and Submit: in the apps of GSC's contracts that extend GKK, where the request type offers `introduce(P, Q, C)`, "Introduce Carol to Bob." with the button Introduce, no Submit being drawn in GSC's apps, `C` a contact of the person's and `Q` another the agent knows, proposing `introduce(Me, Q, C)` (GKK:3; `introduce(p?, q? ; c)`, programs/gsc/GKK.glp); once sent, "You asked to introduce Carol to Bob." (the words drawn by Code for GSC's confirmation, GSC #5 Cowork, Code_inbox.md, 2026-10-20 17:53 UTC); and in CSSN's app, in the words Udi gave for CSSN's Figure 3 (reported by CSSN #8 Cowork, Code_inbox.md, 2026-10-20 10:10 UTC, (2) and (6)):
  ///
  ///   - where the request type offers `child_befriend(R, S, P, Q)` and the person is a parent and no child: "Ask that Carol and Dave befriend, with Bob.", `R` one of their children, `parenting(R)`, `S` a person they know, and `Q` a friend of theirs who is not their child, proposing `child_befriend(R, S, Me, Q)` (CSSN:2; `child_befriend(r?, s?, p?, q?)`, programs/cssn/CSSN.glp); once sent, "You asked that Carol and Dave befriend, with Bob.";
  ///   - where it offers `of_age(P, R)` and the person is a parent: "Declare Dave of age.", `R` one of their children, proposing `of_age(Me, R)` (CSSN:3; `of_age(p?, r)`), which nobody is asked; once sent, "Dave is of age.";
  ///   - where it offers `enter(P, R)` and the person is no child, in the words CSSN gave (CSSN #8 Cowork, Code_inbox.md, 2026-10-20 13:13 UTC, item 4): "Enter Carol as your child.", `R` a person they know who is not their friend, proposing `enter(Me, R)` (CSSN:1; `enter(p?, r?)`); once sent, "You entered Carol as your child.";
  ///   - where it offers `enter_2(P, Q, R)` and the person is no child, in the same words: "Enter Dave as your child, with Grace.", `R` a person they know who is not their friend, and `Q` a friend of theirs who is neither their child nor their parent, proposing `enter_2(Me, Q, R)` (CSSN:1; `enter_2(p?, q?, r?)`); once sent, "You entered Dave as your child, with Grace.".
  ///
  /// What the person sent is drawn in the request's place on every later request question, the screen of tabs holding it ([TabPlace.sent]).
  List<Widget> _composers(BuildContext context, SocialState s, SocialAsk ask) {
    final me = ask.me;
    bool same(GTerm a, GTerm b) => formatTerm(a) == formatTerm(b);
    final children = s.children;
    final known = <GTerm>[];
    final seen = <String>{formatTerm(me)};
    for (final q in [...s.known, ...s.friends]) {
      if (seen.add(formatTerm(q))) known.add(q);
    }
    final r = _chosen['child-befriend:0'];
    // A friend who is neither the person's child nor their parent: the other
    // parent of a child befriending, and of a child entered by two.
    final coparents = [
      for (final q in s.friends)
        if (!s.isParentOf(q) && !s.isChildOf(q) && !same(q, me)) q
    ];
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
      if (ask.offers.has('child_befriend', 4) && !s.isChild)
        _composer(
          context,
          ask,
          kind: 'child-befriend',
          composing: children.isNotEmpty,
          slots: [
            children,
            [
              for (final q in known)
                if (r == null || !same(q, r)) q
            ],
            coparents,
          ],
          pieces: ['Ask that ', 0, ' and ', 1, ' befriend, with ', 2, '.'],
          request: (c) => GStruct('child_befriend', [c[0], c[1], me, c[2]]),
          sent: (c) =>
              'You asked that ${n(c[0])} and ${n(c[1])} befriend, with ${n(c[2])}.',
        ),
      if (ask.offers.has('of_age', 2))
        _composer(
          context,
          ask,
          kind: 'of-age',
          composing: children.isNotEmpty,
          slots: [children],
          pieces: ['Declare ', 0, ' of age.'],
          request: (c) => GStruct('of_age', [me, c[0]]),
          sent: (c) => '${n(c[0])} is of age.',
        ),
      if (ask.offers.has('enter', 2) && !s.isChild)
        _composer(
          context,
          ask,
          kind: 'enter',
          composing: known.any((q) => !s.isFriend(q)),
          slots: [
            [
              for (final q in known)
                if (!s.isFriend(q)) q
            ],
          ],
          pieces: ['Enter ', 0, ' as your child.'],
          request: (c) => GStruct('enter', [me, c[0]]),
          sent: (c) => 'You entered ${n(c[0])} as your child.',
        ),
      if (ask.offers.has('enter_2', 3) && !s.isChild)
        _composer(
          context,
          ask,
          kind: 'enter-2',
          composing: known.any((q) => !s.isFriend(q)) && coparents.isNotEmpty,
          slots: [
            [
              for (final q in known)
                if (!s.isFriend(q)) q
            ],
            coparents,
          ],
          pieces: ['Enter ', 0, ' as your child, with ', 1, '.'],
          request: (c) => GStruct('enter_2', [me, c[1], c[0]]),
          sent: (c) => 'You entered ${n(c[0])} as your child, with ${n(c[1])}.',
        ),
    ];
  }

  /// One composed request [kind]: what the person sent of it, each one line; and, where [composing], its sentence of [pieces], each a text or the index of a place, a person chosen into it from [slots] by tapping it, which opens a picker of the persons it takes, "…" where none is chosen; and the button [button], Submit unless given, enabled once every place holds a person and the question is open, which proposes [request] of the persons chosen and holds [sent] of them on the screen.  Nothing is drawn where nothing is sent and the person cannot compose it.
  Widget _composer(
    BuildContext context,
    SocialAsk ask, {
    required String kind,
    required bool composing,
    required List<List<GTerm>> slots,
    required List<Object> pieces,
    String button = 'Submit',
    required GTerm Function(List<GTerm>) request,
    required String Function(List<GTerm>) sent,
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
              chosen[i] == null ? '…' : personName(chosen[i]!),
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
                        label: Text(personName(q)),
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
                      place?.addSent?.call(kind, sent(c));
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
