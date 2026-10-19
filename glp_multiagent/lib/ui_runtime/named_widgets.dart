/// The named widgets of the grassroots app's construct family: the Dart
/// widgets a widget declaration `T =::= W` may name (vGLP,
/// sections/elicitation.tex, Definition "Widget Declaration": "In the
/// grassroots app, W names a Dart widget: the construct family draws it, and
/// its type fixes the terms it forms").
///
/// A named widget is drawn over its construct's view, as the draw
/// `draw(Id, named(W, D), V)` carries it, and takes the person's input
/// through the grant of the question the view marks `input(P)` (bridge.dart).
/// The draw carries the name `W`, the default widget `D` of the type, and the
/// view `V`: "It is drawn with the default widget of its type beside its
/// name, from which it reads the type's constructors" (vGLP 92e6d3b, Section
/// 6.2, the paragraph after Proposition "Default Widgets"; Section 6.3, the
/// type `Widget`).  The construct family hands it `D` with the view
/// (construct_family.dart, `_named`).  It grants the raw input the default
/// widget of the type grants, so that the construct process forms the same
/// answers from it (Definition "Construct Process").
///
/// The one named widget here is [approvalSentence], over the approval
/// question of every canonical CVA platform vGLP's compiler prints
/// (programs/vglp/cva/compiler; vGLP, Section 5.4, "The compilation"):
/// `Approval ::= approval(Attempt, YesNo?)`, `Attempt ::= attempt(Peer,
/// Integer, Integer, Class)`, `YesNo ::= yes ; no`, each class an act schema
/// applied to its arguments, each argument a `Peer` or a `Speech`, `Speech
/// ::= speech(Utterance, Peer)`, `Utterance ::= utterance(String) ; Speech`.
/// It names nothing of a particular platform: the class is read off the
/// view.
library;

import 'package:flutter/material.dart';

import 'bridge.dart';
import 'construct_family.dart';
import 'term.dart';

/// The name a widget declaration gives [approvalSentence]:
/// `Approval =::= approval_sentence.`
const String approvalSentenceName = 'approval_sentence';

/// The key of the button of construct [id] granting the constant [c] at the
/// question at [at].
ValueKey<String> answerKey(int id, String c, List<int> at) =>
    ValueKey('$id:answer:$c:${pathText(at)}');

/// The key of the sentence of construct [id].
ValueKey<String> sentenceKey(int id) => ValueKey('$id:sentence');

/// The wording of an approval question, read off its view
/// `approval(attempt(I, N, K, C), Q)`: the sentence, the speech acts of the
/// class, and the attempt's number and rank.
class ApprovalWording {
  /// "alice asks to befriend you": the initiator `I` by name, `asks to`,
  /// the schema of the class `C`, its underscores as spaces, and the class's
  /// other arguments that are persons, in order, the person asked as `you`.
  final String sentence;

  /// The speech acts among the class's arguments, in order: each its text
  /// and its byline, "by alice", the person asked as `you`.
  final List<({String text, String byline})> speech;

  /// The attempt's number `N` and rank `K`.
  final String number, rank;

  const ApprovalWording(this.sentence, this.speech, this.number, this.rank);

  /// The wording of [attempt], `attempt(I, N, K, C)`, or null where it is
  /// not one.
  ///
  /// The person asked is the one person among the arguments of `C` other
  /// than `I`, where there is exactly one: the approval question is the one
  /// a Start raises (vGLP, Section 5.4, "The compilation"), at a participant
  /// of the attempt other than its initiator, and the view does not name the
  /// agent it is asked at.  Where the class names two persons or more
  /// besides `I`, each is named and none is `you`.
  static ApprovalWording? of(GTerm attempt) {
    final (af, xs) = ctorArgs(attempt);
    if (af != 'attempt' || xs.length != 4) return null;
    final initiator = formatTerm(xs[0]);
    final (schema, args) = ctorArgs(xs[3]);
    if (schema.isEmpty) return null;
    final persons = [for (final a in args) if (SpeechAct.of(a) == null) a];
    final others = {
      for (final p in persons)
        if (formatTerm(p) != initiator) formatTerm(p)
    };
    final asked = others.length == 1 ? others.single : null;
    String name(GTerm p) => formatTerm(p) == asked ? 'you' : displayText(p);
    final objects = <String>[];
    var initiatorNamed = false;
    for (final p in persons) {
      if (!initiatorNamed && formatTerm(p) == initiator) {
        initiatorNamed = true;
        continue;
      }
      objects.add(name(p));
    }
    return ApprovalWording(
      '${displayText(xs[0])} asks to ${schema.replaceAll('_', ' ')}'
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
      displayText(xs[1]),
      displayText(xs[2]),
    );
  }
}

/// A speech act, `speech(U, P)`: its text, the utterance at its core, and
/// its signers, the one who signed the utterance and each who signed it
/// again after, in order (GSC, Section 3: "A speech act is an utterance or a
/// speech act, signed by a person").
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

/// `approval_sentence`: the approval question of a canonical CVA platform
/// drawn as a sentence --- "alice asks to befriend you." --- each speech act
/// of the class under it, its text and its signers, the attempt's number and
/// rank in small print, "Approve?", and two buttons, Yes and No, granting
/// `yes` and `no` at the question's position, `[2]`, as the default widget
/// of `YesNo?`, `buttons([yes, no])`, grants them.  It is drawn by
/// `named(approval_sentence, D)`, `D` the default widget of `Approval`,
/// whose constructors it reads off the view, the type being one for every
/// canonical CVA platform.  A view that is not an approval question of that
/// type is drawn as such, named, and takes no input: nothing stands in for
/// it.
Widget approvalSentence(
    BuildContext context,
    OpenConstruct construct,
    GTerm dflt,
    GTerm view,
    List<int> at,
    void Function(GTerm path, GTerm raw) grant) {
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
      if (wording != null)
        Padding(
          padding: const EdgeInsets.only(top: 6),
          child: Text('attempt ${wording.number}, rank ${wording.rank}',
              style: small),
        ),
      if (p != null && pAt != null) ...[
        const SizedBox(height: 14),
        Text('Approve?', style: theme.textTheme.bodyLarge),
        const SizedBox(height: 8),
        Row(children: [
          for (final (c, label) in const [('yes', 'Yes'), ('no', 'No')]) ...[
            ElevatedButton(
              key: answerKey(construct.id, c, pAt),
              onPressed: () => grant(p, GAtom(c)),
              child: Text(label),
            ),
            const SizedBox(width: 12),
          ],
        ]),
      ] else
        // The question answered: the answer, shown and no more taken.
        Padding(
          padding: const EdgeInsets.only(top: 8),
          child: Text(displayText(args[1])),
        ),
    ],
  );
}

/// A view a named widget cannot draw: named, taking no input.
Widget _cannotDraw(String widget, GTerm view) => Text(
    '$widget cannot draw ${formatTerm(view)}',
    style: TextStyle(color: Colors.red.shade700));
