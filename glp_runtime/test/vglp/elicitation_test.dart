// glp_runtime/test/vglp/elicitation_test.dart
//
// The interactive runtime with a scripted person: a vGLP program compiled on
// load, its compiled initial goal spawned with the dispatcher on its ask
// stream and on a person channel, and a person procedure that reads the draws
// and grants input by construct identifier.
// Spec: vGLP at c994328 --- sections/elicitation.tex, from "GLP already
// connects the program to the person" to Definition "Implementation of vGLP
// by GLP"; Definition "Construct, Submission, Complete Widget"; Remark
// "Persistence"; sections/vglp.tex, Definition "vmaGLP Transition System"
// (Present: every output before the inputs inside it), and the agent of
// Sections 1 and 3, whose question is the stream of the person's requests.
// vGLP's code task of 2026-10-01 16:30 UTC, tests (iv) and (v); the messages
// of 2026-10-01 23:55 UTC (E); vGLP #5 Cowork's task of 2026-10-03 08:16 UTC,
// with (_) and the handle out of the language (Udi, 2026-10-03, as vGLP
// reports it): (v)'s second half, once a construct withdrawing on its handle,
// is the agent's message clause reducing without a request.
// Programs: programs/tests/vglp/fragments (questions.vglp, the fragments of
// Sections 1 and 3, and its plays) and programs/tests/vglp/stream (chat.vglp,
// the chat input of Section 3, and its play).

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;

final _root = File('../programs/self.glp').absolute.path;
String _program(String name) =>
    Directory('../programs/tests/vglp/$name').absolute.path;

/// A run of [goal] in [program], and its bindings as GLP text.
class _Run {
  final ExecutionResult result;
  final GlpEngine engine;
  _Run(this.result, this.engine);

  ExecutionStatus get status => result.status;
  String? get error => result.error;

  /// The binding of [name], dereferenced through the heap; an unbound
  /// variable, at the top or inside, shown `_`.
  String operator [](String name) => _show(result.bindings[name]);

  String _show(rt.Term? t) {
    if (t == null) return '_';
    if (t is rt.VarRef) {
      final d = engine.runtime.heap.dereference(t);
      return d is rt.VarRef ? '_' : _show(d);
    }
    if (t is rt.ConstTerm) {
      return t.value == null || t.value == 'nil' ? '[]' : '${t.value}';
    }
    if (t is rt.StructTerm) {
      if (t.functor == '.' && t.args.length == 2) {
        final items = <String>[];
        rt.Term? cur = t;
        while (true) {
          final c = cur;
          if (c is rt.StructTerm && c.functor == '.' && c.args.length == 2) {
            items.add(_show(c.args[0]));
            cur = c.args[1];
            continue;
          }
          if (c is rt.VarRef) {
            final d = engine.runtime.heap.dereference(c);
            if (d is rt.VarRef) return '[${items.join(', ')} | _]';
            cur = d;
            continue;
          }
          if (c is rt.ConstTerm && (c.value == null || c.value == 'nil')) {
            return '[${items.join(', ')}]';
          }
          return '[${items.join(', ')} | ${_show(c)}]';
        }
      }
      return '${t.functor}(${t.args.map(_show).join(', ')})';
    }
    return '$t';
  }
}

Future<_Run> _play(String program, String goal) async {
  final engine = GlpEngine(rootSelfGlpPath: _root)
    ..loadProgram(_program(program));
  return _Run(await engine.runGoal(goal), engine);
}

void main() {
  group('(iv) Section 3\'s responder', () {
    test('the card shows the offering peer before any input, and one grant of '
        'yes completes it', () async {
      final r = await _play('fragments', 'play_card(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // The person granted yes on seeing the card drawn with bob in it and
      // its own position input; the responder took the answer.
      expect(r['Resp'], 'accept(bob)');
      expect(
          r['Log'],
          '[drawn(0, form(card, [shown, buttons([yes, no])]), '
          'card(bob, input)), withdrawn(0)]');
    });

    test('a grant that forms no term of the question\'s type answers nothing, '
        'and the card stays', () async {
      final r = await _play('fragments', 'play_card_refused(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Resp'], '_');
      expect(r['Log'], '[drawn(0, card(bob, input)) | _]');
    });

    test('a refused grant leaves its question open, and the next grant that '
        'forms a term of its type answers it', () async {
      final r =
          await _play('fragments', 'play_card_refused_then_yes(Resp, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      // maybe answered nothing; yes, the next grant, answered the card.
      expect(r['Resp'], 'accept(bob)');
      expect(r['Log'], '[drawn(0, card(bob, input)), withdrawn(0)]');
    });
  });

  group('(v) a reader-mode stream question, and the agent\'s message clause',
      () {
    test('a stream question takes one element per submission, a submission '
        'of no element of its type taking none', () async {
      final r = await _play('stream', 'play_chat(Out, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Out'],
          '[msg(bob, "hello"), msg(bob, "world") | _]');
      // One construct, drawn once, an input box that stays open.
      expect(r['Log'], '[drawn(0, input_box(text)) | _]');
      expect(r['Log'], isNot(contains('withdrawn')));
    });

    test('a stream question takes two grants as two elements and stays open',
        () async {
      final r = await _play('stream', 'play_chat_two(Out, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Out'], '[msg(carol, "one"), msg(carol, "two") | _]');
      expect(r['Log'], '[drawn(0, input_box(text)) | _]');
    });

    test('the agent\'s message clause reads no request: it reduces on an '
        'arriving friend offer without waiting, the card of the offer is '
        'drawn, and the request form stays on screen', () async {
      final r = await _play('fragments', 'play_offer(Outs, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      final log = r['Log'];
      expect(
          log,
          contains('drawn(0, input_box(menu([form(post, [text]), '
              'button(quit)])), input)'));
      expect(log,
          contains('drawn(1, form(card, [shown, buttons([yes, no])]), '
              'card(carol, input))'));
      expect(log, isNot(contains('withdrawn')));
    });
  });

  group('the agent\'s question, a stream of requests asked once', () {
    test('the person posts a text, a post of a number refused on the way, and '
        'quits, all on one construct, drawn once and kept open', () async {
      final r = await _play('fragments', 'play_request(Outs, Log)');
      expect(r.status, isNot(ExecutionStatus.failed), reason: '${r.error}');
      expect(r['Outs'], '["hi"]');
      expect(r['Log'],
          '[drawn(0, input_box(menu([form(post, [text]), button(quit)]))) | _]');
    });
  });
}
