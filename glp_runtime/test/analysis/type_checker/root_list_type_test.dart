// glp_runtime/test/analysis/type_checker/root_list_type_test.dart
//
// List(X) in the root self.glp, beside Stream(X): `List(X) ::= [] ; [X |
// List(X)].`  vGLP's Definition "Volitional Procedure" (vGLP
// sections/vglp.tex) names Stream and List as the recursive types an
// interactive type may hold, and sGLP (2026-10-11 21:39 UTC) found List
// defined nowhere, so vGLP's flat interactive types (List(Peer),
// List(Holding)) did not type-check.  Udi approved the root line on
// 2026-10-11 22:50 UTC (vGLP to Integration, 22:51 UTC); TGLP's
// appendix-root-self.tex, which names the root's templates, is GLP's to
// follow.  The covering test the request names: sGLP's menu(List(Peer),
// List(Peer), Choice?) type-checks; [] and [a, b] check against List(String);
// a Stream(X)-typed program is unaffected.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

GlpEngine _engine() =>
    GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);

String _refusal(String source, String name) {
  try {
    _engine().loadSource(source, filename: name);
  } catch (e) {
    return '$e';
  }
  return '';
}

void main() {
  test('the root self.glp defines List(X) beside Stream(X)', () {
    final root = File('../programs/self.glp').readAsStringSync();
    expect(root, contains('Stream(X) ::= [] ; [X | Stream(X)].\n'));
    expect(root, contains('List(X) ::= [] ; [X | List(X)].\n'));
  });

  test("sGLP's menu(List(Peer), List(Peer), Choice?) type-checks", () {
    // graph.glp's ask1/5, its ask stream left out, Peers written List(Peer).
    expect(
        _engine().loadSource('''
Peer   ::= Constant.
Choice ::= same(Peer) ; other(Peer).
Menu   ::= menu(List(Peer), List(Peer), Choice?).

procedure ask1(List(Peer)?, List(Peer)?, Choice, Menu).
ask1(Ss, Os, C?, menu(Ss?, Os?, C)).
''', filename: 'list_menu.glp'),
        isTrue);
  });

  test('and through a named type whose body is List(Peer)', () {
    expect(
        _engine().loadSource('''
Peer   ::= Constant.
Peers  ::= List(Peer).
Choice ::= same(Peer) ; other(Peer).
Menu   ::= menu(Peers, Peers, Choice?).

procedure ask1(Peers?, Peers?, Choice, Menu).
ask1(Ss, Os, C?, menu(Ss?, Os?, C)).
''', filename: 'peers_menu.glp'),
        isTrue);
  });

  test('[] checks against List(String)', () {
    expect(
        _engine().loadSource('''
procedure none(List(String)).
none([]).
''', filename: 'list_empty.glp'),
        isTrue);
  });

  test('[a, b] checks against List(String)', () {
    expect(
        _engine().loadSource('''
procedure two(List(String)).
two([a, b]).
''', filename: 'list_two.glp'),
        isTrue);
  });

  test('the control: [1, 2] is refused against List(String)', () {
    expect(_refusal('''
procedure two(List(String)).
two([1, 2]).
''', 'list_integers.glp'), isNotEmpty);
  });

  test('a Stream(X)-typed program is unaffected', () async {
    final engine = _engine();
    expect(
        engine.loadSource('''
procedure count(Stream(Integer)?, Integer).
count([], 0).
count([_ | Xs], N?) :- count(Xs?, N1), N := N1? + 1.

procedure go(Integer).
go(N?) :- merge([1, 2], [3], Xs), count(Xs?, N).
''', filename: 'stream_count.glp'),
        isTrue);
    final r = await engine.runGoal('go(N)');
    expect(r.error, isNull);
    expect((r.bindings['N'] as ConstTerm).value, 3);
  });
}
