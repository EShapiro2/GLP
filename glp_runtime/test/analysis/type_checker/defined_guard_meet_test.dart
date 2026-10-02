// glp_runtime/test/analysis/type_checker/defined_guard_meet_test.dart
//
// A defined guard's argument is checked as a built-in guard's is.
// Spec: TGLP sections/typed-glp.tex, "Type checking of guards": "Let S be the
// type of the occurrence and T the type declared for the position it occupies
// in the guard.  The guard atom is well-typed if the meet of S and T ... is
// non-empty."  A defined guard (GLP-Spec appendix-guards.tex, "Defined guard
// predicates") is unfolded by the partial evaluator before the clause is
// checked, so the checker asks it of the clause as written.  Until 2026-10-02
// the fixture below was refused only for the head its unfolding wrote,
// p(ch([], [])), and a defined guard over a type with no term in common with
// the occurrence's was otherwise not refused for it.  GLP's task of
// 2026-10-01 23:58 UTC, item 5; the fixture is its clause.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
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
  test('close(A?) with A at Request? is refused against close/1', () {
    final e = _refusal('''
Request ::= req(Constant) ; quit.

procedure p(Request?).
p(A) :- close(A?) | true.
''', 'close_request.glp');
    expect(e, contains('Guard close tests A? at Channel<Closed,Closed>?'));
    expect(e, contains('the meet is empty'));
  });

  test('the control: close(A?) with A at the channel type loads', () {
    expect(
        _engine().loadSource('''
procedure p(Channel(Closed, Closed)?).
p(A) :- close(A?) | true.
''', filename: 'close_channel.glp'),
        isTrue);
  });

  test('a program\'s own defined guard over a disjoint type is refused', () {
    final e = _refusal('''
procedure is_mod(Module?).
is_mod(_).

procedure p(String?).
p(M) :- is_mod(M?) | true.
''', 'is_mod_string.glp');
    expect(e, contains('Guard is_mod tests M? at Module?'));
    expect(e, contains('the meet is empty'));
  });

  test('and over a type that holds a module it loads', () {
    expect(
        _engine().loadSource('''
Content ::= String ; Module.

procedure is_mod(Module?).
is_mod(_).

procedure p(Content?).
p(M) :- is_mod(M?) | true.
''', filename: 'is_mod_content.glp'),
        isTrue);
  });
}
