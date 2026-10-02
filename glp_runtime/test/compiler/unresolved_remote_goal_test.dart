// glp_runtime/test/compiler/unresolved_remote_goal_test.dart
//
// A cross-module call M # G becomes a local call when its program is linked
// (TGLP modules.tex, Compilation, fourth step), so the code generator never
// meets one in a linked program.  A source compiled directly, never linked,
// carries it there unresolved, and the generator refuses it.  Until 2026-10-02
// it compiled such a call to the distribute instruction of the dynamic-dispatch
// mechanism (TGLP modules.tex, Implementation), which no artefact can carry,
// and the program failed only when it was encoded.

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/error.dart';

void main() {
  Matcher refusedUnresolved(String call) => throwsA(predicate(
      (e) =>
          e is CompileError &&
          e.message.contains('"$call" reached the code generator unresolved'),
      'a CompileError refusing the unresolved call $call'));

  test('an unresolved cross-module call is refused by the code generator', () {
    expect(
        () => GlpCompiler().compile('''
boot :- otherwise |
    math # factorial(5, R),
    io # print(R?).
'''),
        refusedUnresolved('math # factorial(5, R)'));
  });

  test('the same program without the qualifier compiles', () {
    expect(
        GlpCompiler()
            .compile('''
boot :- otherwise |
    factorial(5, R),
    print(R?).
''')
            .labels
            .keys,
        contains('boot/0'));
  });
}
