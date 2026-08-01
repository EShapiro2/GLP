// glp_runtime/test/compiler/reserved_constant_test.dart
//
// Underscore-prefixed constants in DATA position, through the compiler's own
// path (the loader path is primitive_layer_test.dart).
//
// Until 2026-07-31 the analyzer rejected a constant here if it named a
// registered primitive, and this file asserted those rejections. GLP-Spec
// narrowed the rule that day, with Udi's approval at each step: the restriction
// is on names in CALL POSITION only, and a constant with the prefix is
// unrestricted as data — as a message tag, or as a member of a type union. The
// prefix-only-but-everywhere rule forbids '_net', which 242 green code lines
// name as data and which is not being renamed.
//
// So every case below is accepted, whatever the constant names, and in either
// mode. What is still rejected — defining or calling a procedure so named —
// belongs to the loader and is tested in primitive_layer_test.dart.
//
// Spec: TGLP appendix-root-self.tex, "Admission to the Primitive Layer";
// GLP-Spec appendix-guards.tex, "Naming and admission of body kernels".

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/error.dart';

/// Helper to compile GLP source
void compile(String source) {
  GlpCompiler().compile(source);
}

void main() {
  group('Underscore-prefixed constants as data', () {
    test('accepts a constant naming a kernel, in user mode (default)', () {
      const source = '''
        procedure foo(_).
        foo('_output').
      ''';
      expect(() => compile(source), returnsNormally);
    });

    test('accepts one inside a structure, in user mode', () {
      const source = '''
        procedure foo(_).
        foo(msg('_send', alice, connect(bob))).
      ''';
      expect(() => compile(source), returnsNormally);
    });

    test('accepts a kernel-naming constant in system mode', () {
      const source = '''
        -mode(system).

        procedure foo(_).
        foo('_output').
      ''';
      expect(() => compile(source), returnsNormally);
    });

    test('accepts one inside a structure, in system mode', () {
      const source = '''
        -mode(system).

        procedure foo(_).
        foo(msg('_send', alice, connect(bob))).
      ''';
      expect(() => compile(source), returnsNormally);
    });

    test('accepts a _-prefixed constant naming no kernel, in user mode', () {
      // The case that forced the narrowing: '_user' and '_net' are ordinary
      // message tags in sixty-one application modules.
      const source = '''
        procedure foo(_).
        foo(msg('_user', alice, connect(bob))).
      ''';
      expect(() => compile(source), returnsNormally);
    });

    test('accepts regular atoms in user mode', () {
      const source = '''
        procedure foo(_).
        foo(bar).
      ''';
      expect(() => compile(source), returnsNormally);
    });

    test('accepts regular quoted atoms in user mode', () {
      const source = '''
        procedure foo(_).
        foo('hello world').
      ''';
      expect(() => compile(source), returnsNormally);
    });

    test('explicit user mode accepts a kernel-naming constant as data', () {
      const source = '''
        -mode(user).

        procedure foo(_).
        foo('_sign').
      ''';
      expect(() => compile(source), returnsNormally);
    });
  });

  group('-mode directive', () {
    test('rejects -mode with invalid argument', () {
      const source = '''
        -mode(invalid).

        procedure foo(_).
        foo(bar).
      ''';
      expect(
        () => compile(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains('Invalid mode'),
        )),
      );
    });

    test('allows explicit user mode', () {
      const source = '''
        -mode(user).

        procedure foo(_).
        foo(bar).
      ''';
      expect(() => compile(source), returnsNormally);
    });
  });
}
