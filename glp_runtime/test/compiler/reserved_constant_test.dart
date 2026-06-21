// glp_runtime/test/compiler/reserved_constant_test.dart
//
// Tests for reserved constant validation.
// A constant is reserved iff it NAMES a language primitive (kernel predicate /
// reserved functor — e.g. '_output', '_send', '_w'). A '_'-prefixed constant
// that names no primitive (e.g. '_user', '_net', '_bar') is NOT reserved.
// Verifies that:
//   - Constants naming a primitive are rejected in user mode (default)
//   - Constants naming a primitive are allowed in system mode (-mode(system).)
//   - '_'-prefixed constants naming no primitive are allowed in user mode
//   - Regular constants work in both modes
//
// Spec: TGLP appendix-root-self.tex, "Admission to the Primitive Layer".

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/error.dart';

/// Helper to compile GLP source
void compile(String source) {
  GlpCompiler().compile(source);
}

void main() {
  group('Reserved constant validation', () {
    test('rejects a constant naming a primitive in user mode (default)', () {
      const source = '''
        procedure foo(_).
        foo('_output').
      ''';
      expect(
        () => compile(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains("names a language primitive"),
        )),
      );
    });

    test('rejects a primitive-naming constant in a structure in user mode', () {
      const source = '''
        procedure foo(_).
        foo(msg('_send', alice, connect(bob))).
      ''';
      expect(
        () => compile(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains("names a language primitive"),
        )),
      );
    });

    test('allows a primitive-naming constant in system mode', () {
      const source = '''
        -mode(system).

        procedure foo(_).
        foo('_output').
      ''';
      // Should not throw
      expect(() => compile(source), returnsNormally);
    });

    test('allows a primitive-naming constant in a structure in system mode', () {
      const source = '''
        -mode(system).

        procedure foo(_).
        foo(msg('_send', alice, connect(bob))).
      ''';
      // Should not throw
      expect(() => compile(source), returnsNormally);
    });

    test('allows a _-prefixed constant naming no primitive in user mode', () {
      // '_user'/'_net' name no primitive: not reserved, even with the prefix.
      const source = '''
        procedure foo(_).
        foo(msg('_user', alice, connect(bob))).
      ''';
      // Should not throw
      expect(() => compile(source), returnsNormally);
    });

    test('allows regular atoms in user mode', () {
      const source = '''
        procedure foo(_).
        foo(bar).
      ''';
      // Should not throw
      expect(() => compile(source), returnsNormally);
    });

    test('allows regular quoted atoms in user mode', () {
      const source = '''
        procedure foo(_).
        foo('hello world').
      ''';
      // Should not throw
      expect(() => compile(source), returnsNormally);
    });

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
      // Should not throw
      expect(() => compile(source), returnsNormally);
    });

    test('explicit user mode still rejects a primitive-naming constant', () {
      const source = '''
        -mode(user).

        procedure foo(_).
        foo('_sign').
      ''';
      expect(
        () => compile(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains("names a language primitive"),
        )),
      );
    });
  });
}
