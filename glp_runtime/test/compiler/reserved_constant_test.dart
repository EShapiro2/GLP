// glp_runtime/test/compiler/reserved_constant_test.dart
//
// Tests for reserved constant validation.
// Verifies that:
//   - Constants starting with '_' are rejected in user mode (default)
//   - Constants starting with '_' are allowed in system mode (-mode(system).)
//   - Regular constants work in both modes
//
// Spec: docs/typed-glp-manual.md Section 12 (Reserved Constants)
// Spec: docs/ma/madGLP-spec.md Section 15 (Reserved Constants)

import 'package:test/test.dart';
import 'package:glp_runtime/compiler/compiler.dart';
import 'package:glp_runtime/compiler/error.dart';

/// Helper to compile GLP source
void compile(String source) {
  GlpCompiler().compile(source);
}

void main() {
  group('Reserved constant validation', () {
    test('rejects quoted underscore constant in user mode (default)', () {
      const source = '''
        procedure foo(_).
        foo('_bar').
      ''';
      expect(
        () => compile(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains("reserved for system use"),
        )),
      );
    });

    test('rejects underscore constant in structure in user mode', () {
      const source = '''
        procedure foo(_).
        foo(msg('_user', alice, connect(bob))).
      ''';
      expect(
        () => compile(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains("reserved for system use"),
        )),
      );
    });

    test('allows underscore constant in system mode', () {
      const source = '''
        -mode(system).

        procedure foo(_).
        foo('_bar').
      ''';
      // Should not throw
      expect(() => compile(source), returnsNormally);
    });

    test('allows underscore constant in structure in system mode', () {
      const source = '''
        -mode(system).

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

    test('explicit user mode still rejects underscore constants', () {
      const source = '''
        -mode(user).

        procedure foo(_).
        foo('_reserved').
      ''';
      expect(
        () => compile(source),
        throwsA(isA<CompileError>().having(
          (e) => e.message,
          'message',
          contains("reserved for system use"),
        )),
      );
    });
  });
}
