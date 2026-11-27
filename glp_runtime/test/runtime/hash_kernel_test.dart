/// Tests for the body kernel `'_hash'`/2, behind the kernel predicate hash/2:
/// H is the SHA-256 of the UTF-8 bytes of the string S, lowercase hex, the
/// catalogue's Hash (programs/self.glp, `Hash ::= String`), the same at every
/// participant, so that a value sealed by the hash of a text naming it and a
/// secret is checked later against the seal (GLP-Spec
/// appendix-guards, "Text").
library;

import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:test/test.dart';

/// '_hash'/2 on [input], in a fresh runtime: its result and the hash it
/// assigns, null where it aborted.
(BodyKernelResult, String?) _hash(Term input) {
  final rt = GlpRuntime();
  final kernel = rt.bodyKernels.lookup('_hash', 2);
  expect(kernel, isNotNull, reason: "'_hash'/2 should be registered");
  final (w, _) = rt.heap.allocateVariable();
  final r = kernel!(rt, [input, VarRef(w)]);
  if (r != BodyKernelResult.success) return (r, null);
  final v = rt.heap.getValue(w);
  expect(v, isA<ConstTerm>());
  return (r, (v as ConstTerm).value as String);
}

void main() {
  group("'_hash'/2", () {
    test('the SHA-256 of "abc", the standard test vector', () {
      final (r, h) = _hash(ConstTerm('abc'));
      expect(r, BodyKernelResult.success);
      expect(h, 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
    });
    test('the SHA-256 of the empty string', () {
      final (r, h) = _hash(ConstTerm(''));
      expect(r, BodyKernelResult.success);
      expect(h, 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855');
    });
    test('of UTF-8 bytes: the same text, the same hash; another, another', () {
      final (_, a) = _hash(ConstTerm('3,8231'));
      final (_, b) = _hash(ConstTerm('3,8231'));
      final (_, c) = _hash(ConstTerm('4,8231'));
      expect(a, b);
      expect(a, isNot(c));
      expect(a!.length, 64);
      expect(RegExp(r'^[0-9a-f]{64}$').hasMatch(a), isTrue);
    });
    test('a first argument that is no string aborts', () {
      final (r, h) = _hash(ConstTerm(7));
      expect(r, isNot(BodyKernelResult.success));
      expect(h, isNull);
    });
  });
}
