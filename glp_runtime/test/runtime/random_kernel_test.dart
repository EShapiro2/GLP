/// Tests for the body kernel `'_random'`/4, behind the system predicate
/// random/4.
///
/// Spec: GLP-ICLP appendix-guards, "Seeded random numbers" (GLP-ICLP-2026
/// 2fc437f): the generator is SplitMix64, in 64-bit arithmetic modulo 2^64; a
/// seed is a 64-bit integer and every 64-bit integer is a seed; NextSeed is
/// Seed plus the increment 0x9E3779B97F4A7C15; Value is one plus the mix of
/// NextSeed, taken as an unsigned 64-bit integer, modulo N; the mix is the
/// source's mix64 (Steele, Lea and Flood 2014, Figure 16).
///
/// The covering test of GLP #11's task (Code_inbox 2026-11-01 07:26 UTC):
/// random/4 from seed 0, from 2147483647 and from -1 each gives draws in
/// [1, N] over a hundred steps, and the three streams differ.
///
/// The pinned values were computed from the paper's sentences in Python's
/// unbounded integers reduced modulo 2^64, independently of this code.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;

int _out(GlpRuntime rt, HeapCell writer) {
  final v = rt.heap.getValue(writer);
  expect(v, isA<ConstTerm>());
  return (v as ConstTerm).value as int;
}

/// One draw: '_random'(Seed, N, Value, NextSeed), giving (Value, NextSeed).
(int, int) _draw(GlpRuntime rt, int seed, int n) {
  final kernel = rt.bodyKernels.lookup('_random', 4);
  expect(kernel, isNotNull, reason: '_random/4 kernel should be registered');
  final (wv, _) = rt.heap.allocateVariable();
  final (ws, _) = rt.heap.allocateVariable();
  final r = kernel!(rt, [ConstTerm(seed), ConstTerm(n), VarRef(wv), VarRef(ws)]);
  expect(r, equals(BodyKernelResult.success));
  return (_out(rt, wv), _out(rt, ws));
}

/// The first [k] values of random/4 from [seed] with bound [n], threading
/// NextSeed.
List<int> _stream(int seed, int n, int k) {
  final rt = GlpRuntime();
  final values = <int>[];
  var s = seed;
  for (var i = 0; i < k; i++) {
    final (v, next) = _draw(rt, s, n);
    values.add(v);
    s = next;
  }
  return values;
}

void main() {
  group('random/4 kernel (SplitMix64)', () {
    const seeds = [0, 2147483647, -1];

    for (final n in [1, 6, 1000, 9223372036854775807]) {
      test('N = $n: draws from 0, 2147483647 and -1 are in [1, N] over a '
          'hundred steps', () {
        for (final seed in seeds) {
          for (final v in _stream(seed, n, 100)) {
            expect(v, inInclusiveRange(1, n), reason: 'seed $seed');
          }
        }
      });
    }

    test('the streams from 0, 2147483647 and -1 differ', () {
      final streams = [for (final s in seeds) _stream(s, 1000, 100)];
      expect(streams[0], isNot(equals(streams[1])));
      expect(streams[0], isNot(equals(streams[2])));
      expect(streams[1], isNot(equals(streams[2])));
    });

    test('each stream from 0, 2147483647 and -1 is the reference stream', () {
      const first5 = {
        0: [555, 960, 228, 535, 672],
        2147483647: [53, 114, 750, 739, 71],
        -1: [249, 389, 283, 303, 712],
      };
      const sums = {0: 52306, 2147483647: 52161, -1: 52301};
      for (final seed in seeds) {
        final s = _stream(seed, 1000, 100);
        expect(s.sublist(0, 5), equals(first5[seed]), reason: 'seed $seed');
        expect(s.fold<int>(0, (a, b) => a + b), equals(sums[seed]),
            reason: 'seed $seed');
      }
      expect(_stream(0, 6, 5), equals([3, 6, 4, 5, 4]));
      expect(_stream(2147483647, 6, 5), equals([5, 6, 6, 5, 5]));
      expect(_stream(-1, 6, 5), equals([3, 5, 5, 3, 4]));
    });

    test('NextSeed is Seed plus the increment, modulo 2^64', () {
      final rt = GlpRuntime();
      expect(_draw(rt, 0, 6).$2, equals(-7046029254386353131));
      expect(_draw(rt, -1, 6).$2, equals(-7046029254386353132));
      expect(_draw(rt, 2147483647, 6).$2, equals(-7046029252238869484));
      expect(_draw(rt, 42, 6), equals((4, -7046029254386353089)));
    });

    test('Value takes the mix as unsigned: bounds above 2^62', () {
      final rt = GlpRuntime();
      const cases = <(int, int, int)>[
        (0, 9223372036854775807, 2062761817371520748),
        (0, 6917529027641081856, 4368604826585214699),
        (0, 4611686018427387905, 2062761817371520745),
        (-1, 9223372036854775807, 2717770620714153249),
        (2147483647, 9223372036854775807, 3892878248730454246),
        (2147483647, 6917529027641081856, 6198721257944148197),
        (2147483647, 4611686018427387905, 3892878248730454243),
        (42, 9223372036854775807, 3250621881602310088),
      ];
      for (final (seed, n, value) in cases) {
        expect(_draw(rt, seed, n).$1, equals(value), reason: 'seed $seed, N $n');
      }
    });

    test('split_random_seed/3: S1 and S2 are the mixes of Seed plus once and '
        'twice the increment', () {
      final rt = GlpRuntime();
      final kernel = rt.bodyKernels.lookup('_split_random_seed', 3)!;
      const cases = <(int, int, int)>[
        (42, 3250621881602310087, 540350159304224772),
        (0, -7160610219483255062, -3239489724241199657),
        (-1, 2717770620714153248, 5036830254254310388),
      ];
      for (final (seed, s1, s2) in cases) {
        final (w1, _) = rt.heap.allocateVariable();
        final (w2, _) = rt.heap.allocateVariable();
        expect(kernel(rt, [ConstTerm(seed), VarRef(w1), VarRef(w2)]),
            equals(BodyKernelResult.success));
        expect((_out(rt, w1), _out(rt, w2)), equals((s1, s2)),
            reason: 'seed $seed');
      }
    });

    test('N < 1 aborts', () {
      final rt = GlpRuntime();
      final kernel = rt.bodyKernels.lookup('_random', 4)!;
      final (wv, _) = rt.heap.allocateVariable();
      final (ws, _) = rt.heap.allocateVariable();
      expect(kernel(rt, [ConstTerm(42), ConstTerm(0), VarRef(wv), VarRef(ws)]),
          equals(BodyKernelResult.abort));
    });
  });
}
