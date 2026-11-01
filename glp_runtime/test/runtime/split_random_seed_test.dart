/// Tests for the body kernel `'_split_random_seed'`/3, behind the system
/// predicate split_random_seed/3.
///
/// Spec: GLP-ICLP appendix-guards §"Splitting a seed" — "split_random_seed(Seed,
/// S1, S2) assigns S1 and S2 two seeds determined by Seed, such that the
/// sequences of draws from Seed, from S1 and from S2 are independent streams;
/// a splittable generator, such as SplitMix, implements it", and "a
/// computation that draws from several seeds made by splitting one seed
/// reproduces exactly from that seed".
///
/// The covering test of GLP #11's task (Code_inbox 2026-10-31 15:20 UTC): from
/// one seed, the three streams of random/4 draws from Seed, S1 and S2 differ
/// from one another and from any shift of one another over the first hundred
/// draws, and a second run from the same seed reproduces them.
library;

import 'package:test/test.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart' show HeapCell;

const int _draws = 100;

/// The bound of each draw: large, so that two streams agree at a draw only
/// with a chance of one in about 2^31.
const int _bound = 2147483646;

int _out(GlpRuntime rt, HeapCell writer) {
  final v = rt.heap.getValue(writer);
  expect(v, isA<ConstTerm>());
  return (v as ConstTerm).value as int;
}

/// '_split_random_seed'(Seed, S1, S2), run in a fresh runtime.
(int, int) _split(int seed) {
  final rt = GlpRuntime();
  final kernel = rt.bodyKernels.lookup('_split_random_seed', 3);
  expect(kernel, isNotNull,
      reason: '_split_random_seed/3 kernel should be registered');
  final (w1, _) = rt.heap.allocateVariable();
  final (w2, _) = rt.heap.allocateVariable();
  final r = kernel!(rt, [ConstTerm(seed), VarRef(w1), VarRef(w2)]);
  expect(r, equals(BodyKernelResult.success));
  return (_out(rt, w1), _out(rt, w2));
}

/// The first [_draws] values of random/4 from [seed], threading NextSeed.
List<int> _stream(int seed) {
  final rt = GlpRuntime();
  final kernel = rt.bodyKernels.lookup('_random', 4)!;
  final values = <int>[];
  var s = seed;
  for (var i = 0; i < _draws; i++) {
    final (wv, _) = rt.heap.allocateVariable();
    final (ws, _) = rt.heap.allocateVariable();
    final r = kernel(rt, [ConstTerm(s), ConstTerm(_bound), VarRef(wv), VarRef(ws)]);
    expect(r, equals(BodyKernelResult.success));
    values.add(_out(rt, wv));
    s = _out(rt, ws);
  }
  return values;
}

/// Whether [b] read from position [k] on agrees with [a] from its start, over
/// their overlap: [b] is [a] shifted by k.
bool _shiftedBy(List<int> a, List<int> b, int k) {
  for (var i = 0; i + k < b.length && i < a.length; i++) {
    if (a[i] != b[i + k]) return false;
  }
  return true;
}

void main() {
  group('split_random_seed/3 kernel', () {
    for (final seed in [42, 1, 0, -7, 2147483646, 9007199254740993]) {
      test('Seed $seed: three streams pairwise unshifted, reproducible', () {
        final (s1, s2) = _split(seed);
        expect({seed, s1, s2}.length, equals(3),
            reason: 'S1 and S2 are seeds apart from Seed and each other');

        final streams = {'Seed': _stream(seed), 'S1': _stream(s1), 'S2': _stream(s2)};
        for (final a in streams.keys) {
          for (final b in streams.keys) {
            if (a == b) continue;
            // k = 0 is "differ from one another"; k > 0, with both orders of
            // the pair, is "from any shift of one another".
            for (var k = 0; k < _draws; k++) {
              expect(_shiftedBy(streams[a]!, streams[b]!, k), isFalse,
                  reason: '$b is $a shifted by $k over the first $_draws draws');
            }
          }
        }

        // A second run from the same seed reproduces them.
        final (t1, t2) = _split(seed);
        expect((t1, t2), equals((s1, s2)));
        expect(_stream(seed), equals(streams['Seed']));
        expect(_stream(t1), equals(streams['S1']));
        expect(_stream(t2), equals(streams['S2']));
      });
    }

    test('control: the stream from the next seed is the stream shifted by 1',
        () {
      final a = _stream(42);
      final rt = GlpRuntime();
      final (wv, _) = rt.heap.allocateVariable();
      final (ws, _) = rt.heap.allocateVariable();
      rt.bodyKernels.lookup('_random', 4)!(
          rt, [ConstTerm(42), ConstTerm(_bound), VarRef(wv), VarRef(ws)]);
      final b = _stream(_out(rt, ws));
      expect(_shiftedBy(b, a, 1), isTrue);
      expect(_shiftedBy(b, a, 0), isFalse);
    });

    test('a non-integer Seed aborts', () {
      final rt = GlpRuntime();
      final kernel = rt.bodyKernels.lookup('_split_random_seed', 3)!;
      final (w1, _) = rt.heap.allocateVariable();
      final (w2, _) = rt.heap.allocateVariable();
      expect(kernel(rt, [ConstTerm('a'), VarRef(w1), VarRef(w2)]),
          equals(BodyKernelResult.abort));
    });
  });
}
