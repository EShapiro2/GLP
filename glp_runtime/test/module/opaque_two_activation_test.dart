/// Opaque activation at a template of two type parameters.
///
/// PROVISIONAL (Udi, 2026-10-15), pending GLP-Spec's final form:
/// opaque_type(P/N, M, Hs) gives the LIST of type identities, one per type
/// parameter of the template in declaration order, and find_type(P/N, Hs,
/// Type) takes it; a guest's exports record D<iota(T1), ..., iota(Tk)> and
/// each iota(Ti) (GLP-Spec appendix-guards, "Dynamic activation"; TGLP
/// modules.tex, Dynamic Activation, "Opaque types", for one parameter).
///
/// programs/tests/opaque_two/self.glp declares the host's template
/// procedure(D, T) t(PersonChannel(D)?, SgChannel(T)?); guest/ is its
/// instance at Colour and Note; host/ activates it, holding no declaration of
/// either, the channels it hands over at Opaque.
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/module_kernels.dart';
import 'package:glp_runtime/runtime/body_kernels.dart';
import 'package:glp_runtime/runtime/runtime.dart';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/artefact.dart';

const _root = '../programs/self.glp';
const _dir = '../programs/tests/opaque_two';

void main() {
  late ModuleTerm guest;
  late GlpEngine host;

  setUpAll(() {
    final engine = GlpEngine(rootSelfGlpPath: _root)
      ..loadProgram(File('$_dir/guest').absolute.path);
    guest = engine.appModule!;
    host = GlpEngine(rootSelfGlpPath: _root)
      ..loadProgram(File('$_dir/host').absolute.path);
  });

  GlpRuntime inHost() {
    final rt = GlpRuntime();
    registerModuleKernels(rt);
    const goalId = 9292;
    rt.setGoalModule(goalId, host.appModule!);
    rt.currentGoalId = goalId;
    return rt;
  }

  Term ref(String name, int arity) =>
      StructTerm('/', [ConstTerm(name), ConstTerm(arity)]);

  (BodyKernelResult, Object?) call(
      GlpRuntime rt, String kernel, List<Object?> inputs) {
    final (w, _) = rt.heap.allocateVariable();
    final result = rt.bodyKernels.lookup(kernel, inputs.length + 1)!(
        rt, [...inputs, VarRef(w)]);
    final v = rt.heap.getValue(w);
    return (result, v is ConstTerm ? v.value : v);
  }

  Term list(List<String> ids) {
    Term t = ConstTerm(nil);
    for (final id in ids.reversed) {
      t = StructTerm('.', [ConstTerm(id), t]);
    }
    return t;
  }

  List<Object?> values(GlpRuntime rt, Object? t) {
    final out = <Object?>[];
    var c = t;
    while (c is StructTerm && c.functor == '.') {
      final h = rt.heap.dereference(c.args[0]);
      out.add(h is ConstTerm ? h.value : h);
      c = rt.heap.dereference(c.args[1]);
    }
    return out;
  }

  test('the guest\'s exports record two identities, one per parameter, and '
      'the identity of the instance at both', () {
    final record = interfaceTypeIdentityTables(
      typeDefsText: (guest.artefact as Artefact).typeDefsText,
      exportDeclarationTexts:
          (guest.artefact as Artefact).exports.map((e) => e.declarationText),
    ).opaque['t/2'];
    expect(record, isNotNull);
    expect(record!.types, hasLength(2));
    expect(record.types[0], isNot(record.types[1]));
  });

  test('the host, holding the channels at Opaque, loads: the step back admits '
      'each at the position of its own parameter', () {
    expect(host.appModule, isNotNull);
  });

  test('opaque_type/3 gives the list of two; find_type/3 the identity of the '
      'opaque instance at them, which run/3 activates the guest under', () {
    final rt = inHost();
    final (r1, hs) = call(rt, '_opaque_type', [ref('t', 2), guest]);
    expect(r1, BodyKernelResult.success);
    final ids = values(rt, hs).cast<String>();
    expect(ids, hasLength(2));

    final (r2, t) =
        call(rt, '_find_type', [ref('tests/opaque_two:t', 2), list(ids)]);
    expect(r2, BodyKernelResult.success);

    final (w1, _) = rt.heap.allocateVariable();
    final (w2, _) = rt.heap.allocateVariable();
    final goal = StructTerm('t', [VarRef(w1), VarRef(w2)]);
    expect(rt.bodyKernels.lookup('_run', 3)!(rt, [goal, ConstTerm(t), guest]),
        BodyKernelResult.success);

    // The two identities in the other order name another instance, which
    // the guest's exports do not record.
    final (r3, swapped) = call(rt, '_find_type',
        [ref('tests/opaque_two:t', 2), list(ids.reversed.toList())]);
    expect(r3, BodyKernelResult.success);
    expect(swapped, isNot(t));
    expect(
        rt.bodyKernels.lookup('_run', 3)!(
            rt, [goal, ConstTerm(swapped), guest]),
        BodyKernelResult.abort);

    // One identity for a template of two is refused.
    expect(
        call(rt, '_find_type', [ref('tests/opaque_two:t', 2), list([ids[0]])])
            .$1,
        BodyKernelResult.abort);
  });
}
