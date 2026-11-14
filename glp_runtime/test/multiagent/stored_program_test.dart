/// The compiled program the store keeps (IGLP 61f3283, Implementation Notes,
/// "Persistence": "the compiled program it runs"; [StoredProgram]) runs as
/// the program the engine compiled: its code is the same code, and its module
/// value's type-identity tables, built again from the scope the store keeps,
/// are the tables the engine built --- the declared table `find_type/2`
/// reads, and the opaque instances of the parameterised declarations
/// `find_type/3` takes (TGLP modules.tex, Dynamic Activation) --- so that an
/// agent restarted under the version it terminated under, an update having
/// been refused (Termination and Restart), runs it whole.  The program is the
/// Grassroots App's, `programs/social/graph/core`, whose goals launch its
/// mini-apps through find_type/3.
@Timeout.none
library;

import 'dart:io';

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_identity.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/engine_v2/interp.dart'
    show codeArtefactFromProgram, codeImageFromProgram;
import 'package:glp_runtime/multiagent/stored_program.dart';
import 'package:glp_runtime/wire/artefact.dart' show Artefact;
import 'package:glp_runtime/wire/codec.dart' show bytesEqual;

void main() {
  test('the program the store keeps is the code and the tables the engine '
      'compiled', () {
    final engine =
        GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);
    engine.enableMadGLP(agentId: 'k');
    engine.loadProgram('../programs/social/graph/core');
    final code = codeArtefactFromProgram(engine.combinedProgram);
    final module = engine.appModule!;
    final tables = module.declaredTypes as TypeIdentityTables;

    final kept =
        StoredProgram.fromBytes(StoredProgram.compiled(code, module).toBytes());

    expect(bytesEqual(kept.code, code), isTrue);
    expect(
        bytesEqual(kept.identity,
            codeIdentityOf(codeImageFromProgram(engine.combinedProgram))),
        isTrue);
    final again = kept.module!;
    expect(again.name, module.name);
    expect(again.directory, module.directory);
    expect(
        bytesEqual((again.artefact as Artefact).toBytes(),
            (module.artefact as Artefact).toBytes()),
        isTrue);

    final keptTables = kept.tables!;
    expect(keptTables.declared, tables.declared);
    expect(keptTables.exported, tables.exported);
    expect(keptTables.parametric, tables.parametric);
    expect(keptTables.unresolved, tables.unresolved);
    expect(keptTables.opaque, tables.opaque);
    expect(tables.parametric, isNotEmpty);
    for (final key in tables.parametric) {
      final identity = tables.procedureIdentity(key);
      expect(identity, isNotNull, reason: key);
      expect(keptTables.procedureIdentity(key), identity, reason: key);
    }
    // find_type/3 at the identities of the mini-apps' types, as the
    // Grassroots App's goals take it: the root's templates miniapp/3 and
    // miniapp_person/5, each of two type parameters.
    for (final key in ['miniapp/3', 'miniapp_person/5']) {
      final template = tables.parametric.contains(key)
          ? key
          : tables.parametric.firstWhere((k) => k.endsWith(':$key'));
      const labels = ['h0', 'h1'];
      expect(keptTables.opaqueInstanceIdentity(template, labels),
          tables.opaqueInstanceIdentity(template, labels),
          reason: template);
    }
  }, timeout: const Timeout(Duration(minutes: 5)));
}
