/// A program outside the root is refused.
///
/// TGLP modules.tex, "Scope construction": "A device nominates one directory
/// as its root, and every compilation on that device is under that root.  A
/// program lies at or below the root, and the scope of each of its modules
/// runs from the root down to that module."  A program file or directory
/// outside programs/, the root, is refused, naming its path and the sentence,
/// and so is a boot source's scope asked for a file outside it; the same
/// program under the root loads and runs (GLP #3 Cowork, 2026-10-04 09:06 UTC,
/// "23:49").  Until 2026-10-04 such a program was compiled in a scope without
/// its own directory's self.glp and the root's exposes, discoverSelfChain
/// stopping at once outside the root, whose bound was a string prefix, so
/// that a sibling `programs_x` of the root lay inside it.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/module_hierarchy.dart';
import 'package:glp_runtime/runtime/scheduler.dart' show ExecutionStatus;
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;
final String _root = File(_rootSelf).parent.path;

GlpEngine _engine() => GlpEngine(rootSelfGlpPath: _rootSelf);

const String _file = 'procedure go(Constant).\ngo(ok).\n';
const String _dirSelf = 'exported procedure go(Constant).\ngo(ok).\n';

/// A program file and a program directory written under [base].
(String file, String dir) _programsUnder(Directory base) {
  final file = '${base.path}/m.glp';
  File(file).writeAsStringSync(_file);
  final dir = Directory('${base.path}/prog')..createSync();
  File('${dir.path}/self.glp').writeAsStringSync(_dirSelf);
  return (file, dir.path);
}

Matcher _refused(String path) => throwsA(predicate(
    (e) =>
        e is OutsideRootError &&
        e.path == path &&
        e.toString().contains('$path lies outside the root') &&
        e.toString().contains('A program lies at or below the root') &&
        e.toString().contains('"Scope construction"'),
    'refused as outside the root, naming $path and the sentence'));

void main() {
  late Directory outside;
  late Directory inside;
  late String outFile, outDir, inFile, inDir;

  setUpAll(() {
    outside = Directory.systemTemp.createTempSync('glp_outside_root_');
    (outFile, outDir) = _programsUnder(outside);
    inside = Directory('../programs/tests').createTempSync('glp_under_root_');
    (inFile, inDir) = _programsUnder(inside);
  });

  tearDownAll(() {
    outside.deleteSync(recursive: true);
    inside.deleteSync(recursive: true);
  });

  test('a program file outside the root is refused', () {
    expect(() => _engine().loadFile(outFile), _refused(outFile));
  });

  test('a program directory outside the root is refused', () {
    expect(() => _engine().loadProgram(outDir), _refused(outDir));
  });

  test('the scope of a boot file outside the root is refused', () {
    expect(() => _engine().scopeFor(outFile), _refused(outFile));
  });

  test('the same file under the root loads and runs', () async {
    final engine = _engine();
    expect(engine.loadFile(inFile), isTrue);
    final r = await engine.runGoal('go(X).');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['X'].toString(), 'Const(ok)');
  });

  test('the same directory under the root loads and runs', () async {
    final engine = _engine();
    expect(engine.loadProgram(inDir), isTrue);
    final r = await engine.runGoal('go(X).');
    expect(r.status, ExecutionStatus.succeeded, reason: '${r.error}');
    expect(r.bindings['X'].toString(), 'Const(ok)');
  });

  test('a path lies at or below the root by path segments, not by prefix', () {
    expect(liesAtOrBelow(_root, _root), isTrue);
    expect(liesAtOrBelow('$_root/tests/x.glp', _root), isTrue);
    expect(liesAtOrBelow('${_root}_x/x.glp', _root), isFalse);
    expect(liesAtOrBelow(File(_root).parent.path, _root), isFalse);
    expect(() => requireUnderRoot('${_root}_x/x.glp', _root),
        _refused('${_root}_x/x.glp'));
  });

  test('a sibling of the root whose name the root\'s begins is no part of it',
      () {
    // root `programs`, sibling `programs_x` with a self.glp at each level: the
    // chain of a module of the sibling under the root's bound holds none of
    // them (until 2026-10-04 it held both, the bound a string prefix).
    final t = Directory.systemTemp.createTempSync('glp_root_sibling_');
    try {
      final root = Directory('${t.path}/programs')..createSync();
      final sib = Directory('${t.path}/programs_x/a')
        ..createSync(recursive: true);
      File('${t.path}/programs_x/self.glp').writeAsStringSync('T ::= t.\n');
      File('${sib.path}/self.glp').writeAsStringSync('U ::= u.\n');
      final m = '${sib.path}/m.glp';
      File(m).writeAsStringSync(_file);
      expect(
          discoverSelfChain(
              targetFile: m, rootDir: sib.path, programsDir: root.path),
          isEmpty);
    } finally {
      t.deleteSync(recursive: true);
    }
  });
}
