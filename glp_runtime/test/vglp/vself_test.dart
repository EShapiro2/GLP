// glp_runtime/test/vglp/vself_test.dart
//
// A self.vglp in a directory with no self.glp.  Spec: vGLP, Section 6.3,
// sections/parts/canonical-compilation.tex: "the self.vglp of a directory
// holds the procedures, volitional or not, and the widget declarations shared
// by every module in that directory and below it, as its self.glp holds those
// of GLP."  vGLP Cowork, 2026-10-16 11:25 UTC, item 1, approved by Udi: "A
// self.vglp holding procedures in a directory with no self.glp: yes, as you
// recommend.  :emit writes it as that directory's self.glp, with the
// compiler's header, linked as N.glp is for N.vglp."
//
// Program: programs/tests/vglp/vself, a directory with a self.vglp holding
// the chore card, approve, and no self.glp, and below it family, whose two
// modules, parent and child, each call approve, run with a scripted person:
// in memory, and from the self.glp and modules :emit writes, in a copy.

import 'dart:io';

import 'package:path/path.dart' as ppath;
import 'package:test/test.dart';
import 'package:glp_runtime/compiler/program_linker.dart';
import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:glp_runtime/runtime/scheduler.dart';
import 'package:glp_runtime/runtime/terms.dart' as rt;
import 'package:glp_runtime/vglp/program_compilation.dart' show compiledHeader;

const _programs = '../programs';
final _root = File('$_programs/self.glp').absolute.path;
final _vself =
    ppath.normalize(Directory('$_programs/tests/vglp/vself').absolute.path);

/// A run of [goal] in the program at [dir]: its status and each binding as
/// GLP text, an unbound variable shown `_` and an open list's tail `| _`.
Future<(ExecutionStatus, Map<String, String>)> _play(
    String dir, String goal, {String? root}) async {
  final engine = GlpEngine(rootSelfGlpPath: root ?? _root)..loadProgram(dir);
  final result = await engine.runGoal(goal);
  String show(rt.Term? t) {
    if (t == null) return '_';
    if (t is rt.VarRef) {
      final d = engine.runtime.heap.dereference(t);
      return d is rt.VarRef ? '_' : show(d);
    }
    if (t is rt.ConstTerm) {
      return t.value == null || t.value == rt.nil ? '[]' : '${t.value}';
    }
    if (t is rt.StructTerm) {
      if (t.functor == '.' && t.args.length == 2) {
        final items = <String>[];
        rt.Term? cur = t;
        while (true) {
          final c = cur;
          if (c is rt.StructTerm && c.functor == '.' && c.args.length == 2) {
            items.add(show(c.args[0]));
            cur = c.args[1];
            continue;
          }
          if (c is rt.VarRef) {
            final d = engine.runtime.heap.dereference(c);
            if (d is rt.VarRef) return '[${items.join(', ')} | _]';
            cur = d;
            continue;
          }
          if (c is rt.ConstTerm && (c.value == null || c.value == rt.nil)) {
            return '[${items.join(', ')}]';
          }
          return '[${items.join(', ')} | ${show(c)}]';
        }
      }
      return '${t.functor}(${t.args.map(show).join(', ')})';
    }
    return '$t';
  }

  return (
    result.status,
    {for (final e in result.bindings.entries) e.key: show(e.value)}
  );
}

/// The family plays' expected outcome, both modules served by one dispatcher.
void _expectFamily(ExecutionStatus status, Map<String, String> b) {
  expect(status, isNot(ExecutionStatus.failed));
  expect(b['P'], '[done(dishes, yes), allowed(5)]');
  expect(b['C'], '[done(homework, no), wished(bike)]');
  for (final seen in [
    'chore(dishes)',
    'chore(homework)',
    'allowance',
    'wish',
    'withdrawn(1)',
    'withdrawn(2)',
    'withdrawn(3)',
    'withdrawn(4)',
  ]) {
    expect(b['L'], contains(seen));
  }
}

/// Copy the directory [from] to [to], recursively.
void _copy(String from, String to) {
  for (final e in Directory(from).listSync(recursive: true)) {
    final rel = ppath.relative(e.path, from: from);
    if (e is File) {
      (File(ppath.join(to, rel))..createSync(recursive: true))
          .writeAsStringSync(e.readAsStringSync());
    }
  }
}

void main() {
  group('a self.vglp holding a volitional procedure in a directory with no '
      'self.glp, and two modules below it calling it', () {
    test('the directory has a self.glp, compiled from its self.vglp in '
        'memory, a module of the program with the layer\'s procedures and '
        'types', () {
      expect(File('$_vself/self.glp').existsSync(), isFalse);
      final modules = discoverProgram('$_vself/family', rootSelfGlpPath: _root);
      expect(modules.any((m) => m.filePath.endsWith('self.vglp')), isFalse);
      final self = modules.singleWhere(
          (m) => ppath.normalize(m.filePath) == ppath.join(_vself, 'self.glp'));
      expect(self.isSelfGlp, isTrue);
      expect(self.ast.procedures.map((p) => p.signature),
          containsAll(['approve/3', 'approve1/4']));
      expect(self.ast.typeDefs.map((t) => t.name),
          containsAll(['Chore', 'Done', 'YesNo']));
    });

    test('both modules at once, in memory, their questions served by one '
        'dispatcher', () async {
      final (status, b) =
          await _play('$_vself/family', 'play_family(P, C, L)');
      _expectFamily(status, b);
    });

    group('emitted, in a copy', () {
      late String copy;
      setUp(() {
        copy = ppath.normalize(Directory('$_programs/tests/vglp')
            .createTempSync('vself_emit_')
            .absolute
            .path);
        _copy(_vself, copy);
      });
      tearDown(() {
        if (Directory(copy).existsSync()) {
          Directory(copy).deleteSync(recursive: true);
        }
      });

      test(':emit writes the self.vglp as the directory\'s self.glp, with the '
          'compiler\'s header, and the program runs from what it wrote, the '
          'same; a second :emit writes the same bytes', () async {
        final written = emitVglpSources(copy, rootSelfGlpPath: _root);
        final self = File('$copy/self.glp');
        expect(written.map(ppath.normalize),
            containsAll([
              ppath.join(copy, 'self.glp'),
              ppath.join(copy, 'family', 'parent.glp'),
              ppath.join(copy, 'family', 'child.glp'),
            ]));
        expect(self.existsSync(), isTrue);
        final text = self.readAsStringSync();
        expect(text.startsWith(compiledHeader), isTrue);
        expect(text, contains('approve('));
        // The hand-written self.glp of family is not touched.
        expect(File('$copy/family/self.glp').readAsStringSync(),
            File('$_vself/family/self.glp').readAsStringSync());

        // Loaded from disc: the self.glp written is the directory's own, its
        // self.vglp a source still, and the compiled modules stand for theirs.
        final modules = discoverProgram('$copy/family', rootSelfGlpPath: _root);
        final selfModule = modules.singleWhere(
            (m) => ppath.normalize(m.filePath) == ppath.join(copy, 'self.glp'));
        final inMemory = discoverProgram('$_vself/family',
                rootSelfGlpPath: _root)
            .singleWhere((m) =>
                ppath.normalize(m.filePath) == ppath.join(_vself, 'self.glp'));
        expect(selfModule.ast.procedures.map((p) => p.signature).toSet(),
            inMemory.ast.procedures.map((p) => p.signature).toSet());
        expect(selfModule.volitional.keys, inMemory.volitional.keys);
        final (status, b) = await _play('$copy/family', 'play_family(P, C, L)');
        _expectFamily(status, b);

        final before = {
          for (final p in written) p: File(p).readAsStringSync(),
        };
        final again = emitVglpSources(copy, rootSelfGlpPath: _root);
        expect(again.map(ppath.normalize).toSet(),
            written.map(ppath.normalize).toSet());
        for (final p in again) {
          expect(File(p).readAsStringSync(), before[p], reason: p);
        }
      });
    });
  });

  group('a call N # p into a directory whose self.glp is its self.vglp', () {
    late Directory fixture;
    setUp(() {
      fixture = Directory('$_programs/tests/vglp').createTempSync('vself_np_');
      Directory('${fixture.path}/sub').createSync();
      File('${fixture.path}/self.glp').writeAsStringSync('''
imported procedure sub#hello(Constant).
exported procedure go(Constant).
go(X?) :- sub # hello(X).
''');
      File('${fixture.path}/sub/self.vglp').writeAsStringSync('''
exported procedure hello(Constant).
hello(hi).
''');
    });
    tearDown(() {
      if (fixture.existsSync()) fixture.deleteSync(recursive: true);
    });

    test('resolves to the procedure that self.glp exports, in memory and '
        'from what :emit writes', () async {
      final (s1, b1) = await _play(fixture.path, 'go(X)');
      expect(s1, isNot(ExecutionStatus.failed));
      expect(b1['X'], 'hi');
      emitVglpSources(fixture.path, rootSelfGlpPath: _root);
      final written = File('${fixture.path}/sub/self.glp');
      expect(written.readAsStringSync().startsWith(compiledHeader), isTrue);
      final (s2, b2) = await _play(fixture.path, 'go(X)');
      expect(s2, isNot(ExecutionStatus.failed));
      expect(b2['X'], 'hi');
    });
  });
}
