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

/// A root of its own in a system temporary directory, named by [prefix]: the
/// path of its programs/, which links every entry of the root programs/ but
/// tests/.  A program written below its tests/ lies under a root (TGLP
/// modules.tex, "Scope construction", which program_linker.dart's
/// requireUnderRoot holds) and has the scope it has below programs/: the root
/// self.glp, the modules the root exposes, and the dispatcher's source in
/// vglp/; and none of its files is under programs/, where another test file
/// running in parallel collects every .vglp when it loads (canonical_test.dart,
/// "an old source with no mediator ... is refused").  Until 2026-10-20 the
/// copies were made in programs/tests/vglp, and that test met one as its
/// tearDown deleted it, PathNotFoundException on
/// vself_emit_.../family/parent.vglp, about one run in two (Code #11,
/// 2026-10-19 15:55 UTC; GLP #9 Cowork, 2026-10-20 09:22 UTC, item 2: "yes,
/// vself_test makes its copy in a system temporary directory").  Deleting the
/// directory deletes the links and not what they point to.
String _rootOfItsOwn(String prefix) {
  final tmp = Directory.systemTemp.createTempSync(prefix);
  final programs = Directory(ppath.join(tmp.path, 'programs'))..createSync();
  final real = ppath.normalize(Directory(_programs).absolute.path);
  for (final e in Directory(real).listSync()) {
    final name = ppath.basename(e.path);
    if (name == 'tests') continue;
    Link(ppath.join(programs.path, name)).createSync(e.path);
  }
  return ppath.normalize(programs.path);
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
      // The copy is the program's directory in a root of its own, at the
      // path it has below programs/ ([_rootOfItsOwn]).
      late String tree;
      late String copy;
      late String root;
      setUp(() {
        final programs = _rootOfItsOwn('vself_emit_');
        tree = ppath.dirname(programs);
        root = ppath.join(programs, 'self.glp');
        copy = ppath.join(programs, 'tests', 'vglp', 'vself');
        _copy(_vself, copy);
      });
      tearDown(() {
        if (Directory(tree).existsSync()) {
          Directory(tree).deleteSync(recursive: true);
        }
      });

      test(':emit writes the self.vglp as the directory\'s self.glp, with the '
          'compiler\'s header, and the program runs from what it wrote, the '
          'same; a second :emit writes the same bytes', () async {
        final written = emitVglpSources(copy, rootSelfGlpPath: root);
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
        final modules = discoverProgram('$copy/family', rootSelfGlpPath: root);
        final selfModule = modules.singleWhere(
            (m) => ppath.normalize(m.filePath) == ppath.join(copy, 'self.glp'));
        final inMemory = discoverProgram('$_vself/family',
                rootSelfGlpPath: _root)
            .singleWhere((m) =>
                ppath.normalize(m.filePath) == ppath.join(_vself, 'self.glp'));
        expect(selfModule.ast.procedures.map((p) => p.signature).toSet(),
            inMemory.ast.procedures.map((p) => p.signature).toSet());
        expect(selfModule.volitional.keys, inMemory.volitional.keys);
        final (status, b) =
            await _play('$copy/family', 'play_family(P, C, L)', root: root);
        _expectFamily(status, b);

        final before = {
          for (final p in written) p: File(p).readAsStringSync(),
        };
        final again = emitVglpSources(copy, rootSelfGlpPath: root);
        expect(again.map(ppath.normalize).toSet(),
            written.map(ppath.normalize).toSet());
        for (final p in again) {
          expect(File(p).readAsStringSync(), before[p], reason: p);
        }
      });
    });
  });

  group('a call N # p into a directory whose self.glp is its self.vglp', () {
    // The fixture is a program in a root of its own ([_rootOfItsOwn]).
    late String tree;
    late String root;
    late Directory fixture;
    setUp(() {
      final programs = _rootOfItsOwn('vself_np_');
      tree = ppath.dirname(programs);
      root = ppath.join(programs, 'self.glp');
      fixture = Directory(ppath.join(programs, 'tests', 'vglp', 'np'))
        ..createSync(recursive: true);
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
      if (Directory(tree).existsSync()) {
        Directory(tree).deleteSync(recursive: true);
      }
    });

    test('resolves to the procedure that self.glp exports, in memory and '
        'from what :emit writes', () async {
      final (s1, b1) = await _play(fixture.path, 'go(X)', root: root);
      expect(s1, isNot(ExecutionStatus.failed));
      expect(b1['X'], 'hi');
      emitVglpSources(fixture.path, rootSelfGlpPath: root);
      final written = File('${fixture.path}/sub/self.glp');
      expect(written.readAsStringSync().startsWith(compiledHeader), isTrue);
      final (s2, b2) = await _play(fixture.path, 'go(X)', root: root);
      expect(s2, isNot(ExecutionStatus.failed));
      expect(b2['X'], 'hi');
    });
  });
}
