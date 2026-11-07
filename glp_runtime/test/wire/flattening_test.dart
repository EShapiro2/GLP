/// S5 — deterministic flattening + source identity h(M) (§6).
///
/// h(M) is the SHA-256 of the canonical print of the linked, *pruned* program.
/// Determinism (two builds equal), stability (editing unreachable code does not
/// change h(M) — it is pruned by DCE before printing), sensitivity (editing a
/// reachable clause does).
///
/// The print names every procedure by its module's path from the root (TGLP
/// modules.tex, Compilation, third step), so the flattens compared are of one
/// project, edited in place between them: the same source at another path is
/// another print.
library;

import 'dart:io';
import 'package:glp_runtime/wire/flattening.dart';
import 'package:test/test.dart';

const _rootSelf = '../programs/self.glp';

/// A project whose exported `go/2` reaches `helper/2`; `dead/2` is unreachable.
/// Its self.glp exports go/2, forwarding it to main.glp: a directory with no
/// self.glp is not a program (TGLP modules.tex, "Entry and the absence of a
/// boot module").
String _src({required String helperBody, required String deadBody}) => '''
exported procedure go(Integer?, Integer).
go(X, Y?) :- helper(X?, Y).

procedure helper(Integer?, Integer).
helper(X, Y?) :- $helperBody.

procedure dead(Integer?, Integer).
dead(X, Y?) :- $deadBody.
''';

Directory _project(String source) {
  // Under the root, programs/ (TGLP modules.tex, "Scope construction": "A
  // program lies at or below the root"): a program in the system's temporary
  // directory, outside it, is refused since 2026-10-04.
  final dir = Directory('../programs/tests').createTempSync('glp_flatten_');
  File('${dir.path}/self.glp').writeAsStringSync(_self);
  _edit(dir, source);
  return dir;
}

const _self = '''
imported procedure main#go(Integer?, Integer).
exported procedure go(Integer?, Integer).
go(X, Y?) :- main # go(X?, Y).
''';

/// [dir]'s main.glp rewritten to [source].
void _edit(Directory dir, String source) =>
    File('${dir.path}/main.glp').writeAsStringSync(source);

void main() {
  group('deterministic flattening + h(M)', () {
    test('determinism: two flattens of the same project give equal h(M)', () {
      final dir = _project(_src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 99'));
      try {
        final a = flattenProject(dir.path, rootSelfGlpPath: _rootSelf);
        final b = flattenProject(dir.path, rootSelfGlpPath: _rootSelf);
        expect(a.hM, b.hM);
        expect(a.hM.length, 32);
        // The reachable procedures are printed; the unreachable one is not.
        expect(a.print, contains('helper'));
        expect(a.print, isNot(contains('dead')));
      } finally {
        dir.deleteSync(recursive: true);
      }
    });

    test('stability: editing unreachable code does not change h(M)', () {
      final d = _project(_src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 99'));
      try {
        final h1 = flattenProject(d.path, rootSelfGlpPath: _rootSelf).hM;
        _edit(d, _src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 12345'));
        final h2 = flattenProject(d.path, rootSelfGlpPath: _rootSelf).hM;
        expect(h2, h1, reason: 'dead/2 is pruned, so its edit is invisible to h(M)');
      } finally {
        d.deleteSync(recursive: true);
      }
    });

    test('sensitivity: editing a reachable clause changes h(M)', () {
      final d = _project(_src(helperBody: 'Y := X? + 1', deadBody: 'Y := X? + 99'));
      try {
        final h1 = flattenProject(d.path, rootSelfGlpPath: _rootSelf).hM;
        _edit(d, _src(helperBody: 'Y := X? + 2', deadBody: 'Y := X? + 99'));
        final h2 = flattenProject(d.path, rootSelfGlpPath: _rootSelf).hM;
        expect(h2, isNot(h1), reason: 'helper/2 is reachable, so its edit moves h(M)');
      } finally {
        d.deleteSync(recursive: true);
      }
    });

    // The print keeps "the type definitions their declarations reference",
    // the reachable procedures' (IGLP 9c605b3, code-format-fragment.tex,
    // "Deterministic Flattening"): a type defined in a module the program does
    // not reach is not printed, and its edit leaves h(M) as it was; a type a
    // reachable declaration references is printed, with the types its
    // definition references, and its edit moves h(M).
    test('types: a type of a module the program does not reach leaves h(M) '
        'unchanged; a reached type and the types it references are printed',
        () {
      final d = _project(_typed(tag: 'red ; green'));
      try {
        File('${d.path}/other.glp').writeAsStringSync(_other('a ; b'));
        final f1 = flattenProject(d.path, rootSelfGlpPath: _rootSelf);
        expect(f1.print, contains('Item ::= item(Integer, Tag).'));
        expect(f1.print, contains('Tag ::= red ; green.'),
            reason: 'Tag is reached through Item\'s definition');
        expect(f1.print, isNot(contains('Unreached')),
            reason: 'no reachable declaration references other.glp\'s type');
        expect(f1.print, isNot(contains('Spare')),
            reason: 'main.glp defines Spare, but no declaration references it');

        File('${d.path}/other.glp').writeAsStringSync(_other('a ; b ; c'));
        final f2 = flattenProject(d.path, rootSelfGlpPath: _rootSelf);
        expect(f2.hM, f1.hM,
            reason: 'other.glp is a module of the program the entry points do '
                'not reach, so its type is not in the print');

        _edit(d, _typed(tag: 'red ; green ; blue'));
        final f3 = flattenProject(d.path, rootSelfGlpPath: _rootSelf);
        expect(f3.hM, isNot(f1.hM),
            reason: 'Tag is reached from count/2\'s declaration, through Item');
      } finally {
        d.deleteSync(recursive: true);
      }
    });
  });
}

/// main.glp of a project whose go/2 reaches count/2, whose declaration
/// references Item, whose definition references Tag, of the alternatives
/// [tag]; Spare is defined and referenced by no declaration.
String _typed({required String tag}) => '''
Tag ::= $tag.
Item ::= item(Integer, Tag).
Spare ::= spare(Integer).

exported procedure go(Integer?, Integer).
go(X, Y?) :- count(item(X?, red), Y).

procedure count(Item?, Integer).
count(item(N, _), Y?) :- Y := N? + 1.
''';

/// other.glp: a module of the program that nothing the entry points reach
/// calls, defining the type Unreached with the alternatives [alts].
String _other(String alts) => '''
Unreached ::= $alts.

procedure pick(Unreached?, Integer).
pick(_, 1).
''';

