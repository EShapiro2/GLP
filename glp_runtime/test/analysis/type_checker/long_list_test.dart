/// A clause whose argument is a list of any length loads and is type-checked.
///
/// TGLP well-typing.tex, Definition "Well-Typed Clause" (def:well-typed-clause),
/// puts no bound on a term's size.  Until 2026-10-19 a clause whose argument
/// was a list of more than some 3100 elements was refused at load with "Error
/// checking clause: Stack Overflow": 3100 quoted atoms of a declared list type
/// loaded and 3125 did not, and a Stream(Integer) of 4000 integers failed the
/// same way (Code #11, 2026-10-19, for vGLP's programs/vglp/cva/compiler/
/// text.glp, a list of more than 3100 lines).  The walks of the load path that
/// recursed on a list's tail --- the moded head, its paths, the anonymous
/// reader check, the SRSW analysis, the partial evaluator, input coverage, the
/// parameter inspection and the code generator --- loop along the list's
/// spine, and the paths of a term are checked in one walk of it.
///
/// Each file below is written to a directory under the root, programs/, and
/// loaded as the REPL loads a file ([GlpEngine.loadFile]), which type-checks
/// it and refuses it if it does not check.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

const int _n = 100000;

/// [n] elements written by [element], separated by commas.
String _elements(int n, String Function(int i) element) =>
    List.generate(n, element).join(', ');

/// The declared list of quoted atoms of the report, [n] elements of it, the
/// last written by [last] where it is given.
String _atoms(int n, {String? last}) =>
    'Line ::= String ; guarded_only(String).\n'
    'Lines ::= [] ; [Line | Lines].\n'
    '\n'
    'exported procedure f(Lines).\n'
    'f([${_elements(n, (i) => i == n - 1 && last != null ? last : "'line $i of the text'")}]).\n';

/// A Stream(Integer) of [n] integers, the last written by [last] where it is
/// given.
String _integers(int n, {String? last}) =>
    'exported procedure g(Stream(Integer)).\n'
    'g([${_elements(n, (i) => i == n - 1 && last != null ? last : '$i')}]).\n';

void main() {
  late Directory dir;

  setUp(() {
    // Under the root, programs/ (TGLP modules.tex, "Scope construction").
    dir = Directory('../programs/tests').createTempSync('glp_long_list_');
  });

  tearDown(() => dir.deleteSync(recursive: true));

  /// Load [source] as the file [name] in [dir].
  bool load(String name, String source) {
    final file = File('${dir.path}/$name')..writeAsStringSync(source);
    return GlpEngine(rootSelfGlpPath: _rootSelf).loadFile(file.absolute.path);
  }

  /// The load of [source] as [name] is refused by the type check, its error
  /// naming [reason].
  void refused(String name, String source, String reason) {
    expect(
        () => load(name, source),
        throwsA(predicate((e) {
          final s = e.toString();
          return s.contains('Type checking failed') && s.contains(reason);
        }, 'refused by the type check: $reason')));
  }

  group('a clause whose argument is a list of $_n elements', () {
    test('of quoted atoms of a declared list type loads', () {
      expect(load('atoms.glp', _atoms(_n)), isTrue);
    });

    test('of integers, a Stream(Integer), loads', () {
      expect(load('integers.glp', _integers(_n)), isTrue);
    });

    test('is checked to its last element: an integer among the atoms is refused',
        () {
      refused('atoms_bad.glp', _atoms(_n, last: '7'),
          'No alternative of Line matches the constant 7');
    });

    test('is checked to its last element: a string among the integers is refused',
        () {
      refused('integers_bad.glp', _integers(_n, last: "'x'"),
          'Integer type requires integer literal');
    });
  });

  group('the other walks of the load path, at a list of $_n elements', () {
    final ints = _elements(_n, (i) => '$i');

    test('a body goal building the list loads (code generation in the body)',
        () {
      expect(
          load(
              'body.glp',
              'exported procedure p.\np :- h([$ints]).\n\n'
                  'procedure h(Stream(Integer)?).\nh(_).\n'),
          isTrue);
    });

    test('the list inside a structure of the head loads', () {
      expect(
          load(
              'nested.glp',
              'T ::= t(Stream(Integer)).\n\n'
                  'exported procedure q(T).\nq(t([$ints])).\n'),
          isTrue);
    });

    test('the list at an input of the head, with a clause covering it, loads',
        () {
      expect(
          load(
              'input.glp',
              'exported procedure kk(Stream(Integer)?).\n'
                  'kk([$ints]).\nkk(_).\n'),
          isTrue);
    });

    test('a clause with a defined guard loads (the partial evaluator)', () {
      expect(
          load(
              'guard.glp',
              'exported procedure dg(Channel(Closed, Closed)?, Stream(Integer)).\n'
                  'dg(Ch, [$ints]) :- close(Ch?) | true.\n'),
          isTrue);
    });

    test('the list in a guard loads', () {
      expect(
          load(
              'guard_arg.glp',
              'exported procedure pg(Stream(Integer)?).\n'
                  'pg(X) :- X? =?= [$ints] | true.\n'
                  'pg(_) :- otherwise | true.\n'),
          isTrue);
    });

    test('a call to a parameterised procedure with the list loads', () {
      expect(
          load(
              'call.glp',
              'procedure(X) pcount(Stream(X)?).\npcount([]).\n'
                  'pcount([_|Xs]) :- pcount(Xs?).\n\n'
                  'exported procedure pp.\npp :- pcount([$ints]).\n'),
          isTrue);
    });

    test(
        'a parameterised procedure whose head is a list of $_n anonymous '
        'variables loads', () {
      expect(
          load(
              'param_head.glp',
              'procedure(X) pl(Stream(X)?).\n'
                  'pl([${_elements(_n, (_) => '_')}]).\npl(_).\n\n'
                  'exported procedure pq.\npq :- pl([1]).\n'),
          isTrue);
    });
  });

  // "the error for an ill-typed term prints the first failing path and the
  // number of the rest" (GLP #9 Cowork, 2026-10-20 09:22 UTC, item 3): each
  // path of a list runs down its spine, and until 2026-10-20 every failing
  // path was printed in full, text quadratic in the list's length (Code #11,
  // 2026-10-19 17:32 UTC).
  test(
      'a clause whose argument is a list of $_n ill-typed elements is refused, '
      'its error printing the first failing path and the number of the rest',
      () {
    final source = 'exported procedure g(Stream(Integer)).\n'
        'g([${_elements(_n, (i) => "'s$i'")}]).\n';
    Object? error;
    try {
      load('all_bad.glp', source);
    } catch (e) {
      error = e;
    }
    final s = error.toString();
    expect(s, contains('Type checking failed'));
    expect(s, contains('Head of g is not well-typed:\n'
        '  Inconsistent path: Integer type requires integer literal\n'
        '  Path: '));
    expect(s, contains('and ${_n - 1} more inconsistent paths of the term'));
    expect('Inconsistent path:'.allMatches(s).length, 1);
    expect(s.length, lessThan(2000));
  });

  // "the input-coverage error prints the first uncovered path and the number
  // of the rest, as the ill-typed term's error does" (GLP #9 Cowork,
  // Code_inbox 2026-10-20 22:54 UTC, item 3): until 2026-10-21 every
  // uncovered alternative of a list in a head was printed with its full path,
  // text quadratic in the list's length, 545 MB for a clause of 8000 integers
  // (Code #12, 2026-10-20 11:08 UTC).
  test(
      'input coverage of a list in the head reports its first uncovered '
      'alternative, in the order of the walk, and the number of the rest', () {
    Object? error;
    try {
      load('cover.glp', 'exported procedure k(Stream(Integer)?).\nk([0, 1, 2]).\n');
    } catch (e) {
      error = e;
    }
    final s = error.toString();
    expect(s, contains('Type checking failed'));
    expect(
        s,
        contains(
            '  k argument 1: uncovered alternative "[]" at path: Stream<Integer> → []\n'
            '  and 4 more uncovered alternatives of the argument at line 1'));
    expect('uncovered alternative "'.allMatches(s).length, 1);
  });

  test('an argument with one uncovered alternative prints it and no count', () {
    Object? error;
    try {
      load('cover1.glp',
          'Kind ::= x ; y.\nexported procedure q(Kind?).\nq(x).\n');
    } catch (e) {
      error = e;
    }
    final s = error.toString();
    expect(s, contains('Type checking failed'));
    expect(s, contains('q argument 1: uncovered alternative "y" at path: '));
    expect(s, isNot(contains('more uncovered')));
  });

  test(
      'a clause whose input argument is a list of $_n integers, uncovered at '
      'each of its cells, is refused, its error printing the first uncovered '
      'alternative and the number of the rest', () {
    Object? error;
    try {
      load('cover_long.glp', _integers(_n).replaceFirst('g(Stream(Integer))', 'g(Stream(Integer)?)'));
    } catch (e) {
      error = e;
    }
    final s = error.toString();
    expect(s, contains('Type checking failed'));
    expect(
        s,
        contains('  g argument 1: uncovered alternative "[]" at path: Stream<Integer> → []\n'
            '  and ${_n + 1} more uncovered alternatives of the argument at line 1'));
    expect('uncovered alternative "'.allMatches(s).length, 1);
    expect(s.length, lessThan(2000));
  });
}
