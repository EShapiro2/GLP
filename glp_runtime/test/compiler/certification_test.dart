/// The OS-privileged refusal (compiler/certification.dart): the names of the
/// root scope from which the network or the person is reachable, and the
/// boundary calls named when a flat program reaches one of them — directly,
/// or through a wrapper of its own, which reachability does not let pass.
library;

import 'dart:io';

import 'package:glp_runtime/compiler/certification.dart';
import 'package:glp_runtime/compiler/lexer.dart';
import 'package:glp_runtime/compiler/parser.dart';
import 'package:test/test.dart';

final String _rootSelf = File('../programs/self.glp').absolute.path;

void main() {
  late Set<String> root;

  setUpAll(() {
    root = privilegedRootNames([File(_rootSelf).readAsStringSync()]);
  });

  group('the privileged names of the root scope', () {
    test('the kernels and the seam predicates are privileged', () {
      expect(root, containsAll(privilegedKernels));
      expect(root, containsAll(privilegedPredicates));
    });

    test('a root-scope procedure that reaches a privileged kernel is '
        'privileged by closure', () {
      // send_to_user/1 names '_output'/1; the seam wrappers name their kernels.
      expect(root, contains('send_to_user/1'));
      expect(root, contains('peer_address/2'));
      expect(root, contains('trust_declare/2'));
      expect(root, contains('authorise_link/2'));
    });

    test('the signature predicates are not privileged: a mini-app may sign',
        () {
      expect(root, isNot(contains('sign/3')));
      expect(root, isNot(contains('self_key/1')));
      expect(root, isNot(contains('signed/4')));
      expect(root, isNot(contains('merge/3')));
    });
  });

  group('the boundary calls a program is refused for', () {
    List<PrivilegedCall> callsOf(String source) {
      final program = Parser(Lexer(source).tokenize()).parse();
      return privilegedCalls(program, root, ownModules: const {});
    }

    test('a program calling nothing privileged is certifiable', () {
      expect(callsOf('''
double(X, Y?) :- Y := X? * 2.
'''), isEmpty);
    });

    test('a direct call is named', () {
      final calls = callsOf('''
leak(S) :- send_to_user(S?).
''');
      expect(calls.map((c) => c.toString()), ['leak/1 calls send_to_user/1']);
    });

    test('a wrapper does not pass: the boundary call beneath it is named',
        () {
      final calls = callsOf('''
shout(S) :- wrapper(S?).
wrapper(S) :- send_to_user(S?).
''');
      expect(calls.map((c) => c.toString()),
          ['wrapper/1 calls send_to_user/1']);
    });

    test('a kernel named in call position is named', () {
      final calls = callsOf('''
raw(T) :- '_send'(T?, x, y).
''');
      expect(calls.map((c) => c.toString()), ['raw/1 calls _send/3']);
    });

    test('each boundary call is named once per caller', () {
      final calls = callsOf('''
twice(S) :- send_to_user(S?), send_to_user([]).
''');
      expect(calls.length, 1);
    });
  });
}
