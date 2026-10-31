// glp_runtime/test/vglp/mediator_test.dart
//
// A type expression as GLP source, as the canonical compilation prints it
// (mediator.dart, typeSource).  The tests of the old compilation's mediator,
// its instantiation and its clauses, went with it (vGLP #22's task,
// Code_inbox 2026-10-31 15:20 UTC).
// Spec: vGLP, Section 6, "The Implementation: Compiling vGLP onto GLP".

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart' show TypeRef;
import 'package:glp_runtime/vglp/mediator.dart';

void main() {
  test("an expanded name off the checker's environment prints as source", () {
    // Stream<Coin> is the environment's internal name for an instantiation
    // and is not source syntax; it is printed as the instantiation it stands
    // for.
    expect(typeSource(TypeRef('Stream<Coin>', 0, 0)), 'Stream(Coin)');
    expect(typeSource(TypeRef('Channel<Closed, Stream<Coin>>', 0, 0)),
        'Channel(Closed, Stream(Coin))');
    expect(typeSource(TypeRef('Stream<Coin>', 0, 0, isInput: true)),
        'Stream(Coin)?');
  });
}
