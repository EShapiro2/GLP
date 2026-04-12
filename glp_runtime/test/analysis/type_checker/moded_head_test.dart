// glp_runtime/test/analysis/type_checker/moded_head_test.dart
//
// Unit tests for moded head construction.
// Spec: docs/type system/moded-head.md
// Paper: Definition 5.5

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/moded_head.dart';
import 'package:glp_runtime/analysis/type_checker/moded_term.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';
import 'package:glp_runtime/analysis/type_checker/type_ast.dart';
import 'package:glp_runtime/compiler/ast.dart' as ast;

void main() {
  group('modedHead', () {
    // Helper to build a Goal from functor and args
    ast.Goal goal(String f, List<ast.Term> args) =>
        ast.Goal(f, args, 0, 0);

    // Helper to build variable terms
    ast.VarTerm writer(String name) => ast.VarTerm(name, false, 0, 0);
    ast.VarTerm reader(String name) => ast.VarTerm(name, true, 0, 0);

    // Helper to build list terms
    ast.ListTerm nil() => ast.ListTerm(null, null, 0, 0);
    ast.ListTerm cons(ast.Term h, ast.Term t) => ast.ListTerm(h, t, 0, 0);

    // Helper to build constants
    ast.ConstTerm constTerm(Object v) => ast.ConstTerm(v, 0, 0);

    group('basic construction', () {
      test('merge head: merge([X|Xs], Ys, [X?|Zs?])', () {
        // Type: procedure merge(Stream?, Stream?, Stream).
        // Stream? = input, Stream = output
        final decl = ProcDecl(
          'merge',
          [
            TypeRef('Stream', 0, 0, isInput: true),   // arg 1: Stream?
            TypeRef('Stream', 0, 0, isInput: true),   // arg 2: Stream?
            TypeRef('Stream', 0, 0, isInput: false),  // arg 3: Stream
          ],
          0, 0,
        );

        // Head: merge([X|Xs], Ys, [X?|Zs?])
        final head = goal('merge', [
          cons(writer('X'), writer('Xs')),    // [X|Xs]
          writer('Ys'),                        // Ys
          cons(reader('X'), reader('Zs')),    // [X?|Zs?]
        ]);

        final result = modedHead(head, decl);

        // Root should be consume mode
        expect(result.mode, equals(Mode.consume));
        expect(result, isA<ModedCompound>());

        final compound = result as ModedCompound;
        expect(compound.functor, equals('merge'));
        expect(compound.arity, equals(3));

        // Arg 1: [X|Xs] at Stream? position → mode ↓
        // Writers X, Xs at ↓ positions become readers X?, Xs?
        final arg1 = compound.args[0] as ModedCompound;
        expect(arg1.mode, equals(Mode.consume));
        expect(arg1.isListCons, isTrue);

        final x1 = arg1.args[0] as ModedVariable;
        expect(x1.name, equals('X'));
        expect(x1.isReader, isTrue);  // writer → reader at ↓
        expect(x1.mode, equals(Mode.consume));

        final xs1 = arg1.args[1] as ModedVariable;
        expect(xs1.name, equals('Xs'));
        expect(xs1.isReader, isTrue);  // writer → reader at ↓
        expect(xs1.mode, equals(Mode.consume));

        // Arg 2: Ys at Stream? position → mode ↓
        // Writer Ys at ↓ position becomes reader Ys?
        final ys = compound.args[1] as ModedVariable;
        expect(ys.name, equals('Ys'));
        expect(ys.isReader, isTrue);  // writer → reader at ↓
        expect(ys.mode, equals(Mode.consume));

        // Arg 3: [X?|Zs?] at Stream position → mode ↑
        // Readers X?, Zs? at ↑ positions become writers X, Zs
        final arg3 = compound.args[2] as ModedCompound;
        expect(arg3.mode, equals(Mode.produce));
        expect(arg3.isListCons, isTrue);

        final x3 = arg3.args[0] as ModedVariable;
        expect(x3.name, equals('X'));
        expect(x3.isReader, isFalse);  // reader → writer at ↑
        expect(x3.mode, equals(Mode.produce));

        final zs3 = arg3.args[1] as ModedVariable;
        expect(zs3.name, equals('Zs'));
        expect(zs3.isReader, isFalse);  // reader → writer at ↑
        expect(zs3.mode, equals(Mode.produce));
      });

      test('base case: merge(Xs, [], Xs?)', () {
        final decl = ProcDecl(
          'merge',
          [
            TypeRef('Stream', 0, 0, isInput: true),
            TypeRef('Stream', 0, 0, isInput: true),
            TypeRef('Stream', 0, 0, isInput: false),
          ],
          0, 0,
        );

        // Head: merge(Xs, [], Xs?)
        final head = goal('merge', [
          writer('Xs'),  // Xs
          nil(),         // []
          reader('Xs'),  // Xs?
        ]);

        final result = modedHead(head, decl) as ModedCompound;

        // Arg 1: Xs at ↓ → Xs?
        final xs1 = result.args[0] as ModedVariable;
        expect(xs1.name, equals('Xs'));
        expect(xs1.isReader, isTrue);

        // Arg 2: [] at ↓
        final nilArg = result.args[1] as ModedConstant;
        expect(nilArg.isNil, isTrue);
        expect(nilArg.mode, equals(Mode.consume));

        // Arg 3: Xs? at ↑ → Xs
        final xs3 = result.args[2] as ModedVariable;
        expect(xs3.name, equals('Xs'));
        expect(xs3.isReader, isFalse);
      });
    });

    group('variable replacement', () {
      test('writer at consume position becomes reader', () {
        // procedure foo(T?). where T? is input
        final decl = ProcDecl(
          'foo',
          [TypeRef('T', 0, 0, isInput: true)],
          0, 0,
        );

        // Head: foo(X) - writer at input position
        final head = goal('foo', [writer('X')]);
        final result = modedHead(head, decl) as ModedCompound;

        final x = result.args[0] as ModedVariable;
        expect(x.name, equals('X'));
        expect(x.isReader, isTrue);  // flipped
        expect(x.mode, equals(Mode.consume));
      });

      test('reader at produce position becomes writer', () {
        // procedure foo(T). where T is output
        final decl = ProcDecl(
          'foo',
          [TypeRef('T', 0, 0, isInput: false)],
          0, 0,
        );

        // Head: foo(X?) - reader at output position
        final head = goal('foo', [reader('X')]);
        final result = modedHead(head, decl) as ModedCompound;

        final x = result.args[0] as ModedVariable;
        expect(x.name, equals('X'));
        expect(x.isReader, isFalse);  // flipped
        expect(x.mode, equals(Mode.produce));
      });

      test('reader at consume position is complemented', () {
        final decl = ProcDecl(
          'foo',
          [TypeRef('T', 0, 0, isInput: true)],
          0, 0,
        );

        // Head: foo(X?) - reader at input position
        // With unconditional complementation, reader becomes writer
        final head = goal('foo', [reader('X')]);
        final result = modedHead(head, decl) as ModedCompound;

        final x = result.args[0] as ModedVariable;
        expect(x.name, equals('X'));
        expect(x.isReader, isFalse);  // unconditionally flipped: reader → writer
      });

      test('writer at produce position is complemented', () {
        final decl = ProcDecl(
          'foo',
          [TypeRef('T', 0, 0, isInput: false)],
          0, 0,
        );

        // Head: foo(X) - writer at output position
        // With unconditional complementation, writer becomes reader
        final head = goal('foo', [writer('X')]);
        final result = modedHead(head, decl) as ModedCompound;

        final x = result.args[0] as ModedVariable;
        expect(x.name, equals('X'));
        expect(x.isReader, isTrue);  // unconditionally flipped: writer → reader
      });
    });

    group('anonymous variables', () {
      test('each anonymous variable gets unique name', () {
        final decl = ProcDecl(
          'foo',
          [
            TypeRef('T', 0, 0, isInput: true),
            TypeRef('T', 0, 0, isInput: true),
          ],
          0, 0,
        );

        // Head: foo(_, _)
        final head = goal('foo', [
          ast.UnderscoreTerm(0, 0),
          ast.UnderscoreTerm(0, 0),
        ]);

        final result = modedHead(head, decl) as ModedCompound;

        final v1 = result.args[0] as ModedVariable;
        final v2 = result.args[1] as ModedVariable;

        // Both should be readers (at ↓ positions)
        expect(v1.isReader, isTrue);
        expect(v2.isReader, isTrue);

        // Names should be different
        expect(v1.name, isNot(equals(v2.name)));
        expect(v1.name, startsWith('_#'));
        expect(v2.name, startsWith('_#'));
      });
    });

    group('error handling', () {
      test('arity mismatch throws ArityMismatchError', () {
        final decl = ProcDecl(
          'foo',
          [TypeRef('T', 0, 0)],  // 1 arg
          0, 0,
        );

        // Head with 2 args
        final head = goal('foo', [writer('X'), writer('Y')]);

        expect(
          () => modedHead(head, decl),
          throwsA(isA<ArityMismatchError>()),
        );
      });
    });
  });

  group('producedTerm', () {
    ast.Goal goal(String f, List<ast.Term> args) =>
        ast.Goal(f, args, 0, 0);
    ast.VarTerm writer(String name) => ast.VarTerm(name, false, 0, 0);
    ast.VarTerm reader(String name) => ast.VarTerm(name, true, 0, 0);

    test('body atom has produce mode at root', () {
      final decl = ProcDecl(
        'merge',
        [
          TypeRef('Stream', 0, 0, isInput: true),
          TypeRef('Stream', 0, 0, isInput: true),
          TypeRef('Stream', 0, 0, isInput: false),
        ],
        0, 0,
      );

      // Body: merge(Ys?, Xs?, Zs)
      final atom = goal('merge', [
        reader('Ys'),
        reader('Xs'),
        writer('Zs'),
      ]);

      final result = producedTerm(atom, decl);

      // Root should be produce mode (body atoms are produced)
      expect(result.mode, equals(Mode.produce));

      final compound = result as ModedCompound;

      // Variables should NOT be flipped (body atoms keep variables as-is)
      final ys = compound.args[0] as ModedVariable;
      expect(ys.name, equals('Ys'));
      expect(ys.isReader, isTrue);  // reader stays reader

      final xs = compound.args[1] as ModedVariable;
      expect(xs.name, equals('Xs'));
      expect(xs.isReader, isTrue);

      final zs = compound.args[2] as ModedVariable;
      expect(zs.name, equals('Zs'));
      expect(zs.isReader, isFalse);  // writer stays writer
    });

    test('arity mismatch throws ArityMismatchError', () {
      final decl = ProcDecl(
        'foo',
        [TypeRef('T', 0, 0)],
        0, 0,
      );

      final atom = goal('foo', [writer('X'), writer('Y')]);

      expect(
        () => producedTerm(atom, decl),
        throwsA(isA<ArityMismatchError>()),
      );
    });
  });

  group('explicit dual type definitions', () {
    ast.Goal goal(String f, List<ast.Term> args) =>
        ast.Goal(f, args, 0, 0);
    ast.VarTerm writer(String name) => ast.VarTerm(name, false, 0, 0);
    ast.VarTerm reader(String name) => ast.VarTerm(name, true, 0, 0);
    ast.StructTerm struct(String f, List<ast.Term> args) =>
        ast.StructTerm(f, args, 0, 0);

    test('Channel with explicit dual preserves internal structure', () {
      // Channel ::= ch(Stream?, Stream).
      // Channel? ::= ch(Stream?, Stream)?.
      // The explicit dual preserves internal structure - position 1 is always
      // Stream? (input), position 2 is always Stream (output).
      
      final typeEnv = TypeEnvironment({}, {});
      
      // Define Channel
      typeEnv.addType(TypeDef(
        'Channel',
        [
          StructAlt('ch', [
            TypeRef('Stream', 0, 0, isInput: true),   // Stream?
            TypeRef('Stream', 0, 0, isInput: false),  // Stream
          ], 0, 0),
        ],
        0, 0,
      ));
      
      // Define explicit dual Channel?
      typeEnv.addType(TypeDef(
        'Channel?',
        [
          StructAlt('ch', [
            TypeRef('Stream', 0, 0, isInput: true),   // Stream? (preserved!)
            TypeRef('Stream', 0, 0, isInput: false),  // Stream (preserved!)
          ], 0, 0),
        ],
        0, 0,
      ));
      
      // Define Stream (simple for this test)
      typeEnv.addType(TypeDef(
        'Stream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            PrimitiveModeAlt(false, 0, 0),  // _ (output)
            TypeRef('Stream', 0, 0, isInput: false),
          0, 0),
        ],
        0, 0,
      ));
      
      // procedure send(_?, Channel?, Channel).
      final decl = ProcDecl(
        'send',
        [
          PrimitiveModeAlt(true, 0, 0),  // _?
          TypeRef('Channel', 0, 0, isInput: true),   // Channel?
          TypeRef('Channel', 0, 0, isInput: false),  // Channel
        ],
        0, 0,
      );
      
      // Head: send(X, ch(In, Out), ch(In?, Out?))
      // Arg 2 (Channel?) should have position 1 at mode ↓, position 2 at mode ↑
      // (preserved from explicit dual definition)
      final head = goal('send', [
        writer('X'),
        struct('ch', [writer('In'), writer('Out')]),
        struct('ch', [reader('In'), reader('Out')]),
      ]);
      
      final result = modedHead(head, decl, typeEnv: typeEnv) as ModedCompound;
      
      // Arg 2: ch(In, Out) at Channel? position
      final arg2 = result.args[1] as ModedCompound;
      expect(arg2.functor, equals('ch'));
      expect(arg2.mode, equals(Mode.consume));  // Channel? is consumed
      
      // At consume position, internal modes are complemented:
      // Position 1 (Stream?) becomes mode ↑ (produce)
      // Position 2 (Stream) becomes mode ↓ (consume)
      final in2 = arg2.args[0] as ModedVariable;
      expect(in2.mode, equals(Mode.produce));  // Stream? complemented
      expect(in2.isReader, isTrue);  // writer at ↑ complemented to reader

      final out2 = arg2.args[1] as ModedVariable;
      expect(out2.mode, equals(Mode.consume));  // Stream complemented
      expect(out2.isReader, isTrue);  // writer at ↓ complemented to reader
    });

    test('DiffList with explicit dual preserves internal structure', () {
      // DiffList ::= Stream? \ Stream.
      // DiffList? ::= (Stream? \ Stream)?.
      // Position 1 is always content (Stream?, consumed), position 2 is always hole (Stream, produced)
      
      final typeEnv = TypeEnvironment({}, {});
      
      // Define Stream
      typeEnv.addType(TypeDef(
        'Stream',
        [
          ListNilAlt(0, 0),
          ListConsAlt(
            PrimitiveModeAlt(false, 0, 0),
            TypeRef('Stream', 0, 0, isInput: false),
          0, 0),
        ],
        0, 0,
      ));
      
      // Define DiffList
      typeEnv.addType(TypeDef(
        'DiffList',
        [
          DiffListAlt(
            TypeRef('Stream', 0, 0, isInput: true),   // Stream? (content)
            TypeRef('Stream', 0, 0, isInput: false),  // Stream (hole)
          0, 0),
        ],
        0, 0,
      ));
      
      // Define explicit dual DiffList?
      typeEnv.addType(TypeDef(
        'DiffList?',
        [
          DiffListAlt(
            TypeRef('Stream', 0, 0, isInput: true),   // Stream? (preserved!)
            TypeRef('Stream', 0, 0, isInput: false),  // Stream (preserved!)
          0, 0),
        ],
        0, 0,
      ));
      
      // procedure dl_append(DiffList?, DiffList?, DiffList).
      final decl = ProcDecl(
        'dl_append',
        [
          TypeRef('DiffList', 0, 0, isInput: true),   // DiffList?
          TypeRef('DiffList', 0, 0, isInput: true),   // DiffList?
          TypeRef('DiffList', 0, 0, isInput: false),  // DiffList
        ],
        0, 0,
      );
      
      // Head: dl_append(A\B?, B\C?, A?\C)
      final head = goal('dl_append', [
        struct(r'\', [writer('A'), reader('B')]),  // A\B?
        struct(r'\', [writer('B'), reader('C')]),  // B\C?
        struct(r'\', [reader('A'), writer('C')]),  // A?\C
      ]);
      
      final result = modedHead(head, decl, typeEnv: typeEnv) as ModedCompound;
      
      // Arg 1: A\B? at DiffList? position
      final arg1 = result.args[0] as ModedCompound;
      expect(arg1.functor, equals(r'\'));
      expect(arg1.mode, equals(Mode.consume));  // DiffList? is consumed
      
      // At consume position, internal modes are complemented:
      // Position 1 (Stream? content) becomes mode ↑ (produce)
      // Position 2 (Stream hole) becomes mode ↓ (consume)
      final a1 = arg1.args[0] as ModedVariable;
      expect(a1.mode, equals(Mode.produce));  // Stream? complemented
      expect(a1.isReader, isTrue);  // writer at ↑ complemented to reader

      final b1 = arg1.args[1] as ModedVariable;
      expect(b1.mode, equals(Mode.consume));  // Stream complemented
      expect(b1.isReader, isFalse);  // reader at ↓ complemented to writer
    });
  });
}
