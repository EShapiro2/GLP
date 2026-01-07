/// Tests for TypeEnvironment
/// Specification: docs/modules/type-environment.md
/// Paper Reference: Definition 4.1

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/type_environment.dart';

void main() {
  group('TypeEnvironment', () {
    late TypeEnvironment env;

    setUp(() {
      env = TypeEnvironment();
    });

    group('predefined types', () {
      // Positive controls
      test('isPredefinedType returns true for Integer', () {
        expect(env.isPredefinedType('Integer'), isTrue);
      });

      test('isPredefinedType returns true for String', () {
        expect(env.isPredefinedType('String'), isTrue);
      });

      test('isPredefinedType returns true for _', () {
        expect(env.isPredefinedType('_'), isTrue);
      });

      test('isPredefinedType returns true for _?', () {
        expect(env.isPredefinedType('_?'), isTrue);
      });

      // Negative controls
      test('isPredefinedType returns false for user type', () {
        expect(env.isPredefinedType('Stream'), isFalse);
      });

      test('getType returns null for predefined types', () {
        expect(env.getType('Integer'), isNull);
        expect(env.getType('_'), isNull);
      });
    });

    group('addType', () {
      // Positive controls
      test('adds type definition successfully', () {
        final typeDef = TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        );

        env.addType(typeDef);

        expect(env.getType('Stream'), equals(typeDef));
        expect(env.isTypeDefined('Stream'), isTrue);
      });

      // Negative controls
      test('throws RedefinitionError when type already defined', () {
        final typeDef = TypeDef('Stream', [ListNilAlt()]);
        env.addType(typeDef);

        expect(
          () => env.addType(TypeDef('Stream', [ListNilAlt()])),
          throwsA(isA<RedefinitionError>())
        );
      });

      test('throws PredefinedTypeError when redefining Integer', () {
        expect(
          () => env.addType(TypeDef('Integer', [ConstantAlt(0)])),
          throwsA(isA<PredefinedTypeError>())
        );
      });

      test('throws PredefinedTypeError when redefining _', () {
        expect(
          () => env.addType(TypeDef('_', [ConstantAlt(0)])),
          throwsA(isA<PredefinedTypeError>())
        );
      });

      test('throws PredefinedTypeError when redefining _?', () {
        expect(
          () => env.addType(TypeDef('_?', [ConstantAlt(0)])),
          throwsA(isA<PredefinedTypeError>())
        );
      });
    });

    group('addProcedure', () {
      // Positive controls
      test('adds procedure declaration successfully', () {
        final decl = ProcDecl('merge', 3, [
          TypeRef('Stream', isComplement: true),
          TypeRef('Stream', isComplement: true),
          TypeRef('Stream', isComplement: false)
        ]);

        env.addProcedure(decl);

        expect(env.getProcedure('merge', 3), equals(decl));
      });

      // Negative controls
      test('throws RedefinitionError when procedure already declared', () {
        final decl = ProcDecl('merge', 3, [
          TypeRef('Stream', isComplement: true),
          TypeRef('Stream', isComplement: true),
          TypeRef('Stream', isComplement: false)
        ]);
        env.addProcedure(decl);

        expect(
          () => env.addProcedure(decl),
          throwsA(isA<RedefinitionError>())
        );
      });
    });

    group('getProcedure', () {
      test('returns null for undeclared procedure', () {
        expect(env.getProcedure('unknown', 2), isNull);
      });

      test('distinguishes procedures by arity', () {
        env.addProcedure(ProcDecl('foo', 1, [TypeRef('T')]));
        env.addProcedure(ProcDecl('foo', 2, [TypeRef('T'), TypeRef('T')]));

        expect(env.getProcedure('foo', 1)?.arity, equals(1));
        expect(env.getProcedure('foo', 2)?.arity, equals(2));
        expect(env.getProcedure('foo', 3), isNull);
      });
    });

    group('validateTypeReferences', () {
      // Positive controls
      test('succeeds when all type references are defined', () {
        env.addType(TypeDef(
          'Stream',
          [ListNilAlt(), ListConsAlt(PrimitiveType('_'), TypeRef('Stream'))]
        ));
        env.addProcedure(ProcDecl('merge', 3, [
          TypeRef('Stream', isComplement: true),
          TypeRef('Stream', isComplement: true),
          TypeRef('Stream', isComplement: false)
        ]));

        expect(() => env.validateTypeReferences(), returnsNormally);
      });

      // Negative controls
      test('throws UndefinedTypeError for undefined type in type definition', () {
        env.addType(TypeDef(
          'BadStream',
          [ListConsAlt(PrimitiveType('_'), TypeRef('UndefinedType'))]
        ));

        expect(
          () => env.validateTypeReferences(),
          throwsA(isA<UndefinedTypeError>())
        );
      });

      test('throws UndefinedTypeError for undefined type in procedure declaration', () {
        env.addProcedure(ProcDecl('foo', 1, [TypeRef('UndefinedType')]));

        expect(
          () => env.validateTypeReferences(),
          throwsA(isA<UndefinedTypeError>())
        );
      });
    });
  });
}
