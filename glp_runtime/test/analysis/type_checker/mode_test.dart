/// Tests for Mode enum
/// Specification: docs/modules/moded-term.md
/// Paper Reference: Definition 4.2

import 'package:test/test.dart';
import 'package:glp_runtime/analysis/type_checker/mode.dart';

void main() {
  group('Mode', () {
    group('flip operation', () {
      // Positive controls
      test('consume.flip returns produce', () {
        expect(Mode.consume.flip, equals(Mode.produce));
      });

      test('produce.flip returns consume', () {
        expect(Mode.produce.flip, equals(Mode.consume));
      });

      // Involution property
      test('flip is involution: consume.flip.flip == consume', () {
        expect(Mode.consume.flip.flip, equals(Mode.consume));
      });

      test('flip is involution: produce.flip.flip == produce', () {
        expect(Mode.produce.flip.flip, equals(Mode.produce));
      });
    });
  });
}
