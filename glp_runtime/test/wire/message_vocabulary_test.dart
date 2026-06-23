/// S6 — adoption/handshake message vocabulary (§8), byte forms.
///
/// Round-trip each message term through the canonical codec; the accept/decline
/// atoms; the signed-bytes path e(sig(HSrc, Term)).
library;

import 'dart:typed_data';
import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/message_vocabulary.dart' as mv;
import 'package:test/test.dart';

Uint8List _h(int seed) =>
    Uint8List.fromList(List<int>.generate(32, (i) => (i + seed) & 0xFF));

void main() {
  final hSrc = _h(1);
  final hSrcDef = _h(2);
  final hBin = _h(3);

  group('message round-trips', () {
    test('offer(HSrc, tau(HSrcDef, TypeName))', () {
      final t = mv.offer(hSrc, hSrcDef, 'ch');
      final back = mv.decodeMessage(mv.encodeMessage(t));
      final f = mv.parseOffer(back);
      expect(f.hSrc, hSrc);
      expect(f.hSrcDef, hSrcDef);
      expect(f.typeName, 'ch');
    });

    test('ship(Artefact, HBin, HSrc)', () {
      final artefact = Uint8List.fromList([9, 8, 7, 6, 5]);
      final t = mv.ship(artefact, hBin, hSrc);
      final f = mv.parseShip(mv.decodeMessage(mv.encodeMessage(t)));
      expect(f.artefact, artefact);
      expect(f.hBin, hBin);
      expect(f.hSrc, hSrc);
    });

    test('handshake(HSrc, tau(HSrcDef, TypeName))', () {
      final t = mv.handshake(hSrc, hSrcDef, 'root');
      final f = mv.parseHandshake(mv.decodeMessage(mv.encodeMessage(t)));
      expect(f.hSrc, hSrc);
      expect(f.hSrcDef, hSrcDef);
      expect(f.typeName, 'root');
    });

    test('accept / decline atoms', () {
      expect(mv.isAccept(mv.decodeMessage(mv.encodeMessage(mv.accept()))),
          isTrue);
      expect(mv.isDecline(mv.decodeMessage(mv.encodeMessage(mv.decline()))),
          isTrue);
      // distinct
      expect(mv.isDecline(mv.decodeMessage(mv.encodeMessage(mv.accept()))),
          isFalse);
    });

    test('two equal handshakes encode identically (the equality check basis)',
        () {
      final a = mv.encodeMessage(mv.handshake(hSrc, hSrcDef, 'root'));
      final b = mv.encodeMessage(mv.handshake(hSrc, hSrcDef, 'root'));
      expect(a, b);
    });
  });

  group('signed content e(sig(HSrc, Term))', () {
    test('equals the canonical encoding of sig(HSrc, Term)', () {
      final term = StructTerm('pay', [ConstTerm(42), ConstTerm('x')]);
      final bytes = mv.signedBytes(hSrc, term);
      final direct = mv.encodeMessage(mv.sigStructure(hSrc, term));
      expect(bytes, direct);
    });

    test('decodes back to sig(HSrc, Term) with the fixed functor', () {
      final term = StructTerm('pay', [ConstTerm(42)]);
      final decoded = mv.decodeMessage(mv.signedBytes(hSrc, term)) as StructTerm;
      expect(decoded.functor, mv.sigFunctor); // 'sig'
      expect(decoded.args.length, 2);
      expect((decoded.args[0] as ConstTerm).value, hSrc);
      final inner = decoded.args[1] as StructTerm;
      expect(inner.functor, 'pay');
    });

    test('deterministic and sensitive to the term', () {
      final t1 = ConstTerm('a');
      final t2 = ConstTerm('b');
      expect(mv.signedBytes(hSrc, t1), mv.signedBytes(hSrc, t1));
      expect(mv.signedBytes(hSrc, t1), isNot(mv.signedBytes(hSrc, t2)));
      // sensitive to the module identity too
      expect(mv.signedBytes(hSrc, t1), isNot(mv.signedBytes(hSrcDef, t1)));
    });
  });

  group('encoding shape', () {
    test('a 32-byte identity rides as a blob constant (tag 1, subtag 5)', () {
      // offer starts: tag 3 (structure) ... first arg is the HSrc blob.
      final bytes = mv.encodeMessage(mv.offer(hSrc, hSrcDef, 'ch'));
      expect(bytes[0], 0x03); // structure tag
      // Decoding yields the blob back intact (shape verified via round-trip).
      expect(mv.parseOffer(mv.decodeMessage(bytes)).hSrc.length, 32);
    });

    test('rejects a malformed message', () {
      expect(() => mv.parseShip(mv.offer(hSrc, hSrcDef, 'ch')),
          throwsA(isA<WireFormatException>()));
    });
  });
}
