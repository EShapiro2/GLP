/// The wire codecs keep the empty list and the string `nil` apart, by IGLP's
/// code format, Terms: "1 constant --- followed by a u8 constant tag and its
/// payload: 0 nil (no payload, the empty list) ... 3 string (string)"; "the
/// empty list is the constant nil".  Until 2026-10-07 the runtime held `[]`
/// as the string 'nil', and the codecs sent the string 'nil' as tag 0, so it
/// arrived as the empty list.
library;

import 'package:glp_runtime/runtime/terms.dart';
import 'package:glp_runtime/wire/codec.dart';
import 'package:glp_runtime/wire/payload_codec.dart';
import 'package:test/test.dart';

void main() {
  test('[] is constant tag 0 and the string nil tag 3', () {
    expect(encodeTermToBytes(PayloadCodec.termToWire(ConstTerm(nil))),
        [0x01, 0x00]);
    expect(encodeTermToBytes(PayloadCodec.termToWire(ConstTerm('nil'))),
        [0x01, 0x03, 0x03, 0x6E, 0x69, 0x6C]);
  });

  test('a term holding both comes back with both, each as itself', () {
    final sent = StructTerm('f', [
      ConstTerm(nil),
      ConstTerm('nil'),
      StructTerm('.', [ConstTerm('nil'), ConstTerm(nil)]),
    ]);
    final back = PayloadCodec.wireToTerm(decodeTermFromBytes(
        encodeTermToBytes(PayloadCodec.termToWire(sent)))) as StructTerm;
    expect((back.args[0] as ConstTerm).value, same(nil));
    expect((back.args[1] as ConstTerm).value, 'nil');
    final cell = back.args[2] as StructTerm;
    expect((cell.args[0] as ConstTerm).value, 'nil');
    expect((cell.args[1] as ConstTerm).value, same(nil));
  });
}
