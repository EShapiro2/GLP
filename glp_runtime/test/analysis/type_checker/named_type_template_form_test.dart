// glp_runtime/test/analysis/type_checker/named_type_template_form_test.dart
//
// A named type is read as its template form when a call instantiates a
// parameterised procedure.  Spec: TGLP sections/parameterized-types.tex, after
// Definition "Instantiation": "type identity is structural, so two types with
// the same automaton bind the parameter consistently whatever their names or
// defining modules"; and Section "Expansion", "Expansion rule": the instance
// T(S1, ..., Sk) is the template's alternatives with each Xi replaced by Si.
// So `MsgChannel ::= ch(MsgStream, MsgStream?)` IS
// `Channel<MsgStream,MsgStream>`, and passed where a procedure declares
// `Channel(Stream(C), Stream(C))?` it instantiates C.  Until 2026-10-02 only a
// named LIST type was read so (`T ::= [] ; [E | T]` as Stream<E>), and a named
// channel type bound no parameter: the call recorded no instantiation and the
// inspecting procedure was refused "no call in the program instantiates it".
// GLP's task of 2026-10-01 23:58 UTC, item 3; the probe is
// /Users/udi/Grassroots/tmp/gsg-gap2-probe-channel.txt.
library;

import 'dart:io';

import 'package:glp_runtime/engine/glp_engine.dart';
import 'package:test/test.dart';

GlpEngine _engine() =>
    GlpEngine(rootSelfGlpPath: File('../programs/self.glp').absolute.path);

String _v(Object? term) =>
    '$term'.replaceAllMapped(RegExp(r'^Const\((.*)\)$'), (m) => m[1]!);

/// [peek] inspects its parameter C (it matches ack(X) and nack against it),
/// so it has no abstract certificate and is checked only per instantiation.
const _peek = '''
procedure(C) peek(Channel(Stream(C), Stream(C))?, Result).
peek(ch([ack(X)|_], []), got(X?)).
peek(ch([nack|_], []), none).
peek(ch([], []), none).
''';

const _types = '''
Msg ::= ack(Constant) ; nack.
Result ::= got(Constant) ; none.
''';

void main() {
  test('a named channel type instantiates the parameter of a Channel template',
      () async {
    final engine = _engine();
    expect(
        engine.loadSource('''
$_types
MsgStream ::= [] ; [Msg | MsgStream].
MsgChannel ::= ch(MsgStream, MsgStream?).
$_peek
procedure go(MsgChannel?, Result).
go(Ch, R?) :- peek(Ch?, R).
''', filename: 'named_channel.glp'),
        isTrue);
    final r = await engine.runGoal('go(ch([ack(x)], []), R)');
    expect(r.succeeded, isTrue, reason: '${r.error}');
    expect('${r.bindings['R']}', contains('got'));
    final n = await engine.runGoal('go(ch([nack], []), R)');
    expect(_v(n.bindings['R']), 'none');
  });

  test('the control: the caller declaring the template form', () async {
    final engine = _engine();
    expect(
        engine.loadSource('''
$_types
$_peek
procedure go(Channel(Stream(Msg), Stream(Msg))?, Result).
go(Ch, R?) :- peek(Ch?, R).
''', filename: 'template_channel.glp'),
        isTrue);
    final r = await engine.runGoal('go(ch([ack(x)], []), R)');
    expect(r.succeeded, isTrue, reason: '${r.error}');
  });

  test('a named list type is read as Stream of its element, as before', () async {
    final engine = _engine();
    expect(
        engine.loadSource('''
$_types
MsgStream ::= [] ; [Msg | MsgStream].
procedure(C) first(Stream(C)?, Result).
first([ack(X)|_], got(X?)).
first([nack|_], none).
first([], none).
procedure go(MsgStream?, Result).
go(S, R?) :- first(S?, R).
''', filename: 'named_stream.glp'),
        isTrue);
    final r = await engine.runGoal('go([ack(y)], R)');
    expect(r.succeeded, isTrue, reason: '${r.error}');
  });

  test('a named type of another polarity is read at that polarity, and refused',
      () {
    // ch(MsgStream?, MsgStream?) is Channel<MsgStream?,MsgStream>: its first
    // argument is an input type, which no Stream(C) is, and its second fixes
    // C = Msg; the call then hands a BadChannel where the instance accepts
    // Channel<Stream<Msg>,Stream<Msg>>, and condition 3(b) refuses it.
    final engine = _engine();
    expect(
        () => engine.loadSource('''
$_types
MsgStream ::= [] ; [Msg | MsgStream].
BadChannel ::= ch(MsgStream?, MsgStream?).
$_peek
procedure go(BadChannel?, Result).
go(Ch, R?) :- peek(Ch?, R).
''', filename: 'bad_channel.glp'),
        throwsA(predicate((e) =>
            '$e'.contains('BadChannel, which is not within') &&
            '$e'.contains('Channel<Stream<Msg>,Stream<Msg>>'))));
  });
}
