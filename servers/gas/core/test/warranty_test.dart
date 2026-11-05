// A package's warranties (lib/warranty.dart): GKK's contract unit, warranted
// by a compiler and a creator, checks; changed code, changed schemas, a
// forged signature, and a unit that reaches the person do not.  Needs the
// worlds built (python3 -I ../tools/build.py); self-skips without them.
import 'dart:io';
import 'dart:typed_data';

import 'package:gas_core/warranty.dart';
import 'package:glp_runtime/multiagent/identity.dart';
import 'package:test/test.dart';

void main() {
  final programs = Directory('../../../programs').absolute.path;
  final world = '$programs/vglp/cva/gas_worlds/live_gkk';
  final cascade = Directory('../../../cascade/GKK').absolute.path;

  Map<String, Object?> files() => {
        for (final f in contractFiles) f: File('$world/$f').readAsStringSync(),
      };

  final compiler = PersonIdentity.fromSeed(Uint8List.fromList(List.filled(32, 1)));
  final creator = PersonIdentity.fromSeed(Uint8List.fromList(List.filled(32, 2)));

  Map<String, Object?> warranted(Map<String, Object?> fs) => warrant(
        programs: programs,
        id: 'GKK',
        files: fs,
        text: File('$cascade/GKK.txt').readAsStringSync(),
        schemas: File('$cascade/schemas.txt').readAsStringSync(),
        verdict: File('$cascade/verdict.txt').readAsStringSync().trim(),
        compiler: compiler,
        creator: creator,
      );

  test('warranties of GKK check, and what is changed does not', () {
    if (!Directory(world).existsSync()) return markTestSkipped('no worlds: run python3 -I ../tools/build.py');
    final w = warranted(files());
    final ok = checkWarranty(programs, 'GKK', files(), w);
    expect(ok.compiler, compiler.pub.hex);
    expect(ok.creator, creator.pub.hex);
    expect(ok.verdict, startsWith(grassroots));

    // A comment changed is the same program: its source identity is of the
    // linked program, not of the text.
    final commented = files();
    commented['contract/platform.vglp'] = '%% changed\n${commented['contract/platform.vglp']}';
    expect(checkWarranty(programs, 'GKK', commented, w).hSrc, ok.hSrc);

    // Other code: a tab of the platform named otherwise.
    final other = files();
    other['contract/platform.vglp'] =
        (other['contract/platform.vglp'] as String).replaceFirst('tab("Contacts", knows)', 'tab("People", knows)');
    expect(() => checkWarranty(programs, 'GKK', other, w),
        throwsA(isA<FormatException>().having((e) => e.message, 'message', contains('other code'))));

    // Other schemas than the creator signed.
    final ws = {...w, 'contract': {...(w['contract'] as Map), 'schemas': 'establish(Alice?) :\n'}};
    expect(() => checkWarranty(programs, 'GKK', files(), ws),
        throwsA(isA<FormatException>().having((e) => e.message, 'message', contains('creator'))));

    // A forged compiler's signature.
    final comp = Map<String, Object?>.from(w['compiler'] as Map);
    final sig = comp['sig'] as String;
    comp['sig'] = '${sig.substring(0, sig.length - 2)}${sig.endsWith('00') ? '01' : '00'}';
    expect(() => checkWarranty(programs, 'GKK', files(), {...w, 'compiler': comp}),
        throwsA(isA<FormatException>().having((e) => e.message, 'message', contains('signature'))));
  }, timeout: const Timeout(Duration(minutes: 5)));

  test('a contract unit that reaches the person is refused a warranty, and not installed', () {
    if (!Directory(world).existsSync()) return markTestSkipped('no worlds: run python3 -I ../tools/build.py');
    final fs = files();
    // The person's draws written straight to the person, by send_to_user/1.
    fs['contract/self.glp'] = (fs['contract/self.glp'] as String).replaceFirst(
        '    to_person(M?, T?).', '    to_person(M?, T?),\n    send_to_user([hello]).');
    expect(() => warranted(fs),
        throwsA(isA<FormatException>().having((e) => e.message, 'message', contains('reaches'))));
    final w = warranted(files());
    expect(() => checkWarranty(programs, 'GKK', fs, w),
        throwsA(isA<FormatException>().having((e) => e.message, 'message', contains('reaches'))));
  }, timeout: const Timeout(Duration(minutes: 5)));
}
