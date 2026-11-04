// Persons over the headless layer, named by their keys: GKK's introduction
// (Ben introduces Cleo to Ann, who then knows her, as the one-heap world
// has it), and
// GC's coins (Ann and Ben each mint, and swap).
import 'package:flutter_test/flutter_test.dart';

import 'people.dart';

void main() {
  test('GKK: Ben introduces Cleo to Ann', () async {
    final why = await cannotRun('gkk');
    if (why != null) return markTestSkipped(why);
    final [ann, ben, cleo] = await people('gkk', 3);
    final all = [ann, ben, cleo];
    for (final p in all) {
      await propose(p, S('establish', [p.me]));
    }
    await propose(ann, S('acquaint', [ann.me, ben.me]));
    await until(all, () => hasAtom(ann, 'knows', [ben.me]) && hasAtom(ben, 'knows', [ann.me]), 'Ann and Ben');
    await propose(ben, S('acquaint', [ben.me, cleo.me]));
    await until(all, () => hasAtom(cleo, 'knows', [ben.me]) && hasAtom(ben, 'knows', [cleo.me]), 'Ben and Cleo');
    await propose(ben, S('introduce', [ben.me, ann.me, cleo.me]));
    await until(all, () => hasAtom(ann, 'knows', [cleo.me]), 'Ann knowing Cleo, introduced');
  }, timeout: const Timeout(Duration(minutes: 8)));

  test('GC: Ann and Ben mint and swap', () async {
    final why = await cannotRun('gc');
    if (why != null) return markTestSkipped(why);
    final [ann, ben] = await people('gc', 2);
    final all = [ann, ben];
    for (final p in all) {
      await propose(p, S('establish', [p.me]));
    }
    await propose(ann, S('acquaint', [ann.me, ben.me]));
    await until(all, () => hasAtom(ann, 'knows', [ben.me]) && hasAtom(ben, 'knows', [ann.me]), 'Ann and Ben');
    await propose(ann, S('mint', [ann.me, 5]));
    await propose(ben, S('mint', [ben.me, 3]));
    await until(all, () => hasAtom(ann, 'coin', [ann.me]) && hasAtom(ben, 'coin', [ben.me]), 'the mints');
    await propose(ann, S('swap', [ann.me, ben.me, ann.me, 2, ben.me, 1]));
    await until(all, () => hasAtom(ann, 'coin', [ben.me]) && hasAtom(ben, 'coin', [ann.me]), 'the swap');
  }, timeout: const Timeout(Duration(minutes: 8)));
}
