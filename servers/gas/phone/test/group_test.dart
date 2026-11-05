// A group chat in GSN (Grassroots Social Network), as the app's Groups tab
// draws it: Ann and Ben meet and befriend, Ann creates the group Family and
// adds Ben, Ben posts to it, and Ann, its leader, delivers his message to
// every member: Ann's view holds it posted, Ben's received in the group.
import 'package:flutter_test/flutter_test.dart';

import 'package:gas_core/person_host.dart';

import 'people.dart';

void main() {
  test('a group chat between two friends, in GSN', () async {
    final why = await cannotRun('gsn');
    if (why != null) return markTestSkipped(why);
    final [ann, ben] = await people('gsn', 2);
    final all = [ann, ben];
    Map<String, Object?> sp(String t, String who) => S('speech', [S('utterance', [t]), who]);
    bool holds(PersonHost p, String f, bool Function(List a) test) =>
        atoms(p).any((a) => a is Map && a['f'] == f && test(a['a'] as List));
    for (final p in all) {
      await propose(p, S('establish', [p.me]));
    }
    await propose(ann, S('acquaint', [ann.me, ben.me]));
    await until(all, () => hasAtom(ann, 'knows', [ben.me]) && hasAtom(ben, 'knows', [ann.me]), 'Ann and Ben acquainted');
    await propose(ann, S('befriend', [ann.me, ben.me]));
    await until(all, () => hasAtom(ann, 'friend', [ben.me]) && hasAtom(ben, 'friend', [ann.me]), 'Ann and Ben friends');
    final family = sp('family', ann.me);
    await propose(ann, S('create', [ann.me, family]));
    await until(all, () => holds(ann, 'member', (_) => true), 'Family created');
    await propose(ann, S('join', [ann.me, ben.me, family]));
    await until(all, () => holds(ben, 'member', (_) => true), 'Ben in Family');
    final hello = sp('hello family', ben.me);
    await propose(ben, S('post', [ben.me, ann.me, hello, family]));
    await until(all, () => holds(ann, 'posted', (a) => '${a[0]}' == '$hello'), 'Ann holds the post');
    await until(all, () => holds(ben, 'received_in', (a) => '${a[0]}' == '$hello' && a[1] == ann.me), 'Ben received it in Family');
  }, timeout: const Timeout(Duration(minutes: 5)));
}
