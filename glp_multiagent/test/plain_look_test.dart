import 'package:flutter_test/flutter_test.dart';

import 'package:glp_multiagent/ui_runtime/plain_look.dart';
import 'package:glp_multiagent/ui_runtime/term.dart';

/// The plain look writes names in words and keys short, and writes nothing
/// differently while it is off (plain_look.dart).
void main() {
  tearDown(() => plainLook.value = false);

  const key =
      '63d368ee6df60e25adb0567238cc9e4ad2eb552b02a4316027007adb30b16f58';

  test('off, every text is written as it is', () {
    plainLook.value = false;
    expect(plainWords('open_place'), 'open_place');
    expect(plainWords('NoticeContent'), 'NoticeContent');
    expect(plainWords(key), key);
    expect(plainDisplay(GStruct('befriend', [GAtom(key)])), displayText(GStruct('befriend', [GAtom(key)])));
  });

  test('on, a name is written in words', () {
    plainLook.value = true;
    expect(plainWords('open_place'), 'Open place');
    expect(plainWords('list_unlisted'), 'List unlisted');
    expect(plainWords('NoticeContent'), 'Notice content');
    expect(plainWords('Key'), 'Key');
    expect(plainWords('trust'), 'Trust');
  });

  test('on, a key is written by its first eight digits', () {
    plainLook.value = true;
    expect(plainWords(key), '63d368ee…');
    expect(plainDisplay(GAtom(key)), '63d368ee…');
  });

  test('on, what is not a name or a key is written as it is', () {
    plainLook.value = true;
    expect(plainWords('Hi Bob, this is Alice'), 'Hi Bob, this is Alice');
    expect(plainWords('12'), '12');
    expect(plainWords(''), '');
  });
}
