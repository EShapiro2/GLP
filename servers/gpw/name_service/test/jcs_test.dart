import 'package:gpw_name_service/gpw_name_service.dart';
import 'package:test/test.dart';

void main() {
  test('scalars', () {
    expect(jcsString(null), 'null');
    expect(jcsString(true), 'true');
    expect(jcsString(false), 'false');
    expect(jcsString(0), '0');
    expect(jcsString(-42), '-42');
    expect(jcsString('abc'), '"abc"');
  });

  test('rejects doubles', () {
    expect(() => jcsString(1.5), throwsArgumentError);
  });

  test('key sorting by UTF-16 code units', () {
    // From RFC 8785 §3.2.3 (restricted to our value space): digits sort
    // before uppercase, uppercase before lowercase.
    expect(
      jcsString({'b': 1, 'a': 2, 'A': 3, '10': 4, '1': 5}),
      '{"1":5,"10":4,"A":3,"a":2,"b":1}',
    );
  });

  test('string escapes per RFC 8785', () {
    expect(jcsString('\b\t\n\f\r"\\'), r'"\b\t\n\f\r\"\\"');
    expect(jcsString("\u0001"), r'"\u0001"');
    expect(jcsString("\u001f"), r'"\u001f"');
    expect(jcsString('€'), '"€"'); // literal UTF-8, no escaping
  });

  test('nested structure, no whitespace', () {
    expect(
      jcsString({
        'z': [1, 'two', null],
        'a': {
          'y': false,
          'x': <String, Object?>{},
        },
      }),
      '{"a":{"x":{},"y":false},"z":[1,"two",null]}',
    );
  });
}
