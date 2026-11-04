// Terms to and from the JSON the GAS app page reads and writes, as
// servers/gas/bin/server.dart converts them: a constant as itself, a list as
// a JSON list, a structure as {f, a}.
import 'package:glp_runtime/runtime/terms.dart' as rt;

rt.Term listOf(List<rt.Term> items) =>
    items.reversed.fold<rt.Term>(rt.ConstTerm(rt.nil), (tail, x) => rt.StructTerm('.', [x, tail]));

Object? toJson(rt.Term t) {
  if (t is rt.ConstTerm) {
    if (t.value == rt.nil || t.value == null) return <Object?>[];
    final v = t.value;
    if (v is num || v is String || v is bool) return v;
    return v.toString();
  }
  if (t is rt.StructTerm) {
    if (t.functor == '.' && t.args.length == 2) {
      final out = <Object?>[];
      rt.Term cur = t;
      while (cur is rt.StructTerm && cur.functor == '.' && cur.args.length == 2) {
        out.add(toJson(cur.args[0]));
        cur = cur.args[1];
      }
      if (!(cur is rt.ConstTerm && (cur.value == rt.nil || cur.value == null))) out.add({'tail': toJson(cur)});
      return out;
    }
    return {'f': t.functor, 'a': [for (final a in t.args) toJson(a)]};
  }
  return {'v': 1};
}

rt.Term fromJson(Object? j) {
  if (j is List) return listOf([for (final x in j) fromJson(x)]);
  if (j is Map) return rt.StructTerm(j['f'] as String, [for (final a in (j['a'] as List)) fromJson(a)]);
  return rt.ConstTerm(j);
}
