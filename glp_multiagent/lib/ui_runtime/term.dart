/// Ground-term model and boundary parser/formatter.
///
/// The Dart/GLP boundary carries ground terms as GLP-syntax strings, both ways
/// (paper §7.4): a `UserNotify` arrives as text via `_output/1`, and a
/// `UserCmd` is sent as text and re-parsed by the agent runtime's `parseTerm`.
/// This file gives the generic UI runtime its own self-contained model of those
/// ground terms, so it never depends on GLP-runtime internals. It is the
/// "parser at the boundary, still ground-terms-only in spirit".
library;

/// A ground GLP term.
sealed class GTerm {
  const GTerm();
}

/// An atom: an identifier (`alice`, `yes`) or a quoted constant (`'_user'`).
class GAtom extends GTerm {
  final String name;
  const GAtom(this.name);
}

/// An integer constant.
class GInt extends GTerm {
  final int value;
  const GInt(this.value);
}

/// A string constant, `"..."` — GLP's `String`, which is not an atom: a
/// `text` widget over a `String` writer grants one of these, and over a
/// `Constant` writer an atom, so the widget alone does not settle the term.
class GString extends GTerm {
  final String value;
  const GString(this.value);
}

/// A compound term `functor(arg, ...)` with arity >= 1.
class GStruct extends GTerm {
  final String functor;
  final List<GTerm> args;
  const GStruct(this.functor, this.args);
}

/// A proper list `[a, b, c]`.
class GList extends GTerm {
  final List<GTerm> items;
  const GList(this.items);
}

/// The functor name and argument list of any term.
///
/// Atoms are arity-0 constructors; integers and lists have no constructor name
/// and yield an empty functor (they never match a notify descriptor).
(String functor, List<GTerm> args) ctorArgs(GTerm t) {
  return switch (t) {
    GAtom(:final name) => (name, const []),
    GStruct(:final functor, :final args) => (functor, args),
    GInt() => ('', const []),
    GString() => ('', const []),
    GList() => ('', const []),
  };
}

/// Render a ground term as GLP source text, suitable for the command boundary.
String formatTerm(GTerm t) {
  switch (t) {
    case GAtom(:final name):
      return name;
    case GInt(:final value):
      return '$value';
    case GString(:final value):
      return '"$value"';
    case GStruct(:final functor, :final args):
      return '$functor(${args.map(formatTerm).join(', ')})';
    case GList(:final items):
      return '[${items.map(formatTerm).join(', ')}]';
  }
}

/// Parse one ground term from [s], requiring the whole string to be consumed.
///
/// Returns `null` when [s] is not a single well-formed ground term — e.g. log
/// lines like `[INIT] Loaded ...` (trailing text) or `> connect(bob)` (leading
/// prompt). The caller uses this to separate notify terms from log noise.
GTerm? tryParseTerm(String s) {
  final p = _Parser(s);
  try {
    p._skipWs();
    final t = p._term();
    p._skipWs();
    if (!p._atEnd) return null;
    return t;
  } catch (_) {
    return null;
  }
}

class _ParseError implements Exception {}

class _Parser {
  final String _s;
  int _i = 0;
  _Parser(this._s);

  bool get _atEnd => _i >= _s.length;

  void _skipWs() {
    while (!_atEnd) {
      final c = _s[_i];
      if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
        _i++;
      } else {
        break;
      }
    }
  }

  Never _fail() => throw _ParseError();

  GTerm _term() {
    _skipWs();
    if (_atEnd) _fail();
    final c = _s[_i];
    if (c == '[') return _list();
    if (c == '"') return _string();
    if (c == "'") return _quotedAtomOrStruct();
    if (c == '-' || _isDigit(c)) return _number();
    if (_isIdentStart(c)) return _identAtomOrStruct();
    _fail();
  }

  GTerm _list() {
    _expect('[');
    _skipWs();
    final items = <GTerm>[];
    if (_peekChar() == ']') {
      _i++;
      return const GList([]);
    }
    items.add(_term());
    _skipWs();
    while (_peekChar() == ',') {
      _i++;
      items.add(_term());
      _skipWs();
    }
    // A tail `[H | T]` is not expected in ground notifies; reject it.
    if (_peekChar() != ']') _fail();
    _i++;
    return GList(items);
  }

  GTerm _string() {
    _expect('"');
    final sb = StringBuffer();
    while (!_atEnd && _s[_i] != '"') {
      sb.write(_s[_i]);
      _i++;
    }
    if (_atEnd) _fail();
    _i++; // closing quote
    return GString(sb.toString());
  }

  GTerm _number() {
    final start = _i;
    if (_peekChar() == '-') _i++;
    if (!_isDigit(_s[_i])) _fail();
    while (!_atEnd && _isDigit(_s[_i])) {
      _i++;
    }
    return GInt(int.parse(_s.substring(start, _i)));
  }

  GTerm _quotedAtomOrStruct() {
    _expect("'");
    final sb = StringBuffer();
    while (!_atEnd && _s[_i] != "'") {
      sb.write(_s[_i]);
      _i++;
    }
    if (_atEnd) _fail();
    _i++; // closing quote
    return _maybeStruct(sb.toString());
  }

  GTerm _identAtomOrStruct() {
    final start = _i;
    while (!_atEnd && _isIdentPart(_s[_i])) {
      _i++;
    }
    return _maybeStruct(_s.substring(start, _i));
  }

  GTerm _maybeStruct(String name) {
    if (_peekChar() == '(') {
      _i++;
      final args = <GTerm>[];
      args.add(_term());
      _skipWs();
      while (_peekChar() == ',') {
        _i++;
        args.add(_term());
        _skipWs();
      }
      if (_peekChar() != ')') _fail();
      _i++;
      return GStruct(name, args);
    }
    return GAtom(name);
  }

  void _expect(String ch) {
    if (_atEnd || _s[_i] != ch) _fail();
    _i++;
  }

  String? _peekChar() {
    _skipWs();
    return _atEnd ? null : _s[_i];
  }

  static bool _isDigit(String c) => c.codeUnitAt(0) >= 0x30 && c.codeUnitAt(0) <= 0x39;

  static bool _isIdentStart(String c) {
    final u = c.codeUnitAt(0);
    return (u >= 0x61 && u <= 0x7a) || (u >= 0x41 && u <= 0x5a) || c == '_';
  }

  static bool _isIdentPart(String c) {
    return _isIdentStart(c) || _isDigit(c);
  }
}

// =============================================================================
// Patterns — a display declaration's message pattern (vGLP, Definition
// "Display Declaration")
// =============================================================================

/// A message pattern of a display declaration, e.g.
/// `msg(agent, person, holdings(Lots))`: the same ground-term syntax, with a
/// capitalised identifier standing for a variable that binds what it matches.
/// A declaration names the messages it views by such a pattern, so the runtime
/// selects a view without naming any constructor of its own.
class GPattern {
  final GTerm term;
  const GPattern(this.term);

  /// Parse [source] as a pattern. Returns null if it is not a well-formed term.
  static GPattern? parse(String source) {
    final t = tryParseTerm(source);
    return t == null ? null : GPattern(t);
  }

  /// The variable names occurring in the pattern, in order of occurrence.
  List<String> get variables {
    final out = <String>[];
    void walk(GTerm t) {
      switch (t) {
        case GAtom(:final name):
          if (isPatternVariable(name) && !out.contains(name)) out.add(name);
        case GStruct(:final args):
          args.forEach(walk);
        case GList(:final items):
          items.forEach(walk);
        case GInt():
        case GString():
          break;
      }
    }

    walk(term);
    return out;
  }

  /// Match [t] against this pattern, returning the bindings of its variables,
  /// or null where it does not match.
  Map<String, GTerm>? match(GTerm t) {
    final out = <String, GTerm>{};
    return _match(term, t, out) ? out : null;
  }
}

/// Whether an identifier read off a pattern is a variable: GLP spells a
/// variable with a leading capital or underscore, a constant without one.
bool isPatternVariable(String name) {
  if (name.isEmpty) return false;
  final u = name.codeUnitAt(0);
  return name[0] == '_' || (u >= 0x41 && u <= 0x5a);
}

bool _match(GTerm pattern, GTerm t, Map<String, GTerm> out) {
  if (pattern is GAtom && isPatternVariable(pattern.name)) {
    if (pattern.name == '_') return true;
    final seen = out[pattern.name];
    if (seen != null) return formatTerm(seen) == formatTerm(t);
    out[pattern.name] = t;
    return true;
  }
  switch (pattern) {
    case GAtom(:final name):
      return t is GAtom && t.name == name;
    case GInt(:final value):
      return t is GInt && t.value == value;
    case GString(:final value):
      return t is GString && t.value == value;
    case GStruct(:final functor, :final args):
      if (t is! GStruct || t.functor != functor || t.args.length != args.length) {
        return false;
      }
      for (var i = 0; i < args.length; i++) {
        if (!_match(args[i], t.args[i], out)) return false;
      }
      return true;
    case GList(:final items):
      if (t is! GList || t.items.length != items.length) return false;
      for (var i = 0; i < items.length; i++) {
        if (!_match(items[i], t.items[i], out)) return false;
      }
      return true;
  }
}
