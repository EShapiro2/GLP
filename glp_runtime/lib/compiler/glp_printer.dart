// GLP Printer - Converts AST back to GLP source code
//
// This serializer produces valid GLP source from AST nodes,
// preserving SRSW annotations (X vs X?) and all term types.

import 'ast.dart';
import 'lexer.dart';
import 'parser.dart';
import 'token.dart';

// A term is printed so that it reads back as itself (GLP-Spec appendix-lp.tex,
// Definition "Logic Programs Syntax": the text denotes the term; GLP #3
// Cowork, 2026-10-04 09:06 UTC, "23:49. Q1 and Q3"): a constant in single
// quotes exactly where unquoted it would read as a variable, an operator or a
// number, or would not read as one name at all, escaped as the reader reads a
// quoted name (lexer.dart, `_string`).  Until 2026-10-04 GlpPrinter printed a
// constant that is not a lowercase identifier as a double-quoted string, which
// reads back as a string literal --- `'G'` and `'+'` came out `"G"` and `"+"`
// --- and every functor and predicate name bare, `'_send'(...)` as
// `_send(...)`, which reads back as a variable.  The REPL's display prints its
// constants and functors by the same functions (bin/glp_repl.dart).

/// A name the lexer reads bare as that one name: a lower-case letter and then
/// letters, digits and `_` (lexer.dart, `_identifier`), and not one of the two
/// words it makes tokens of their own, `mod` and `procedure`.
final RegExp _plainName = RegExp(r'^[a-z][A-Za-z0-9_]*$');
const Set<String> _keywords = {'mod', 'procedure'};

/// [name] as source that reads back as the constant of that name: bare where
/// the lexer reads it bare as that one name, and in single quotes otherwise ---
/// where unquoted it would read as a variable (`G`, `_x`), an operator or a
/// keyword (`+`, `=..`, `mod`, `procedure`), a number (`42`), or as no one
/// name (`a b`, `[]`, `it's`).  Also the name of a predicate, in a clause head
/// or a goal, where the reader takes no operator name for one.
String constantNameSource(String name) =>
    _plainName.hasMatch(name) && !_keywords.contains(name)
        ? name
        : _singleQuoted(name);

/// [name] as the functor of a compound term, written before "(": bare where
/// the reader reads it there as that functor --- a name the lexer reads bare,
/// `mod` among them, or an operator name or keyword, which where a term is
/// expected the reader takes as a functor before "(" (parser.dart,
/// [Parser.isOperatorName]) --- and in single quotes otherwise.
String functorNameSource(String name) {
  if (_plainName.hasMatch(name)) return name;
  return _functorNames.putIfAbsent(name, () {
    try {
      final t = Lexer('$name(').tokenize();
      if (t.length == 3 &&
          t[0].lexeme == name &&
          Parser.isOperatorName(t[0].type) &&
          t[1].type == TokenType.LPAREN) {
        return name;
      }
    } catch (_) {
      // Text the lexer refuses is no name it reads bare.
    }
    return _singleQuoted(name);
  });
}

final Map<String, String> _functorNames = {};

/// A constant's [value] as the AST and the runtime hold it, as source: a
/// number as written, a string literal --- whose value carries its double
/// quotes --- in double quotes, escaped as the reader reads one, and a name by
/// [constantNameSource].  The runtime's empty list, the value `nil`, is the
/// caller's to print.
String constantSource(Object value) {
  if (value is String) {
    if (value.length >= 2 && value.startsWith('"') && value.endsWith('"')) {
      return '"${_escaped(value.substring(1, value.length - 1), '"')}"';
    }
    return constantNameSource(value);
  }
  return value.toString();
}

String _singleQuoted(String name) => "'${_escaped(name, "'")}'";

/// [s] escaped inside [quote]s as the lexer reads a quoted name or string
/// back: a backslash, the quote, newline, tab and carriage return by their
/// escapes.
String _escaped(String s, String quote) => s
    .replaceAll('\\', '\\\\')
    .replaceAll(quote, '\\$quote')
    .replaceAll('\n', '\\n')
    .replaceAll('\t', '\\t')
    .replaceAll('\r', '\\r');

/// Converts GLP AST back to source code
class GlpPrinter {
  /// Print a complete program
  String printProgram(Program program) {
    final buffer = StringBuffer();

    for (final procedure in program.procedures) {
      buffer.write(printProcedure(procedure));
      buffer.writeln();
    }

    return buffer.toString();
  }

  /// Print a procedure (all clauses)
  String printProcedure(Procedure procedure) {
    final buffer = StringBuffer();

    for (final clause in procedure.clauses) {
      buffer.writeln(printClause(clause));
    }

    return buffer.toString();
  }

  /// Print a single clause
  String printClause(Clause clause) {
    final buffer = StringBuffer();

    // Head
    buffer.write(printAtom(clause.head));

    // Guards and body
    final hasGuards = clause.guards != null && clause.guards!.isNotEmpty;
    final hasBody = clause.body != null && clause.body!.isNotEmpty;

    if (hasGuards || hasBody) {
      buffer.write(' :- ');

      // Guards
      if (hasGuards) {
        buffer.write(clause.guards!.map(printGuard).join(', '));
      }

      // Body separator - only use | if there are guards
      if (hasBody) {
        if (hasGuards) {
          buffer.write(' | ');
        }
        buffer.write(clause.body!.map(printGoal).join(', '));
      }
    }

    buffer.write('.');
    return buffer.toString();
  }

  /// Print an atom (clause head)
  String printAtom(Atom atom) {
    if (atom.args.isEmpty) {
      return constantNameSource(atom.functor);
    }

    // Handle special infix operators
    if (_isInfixOperator(atom.functor) && atom.args.length == 2) {
      return '${printTerm(atom.args[0])} ${atom.functor} ${printTerm(atom.args[1])}';
    }

    return '${constantNameSource(atom.functor)}(${atom.args.map(printTerm).join(', ')})';
  }

  /// Print a goal (body call)
  String printGoal(Goal goal) {
    // Handle remote goals
    if (goal is RemoteGoal) {
      return '${printTerm(goal.module)} # ${printGoal(goal.goal)}';
    }

    // Handle spawn goals
    if (goal is SpawnGoal) {
      return '${printGoal(goal.innerGoal)}@${constantNameSource(goal.agentId)}';
    }

    if (goal.args.isEmpty) {
      return constantNameSource(goal.functor);
    }

    // Handle special infix operators
    if (_isInfixOperator(goal.functor) && goal.args.length == 2) {
      return '${printTerm(goal.args[0])} ${goal.functor} ${printTerm(goal.args[1])}';
    }

    return '${constantNameSource(goal.functor)}(${goal.args.map(printTerm).join(', ')})';
  }

  /// Print a guard
  String printGuard(Guard guard) {
    if (guard.args.isEmpty) {
      return constantNameSource(guard.predicate);
    }

    // Handle special infix operators
    if (_isInfixGuardOperator(guard.predicate) && guard.args.length == 2) {
      return '(${printTerm(guard.args[0])} ${guard.predicate} ${printTerm(guard.args[1])})';
    }

    return '${constantNameSource(guard.predicate)}(${guard.args.map(printTerm).join(', ')})';
  }

  /// Print a term
  String printTerm(Term term) {
    if (term is VarTerm) {
      return term.isReader ? '${term.name}?' : term.name;
    }

    // The anonymous variable as written: `_`, and `_?`, the output placeholder
    // a head's produced position carries (TGLP typed-glp.tex, "Anonymous
    // variables").  Until 2026-10-02 `_?` printed `_`, a writer at a produced
    // position, which is another clause, and one the type checker refuses.
    if (term is UnderscoreTerm) {
      return term.isReader ? '_?' : '_';
    }

    if (term is ConstTerm) {
      return _printConstValue(term.value);
    }

    if (term is ListTerm) {
      return _printList(term);
    }

    if (term is StructTerm) {
      return _printStruct(term);
    }

    // Fallback
    return term.toString();
  }

  /// Print a constant value ([constantSource]).
  String _printConstValue(Object? value) {
    if (value == null) {
      return 'null';
    }
    return constantSource(value);
  }

  /// Print a list term
  String _printList(ListTerm list) {
    if (list.isNil) {
      return '[]';
    }

    // Collect elements if it's a proper list
    final elements = <Term>[];
    Term? current = list;
    Term? tail;

    while (current is ListTerm && !current.isNil) {
      if (current.head != null) {
        elements.add(current.head!);
      }
      if (current.tail == null) {
        break;
      }
      if (current.tail is ListTerm) {
        current = current.tail;
      } else {
        // Improper list with non-list tail
        tail = current.tail;
        break;
      }
    }

    if (tail != null) {
      // Improper list: [a, b | T]
      return '[${elements.map(printTerm).join(', ')} | ${printTerm(tail)}]';
    } else {
      // Proper list: [a, b, c]
      return '[${elements.map(printTerm).join(', ')}]';
    }
  }

  /// Print a structure term
  String _printStruct(StructTerm struct) {
    // Handle comma/conjunction specially - no functor prefix
    if (struct.functor == ',' && struct.args.length == 2) {
      return '(${printTerm(struct.args[0])}, ${printTerm(struct.args[1])})';
    }

    // An infix operator the term reader reads infix, in parentheses; any
    // other operator name before "(", where the reader takes it as the
    // functor ([functorNameSource]).
    if (_termInfixOperators.contains(struct.functor) &&
        struct.args.length == 2) {
      return '(${printTerm(struct.args[0])} ${struct.functor} ${printTerm(struct.args[1])})';
    }

    // A structure of no arguments, `f()`, which the reader reads apart from
    // the constant `f`: until 2026-10-04 it printed `f`, the constant.
    return '${functorNameSource(struct.functor)}(${struct.args.map(printTerm).join(', ')})';
  }

  /// Check if a functor is an infix operator
  bool _isInfixOperator(String functor) {
    const infixOps = {
      ':=', '=', '\\=', '=..',
      '+', '-', '*', '/', '//', 'mod',
      '<', '>', '=<', '>=', '=:=', '=\\=',
      '=?=', '=?\\=',
    };
    return infixOps.contains(functor);
  }

  /// The operators the reader reads infix where a term is expected
  /// (parser.dart, `_isOperator`), `#` and the backslash apart, which print
  /// before "(" as they did.  Until 2026-10-04 `:=`, `=..`, `=?=`, `=?\=` and
  /// `\=`, which the lexer reads as no one token, were printed infix inside a
  /// term too, where the reader does not read them so.
  static const Set<String> _termInfixOperators = {
    '=', '+', '-', '*', '/', '//', 'mod',
    '<', '>', '=<', '>=', '=:=', '=\\=',
  };

  /// Check if a guard predicate is infix
  bool _isInfixGuardOperator(String predicate) {
    const infixGuards = {
      '<', '>', '=<', '>=', '=:=', '=\\=', '=?=', '=?\\=',
    };
    return infixGuards.contains(predicate);
  }
}
