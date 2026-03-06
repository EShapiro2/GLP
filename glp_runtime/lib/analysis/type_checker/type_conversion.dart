// lib/analysis/type_checker/type_conversion.dart
//
// Converts Term AST (from unified parser) to TypeExpr AST for type definitions.
// Per spec: /Users/udi/GLP/docs/type system/type-conversion.md

import '../../compiler/ast.dart';
import 'type_ast.dart';

/// Converts a Term AST node to a TypeExpr AST node.
/// 
/// This is a pure structural conversion with no semantic validation.
/// Semantic checks (determinism, alias prohibition) happen in type_environment.
TypeExpr termToTypeExpr(Term term) {
  if (term is VarTerm) {
    // Variable → TypeRef
    return TypeRef(term.name, term.line, term.column, isInput: term.isReader);
  }
  
  if (term is UnderscoreTerm) {
    // Anonymous _ or _? → PrimitiveModeAlt
    return PrimitiveModeAlt(term.isReader, term.line, term.column);
  }
  
  if (term is ConstTerm) {
    // Constant → ConstantAlt
    // ConstTerm.value is Object? but ConstantAlt expects Object
    final value = term.value ?? '';
    return ConstantAlt(value, term.line, term.column);
  }
  
  if (term is ListTerm) {
    // Empty list
    if (term.head == null && term.tail == null) {
      return ListNilAlt(term.line, term.column);
    }
    // List cons
    return ListConsAlt(
      termToTypeExpr(term.head!),
      termToTypeExpr(term.tail!),
      term.line,
      term.column,
    );
  }
  
  if (term is StructTerm) {
    // Difference list: A \ B
    if (term.functor == '\\' && term.args.length == 2) {
      return DiffListAlt(
        termToTypeExpr(term.args[0]),
        termToTypeExpr(term.args[1]),
        term.line,
        term.column,
      );
    }

    // Parameterized type reference — uppercase-initial functor (possibly with trailing '?')
    // e.g., Stream(X), Channel(In, Out), Stream(X)? (encoded as "Stream?")
    // In type definition bodies, uppercase functor with arguments is always a parameterized
    // type reference, never a struct alternative (struct functors are lowercase atoms).
    var functor = term.functor;
    bool isInput = false;
    if (functor.endsWith('?')) {
      functor = functor.substring(0, functor.length - 1);
      isInput = true;
    }
    if (functor.isNotEmpty && _isUppercaseLetter(functor[0])) {
      return TypeRef(functor, term.line, term.column,
          isInput: isInput,
          typeArgs: term.args.map(termToTypeExpr).toList());
    }

    // Regular structure (lowercase functor, including conjunction ',')
    return StructAlt(
      term.functor,
      term.args.map(termToTypeExpr).toList(),
      term.line,
      term.column,
    );
  }
  
  // Should not reach here for valid terms
  throw ArgumentError('Cannot convert term to type expression: $term');
}

/// Check if a character is an uppercase letter (A-Z).
/// Excludes operators (+, -, etc.) and underscore which satisfy toUpperCase() == self.
bool _isUppercaseLetter(String ch) {
  return ch.codeUnitAt(0) >= 65 && ch.codeUnitAt(0) <= 90; // A-Z
}
