abstract class Term {}

class ConstTerm implements Term {
  final Object? value;
  ConstTerm(this.value);
  @override
  String toString() => 'Const($value)';
}

class StructTerm implements Term {
  final String functor;
  final List<Term> args;
  StructTerm(this.functor, this.args);
  @override
  String toString() => '$functor(${args.join(",")})';
}

/// Reader reference in a structure - just holds the reader ID
/// The actual value is obtained by dereferencing through the heap
class ReaderTerm implements Term {
  final int readerId;
  ReaderTerm(this.readerId);
  @override
  String toString() => 'R$readerId';
}

/// Writer reference in a structure - just holds the writer ID
/// Used when a structure contains an unbound writer variable
class WriterTerm implements Term {
  final int writerId;
  WriterTerm(this.writerId);
  @override
  String toString() => 'W$writerId';
}
