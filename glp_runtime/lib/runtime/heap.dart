import 'cells.dart';
import 'terms.dart';

class Heap {
  final Map<int, WriterCell> writers = <int, WriterCell>{};
  final Map<int, ReaderCell> readers = <int, ReaderCell>{};

  // Writer bindings (value after commit/body)
  final Map<int, Term> writerValue = <int, Term>{};

  // Heap pointer for fresh variable allocation (WAM-style)
  // Increments with each writer/reader pair allocation
  int _nextId = 1000;

  WriterCell? writer(int id) => writers[id];
  ReaderCell? reader(int id) => readers[id];

  void addWriter(WriterCell w) => writers[w.writerId] = w;
  void addReader(ReaderCell r) => readers[r.readerId] = r;

  /// Allocate a fresh writer/reader pair (WAM-style heap allocation)
  /// Returns (writerId, readerId)
  (int, int) allocateFreshPair() {
    final writerId = _nextId++;
    final readerId = _nextId++;
    return (writerId, readerId);
  }

  /// Single-ID allocation (migration support)
  /// Returns a single variable ID
  /// Default implementation uses allocateFreshPair and returns writer ID
  int allocateFreshVar() {
    final (wid, _) = allocateFreshPair();
    return wid;
  }

  /// Add variable to heap (migration support)
  /// Default no-op implementation - adapter will override
  void addVariable(int varId) {
    // No-op in old two-ID system
  }

  bool containsWriter(int id) => writers.containsKey(id);
  bool containsReader(int id) => readers.containsKey(id);

  bool isWriterBound(int writerId) => writerValue.containsKey(writerId);
  Term? valueOfWriter(int writerId) => writerValue[writerId];

  void bindWriterConst(int writerId, Object? v) {
    writerValue[writerId] = ConstTerm(v);
  }

  void bindWriterStruct(int writerId, String f, List<Term> args) {
    // Dereference any ReaderTerms in args before storing
    final dereferencedArgs = args.map((arg) {
      if (arg is ReaderTerm) {
        // Dereference reader to get actual value
        final wid = writerIdForReader(arg.readerId);
        if (wid != null && isWriterBound(wid)) {
          return writerValue[wid]!;
        }
      }
      return arg;
    }).toList();

    writerValue[writerId] = StructTerm(f, dereferencedArgs);
  }

  // Helper: find the paired writerId for a given readerId (slow scan is fine for tests).
  int? writerIdForReader(int readerId) {
    for (final w in writers.values) {
      if (w.readerId == readerId) return w.writerId;
    }
    return null;
  }
}
