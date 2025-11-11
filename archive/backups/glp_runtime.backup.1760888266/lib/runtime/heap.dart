import 'cells.dart';
import 'terms.dart';

class Heap {
  final Map<int, WriterCell> writers = <int, WriterCell>{};
  final Map<int, ReaderCell> readers = <int, ReaderCell>{};

  // Writer bindings (value after commit/body)
  final Map<int, Term> writerValue = <int, Term>{};

  WriterCell? writer(int id) => writers[id];
  ReaderCell? reader(int id) => readers[id];

  void addWriter(WriterCell w) => writers[w.writerId] = w;
  void addReader(ReaderCell r) => readers[r.readerId] = r;

  bool containsWriter(int id) => writers.containsKey(id);
  bool containsReader(int id) => readers.containsKey(id);

  bool isWriterBound(int writerId) => writerValue.containsKey(writerId);
  Term? valueOfWriter(int writerId) => writerValue[writerId];

  void bindWriterConst(int writerId, Object? v) {
    writerValue[writerId] = ConstTerm(v);
  }

  void bindWriterStruct(int writerId, String f, List<Term> args) {
    writerValue[writerId] = StructTerm(f, args);
  }

  // Helper: find the paired writerId for a given readerId (slow scan is fine for tests).
  int? writerIdForReader(int readerId) {
    for (final w in writers.values) {
      if (w.readerId == readerId) return w.writerId;
    }
    return null;
  }
}
