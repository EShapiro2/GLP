/// Minimal cell tags (extend later as needed).
enum CellTag { writer, reader }

/// Abstract cell.
abstract class Cell {
  CellTag get tag;
}

/// Writable endpoint cell (WR). Paired to a ReaderId (RO) in the heap model.
/// For now, we carry only identifiers; actual heap/value comes later.
class WriterCell implements Cell {
  @override
  final CellTag tag = CellTag.writer;
  final int writerId;   // WriterId
  final int readerId;   // ReaderId (pair)
  bool abandoned = false;

  WriterCell(this.writerId, this.readerId);
}

/// Read-only endpoint cell (RO). Owns a suspension-queue identity.
class ReaderCell implements Cell {
  @override
  final CellTag tag = CellTag.reader;
  final int readerId;   // ReaderId

  ReaderCell(this.readerId);
}
