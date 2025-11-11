import 'lib/runtime/heap_v2.dart';
import 'lib/runtime/terms.dart';
import 'lib/runtime/cells.dart';

void main() {
  final heap = HeapV2();
  
  // Simulate REPL allocation
  final (writerId, readerId) = heap.allocateFreshPair();
  print('Allocated: writer=$writerId, reader=$readerId');
  
  heap.addWriter(WriterCell(writerId, readerId));
  print('Added writer cell');
  
  print('isWriterBound($writerId) = ${heap.isWriterBound(writerId)}');
  print('isBound($writerId) = ${heap.isBound(writerId)}');
  print('getValue($writerId) = ${heap.getValue(writerId)}');
  
  // Now bind it
  heap.bindWriterConst(writerId, 'test');
  print('\nAfter binding to "test":');
  print('isWriterBound($writerId) = ${heap.isWriterBound(writerId)}');
  print('isBound($writerId) = ${heap.isBound(writerId)}');
  print('getValue($writerId) = ${heap.getValue(writerId)}');
}
