import 'package:test/test.dart';
import 'package:glp_runtime/runtime/heap_fcp.dart';
import 'package:glp_runtime/runtime/external_io.dart';
import 'package:glp_runtime/runtime/terms.dart';

void main() {
  test('buildChannelTerm creates ch(Reader, Writer)', () {
    final heap = HeapFCP();

    // Create user channel
    final userChannel = createExternalChannel(heap, 'user');
    print('User channel: $userChannel');
    print('  inputWriterAddr: ${userChannel.inputWriterAddr}');
    print('  inputReaderAddr: ${userChannel.inputReaderAddr}');
    print('  outputWriterAddr: ${userChannel.outputWriterAddr}');
    print('  outputReaderAddr: ${userChannel.outputReaderAddr}');

    // Build channel term
    final term = buildChannelTerm(userChannel);
    print('\nChannel term: $term');

    expect(term, isA<StructTerm>());
    final st = term as StructTerm;
    expect(st.functor, equals('ch'));
    expect(st.args.length, equals(2));

    // First arg should be input READER
    expect(st.args[0], isA<VarRef>());
    final arg0 = st.args[0] as VarRef;
    print('  arg[0]: VarRef(${arg0.addr}) isReader=${heap.isReader(arg0.addr)}');
    expect(heap.isReader(arg0.addr), isTrue, reason: 'First arg should be reader');
    expect(arg0.addr, equals(userChannel.inputReaderAddr));

    // Second arg should be output WRITER
    expect(st.args[1], isA<VarRef>());
    final arg1 = st.args[1] as VarRef;
    print('  arg[1]: VarRef(${arg1.addr}) isReader=${heap.isReader(arg1.addr)}');
    expect(heap.isReader(arg1.addr), isFalse, reason: 'Second arg should be writer');
    expect(arg1.addr, equals(userChannel.outputWriterAddr));

    print('\n✓ Channel term is ch(Reader, Writer) as expected');
  });
}
