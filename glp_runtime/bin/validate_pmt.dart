/// Quick script to validate a GLP file using PMT
import 'dart:io';
import 'package:glp_runtime/compiler/pmt/validator.dart';

void main(List<String> args) {
  if (args.isEmpty) {
    print('Usage: dart run bin/validate_pmt.dart <file.glp>');
    exit(1);
  }

  final file = File(args[0]);
  if (!file.existsSync()) {
    print('File not found: ${args[0]}');
    exit(1);
  }

  final source = file.readAsStringSync();
  print('Validating: ${args[0]}');
  print('=' * 50);

  final errors = PmtValidator.validateSource(source);

  if (errors.isEmpty) {
    print('✓ PASS - No PMT errors found');
  } else {
    print('✗ FAIL - ${errors.length} PMT error(s) found:\n');
    for (final error in errors) {
      print('  $error');
    }
  }
}
