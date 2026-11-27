/// A person's code, drawn as a QR code another phone scans, and the scanner that reads one: how a value a UI style
/// marks `IS SCANNED` (a key, given face to face) is put in a field without typing it (described.dart).
library;

import 'package:flutter/material.dart';
import 'package:mobile_scanner/mobile_scanner.dart';
import 'package:qr/qr.dart';

/// [data] drawn as a QR code, [size] wide, its quiet zone included.
class QrView extends StatelessWidget {
  final String data;
  final double size;
  const QrView(this.data, {super.key, this.size = 220});

  @override
  Widget build(BuildContext context) {
    final image = QrImage(QrCode(payload: QrPayload.fromString(data), errorCorrectLevel: QrErrorCorrectLevel.medium));
    return Container(
      color: Colors.white,
      width: size,
      height: size,
      child: CustomPaint(painter: _QrPainter(image)),
    );
  }
}

class _QrPainter extends CustomPainter {
  final QrImage image;
  _QrPainter(this.image);

  @override
  void paint(Canvas canvas, Size size) {
    final n = image.moduleCount, quiet = 4, cell = size.width / (n + 2 * quiet), dark = Paint()..color = Colors.black;
    for (var r = 0; r < n; r++) {
      for (var c = 0; c < n; c++) {
        if (image.isDark(r, c)) {
          canvas.drawRect(Rect.fromLTWH((c + quiet) * cell, (r + quiet) * cell, cell + 0.4, cell + 0.4), dark);
        }
      }
    }
  }

  @override
  bool shouldRepaint(_QrPainter old) => old.image != image;
}

/// The camera, until it reads a code: the code's text, or null where the person goes back.
Future<String?> scanCode(BuildContext context) => Navigator.of(context).push<String>(MaterialPageRoute(
    builder: (ctx) {
      var done = false;
      return Scaffold(
        appBar: AppBar(title: const Text('Scan their code')),
        body: MobileScanner(onDetect: (capture) {
          final v = capture.barcodes.map((b) => b.rawValue).whereType<String>().firstOrNull;
          if (v == null || done) return;
          done = true;
          Navigator.of(ctx).pop(v);
        }),
      );
    }));
