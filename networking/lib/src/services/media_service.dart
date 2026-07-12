import 'dart:async';
import 'dart:io';

import 'package:cryptography/cryptography.dart';
import 'package:flutter/foundation.dart';
import 'package:image/image.dart' as img;
import 'package:path_provider/path_provider.dart';

/// Subdirectory under `getApplicationDocumentsDirectory()` where image
/// payloads are written. Files are named `<sha256>.<ext>` so identical
/// images dedup naturally.
const String _mediaSubdir = 'grassroots_media';

const int _bleCompressionTargetBytes = 100 * 1024;
const int _bleCompressionFloorQuality = 60;

/// Compress an image down to a size BLE can move in seconds rather than
/// minutes. Decodes the input, downscales to [maxDim] (preserving aspect
/// ratio), and re-encodes JPEG starting at [initialQuality], stepping the
/// quality down to a floor of 60 if the result is still over ~100 KB. The
/// floor result is accepted regardless. Runs the heavy decode/encode in a
/// background isolate via `compute()` so it doesn't stall the UI.
Future<Uint8List> compressForBle(
  Uint8List bytes, {
  int maxDim = 1280,
  int initialQuality = 75,
}) {
  return compute<_CompressInput, Uint8List>(
    _compressForBleIsolate,
    _CompressInput(
      bytes: bytes,
      maxDim: maxDim,
      initialQuality: initialQuality,
    ),
  );
}

class _CompressInput {
  final Uint8List bytes;
  final int maxDim;
  final int initialQuality;
  const _CompressInput({
    required this.bytes,
    required this.maxDim,
    required this.initialQuality,
  });
}

Uint8List _compressForBleIsolate(_CompressInput input) {
  final decoded = img.decodeImage(input.bytes);
  if (decoded == null) {
    // Could not decode — fall back to the raw bytes. Caller will hit the
    // BLE size cap and the send may be slow, but that's better than failing.
    return input.bytes;
  }

  img.Image working = decoded;
  final longest = working.width > working.height ? working.width : working.height;
  if (longest > input.maxDim) {
    if (working.width >= working.height) {
      working = img.copyResize(working, width: input.maxDim);
    } else {
      working = img.copyResize(working, height: input.maxDim);
    }
  }

  int quality = input.initialQuality;
  Uint8List encoded = Uint8List.fromList(img.encodeJpg(working, quality: quality));
  while (encoded.length > _bleCompressionTargetBytes &&
      quality > _bleCompressionFloorQuality) {
    quality -= 5;
    encoded = Uint8List.fromList(img.encodeJpg(working, quality: quality));
  }
  return encoded;
}

/// Write [bytes] to disk under the per-app documents directory using a
/// SHA-256 hash filename. If a file with the same hash already exists, skip
/// the write and return that path (natural content-addressed dedup).
///
/// Returns the absolute path on disk.
Future<File> writeMediaFile(Uint8List bytes, String mime) async {
  final dir = await _ensureMediaDir();
  final hash = await Sha256().hash(bytes);
  final hex = hash.bytes.map((b) => b.toRadixString(16).padLeft(2, '0')).join();
  final ext = _extensionFromMime(mime);
  final file = File('${dir.path}/$hex.$ext');
  if (!await file.exists()) {
    await file.writeAsBytes(bytes, flush: true);
  }
  return file;
}

/// Delete a media file at [path]. Silently ignores `FileSystemException` —
/// the file was already gone, which is the desired end state anyway.
Future<void> deleteMediaFile(String path) async {
  try {
    final f = File(path);
    if (await f.exists()) {
      await f.delete();
    }
  } on FileSystemException catch (e) {
    debugPrint('deleteMediaFile($path) ignored: $e');
  }
}

Future<Directory> _ensureMediaDir() async {
  final docs = await getApplicationDocumentsDirectory();
  final dir = Directory('${docs.path}/$_mediaSubdir');
  if (!await dir.exists()) {
    await dir.create(recursive: true);
  }
  return dir;
}

String _extensionFromMime(String mime) {
  switch (mime.toLowerCase()) {
    case 'image/jpeg':
    case 'image/jpg':
      return 'jpg';
    case 'image/png':
      return 'png';
    case 'image/heic':
      return 'heic';
    case 'image/webp':
      return 'webp';
    default:
      return 'bin';
  }
}
