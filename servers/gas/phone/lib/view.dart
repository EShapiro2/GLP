// The web view the app shows its page in: the platform's own.  On Android,
// iOS and macOS, webview_flutter's; on Windows, WebView2's (webview_windows),
// where the page's GASNative channel is window.chrome.webview's, made to
// look the same by a script run before the page's.
import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'package:flutter/widgets.dart';
import 'package:webview_flutter/webview_flutter.dart' as wf;
import 'package:webview_windows/webview_windows.dart' as ww;

abstract class GasView {
  /// A view of [url]; [onMessage] gets what the page posts to GASNative,
  /// [onLoaded] is told each time a page has loaded.
  static Future<GasView> open(String url,
      {required void Function(String) onMessage, required void Function() onLoaded, required Color background}) async {
    final v = Platform.isWindows ? _Windows() : _Flutter();
    await v._open(url, onMessage, onLoaded, background);
    return v;
  }

  Future<void> _open(String url, void Function(String) onMessage, void Function() onLoaded, Color background);
  Future<void> run(String js);
  Future<bool> back();
  Widget widget();
  Future<void> dispose();
}

class _Flutter extends GasView {
  late final wf.WebViewController c;

  @override
  Future<void> _open(String url, void Function(String) onMessage, void Function() onLoaded, Color background) async {
    c = wf.WebViewController()
      ..setJavaScriptMode(wf.JavaScriptMode.unrestricted)
      ..addJavaScriptChannel('GASNative', onMessageReceived: (m) => onMessage(m.message))
      ..setNavigationDelegate(wf.NavigationDelegate(onPageFinished: (_) => onLoaded()));
    // macOS's WebView has no background colour of its own (it throws); the
    // page paints the same colour.
    if (!Platform.isMacOS) await c.setBackgroundColor(background);
    await c.loadRequest(Uri.parse(url));
  }

  @override
  Future<void> run(String js) => c.runJavaScript(js);

  @override
  Future<bool> back() async {
    if (!await c.canGoBack()) return false;
    await c.goBack();
    return true;
  }

  @override
  Widget widget() => wf.WebViewWidget(controller: c);

  @override
  Future<void> dispose() async {}
}

class _Windows extends GasView {
  final c = ww.WebviewController();
  StreamSubscription<dynamic>? _m, _l;

  @override
  Future<void> _open(String url, void Function(String) onMessage, void Function() onLoaded, Color background) async {
    await c.initialize();
    await c.setBackgroundColor(background);
    // The page asks for GASNative.postMessage, as on the other platforms; a
    // scanner there is none.
    await c.addScriptToExecuteOnDocumentCreated(
        'window.GASNative = {postMessage: m => window.chrome.webview.postMessage(m)}; window.GAS_NOSCAN = true;');
    _m = c.webMessage.listen((m) => onMessage(m is String ? m : jsonEncode(m)));
    _l = c.loadingState.listen((s) {
      if (s == ww.LoadingState.navigationCompleted) onLoaded();
    });
    await c.loadUrl(url);
  }

  @override
  Future<void> run(String js) async {
    await c.executeScript(js);
  }

  @override
  Future<bool> back() async {
    await c.goBack();
    return true;
  }

  @override
  Widget widget() => ww.Webview(c);

  @override
  Future<void> dispose() async {
    await _m?.cancel();
    await _l?.cancel();
    await c.dispose();
  }
}
