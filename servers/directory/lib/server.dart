// The directory's page at <address>/directory (GDS's implementation pragma
// Lookup: "a listed person is shown by name where they gave one and by email
// address otherwise, and a click on one is a lookup"), the lookup of an
// unlisted person by email address and name (GDS:3), the link given to
// whoever looked a person up (GDS:4), as text to open in a Grassroots App,
// and the verification mail's link (GDS:2).  The person's own acts, List,
// List unlisted and Unlist, are their app's, through the directory's agent
// (agent.dart), which calls [DirectoryService.list] for the mail.
import 'dart:convert';
import 'dart:io';

import 'mail.dart';
import 'store.dart';

class DirectoryService {
  final Store store;
  final Mailer mailer;
  /// The address the mail's link and the page are under, as people reach it,
  /// e.g. https://peoplesweb.org.
  final String base;
  DirectoryService(this.store, this.mailer, this.base);

  /// Called with a line for the server's log for each verification mail
  /// sent, `mailed <address>: <link>`, so that the live suite, which reads
  /// the log on the VPS, can open the link of a mail it made the directory
  /// send to an address that receives none (tools/live/bin/live_gds.dart).
  void Function(String line)? onLog;

  /// GDS:2's verification: the mail with its link.  Called by the
  /// directory's agent on the person's List or List unlisted, [key] the
  /// person's; refused as the store refuses.  Returns null where the mail was
  /// sent, and the reason otherwise.
  Future<String?> list(String key, String email, {String? name, bool listed = true}) async {
    final r = store.request(key, email, name: name, listed: listed);
    switch (r) {
      case Refused(:final reason):
        return reason;
      case MailToSend(:final email, :final token):
        await mailer.send(email, 'Confirm your listing in the Grassroots Directory',
            'Someone, we hope you, asked to be ${listed ? 'listed' : 'found by email address and name'} in the Grassroots Directory at $base/directory under this address${name == null || name.isEmpty ? '' : ', with the name $name'}.\n\n'
            'To confirm, open this link:\n$base/directory/verify?t=$token\n\n'
            'If it was not you, ignore this mail; nothing is listed until the link is opened.\n');
        onLog?.call('mailed $email: $base/directory/verify?t=$token');
        return null;
    }
  }

  Future<void> handle(HttpRequest q) async {
    final p = q.uri.path.replaceAll(RegExp(r'/+$'), '');
    try {
      if (p == '/directory' && q.method == 'GET') return await _page(q, _index());
      if (p == '/directory/lookup' && q.method == 'POST') {
        final f = await _form(q);
        final l = store.byId(f['id'] ?? '');
        return await _page(q, l == null ? _msg('No such listing.') : _give(l));
      }
      if (p == '/directory/find' && q.method == 'POST') {
        final f = await _form(q);
        final l = store.find(f['email'] ?? '', f['name'] ?? '');
        return await _page(q, l == null ? _msg('Nobody is listed by that email address and name.') : _give(l));
      }
      if (p == '/directory/verify' && q.method == 'GET') {
        final l = store.verify(q.uri.queryParameters['t'] ?? '');
        return await _page(q, _msg(l == null ? 'That link is not one the directory sent, or it was used already.'
            : '${_e(l.email)} is verified${l.listed ? ' and listed' : ''}. Your Grassroots App keeps the directory supplied with invitation links while it runs.'));
      }
      q.response.statusCode = HttpStatus.notFound;
      return await _page(q, _msg('Not found.'));
    } catch (e) {
      q.response.statusCode = HttpStatus.badRequest;
      return await _page(q, _msg('Bad request.'));
    }
  }

  Future<Map<String, String>> _form(HttpRequest q) async {
    final body = await utf8.decodeStream(q);
    return Uri.splitQueryString(body);
  }

  String _index() {
    final ls = store.listed();
    final rows = ls.isEmpty ? '<p class="none">Nobody is listed yet.</p>'
        : '<ul>${ls.map((l) => '<li><form method="post" action="$base/directory/lookup"><input type="hidden" name="id" value="${l.id}"><button>${_e(l.shown)}</button></form></li>').join()}</ul>';
    return '<h1>Grassroots Directory</h1>'
        '<p>People who chose to be found. Pick one to get an invitation link of theirs; open it in your Grassroots App to ask to connect.</p>'
        '$rows'
        '<h2>Someone not listed</h2><p>An unlisted person is found by their email address and name, as they gave them.</p>'
        '<form method="post" action="$base/directory/find" class="find"><input name="email" type="email" placeholder="Email address" required><input name="name" placeholder="Name" required><button>Find</button></form>';
  }

  String _give(Listing l) {
    final k = store.take(l);
    if (k == null) return _msg('${_e(l.shown)} has no invitation link here now: their Grassroots App has been off for more than a day. Try again later.');
    return '<h1>${_e(l.shown)}</h1><p>Here is an invitation link of theirs, good once and until ${DateTime.fromMillisecondsSinceEpoch(k.expiry, isUtc: true).toIso8601String().substring(0, 16).replaceFirst('T', ' ')} UTC. Copy it and open it in your Grassroots App, which asks them to connect.</p>'
        '<textarea readonly rows="6" onclick="this.select()">${_e(k.text)}</textarea><p><a href="$base/directory">Back to the directory</a></p>';
  }

  String _msg(String s) => '<h1>Grassroots Directory</h1><p>$s</p><p><a href="$base/directory">Back to the directory</a></p>';

  Future<void> _page(HttpRequest q, String body) async {
    q.response.headers.contentType = ContentType.html;
    q.response.write('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">'
        '<title>Grassroots Directory</title><style>'
        ':root{--bg:#fff;--fg:#1d2733;--mute:#5f6b78;--line:#d5dee7;--acc:#2f7fbf}'
        '@media (prefers-color-scheme:dark){:root{--bg:#121820;--fg:#e8eef5;--mute:#9aa7b4;--line:#2c3a48;--acc:#6fb0e6}}'
        'body{margin:0;background:var(--bg);color:var(--fg);font:16px/1.5 system-ui,sans-serif}main{max-width:640px;margin:0 auto;padding:24px 16px}'
        'ul{list-style:none;padding:0}li{border-bottom:1px solid var(--line)}li button{all:unset;cursor:pointer;display:block;padding:12px 0;color:var(--acc)}'
        'input,button{font:inherit}.find{display:flex;flex-wrap:wrap;gap:8px}.find input{flex:1 1 180px;padding:8px;border:1px solid var(--line);border-radius:8px;background:var(--bg);color:var(--fg)}'
        '.find button{padding:8px 16px;border:0;border-radius:8px;background:var(--acc);color:#fff}textarea{width:100%;box-sizing:border-box;font:13px monospace;background:var(--bg);color:var(--fg);border:1px solid var(--line);border-radius:8px;padding:8px}'
        'a{color:var(--acc)}.none{color:var(--mute)}</style></head><body><main>$body</main></body></html>');
    await q.response.close();
  }
}

String _e(String s) => const HtmlEscape().convert(s);
