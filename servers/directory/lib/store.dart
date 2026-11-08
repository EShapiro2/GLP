// The directory's listings and their links (GNA Section 7, the contract GDS,
// programs/examples/rendezvous/GDS.txt, with its implementation pragmas).
//
// A listing is an email address of a person's that the directory verified,
// with a name of their choosing if they give one, listed or unlisted (GDS:2),
// and the person's key, by which their app lists, supplies and unlists.  A
// person has one listing ("Alice may list herself"): a verified listing of a
// key takes the place of that key's earlier one, and keeps its links, which
// are the person's.  A request to list changes nothing until the mail's link
// is opened.
//
// The directory keeps, for each listing, a buffer of unexpired invitation
// links of the person's, made and registered by their app (GRV:2) and given
// to the directory, each good for a day; a lookup takes the one expiring
// first (GDS:3, GDS:4).  The buffer is kept at one link at least, and two
// more are asked for each taken ("Listing, Contact").  The store says how
// many to ask for, through [Store.onAsk]; the directory's agent asks the
// person's app, on the Supply stream of their list (GNA Section 7, "The
// messages").  [Listing.wanted] counts the links asked for and not yet
// supplied; it is asked again when the person lists again, on the new Supply
// stream (GNA Section 7).
//
// The store is a JSON file, written whole after each change.
import 'dart:convert';
import 'dart:io';
import 'dart:math';

/// An invitation link as given to the directory: the signed term, a string,
/// and its expiry in Unix milliseconds, as the person's app made it.
class Link {
  final String text;
  final int expiry;
  Link(this.text, this.expiry);
  Map<String, Object> toJson() => {'text': text, 'expiry': expiry};
  static Link fromJson(Map<String, dynamic> j) => Link(j['text'] as String, j['expiry'] as int);
}

/// A request to list, waiting on its mail's link: the key that asked, and
/// the name and the visibility it gave.
class Request {
  final String key;
  final String? name;
  final bool listed;
  final String token;
  Request(this.key, this.name, this.listed, this.token);
  Map<String, Object?> toJson() => {'key': key, 'name': name, 'listed': listed, 'token': token};
  static Request fromJson(Map<String, dynamic> j) =>
      Request(j['key'] as String, j['name'] as String?, j['listed'] as bool, j['token'] as String);
}

/// An email address the directory knows: verified for a key, with its
/// listing, or not yet; and the request waiting on its mail, if any.
class Listing {
  final String email;
  String? key;
  String? name;
  bool listed;
  bool verified;
  Request? request;
  int mailedAt;
  final List<Link> links;
  int wanted;
  Listing(this.email, {this.key, this.name, this.listed = true, this.verified = false, this.request, this.mailedAt = 0, List<Link>? links, this.wanted = 0})
      : links = links ?? [];

  /// The page shows a listed person by name where they gave one, and by
  /// email address otherwise (GDS, "Lookup").
  String get shown => (name == null || name!.isEmpty) ? email : name!;

  /// A stable handle for the page, so the page names no email address.
  String get id => _hash(email);

  Map<String, Object?> toJson() => {
        'email': email, 'key': key, 'name': name, 'listed': listed, 'verified': verified,
        'request': request?.toJson(), 'mailedAt': mailedAt, 'wanted': wanted,
        'links': [for (final l in links) l.toJson()],
      };

  /// A listing as the file holds it.
  static Listing fromJson(Map<String, dynamic> j) => Listing(j['email'] as String,
      key: j['key'] as String?, name: j['name'] as String?, listed: j['listed'] as bool, verified: j['verified'] as bool,
      request: j['request'] == null ? null : Request.fromJson(j['request'] as Map<String, dynamic>),
      mailedAt: j['mailedAt'] as int, wanted: (j['wanted'] as int?) ?? 0,
      links: [for (final l in j['links'] as List) Link.fromJson(l as Map<String, dynamic>)]);
}

String _hash(String s) {
  // FNV-1a, 64 bits, as hex: a handle, not a secret.
  var h = BigInt.parse('cbf29ce484222325', radix: 16);
  final p = BigInt.parse('100000001b3', radix: 16), m = (BigInt.one << 64) - BigInt.one;
  for (final c in utf8.encode(s.toLowerCase())) { h = ((h ^ BigInt.from(c)) * p) & m; }
  return h.toRadixString(16).padLeft(16, '0');
}

String normalEmail(String e) => e.trim().toLowerCase();

/// What a request to list gives: a verification mail to send, its token, or
/// a refusal with its reason.
sealed class ListResult {}
class MailToSend extends ListResult { final String email, token; MailToSend(this.email, this.token); }
class Refused extends ListResult { final String reason; Refused(this.reason); }

class Store {
  final File? file;
  final int Function() now;
  final Random _rng;
  final Map<String, Listing> _byEmail = {};

  /// Called with a person's key and how many links to ask their app for.
  void Function(String key, int count)? onAsk;

  /// The buffer the directory keeps for each person, at least one link.
  static const int floor = 1;
  /// Two more links asked for each one a lookup takes.
  static const int perTake = 2;
  /// A verification mail per address per hour at most.
  static const int mailGap = 3600 * 1000;

  Store({this.file, int Function()? now, Random? rng, this.onAsk})
      : now = now ?? (() => DateTime.now().millisecondsSinceEpoch),
        _rng = rng ?? Random.secure() {
    final f = file;
    if (f != null && f.existsSync()) {
      for (final j in jsonDecode(f.readAsStringSync()) as List) {
        final l = Listing.fromJson(j as Map<String, dynamic>);
        _byEmail[l.email] = l;
      }
    }
  }

  void _save() {
    final f = file;
    if (f == null) return;
    final tmp = File('${f.path}.tmp');
    tmp.writeAsStringSync(jsonEncode([for (final l in _byEmail.values) l.toJson()]));
    tmp.renameSync(f.path);
  }

  String _token() => List.generate(24, (_) => _rng.nextInt(256).toRadixString(16).padLeft(2, '0')).join();

  void _ask(Listing l, int count) {
    if (count <= 0 || l.key == null) return;
    l.wanted += count;
    onAsk?.call(l.key!, count);
  }

  /// Asks for what keeps the buffer at its floor, counting what was asked
  /// for already.
  void _toFloor(Listing l) => _ask(l, floor - l.links.length - l.wanted);

  /// GDS:2, the person of [key] asks to be listed by an email address, with a
  /// name if they choose, listed or unlisted: the directory verifies the
  /// address by sending it a message with a link.  Nothing changes until the
  /// link is opened.
  ListResult request(String key, String email, {String? name, bool listed = true}) {
    final e = normalEmail(email);
    if (!RegExp(r'^[^@\s]+@[^@\s]+\.[^@\s]+$').hasMatch(e)) return Refused('not an email address');
    final t = now();
    final old = _byEmail[e];
    if (old != null && t - old.mailedAt < mailGap) return Refused('a verification mail was sent to this address within the hour');
    final l = old ?? Listing(e);
    final n = (name == null || name.trim().isEmpty) ? null : name.trim();
    l.request = Request(key, n, listed, _token());
    l.mailedAt = t;
    _byEmail[e] = l;
    _save();
    return MailToSend(e, l.request!.token);
  }

  /// The person opened the mail's link: the address is verified for the key
  /// that asked, the listing takes the name and the visibility it gave, and
  /// takes the place of that key's earlier listing, with its links; the
  /// buffer is asked for.  Returns the listing, or null for a token not
  /// issued.
  Listing? verify(String token) {
    for (final l in _byEmail.values) {
      final r = l.request;
      if (r == null || r.token != token) continue;
      if (l.key != r.key) {
        l.links.clear();
        l.wanted = 0;
      }
      for (final other in _byEmail.values.where((o) => o != l && o.key == r.key).toList()) {
        if (other.verified) {
          l.links.addAll(other.links);
          l.wanted += other.wanted;
        }
        if (other.request == null) {
          _byEmail.remove(other.email);
        } else {
          other
            ..key = null
            ..verified = false
            ..links.clear()
            ..wanted = 0;
        }
      }
      l
        ..key = r.key
        ..name = r.name
        ..listed = r.listed
        ..verified = true
        ..request = null;
      _drop(l);
      _toFloor(l);
      _save();
      return l;
    }
    return null;
  }

  /// GNA Section 7: a list from the person of [key] for an address the
  /// directory already holds verified for them is answered with no mail, its
  /// name and visibility replacing the earlier ones.  Returns whether it was
  /// such a list; the caller then asks on the person's new Supply by
  /// [reask].
  bool relist(String key, String email, {String? name, bool listed = true}) {
    final l = _byEmail[normalEmail(email)];
    if (l == null || !l.verified || l.key != key) return false;
    l
      ..name = (name == null || name.trim().isEmpty) ? null : name.trim()
      ..listed = listed;
    _save();
    return true;
  }

  /// The person of [key] listed again, on a new Supply stream: what was asked
  /// on the earlier one is asked again on it, at least enough to keep the
  /// floor (GNA Section 7: "on which the directory asks at once for the
  /// buffer").
  void reask(String key) {
    final l = _byKey(key);
    if (l == null) return;
    _drop(l);
    final n = max(l.wanted, floor - l.links.length);
    l.wanted = 0;
    _ask(l, n);
  }

  Listing? _byKey(String key) {
    for (final l in _byEmail.values) {
      if (l.verified && l.key == key) return l;
    }
    return null;
  }

  /// GDS:2, the person of [key] unlists themselves: their listing and its
  /// links go.
  bool unlist(String key) {
    final l = _byKey(key);
    if (l == null) return false;
    if (l.request == null) {
      _byEmail.remove(l.email);
    } else {
      l
        ..key = null
        ..verified = false
        ..links.clear()
        ..wanted = 0;
    }
    _save();
    return true;
  }

  /// The person's app gives the directory links of theirs; each counts
  /// against what was asked for.  Links already expired are not kept, nor
  /// links of a person not listed.
  void supply(String key, List<Link> links) {
    final l = _byKey(key);
    if (l == null) return;
    final t = now();
    final fresh = links.where((k) => k.expiry > t).toList();
    l.links.addAll(fresh);
    l.wanted = max(0, l.wanted - fresh.length);
    _save();
  }

  void _drop(Listing l) => l.links.removeWhere((k) => k.expiry <= now());

  /// Expired links are dropped, and each buffer that falls below its floor
  /// is asked for again.  The directory's server calls it every minute.
  void sweep() {
    for (final l in _byEmail.values.where((l) => l.verified)) {
      final before = l.links.length;
      _drop(l);
      if (l.links.length != before) _save();
      _toFloor(l);
    }
  }

  /// The verified, listed persons, in the order the page shows them.
  List<Listing> listed() {
    final ls = _byEmail.values.where((l) => l.verified && l.listed).toList();
    ls.sort((a, b) => a.shown.toLowerCase().compareTo(b.shown.toLowerCase()));
    return ls;
  }

  Listing? byId(String id) {
    for (final l in _byEmail.values) {
      if (l.verified && l.listed && l.id == id) return l;
    }
    return null;
  }

  /// The verified listing of the person of [key], or null.
  Listing? byKey(String key) => _byKey(key);

  /// GDS:3, an unlisted person is looked up by their email address and name,
  /// both as they gave them; a listed one may be found so too.
  Listing? find(String email, String name) {
    final l = _byEmail[normalEmail(email)];
    if (l == null || !l.verified) return null;
    if ((l.name ?? '').toLowerCase() != name.trim().toLowerCase()) return null;
    return l;
  }

  /// GDS:4, the directory gives whoever looked the person up a link of
  /// theirs: the one expiring first among those unexpired, taken from the
  /// buffer; two more are asked for.  Null where the buffer holds none, the
  /// person's app having been off longer than a day.
  Link? take(Listing l) {
    _drop(l);
    _ask(l, perTake);
    if (l.links.isEmpty) { _save(); return null; }
    l.links.sort((a, b) => a.expiry.compareTo(b.expiry));
    final k = l.links.removeAt(0);
    _save();
    return k;
  }

  /// How many links the directory wants of each person now, by email
  /// address: what was asked for and not supplied, and at least enough to
  /// keep the floor.
  Map<String, int> wanted() {
    final w = <String, int>{};
    for (final l in _byEmail.values.where((l) => l.verified)) {
      _drop(l);
      final n = max(l.wanted, floor - l.links.length);
      if (n > 0) w[l.email] = n;
    }
    return w;
  }
}
