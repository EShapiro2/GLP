// The directory's listings and their links (GNA Section 7, the contract GDS,
// programs/examples/rendezvous/GDS.txt, with its implementation pragmas).
//
// A listing is an email address of a person's that the directory verified,
// with a name of their choosing if they give one, listed or unlisted (GDS:2).
// The directory keeps, for each, a buffer of unexpired invitation links of
// the person's, made and registered by their app (GRV:2) and given to the
// directory, each good for a day; a lookup takes one (GDS:3, GDS:4).  The
// buffer is kept at one link at least; when a lookup takes one, two more are
// wanted, and so on by how often the person is looked up ("Listing,
// Contact").  Who is told that links are wanted, and how, is the directory's
// agent's, whose messages GNA has yet to give; the store says only how many.
//
// The store is a JSON file, written whole after each change.
import 'dart:convert';
import 'dart:io';
import 'dart:math';

/// An invitation link as given to the directory: the signed term as text, the
/// hex of its signed content, and its expiry in Unix milliseconds, as the
/// person's app made it.
class Link {
  final String text;
  final int expiry;
  Link(this.text, this.expiry);
  Map<String, Object> toJson() => {'text': text, 'expiry': expiry};
  static Link fromJson(Map<String, dynamic> j) => Link(j['text'] as String, j['expiry'] as int);
}

class Listing {
  final String email;
  String? name;
  bool listed;
  bool verified;
  String? token;
  int mailedAt;
  final List<Link> links;
  int wanted;
  Listing(this.email, {this.name, this.listed = true, this.verified = false, this.token, this.mailedAt = 0, List<Link>? links, this.wanted = 0})
      : links = links ?? [];

  /// The page shows a listed person by name where they gave one, and by
  /// email address otherwise (GDS, "Lookup").
  String get shown => (name == null || name!.isEmpty) ? email : name!;

  /// A stable handle for the page, so the page names no email address.
  String get id => _hash(email);

  Map<String, Object?> toJson() => {
        'email': email, 'name': name, 'listed': listed, 'verified': verified,
        'token': token, 'mailedAt': mailedAt, 'wanted': wanted,
        'links': [for (final l in links) l.toJson()],
      };
  static Listing fromJson(Map<String, dynamic> j) => Listing(j['email'] as String,
      name: j['name'] as String?, listed: j['listed'] as bool, verified: j['verified'] as bool,
      token: j['token'] as String?, mailedAt: j['mailedAt'] as int, wanted: j['wanted'] as int,
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

  /// The buffer the directory keeps for each person, at least one link.
  static const int floor = 1;
  /// A verification mail per address per hour at most.
  static const int mailGap = 3600 * 1000;

  Store({this.file, int Function()? now, Random? rng})
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

  /// GDS:2, a person asks to be listed by an email address, with a name if
  /// they choose, listed or unlisted: the directory verifies the address by
  /// sending it a message with a link.  The listing waits unverified.
  ListResult request(String email, {String? name, bool listed = true}) {
    final e = normalEmail(email);
    if (!RegExp(r'^[^@\s]+@[^@\s]+\.[^@\s]+$').hasMatch(e)) return Refused('not an email address');
    final t = now();
    final old = _byEmail[e];
    if (old != null && t - old.mailedAt < mailGap) return Refused('a verification mail was sent to this address within the hour');
    final l = old ?? Listing(e);
    l.name = (name == null || name.trim().isEmpty) ? null : name.trim();
    l.listed = listed;
    l.token = _token();
    l.mailedAt = t;
    _byEmail[e] = l;
    _save();
    return MailToSend(e, l.token!);
  }

  /// The person opened the mail's link: the address is verified, and the
  /// listing stands.  Returns the listing, or null for a token not issued.
  Listing? verify(String token) {
    for (final l in _byEmail.values) {
      if (l.token != null && l.token == token) {
        l.verified = true;
        l.token = null;
        if (l.wanted < floor) l.wanted = floor;
        _save();
        return l;
      }
    }
    return null;
  }

  /// GDS:2, the person unlists themselves: the listing and its links go.
  bool unlist(String email) {
    final removed = _byEmail.remove(normalEmail(email)) != null;
    if (removed) _save();
    return removed;
  }

  /// The person's app gives the directory links of theirs; each counts
  /// against what was wanted.  Links already expired are not kept.
  void supply(String email, List<Link> links) {
    final l = _byEmail[normalEmail(email)];
    if (l == null || !l.verified) return;
    final t = now();
    final fresh = links.where((k) => k.expiry > t).toList();
    l.links.addAll(fresh);
    l.wanted = max(0, l.wanted - fresh.length);
    _save();
  }

  void _drop(Listing l) => l.links.removeWhere((k) => k.expiry <= now());

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
  /// buffer; two more are wanted for the one taken.  Null where the buffer
  /// holds none, the person's app having been off longer than a day.
  Link? take(Listing l) {
    _drop(l);
    l.wanted += 2;
    if (l.links.isEmpty) { _save(); return null; }
    l.links.sort((a, b) => a.expiry.compareTo(b.expiry));
    final k = l.links.removeAt(0);
    _save();
    return k;
  }

  /// How many links the directory wants of each person now: what lookups
  /// asked for, and enough to keep the floor.
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
