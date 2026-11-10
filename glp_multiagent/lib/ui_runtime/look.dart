/// The look of an app: its contract's icon and its family's colours (GSA #2,
/// 2026-11-09, Udi approving: "each contract's colour and icon come from the
/// contract"), read where the site's build reads them
/// (servers/peoplesweb/tools/build.py): each family's name and colours from
/// `programs/apps/look.json`; a contract's icon and family from the line its
/// interface pragmas open with, `Look: <icon>, <family>` (GSC's Section 2,
/// 34bc713), in its `NAME.txt`, and from the site's
/// `servers/peoplesweb/web/about.json` until its owner writes that line.  On
/// a phone the asset sync bundles all three at the same places
/// (tool/sync_glp_assets.sh).
library;

import 'dart:convert';
import 'dart:io';

import 'package:flutter/material.dart';

/// The contract whose look the Grassroots App takes: GAS, the Grassroots App
/// Store, one of the four its home is compiled from (GSA, Appendix A), until
/// GSA names the app's contract.
const String superappContract = 'GAS';

/// A family of contracts: its name and its two colours, from and to, the
/// light and the dark end of its gradient on the site.
class FamilyLook {
  final String name;
  final Color from, to;
  const FamilyLook(this.name, this.from, this.to);
}

/// A contract's look: its icon, an emoji, and its family's.
class ContractLook {
  final String contract, icon, family;
  final FamilyLook colours;
  const ContractLook(this.contract, this.icon, this.family, this.colours);

  /// The theme of the contract's app: Material 3 seeded by the family's dark
  /// colour, which is the primary colour itself, its buttons filled with it;
  /// [fontFamily] the font, the platform's where none is given.
  ThemeData theme({String? fontFamily}) => ThemeData(
        colorScheme:
            ColorScheme.fromSeed(seedColor: colours.to, primary: colours.to),
        useMaterial3: true,
        brightness: Brightness.light,
        fontFamily: fontFamily,
        elevatedButtonTheme: ElevatedButtonThemeData(
            style: ElevatedButton.styleFrom(
                backgroundColor: colours.to, foregroundColor: Colors.white)),
      );
}

/// Every contract's look.
class Looks {
  final Map<String, FamilyLook> families;
  final Map<String, ({String icon, String family})> contracts;
  const Looks(this.families, this.contracts);

  /// The family a contract with none named takes, and the icon.
  static const String otherFamily = 'other';
  static const String otherIcon = '📄';

  /// Reads the looks of the tree whose root `self.glp` is [rootSelfGlp]:
  /// `apps/look.json` under it, every contract's `NAME.txt` in it, and
  /// `servers/peoplesweb/web/about.json` beside it, where it is.
  static Looks readSync(String rootSelfGlp) {
    final programs = File(rootSelfGlp).parent;
    final about =
        File('${programs.parent.path}/servers/peoplesweb/web/about.json');
    final texts = <String>[
      for (final f in programs.listSync(recursive: true, followLinks: false))
        if (f is File && f.path.endsWith('.txt')) f.readAsStringSync()
    ];
    return parse(File('${programs.path}/apps/look.json').readAsStringSync(),
        about: about.existsSync() ? about.readAsStringSync() : null,
        contractTexts: texts);
  }

  /// [readSync], for a caller that awaits.
  static Future<Looks> read(String rootSelfGlp) async => readSync(rootSelfGlp);

  /// The looks from the text of `look.json`, of `about.json` and of the
  /// files of the tree that may be contracts: a contract is a `.txt` with a
  /// `% Owner:` line, named by its first other line, `NAME: Title`.
  static Looks parse(String lookJson,
      {String? about, Iterable<String> contractTexts = const []}) {
    final j = jsonDecode(lookJson) as Map<String, dynamic>;
    final fs = <String, FamilyLook>{};
    (j['families'] as Map<String, dynamic>).forEach((k, v) {
      final f = v as Map<String, dynamic>;
      fs[k] = FamilyLook(f['name'] as String, _colour(f['from'] as String),
          _colour(f['to'] as String));
    });
    if (!fs.containsKey(otherFamily)) {
      throw const FormatException('look.json names no family "other"');
    }
    final cs = <String, ({String icon, String family})>{};
    if (about != null) {
      (jsonDecode(about) as Map<String, dynamic>).forEach((k, v) {
        final a = (v as List).cast<String>();
        cs[k.toUpperCase()] = (icon: a[0], family: a[1]);
      });
    }
    for (final t in contractTexts) {
      final c = contractLook(t);
      if (c != null) cs[c.name] = (icon: c.icon, family: c.family);
    }
    return Looks(fs, cs);
  }

  static const _heads = {
    'Pragmas',
    'Interface pragmas',
    'Implementation pragmas'
  };

  /// A contract's name and the Look line its interface pragmas open with, as
  /// the site's build reads them (build.py, look_line); null for a text that
  /// is no contract or a contract with no Look line.
  static ({String name, String icon, String family})? contractLook(
      String text) {
    final lines = text.split('\n');
    if (!lines.any((l) => l.startsWith('% Owner:'))) return null;
    final body = [
      for (final l in lines)
        if (l.trim().isNotEmpty && !l.startsWith('%')) l.trim()
    ];
    if (body.isEmpty || !body.first.contains(': ')) return null;
    final name =
        body.first.substring(0, body.first.indexOf(': ')).toUpperCase();
    final start = body.indexWhere(_heads.contains);
    if (start < 0) return null;
    for (final l in body.skip(start + 1)) {
      if (_heads.contains(l)) continue;
      final m = RegExp(r'^Look:\s*(.+?),\s*([\w-]+)$').firstMatch(l);
      return m == null ? null : (name: name, icon: m[1]!, family: m[2]!);
    }
    return null;
  }

  /// The look of [contract] with the [icon] and the [family] its compiled
  /// platform exports in `contract(Text, Schemas)`, the look among the
  /// pragmas (vGLP b413fe1, Section 5.4), the colours the family's; a family
  /// the file does not name is the family other.
  ContractLook withLook(String contract, String icon, String family) {
    final f = families.containsKey(family) ? family : otherFamily;
    return ContractLook(contract.toUpperCase(), icon, f, families[f]!);
  }

  /// The look of [contract], its name in any case; a contract with no look,
  /// or of a family the file does not name, is of the family other.
  ContractLook of(String contract) {
    final name = contract.toUpperCase();
    final c = contracts[name];
    final family =
        c != null && families.containsKey(c.family) ? c.family : otherFamily;
    return ContractLook(name, c?.icon ?? otherIcon, family, families[family]!);
  }

  static Color _colour(String hex) {
    final h = hex.startsWith('#') ? hex.substring(1) : hex;
    if (h.length != 6) throw FormatException('not a colour: $hex');
    return Color(0xFF000000 | int.parse(h, radix: 16));
  }
}
