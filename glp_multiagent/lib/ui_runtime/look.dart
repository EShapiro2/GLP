/// The look of an app: its contract's icon and its family's colours, from
/// `programs/apps/look.json`, the one file the site's build and the app's
/// asset sync both read (GSA #2, 2026-11-09, Udi approving: "each contract's
/// colour and icon come from the contract ... the families' colours are one
/// file in the repository beside the contracts, programs/apps/look.json").
/// Until GSC gives the pragma line that names a contract's icon and family,
/// the file holds them too.
library;

import 'dart:convert';
import 'dart:io';

import 'package:flutter/material.dart';

/// The contract whose look the Grassroots App takes: GAS, the Grassroots App
/// Store, one of the four its home is compiled from (GSA, Appendix A), until
/// GSA names the app's contract.
const String superappContract = 'GAS';

/// A family of contracts: its name and its two colours, light and dark.
class FamilyLook {
  final String name;
  final Color light, dark;
  const FamilyLook(this.name, this.light, this.dark);
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
        colorScheme: ColorScheme.fromSeed(
            seedColor: colours.dark, primary: colours.dark),
        useMaterial3: true,
        brightness: Brightness.light,
        fontFamily: fontFamily,
        elevatedButtonTheme: ElevatedButtonThemeData(
            style: ElevatedButton.styleFrom(
                backgroundColor: colours.dark, foregroundColor: Colors.white)),
      );
}

/// Every contract's look, as `programs/apps/look.json` gives it.
class Looks {
  final Map<String, FamilyLook> families;
  final Map<String, ({String icon, String family})> contracts;
  const Looks(this.families, this.contracts);

  /// The family a contract with none named takes, and the icon.
  static const String otherFamily = 'other';
  static const String otherIcon = '📄';

  /// The file beside the contracts, under the root `self.glp`'s directory.
  static String pathFor(String rootSelfGlp) =>
      '${File(rootSelfGlp).parent.path}/apps/look.json';

  /// Reads the file beside [rootSelfGlp].
  static Future<Looks> read(String rootSelfGlp) async =>
      parse(await File(pathFor(rootSelfGlp)).readAsString());

  /// Reads the file beside [rootSelfGlp], at once.
  static Looks readSync(String rootSelfGlp) =>
      parse(File(pathFor(rootSelfGlp)).readAsStringSync());

  static Looks parse(String text) {
    final j = jsonDecode(text) as Map<String, dynamic>;
    final fs = <String, FamilyLook>{};
    (j['families'] as Map<String, dynamic>).forEach((k, v) {
      final f = v as Map<String, dynamic>;
      final c = (f['colours'] as List).cast<String>();
      fs[k] = FamilyLook(f['name'] as String, _colour(c[0]), _colour(c[1]));
    });
    final cs = <String, ({String icon, String family})>{};
    (j['contracts'] as Map<String, dynamic>).forEach((k, v) {
      final c = v as Map<String, dynamic>;
      cs[k.toUpperCase()] =
          (icon: c['icon'] as String, family: c['family'] as String);
    });
    if (!fs.containsKey(otherFamily)) {
      throw const FormatException('look.json names no family "other"');
    }
    return Looks(fs, cs);
  }

  /// The look of [contract], its name in any case; a contract the file does
  /// not name, or of a family it does not name, is of the family other.
  ContractLook of(String contract) {
    final name = contract.toUpperCase();
    final c = contracts[name];
    final family =
        c != null && families.containsKey(c.family) ? c.family : otherFamily;
    return ContractLook(
        name, c?.icon ?? otherIcon, family, families[family]!);
  }

  static Color _colour(String hex) {
    final h = hex.startsWith('#') ? hex.substring(1) : hex;
    if (h.length != 6) throw FormatException('not a colour: $hex');
    return Color(0xFF000000 | int.parse(h, radix: 16));
  }
}
