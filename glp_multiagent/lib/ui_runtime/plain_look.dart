/// The plain look: the grassroots app's construct family drawn in plain
/// words for a person who is not a programmer.  "A question's type fixes a
/// construct's semantic content --- what it shows, what it takes, and the
/// answer each submission forms --- and nothing of its layout ... so families
/// are interchangeable and each person may choose theirs --- the same program
/// rendered plainly for one person and child-friendly for another" (vGLP,
/// Remark "Construct Families").  The plain look changes only how a name or
/// a value is written on the screen: no position is hidden, nothing the
/// person may answer changes, and the term each submission forms is the same.
///
/// - A name, a functor or the name of a type, is written in words: its
///   underscores spaces, its humps of capitals split, its first letter a
///   capital (`open_place` "Open place", `NoticeContent` "Notice content").
/// - A key, an agent identifier, 64 hex digits, is written by its first
///   eight and an ellipsis (`63d368ee…`).
///
/// The person chooses the look by the button in the app's bar
/// (agent_surface.dart); a label a widget declaration gives is written as
/// given, it being the platform's own words (vGLP, Section 6.2, "The words a
/// platform shows").
library;

import 'package:flutter/foundation.dart';

import 'term.dart';

/// Whether the plain look is on: off unless the app turns it on, so that a
/// construct family drawn outside the app draws the names as they are.
final ValueNotifier<bool> plainLook = ValueNotifier<bool>(false);

final RegExp _key = RegExp(r'^[0-9a-f]{64}$');
final RegExp _name = RegExp(r'^[A-Za-z][A-Za-z0-9_]*$');

/// [s] as the plain look writes it, where the look is on and [s] is a name
/// or a key; [s] itself otherwise.
String plainWords(String s) {
  if (!plainLook.value) return s;
  if (_key.hasMatch(s)) return '${s.substring(0, 8)}…';
  if (!_name.hasMatch(s)) return s;
  final words = s
      .replaceAll('_', ' ')
      .replaceAllMapped(
          RegExp(r'([a-z0-9])([A-Z])'), (m) => '${m[1]} ${m[2]!.toLowerCase()}')
      .trim();
  if (words.isEmpty) return s;
  return words[0].toUpperCase() + words.substring(1);
}

/// The text of [t] as the plain look writes it: its scalars as
/// [displayText] runs them together, each written by [plainWords].
String plainDisplay(GTerm t) => plainLook.value
    ? scalarGroups(t).map((row) => row.map(plainWords).join(' ')).join(', ')
    : displayText(t);
