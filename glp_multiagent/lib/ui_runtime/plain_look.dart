/// The plain look: the grassroots app's construct family drawn with the names
/// of a program in words, for a person who is not a programmer.  "A question's
/// type fixes a construct's semantic content --- what it shows, what it takes,
/// and the answer each submission forms --- and nothing of its layout ... so
/// families are interchangeable and each person may choose theirs --- the
/// same program rendered plainly for one person and child-friendly for
/// another" (vGLP, Remark "Construct Families").  A family may change how a
/// construct is drawn, never its content (GLP #68, 2026-11-11), so:
///
/// - A name of the program, a functor, a constant of a type's alternatives or
///   the name of a type, is written in words where the family draws it as a
///   name (a form's heading, a field's label, a button or a choice): its
///   underscores spaces, its humps of capitals split, its first letter a
///   capital (`open_place` "Open place", `NoticeContent` "Notice content").
///   The family knows a name from a value by the position it draws: a
///   value the program or a person wrote is drawn by [displayText], as
///   written, and never passes through [plainWords].
/// - A key, 64 hex digits, is never cut or reworded: a key the person answers
///   about is shown whole.
///
/// The person chooses the look by the button in the app's bar
/// (agent_surface.dart).
library;

import 'package:flutter/foundation.dart';

/// Whether the plain look is on: off unless the app turns it on, so that a
/// construct family drawn outside the app draws the names as they are.
final ValueNotifier<bool> plainLook = ValueNotifier<bool>(false);

final RegExp _key = RegExp(r'^[0-9a-fA-F]{64}$');
final RegExp _name = RegExp(r'^[A-Za-z][A-Za-z0-9_]*$');

/// The name [s] as the plain look writes it, where the look is on and [s] is
/// a name and not a key; [s] itself otherwise.
String plainWords(String s) {
  if (!plainLook.value) return s;
  if (_key.hasMatch(s) || !_name.hasMatch(s)) return s;
  final words = s
      .replaceAll('_', ' ')
      .replaceAllMapped(
          RegExp(r'([a-z0-9])([A-Z])'), (m) => '${m[1]} ${m[2]!.toLowerCase()}')
      .trim();
  if (words.isEmpty) return s;
  return words[0].toUpperCase() + words.substring(1);
}
