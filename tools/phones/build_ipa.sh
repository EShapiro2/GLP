#!/bin/bash
# Builds GrassApp as an unsigned .ipa for AltServer to sign and install over WiFi (Code #26, 2026-11-08).
# Usage: bash tools/phones/build_ipa.sh [out-dir]   (from the repository root; out-dir defaults to /Users/udi/Grassroots/tmp/phones)
# AltServer re-signs the .ipa with the free team at install, so the build carries no signature and no device list.
set -e
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
OUT="${1:-/Users/udi/Grassroots/tmp/phones}"
mkdir -p "$OUT"
cd "$ROOT/glp_multiagent"
bash tool/sync_glp_assets.sh
flutter build ios --release --no-codesign -t lib/main_superapp.dart
APP="$ROOT/glp_multiagent/build/ios/iphoneos/Runner.app"
SHA="$(git -C "$ROOT" rev-parse --short HEAD)"
STAGE="$OUT/ipa-$SHA"
rm -rf "$STAGE" && mkdir -p "$STAGE/Payload"
cp -R "$APP" "$STAGE/Payload/Runner.app"
(cd "$STAGE" && zip -qr "../GrassApp-$SHA.ipa" Payload)
rm -rf "$STAGE"
ln -sf "GrassApp-$SHA.ipa" "$OUT/GrassApp.ipa"
echo "IPA: $OUT/GrassApp-$SHA.ipa (GrassApp.ipa points to it)"
