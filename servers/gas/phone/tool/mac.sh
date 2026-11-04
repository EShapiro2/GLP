#!/usr/bin/env bash
# Build and run the GAS phone app on a Mac, one stage at a time:
#   bash mac.sh check     what is installed (Xcode, Flutter, CocoaPods, Python)
#   bash mac.sh install   Flutter and CocoaPods, by Homebrew (Xcode comes from the App Store)
#   bash mac.sh get       clone or update the GAS-twophone branch into ~/src/GLP-gas
#   bash mac.sh build     the worlds, the bundle, the packages, and the macOS app
#   bash mac.sh run       open the macOS app
#   bash mac.sh devices   the phones Flutter sees (plug the phone in first)
#   bash mac.sh phone ID  build and install on the phone with that id
# Each stage ends with one line, OK or FAILED, and keeps its full output in
# ~/src/gas-<stage>.log.
set -u
STAGE=${1:-check}
DIR=$HOME/src/GLP-gas
PHONE=$DIR/servers/gas/phone
mkdir -p "$HOME/src"
LOG=$HOME/src/gas-$STAGE.log
: > "$LOG"
say() { echo "$@" | tee -a "$LOG"; }
run() { echo "\$ $*" >> "$LOG"; "$@" >> "$LOG" 2>&1; }
ok() { say "OK: $*"; exit 0; }
fail() { say "FAILED: $*  (full output: $LOG)"; tail -n 25 "$LOG"; exit 1; }

case "$STAGE" in
check)
  miss=""
  if xcode-select -p >/dev/null 2>&1 && xcodebuild -version >/dev/null 2>&1; then say "Xcode: $(xcodebuild -version | head -1)"; else say "Xcode: missing (install from the App Store, open it once)"; miss="$miss xcode"; fi
  if command -v flutter >/dev/null 2>&1; then say "Flutter: $(flutter --version 2>/dev/null | head -1)"; else say "Flutter: missing"; miss="$miss flutter"; fi
  if command -v pod >/dev/null 2>&1; then say "CocoaPods: $(pod --version)"; else say "CocoaPods: missing"; miss="$miss cocoapods"; fi
  if command -v python3 >/dev/null 2>&1; then say "Python: $(python3 --version)"; else say "Python: missing"; miss="$miss python"; fi
  if command -v brew >/dev/null 2>&1; then say "Homebrew: $(brew --version | head -1)"; else say "Homebrew: missing"; fi
  if command -v git >/dev/null 2>&1; then say "git: $(git --version)"; else say "git: missing"; miss="$miss git"; fi
  [ -z "$miss" ] && ok "everything needed is installed" || fail "missing:$miss"
  ;;
install)
  command -v brew >/dev/null 2>&1 || fail "Homebrew is missing: see https://brew.sh"
  command -v flutter >/dev/null 2>&1 || run brew install --cask flutter || fail "brew install flutter"
  command -v pod >/dev/null 2>&1 || run brew install cocoapods || fail "brew install cocoapods"
  run flutter --disable-analytics
  run flutter precache --ios --macos
  ok "Flutter $(flutter --version 2>/dev/null | head -1 | cut -d' ' -f2) and CocoaPods $(pod --version) installed"
  ;;
get)
  if [ -d "$DIR/.git" ]; then
    run git -C "$DIR" fetch --depth 1 origin GAS-twophone && run git -C "$DIR" checkout -q -B GAS-twophone FETCH_HEAD || fail "git update"
  else
    run git clone --depth 1 -b GAS-twophone https://github.com/nimrodtalmon/GLP.git "$DIR" || fail "git clone"
  fi
  ok "branch GAS-twophone at $(git -C "$DIR" log -1 --format='%h %s' | cut -c1-70) in $DIR"
  ;;
build)
  cd "$DIR/servers/gas" || fail "no checkout: run  bash mac.sh get"
  run python3 -I tools/build.py || fail "the worlds and the site (build.py)"
  cd "$PHONE" || fail "no phone app"
  run bash tool/sync_assets.sh || fail "the bundle (sync_assets.sh)"
  run flutter pub get || fail "flutter pub get"
  run flutter build macos --release || fail "flutter build macos"
  ok "built $PHONE/build/macos/Build/Products/Release/GAS.app"
  ;;
run)
  APP="$PHONE/build/macos/Build/Products/Release/GAS.app"
  [ -d "$APP" ] || fail "not built: run  bash mac.sh build"
  open "$APP" && ok "opened GAS.app"
  ;;
devices)
  cd "$PHONE" || fail "no phone app"
  flutter devices 2>&1 | tee -a "$LOG"
  ok "listed"
  ;;
phone)
  ID=${2:-}
  [ -n "$ID" ] || fail "give the phone's id:  bash mac.sh phone <id>  (see: bash mac.sh devices)"
  cd "$PHONE" || fail "no phone app"
  [ -d assets/bundle ] || { run bash tool/sync_assets.sh || fail "the bundle"; }
  run flutter run --release -d "$ID" --no-resident || fail "flutter run on $ID"
  ok "installed and started on $ID"
  ;;
*) fail "unknown stage $STAGE (check, install, get, build, run, devices, phone)";;
esac
