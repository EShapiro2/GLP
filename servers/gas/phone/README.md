# GAS on a phone

One person, the phone's owner, runs one grassroots contract's canonical platform as an agent of madGLP, over GNA's networking layer: Bluetooth (PAN) and the local network (LAN), the person named by the layer's key. The screen is the GAS app page for one person (`../web/app.html?solo`), served by the app's own server on the phone's loopback. The people the layer finds near the phone are the persons the page offers. The button at the top right (connected/found) opens what the layer sees.

This is the first test bed of madGLP on phones: no rendezvous server yet (no Internet between phones), no attestation (every peer is reported unattested), and first contact Open on both proximity underlays.

## What is where

- `lib/person_host.dart`: the person's agent, `AgentRuntime` over a given `GlpNetwork`, running the live harness's `live_person/3` (told `me(Key)` first), and the state the page reads.
- `lib/local_server.dart`: the page and its API, `/api/<contract>/{state,act,day}`, on 127.0.0.1.
- `lib/main.dart`: unpacks the bundle, lets the person pick a contract, starts the layer (`GrassrootsNetwork` through `GrassrootsGlpNetwork`), the agent and the server, and shows the page in a web view.
- `tool/sync_assets.sh`: bundles the GLP files the harnesses load and the GAS site into `assets/` (gitignored).
- `test/pair_test.dart`: two persons of GKK, each over its own headless layer on the loopback, acquaint each other. `GAS_PROGRAMS=$PWD/assets/tree/programs flutter test test/pair_test.dart` runs it from the bundle alone.
- `test/demo_test.dart`: the same pair kept running with the app's own servers, to play in a browser (`GAS_DEMO=gkk flutter test test/demo_test.dart`, then http://127.0.0.1:8201/app.html?c=gkk&solo and :8202).

## Build

On a Mac with Xcode, Flutter (https://docs.flutter.dev/get-started/install/macos), CocoaPods and Python 3; for Android, Android Studio with an SDK.

    git clone -b GAS-twophone https://github.com/nimrodtalmon/GLP.git
    cd GLP/servers/gas
    python3 -I tools/build.py        # the worlds and the site
    cd phone
    bash tool/sync_assets.sh         # the bundle
    flutter pub get

Then one of:

    flutter run -d macos             # the Mac as a device
    flutter run --release -d <iPhone or Android device id>   # see: flutter devices

An iPhone needs, once: Developer Mode on the phone, and in Xcode (`open ios/Runner.xcworkspace`) the target Runner's Signing & Capabilities set to your team (a free Apple ID works for your own phones) with a bundle identifier of your own.

## A first test

Two devices on the same Wi-Fi, or near each other with Bluetooth on. On both, pick the same contract (GKK first), allow Bluetooth and the local network when asked, and wait until the top right shows a connected person. Then on each tap Establish; on one tap Acquaint, pick the other person, Propose; on the other, Accept. Each now lists the other under Contacts.
