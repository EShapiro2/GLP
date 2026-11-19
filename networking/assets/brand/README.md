# The Grassroots mark

The mark of the Grassroots Project (peoplesweb.org): a hand-cut patchwork globe, many different patches sewn into one world.  The networking demo app is the first to wear it, as its app icon.

| File | What it is |
|------|------------|
| `grassroots-mark.svg` | The mark, rich form: hand-cut outline, wobbly seams with a running stitch, a stitch ring inside the edge.  For 48 px and up. |
| `grassroots-mark-flat.svg` | The flat form: the patches and white seams only, for 16 to 32 px (a favicon), where stitches turn to noise. |
| `grassroots-mark-foreground.svg` | A copy of the rich mark, scaled to 0.9 on a 108-unit canvas, for Android's adaptive icon: the globe is 55% of the canvas, inside the 61% an adaptive icon's mask never crops.  Keep it in step with `grassroots-mark.svg`. |
| `icon-1024.png` | The iOS and legacy Android master: the rich mark on white, with no alpha, since iOS refuses an app icon with transparency. |
| `icon-foreground-1024.png` | The Android adaptive foreground: the foreground SVG on transparent. |

## Regenerating

After any change to an SVG, from this directory:

```bash
rsvg-convert -w 1024 -h 1024 -b '#FFFFFF' grassroots-mark.svg -o icon-1024.png
rsvg-convert -w 1024 -h 1024 grassroots-mark-foreground.svg -o icon-foreground-1024.png
```

and then, from `networking/`, the app icons, as `pubspec.yaml`'s `flutter_launcher_icons` block configures them:

```bash
dart run flutter_launcher_icons
```

`rsvg-convert` is librsvg's (Homebrew `librsvg`).
