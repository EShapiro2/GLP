#!/usr/bin/env python3
"""GrassApp icon generator.

Sky gradient (deep blue at the top, pale near the horizon so the green grass
stands out), an optional sun, and a band of thin dense grass blades at the
bottom. No cloud. Run from the glp_multiagent directory:

    python3 tool/make_icon.py            # sun on
    python3 tool/make_icon.py --no-sun   # sun off

Writes assets/grassapp_icon.png (1024x1024).
"""
import math
import random
import sys

from PIL import Image, ImageDraw

S = 1024
SHOW_SUN = "--no-sun" not in sys.argv

img = Image.new("RGB", (S, S))
px = img.load()

# --- Sky gradient: deep blue at top -> pale blue near the horizon ----------
TOP = (74, 144, 217)    # deep blue
BOT = (200, 226, 248)   # pale blue at the horizon
HORIZON = int(S * 0.56)  # where the grass band begins
for y in range(S):
    t = min(y, HORIZON) / HORIZON
    r = round(TOP[0] + (BOT[0] - TOP[0]) * t)
    g = round(TOP[1] + (BOT[1] - TOP[1]) * t)
    b = round(TOP[2] + (BOT[2] - TOP[2]) * t)
    for x in range(S):
        px[x, y] = (r, g, b)

draw = ImageDraw.Draw(img, "RGBA")

# --- Sun: a sunrise on the horizon, rising from behind the grass -----------
# Centred on the horizon line so its lower half sits behind the grass band
# (the blades, drawn next, break across it). Warm sunrise tone with a halo.
if SHOW_SUN:
    cx, cy, rad = int(S * 0.50), HORIZON, int(S * 0.215)
    # soft warm halo
    for i in range(24, 0, -1):
        rr = rad + i * 9
        a = int(9 * (i / 24))
        draw.ellipse([cx - rr, cy - rr, cx + rr, cy + rr],
                     fill=(255, 213, 128, a))
    draw.ellipse([cx - rad, cy - rad, cx + rad, cy + rad], fill=(255, 198, 73))

# --- Grass: thin, dense blades ---------------------------------------------
rng = random.Random(7)
GREENS = [(56, 142, 60), (67, 160, 71), (46, 125, 50), (76, 175, 80),
          (39, 110, 44)]
base = int(S * 1.00)                 # blades rooted at the very bottom
band = S - HORIZON                   # vertical room for grass
n = 520
for i in range(n):
    x = rng.uniform(-10, S + 10)
    h = rng.uniform(band * 0.55, band * 1.12)
    tip_y = base - h
    width = rng.uniform(2.2, 5.0)
    lean = rng.uniform(-0.16, 0.16) * h
    col = rng.choice(GREENS)
    tip_x = x + lean
    # tapered blade: wide root -> sharp tip
    draw.polygon([(x - width, base), (x + width, base), (tip_x, tip_y)],
                 fill=col)

img.save("assets/grassapp_icon.png")
print("wrote assets/grassapp_icon.png  sun=%s" % SHOW_SUN)
