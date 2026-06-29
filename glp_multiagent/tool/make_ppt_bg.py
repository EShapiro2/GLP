#!/usr/bin/env python3
"""GrassApp PowerPoint backgrounds (16:9).

Two slide backgrounds in the grassroots motif of the app icon:

  * grassapp_ppt_title.png   — title page: a round sun cresting a band of grass,
                               deep-blue-to-pale sky, like the app icon.
  * grassapp_ppt_content.png — ordinary page: the same sky and a short band of
                               grass along the bottom, no sun, so the page is
                               clear for content.

Run from glp_multiagent:  python3 tool/make_ppt_bg.py
"""
import random

from PIL import Image, ImageDraw

W, H = 1920, 1080
GREENS = [(56, 142, 60), (67, 160, 71), (46, 125, 50), (76, 175, 80),
          (39, 110, 44)]
TOP = (74, 144, 217)        # deep blue
BOT = (208, 230, 249)       # pale blue at the horizon


def sky(horizon, top=TOP, bot=BOT):
    img = Image.new("RGB", (W, H))
    px = img.load()
    for y in range(H):
        t = min(y, horizon) / horizon
        row = (round(top[0] + (bot[0] - top[0]) * t),
               round(top[1] + (bot[1] - top[1]) * t),
               round(top[2] + (bot[2] - top[2]) * t))
        for x in range(W):
            px[x, y] = row
    return img


def sun(draw, horizon, rad, peek):
    cx = int(W * 0.50)
    cy = horizon - peek + rad           # top of the circle sits `peek` above horizon
    for i in range(26, 0, -1):
        rr = rad + i * 13
        a = int(8 * (i / 26))
        draw.ellipse([cx - rr, cy - rr, cx + rr, cy + rr], fill=(255, 213, 128, a))
    draw.ellipse([cx - rad, cy - rad, cx + rad, cy + rad], fill=(255, 198, 73))


def grass(draw, horizon, n):
    rng = random.Random(7)
    band = H - horizon
    for _ in range(n):
        x = rng.uniform(-12, W + 12)
        h = rng.uniform(band * 0.45, band * 1.15)
        width = rng.uniform(2.0, 4.6)
        lean = rng.uniform(-0.18, 0.18) * h
        draw.polygon([(x - width, H), (x + width, H), (x + lean, H - h)],
                     fill=rng.choice(GREENS))


# --- Title page: round sun cresting a taller band of grass, like the icon ---
horizon = int(H * 0.72)
img = sky(horizon)
d = ImageDraw.Draw(img, "RGBA")
sun(d, horizon, rad=int(W * 0.17), peek=int(H * 0.26))
grass(d, horizon, n=1100)
img.save("assets/grassapp_ppt_title.png")

# --- Ordinary page: a gentle blue wash, same direction as the title ---------
# Bluer at the top and fading to pale toward the grass (like the title sky),
# but a weak gradient so the top stays light enough for black body text.
horizon = int(H * 0.88)
img = sky(horizon, top=(188, 216, 243), bot=(233, 241, 250))
d = ImageDraw.Draw(img, "RGBA")
grass(d, horizon, n=900)
img.save("assets/grassapp_ppt_content.png")

print("wrote assets/grassapp_ppt_title.png and assets/grassapp_ppt_content.png")
