#!/usr/bin/env python3
# fig_edges.py --- Figure "The child friendships at the end of each month" of
# CSSN's simulation section, from the measures of one run (measure.py --json):
# a line of the child friendships at the end of each month 0 to 12.
#
#   python3 fig_edges.py <run.json> <png> "<caption line>"
#
# Drawn with PIL, as programs/cssn/figures/village.py draws Figure 2; the font
# Calibri where installed (the style guide: never Arial), Helvetica otherwise.
import json, sys, os
from PIL import Image, ImageDraw, ImageFont

def font(size):
    for p in ["/Library/Fonts/Calibri.ttf", "/System/Library/Fonts/Supplemental/Calibri.ttf",
              os.path.expanduser("~/Library/Fonts/Calibri.ttf"), "/System/Library/Fonts/Helvetica.ttc"]:
        if os.path.exists(p):
            try: return ImageFont.truetype(p, size)
            except OSError: pass
    return ImageFont.load_default()

d = json.load(open(sys.argv[1]))
ys = [0] + [y if y is not None else 0 for y in d["child_friendships_by_month"]]
W, H = 1600, 900
L, R, T, B = 150, 60, 110, 130
img = Image.new("RGB", (W, H), "white")
g = ImageDraw.Draw(img)
f, fs, ft = font(30), font(26), font(34)
ymax = max(ys) if max(ys) > 0 else 1
step = 1
for s in (1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000):
    if ymax / s <= 8: step = s; break
top = step * (-(-ymax // step))
def X(m): return L + (W - L - R) * m / 12.0
def Y(v): return H - B - (H - T - B) * v / float(top)
for v in range(0, int(top) + 1, step):
    g.line([(L, Y(v)), (W - R, Y(v))], fill=(225, 225, 225), width=2)
    g.text((L - 15, Y(v)), str(v), fill="black", font=fs, anchor="rm")
for m in range(0, 13):
    g.line([(X(m), H - B), (X(m), H - B + 10)], fill="black", width=2)
    g.text((X(m), H - B + 18), str(m), fill="black", font=fs, anchor="mt")
g.line([(L, H - B), (W - R, H - B)], fill="black", width=3)
g.line([(L, T), (L, H - B)], fill="black", width=3)
pts = [(X(m), Y(v)) for m, v in enumerate(ys)]
g.line(pts, fill=(31, 119, 180), width=5)
for p in pts:
    g.ellipse([p[0] - 8, p[1] - 8, p[0] + 8, p[1] + 8], fill=(31, 119, 180))
g.text(((L + W - R) / 2, H - 45), "month", fill="black", font=f, anchor="mm")
lab = Image.new("RGB", (500, 50), "white")
ImageDraw.Draw(lab).text((250, 25), "child friendships", fill="black", font=f, anchor="mm")
lab = lab.rotate(90, expand=True)
img.paste(lab, (20, int((T + H - B) / 2 - 250)))
g.text(((L + W - R) / 2, 50), sys.argv[3], fill="black", font=ft, anchor="mm")
img.save(sys.argv[2])
print(sys.argv[2], ys)
