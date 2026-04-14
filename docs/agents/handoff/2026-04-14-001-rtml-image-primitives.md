# Handoff: RTML IMAGE/RENDER/FUSE primitives + nav button pixel analysis

**Date:** 2026-04-14  
**Branch:** `viaweb`

---

## What was accomplished

### rtml.arc — RENDER / FUSE / IMAGE fully implemented

All three core RTML image primitives are now working:

- **`RENDER`** — renders text or image (or both) to a PNG rim. Supports: `text:`, `image:`, `font:`, `font-size:`, `text-color:`, `background-color:`, `text-align:`, `top/bottom/left/right-margin:`, `min/max-width/height:`, `thickness:`, `intaglio:`, `destination:`, `alt:`.
  - **Fixed-size button path**: when `min-width = max-width` and `min-height = max-height`, skips variable-width render+trim and instead calls `render-text-on-canvas` directly (avoids resize distortion).
  - **Intaglio**: when `intaglio: t`, calls `apply-shadow` on the transparent text image before flattening onto background, producing a 60%-opacity black drop shadow at offset (-1,-1).

- **`FUSE`** — composites a sequence of rims into one image along `axis:` (`'horizontal` or `'vertical`). Tracks each child's position as hotspots for image-map generation. Propagates nested hotspots from sub-FUSEs.

- **`IMAGE`** — emits `<img>` or `<map>+<img usemap="...">` depending on whether hotspots are present.

### Helper functions added/fixed in rtml.arc

| Function | Change |
|---|---|
| `make-rim` | New — wraps a PNG path into a `{type:'rim path width height destination alt hotspots}` table |
| `unique-id*` / `unique-id` | New — sequential counter for `map-` id generation |
| `render-image-src` | New — copies or downloads a source image, optionally resizes |
| `render-text-over` | New — annotates text over a background image |
| `resize-rim` | New — resizes to fit max/min constraints |
| `add-margins` | New — adds transparent/colored padding via `-splice`/`-extent` |
| `render-text-on-canvas` | Fixed: uses `-draw "text x,-1"` + `-kerning 0.0` (was `-annotate`, which doesn't match `imbutton` behavior) |
| `apply-shadow` | New — 60%-black hard shadow at -1,-1, merged with `-layers merge` |
| `add-background` | Fixed: uses `-flatten` (was manual `-size xc: ... -composite`) |
| `add-frame` | Fixed: mattecolor `#999999` (was `silver`/bgcolor); plain `-frame txT+t+0` raised bevel |

### imbutton in pg.arc

The user reverted `imbutton` to the original implementation (near-white text `#f7f7f7`, shadow at -1,-1, mattecolor `#a0a0a0`, `-frame "2x2+2+0"`). The pixel-perfect pixel-perfect rewrite (manual bevel drawing, 69×21 layout with correct web-safe colors) was explored but not kept.

**The pixel-correct imbutton was developed and tested:**
- Uses `( content-62x15 ) ( transparent-69x21 ) +swap -geometry +3+4 -composite` to place content at the right offset
- **Critical bug**: `-gravity west` inside a `( ... )` parenthetical group leaks out and corrupts `-composite` positioning — must add `-gravity none` at the end of the sub-image group to contain it
- Then manually draws bevel with exact hex values

The user chose to keep the original `imbutton` for now.

---

## Key decisions

- **`render-text-on-canvas` uses `-draw "text x,-1"` not `-annotate`**: This matches `imbutton`'s draw style exactly (same y=-1 baseline nudge, same kerning).
- **Fixed-size path in RENDER**: Detected by `min-width = max-width AND min-height = max-height`. Bypasses resize entirely; left-margin acts as pixel offset in the `-draw` call.
- **`add-frame` is always raised**: The `intaglio` parameter is accepted but the frame direction is not reversed. Intaglio effect is in `apply-shadow` (text shadow), not in the bevel.
- **`-gravity west` leaks out of ImageMagick sub-images**: Any gravity set inside `( ... )` affects subsequent operators like `-composite`. Always reset with `-gravity none` before closing the paren group.
- **Mattecolor `#999999`**: Produces web-safe bevel colors that match the original paulgraham.com buttons exactly when quantized (ImageMagick's continuous colors round to the same web-safe steps).

---

## Current state

- Branch: `viaweb` (diverged from `main`)
- Last meaningful commit: `a93c1ed WIP add-frame shading`
- `rtml.arc`: RENDER/FUSE/IMAGE complete; helper functions in place
- `pg.arc`: `imbutton` is the original (not the pixel-perfect rewrite)
- Working tree: likely has uncommitted changes to `rtml.arc` and `pg.arc` from this session

### Build

```sh
cd /Users/shawn/ml/pg && ./pg.arc
```

Generates the site HTML and PNG files. Requires ImageMagick (`magick`) and the MetaPlus fonts in `assets/fonts/`.

---

## Remaining / next steps

- Wire up `viaweb.arc`'s `nav-buttons.` / `nav-bar.` templates to the RENDER/FUSE/IMAGE primitives (they currently call stubs or use `imbutton` directly from `pg.arc`)
- The `imbutton` in pg.arc could be replaced with a RENDER call once the pixel-perfect bevel drawing is settled
- `intaglio` frame direction: the user wants intaglio to use the same `-frame` (raised) — this is the current state. If a true sunken bevel is needed later, the rotate-180-frame-rotate-180 trick works but was ruled out for now
- `nav-button.` and `nav-bar.` RTML templates in `viaweb.arc` are stubs; they need to be filled in using RENDER/FUSE
