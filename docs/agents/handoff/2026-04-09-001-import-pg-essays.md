# Handoff: Import all PG essays from paulgraham.com

**Date:** 2026-04-09  
**Repo:** `/Users/shawn/ml/pg`  
**Branch:** `main`

---

## What was accomplished

- Imported all 231 essays from paulgraham.com/articles.html into `pages/`
- Rewrote `pages/articles.page` to list all 231 in paulgraham.com order
- Site builds cleanly with `./pg.arc`
- **Not yet committed** — ready to go (see below)

Also during this session (earlier work):

- Set up `~/scrap/git-reset-perceptualdiff` — a script that reverts PNGs which are perceptually identical to HEAD. Usage: `git-reset-perceptualdiff '*.png'`
- Configured `.git/config` with `diff.external = sh -c 'perceptualdiff "$2" "$5"' --` for visual PNG diffs
- Updated `README.md` to document the perceptualdiff workflow, GitHub Pages deployment, and the essay import process (including the UTF-8/ISO-8859-1 encoding fix)

---

## Key decisions

- **`(sym "...")` for numeric-prefixed slugs**: Arc's reader parses `'95`, `'5founders`, `'13sentences`, `'6631327` as numbers or fails. Used `(sym "95")` etc. in `articles.page` so they resolve to the correct page objects.
- **Unicode curly quotes for titles containing `"`**: `artistsship.page` and `gba.page` had double quotes in their titles that broke both Arc string parsing and ImageMagick's MVG draw command. Replaced with Unicode `"` / `"`.
- **`@@` to escape `@` in page content**: `seesv.page` had a Flickr URL `38037974@N00/...` which Arc's `load-text` (which calls `eval` on the HTML) tried to interpolate. Escaped as `@@N00`.
- **No iconv in import script**: The user removed the `iconv -f ISO-8859-1 -t UTF-8` pipe from the import function — encoding fixes are handled manually if needed (as documented in README.md).
- **`diff.external` not `diff=perceptualdiff` in `.gitattributes`**: A named diff driver in `.gitattributes` overrides `GIT_EXTERNAL_DIFF`, breaking the reset script. The `.gitattributes` only has `*.arc diff=default`; images use `diff.external` in `.git/config`.

---

## Important context for future sessions

### Pending commit

Everything is staged-ready but **not committed**. Run:

```sh
git add pages/ *.html *.png pgessays.rss
git commit -m "Import all PG essays from paulgraham.com/articles.html"
git push
```

### Build workflow

```sh
./pg.arc && git-reset-perceptualdiff '*.png'
```

`./pg.arc` generates HTML + PNGs from `pages/*.page`. `git-reset-perceptualdiff` reverts PNGs that are perceptually identical to HEAD (they're byte-different but visually the same every build).

### `.page` file format

```
(title: "Article Title")
<html content here>
```

First line is an Arc s-expression read by `load-page`. The rest is HTML text, evaluated via `(eval it)` in `load-text`. This means:
- `@symbol` in the HTML is treated as Arc string interpolation — escape with `@@`
- Double quotes inside the title string must be `\"` (Arc escape) — but this also breaks ImageMagick title rendering, so use Unicode curly quotes instead

### `articles.page` format

Arc list syntax. Slugs are `'symbol`. For slugs starting with digits use `(sym "slug")`. External links use `(make-link "Title" "url")`.

### Known encoding issue pattern

Some older PG essays are ISO-8859-1 encoded. If `./pg.arc` fails with `bytes->string/utf-8`, fix with:

```sh
iconv -f ISO-8859-1 -t UTF-8 pages/<slug>.page | sponge pages/<slug>.page
```

### Deployed site

GitHub Pages serves from root of `main` branch → https://shawwn.github.io/pg/  
Builds within ~30 seconds of a push.

### git-reset-perceptualdiff

Located at `~/scrap/git-reset-perceptualdiff` (part of the [scrap](https://github.com/shawwn/scrap) repo). Takes a pathspec: `git-reset-perceptualdiff '*.png'`.
