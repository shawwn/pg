# pg.arc

[pg.arc](https://github.com/shawwn/pg/blob/main/pg.arc) is a site generator
for [paulgraham.com](http://paulgraham.com)-style websites.
Since paulgraham.com was made with [Viaweb](https://paulgraham.com/vw.html),
I ended up reimplementing most of what made Viaweb special. And since
it's written in Arc, it ended up being just a [few hundred lines](https://github.com/shawwn/pg/blob/main/pg.arc).

# demo

Live site: [https://shawwn.github.io/pg/](https://shawwn.github.io/pg/)

<img width="861" alt="image" src="https://github.com/shawwn/pg/assets/59632/c7f924f5-f452-475f-a1c6-1a009ef81a1e">

By default, this repository generates a facsimile of paulgraham.com,
which you can see above. But it's easy to make your own;
the reason I wrote pg.arc was to make mine:
[https://shawwn.github.io](https://shawwn.github.io)

# setup

- Install cmake, rlwrap, and imagemagick 7:

## macOS

```
brew install cmake rlwrap imagemagick
```

## Linux

```
apt install -y cmake rlwrap
```

**NOTE: imagemagick 7 is required, but Ubuntu's apt repository only
has 6**. Download it from
[https://imagemagick.org/script/download.php#linux](https://imagemagick.org/script/download.php#linux):

```sh
# install fuse, which is required for `magick` AppImage.
sudo apt install -y fuse

# download `magick` AppImage to ~/.bin and make it executable.
mkdir -p ~/.bin
wget https://imagemagick.org/archive/binaries/magick -O ~/.bin/magick
chmod +x ~/.bin/magick

# ensure ~/.bin is on your PATH, e.g.
pip3 install userpath
userpath prepend ~/.bin
exec $SHELL
```

Then `magick --version` should show something similar to this:
```
$ magick --version
Version: ImageMagick 7.1.1-33 Q16-HDRI x86_64 e31ad5194:20240524 https://imagemagick.org
Copyright: (C) 1999 ImageMagick Studio LLC
License: https://imagemagick.org/script/license.php
Features: Cipher DPC HDRI OpenMP(4.5)
Delegates (built-in): bzlib djvu fontconfig freetype heic jbig jng jp2 jpeg lcms lqr lzma openexr png raqm tiff webp x xml zlib
Compiler: gcc (9.4)
```

## Installing `sparc`
- Install [https://github.com/shawwn/sparc](https://github.com/shawwn/sparc):
```
# clone the sparc repo.
git clone https://github.com/shawwn/sparc ~/sparc

# ensure `sparc/bin` is on your PATH, e.g.
pip3 install userpath
userpath append ~/sparc/bin
exec $SHELL
```

# running pg.arc

Clone the repo and cd into it:
```
git clone https://github.com/shawwn/pg
cd pg
```

Run the site generator:
```
./pg.arc
```

Fire up a webserver and open [http://localhost:8000](http://localhost:8000):
```
python3 -m http.server
```

You should see something similar to the demo screenshot.

# building the site

Since pg.arc will spit out pngs that are perceptually identical but
not byte-for-byte identical, every call to `./pg.arc` will cause all
pngs in the repo to be marked as modified, even though they haven't
changed visually. That's annoying.

Instead, I run pg.arc like this:
```
./pg.arc && git-reset-perceptualdiff '*.png'
```

Where `git-reset-perceptualdiff` is a part of
[scrap](https://github.com/shawwn/scrap).

That will revert all the images that are perceptually identical,
resulting in a clean repository.

# deploying to GitHub Pages

Go to your repo's **Settings → Pages**, set the source to **Deploy from a branch**, choose `main`, and set the folder to `/ (root)`. Save it.

After that, every `git push` to `main` will trigger a build. The site goes live at `https://<username>.github.io/<repo>/` within about 30 seconds.

You can check build status with:
```sh
gh api repos/<owner>/<repo>/pages/builds --jq '.[0] | {status, error, updated_at}'
```

# importing pg essays

To import pg essays, for example [The Brand Age](https://paulgraham.com/brandage.html):
```
echo '(title: "The Brand Age")' > pages/brandage.page
pg-import https://paulgraham.com/brandage.html >> pages/brandage.page
```
Where `pg-import` is a part of
[scrap](https://github.com/shawwn/scrap).

Then open `pages/articles.page` and add `'brandage` to the list of
articles, which for me at the time of writing this was above
`'reddits`.

Then follow the above steps for `building the site`:
```
./pg.arc && git-reset-perceptualdiff '*.png'
```
The output can be [found
here](https://gist.githubusercontent.com/shawwn/82246277f6cf8f80a66e959b9d72e41c/raw/7677a72716a085428e528f952a300185fc573d7b/gistfile1.txt).

Let's look at `git status` to see what pg.arc did:
```
$ git status
On branch main
Your branch is up to date with 'origin/main'.

Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git restore <file>..." to discard changes in working directory)
	modified:   README.md
	modified:   articles.html
	modified:   ind.html
	modified:   pages/articles.page
	modified:   pgessays.rss

Untracked files:
  (use "git add <file>..." to include in what will be committed)
	brandage.html
	pages/brandage.page
	the-brand-age-1.png

no changes added to commit (use "git add" and/or "git commit -a")
```

`README.md` is listed as modified because I'm presently writing it.

The rest are expected changes: it added the article to
`articles.html`, `ind.html` (the site index), and `pgessays.rss` (the
RSS feed). Lastly, it made `brandage.html` and `the-brand-age-1.png`,
the essay's title image displayed at the top of `brandage.html`.

Now commit and push:
```
git add .
git commit -m "Add brandage.html"
git push
```

And the new essay shows up on the live site within a minute or so:
[https://shawwn.github.io/pg/brandage.html](https://shawwn.github.io/pg/brandage.html)
