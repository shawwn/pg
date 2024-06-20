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

# customizing your site

TODO. Stay tuned!

