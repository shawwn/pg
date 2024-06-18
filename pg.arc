#!/usr/bin/env arc

(require (libpath "html.arc"))

(or= pages* (obj) rootdir* (expandpath "."))

(def load-text (text)
  (aand (multisubst (list (list "\n\n" "<br /><br />"))
                    text)
        (eval `(string ,@(codestring it)))))

(def load-page (id)
  (ero 'load-page id)
  (zap sym id)
  (fromfile (cat id ".page")
    (with p (eval `(obj ,@(read)))
      (unless p!disabled
        (= p!id id
           p!counter 0
           p!text (allchars)
           (pages* id) p)
        (or= p!template (or @!default-template 'page))))))

(def load-pages ((o pagesdir))
  (w/param cwd (or pagesdir (cwd))
    (= site* (assert (load-page 'index)))
    (each name (sort < (glob "*.page"))
      (let id (sym:cut name 0 -5)
        (unless (is id 'index)
          (load-page id))))))

(or= site* nil self* (make-param nil false 'self*))

(def as-object (x)
  (if (null x)
       x
      (isa!sym x)
       (assert (pages* x) "Page '@x' doesn't exist")
      (isa!table x)
       x
      (isa!fn x)
       (as-object (x (current-object)))
       (err "Can't use as object" x)))

(mac with-object (x . body)
  (w/uniq v
    `(whenlet ,v (as-object ,x)
       (w/param self* ,v
         ,@body))))

(mac each-object (lst . body)
  (w/uniq var
    `(each ,var ,lst
       (with-object ,var
         ,@body))))

(def @ (prop (o fail))
  (let x (aand (self*) (it prop))
    (if (~null x) x
        site*     (site* prop fail)
                  fail)))

(def current-object ()
  (or (self*) site*))

(def set-prop (prop value)
  (def me (current-object))
  (= (me prop) value))

(defset @ (prop (o fail))
  (w/uniq p
    (list (list p prop)
          `(@ ,prop ,fail)
          `(fn (val) (set-prop ,p val)))))

(deftem item
  id       nil
  title    nil
  text     nil
  contents nil
  counter  0)

(mac defitem (id title text :kwargs . body)
  `(= (pages* ,id)
      (inst 'item
            'id       ,id
            'title    ,title
            'text     ,text
            'contents (list ,@body)
            ,@kwargs)))

(def html-file (dest)
  (cat dest ".html"))

(def to (dest)
  (assert dest)
  (if (isa!sym dest)
      (html-file dest)
      dest))

(mac sitetable (width . body)
  `(tag (table border 0 cellspacing 0 cellpadding 0 width ,width)
     ,@body))

(mac page (name title . body)
  (w/uniq ti
    `(tofile (html-file ,name)
       (let ,ti ,title
         (tag html
           (tag head
             ;(gentag meta name 'viewport content "width=device-width, initial-scale=1.0")
             ;(gentag meta name 'description             content @!site-desc)
             ;(gentag meta name 'theme-color             content "#@(hexrep teal)")
             ;(gentag meta name 'msapplication-TileColor content "#@(hexrep orangered)")
             
             (gentag meta property 'og:type             content "website")
             (gentag meta property 'og:title            content ,ti)
             (gentag meta property 'og:site_name        content @!site-name)
             ;(gentag meta property 'og:description      content @!site-desc)
             (awhen @!site-image
               (gentag meta property 'og:image            content it!url)
               (gentag meta property 'og:image:type       content it!type)
               (gentag meta property 'og:image:width      content it!width)
               (gentag meta property 'og:image:height     content it!height))

             (tag title (pr ,ti))
             
             (awhen @!favicon-url
               (gentag link rel "shortcut icon" href it))
             )
           (tag (body text @!text-color
                      link @!link-color
                      vlink @!visited-link-color)
             (sitetable nil
               (tag (tr valign 'top)
                 (td (navbuttons ,name))
                 (td (shim 1 26))
                 (td ,@body)))))))))

(def romandigit (n (o chars "ivx"))
  (defs one (chars 0) five (chars 1) ten (chars 2))
  (case n
    0 (cat)
    1 (cat one)
    2 (cat one one)
    3 (cat one one one)
    4 (cat one five)
    5 (cat five)
    6 (cat five one)
    7 (cat five one one)
    8 (cat five one one one)
    9 (cat one ten)
    (err "Bad digit")))

(def romannum (n)
  (def ones (mod n 10)) (= n (trunc:/ n 10))
  (def tens (mod n 10)) (= n (trunc:/ n 10))
  (def hund (mod n 10)) (= n (trunc:/ n 10))
  (def thou (mod n 10)) (= n (trunc:/ n 10))
  (cat (romandigit thou "m??")
       (romandigit hund "cdm")
       (romandigit tens "xlc")
       (romandigit ones "ivx")))

(def spacer (height (o width 1))
  (gentag img src "http://www.virtumundo.com/images/spacer.gif" height height width width))

(def rss-url ()
  (or @!rss-url "essays.rss"))

(def gen-contents ()
  (page 'index @!site-name
    (tag (font size 2 face 'verdana)
      (site-banner)
      (gentag img src "https://s.turbifycdn.com/aah/paulgraham/index-14.gif"
              width 410 height 308
              border 0 hspace 0 vspace 0)
      (br 2)
      (sitetable 435
        (trtd
          (tag (font size 2 face 'verdana)
            (tag (table width 410 cellspacing 0)
              (tr
                (tdcolor (color 0xff 0xcc 0x33)
                  (spacer 15)
                  (tag (font size 2) (prn)
                    (tag b "New:") (prn)
                    (link "How to Start Google" "google.html") (prn " |")
                    (link "Best Essay" "best.html") (prn " |")
                    (link "Superlinear" "superlinear.html") (prn))
                  (br)
                  (spacer 5))))
            (tag (table width 410 cellspacing 0)
              (tr
                (tdcolor (color 0xff 0x99 0x22)
                  (spacer 15)
                  (tag (font size 2) (prn)
                    (tag b "Want to start a startup?") (prn " Get funded by ")
                    (link "Y Combinator" "http://ycombinator.com/apply.html") (prn "."))
                  (br)
                  (spacer 5))))
            (br 2)
            (gentag link rel "alternate" type "application/rss+xml" title "RSS" href (rss-url))
            )))
      (br)
      (sitetable 435
        (trtd
          (tag (font size 2 face 'verdana)
            (br)
            (tag (font size 1)
              (tag (font color (color 0xcc 0xcc 0xcc))
                (pr "&copy; " (romannum:car:date) " " (or @!author "pg"))))))
        )
      (br))
    ))

(def gen-sitemap ()
  (with-object (copy site*)
    (= @!id 'ind @!title (cat @!site-name " Index"))
    (page @!id @!title
      (site-banner)
      (sitetable 435
        (trtd
          (each-object (all-items)
            (unless @!hidden
              (link @!title (to @!id))
              (br))))))))

(def all-items ((o sections (keys pages*)))
  (let tems nil
    (each s sections
      (with-object s
        (when @!title
          (pushnew (self*) tems (compare is !title)))))
    (sort (compare < !title) tems)))

(def gen-rss ()
  (tofile (rss-url)
    (prn "<rss version=\"2.0\" xmlns:content=\"http://purl.org/rss/1.0/modules/content/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><channel>")
    (prn "  <title>" (or @!site-desc @!site-name) "</title>")
    (prn "  <link>" @!site-url "</link>")
    (prn "  <description>" (or @!rss-desc "") "</description>")
    (with-object 'articles
      (each-object @!contents
        (unless (or @!hidden (is @!type 'link))
          (prn "  <item>")
          (prn "    <link>" @!site-url (to @!id) "</link>")
          (prn "    <title>" @!title "</title>")
          (prn "  </item>"))))
    (prn "</channel></rss>")))

(def render-object (x)
  (ero 'render-object x)
  (if (isa!sym x)
      (with-object x
        (ero 'link @!title @!id)
        (link @!title (to @!id)))
      (isa!table x)
      (case x!type
        image (withs (url x!url
                      width (or x!width (imwidth url))
                      height (or x!height (imheight url)))
                (let v (tostring
                         (gentag img src url
                                 width width height height align x!align
                                 border (or x!border 0) hspace (or x!hspace 0) vspace (or x!vspace 0)
                                 alt (or x!alt (if x!destination "Click to enlarge"))))
                  (aif x!destination (link v it) (pr v))))
        link (link (assert (or x!label x!title)) x!url)
        (err "Don't know how to render" x))
      (isa!string x)
      (pr (multisubst (list (list "\n\n" "<br /><br />"))
                      x))
      (err "Don't know how to render" x)))

(def rowshim (height)
  (when (and height (> height 0))
    (trtd (shim height))))

(def shim (height (o width 1) :align)
  (gentag img src "https://sep.turbifycdn.com/ca/Img/trans_1x1.gif"
          height height width width border 0 align align))

(def display-text (text)
  (unless (empty text)
    (let im (imtitle text)
      (gentag img src im
              height (imheight im) border 0 hspace 0 vspace 0
              alt text))
    (br 2)))

(def site-banner ((o text @!site-name))
  (unless (empty text)
    (let im (imbanner (downcase text))
      (tag-if (isnt @!id 'index) a href: (to 'index)
        ;(gentag img src "https://s.turbifycdn.com/aah/paulgraham/essays-6.gif"
        ;        width 410 height 45
        ;        border 0 hspace 0 vspace 0)
        (gentag img src im
                width (imwidth im) height (imheight im)
                border 0 hspace 0 vspace 0)))
    (br 2)))

(def bullet (:align)
  (gentag img src @!bullet-url
          width @!bullet-width height 14 border 0 hspace 0 vspace 0 align align))

(def gen-section ()
  (page @!id @!title
    (site-banner)
    (sitetable 435
      (tag (tr valign 'top)
        (tag (td width 435)
          (awhen @!image
            (let im (if (isa!string it) (inst 'image url: it destination: it) it)
              (render-object im)
              (if (is im!align 'left)
                  (shim (+ (or im!height (imheight im!url)) 8) 10 align: 'left)
                  (br 2))))
          (display-text (or @!headline @!title))
          (tag (font size 2 face 'verdana)
            (pr (load-text @!text))
            (when @!image
              ;(tag (br clear 'all)) ; incorrect: <br clear=all></br>
              (pr "<br clear=\"all\" />")
              )))))
    (only&pr @!caption)
    (when @!contents
      (sitetable 435
        (defs n   (either @!columns 1)
              wid (either @!column-width (if (> n 1) 210 421)))
        (each cols (segments n @!contents)
          (rowshim (either @!margin-top 5))
          (tag (tr valign 'top)
            (on x cols
              (unless (is index 0)
                (td (shim 8)))
              (unless @!bullet-inline
                (tag (td width (assert @!bullet-width))
                  (tag center
                    (bullet)))
                (tag (td width 8)
                  (shim 8)))
              (tag (td width wid)
                (when @!bullet-inline
                  (bullet align: 'left))
                (tag (font size 2 face 'verdana)
                  (render-object x)
                  (br)
                  (unless @!bullet-inline
                    (shim 2))))))
          (rowshim (either @!margin-bottom 8)))))
    (only&pr @!final-text)
    (unless @!nofoot
      (sitetable 435
        (trtd
          (tag (font size 2 face 'verdana)
            (awhen @!bottom-text
              (br 2)
              (pr it))
            (br 2)
            (gentag hr)
            (only&pr @!footer)
            ))))))

(def gen-site ((o pagesdir (expandpath "pages")))
  (load-pages pagesdir)
  (gen-contents)
  (gen-sitemap)
  (gen-rss)
  (each (k v) pages*
    (unless (is k 'index)
      (with-object k
        (gen-section)))))


(def clean-name (name)
  (multisubst (list (list "--" "-"))
              (map [if (alphadig _) _ #\-]
                   (downcase name))))

(def render-image-name ()
  (withs (name (clean-name (or @!title (cat @!id)))
          n (++ (@ 'counter 0)))
    (ero (cat name "-" n ".png") 'image-name)))

(def render-color (col)
  (if (isa!sym col) (cat col) (cat "#" (hexrep col))))

(def escaped (x)
  (multisubst (list (list "\\n" "\n"))
    (tostring:write x)))

(def render-text (text
                   (o :text-color black)
                   (o :text-align 'left)
                   (o :background-color 'none)
                   (o :font 'verdana)
                   (o :kerning 0)
                   (o :font-size 18)
                   (o :gravity "west")
                   (o :trim-edges "east,west")
                   (o :size "1500x@(* (round font-size) (len:lines text))"))
  (with img (render-image-name)
    (shell 'convert
           '-font font
           '-pointsize font-size
           '-kerning kerning
           '-gravity gravity
           '-size size
           '-interline-spacing -3
           '-fill (render-color text-color)
           "xc:"
           '-background (render-color background-color)
           '-draw "text 0,-1 @(escaped text)"
           '-define "trim:edges=@trim-edges" '-trim '+repage
           img)))

(def imsize (img)
  (if (valid-url img)
      (fromstring (GET img :bytes)
        (imsize "-"))
      (map int (tokens (shell 'identify '-format "%w %h" img)))))

(defmemo imwidth (img)
  (car (imsize img)))

(defmemo imheight (img)
  (cadr (imsize img)))

(defmemo imtitle (text)
  (= text (multisubst (list (list "-" "–"))
                      text))
  (render-text text
               font: (expandpath "assets/fonts/metaplusbook-caps.ttf" rootdir*)
               ;kerning: 0.28
               font-size: 17.5
               text-color: (color 0x7f 0x1b 0x16)))

(defmemo imbanner (text)
  (render-text text
               kerning: (or @!banner-kerning 6.5)
               font-size: (or @!banner-size 62)
               size: "1500x45"
               gravity: 'south
               font: (expandpath "assets/fonts/metaplusbook-caps.ttf" rootdir*)
               trim-edges: "east,west,north"
               text-color: (color 247 247 252)))

(defmemo imbutton (text)
  (with img (render-image-name)
    (shell 'convert
           '-size '62x15
           '-background 'none
           "xc:"
           '-gravity 'west
           '-font (expandpath "assets/fonts/metaplusbold-roman.ttf" rootdir*)
           '-fill (render-color (color 0xf7 0xf7 0xf7))
           '-pointsize "11.95" '-kerning 0.0
           '-draw (+ "text 2,-1 " (tostring:write text))
           "(" '+clone
               '-background 'black
               '-shadow "60x0-1-1"
           ")"
           '+swap
           '-background (render-color (color 0x66 0x66 0x99))
           '-layers 'merge
           '-mattecolor (render-color (color 0xa0 0xa0 0xa0))
           '-frame "2x2+2+0"
           img)))

(def maxim (im height width)
  (with img (render-image-name)
    (shell 'convert im
           '-resize "@{width}x@{height}>"
           img)))

(def shown-image ()
  (if @!template
      (or @!icon @!image)
      (or @!image @!icon)))

(def navbutton ((o text @!title) (o dest (or @!url @!id)))
  (assert text "navbutton: @@!title not set")
  (link (tostring:gentag img src (with-object 'index
                                   (imbutton text))
                         alt text
                         width 67 height 21
                         border 0 hspace 0 vspace 0)
        (to dest))
  (br))

(def navbuttons ((o id @!id))
  ;(gentag img src "https://s.turbifycdn.com/aah/paulgraham/img-26.gif"
  ;        width 69 height 399
  ;        border 0 hspace 0 vspace 0)
  (awhen (tostring
           (navbutton "Home" 'index))
    (if (is id 'index)
        (do (shim 21 69) (br))
        (pr it)))
  (each-object @!buttons
    (navbutton))
  nil)

(def make-link (title url)
  (inst 'link :title :url))

(def segments (n seq)
  (tuples seq n))

gen-site
