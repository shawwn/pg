#!/usr/bin/env sparc

(or= pages* (obj))

(= rootdir* (expandpath "./")
   pagesdir* (+ rootdir* "pages/"))

(def load-page (id)
  (w/param cwd pagesdir*
    (zap sym id)
    (fromfile (cat id ".page")
      (with p (eval `(obj ,@(read)))
        (= p!id id
           p!counter 0
           p!text (multisubst (list
                                (list "\n\n" "<br /><br />")
                                )
                    (allchars))
           (pages* id) p)
        ))))

(= site* (load-page 'index)
   self* (make-param nil false 'self*))

(mac with-object (id . body)
  `(w/param self* (assert (pages* ,id))
     ,@body))

(def @ (prop (o fail))
  (let x (aand (self*) (it prop))
    (if (~null x) x
      (site* prop fail))))

(def set-prop (prop value)
  (= ((or (self*) site*) prop) value))

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
  (if (isa!sym dest)
      (html-file dest)
      dest))

(mac sitetable (width . body)
  `(tag (table border 0 cellspacing 0 cellpadding 0 width ,width)
     ,@body))

(mac page (name title . body)
  (w/uniq (go ti)
    `(tofile (html-file ,name)
       (let ,ti ,title
         (tag html
           (tag head
             ;(gentag meta name 'viewport content "width=device-width, initial-scale=1.0")
             (gentag meta name 'description             content @!site-desc)
             ;(gentag meta name 'theme-color             content "#@(hexrep teal)")
             ;(gentag meta name 'msapplication-TileColor content "#@(hexrep orangered)")
             
             (gentag meta property 'og:type             content "website")
             (gentag meta property 'og:title            content ,ti)
             (gentag meta property 'og:site_name        content @!site-name)
             (gentag meta property 'og:description      content @!site-desc)
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
                 (td (navbuttons))
                 (td (shim 1 28))
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

(def gen-contents ()
  (page 'index @!site-name
    (tag (font size 2 face 'verdana)
      (gentag img src "https://s.turbifycdn.com/aah/paulgraham/essays-6.gif"
              width 410 height 45
              border 0 hspace 0 vspace 0)
      (br 2)
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
                  (tag (font size 2)
                    (prn "\n<b>New:</b>\n")
                    (link "Superlinear Returns" "superlinear.html")
                    (prn " |")
                    (link "How to Do Great Work" "greatwork.html")
                    (prn))
                  (br)
                  (spacer 5))))
            (tag (table width 410 cellspacing 0)
              (tr
                (tdcolor (color 0xff 0x99 0x22)
                  (spacer 15)
                  (tag (font size 2)
                    (prn "\n<b>Want to start a startup?</b> Get funded by ")
                    (link "Y Combinator" "http://ycombinator.com/apply.html")
                    (prn "."))
                  (br)
                  (spacer 5))))
            (br 2)
            (gentag link rel "alternate" type "application/rss+xml" title "RSS" href "pgessays.rss")
            )))
      (br)
      (sitetable 435
        (trtd
          (tag (font size 2 face 'verdana)
            (br)
            (tag (font size 1)
              (tag (font color (color 0xcc 0xcc 0xcc))
                (pr "&copy; " (romannum:car:date) " pg")))))
        )
      (br))
    ))

(def gen-index ()
  (page 'ind (cat @!site-name " Index")
    (gentag img src "https://s.turbifycdn.com/aah/paulgraham/essays-6.gif"
            width 410 height 45
            border 0 hspace 0 vspace 0)
    (br 2)
    (sitetable 435
      (trtd
        (each i (all-items)
          (link i!title (to i!id))
          (br))))))

(def all-items ((o sections (keys pages*)))
  (let tems nil
    (each s sections
      (with-object s
        (when @!title
          (pushnew (self*) tems (compare is !title)))))
    (sort (compare < !title) tems)))

(def gen-rss ()
  (tofile "pgessays.rss"
    (prn "<rss version=\"2.0\" xmlns:content=\"http://purl.org/rss/1.0/modules/content/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"><channel>")
    (prn "  <title>" @!site-desc "</title>")
    (prn "  <link>" @!site-url "</link>")
    (prn "  <description>Scraped feed provided by aaronsw.com</description>")
    (with-object 'articles
      (each x @!contents
        (with-object x
          (prn "  <item>")
          (prn "    <link>" @!site-url (to @!id) "</link>")
          (prn "    <title>" @!title "</title>")
          (prn "  </item>"))))
    (prn "</channel></rss>")))

(def render-object (x)
  (if (isa!sym x)
      (with-object x
        (link @!title (to @!id)))
      (isa!table x)
      (case x!type
        image (let v (tostring:gentag img src x!src
                                      width x!width height x!height align x!align
                                      border (or x!border 0) hspace (or x!hspace 0) vspace (or x!vspace 0)
                                      alt (or x!alt "Click to enlarge"))
                (aif x!destination (link v it) (pr v)))
        link (link x!title x!src)
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

(def gen-section ()
  (page @!id @!title
    (link (tostring:gentag img src "https://s.turbifycdn.com/aah/paulgraham/essays-6.gif"
                           width 410 height 45
                           border 0 hspace 0 vspace 0)
          (to 'index))
    (br 2)
    (sitetable 435
      (tag (tr valign 'top)
        (tag (td width 435)
          (awhen @!image
            (render-object it)
            (shim (+ it!height 8) 10 align: 'left))
          (display-text (or @!headline @!title))
          (tag (font size 2 face 'verdana)
            (pr @!text)
            (when @!image
              (tag (br clear 'all)))))))
    (when @!contents
      (br)
      (sitetable 435
        (each cols (tuples @!contents (either @!columns 1))
          (rowshim (either @!margin-top 5))
          (tag (tr valign 'top)
            (on x cols
              (unless (is index 0)
                (td (shim 8)))
              (tag (td width (either @!column-width (if (> (either @!columns 1) 1) 210 421)))
                (gentag img src "https://s.turbifycdn.com/aah/paulgraham/how-to-get-new-ideas-5.gif"
                        width 12 height 14 align 'left border 0 hspace 0 vspace 0)
                (tag (font size 2 face 'verdana)
                  (render-object x)
                  (shim 2)))))
          (rowshim (either @!margin-bottom 8)))))
    (br)
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

(def gen-site ()
  (each name (sort < (dir pagesdir*))
    (when (endmatch ".page" name)
      (let id (sym:cut name 0 -5)
        (load-page id))))
  (gen-contents)
  (gen-index)
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
    (cat name "-" n ".png")))

(def render-color (col)
  (if (isa!sym col) (cat col) (cat "#" (hexrep col))))

(def render-text (text
                   (o :text-color black)
                   (o :text-align 'left)
                   (o :background-color 'none)
                   (o :font 'verdana)
                   (o :kerning 0)
                   (o :font-size 18))
  (with img (render-image-name)
    (shell 'convert
           '-font font
           '-pointsize font-size
           '-kerning kerning
           '-gravity "west"
           '-size "1500x@(round font-size)"
           '-fill (render-color text-color)
           "xc:"
           '-background (render-color background-color)
           ;"label:@text"
           '-draw "text 0,-1 @(tostring:write text)"
           '-define' "trim:edges=east,west" '-trim '+repage
           img)))

(def imsize (img)
  (map int (tokens (shell 'identify '-format "%w %h" img))))

(def imwidth (img)
  (car (imsize img)))

(def imheight (img)
  (cadr (imsize img)))

(defmemo imtitle (text)
  (render-text text
               font: (+ rootdir* "assets/fonts/metaplusbook-caps.ttf")
               ;kerning: 0.28
               font-size: 17.5
               text-color: (color 0x64 0x1b 0x16)))

(defmemo imbutton (text)
  (with img (render-image-name)
    (shell 'convert
           '-size '62x15
           '-background 'none
           "xc:"
           '-gravity 'west
           '-font (+ rootdir* "assets/fonts/metaplusbold-roman.ttf")
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

(def navbutton (text dest)
  (link (tostring:gentag img src (imbutton text)
                         width 67 height 21
                         border 0 hspace 0 vspace 0)
        (to dest))
  (br))

(def navbuttons ()
  ;(gentag img src "https://s.turbifycdn.com/aah/paulgraham/img-26.gif"
  ;        width 69 height 399
  ;        border 0 hspace 0 vspace 0)
  (awhen (tostring
           (navbutton "Home" 'index))
    (if (is @!id 'index)
        (do (shim 21 69) (br))
        (pr it)))
  (navbutton "Essays" 'articles)
  (navbutton "H&P" "http://www.amazon.com/gp/product/0596006624")
  (navbutton "Books" "books.html")
  (navbutton "YC" "https://ycombinator.com")
  (navbutton "Arc" 'arc)
  (navbutton "Bel" 'bel)
  (navbutton "Lisp" "lisp.html")
  (navbutton "Spam" "spam.html")
  (navbutton "Responses" "responses.html")
  (navbutton "FAQs" "faq.html")
  (navbutton "RAQs" "raq.html")
  (navbutton "Quotes" "quo.html")
  (navbutton "RSS" "pgessays.rss")
  (navbutton "Bio" 'bio)
  (navbutton "Twitter" "https://twitter.com/paulg")
  (navbutton "Mastodon" "https://mas.to/@@paulg")
  (awhen (tostring
           (navbutton "Index" 'ind)
           (navbutton "Email" 'info))
    (when (is @!id 'index)
      (pr it)))
  nil)

gen-site
