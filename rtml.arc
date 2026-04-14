(require (libpath "html.arc"))

(or= pages* (obj) site* nil rootdir* (expandpath "."))

(defvar self*)

(def current-object ()
  (or (self*) site*))

(def @ (prop (o fail))
  (let x (aand (self*) (it prop))
    (if (~null x) x
        site*     (site* prop fail)
                  fail)))

(defset @ (prop (o fail))
  (w/uniq p
    (list (list p prop)
          `(@ ,prop ,fail)
          `(fn (val) (set-prop ,p val)))))

(def set-prop (prop value)
  (= ((current-object) prop) value))

(mac with-object (x . body)
  (w/uniq v
    `(whenlet ,v (as-object ,x)
       (w/param self* ,v
         ,@body))))

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

(mac each-object (lst . body)
  (w/uniq v
    `(each ,v ,lst
       (with-object ,v
         ,@body))))

(def imsize (img)
  (if (valid-url img)
      (fromstring (GET img :bytes)
        (imsize "-"))
      (map int (tokens (shell 'identify '-format "%w %h" img)))))

(def imwidth (img)
  (car (imsize img)))

(def imheight (img)
  (cadr (imsize img)))


(def clean-name (name)
  (def prev nil)
  (aand (each c (downcase name)
          (if (~alphadig c) (= c #\-))
          (if (or (isnt prev c)
                  (isnt prev #\-))
              (out c))
          (= prev c))
        (trim (str it) 'both #\-)))

(def render-image-name ()
  (defs name (clean-name (or @!title (cat @!id)))
        n    (++ (@ 'counter 0)))
  (ero (cat name "-" n ".png") 'image-name))


;; A "rim" (rendered image) is what RENDER and FUSE return.
;; Fields: type='rim, path, width, height, destination, alt, hotspots.
;; hotspots is a list of (x y w h url) for image-map areas.
(def make-rim (path (o :destination) (o :alt) (o :hotspots))
  (obj type:        'rim
       path:        path
       width:       (imwidth path)
       height:      (imheight path)
       destination: destination
       alt:         alt
       hotspots:    (or hotspots nil)))

(= unique-id* 0)
(def unique-id ()
  (cat (++ unique-id*)))

;; ----

(mac AND args
  `(and ,@args))

(mac BODY ( ; the color used for the background of the page. Usually
            ; set to @!background-color.
            :background-color

            ; the image used for the background of the page. When
            ; specified, this must be the result of a RENDER or FUSE
            ; operator.
            :background-image

            ; the default color of any text on the page. Usually set
            ; to @!text-color.
            :text-color

            ; the color for the regular hyperlinks on the page.
            ; Usually set to @!link-color.
            :link-color

            ; the color used for already visited hyperlinks. Usually
            ; set to @!visited-link-color.
            :visited-link-color

            ; the distance, in pixels, of the page body from the top
            ; side of the browser window. Set it to 0 if you want your
            ; page to begin right at the top of the browser window.
            :topmargin

            ; the distance, in pixels, of the page body from the left
            ; side of the browser window. Set it to 0 if you want your
            ; page to begin at the left side of the browser.
            :leftmargin

            ; the size of the bottom margin of the page in pixels.
            :marginheight

            ; the size of the right margin of the page in pixels.
            :marginwidth

            ; optional, a css class name.
            :class
            
            ; optional, a css ID.
            :id

            ; optional, a valid css style definition.
            :style

            ; optional title parameter. The title parameter is not
            ; displayed on the page, but may be read by search engine
            ; crawlers or web page readers.
            :title

            . body)

  `(tag body
     bgcolor: ,background-color
     text: ,text-color
     link: ,link-color
     vlink: ,visited-link-color

     class: ,class
     id: ,id
     style: ,style

     ,@body))

(def CALL (f :kws . body)
  (kwapply f kws body))

(mac CENTER body
  `(tag center ,@body))

(def ELEMENT (:position :sequence)
  (sequence position))

(def ELEMENTS (:sequence first: a last: b)
  (if a (zap max a 0))
  (if b (zap max b 0))
  (cut sequence (or a 0) b))

(mac EQUALS (:value1 :value2)
  `(is ,(assert value1)
       ,(assert value2)))

;; FOR-EACH takes a variable and a sequence. It then assigns each
;; element of the sequence to the variable, one after the other, and
;; for each element, it evaluates the expression pasted within its
;; body. FOR-EACH returns a sequence consisting of the values
;; returned by the last expression during each iteration.
(mac FOR-EACH (:var :variable :sequence . body)
  `(each ,(or var variable) ,sequence
     ,@body))

;; FOR-EACH-BUT is very similar to FOR-EACH. It takes a variable, a
;; sequence, and a last expression. It evaluates the expression or
;; expressions pasted within for every element of the sequence, but—and
;; here is where it differs from FOR-EACH – it also evaluates last
;; expression for each element except for the last one.
(mac FOR-EACH-BUT (:var :variable :sequence :last . body)
  (letu (s n i)
    `(withs (,s ,sequence ,n (len ,s) ,i 0)
       (FOR-EACH var: ,var variable: ,variable sequence: ,s
         ,@body
         (++ ,i)
         (if (< ,i ,n) ,last)))))

;; FOR-EACH-OBJECT takes a single argument, a list of objects or IDs,
;; such as the Contents property of a page. It then walks through and
;; changes context to each element of the list, so every expression
;; pasted within the FOR-EACH-OBJECT block will be evaluated in the
;; context of that element.
(mac FOR-EACH-OBJECT (:var :variable :sequence . body)
  (let v (or var variable)
    `(FOR-EACH variable: ,v
               sequence: ,sequence
       (WITH-OBJECT ,v
         ,@body))))

;; Returns a number indicating how wide a font is relative to He
;; lvetica Bold. The FONT-WIDTH operator works with Yahoo! Store®’s
;; graphical fonts only. Those fonts can be selected from a list. An
;; example is Display-font. According to FONT-WIDTH, Lithos-Bold, for
;; example, is 1.3068392 times wider than Helvetica Bold.
(def FONT-WIDTH (font)
  (err 'todo-font-width))

;; Glues images pasted within its body into a single image, arranged either
;; vertically or horizontally. All child images must be passed through RENDER   
;; or another FUSE. Produces an image map if any child is a RENDER with a       
;; destination, enabling "hot spot" links (e.g. the standard nav bar). The      
;; result is typically passed to IMAGE for display. Has 10 parameters:          
;;                                                                              
;;   axis: :vertical or :horizontal. Direction in which pieces are joined.      
;;     May be omitted if fewer than two images are pasted inside; in that       
;;     case FUSE degenerates to a RENDER.                                       
;;                                                                              
;;   background-color: color for the resultant image. Must be a color           
;;     variable, the result of COLOR, or the constant transparent.              
;;                                                                              
;;   top-margin, bottom-margin, left-margin, right-margin: how far inside       
;;     the resultant image the individual pieces are aligned.                   
;;                                                                              
;;   spacing: distance in pixels between individual images.                     
;;                                                                              
;;   destination: currently ignored. To hyperlink the fused image, wrap
;;     it in a WITH-LINK block instead.                                         
;;
;;   align: :left, :right, or :center. How individual pieces are aligned.       
;;     Defaults to :vertical; if omitted and axis is :vertical, all images      
;;     are stretched to the width of the widest.                                
;;                                                                              
;;   thickness: if non-nil, draws a raised border around the entire image,      
;;     but only if background-color is a color other than transparent.          
;;                                                                              
;; Example:                                                                     
;;   (IMAGE source (FUSE axis :vertical                                           
;;                       (RENDER image: @name-image)
;;                       (RENDER text: @title)))
;;                                                                              
;; See also: RENDER, IMAGE

(mac FUSE (:axis :background-color :top-margin :bottom-margin :left-margin :right-margin :spacing :destination :align :thickness . body)
  `(fuse* axis:             ,axis
          background-color: ,background-color
          top-margin:       ,top-margin
          bottom-margin:    ,bottom-margin
          left-margin:      ,left-margin
          right-margin:     ,right-margin
          spacing:          ,spacing
          destination:      ,destination
          align:            ,align
          thickness:        ,thickness
          children:         (rem nil (list ,@body))))

;; fuse* is the runtime function called by the FUSE macro.
;; children is a list of rim objects (results of RENDER or nested FUSE).
(def fuse* (:axis :background-color :top-margin :bottom-margin
            :left-margin :right-margin :spacing :destination
            :align :thickness :children)
  (or= top-margin 0 bottom-margin 0 left-margin 0 right-margin 0 spacing 0)
  (if (no children)
      nil
      (is (len children) 1)
      (car children)
      (withs (horiz    (is axis 'horizontal)
              n        (len children)
              sp       spacing
              bg       (render-color (or background-color 'none))
              ;; Total canvas size
              cw       (if horiz
                           (+ (apply + (map !width children))
                              (* sp (- n 1)) left-margin right-margin)
                           (+ (apply max (map !width children))
                              left-margin right-margin))
              ch       (if horiz
                           (+ (apply max (map !height children))
                              top-margin bottom-margin)
                           (+ (apply + (map !height children))
                              (* sp (- n 1)) top-margin bottom-margin))
              img      (render-image-name)
              hotspots nil
              cx       left-margin
              cy       top-margin)
        ;; Create blank canvas
        (shell 'magick '-size (cat cw "x" ch) (cat "xc:" bg) img)
        ;; Composite each child onto canvas, tracking hotspot positions
        (each child children
          (when child!destination
            (push (list cx cy child!width child!height child!destination) hotspots))
          (when child!hotspots
            (each hs child!hotspots
              (push (list (+ cx (hs 0)) (+ cy (hs 1)) (hs 2) (hs 3) (hs 4)) hotspots)))
          (shell 'magick img child!path
                 '-geometry (cat "+" cx "+" cy)
                 '-composite img)
          (if horiz
              (zap [+ _ child!width sp] cx)
              (zap [+ _ child!height sp] cy)))
        (when thickness
          (zap [add-frame _ thickness background-color nil] img))
        (make-rim img destination: destination hotspots: (rev hotspots)))))

(mac HEAD body
  `(tag head
     ,@body))

;; HEIGHT returns the height of an image in pixels. The image passed
;; to the HEIGHT operator must be an image that is already rendered,
;; i.e., one that was returned by the RENDER or FUSE operators.
;; Passing a variable of type image (such as the name-image), to
;; HEIGHT returns nil.

(def HEIGHT (img)
  (if (and (isa img 'table) (is img!type 'rim))
      img!height
      (imheight img)))

(mac IF (:test :then :else)
  `(if ,(assert test) ,(assert then) ,else))

;; IMAGE inserts an image into the current page. The source must be the
;; result of a RENDER or FUSE operator — a common mistake is to pass a         
;; variable or property of type image directly (e.g. @name-image). Takes       
;; 10 parameters:                                                              
;;                                                                             
;;   source: result of a RENDER or FUSE operator. Required.                    
;;                                                                             
;;   lowsource: a low-resolution version of the image (also a RENDER or        
;;     FUSE result), typically the same image in grayscale at lower            
;;     resolution or higher compression. The browser renders it first,         
;;     then gradually wipes it away as the full image loads. Only              
;;     noticeable on slow connections.                                         
;;                                                                             
;;   width, height: size of the image in pixels. Lets the browser reserve      
;;     space before the image loads. Does not scale the image — use the        
;;     sizing parameters of RENDER instead.
;;                                                                             
;;   align: :top, :middle, :bottom, :left, or :right. The first three
;;     align the image vertically relative to surrounding text. :left and      
;;     :right float the image to the side of the page; surrounding text        
;;     wraps around it.                                                        
;;                                                                             
;;   border: size of the border in pixels. If the image is hyperlinked,        
;;     set to 0 to suppress the default border. When not hyperlinked,          
;;     omitting border may also cause no border to appear.                     
;;                                                                             
;;   hspace, vspace: horizontal and vertical spacing around the image in       
;;     pixels. Useful for adding breathing room so surrounding text does       
;;     not flush against the image.                                            
;;
;;   alt: textual representation of the image. Used in three ways:             
;;     displayed in place of the image in non-graphical browsers or when       
;;     image loading is off; shown as a tooltip on mouseover; and indexed      
;;     by search engines.                                                      
;;                                                                             
;;   antialias-color: color used to anti-alias the image, blurring jagged      
;;     edges to create a more continuous border. Best results when it          
;;     matches the background behind the image. Most noticeable when using     
;;     RENDER to create text images.                                           
;;                                                                             
;; Example:                                                                    
;;   IMAGE source RENDER image @image                                          
;;                                                                             
;; See also: RENDER, FUSE

(def IMAGE (:source :lowsource :width :height :align :border
            :hspace :vspace :alt :antialias-color)
  (when source
    (assert (is source!type 'rim) "IMAGE source must be a RENDER or FUSE result")
    (withs (path   source!path
            w      (or width  source!width)
            h      (or height source!height)
            alt-s  (or alt source!alt "")
            spots  source!hotspots
            dest   source!destination
            bdr    (or border 0))
      (if (and spots (~empty spots))
          ;; Image-map case: emit <map> then <img usemap="...">
          (let map-id (cat "map-" (unique-id))
            (tag map name: map-id
              (each (sx sy sw sh url) spots
                (gentag area shape 'rect
                        coords (cat sx "," sy "," (+ sx sw) "," (+ sy sh))
                        href url alt "")))
            (gentag img src path width w height h
                    usemap (cat "#" map-id)
                    border bdr
                    hspace (or hspace 0) vspace (or vspace 0)
                    alt alt-s))
          ;; Plain image, optionally wrapped in a link
          (let img-tag (tostring:gentag img src path width w height h
                                        align align border bdr
                                        hspace (or hspace 0) vspace (or vspace 0)
                                        alt alt-s)
            (aif dest
                 (link img-tag it)
                 (pr img-tag)))))))

(def META (:name :content)
  (err 'todo-META)) ; todo

(mac NOT (x)
  `(no ,x))

(mac OR args
  `(or ,@args))

;; RENDER creates an image from an image property, renders text as an image,
;; or both. Does not display the image itself — pass the result to IMAGE as
;; its source value. Has 20 parameters:
;;
;;   image: a property or variable of type image (e.g. @name-image or an
;;     object's image property). When specified, RENDER generates an image
;;     from that variable.
;;
;;   text: when specified, renders the text as an image. If both image and
;;     text are given, RENDER superimposes text over image, useful for
;;     generating uniform buttons.
;;
;;   text-align: :center, :left, or :right. Alignment of the text.
;;
;;   background-color: color for the image. Has no visible effect if image
;;     is specified. Must be a color variable, result of COLOR, or
;;     transparent.
;;
;;   font: font for rendering text. Must be a property or variable of type
;;     font (button-font or the name of one of Yahoo! Store's graphical
;;     fonts), in the format .font-name (e.g. .xsica). Note the colon
;;     before the period after the font name.
;;
;;   font-size: size of the font to use when rendering text as an image.
;;
;;   destination: a URL. When specified, the image will be hyperlinked to
;;     this URL. Can be entered as a string or obtained from TO or ACTION.
;;
;;   top-margin, bottom-margin, left-margin, right-margin: margins in
;;     pixels. Default 0.
;;
;;   max-height, min-height, max-width, min-width: used to resize the
;;     image. If neither is specified, image is rendered at original size.
;;     Resampling changes pixel count to match desired display size; a
;;     resampled image will appear "smooth".
;;
;;   thickness: if non-nil, draws a raised border, but only if thickness
;;     is specified and background-color is other than transparent. Causes
;;     RENDER to create a button image out of text.
;;
;;   intaglio: when set, causes text to have a "chiseled" or "incised"
;;     appearance.
;;
;;   crop: :off, :right, or :center. If max-width is smaller than the
;;     rendered text, determines how the text should be cropped.
;;
;;   expand: when true, the image is clickable and hyperlinked to the
;;     full-size version of the image.
;;
;; Example — uniform buttons using a blank button image as background:
;;   WITH-OBJECT :index
;;     FOR-EACH-OBJECT @contents
;;       WITH-LINK TO id
;;         IMAGE source RENDER image @blank-button
;;                             text @name
;;                             text-align :center
;;                             max-width 150
;;               alt @name
;;       LINEBREAK
;;
;; See also: FUSE, IMAGE

(def RENDER (:image :text :text-color :text-align :background-color
             :font :font-size :destination :alt
             :top-margin :bottom-margin :left-margin :right-margin
             :max-height :min-height :max-width :min-width
             :thickness :intaglio :crop :expand)
  (or= text-align 'left background-color 'none font 'verdana font-size 18
       top-margin 0 bottom-margin 0 left-margin 0 right-margin 0)
  (let src
    (if (and image text)
        ;; Both: superimpose text over image
        (render-text-over image text :font :font-size :text-align :text-color)
        image
        ;; Image only: copy/resize source
        (render-image-src image :max-width :max-height :min-width :min-height)
        text
        ;; Fixed-size button: render text directly onto a canvas of the target size.
        ;; This avoids resize distortion and lets left-margin act as an x-offset.
        (if (and min-width (is min-width max-width) min-height (is min-height max-height))
            (let fixed (render-text-on-canvas text min-width min-height
                                              text-color:  (or text-color black)
                                              font:        font
                                              font-size:   font-size
                                              text-align:  text-align
                                              left-margin: left-margin)
              ;; Canvas is already the right size; skip resize and horizontal margins
              (= min-width nil max-width nil min-height nil max-height nil left-margin 0)
              fixed)
            ;; Variable width: render transparently and trim
            (render-text text
                         font: font font-size: font-size
                         text-color: (or text-color black)
                         gravity: (case text-align
                                    left 'west center 'center right 'east 'west)))
        (err "RENDER requires image: or text:"))
    ;; intaglio: apply drop shadow while background is still transparent
    (when (and text intaglio)
      (zap apply-shadow src))
    ;; Size constraints for variable-width text (fixed-size already handled above)
    (when (and (no image) (or max-width max-height min-width min-height))
      (zap [resize-rim _ max-width max-height min-width min-height] src))
    ;; Merge transparent text onto solid background
    (when (and text (~is background-color 'none))
      (zap [add-background _ background-color] src))
    ;; Margins
    (when (or (> top-margin 0) (> bottom-margin 0)
              (> left-margin 0) (> right-margin 0))
      (zap [add-margins _ top-margin bottom-margin left-margin right-margin
                          background-color] src))
    ;; Raised/sunken 3D border (button effect)
    (when thickness
      (zap [add-frame _ thickness background-color intaglio] src))
    (make-rim src destination: destination alt: (or alt text))))

;; Copy or resize a source image (URL or local path) to a fresh output file.
(def render-image-src (src :max-width :max-height :min-width :min-height)
  (with img (render-image-name)
    (if (valid-url src)
        (shell 'curl '-sL src '-o img)
        (shell 'cp src img))
    (when (or max-width max-height min-width min-height)
      (zap [resize-rim _ max-width max-height min-width min-height] img))))

;; Superimpose text over a background image via ImageMagick -annotate.
(def render-text-over (base-src text :font :font-size :text-align :text-color)
  (let base (render-image-src base-src)
    (with img (render-image-name)
      (shell 'magick base
             '-font      (find-font (or font 'verdana))
             '-pointsize (or font-size 18)
             '-fill      (render-color (or text-color black))
             '-gravity   (case text-align
                           left 'west center 'center right 'east 'center)
             '-annotate "0" text
             img))))

;; Resize to fit within max/min constraints using ImageMagick geometry.
(def resize-rim (src max-w max-h min-w min-h)
  (with img (render-image-name)
    (let geom (cat (or max-w "") "x" (or max-h "")
                   (if (or min-w min-h) "^" ">"))
      (shell 'magick src '-resize geom img))))

;; Add margin padding around an image using -splice and -extent.
(def add-margins (src top bot left right bgcolor)
  (with img (render-image-name)
    (let bg (render-color (or bgcolor 'none))
      ;; Add top and left margins via splice
      (shell 'magick src
             '-background bg '-gravity 'none
             '-splice (cat left "x" top "+0+0")
             img)
      ;; Extend canvas to add bottom and right margins
      (withs (w (imwidth img) h (imheight img))
        (shell 'magick img '-background bg
               '-extent (cat (+ w right) "x" (+ h bot))
               img)))))

;; Render text onto a fixed-size transparent canvas at an explicit x offset.
;; Used by RENDER when min-width=max-width and min-height=max-height (button case).
(def render-text-on-canvas (text w h :text-color :font :font-size :text-align :left-margin)
  (with img (render-image-name)
    (shell 'magick
           '-size (cat w "x" h) '-background 'none "xc:"
           '-gravity  (case text-align left 'west center 'center right 'east 'west)
           '-font     (find-font (or font 'verdana))
           '-fill     (render-color (or text-color black))
           '-pointsize (or font-size 18)
           '-kerning  0.0
           '-draw     (cat "text " (or left-margin 0) ",-1 " (tostring:write text))
           img)))

;; Apply the imbutton-style drop shadow to a transparent-background text image.
;; Shadow is black, 60% opacity, offset -1,-1 (upper-left), giving an embossed look.
(def apply-shadow (src)
  (with img (render-image-name)
    (shell 'magick src
           "(" '+clone '-background 'black '-shadow "60x0-1-1" ")"
           '+swap
           '-background 'none
           '-layers 'merge
           img)))

;; Flatten a transparent-background image onto a solid colored canvas.
(def add-background (src bgcolor)
  (with img (render-image-name)
    (shell 'magick src '-background (render-color bgcolor) '-flatten img)))

;; Add a 3D frame around an image.
(def add-frame (src thickness bgcolor intaglio)
  (with img (render-image-name)
    (let h (or thickness 2)
      (shell 'magick src
             '-mattecolor (render-color (color 0x99 0x99 0x99))
             '-frame (cat h "x" h "+" h "+" 0)
             ;'-shade (cat (+ 45 90 90 90) "x" (+ 45 0))
             img))))

(def TEXT (text)
  (if text (pr text)))

(def TITLE (name)
  (tag title
    (TEXT name)))

(mac WHEN (cond . body)
  `(when ,cond ,@body))

;; WIDTH returns the width of an image in pixels. The image passed
;; to the WIDTH operator must be an image that is already rendered,
;; i.e., one that was returned by the RENDER or FUSE operators.
;; Passing a variable of type image (such as the name-image) to
;; WIDTH returns nil.
(def WIDTH (img)
  (if (and (isa img 'table) (is img!type 'rim))
      img!width
      (imwidth img)))

(mac WITH= (:var :variable :value . body)
  `(let ,(or var variable) ,value
     ,@body))

(mac WITH-OBJECT (id . body)
  `(with-object ,id
     ,@body))

