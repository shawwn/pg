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
  `(do (err 'todo-FUSE) ,@body))

(mac HEAD body
  `(tag head
     ,@body))

;; HEIGHT returns the height of an image in pixels. The image passed
;; to the HEIGHT operator must be an image that is already rendered,
;; i.e., one that was returned by the RENDER or FUSE operators.
;; Passing a variable of type image (such as the name-image), to
;; HEIGHT returns nil.

(def HEIGHT (img)
  (imheight img))

(mac IF (:test :then :else)
  `(if ,(assert test) ,(assert then) ,else))

(def META (:name :content)
  nil) ; todo

(mac NOT (x)
  `(no ,x))

(mac OR args
  `(or ,@args))

(def RENDER (:kws)
  (err 'todo-render))

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
  (imwidth img))

(mac WITH= (:var :variable :value . body)
  `(let ,(or var variable) ,value
     ,@body))

(mac WITH-OBJECT (id . body)
  `(with-object ,id
     ,@body))

