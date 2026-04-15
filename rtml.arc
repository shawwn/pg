(require (libpath "strings.arc")) ; for tokens
(require (libpath "app.arc")) ; for paras
(require (libpath "html.arc"))

(def transparent (obj r: 0 g: 0 b: 0 a: 0))

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
      (map int (tokens (SHELL 'identify '-format "%w %h" img)))))

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

(def SHELL args
  (ero `(SHELL ,@args))
  (apply shell args))


;;;
;;;
;;; Logical operators
;;;
;;;

;; AND takes one or more arguments. Each argument can be any valid RTML
;; expression pasted within the body of AND. The operator then evaluates each of
;; those expressions, and if all are true (none are nil), it returns the value of
;; the last expression.
;;
;; Consider the following template segment:
;;
;;   (IF test: (AND @!taxable @!orderable)
;;       then: (TEXT "This item is orderable and taxable.")
;;       else: (TEXT "This item is either not orderable or not taxable."))
;;
;; This example prints, "This item is orderable and taxable." if both the
;; orderable and taxable properties of the current page are set to "Yes."
;; If any of the expressions within the AND block are nil (false), then the rest
;; of the expressions are ignored.

(mac AND args
  `(and ,@args))

;; OR takes one or more arguments (pasted within it)—each being a valid
;; RTML expression—and returns the value of the first one that is other than nil.
;; Once it finds an expression whose value is other than nil, the rest of the
;; expressions are ignored.
;;
;; If either the orderable or the taxable (or both) properties of the current page
;; is set to "Yes," the following example will print, "This item is orderable or
;; taxable."
;;
;;   (IF test: (OR
;;               @!taxable
;;               @!orderable)
;;       then: (TEXT "This item is orderable or taxable.")
;;       else: (TEXT "This item is neither orderable nor taxable."))
;;
;; The fact that the first non-nil value of the OR operator is returned and the
;; rest ignored is important. Based on this fact, we can write expressions such as:
;;
;;   (WITH= variable: price
;;          value: (OR @!sale-price @!price))
;;
;; In this example, the local variable price will be set to the value of the
;; Sale-price property, if Sale-price is not empty, otherwise, to the value of
;; the Price property. Notice, that the above example is NOT equivalent to:
;;
;;   (WITH= variable: price
;;          value: (OR @!price @!sale-price))
;;
;; Here, OR returns the value of its first non-nil expression. This example will
;; almost always set the local variable price to the value of the regular price of
;; the current item (unless you forgot to enter the regular price but not the sale
;; price).

(mac OR args
  `(or ,@args))

;; This operator returns the logical opposite of its argument. It takes a single
;; RTML expression as its argument. If the expression returns nil, NOT returns true
;; (the logical opposite of its argument). If the expression returns a value other
;; than nil, NOT returns false.

(def NOT (x)
  (no x))



;;;
;;;
;;; Control structures
;;;
;;;

(mac MULTI exprs
  `(do ,@exprs))

(mac IF (:test :then :else)
  `(if ,(assert test)
       ,(assert then)
       ,else))

;; The WHEN operator is similar to the IF operator: it is basically "one half" of
;; the IF operator. It says, "evaluate the following if this expression is true
;; (not nil)" and is equivalent to the following IF block:
;;
;;   (IF test: <some expression>
;;       then: <some other expression>
;;       else: nil)
;;
;; WHEN has a single argument, a condition. If the result of the condition is true
;; (not nil) then the expression or expressions pasted within the WHEN block
;; is/are evaluated. Similar to the IF operator arguments, the WHEN operator's
;; condition argument can only be a single operator. However, the WHEN operator
;; may contain any simple or complex expression. (Use the CALL or MULTI operators
;; to enter a more complex expression.)
;;
;; For an example on how to use the WHEN operator, consider the following
;; template snippet:
;;
;;   (WHEN @!sale-price
;;     (TEXT "This item is on sale."))
;;
;; If the current page has a sale price entered, the code above will print: "This
;; item is on sale." Otherwise, it will do nothing.

(mac WHEN (cond . body)
  `(when ,cond
     ,@body))

;; The SWITCH operator is very much like a multi-state switch (hence the name).
;; It takes one argument, the switch expression, and one or more key-expression
;; pairs. It then compares the result of the switch expression to each key. If
;; there is a match, the corresponding expression is evaluated and its value
;; returned. If there is no match, SWITCH returns nil. It is much easier to
;; understand how SWITCH works by looking at an example:
;;
;;   (SWITCH @!page-format
;;     :top-buttons  (TEXT "You have top buttons.")
;;     :side-buttons (TEXT "You have side buttons."))
;;
;; In this example, the switch expression is the global variable @!page-format.
;; There are two key-expression pairs, one starting with :top-buttons, the other,
;; with :side-buttons. The operator will compare the value of @!page-format first
;; to the constant :top-buttons (because @!page-format is a variable that has
;; values from a drop-down list, we must use the constant notation for those
;; values). If our Page-format variable is set to "Top-buttons" then this template
;; segment will print "You have top buttons." If, on the other hand, our
;; Page-format variable is set to "Side-buttons", then the template will print
;; "You have side buttons."

(mac SWITCH (expr . body)
  `(SWITCH-let ,(uvar) ,expr ,@body))

(mac SWITCH-let (var expr . args)
  (def ex (args)
    (if (no (cdr args))
        (car args)
        `(if (is ,var ,(car args))
             ,(cadr args)
             ,(ex (cddr args)))))
  `(let ,var ,expr ,(ex args)))



;;;
;;;
;;; Sequences
;;;
;;;

;; The ELEMENTS operator returns a subsequence of a sequence. It takes three
;; parameters: a sequence, a start index, and an end index. It returns a
;; subsequence consisting of the elements of sequence from start index to end
;; index. The following example, for instance, will print "bcd":
;;
;;   (TEXT (ELEMENTS sequence: "abcde" first: 1 last: 3))
;;
;; From this example, you can see that the numbering of sequence elements is
;; zero-based, so the letter "a" in the sequence "abcde" is at position 0.
;; The second and third parameters of ELEMENTS are optional. If first is omitted,
;; it is assumed to be 0 (meaning the start of the sequence). If last is omitted,
;; it is assumed to be the last element of the sequence. Consequently, if both
;; first and last are omitted, then the ELEMENTS operator simply returns the
;; sequence itself.

(def ELEMENTS (:sequence first: a last: b)
  (if a (zap max a 0))
  (if b (++ b))
  (if b (zap max b 0))
  (cut sequence (or a 0) b))

;; ELEMENT takes two parameters, a position and a sequence. It then returns
;; the element of sequence sequence at position position. As in the case of the
;; ELEMENTS operator, the numbering of sequence elements starts at zero. If
;; sequence sequence has no element at position position, ELEMENT returns nil.
;; The following example will print the first letter of every special in your
;; store:
;;
;;   (WITH-OBJECT 'index
;;     (FOR-EACH-OBJECT @!specials
;;       (TEXT (ELEMENT position: 0 sequence: @!name))
;;       (LINEBREAK)))

(def ELEMENT (:position :sequence)
  (sequence position))

;; If you use the ELEMENT or ELEMENTS operator, you might need to find out how
;; many elements there are in a sequence. You can do that with the LENGTH
;; operator. The length operator takes a single parameter, a sequence, and
;; returns the number of elements within the sequence. If the sequence is empty,
;; LENGTH returns 0.
;;
;; Remember, that all operators that work on sequences and refer to particular
;; positions within a sequence (such as the ELEMENT or ELEMENTS operator)
;; consider element 0 to be the first element within a sequence. Therefore, when
;; you use these operators in conjunction with the LENGTH operator, you should
;; know that the last element of a sequence is at the position returned by LENGTH
;; minus one.
;;
;; To demonstrate this, consider the following example:
;;
;;   (WITH-OBJECT 'index
;;     (FOR-EACH-OBJECT @!specials
;;       (WITH= variable: len
;;              value: (LENGTH @!name))
;;       (TEXT (ELEMENT position: (- len 1) sequence: @!name))
;;       (LINEBREAK)))
;;
;; This example is very similar to last one where we printed the first letter of
;; each special item, except in this case; we are printing the last element of
;; every special item. For each special, we first store the length of the
;; special's name in the local variable len, and then we print the element of
;; the name at position len - 1.

(def LENGTH (sequence)
  (len sequence))

;; Returns true if a text string contains at least one non-whitespace character.
;; Whitespace characters include the space character, the tab character, and a new
;; line (carriage return) character. NONEMPTY takes a single argument, a text
;; string. This operator should only be used to test whether a text string is
;; empty or not. Do not use it with any other sequence.
(def NONEMPTY (str)
  (unless (is str nil)
    (assert (isa!string str) "NONEMPTY expected a string")
    (any str nonwhite)))

;; PARAGRAPHS takes a text string and returns a sequence in which each element
;; is a paragraph of the text string.

(def PARAGRAPHS (s)
  (paras s))

(def LINES (text)
  (lines text))

;; POSITION takes two arguments: an element and a sequence. It returns the
;; position at which the sequence contains the specified element or nil, if the
;; element was not found in the sequence. The numbering of the elements within a
;; sequence starts at position 0.
;; This operator is most commonly used to check if an element exists within a
;; sequence. The following example demonstrates this use.
;;
;; Example:
;;
;;   (WHEN (POSITION element: 'contents sequence: @!nav-buttons)
;;     (TEXT "Contents are part of Nav-buttons."))
;;
;; See also: ELEMENT, ELEMENTS

(def POSITION (:element :sequence)
  (pos element sequence))

;; The SEGMENTS operator takes a sequence, and returns successive segments of a
;; specified length of that sequence. Consider the built-in template
;; Pack-contents. This template is used to show the contents of a page if
;; contents-format is set to Pack. For your reference, the template is included
;; below:
;;
;;   (def pack-contents. (ids)
;;     (FOR-EACH variable: tuple
;;               sequence: (SEGMENTS length: @!columns sequence: ids)
;;       (FOR-EACH-OBJECT tuple
;;         (WITH-LINK TO: id
;;           (IMAGE source: (RENDER image: (CALL :shown-image)) alt: @!name))
;;         (LINEBREAK))))
;;
;; This template takes a sequence of IDs, those included in the contents property
;; of a page. Visualize how contents are rendered on the page when contents
;; format is set to Pack and you will understand how SEGMENTS is used. Contents
;; are generated on the page arranged into a number of columns as set by the
;; columns global variable. If the columns global variable is set to 3, for
;; instance, and a page has, say, 6 items in its contents property, then the
;; first row will contain the images (or icons) of the first three items, and the
;; second row will contain the second three items. Basically, what you need to do
;; is to break apart the contents property into subsets of three—each subset
;; being a sequence. This is exactly what is happening in the Pack-contents
;; template.
;;
;; The first FOR-EACH operator (the "outer loop") takes each subset of the
;; contents. These subsets are returned from the SEGMENTS operator. The SEGMENTS
;; operator takes the IDs (from the contents property) and returns subsets of the
;; IDs each having a length specified by the columns global variable.
;;
;; The second FOR-EACH operator—the "inner loop"—walks through each of these
;; segments, and displays the image for each object pointed to by the IDs
;; contained within the segments.
;;
;; If the last segment doesn't contain enough elements (fewer than the number
;; specified by the length parameter), it will simply contain however many
;; elements are left from the original sequence. For example, if the sequence has
;; seven elements and we want subsets of three elements each, SEGMENTS will return
;; three subsequences: the first two containing three elements each, while the
;; last sub-sequence, only one element.

(def SEGMENTS (:length :sequence)
  (assert (isa!int length)
          "SEGMENTS expected :length to be an integer")
  (assert (> length 0)
          "SEGMENTS expected :length to be greater than zero")
  (tuples sequence length))


;; The TOKENS operator takes a text string and turns it into a sequence in which
;; each element is a "token" from the original string. Tokens are either single
;; words or phrases enclosed in double quotes and separated by spaces from one
;; another. The following example will print each word of the sentence "This is
;; how TOKENS works" on a new line:
;;
;;   (FOR-EACH variable: word
;;             sequence: (TOKENS "This is how TOKENS works")
;;     (TEXT word)
;;     (LINEBREAK))
;;

(def TOKENS (str)
  (tokens str))

;; YANK has two parameters: a sequence and an element. It returns the same
;; sequence but with all occurrences of the given element removed. Below is a
;; modified version of the template we used to demonstrate the use of the TOKENS
;; operator. This modified version prints each word of the sentence "This is how
;; TOKENS work" but with the word TOKENS removed.
;;
;;   (FOR-EACH variable: 'word
;;             sequence: (YANK element: "TOKENS"
;;                             sequence: (TOKENS "This is how TOKENS works"))
;;     (TEXT word)
;;     (LINEBREAK))

(def YANK (:element :sequence)
  (assert (alist sequence)
          "YANK expected :sequence to be a list")
  (rem element sequence))

;; The REVERSE operator takes a sequence as its argument and returns
;; the same sequence in reverse order.

(def REVERSE (seq)
  (assert (alist seq)
          "REVERSE expected argument to be a list")
  (rev seq))

;; This operator returns a sequence consisting of the IDs of all objects (pages)
;; in the store in alphabetical order. Right now, only the built-in Index-body.
;; template uses this operator to generate the index page of your store (in
;; Editor V3.0, the sitemap. template does the same.) The WHOLE-CONTENTS operator
;; is very useful and is the only way to obtain a list of all the pages in your
;; store.

(def WHOLE-CONTENTS ()
  (err 'todo-WHOLE-CONTENTS))

;; MAKE-LIST takes any number of values pasted within it and returns a
;; sequence consisting of all those values.

(def MAKE-LIST args
  args)

;; APPEND takes any number of sequences (except text strings) pasted
;; within it and returns a new sequence by joining all the sequences
;; together.

(def APPEND args
  (accum a
    (each xs args
      (assert (alist xs)
              "APPEND expected each argument to be a list")
      (if xs
          (each y xs
            (a y))
          (a nil)))))



;;;
;;;
;;; working with images
;;;
;;;

;; HEIGHT returns the height of an image in pixels. The image passed
;; to the HEIGHT operator must be an image that is already rendered,
;; i.e., one that was returned by the RENDER or FUSE operators.
;; Passing a variable of type image (such as the name-image), to
;; HEIGHT returns nil.

(def HEIGHT (img)
  (if (and (isa!table img) (is img!type 'rim))
      img!height
      (imheight img)))

;; WIDTH returns the width of an image in pixels. The image passed
;; to the WIDTH operator must be an image that is already rendered,
;; i.e., one that was returned by the RENDER or FUSE operators.
;; Passing a variable of type image (such as the name-image) to
;; WIDTH returns nil.

(def WIDTH (img)
  (if (and (isa!table img) (is img!type 'rim))
      img!width
      (imwidth img)))



;;;
;;;
;;; working with colors
;;;
;;;

(def RED (col)
  (assert (and (isa!table col) col!r))
  col!r)

(def GREEN (col)
  (assert (and (isa!table col) col!g))
  col!g)

(def BLUE (col)
  (assert (and (isa!table col) col!b))
  col!b)

(def GRAYSCALE (col)
  ;; average of R G B
  (int (/ (+ (RED col)
             (GREEN col)
             (BLUE col))
          3)))


;;;
;;;
;;; RTML operators
;;;
;;;

;; Inserts a line break into the current page. LINEBREAK has two
;; optional arguments: number and clear. When number is specified, it
;; will cause that many number of line breaks inserted into the
;; current document. The clear parameter can take one of the following
;; values: :none (this is the default,) :left, :right, or :all. When
;; specified, this parameter controls the flow of text around floating
;; objects. Floating objects are typically tables or images whose
;; align property is set.
(def LINEBREAK ((o :number 1) (o :clear 'none))
  (assert (in clear 'none 'left 'right 'all)
          "LINEBREAK argument :clear should be 'none 'left 'right or 'all")
  (while (> number 0)
    (if (is clear 'none)
        (TEXT "<br>")
        (do (TEXT "<br clear=\"")
            (TEXT clear)
            (TEXT "\" />")))
    (-- number)))

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
  ;(ero `(CALL ,f ,@kws ,@body))
  (kwapply f kws body))

(mac CENTER body
  `(tag center ,@body))

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

(mac FONT (:size :color :face :class :id :style :title . body)
  `(tag font size: ,size
             color: ,color
             face: ,face
             class: ,class
             id: ,id
             style: ,style
             title: ,title
     ,@body))

;; Returns a number indicating how wide a font is relative to He
;; lvetica Bold. The FONT-WIDTH operator works with Yahoo! Store®’s
;; graphical fonts only. Those fonts can be selected from a list. An
;; example is Display-font. According to FONT-WIDTH, Lithos-Bold, for
;; example, is 1.3068392 times wider than Helvetica Bold.

(defmemo FONT-WIDTH (font)
  (let ref (+ "label:This is a test label."
              " abcdefghijklmnopqrstuvwxyz"
              " ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (withs (base-w (int (SHELL 'magick '-background 'none
                               '-font 'Helvetica-Bold
                               '-pointsize 72 ref
                               '-trim '+repage
                               '-format "%[fx:w]" "info:"))
            font-w (int (SHELL 'magick '-background 'none
                               '-font (find-font font)
                               '-pointsize 72 ref
                               '-trim '+repage
                               '-format "%[fx:w]" "info:")))
      (/ (* 1.0 font-w) base-w))))

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
        (SHELL 'magick '-size (cat cw "x" ch) (cat "xc:" bg) img)
        ;; Composite each child onto canvas, tracking hotspot positions
        (each child children
          (when child!destination
            (push (list cx cy child!width child!height child!destination) hotspots))
          (when child!hotspots
            (each hs child!hotspots
              (push (list (+ cx (hs 0)) (+ cy (hs 1)) (hs 2) (hs 3) (hs 4)) hotspots)))
          (SHELL 'magick img child!path
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
      (if (~empty spots)
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

;; Return the URL of an image stored in the Yahoo! Store system.
;;
;; When an image is uploaded into an image-type property or global variable
;; (e.g. the IMAGE property of an item), it is stored internally by the
;; store system and its exact location is not exposed by the editor.
;; Pass the image value to IMAGE-REF to retrieve its URL.
;;
;; Example:
;;
;;   (WITH-LINK (IMAGE-REF @!image)
;;     (IMAGE source: (RENDER image @!image)
;;            max-height: @!thumb-height
;;            max-width: @!thumb-width))
;;
;; See also: IMAGE, RENDER

(def IMAGE-REF (img)
  (err 'todo-IMAGE-REF))

;; Emit an <IMG> HTML tag. Equivalent to the HTML <IMG> element.
;;
;; Takes seven parameters: class, id, style, title, alt, src, and lowsr,
;; all direct equivalents of the <IMG> tag's attributes. src must be a
;; URL string — image-type properties or variables cannot be used directly.
;; Wrap image variables with IMAGE-REF to obtain their URL first.
;;
;; Example:
;;
;;   ;; Wrong — src cannot be an image-type variable:
;;   (IMG :src @!name-image)
;;
;;   ;; Correct — use IMAGE-REF to get the URL:
;;   (IMG :src (IMAGE-REF @!name-image))
;;
;; See also: IMAGE-REF

(def IMG (:class :id :style :title :alt :src :lowsr)
  (assert (no lowsr))
  (tag img :class :id :style :title :alt :src))

(def META (:name :content)
  (err 'todo-META))

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
  (ero `(RENDER image: ,image text: ,text))
  (if expand (err 'todo-RENDER-expand))
  (if crop (err 'todo-RENDER-crop))
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
                         font: font
                         font-size: font-size
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
  (ero `(render-image-src ,src max-width: ,max-width max-height: ,max-height min-width: ,min-width min-height: ,min-height))
  (with img (render-image-name)
    (if (valid-url src)
        (SHELL 'curl '-sL src '-o img)
        (SHELL 'cp src img))
    (when (or max-width max-height min-width min-height)
      (zap [resize-rim _ max-width max-height min-width min-height] img))))

;; Superimpose text over a background image via ImageMagick -annotate.
(def render-text-over (base-src text :font :font-size :text-align :text-color)
  (ero `(render-text-over ,base-src ,text font: ,font font-size: ,font-size text-align: ,text-align text-color: ,text-color))
  (let base (render-image-src base-src)
    (with img (render-image-name)
      (SHELL 'magick base
             '-font      (find-font (or font 'verdana))
             '-pointsize (or font-size 18)
             '-fill      (render-color (or text-color black))
             '-gravity   (case text-align
                           left 'west center 'center right 'east 'center)
             '-annotate "0" text
             img))))

;; Resize to fit within max/min constraints using ImageMagick geometry.
(def resize-rim (src max-w max-h min-w min-h)
  (ero `(resize-rim ,src ,max-w ,max-h ,min-w ,min-h))
  (with img (render-image-name)
    (let geom (cat (or max-w "") "x" (or max-h "")
                   (if (or min-w min-h) "^" ">"))
      (SHELL 'magick src '-resize geom img))))

;; Add margin padding around an image using -splice and -extent.
(def add-margins (src top bot left right bgcolor)
  (ero `(add-margins ,src ,top ,bot ,left ,right ,bgcolor))
  (with img (render-image-name)
    (let bg (render-color (or bgcolor 'none))
      ;; Add top and left margins via splice
      (SHELL 'magick src
             '-background bg '-gravity 'none
             '-splice (cat left "x" top "+0+0")
             img)
      ;; Extend canvas to add bottom and right margins
      (withs (w (imwidth img) h (imheight img))
        (SHELL 'magick img '-background bg
               '-extent (cat (+ w right) "x" (+ h bot))
               img)))))

;; Render text onto a fixed-size transparent canvas at an explicit x offset.
;; Used by RENDER when min-width=max-width and min-height=max-height (button case).
(def render-text-on-canvas (text w h :text-color :font :font-size :text-align :left-margin)
  (ero `(render-text-on-canvas ,text ,w ,h text-color: ,text-color font: ,font font-size: ,font-size text-align: ,text-align left-margin: ,left-margin))
  (with img (render-image-name)
    (SHELL 'magick
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
  (ero `(apply-shadow ,src))
  (with img (render-image-name)
    (SHELL 'magick src
           "(" '+clone '-background 'black '-shadow "60x0-1-1" ")"
           '+swap
           '-background 'none
           '-layers 'merge
           img)))

;; Flatten a transparent-background image onto a solid colored canvas.
(def add-background (src bgcolor)
  (ero `(add-background ,src ,bgcolor))
  (with img (render-image-name)
    (SHELL 'magick src '-background (render-color bgcolor) '-flatten img)))

;; Add a 3D frame around an image.
(def add-frame (src thickness bgcolor intaglio)
  (ero `(add-frame ,src ,thickness ,bgcolor ,intaglio))
  (with img (render-image-name)
    (let h (if (in thickness nil t)
               2
               (do (assert (isa!int thickness))
                   thickness))
      (SHELL 'magick src
             '-mattecolor (render-color (color 0x99 0x99 0x99))
             '-frame (cat h "x" h "+" h "+" 0)
             ;'-shade (cat (+ 45 90 90 90) "x" (+ 45 0))
             img))))

(mac TABLE (:border :align :cellspacing :cellpadding :units :width :class :id :style :title . body)
  `(tag table border: ,border
              align: ,align
              cellspacing: ,cellspacing
              cellpadding: ,cellpadding
              width: ,width
              class: ,class
              id: ,id
              style: ,style
              title: ,title
     ,@body))

(mac TABLE-ROW (:background-color :align :valign :class :id :style :title . body)
  `(tag tr background-color: ,background-color
           align: ,align
           valign: ,valign
           class: ,class
           id: ,id
           style: ,style
           title: ,title
     ,@body))

(mac TABLE-CELL (:background-color :align :valign :width :colspan :rowspan :class :id :style :title . body)
  `(tag td background-color: ,background-color
           align: ,align
           valign: ,valign
           width: ,width
           colspan: ,colspan
           rowspan: ,rowspan
           class: ,class
           id: ,id
           style: ,style
           title: ,title
     ,@body))

(def TEXT (text)
  (if text (pr text)))

(def TITLE (name)
  (tag title
    (TEXT name)))

(mac WITH= (:var :variable :value . body)
  `(let ,(or var variable) ,value
     ,@body))

(mac WITH-OBJECT (id . body)
  `(with-object ,id
     ,@body))



;;;
;;;
;;; misc
;;;
;;;

(def MAXIMUM args
  (apply max args))

(def MINIMUM args
  (apply min args))

(def TO (x)
  (when (is x id)
    (= x @!id))
  (assert (isa!sym x)
          "TO expected a symbol")
  (cat x ".html"))

(defvar ID* 'index)
(defvar UP* 'index)
(defvar NEXT* 'index)
(defvar PREV* 'index)

;(def ID* ()
;  @!id)

;(def UP* ()
;  @!up)

;(def NEXT* ()
;  (err 'todo-NEXT)
;  @!next)

;(def PREV* ()
;  (err 'todo-PREV)
;  @!prev)
