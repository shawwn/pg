(require "rtml.arc")


(def 3d. ()
  (OR (EQUALS value1: @!button-style
              value2: 'solid)
      (EQUALS value1: @!button-style
              value2: 'incised)))

(def apparent-width. (bar)
  (let bwid (WIDTH bar)
    (if (and
          (or
            (no @!button-edge-color)
            (is @!button-edge-color
                @!background-color))
          @!button-edge-width)
        (- bwid (* 2 @!button-edge-width))
        bwid)))

;; This template takes a text string, a text color, a background color, and a width. It returns the given text turned
;; into an image using the specified text parameters.
;;
;; Width is only used if the variable banner-font-size is not set. In that case, the resultant image will use a font
;; size that will make the text large enough to fit the specified width in pixels.

(def banner. (text color bgcolor wid)
  (RENDER text: text
          text-color: color
          background-color: bgcolor
          font: @!home-button-font
          font-size: (or @!banner-font-size
                         (CALL 'banner-font-size.
                           @!home-button-font
                           (LENGTH text)
                           wid))
          top-margin: 0
          bottom-margin: 5
          crop: 'left))

;; Banner-font-size takes a font (a graphical Yahoo! Store font); a number, chars, representing the number of
;; characters in a line of text; and another number, wid. It then tells what font size to use in order to fit the given
;; number of characters within the specified pixels using the specified font.

(def banner-font-size. (font chars wid)
  (WITH= variable: wid
         value: (OR
                  wid
                  370)
    (WITH= variable: base
           value: (IF test: (> chars 15)
                      then: (* 0.98 (* (/ 720 chars) (/ wid 370)))
                      else: (- 50 (* chars 0.8)))
      (WITH= variable: mult
             value: (FONT-WIDTH font)
        (/ base mult)
      )
    )
  )
)

;; Called by group. or item. to create a page with a specific layout.
;; Sets up the HTML head (keywords meta tag, title, extra head tags)            
;; and body styling (background color/image, text color, link color). 

(def base-item. ((o use 'item))
  (HEAD
    (META name: "Keywords"
          content: @!keywords)
    (TITLE @!name)
    (TEXT @!head-tags))
  (BODY background-color: @!background-color
        background-image: @!background-image
        text-color: @!text-color
        link-color: @!link-color
        visited-link-color: @!visited-link-color
    (CENTER
      (WITH= variable: navbut
             value: (CALL 'nav-buttons.
                          @!nav-buttons
                          'horizontal)
        (WITH= variable: wid
               value: (WITH= variable: w
                             value: (CALL 'apparent-width.
                                      navbut)
                        (AND
                          (> w 0)
                          w))
          (WHEN wid
            (IMAGE source: navbut))
          (LINEBREAK number: 3)
          (TABLE border: 0
                 cellspacing: 0
                 cellpadding: 0
                 width: wid
            (TABLE-ROW valign: 'top
              (TABLE-CELL
                (WITH= variable: im
                       value: (AND
                                @!image
                                (RENDER image: @!image
                                        max-height: @!item-height
                                        max-width: @!item-width
                                        expand: t))
                  (WITH= variable: text
                         value: (IF test: (NONEMPTY @!headline)
                                    then: @!headline
                                    else: @!name)
                    (WITH= variable: label
                           value: (CALL 'display-text.
                                    text
                                    'left)
                      (WHEN im
                        (WITH= variable: imwid
                               value: (WIDTH im)
                          (WITH= variable: labwid
                                 value: (WIDTH label)
                            (IF test: (AND
                                        wid
                                        (> (+ imwid labwid 8)
                                           wid))
                                then: (CENTER
                                        (IMAGE source: im)
                                        (LINEBREAK number: 2))
                                else: (WITH= variable: height
                                             value: (HEIGHT im)
                                        (IMAGE source: im
                                               align: 'left)
                                        (SHIM height: (+ height 8)
                                              width: 10
                                              align: 'left))
                            )
                          )
                        )
                      )
                      (IMAGE source: label
                             alt: text)
                    )
                  )
                  (LINEBREAK number: 2)
                  (FONT size: @!text-size
                        face: @!text-font
                    (WHEN @!inset
                      (CALL 'inset-image.
                        @!inset
                        'left))
                    (FOR-EACH var: para
                              sequence: (PARAGRAPHS @!caption)
                      (TEXT para)
                      (LINEBREAK number: 2)
                    )
                    (WHEN im
                      (LINEBREAK clear: 'all)
                    )
                    (WHEN (EQUALS value1: use
                                  value2: 'item)
                      (CALL 'order.
                        (IF test: (AND
                                    (NONEMPTY @!headline)
                                    (NONEMPTY @!code)
                                    (OR
                                      @!price
                                      @!sale-price)
                                  )
                            then: @!name
                            else: nil
                        )
                        nil
                      )
                      (FOR-EACH-OBJECT @!contents
                        (CALL 'group-element.)
                      )
                    )
                  )
                )
              )
            )
          )
          (WHEN (NONEMPTY @!final-text)
            (LINEBREAK)
            (CALL 'paras-in-box.
              @!final-text
              wid)
          )
        )
      )
    )
  )
)

;; If the current page is the search page (the type of the page is :search.) body-switch calls the search-
;; body template, otherwise the page-body template.
;;
;; The first part of this template (from line 1 to 12) is currently disabled. When the variable show-path.
;; is true, these twelve rows would display the “breadcrumbs” trail leading to the current page. However,
;; the show-path. variable is not accessible and because its name ends with a period it cannot be created as
;; a custom variable.

(def body-switch. (wid)
  (MULTI
    (IF test: (@ 'show-path.)
        then: (MULTI
                (TEXT "[ ")
                (WITH-LINK (TO 'index)
                  (TEXT "Home"))
                (TEXT " &gt; ")
                (CALL 'walk-up.)
                (TEXT @!name)
                (TEXT " ]")
                (LINEBREAK number: 2))
        else: nil)
    (IF test: (EQUALS value1: @!type
                      value2: 'search)
        then: (CALL 'search-body.
                wid)
        else: (CALL 'page-body.
                wid))))

;; This template is used to generate a button in the left or top navigation bar.                           
;; It takes five parameters: text sets the label for the button; dest is the                               
;; target URL of the button; topm sets the top margin; botm sets the bottom                                
;; margin; and sidem sets the left and right margins (this normally comes from                             
;; the button-padding variable.) The color of the button and its text, whether                             
;; the button is raised or not, and whether the label is beveled or not is                                 
;; determined by the various button-related variables such as button-text-color,
;; button-color, button-font-size, and button-style.                                                       
;;
;; Example:                                                                                                
;;              
;;   (IMAGE source: (CALL 'button. "Test Button" (TO 'index) 5 5 5))
;;                                                                                                         
;; Called by: imbutton., mall-button., nav-button., text-nav-button., x-navbuttons.                        

(def button. (text dest topm botm sidem)
  (RENDER text: text
          text-color: @!button-text-color
          text-align: 'left
          background-color: @!button-color
          font: @!button-font
          font-size: @!button-font-size
          destination: dest
          top-margin: topm
          bottom-margin: botm
          left-margin: sidem
          right-margin: sidem
          thickness: (CALL '|3d.|)
          intaglio: (AND
                      (EQUALS value1: @!button-style
                              value2: 'incised)
                      (NOT (EQUALS value1: @!button-color
                                   value2: black))
                      (CALL 'light-color.
                        @!button-text-color))))

;; This template takes a single argument, a text string, and displays the text string separating each para-
;; graph with a blank line.

(def display-paras. (text)
  (WITH= variable: paras
         value: (PARAGRAPHS text)
    (TEXT (ELEMENT position: 0
                   sequence: paras))
    (FOR-EACH var: para
              sequence: (ELEMENTS sequence: paras
                                  first: 1)
      (LINEBREAK number: 2)
      (TEXT para)
    )
  )
)

;; This template takes a text string and a cropping parameter and turns the text into an image using dis-
;; play-text-color as the color, display-font as the font, and display-font-size as the size of the text. The text
;; is always left aligned. The cropping parameter crop can be either :left or :right but it does not seem to
;; have any practical effect on the outcome of this template.
;;
;; The result of this template can be passed to an IMAGE operator for display.

(def display-text. (text crop)
  (FUSE axis: 'vertical
        align: 'left
    (FOR-EACH var: line
              sequence: (LINES text)
      (RENDER text: line
              text-color: @!display-text-color
              text-align: 'left
              font: @!display-font
              font-size: @!display-font-size
              crop: crop))))

(def group. ()
  (CALL 'base-item.
    'group))

(def head. (wid headel headsty)
  (WITH= variable: text
         value: (IF test: (NONEMPTY @!headline)
                    then: @!headline
                    else: @!name)
    (WITH= variable: textim
           value: (AND
                    (POSITION element: 'display-text-title
                              sequence: headel)
                    (CALL 'display-text.
                      text
                      'left))
      (WITH= variable: im
             value: (AND
                      (POSITION element: 'image
                                sequence: headel)
                      @!image
                      (RENDER image: @!image
                              max-height: @!item-height
                              max-width: @!item-width))
        (IF test: (EQUALS value1: headsty
                          value2: 'center)
            then: (CENTER
                    (WHEN im
                      (CALL 'imexpand.
                        im
                        @!image
                        @!item-height
                        @!item-width
                        nil)
                      (LINEBREAK number: 2))
                    (WHEN textim
                      (IMAGE source: textim
                             alt: text)
                      (LINEBREAK number: 2))
                  )
            else: (IF test: (WITH= variable: imwid
                                   value: (IF test: im
                                              then: (WIDTH textim)
                                              else: 0)
                              (WITH= variable: textwid
                                     value: (IF test: textim
                                                then: (WIDTH textim)
                                                else: 0)
                                (AND
                                  imwid
                                  (> wid 0)
                                  (OR
                                    (> (+ imwid textwid 8)
                                       wid)
                                    (< (- wid imwid)
                                       @!minimum-wrap-width)
                                  )
                                )
                              )
                            )
                      then: (MULTI
                              (TAG-WHEN tag: 'center
                                        test: (EQUALS value1: @!page-format
                                                      value2: 'top-buttons)
                                (WHEN im
                                  (CALL imexpand.
                                    im
                                    @!image
                                    @!item-height
                                    @!item-width
                                    nil)
                                  (LINEBREAK number: 2)
                                )
                                (WHEN textim
                                  (IMAGE source: textim
                                         alt: text)
                                  (LINEBREAK number: 2)
                                )
                              )
                            )
                      else: (MULTI
                              (WHEN im
                                (WITH= variable: height
                                       value: (HEIGHT im)
                                  (CALL 'imexpand.
                                    im
                                    @!image
                                    @!item-height
                                    @!item-width
                                    headsty)
                                  (SHIM height: (+ height 8)
                                        width: 10
                                        align: headsty)
                                )
                              )
                              (IMAGE source: textim
                                     alt: text)
                              (WHEN textim
                                (LINEBREAK number: 2))
                            )
                  )
        )
      )
    )
  )
)

;; This template creates a button if the button-style variable is set to "icon".
;; It takes three parameters:
;;
;; Im:   The image to be used as the button. If there is no image, then a simple
;;       button is generated using text as the label.
;; Text: The label to be used if the icon image for the button is not available.
;; Dest: The URL the button should linked to.
;;
;; The following example creates a sample "View Cart" button (if button-style is
;; "icon".)
;;
;;   (IMAGE source: (CALL 'imbutton. @!show-order-image "View Cart" (ACTION 'show-order)))
;;
;; Called by: nav-button.

(def imbutton. (im text dest)
  (IF test: (AND
              (EQUALS value1: @!button-style
                      value2: 'icon)
              im
            )
      then: (RENDER image: im
                    destination: dest)
      else: (CALL 'button.
              text
              dest
              3
              1
              @!button-padding)
  )
)

;; This is the template that’s responsible for displaying the main item (or section) image. If the source
;; image (the one uploaded into the image property) is larger than the item-width and item-height proper-
;; ties, then the image can be clicked to view the full-sized image.

(def imexpand. (im orig hlimit wlimit align)
  (WITH= variable: exp
         value: (CALL 'imexpands.
                  (RENDER image: orig)
                  hlimit
                  wlimit)
    (IF test: (NOT (EQUALS value1: exp
                           value2: 'no))
        then: (WITH-LINK (IMAGE-REF orig)
                (IMAGE source: im
                       align: align
                       alt: (WHEN (EQUALS value1: exp
                                          value2: 'yes)
                              "Click to enlarge"))
              )
        else: (IMAGE source: im
                     align: align)
    )
  )
)

;; This template determines whether an image fits within a box whose size is determined by the pa-
;; rameters hlimit and wlimit. The image parameter must be an image already passed through a RENDER
;; operator (in other words, not an image property directly, such as @name-image.) The template returns
;; the constant :yes if the image is expandable or :no if the image is already resized to its maximum.

(def imexpands. (image hlimit wlimit)
  (AND
    image
    hlimit
    wlimit
    (WITH= variable: h
           value: (HEIGHT image)
      (WITH= variable: w
             value: (WIDTH image)
        (WHEN (AND
                h
                w)
          (IF test: (AND (<= h hlimit)
                         (<= w wlimit))
              then: 'no
              else: 'yes
          )
        )
      )
    )
  )
)

;; Inset-image displays the inset image if one exists for the page. It takes two parameters: the image to
;; display in native (non-rendered) format, and an alignment constant. Based on the alignment constant
;; (:left or :right) it also automatically generates a 10 pixel left or right margin for the image (line 3 and 7
;; in the original template.) The image will be clickable to show its full size.

(def inset-image. (im align)
  (IMAGE source: (FUSE background-color: transparent
                       bottom-margin: 4
                       left-margin: (IF test: (EQUALS value1: align
                                                      value2: 'right)
                                        then: 10
                                        else: 0)
                       right-margin: (IF test: (EQUALS value1: align
                                                       value2: 'left)
                                         then: 10
                                         else: 0)
                   (RENDER image: im
                           max-height: @!inset-height
                           max-width: @!inset-width
                           expand: t))
         align: align))

(def item. ()
  (CALL 'base-item.
    'item))

(def light-color. (color)
  (AND
    color
    (> (GRAYSCALE color)
       180)
  )
)

;; This template creates a single icon-style button based on the type
;; of button needed. The type is one of the selections of the buttons
;; property of the home page or the nav-buttons variable (search,
;; info, index, contents, etc.)

(def nav-button. (type)
  (ero `(nav-button. ,type))
  (SWITCH type
    'help
    (CALL 'imbutton.
          @!help-image
          "Help"
          (ACTION 'help)
    )
    'search
    (CALL 'imbutton.
      @!search-image
      (WITH-OBJECT 'nsearch
        @!name)
      (TO 'nsearch)
    )
    'index
    (CALL 'imbutton.
      @!index-image
      "Index"
      (TO 'ind)
    )
    'info
    (CALL 'imbutton.
      @!info-image
      @!info-text
      (TO 'info)
    )
    'info
    (CALL 'imbutton.
      @!info-image
      @!info-text
      (TO 'info)
    )
    'privacypolicy
    (CALL 'imbutton.
      @!privacypolicy-image
      @!privacypolicy-text
      (TO 'privacypolicy)
    )
    'show-order
    (CALL 'imbutton.
      @!show-order-image
      @!show-order-text
      (ACTION 'show-order)
    )
    'mall
    (CALL 'mall-button.
      nil
    )
    'empty
    (CALL 'text-nav-button.
      type
      nil
    )
    'up
    (CALL 'imbutton.
      @!up-image
      "Up"
      (TO (OR
            (UP*)
            'index
          )
      )
    )
    'next
    (CALL 'imbutton.
      @!next-image
      "Next"
      (TO (NEXT*))
    )
    'home
    (CALL 'imbutton.
      @!home-image
      @!title
      (TO 'index)
    )
    'request
    (CALL 'imbutton.
      @!request-image
      @!request-text
      (ACTION 'request)
    )
    'register
    (CALL 'imbutton.
      @!register-image
      "Register"
      (ACTION 'register)
    )
    'download
    (CALL 'imbutton.
      @!download-image
      "Download"
      (ACTION 'download)
    )
    'contents
    (WITH-OBJECT 'index
      (FOR-EACH-OBJECT @!contents
        (IF test: @!icon
            then: (RENDER image: @!icon
                          destination: (TO (ID*)))
            else: (CALL 'button.
                    @!name
                    (TO (ID*))
                    3
                    1
                    @!button-padding
                  )
        )
      )
    )
    'email
    (WHEN (NONEMPTY @!email)
      (CALL 'imbutton.
        @!email-image
        "Email"
        (ACTION 'email)
      )
    )
    (err (list 'unknown-nav-button type))
  )
)

;; This is the main "wrapper" template that generates the navigation bar        
;; (horizontal or vertical.) The first parameter, buttons, is a sequence        
;; containing the types of buttons to include. This is typically equals to      
;; the value of the button property of the home page or the nav-buttons         
;; variable. The second parameter tells whether the button bar is horizontal    
;; (top-button arrangement) or vertical (side-button arrangement.)              
;;
;; The template first determines what kinds of buttons are needed (icon,        
;; text, solid, or incised) based on the button-style variable. If            
;; button-style is other than "icon", then lines 5 through 32, otherwise        
;; lines 33 through 43 are evaluated.
;;                                                                              
;; For non-icon buttons, the template first checks whether "home" is part
;; of the buttons and whether there is a home-image uploaded on the             
;; Variables page. If "home" is to be included and there is a home-image,
;; it first figures out the home button's position among the buttons. This      
;; is one less the number you see when you click "Change" next to
;; nav-buttons on the Variables page. It uses this number to create the         
;; navigation bar, which is a single image map. As you can see from this        
;; template, if you upload an image into the home-image variable, that
;; image is always used on the navigation bar unless you set the                
;; button-style variable to "icon". This way, you can upload your store         
;; logo into the home-image variable and display it in the usual upper
;; left hand corner right above the rest of the navigation buttons.        

(def nav-buttons. (buttons axis)
  (IF test: (OR
              (EQUALS value1: @!button-style
                      value2: 'text)
              (CALL '|3d.|))
      then: (WITH= variable: home-pos
                   value: (AND
                            @!home-image
                            (POSITION element: 'home
                                      sequence: buttons))
               (IF test: home-pos
                   then: (FUSE axis: axis
                               destination: (TO (ID*))
                               align: (SWITCH axis
                                        'horizontal
                                         'vcenter
                                        'vertical
                                         'left)
                            (WHEN (NOT (EQUALS value1: home-pos
                                               value2: 0))
                              (CALL 'nav-bar.
                                (ELEMENTS sequence: buttons
                                          last: (- home-pos 1))
                                axis))
                            (RENDER image: @!home-image
                                    destination: (TO 'index))
                            (CALL 'nav-bar.
                              (ELEMENTS sequence: buttons
                                        first: (+ home-pos 1))
                              axis))
                   else: (CALL 'nav-bar.
                           buttons
                           axis)))
      else: (FUSE axis: axis
                  destination: (TO (ID*))
                  align: (SWITCH axis
                           'horizontal
                            'vcenter
                           'vertical
                            'center)
              (FOR-EACH var: but
                        sequence: buttons
                (CALL 'nav-button.
                  but)))))

;; This is the main template used by section and item pages. It is
;; responsible for the main layout of the page.

(def page. ()
  (HEAD
    (WHEN (VALUE id: (ID*)
                 query: 'local
                 property: 'keywords)
      (META name: "Keywords"
            content: @!keywords)
    )
    (TITLE (IF test: (NONEMPTY @!page-title)
               then: @!page-title
               else: @!name))
    (TEXT @!head-tags)
  )
  (WITH= variable: vnav
         value: (WHEN (EQUALS value1: @!page-format
                              value2: 'side-buttons)
                  (CALL 'nav-buttons.
                    @!nav-buttons
                    'vertical))
    (WITH= variable: vnav-wid
           value: (WIDTH vnav)
      (BODY background-color: @!background-color
            background-image: (OR @!background-image
                                  (AND
                                    vnav
                                    (CALL 'side-stripe.
                                      vnav-wid)))
            text-color: @!text-color
            link-color: @!link-color
            visited-link-color: @!visited-link-color
        (SWITCH @!page-format
          'top-buttons
          (CENTER
            (WITH= variable: navbut
                   value: (CALL 'nav-buttons.
                            @!nav-buttons
                            'horizontal)
              (WITH= variable: wid
                     value: (CALL 'apparent-width.
                              navbut)
                (WHEN @!name-image
                  (WITH-LINK (TO 'index)
                    (IMAGE source: (RENDER image: @!name-image))
                  )
                  (LINEBREAK)
                )
                (WHEN (> wid 0)
                  (IMAGE source: navbut)
                )
                (CALL 'vspace.
                  20)
                (CALL 'body-switch.
                  wid)
              )
            )
          )
          'side-buttons
          (TABLE border: 0
                 cellspacing: 0
                 cellpadding: 0
            (TABLE-ROW valign: 'top
              (CALL 'side-nav.
                vnav)
              (TABLE-CELL
                (WITH= variable: wid
                       value: (- (- @!page-width 26) vnav-wid)
                  (WITH= variable: banner
                         value: (CALL 'page-name.
                                  wid)
                    (WITH-LINK (TO 'index)
                      (IMAGE source: banner)
                    )
                    (LINEBREAK number: 2)
                    (CALL 'body-switch.
                      (MAXIMUM
                        (IF test: banner
                            then: (WIDTH banner)
                            else: 0)
                        wid
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Page-name creates the store banner. If the name-image variable contains an image, it is used as the
;; store banner. This image is shown without any resizing (line 2.) If there is no name-image, then the title
;; variable (containing the store’s name) is used to generate a simple text image. The color of this text is
;; determined by the home-button-text-color variable. The argument wid is the maximum available width
;; for the banner (usually calculated as the width of the page body) and it is used to calculate the font size
;; so that the generated banner is as wide as the page body’s width allows it to be.
(def page-name. (wid)
  (IF test: @!name-image
      then: (RENDER image: @!name-image)
      else: (CALL 'banner.
              @!title
              @!home-button-text-color
              transparent
              wid)))

;; Paras-in-box takes two arguments, text and width. It then outputs the text string contained in text
;; separating paragraphs by blank lines. The text will be at most as wide as the width argument in pixels.

(def paras-in-box. (text width)
  (TABLE border: 0
         cellspacing: 0
         cellpadding: 0
         width: width
    (TABLE-ROW
      (TABLE-CELL
        (FONT size: @!text-size
              face: @!text-font
          (CALL 'display-paras.
            text)
        )
      )
    )
  )
)


;; This template is used for sites with side-button layouts. It outputs the side navigation bar and a
;; spacer image. The parameter vnav contains the side navigation image map. The spacer cell next to the
;; navigation bar is 26 pixels wide (as set by the spacer image on line 5). In Figure 25 you can see the lay-
;; out of a store with side buttons. The border of the table containing the layout has been highlighted. The
;; leftmost cell is created by the side-nav template on line 1, while the spacer cell next to it is generated on
;; line 4.

(def side-nav. (vnav)
  (TABLE-CELL
    (IMAGE source: vnav
           antialias-color @!button-edge-color))
  (TABLE-CELL
    (SHIM height: 1
          width: 26)))

