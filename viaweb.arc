(require "rtml.arc")


(def 3d. ()
  (or (is @!button-style 'solid)
      (is @!button-style 'incised)))

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

(def banner-font-size. (font chars wid)
  (let wid (or wid 370)
    (let base (if (> chars 15)
                  (* 0.98 (/ 720 chars) (/ wid 370))
                  (- 50 (* chars 0.8)))
      (let mult (FONT-WIDTH font)
        (/ base mult)))))

(def banner. (text color bgcolor wid)
  (render text: text
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
                        @!button-text-color)
                    )
  )
)

(def group. ()
  (CALL 'base-item.
    'group))

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

(def item. ()
  (CALL 'base-item.
    'item))

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



