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

;; Called by group or item to create a page with a specific layout.             
;; Sets up the HTML head (keywords meta tag, title, extra head tags)            
;; and body styling (background color/image, text color, link color). 

(def base-item. ((o use 'item))
  (HEAD
    (META name: "Keywords"
          content: @!keywords)
    (TITLE @!name)
    (TEXT @!head-tags))
  (BODY
    background-color: @!background-color
    background-image: @!background-image
    text-color: @!text-color
    link-color: @!link-color
    visited-link-color: @!visited-link-color
    (CENTER
      (WITH= variable: navbut
             value: (CALL 'nav-buttons.
                          @!nav-buttons
                          'horizontal)
        (TEXT navbut)
        (TEXT "hi")))))

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
                               destination: (TO id)
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
                  destination: (TO id)
                  align: (SWITCH axis
                           'horizontal
                           'vcenter
                           'vertical
                           'center)
              (FOR-EACH var: but
                        sequence: buttons
                (CALL 'nav-button.
                  but)))))




