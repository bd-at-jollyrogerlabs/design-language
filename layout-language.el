;; Layout language - simple language for designing parts and
;; automatically generating various representations.
;;
;; TODO: update license terms.
;;
;; Copyright (C) 2014 Brian Davis
;; All Rights Reserved

(require 'avl-tree)

;; Part designs

(defun make-hole (name center-x center-y radius)
  (list center-x center-y radius name))

(defun hole-get-center-x (hole)
  (car hole))

(defun hole-get-center-y (hole)
  (car (cdr hole)))

(defun hole-get-radius (hole)
  (car (cdr (cdr hole))))

(defun hole-get-name (hole)
  (car (cdr (cdr (cdr hole)))))

(defun make-layout (expected-length-x expected-length-y height holes)
  (list expected-length-x expected-length-y height holes))

(defun layout-get-expected-length-x (layout)
  (car layout))

(defun layout-get-expected-length-y (layout)
  (car (cdr layout)))

(defun layout-get-height (layout)
  (car (cdr (cdr layout))))

(defun layout-get-holes (layout)
  (car (cdr (cdr (cdr layout)))))


;; OpenSCAD generation below here

(defun scad-cube (indent x y z)
  (format "%scube([%f, %f, %f], center=true);\n" indent x y z))

(defun scad-cylinder (indent radius height)
  (format "%scylinder (r=%f, h=%f, center=true);\n" indent radius height))

(defun scad-translate (indent x y z)
  (format "%stranslate([%f, %f, %f])\n" indent x y z))

(defun generate-scad-module-from-layout (name layout)
  (let ((height (layout-get-height layout)))
    (concat
     (format "\nmodule %s() {\n" name)
     (format "  difference() {\n")
     (scad-cube
      "    "
      (layout-get-expected-length-x layout)
      (layout-get-expected-length-y layout)
      height)
     (format "    union() {\n")
     (mapconcat (function (lambda (hole)
                            (concat
                             (scad-translate
                              "      "
                              (hole-get-center-x hole)
                              (hole-get-center-y hole)
                              0.0)
                             (scad-cylinder "        "
                                            (hole-get-radius hole)
                                            (+ height 0.5)))))
                          (layout-get-holes layout) "")
     (format "    }\n")
     (format "  }\n")
     (format "}\n"))))


;; .svg mechanical drawing generation below here

(defun svg-rect (left top length-x length-y)
  (concat
   (format "<rect fill=\"rgb(100%%,100%%,100%%)\" stroke=\"black\" stroke-width=\"4\" ")
   (format "x=\"%fmm\" y=\"%fmm\" width=\"%fmm\" height=\"%fmm\"/>\n"
           left top length-x length-y)))

(defun svg-circle (center-x center-y radius &optional color)
  (concat
   (format "<circle ")
   (when color
     (format "fill=\"%s\" " color))
   (format "stroke=\"black\" stroke-width=\"2\" ")
   (format "cx=\"%fmm\" cy=\"%fmm\" r=\"%fmm\"/>\n" center-x center-y radius)))

(defun svg-text (center-x center-y text &optional anchor-point)
  (let ((effective-anchor (if anchor-point anchor-point "start")))
   (format "<text style=\"text-anchor: %s\" x=\"%fmm\" y=\"%fmm\">%s</text>\n"
	   effective-anchor (+ center-x 5.0) (+ center-y 1.5) text)))

(defun svg-line (x1 y1 x2 y2 &optional stroke-dasharray color)
  (let ((effective-color (if color color "black")))
  (concat
   (format "<line ")
   (when stroke-dasharray
     (format "stroke-dasharray=\"%s\" " stroke-dasharray))
   (format "x1=\"%fmm\" y1=\"%fmm\" x2=\"%fmm\" y2=\"%fmm\" %s\n"
	   x1 y1 x2 y2 "style=\"stroke: black;\"/>"))))

;; Endpoint points to the left.
(defun svg-left-endpoint (x-pos y-pos)
  (concat
   (svg-line (+ x-pos 0.25) y-pos (+ x-pos 2) (+ y-pos 1))
   (svg-line (+ x-pos 0.25) y-pos (+ x-pos 2) (- y-pos 1))))

;; Endpoint points to the right.
(defun svg-right-endpoint (x-pos y-pos)
  (concat
   (svg-line (- x-pos 0.25) y-pos (- x-pos 2) (+ y-pos 1))
   (svg-line (- x-pos 0.25) y-pos (- x-pos 2) (- y-pos 1))))

;; Endpoint points up.
(defun svg-up-endpoint (x-pos y-pos)
  (concat
   (svg-line x-pos (- y-pos 0.25) (- x-pos 1) (+ y-pos 2))
   (svg-line x-pos (- y-pos 0.25) (+ x-pos 1) (+ y-pos 2))))

;; Endpoint points down.
(defun svg-down-endpoint (x-pos y-pos)
  (concat
   (svg-line x-pos (+ y-pos 0.25) (- x-pos 1) (- y-pos 2))
   (svg-line x-pos (+ y-pos 0.25) (+ x-pos 1) (- y-pos 2))))

;; Left to right for horizontal lines, top to bottom for vertical
;; lines.
(defun reorder-points (x1 y1 x2 y2)
  (if (< (abs (- y1 y2)) 0.00001)
      ;; X-axis line
      (if (< x1 x2)
	  ;; First point comes first.
	  (list (list x1 y1) (list x2 y2))
	;; First point comes second.
	(list (list x2 y2) (list x1 y1)))
    ;; Y-axis line
    (if (< y1 y2)
	;; First point comes first.
	(list (list x1 y1) (list x2 y2))
      ;; First point comes second.
      (list (list x2 y2) (list x1 y1)))))

(defun svg-right-pointed-line (x1 y1 x2 y2)
  (if (< x2 x1)
      (svg-right-pointed-line (x2 y2 x1 y1))
    (concat
     (svg-line x1 y1 x2 y2)
     (svg-right-endpoint x2 y2)
     )))

(defun svg-down-pointed-line (x1 y1 x2 y2)
  (if (< y2 y1)
      (svg-down-pointed-line (x2 y2 x1 y1))
    (concat
     (svg-line x1 y1 x2 y2)
     (svg-down-endpoint x2 y2)
     )))

(defun svg-pointed-line (x1 y1 x2 y2)
  (let ((ordered-list (reorder-points x1 y1 x2 y2)))
    (if (< (abs (- y1 y2)) 0.00001)
       ;; horizontal line
       (let ((left-x (car (car ordered-list)))
	     (left-y (car (cdr (car ordered-list))))
	     (right-x (car (car (cdr ordered-list))))
	     (right-y (car (cdr (car (cdr ordered-list))))))
	 (concat
	  (svg-left-endpoint left-x left-y)
	  (svg-right-endpoint right-x right-y)
	  (svg-line left-x left-y right-x right-y)
	  ))
      ;; vertical line
      (let ((top-x (car (car ordered-list)))
	    (top-y (car (cdr (car ordered-list))))
	    (bottom-x (car (car (cdr ordered-list))))
	    (bottom-y (car (cdr (car (cdr ordered-list))))))
	(concat
	 (svg-up-endpoint top-x top-y)
	 (svg-down-endpoint bottom-x bottom-y)
	 (svg-line top-x top-y bottom-x bottom-y)
	 )))))

;; FIXME: this probably exists in some library somewhere...
(defun sgn (x)
  (cond ((< x 0) -1) (t 1)))

(defun get-clean-buffer (name)
  (let* ((tmp (get-buffer name))
	 (buf (if (not tmp)
		  (get-buffer-create name)
		(if (yes-or-no-p (format "Erase the current contents of '%s'? " name))
		    (progn
		      (kill-buffer tmp)
		      (get-buffer-create name))
		  nil))))
    (if buf
	(progn
	  (set-buffer buf)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  buf)
      nil
      )))

;; Useful .svg constants
(setq *svg-dotted-line* "1,2")

(setq *svg-middle* "middle")

(setq *svg-end* "end")

;; Helper functions
(defun svg-hole-diameter (center-x center-y radius wp-bottom &optional unit-conversion-factor unit-string)
  (let* ((start-x (- center-x radius))
	 (end-x (+ center-x radius))
	 (line-y (if (> (+ center-y 15) wp-bottom)
		     (- center-y radius 2)
		   (+ center-y radius 2)))
	 (text-y (if (> (+ center-y 15) wp-bottom)
		     (- center-y radius 5)
		   (+ center-y radius 5)))
	 (dotted-top (if (> (+ center-y 15) wp-bottom)
			 line-y
		       center-y))
	 (dotted-bottom (if (> (+ center-y 15) wp-bottom)
			    center-y
			  line-y))
	 (convert-factor (if unit-conversion-factor unit-conversion-factor 1.0))
	 (units (if unit-string unit-string "mm"))
	 )
    (concat
     (svg-pointed-line start-x line-y end-x line-y)
     (svg-line start-x dotted-top start-x dotted-bottom *svg-dotted-line*)
     (svg-line end-x dotted-top end-x dotted-bottom *svg-dotted-line*)
     (svg-text center-x text-y (format "%.3f%s" (* (* radius 2.0) convert-factor)
	       units) *svg-end*)
     )))

;; TODO: requested features for generate-mechanical-drawing-from-layout
;;
;; 1. Round all values to nearest 0.5mm.  Alternatively, support
;;    rounding policy delivered as a function by the user.
;;
;; 2. Specify hole diameter in a side note if all holes are the same
;;    diameter.  If multiple holes are required, use some sort of
;;    table display.
;;
;; 3. Precision should be a user policy.
;;
;; 4. Eventually, all unit-related stuff (i.e. units, conversion
;;    factor, unit string, rounding policy, display precision) should
;;    be contained in a single unit policy object.

;; name - Name of the design.
;;
;; filename - Name of the file to store the .svg output in.
;;
;; actual-length-x - Actual X-axis length of the workpiece, used to
;;                   compensate for deviations from design length due
;;                   to e.g. manufacturing tolerance.
;;
;; actual-length-y - Actual Y-axis length of the workpiece, used to
;;                   compensate for deviations from design length due
;;                   to e.g. manufacturing tolerance.
;;
;; wp-edge-offset-x - Offset between the edge of the workpiece and the
;;                    left side of the drawing, allowing the user to
;;                    center the representation of the workpiece
;;                    appropriately in the drawing.
;;
;; wp-edge-offset-y - Offset between the edge of the workpiece and the
;;                    top of the drawing, allowing the user to center
;;                    the representation of the workpiece
;;                    appropriately in the drawing.
;;
;; layout - Object representing the design to be displayed in the
;;          drawing.
;;
;; unit-conversion-factor - Factor to multiply distances by for
;;                          display purposes.  Standard distance is
;;                          mm, so this factor should convert from mm
;;                          to the desired units (e.g. 0.03937 to
;;                          convert to inches).
;;
;; unit-string - String used to display the units, should be
;;               consistent with unit-conversion-factor.  Default is
;;               "mm".
(defun generate-mechanical-drawing-from-layout
  (name filename actual-length-x actual-length-y wp-edge-offset-x wp-edge-offset-y layout &optional unit-conversion-factor unit-string)
  (let ((outbuf (get-clean-buffer filename))
	(convert-factor (if unit-conversion-factor unit-conversion-factor 1.0))
	(units (if unit-string unit-string "mm"))
	)
    (if outbuf
	(progn
	  (set-window-buffer (selected-window) outbuf)
	  ;; outbuf should be empty at this point
	  (insert
	   (let* (;; Expected 2D lengths of the workpiece, as defined in the layout.
		  (expected-length-x (layout-get-expected-length-x layout))
		  (expected-length-y (layout-get-expected-length-y layout))
		  ;; Center of the workpiece in the diagram.
		  (wp-center-x (+ (/ actual-length-x 2.0) wp-edge-offset-x))
		  (wp-center-y (+ (/ actual-length-y 2.0) wp-edge-offset-y))
		  ;; Calculated left + top of the workpiece
		  (wp-left
		   (- wp-center-x (/ actual-length-x 2.0)))
		  (wp-top
		   (- wp-center-y (/ actual-length-y 2.0)))
		  ;; Calculated right + bottom of the workpiece
		  (wp-right
		   (+ wp-left actual-length-x))
		  (wp-bottom
		   (+ wp-top actual-length-y))
		  (hole-list-offset-x (+ wp-right 25))
		  (hole-list-offset-y wp-top)
		  ;; AVL trees used to determine which points to mark with
		  ;; lengths.
		  (x-ordered-tree
		   (avl-tree-create (function
				     (lambda (hole1 hole2)
				       (let ((x1 (hole-get-center-x hole1))
					     (x2 (hole-get-center-x hole2)))
					 (< x1 x2))))))
		  (y-ordered-tree
		   (avl-tree-create (function
				     (lambda (hole1 hole2)
				       (let ((y1 (hole-get-center-y hole1))
					     (y2 (hole-get-center-y hole2)))
					 (< y1 y2))))))
		  )
	     (concat
	      (format "\n<svg xmlns=\"http://www.w3.org/2000/svg\">\n")
	      (svg-rect wp-left wp-top actual-length-x actual-length-y)
	      ;; Display all holes.
	      "<!-- Holes -->\n"
	      (mapconcat (function
	      		  (lambda (hole)
	      		    (let* ((center-x (hole-get-center-x hole))
				   (diagram-center-x (+ wp-center-x center-x))
	      			   (center-y (hole-get-center-y hole))
				   ;; Sign of direction is reveresed
				   ;; in the Y-axis between design
				   ;; coordinates and .svg diagram
				   ;; coordinates.
				   (diagram-center-y (- wp-center-y center-y))
	      			   (radius (hole-get-radius hole))
				   (diameter-line-y (+ diagram-center-y radius 2))
				   (hole-name (hole-get-name hole))
	      			   )
			      (setq hole-list-offset-y (+ hole-list-offset-y 5))
	      		      (concat
			       ;; Drawing of the hole.
	      		       (svg-circle diagram-center-x diagram-center-y
					   radius "rgb(100%,100%,100%)")
			       ;; Display of the name/designator of the hole.
	      		       (svg-text (+ diagram-center-x radius) (- diagram-center-y radius)
					 hole-name *svg-end*)
			       ;; Display of the diameter of the hole.
			       (svg-hole-diameter diagram-center-x diagram-center-y
						  radius wp-bottom convert-factor units)
			       ;; Right side display of distances by hole
			       (svg-text hole-list-offset-x hole-list-offset-y
					 (format "%s (%.3f, %.3f) %.3f%s diameter"
						 hole-name
						 (* (- (+ wp-center-x center-x) wp-left)
						    convert-factor)
						 (* (- (- wp-center-y center-y) wp-top)
						    convert-factor)
						 (* (* radius 2.0) convert-factor)
						 units
						 ))

			       ;; Maybe add this hole to the set of
			       ;; holes ordered by X-axis value.
	      		       (if (not (avl-tree-member x-ordered-tree hole))
	      		       	   (progn
	      		       	     (avl-tree-enter x-ordered-tree hole)
				     ""))  ;; NOTE: concatenating empty string
			       ;; Maybe add this hole to the set of
			       ;; holes ordered by X-axis value.
	      		       (if (not (avl-tree-member y-ordered-tree hole))
	      		       	   (progn
	      		       	     (avl-tree-enter y-ordered-tree hole)
				     ""))  ;; NOTE: concatenating empty string
	      		       ))))
	      		 (layout-get-holes layout) "")
	      ;; Display all X-axis layout distances.
	      "<!-- X-axis layout distances -->\n"
	      (svg-pointed-line wp-left (+ wp-bottom 5) wp-right (+ wp-bottom 5))
	      (svg-text (+ (/ actual-length-x 2.0) wp-left) (+ wp-bottom 7.5)
			(format "%.3f%s" (* actual-length-x convert-factor) units)
			*svg-middle*)
	      (let ((previous-end-x wp-left)
		    (offset-above 5)
		    (multiplier-above 0))
		(mapconcat (function
			    (lambda (hole)
			      (let* (;; Calculate various X-axis values.
				     (center-x (hole-get-center-x hole))
				     (distance-line-start-x previous-end-x)
				     (distance-line-end-x (+ wp-center-x center-x))
				     (text-displayed-distance-x
				      (- distance-line-end-x distance-line-start-x))
				     (text-displayed-total-distance-x
				      (- distance-line-end-x wp-left))
				     (text-display-x
				      (if (< text-displayed-distance-x 10)
					  (- distance-line-start-x 2)
					(+ distance-line-start-x
					   (/ text-displayed-distance-x 2.0))))
				     ;; Calculate various Y-axis values.
				     (center-y (- (hole-get-center-y hole)))
				     (distance-line-display-y
				      (- wp-top offset-above
					 (* multiplier-above offset-above)))
				     )
				(setq previous-end-x distance-line-end-x)
				(setq multiplier-above (if (= 0 multiplier-above) 1 0))
				(concat
				 ;; Line with arrows at endpoints to
				 ;; show the distance from workpiece left.
				 (svg-right-pointed-line
				  distance-line-start-x distance-line-display-y  ;; start
				  distance-line-end-x distance-line-display-y)   ;; end
				 ;; Dotted lines projecting from the
				 ;; "pointed" distance lines all the
				 ;; way down through the workpiece.
				 (svg-line
				  distance-line-start-x distance-line-display-y
				  distance-line-start-x wp-bottom *svg-dotted-line*)
				 (svg-line
				  distance-line-end-x distance-line-display-y
				  distance-line-end-x wp-bottom *svg-dotted-line*)
				 ;; Text displaying the actual
				 ;; distance from the workpiece left
				 ;; edge.
				 (svg-text
				  text-display-x
				  (- distance-line-display-y 5)
				  (format "%.3f%s" (* text-displayed-total-distance-x
						      convert-factor)
					  units) *svg-end*)
				 ))))
			   (avl-tree-flatten x-ordered-tree) ""))
	      ;; Display all Y-axis layout distances.  NOTE: order of
	      ;; holes is reversed -> holes start at the bottom and go
	      ;; to the top.
	      "<!-- Y-axis layout distances -->\n"
	      (svg-pointed-line (+ wp-right 5) wp-top (+ wp-right 5) wp-bottom)
	      (svg-text (+ wp-right 6) (+ wp-top (/ actual-length-y 2.0))
			(format "%.3f%s" (* actual-length-y convert-factor) units))
	      (let ((previous-end-y wp-top)
		    (offset-left 5))
		(mapconcat (function
			    (lambda (hole)
			      (let* (;; Calculate various Y-axis values.
				     (center-y (hole-get-center-y hole))
				     (distance-line-start-y previous-end-y)
				     (distance-line-end-y (- wp-center-y center-y))
				     (text-displayed-distance-y
				      (- distance-line-end-y distance-line-start-y))
				     (text-displayed-total-distance-y
				      (- distance-line-end-y wp-top))
				     (text-display-y
				      (+ distance-line-start-y (/ text-displayed-distance-y 2.0)))
				     ;; Calculate various X-axis values.
				     (center-x (- (hole-get-center-x hole)))
				     (distance-line-display-x (- wp-edge-offset-x offset-left))
				     )
				(setq previous-end-y distance-line-end-y)
				(concat
				 ;; Line with arrows at endpoints to
				 ;; show the distance from workpiece top.
				 (svg-down-pointed-line
				  distance-line-display-x distance-line-start-y  ;; start
				  distance-line-display-x distance-line-end-y)   ;; end
				 ;; Dotted lines projecting from the
				 ;; "pointed" distance lines all the
				 ;; way across through the workpiece.
				 (svg-line
				  distance-line-display-x distance-line-start-y
				  wp-right distance-line-start-y *svg-dotted-line*)
				 (svg-line
				  distance-line-display-x distance-line-end-y
				  wp-right distance-line-end-y *svg-dotted-line*)
				 ;; Text displaying the actual
				 ;; distance from the workpiece top
				 ;; edge.
				 (svg-text
				  (- distance-line-display-x 7.5)
				  distance-line-end-y
				  (format "%.3f%s" (* text-displayed-total-distance-y
						      convert-factor)
					  units) *svg-end*)
				 ))))
			   ;; NOTE: reversing the order of the holes
			   ;; here to accommodate the calculation of
			   ;; distances from the top of the workpiece.
			   (reverse (avl-tree-flatten y-ordered-tree)) ""))
	      "<!-- Layout Name -->\n"
	      (svg-text wp-center-x (+ wp-bottom 15) name *svg-middle*)
	      (format "</svg>\n")
	      )))))))
