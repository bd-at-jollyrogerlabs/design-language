;; Test cases for layout language.
;;
;; TODO: update license terms.
;;
;; Copyright (C) 2014 Brian Davis
;; All Rights Reserved

(setq test-plate-single-centered-hole
      (make-layout 20.0 20.0 6.0
		   (list
		    (make-hole "center" 0.0 0.0 3.3))))

(setq test-plate-two-centered-holes
      (make-layout 20.0 20.0 6.0
		   (list
		    (make-hole "top" 0.0 5.0 3.3)
		    (make-hole "bottom" 0.0 -5.0 3.3)
		    )))

(setq test-plate-two-off-center-holes
      (make-layout 20.0 20.0 6.0
		   (list
		    (make-hole "top" 0.0 5.0 2.0)
		    (make-hole "bottom" 0.0 -5.5 2.0)
		    )))

;;(generate-scad-module-from-layout "test_plate_single_centered_hole" test-plate-single-centered-hole)

;;(generate-mechanical-drawing-from-layout "Single Centered Hole" "test-plate-single-centered-hole.svg" 20.0 20.0 50.0 20.0 test-plate-single-centered-hole)

;;(generate-scad-module-from-layout "test_plate_two_centered_holes" test-plate-two-centered-holes)

;;(generate-mechanical-drawing-from-layout "Two Centered Holes" "test-plate-two-centered-holes.svg" 20.0 20.0 50.0 20.0 test-plate-two-centered-holes)

;;(generate-scad-module-from-layout "test_plate_two_off_center_holes" test-plate-two-off-center-holes)

;;(generate-mechanical-drawing-from-layout "Two Off-center Holes" "test-plate-two-off-center-holes.svg" 20.0 20.0 50.0 20.0 test-plate-two-off-center-holes)
