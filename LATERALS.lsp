;;;;;;;;;;;;;;;;;;;; Draws a polyline on the drawing from a given centerline file.
; INPUTS - path: a full filepath to the centerline file.
; OUTPUTS - the drawn centerline as an object.
(defun drawCenterline (path / file line fields code x y pts currentRadius currentDir centerX centerY)
  
  ;;;;;;;;;;;;;;;; Read all points from file
  (setq file (open path "r"))
  (setq pts '())
  (setq currentRadius nil)
  (setq currentDir nil)
  (setq centerX nil)
  (setq centerY nil)
  
  (while (setq line (read-line file))
    (setq fields (str-split line ","))
    (setq code (nth 2 fields))
    (setq x    (atof (nth 4 fields)))  ; easting
    (setq y    (atof (nth 3 fields)))  ; northing

	(cond
	  ;;;; Curve definition row - store radius and center point, draw arc immediately
	  ((equal code "R")
	    (setq currentRadius (atof (nth 1 fields)))
	    (setq centerX       (atof (nth 4 fields)))
	    (setq centerY       (atof (nth 3 fields)))
	  )
	  ;;;; Terminator row - skip
	  ((and (equal code "L") (equal x 0.0) (equal y 0.0))
	    nil
	  )
	  ;;;; Normal point row
	  (T
	    (setq pts (append pts (list (list code x y currentRadius currentDir centerX centerY))))
	    ;;;; Reset curve info after PT
	    (if (equal code "PT")
	      (progn
	        (setq currentRadius nil)
	        (setq currentDir nil)
	        (setq centerX nil)
	        (setq centerY nil)
	      )
	    )
	  )
	)
  )
  (close file)

  ;;;;;;;;;;;;;;;; Begin PLINE command with first point
  (setq firstPt (list (cadr (car pts)) (caddr (car pts)) 0.0))
  (command "PLINE" firstPt)

  ;;;;;;;;;;;;;;;; Iterate through remaining points
  (setq i 1)
  (while (< i (length pts))
    (setq current (nth i pts))
    (setq code    (nth 0 current))
    (setq x       (nth 1 current))
    (setq y       (nth 2 current))
    (setq pt      (list x y 0.0))

    (cond
      ;;;; PC - draw line to PC, then arc using center point and angle from PT entry
      ((equal code "PC")
        (command pt)
        (setq nextPt (nth (1+ i) pts))
        (setq angle  (nth 3 nextPt))
        (setq ncx    (nth 5 nextPt))
        (setq ncy    (nth 6 nextPt))
        (command "A" "CE" (list ncx ncy 0.0) "A" angle "L")
      )
      ;;;; PT - skip, already consumed by PC's arc command
      ((equal code "PT")
        nil
      )
      ;;;; Straight segment
      (T
        (command pt)
      )
    )
    (setq i (1+ i))
  )

  ;;;;;;;;;;;;;;;; Close out PLINE command
  (command "")

  ;;;;;;;;;;;;;;;; Return the drawn object
  (setq clEnt (entlast))
  (vlax-ename->vla-object clEnt)
)

;;;;;;;;;;;;;;;;;;;; Splits a string by a delimiter.
; INPUTS - str: the string to split. delim: the delimiter character.
; OUTPUTS - a list of strings.
(defun str-split (str delim / pos result current)
  (setq result '())
  (setq current "")
  (setq pos 0)
  (while (< pos (strlen str))
    (setq ch (substr str (1+ pos) 1))
    (if (equal ch delim)
      (progn
        (setq result (append result (list current)))
        (setq current "")
      )
      (setq current (strcat current ch))
    )
    (setq pos (1+ pos))
  )
  (setq result (append result (list current)))
  result
)

(defun GetDateTimeString ()
	(setq dateTime (getvar "DATE"))

	(setq jdate (getvar "DATE"))

	;; Time
	(setq frac (- jdate (fix jdate)))
	(setq secs (fix (* frac 86400)))
	(setq h (/ secs 3600))
	(setq m (/ (rem secs 3600) 60))
	(setq s (rem secs 60))

	;; Date - convert Julian Day Number to Gregorian
	(setq jdn (fix jdate))
	(setq p (+ jdn 68569))
	(setq q (/ (* 4 p) 146097))
	(setq r (- p (/ (+ (* 146097 q) 3) 4)))
	(setq v (/ (* 4000 (+ r 1)) 1461001))
	(setq r (- r (+ (/ (* 1461 v) 4) -31)))
	(setq u (/ (* 80 r) 2447))
	(setq day   (- r (/ (* 2447 u) 80)))
	(setq t2    (/ u 11))
	(setq month (+ u 2 (* -12 t2)))
	(setq year  (+ (* 100 (- q 49)) v t2))

	(setq dateTime
  		(strcat
    		(itoa year) "/"
    		(if (< month 10) (strcat "0" (itoa month)) (itoa month)) "/"
    		(if (< day   10) (strcat "0" (itoa day))   (itoa day))   " "
    		(if (< h 10) (strcat "0" (itoa h)) (itoa h)) ":"
    		(if (< m 10) (strcat "0" (itoa m)) (itoa m)) ":"
    		(if (< s 10) (strcat "0" (itoa s)) (itoa s))
  		)
	)
)

;;;;;;;;;;;;;;;;;;;; Checks if a given list has any items. If it does, return true. Else, return false.
; INPUTS - x: a list
; OUTPUTS - BOOL: True if length > 0, else false.
(defun HasIntersection (x)
	(and x (> (length x) 0))
)

;;;;;;;;;;;;;;;;;;;; Writes a result to the output file in the correct format.
; INPUTS - so: a list with two ints representing the station and offset along a centerline.
;          elev: an int, representing an elevation.
;		   stake: offset distance to stake at
;          file: the file to write to
(defun WriteResult (so elev stake file)
	(setq station (car so))
	(setq offset (cadr so))
	(setq num2 (rem station 100)) ; number after the plus
	(setq num1 (/ (- station num2) 100)) ; number before the plus
	(setq num1str (rtos num1 2 0))
	(setq num2str 
		(if (< num2 10.0)
			(strcat "0" (rtos num2 2 2))
			(rtos num2 2 2)
		)
	)
	(setq offsetStr ; if offset is negative, it's Left. else, right.
		(if (< offset 0)
			(strcat "L" (rtos stake 2 3))
			(strcat "R" (rtos stake 2 3))
		)
	)
	(setq elevStr (rtos elev 2 3))

	(write-line
		(strcat
			num1str "+" num2str " "
			offsetStr " "
			elevStr
		)
	file
	)
)

;;;;;;;;;;;;;;;;;;; Converts real-world coordinates to station and offset from a given centerline.
; INPUTS - file: path to a cl file
;          pt: a list with 3 int items, x y and z.
; OUTPUT - a list with 2 int items, station and offset to the point from the centerline.
(defun Point->StationOffset (file pt / stationOffset)
	(setq stationOffset (cf:road_api "cl_location_at_pt" file pt 0))
	stationOffset
)

;;;;;;;;;;;;;;;;;; Main command. Runs the program. Call LATERALS in carlson to execute.
(defun c:LATERALS ()
	;;;;;;;;;;;;;;;; Load extended AutoLISP Functions
	(vl-load-com)
	;;;;;;;;;;;;;;;; Load API Functions from Carlson
	(scload (strcat lspdir$ "tri4"))
	(scload (strcat lspdir$ "eworks"))
	
	;;;;;;;;;;;;;;;; Prompt for user input
	(setq clFile
		(getfiled
			"Select Road CL File"
			""
			"cl"
			0
		)
	)
	(setq clObject (drawCenterline clFile))
	(setq stakeDistance (getreal "\nEnter offset to detect laterals at:"))
	(setq ss (ssget))
	(setq tinFile
		(getfiled
			"Select TIN Surface File"
			""
			"tin;xml;flt;crd"
			0
		)
	)
	(setq outFile
		(getfiled
			"Save Output File"
			"C:/temp/output.txt"
			"txt"
			1   ; 1 = Save dialog
		)
	)

	;;;;;;;;;;;;;;;; Open output filestream and write header
	(setq file (open outFile "w"))
	(write-line "Calculate Lateral Offsets" file)
	(write-line (strcat "Centerline: " clFile) file)
	(write-line (strcat "Date/Time: " (GetDateTimeString)) file)
	(write-line "" file)

	;;;;;;;;;;;;;;;; Load TIN
	(cf:dtm_api "load_tin" tinFile)	

	;;;;;;;;;;;;;;;; Offset CL for intersection detections
	(setq off1 (vlax-invoke clObject 'Offset stakeDistance))
	(setq off2 (vlax-invoke clObject 'Offset (- stakeDistance)))
	(setq offsetObject1 (vla-Item off1 0))
	(setq offsetObject2 (vla-Item off2 0))

	;;;;;;;;;;;;;; Collect intersections
	(setq results '())
	(setq i 0)
	(while (< i (sslength ss))
		(setq ent (ssname ss i ))
		(setq obj (vlax-ename->vla-object ent))

		(setq int1 (vlax-invoke obj 'IntersectWith offsetObject1 0))
		(setq int2 (vlax-invoke obj 'IntersectWith offsetObject2 0))

		(if (HasIntersection int1)
			(progn
				(setq pt1 (list (nth 0 int1) (nth 1 int1) (nth 2 int1)))
				(setq so1 (Point->StationOffset clFile pt1))
				(setq elev1 (cf:dtm_api "tin_z" pt1))
				(if (null elev1)	; if no elevation is present, default to zero
					(setq elev1 0) 
				)

				(setq results (cons (list so1 elev1) results))
			)
		)	
		(if (HasIntersection int2)
			(progn
				(setq pt2 (list (nth 0 int2) (nth 1 int2) (nth 2 int2)))
				(setq so2 (Point->StationOffset clFile pt2))
				(setq elev2 (cf:dtm_api "tin_z" pt2))
				(if (null elev2)	; if no elevation is present, default to zero
					(setq elev2 0) 
				)

				(setq results (cons (list so2 elev2) results))
			)
		)		

		(setq i (1+ i))
	)

	;;;;;;;;;;;;;;;;;;;;; Sort results by station
	(setq results
		(vl-sort results
			'(lambda (a b) (< (car (car a)) (car (car b))))
		)
	)

	;;;;;;;;;;;;;;;;;;;;; Write sorted results to txt file
	(foreach r results
		(WriteResult (nth 0 r) (nth 1 r) stakeDistance file)
	)
	
	(close file)
  	(princ)
)