(defun HasIntersection (x)
	(and x (> (length x) 0))
)

(defun WriteResult (so side elev file)
	(write-line
		(strcat
			(rtos (car so) 2 3) ", "
			side (rtos (cadr so) 2 3) ", "
			(rtos elev 2 3)
		)
	file
	)
)

(defun Point->StationOffset (clObj pt / closest station offset)
	(setq closest
		(vlax-curve-getClosestPointTo clObj pt)
	)

	(setq station
		(vlax-curve-getDistAtPoint clObj closest)
	)

	(setq offset
		(distance pt closest)
	)

	(list station offset)
)

(defun c:LATERALS ()
	;;;;;;;;;;;;;;;; Load extended AutoLISP Functions
	(vl-load-com)
	;;;;;;;;;;;;;;;; Load DTM API Functions from Carlson
	(scload (strcat lspdir$ "tri4"))
	
	;;;;;;;;;;;;;;;; Prompt for user input
	(setq clEnt (car (entsel "\nSelect road centerline: ")))
	(setq clObject (vlax-ename->vla-object clEnt))
	(setq offsetDistance (getreal "\nEnter lateral offset:"))
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

	;;;;;;;;;;;;;;;; Load TIN
	(cf:dtm_api "load_tin" tinFile)	

	;;;;;;;;;;;;;;;; Offset CL for intersection detections
	(setq off1 (vlax-invoke clObject 'Offset offsetDistance))
	(setq off2 (vlax-invoke clObject 'Offset (- offsetDistance)))
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
				(setq so1 (Point->StationOffset clObject pt1))
				(setq elev1 (cf:dtm_api "tin_z" pt1))
				(if (null elev1)	; if no elevation is present, default to zero
					(setq elev1 0) 
				)

				(setq results (cons (list so1 "L" elev1) results))
			)
		)	
		(if (HasIntersection int2)
			(progn
				(setq pt2 (list (nth 0 int2) (nth 1 int2) (nth 2 int2)))
				(setq so2 (Point->StationOffset clObject pt2))
				(setq elev2 (cf:dtm_api "tin_z" pt2))
				(if (null elev2)	; if no elevation is present, default to zero
					(setq elev2 0) 
				)

				(setq results (cons (list so2 "R" elev2) results))
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
		(WriteResult (nth 0 r) (nth 1 r) (nth 2 r) file)
	)
	
	(close file)
  	(princ)
)