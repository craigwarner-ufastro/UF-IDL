;+
; NAME:
;	intersection
; PURPOSE:
;	Function to determine if two intervals, boxes, cubes, etc. intersect.
;	If so, return 1 = true and the Locations of intersection within each.
;	
; CALLING:
;	intersect_flag = intersection( boxA, boxB, sectionA, sectionB )
;
; INPUTS:
;	boxA, boxB = vectors or matrices, of form:
;		[ [ Left-endpoint , Right-endpoint ] X (# dimensions) ]
; OUTPUTS:
;	sectionA, sectionB = optional, vectors or matrices, same form as input,
;		but endpoints are now the starting & ending Locations
;		of the intersection within each box, so they can be used
;		to extract the intersection from each box.
;
;	The function returns 1 if intersection exists, otherwise returns 0.
;
; PROCEDURE:
;	Check the cases for 1-D, then apply recursively for higher dimensions.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1993.
;	F.V. 1996, bug fix: changed one LE to LT and one GE to GT.
;-

function intersection, boxA, boxB, sectionA, sectionB

szA = size( boxA )
szB = size( boxB )

if (szA(0) NE szB(0)) then begin
	message,"check inputs: boxA, boxB must be same size",/INFO
	return,0
   endif

if max( abs( szA(0:szA(0)) - szB(0:szB(0)) ) ) GT 0 then begin
	message,"check inputs: boxA, boxB must be same size",/INFO
	return,0
   endif

if (szA(0) EQ 2) then begin

	edim = szA(2)
	vtyp = szA(3) > szB(3)
	sectionA = make_array( 2, edim, TYPE=vtyp )
	sectionB = make_array( 2, edim, TYPE=vtyp )

	for id = 0,edim-1 do begin

		if intersection( boxA(*,id), boxB(*,id), sexA, sexB ) then begin

			sectionA(0,id) = sexA
			sectionB(0,id) = sexB

		  endif else return,0
	  endfor

	return,1

  endif else if (szA(0) EQ 1) then begin

	if (boxA(0) GT boxA(1)) OR (boxB(0) GT boxB(1)) then begin
		sectionA = [0,0]
		sectionB = [0,0]
		message,"endpoints of boxes should be in increasing order",/INFO
		return,0
	   endif

	if (boxA(0) GE boxB(0)) then begin

		if (boxA(0) LT boxB(1)) then begin

			if (boxA(1) LE boxB(1)) then sex = boxA $
						else sex = [ boxA(0), boxB(1) ]
			sectionA = sex - boxA(0)
			sectionB = sex - boxB(0)
			return,1

		  endif else begin
			sectionA = [0,0]
			sectionB = [0,0]
			return,0
		   endelse

	  endif else if (boxA(1) GT boxB(0)) then begin

		if (boxA(1) GE boxB(1)) then sex = boxB $
					else sex = [ boxB(0), boxA(1) ]
		sectionA = sex - boxA(0)
		sectionB = sex - boxB(0)
		return,1

	  endif else begin
		sectionA = [0,0]
		sectionB = [0,0]
		return,0
	   endelse

  endif else begin

	message,"check inputs: boxA, boxB must be vectors or matrices",/INFO
	return,0
   endelse
end
