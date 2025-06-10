;+
; NAME:
;	MSORT
; PURPOSE:
;	Return a matrix of subscripts which will sort into ascending order
;	the columns of a matrix.  Columns are the last index:  matrix(i,*).
; CALLING:  
;	order_subscripts = msort( matrix )
; or:
;	matrix = msort( matrix, /SORT_NOW )	;to perform the sort.
; INPUT:
;	matrix = 2-D matrix to be sorted.
; KEYWORDS:
;	/SORT_NOW : result is matrix with sorted columns.
;	/DESCENDING = descending order instead of the default ascending order.
; OUTPUT:
;	Subscripts which give sorted order are returned as function value,
;	or the actual sorted matrix if /SORT is specified.
; OPTIONAL OUTPUT:
;	order_column = order indices for each column seperately.
; METHOD:
;	When Nitem per column < 9 applies bubble-sort with gather/scatter
;	(using WHERE function), otherwise just loop over all columns.
; HISTORY:
;	Frank Varosi U.Md. 1988.
;	F.V. NASA/GSFC 1991, adapted to IDL-v.2
;	F.Varosi UF-astro 2015 :  cleaned up code (use [*] for indices)
;-

function msort, matrix, order_column, SORT_NOW=sort_now, DESCENDING=descend

	sm = size( matrix )

	if (sm[0] NE 2) then begin
		message,"expecting 2-D matrix for 1st arg.",/INFO
		return, sm
	   endif

	nitem = sm[2]

	if (nitem LT 9) then begin    ;faster to bubble-sort with gather/scatter

		oindex = make_array( DIM=sm[1:2],/INDEX,/LONG )

		for i = 0, nitem-2 do begin

		    oi = oindex[*,i]

		    for j = i+1, nitem-1 do begin

			oj = oindex[*,j]

			if keyword_set( descend ) then $
				w = where( matrix[oi] LT matrix[oj], nw ) $
			  else 	w = where( matrix[oi] GT matrix[oj], nw )

			if (nw GT 0) then begin    ;exchange values out of order
				ot = oi
				oi[w] = oj[w]
				oj[w] = ot[w]
				oindex[0,j] = oj
			   endif
		      endfor

		    oindex[0,i] = oi

		  endfor

		if N_params() GE 2 then order_column = oindex / sm[1]

		if keyword_set( sort_now ) then return, matrix( oindex ) $
					   else return, oindex

	  endif else begin	;in this case faster to Loop and call IDL sort.

		if keyword_set( sort_now ) then begin

			matout = matrix
			for i=0L,sm[1]-1 do matout[i,0] = matrix[ i, sort( matrix[i,*] ) ]
			return, matout

		  endif else begin

			oindex = Lonarr( sm[2], sm[1] )
			for i=0L,sm[1]-1 do  oindex[0,i] = sort( matrix[i,*] )

			oindex = transpose( oindex )
			if keyword_set( descend ) then oindex = rotate( oindex, 7 )
			if N_params() GE 2 then order_column = oindex

			oindex = oindex * sm[1]
			index = Lindgen( sm[1] )
			for k=0L,sm[2]-1 do  oindex[0,k] = oindex[*,k] + index
			return, oindex

		    endelse
	    endelse
end
