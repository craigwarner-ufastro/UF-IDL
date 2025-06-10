function Fsort, Array, Asort, INFO=info, DESCENDING=descend
;+
; NAME:
;	FSORT
; PURPOSE:
;	Function to sort data into ascending order,
;	original subscript order is maintained when values are equal (FIFO).
;	(unlike IDL sort routine alone,
;			which may rearrange the order of equal values)
; CALLING SEQUENCE:  
;	result = fsort( array, [asort] )
; INPUT:
;	Array - array to be sorted
; KEYWORDS:
;	/INFO = optional keyword to cause brief message about # equal values.
;	/DESCENDING = descending order instead of the default ascending order.
; OUTPUT:
;	Subscripts which give sorted order are returned as function value.
; OPTIONAL OUTPUT:
;	Asort - sorted array
; METHOD:
;	uses WHERE to find equal clumps, instead of looping with IF ( EQ ).
; HISTORY:
;	written by F. Varosi NASA/GSFC 1990.
;-
	N = N_elements( Array )
	if (N EQ 1) then return,[0]    ;Only 1 element

	if (N LE 0) then begin
		message,"expecting an array as input",/INFO
		retall
	   endif

	subs = sort( Array )
	Asort = Array(subs)

; now sort subscripts into ascending order
; whenever clumps of equality are found in Asort:

	weq = where( shift( Asort, -1 ) EQ Asort, Neq )

	if keyword_set( info ) then message, strtrim( Neq,2 ) + $
						" equal values Located",/INFO

	if (Neq GE N) then begin

		subs = Lindgen( N )
		Asort = Array

	 endif else if (Neq GT 0) then begin

		if (Neq GT 1) then begin	      ;find clumps of equality

			wclump = where( (shift( weq, -1 ) - weq) GT 1, Nclump )
			Nclump = Nclump + 1

		  endif else Nclump = 1

		if (Nclump LE 1) then begin
			Clump_Beg = 0
			Clump_End = Neq-1
		  endif else begin
			Clump_Beg = [0,wclump+1]
			Clump_End = [wclump,Neq-1]
		   endelse

		weq_Beg = weq( Clump_Beg )		;subscript ranges
		weq_End = weq( Clump_End ) + 1		; of Asort equalities.

		if keyword_set( info ) then message, strtrim( Nclump, 2 ) + $
				" clumps of equal values Located",/INFO

		for ic = 0L, Nclump-1 do begin		;sort each clump.

			subic = subs( weq_Beg(ic) : weq_End(ic) )
		        subs( weq_Beg(ic) ) = subic( sort( subic ) )
		  endfor

		if keyword_set( descend ) then subs = rotate( subs, 2 )
		if N_params() GE 2 then Asort = Array(subs)	;resort array.
	   endif

return, subs
end
