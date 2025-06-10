;+
; NAME:
;	RemIx
; PURPOSE:
;	Remove indices (determined previously) from an index array.
; CALLING:
;	sub_indices = RemIx( indices, REMOVE=iremov )
; INPUT:
;	indices = array of numbers.
; KEYWORDS:
;	REMOVE = array of numbers to remove from input indices.
;	NINDEX = generate index array of length NINDEX, if input not given.
;	/INFO : print informational messages.
; OUTPUTS:
;	Function returns a subset of input indices, with requested ones removed.
; EXTERNAL CALLS
;	pro match
; PROCEDURE:
;	Apply the match procedure to Locate remove candidates,
;	and return the others with negated where function.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1990.
;-

function RemIx, indices, REMOVE=iremov, NINDEX=Nindex, INFO=info

	if (N_elements( indices ) LE 1) then begin

		if (N_elements( Nindex ) NE 1) then begin

			message,"give array of indices or specify N=",/INFO
			return,(-1)

		  endif else indices = Lindgen( Nindex ) 
	   endif

	if N_elements( iremov ) LE 0 then begin
		message,"specify: REMOVE = <indices>",/INFO
		return,(-1)
	   endif

	match, indices, iremov, mixr

	w = where( mixr GE 0, Nrem )

	if (Nrem GT 0) then begin

		wkeep = Lindgen( N_elements( indices ) )
		wkeep(mixr) = -1
		wgood = where( wkeep GE 0, Ngood )

		if keyword_set( info ) then begin
			message,strtrim( Nrem, 2 )+" indices removed",/INFO
			message,strtrim( Ngood, 2 )+" returned",/INFO
		   endif

		if (Ngood LE 0) then return,(-1) $
				else return, indices( wkeep( wgood ) )

	  endif else if keyword_set( info ) then $
			message,"nothing removed",/INFO

return, indices
end
