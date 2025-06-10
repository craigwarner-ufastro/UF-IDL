;+
; NAME:
;	Local_Variance
; PURPOSE:
;	Compute Local variances of data array (spectrum, image) in a moving box,
;	of width specified by keyword BOX_WIDTH.
; CALLING:
;	Locvar = Local_Variance( data )
; INPUT:
;	data = array of numbers (vector, image, etc.)
; KEYWORDS:
;	BOX_WIDTH = the width in pixels of moving box
;	            in which to compute local variances, default=3.
;
;	/MEDIAN : use median filter to get "mean val" in a moving box.
;
; OUTPUTS:
;	Function returns the corresponding array of local variances.
; PROCEDURE:
;	Call the IDL smooth function twice:
;	compute deviations from local mean values (from smooth),
;	then sum the squares of deviations using smooth function.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;	Mod FV at UF 2008: convert local deviations to float before squaring.
;	Mod FV at UF 2012: added keyword TRIM to discard edges to improve accuracy.
;-

function Local_Variance, data, BOX_WIDTH=boxwid, MEDIAN=usemed, TRIM=trim

	szd = size( data )

	if szd[0] LE 0 then begin
		print,"syntax:  Locvar = Local_Variance( data, BOX_WIDTH= )"
		print," data must be an array"
		return,0
	   endif

	dtype = szd[szd[0]+1]

	if (dtype LE 3) or (dtype eq 12) or (dtype eq 13) then $
		return, Local_Variance( float( data ), BOX=boxwid, MED=usemed )

	if N_elements( boxwid ) NE 1 then boxwid=3
	boxwid = ( 2 * (fix( boxwid )/2) + 1 ) > 3
	bw2 = boxwid^szd[0]
	fact = float( bw2 )/(bw2-1)

	if keyword_set( usemed ) then dm = median( data, boxwid ) $
				else dm = smooth( data, boxwid )

	if keyword_set( trim ) then begin
           nx = szd[1]
           ny = szd[2]
           trim = boxwid/2
           dm = data[trim:nx-trim-1,trim:ny-trim-1] - dm[trim:nx-trim-1,trim:ny-trim-1]
           dvar =  smooth( dm*dm, boxwid )
           szv = size( dvar )
           nx = szv[1]
           ny = szv[2]
           return, dvar[trim:nx-trim-1,trim:ny-trim-1] * fact
        endif else begin
           dm = data - dm
           return, smooth( dm*dm, boxwid ) * fact
        endelse

end
