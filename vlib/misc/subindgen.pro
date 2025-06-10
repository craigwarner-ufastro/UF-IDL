;+
; NAME:
;	subindgen
; PURPOSE:
;	Generate indices which map out a subarray in a larger 2D to 4D array.
;	Limitation is that subarray also starts at subscript zero.
; CALLING:
;	sub_indices = subindgen( array, DIMEN_SUB=dimsub )
; INPUTS:
;	array = 2D, 3D, or 4D array out of which a subarray is to be extracted.
; KEYWORDS:
;	DIMEN_SUB = array, dimensions of the desired subarray to be extracted.
; OUTPUTS:
;	Function returns an array of subscripts which will extract
;	desired subarray, so that extraction can be repeated.
; PROCEDURE:
;	Generate array of 0s and 1s and then user where function.
; HISTORY:
;	Written: Frank Varosi, RSTX @ NASA/GSFC, 1998.
;-

function subindgen, array, DIMEN_SUB=dimsub

	sz = size( array )
	zi = make_array( DIM=sz(1:sz(0)), /BYTE )

	if N_elements( dimsub ) ne sz(0) then begin
		message,"keyword DIMEN= incompatible with array",/INFO
		help,array,dimsub
		return,0
	   endif

	ds = ( dimsub < sz(1:sz(0)) ) - 1

	CASE sz(0) OF
		2:	zi(0:ds(0),0:ds(1)) = 1
		3:	zi(0:ds(0),0:ds(1),0:ds(2)) = 1
		4:	zi(0:ds(0),0:ds(1),0:ds(2),0:ds(3)) = 1
		else:	BEGIN
			print,sz(0)
			message,"dimensional case not yet considered",/INFO
			END
	 ENDCASE

return, where( zi )
end
