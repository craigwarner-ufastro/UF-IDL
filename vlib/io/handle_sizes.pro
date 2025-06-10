;+
; NAME:
;	Handle_Sizes
; PURPOSE:
;	Get the IDL sizes of the values pointed to by an array of handles.
; CALLING:
;	sizes = Handle_Sizes( handles )
; INPUTS:
;	handles = array of IDL handles (pointers to IDL variables).
; OUTPUTS:
;	Function returns matrix of sizes (results of IDL size function),
;	each row (i) is the size( value stored in handle (i) ).
;	If any handle is not valid (undefined) the row is zero.
; PROCEDURE:
;	Get and set each handle value with /NO_COPY in loop of size checks.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1995.
;-

function Handle_Sizes, handles, VERBOSE=verbose

	nh = N_elements( handles )
	if (nh LE 0) then return,0

	wh = where( handle_info( handles ), nwh )

	if (nwh LE 0) then begin
		if keyword_set( verbose ) then message,"no valid handles",/INFO
		return, Lonarr( 3, nh )
	   endif

	vsizes = Lonarr( 11, nh )

	for i=0,nwh-1 do begin
		j = wh(i)
		handle_value, handles(j), value,/NO_COPY
		vsizes(0,j) = size( value )
		handle_value, handles(j), value,/NO_COPY,/SET
	  endfor

	maxdim = max( vsizes(0,*) )
	if (maxdim LT 8) then return,vsizes( 0:maxdim+2, * ) else return,vsizes
end
