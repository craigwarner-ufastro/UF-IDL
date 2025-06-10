;+
; NAME:
;	Handles_Free
; PURPOSE:
;	Free a vector of handles, ignoring any invalid handles.
;
; CALLING:
;	Handles_Free, handles
;
; INPUTS:
;	handles = array of IDL handles (pointers to IDL variables).
;
; OUTPUTS:
;	None.
; PROCEDURE:
;	Loop over valid handles calling IDL instrinsic handle_free.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1995.
;-

pro Handles_Free, handles

	wh = where( handle_info( handles ), nwh )

	for i=0,nwh-1 do handle_free, handles(wh(i))
end
