;+
; NAME:
;	Save_PXYZ
; PURPOSE:
;	Save the values of IDL system structures !P, !X, !Y, and !Z,
;	for restoring them later.
; CALLING:
;	Save_PXYZ
; INPUTS:
;	None.
; KEYWORDS:
;	/RESTORE
; OUTPUTS:
;	None.
; COMMON BLOCKS:
;	common Save_PXYZ, Psave, Xsave, Ysave, Zsave
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1998.
;-

pro Save_PXYZ, RESTORE=restore_pxyz

   common Save_PXYZ, Psave, Xsave, Ysave, Zsave

	if keyword_set( restore_pxyz ) then begin
		if N_struct( Psave ) eq 1 then !P = Psave
		if N_struct( Xsave ) eq 1 then !X = Xsave
		if N_struct( Ysave ) eq 1 then !Y = Ysave
		if N_struct( Zsave ) eq 1 then !Z = Zsave
	 endif else begin
		Psave = !P
		Xsave = !X
		Ysave = !Y
		Zsave = !Z
	  endelse
end
