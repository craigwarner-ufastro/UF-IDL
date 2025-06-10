;+
; NAME:
;	psclose
; PURPOSE:
;	Close the PostScript device output and reset device and font. 
; CALLING:
;	psclose
; INPUTS:	none
; OUTPUTS:	none
; COMMON BLOCKS:
;	common psclose, dnamsav, pfont
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-

pro psclose

   common psclose, dnamsav, pfont

	if !D.name EQ "PS" then begin
		device,/CLOSE
		device,ENCAP=0
		device, Xsiz=7, Ysiz=5, Xoff=0.75, Yoff=5, /INCH
	   endif

	if N_elements( dnamsav ) EQ 1 then set_plot,dnamsav
	if N_elements( pfont ) EQ 1 then !P.font = pfont
end
