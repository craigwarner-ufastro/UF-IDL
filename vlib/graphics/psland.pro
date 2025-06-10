;+
; NAME:
;	psland
; PURPOSE:
;	Set device to PostScript landscape mode
;	and save previous device and font in common.
;	Call pro psclose to close PS device and reset device name and font.
; CALLING:
;	psland, FILE="plot.ps"
; KEYWORDS:
;	FILE = string, name of file to which PostScript should be written.
;	FONT = desired font, otherwise default is 0 (zero) to use hardware font.
;	/ENCAPS : to make encapsulated PS file.
;	/WIDE : make plot a bit wider (10.3" by 9.8" instead of	10" by 9.5")
;	/VERBOSE : print filename to terminal.
;	/COLOR : color mode
;	BITS = bits per pixel for color or grayscale.
;
; COMMON BLOCKS:
;	common psclose, dnamsav, pfont
; EXTERNALS:
;	function VarType
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;	Modified, F.V., UF, 2006: added keywords FONT=, and /ENCAP.
;	Modified, F.V., UF, 2018: added keyword option /WIDE and /COLOR
;	Modified, F.V., UF, 2020: added keyword VERBOSE to print filename.
;	Modified, F.V., UF, 2022: added keyword BITS (device bits per pixel).
;-

pro psland, FILE=file, FONT=font, WIDE=wide, COLOR=cmode, BITS=bits, $
            VERBOSE=verbose, ENCAPS=encaps

   common psclose, Dnamsav, Pfont

	if !D.name NE "PS" then begin
		dnamsav = !D.name
		Pfont = !P.font
		if keyword_set( font ) then  !P.font = font  else  !P.font = 0
		set_plot,'ps'
		if keyword_set( encaps ) then device,/ENCAP
	   endif

        xsize = 9.5
        yoff = 10
	ysize = 7.0

        if keyword_set( wide ) then begin
           xsize = 9.8
           yoff = 10.3
	   ysize = 7.3
        endif

	device, /LANDSCAPE, XOFF=0.7, YOFF=yoff, XSIZ=xsize, YSIZ=ysize, /INCH

        if VarType( file ) EQ "STRING" then begin
           device,FILE=file
           if keyword_set( verbose ) then message,/INFO,"printing to file: "+ file
        endif
        
	if keyword_set( cmode ) then device, /COLOR
	if keyword_set( bits ) then device, BITS=bits
end
