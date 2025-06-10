;+
; NAME:
;	psport
; PURPOSE:
;	Set device to PostScript portrait mode
;	and save previous device name and font in common.
;	User should call pro psclose to close PS device and reset device name and font.
; CALLING EXAMPLE:
;	psport, FILE="plot.ps"
; KEYWORDS:
;	FILE = PostScript output file, default = "idl.ps"
;	FONT = desired font, otherwise default is 0 (zero) to use hardware font.
;	/SQUARE : make graphics occupy 7 by 7 inch square.
;	/LONG : make graphics occupy 7 by 9 inch rectangle.
;	/ENCAPS : to make encapsulated PS file.
;	/VERBOSE : print filename to terminal.
;	/COLOR : color mode
;	BITS = bits per pixel for color or grayscale.
;
; COMMON BLOCKS:
;	common psclose, dnamsav, pfont
; EXTERNALS:
;	function VarType
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;	Modified, F.V., UF, 2006: added keywords FONT=, and /ENCAP.
;	Modified, F.V., UF, 2018: added keyword option /COLOR
;	Modified, F.V., UF, 2020: added keyword VERBOSE to print filename.
;	Modified, F.V., UF, 2022: added keyword BITS (device bits per pixel).
;-

pro psport, FILE=file, SQUARE=square, LONG=long, FONT=font, $
            COLOR=cmode, BITS=bits, ENCAPS=encaps, VERBOSE=verbose

   common psclose, dnamsav, pfont

	if !D.name NE "PS" then begin
		dnamsav = !D.name
		pfont = !P.font
		if keyword_set( font ) then  !P.font = font  else  !P.font = 0
		set_plot,'ps'
		if keyword_set( encaps ) then device,/ENCAP
	   endif

	device,/PORTRAIT

        if VarType( file ) EQ "STRING" then begin
           device,FILE=file
           if keyword_set( verbose ) then message,/INFO,"printing to file: "+ file
        endif
        
	if keyword_set( square ) then device, XSIZ=8, XOFF=0.4, YSIZ=7, YOFF=1.5, /INCH
	if keyword_set( long ) then device, XSIZ=8, XOFF=0.4, YSIZ=10, YOFF=0.4, /INCH
	if keyword_set( cmode ) then device, /COLOR
	if keyword_set( bits ) then device, BITS=bits
end
