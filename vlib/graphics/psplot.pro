;+
; NAME:
;	psplot
; PURPOSE:
;	Set device to PostScript portrait mode
;	and save previous device name and font in common.
;	User should call pro psclose to close PS device and reset device name and font.
; CALLING EXAMPLE:
;	psplot, FILE="plot.ps"
; KEYWORDS:
;	/LANDSCAPE : use Landscape mode, default is portrait.
;	FILE = PostScript output file, default = "idl.ps"
;	FONT = desired font, otherwise default is 0 (zero) to use hardware font.
;	/SQUARE : make graphics occupy 7 by 7 inch square.
;	/LONG : make graphics occupy 7 by 9 inch rectangle.
;	/WIDE : make plot a bit bigger (for Landscape mode 10.3" by 9.8")
;	/ENCAPS : to make encapsulated PS file.
;	/VERBOSE : print filename to terminal.
;	/COLOR : color mode
;	BITS = bits per pixel for color or grayscale.
; EXTERNALS:
;	pro psport
;	pro psLand
; HISTORY:
;	Written, Frank Varosi @ UFastro, 2022.
;-

pro psplot, FILE=file, SQUARE=square, LONG=long, FONT=font, WIDE=wide, $
            COLOR=cmode, BITS=bits, ENCAPS=encaps, VERBOSE=verbose, LANDSCAPE=Landscape

  if keyword_set( Landscape ) then begin

     psLand, FILE=file, FONT=font, WIDE=wide, $
             COLOR=cmode, BITS=bits, ENCAPS=encaps, VERBOSE=verbose

  endif else begin

     psport, FILE=file, SQUARE=square, LONG=long, FONT=font, $
             COLOR=cmode, BITS=bits, ENCAPS=encaps, VERBOSE=verbose
  endelse

end
