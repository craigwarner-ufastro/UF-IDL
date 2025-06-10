;+
; NAME:
;	frac_pix_shift
; PURPOSE:
;	Shift the image by fraction of a pixel in x and/or y directions
;	using the bilinear interp feature of intrinsic IDL routine poly_2D.
;	Minimum shift is one hundreth of a pixel, anything less is ignored.
;	Best to keep abs( x & y shifts ) < 0.5, otherwise poly_2d extrapolates.
; CALLING:
;	imshift = frac_pix_shift( image, x_shift, y_shift )
; INPUTS:
;	image, x_shift, y_shift
; KEYWORDS:
;	SHIFT_XY = optional method to specify the shifts as 2 element array.
;	/RENORMALIZE : shifted image is renormalized to conserve positive flux.
; OUTPUTS:
;	Returns the image shifted using bilinear interpolation.
; PROCEDURE:
;	Create 2D poly. shifting coefficients and apply bilinear interpolation of poly_2D.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;	Frank Varosi UF-astro 2015 : reduced smallest shift threshold to 0.01 from 0.05.
;-

function frac_pix_shift, image, x_shift, y_shift, SHIFT_XY=shift_xy, RENORMALIZE=renorm

  if N_elements( shift_xy ) EQ 2 then begin
     x_shift = shift_xy[0]
     y_shift = shift_xy[1]
  endif

  if (abs( x_shift ) LT 0.01) AND (abs( y_shift ) LT 0.01) then return,image

  cx = [ [-x_shift,0], [1,0] ]
  cy = [ [-y_shift,1], [0,0] ]

  if keyword_set( renorm ) then begin

     imshift = poly_2D( image, cx, cy, 1 )
     return, imshift * ( total( image>0 )/total( imshift>0 ) )

  endif else return, poly_2D( image, cx, cy, 1 )
end
