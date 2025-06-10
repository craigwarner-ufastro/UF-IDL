pro color_trans, transform
;+
; NAME:
;	color_trans
;
; PURPOSE:
;	Apply linear transform (matrix) to current RGB color table
;	and Load result (with tvlct).
;
; CALLING:
;	color_trans, transform
;
; INPUTS:
;	transform = square matrix, size N_colors-1 by N_colors-1 elements.
;
; EFFECTS:
;	Color tables are updated.
;
; COMMON BLOCKS:
;	common colors, ro,go,bo, rc,gc,bc
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1990.
;-
   common colors, ro,go,bo, rc,gc,bc

	cols = ( transform # [[rc],[gc],[bc]] ) < 255
	tvlct, cols(*,0), cols(*,1), cols(*,2)
return
end
