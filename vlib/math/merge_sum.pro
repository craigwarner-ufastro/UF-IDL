;+
; NAME:
;	Merge_Sum
; PURPOSE:
;	Compute the sum of two tabulated functions having different grids
;	for the independent variable. Function values are interpolated
;	onto a common grid created by merging the two different grids.
; CALLING:
;	Merge_Sum, x1, f1, x2, f2, xm, fm
; INPUTS:
;	x1, x2 = the two different independent variable grids for f1 & f2.
;	f1, f2 = the arrays of function values to be summed, defined on x1 & x2.
; OUTPUTS:
;	xm = grid created by unique merging the two grids x1 & x2.
;	fm = f1 + f1 on new grid xm.
; KEYWORDS:
;	/EXPONENTIAL_INTERP: perform interpolation/extrapolation in Log-Linear
;		space, thereby giving correct result for exponential function.
;
;	/POWER_LAW_INTERP: perform interpolation/extrapolation in Log-Log space,
;		thereby giving correct result for power-law function.
;
;	/EXTRAPOLATE: allow extrapolation of function values beyond given range,
;		default is NO extrapolation, assuming zeros out of range.
; EXTERNAL CALLS:
;	function finterpol
; PROCEDURE:
;
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1998.
;-

pro Merge_Sum, x1, f1, x2, f2, xm, fm, POWER_LAW_INTERP=ipow, $
					EXPONENTIAL_INTERP=iexp, EXTRAPOLATE=ext
	xm = [ x1, x2 ]
	xm = xm( unique( xm,/SORT ) )

	if keyword_set( ext ) then begin
		w1 = Lindgen( N_elements( xm ) )
		w2 = w1
	 endif else begin
		w1 = where( (xm GE min( x1, MAX=mx1 )) and (xm LE mx1 ) )
		w2 = where( (xm GE min( x2, MAX=mx2 )) and (xm LE mx2 ) )
	  endelse

	fi1 = finterpol( f1, x1, xm(w1), POW=ipow, EXP=iexp )
	fi2 = finterpol( f2, x2, xm(w2), POW=ipow, EXP=iexp )

	if keyword_set( ext ) then  fm = fi1 + fi2  else begin
		fm = fltarr( N_elements( xm ) )
		fm(w1) = fm(w1) + fi1
		fm(w2) = fm(w2) + fi2
	 endelse
end
