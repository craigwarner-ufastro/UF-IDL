;+
; NAME:
;	Poisson_Curve
; PURPOSE:
;	Return the Poisson density function, evaluated from 0 to MAX_XV.
; CALLING:
;	density = Poisson_Curve( Lambda_mean, xv )
; INPUTS:
;	Lambda_mean = the mean value parameter of Poisson density.
; OUTPUTS:
;	xv = the array of x-values at which Poisson density is evaluated.
;	Function returns the Poisson density curve evaluated at xv.
; KEYWORDS:
;	MAX_XV = optional, max x-value at which to compute density,
;		default is about twice the mean value.
;	MAGNITUDES = optional, # orders of magnitude at which to cutoff the min.
; PROCEDURE:
;	Use Gamma function and Stirlings approximation.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-

function Poisson_Curve, Lambda_mean, xv, MAX_XV=maxv, MAGNITUDES=magnits

	if (Lambda_mean LE 0) then begin
		xv = [0]
		return,[0]
	   endif

	Lam = Lambda_mean
	sz = size( Lam )
	if sz(sz(0)+1) LT 4 then Lam = float( Lam )
	if N_elements( maxv ) NE 1 then maxv = fix( (5-((Lam/7.)<3)) * Lam )

	pd = findgen( maxv + 1 )
	xv = findgen( maxv + 1 )

	if (Lam LT 86) then begin

		pd(0) = exp( -Lam )
		for x=1,maxv do pd(x) = ( Lam/x ) * pd(x-1)

	 endif else begin

		Nmax = fix( 37/aLog10( Lam>10 ) ) < 34
		wl = where( xv LT Nmax )
		pd(wl) = exp( xv(wl)*aLog( Lam ) - Lam )/gamma( xv(wl)+1 )
		wg = where( xv GE Nmax, ng )

		if (ng GT 0) then begin
			xg = xv(wg)
			cf = (1 - 1/( 12*xg ) ) * (1 + 1/( 167.5*xg^2.2 ) ) $
							/sqrt( 2 * !PI * Lam )
			pd(wg) = cf * exp( (xg+.5)*aLog( Lam/xg ) + xg - Lam )
		   endif
	  endelse

	if keyword_set( magnits ) then begin

		w = where( pd GT max( pd ) * 10.^(-magnits), nw )

		if (nw LE 0) then  return,pd  else begin
			xv = xv(w)
			return, pd(w)
		   endelse

	   endif else return, pd
end
