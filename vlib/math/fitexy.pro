function chisq_fitexy, B_angle

; NAME:
;	chisq_fitexy
; PURPOSE:
;	Function minimized by fitexy  (computes chi-square of linear fit).
;	It is called by minimization procedures during execution of fitexy.
; CALLING:
;		chisq = chisq_fitexy( B_angle )
; INPUTS:
;	B_angle = arc-tangent of B_slope of linear fit.
; OUTPUTS:
;	Result of function = chi_square - offs  (offs is in COMMON).
; COMMON:
;	common fitexy, communicates the data from pro fitexy.
; PROCEDURE:
;	From "Numerical Recipes" column: Computer in Physics Vol.6 No.3
; MODIFICATION HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.

  common fitexy, xx, yy, sigx, sigy, ww, Ai, offs

	B_slope = tan( B_angle )
	ww = 1/( ( (B_slope * sigx)^2 + sigy^2 ) > 1.e-30 )
	if N_elements( ww ) EQ 1 then sumw = ww * N_elements( xx ) $
				 else sumw = total( ww )
	y_Bx = yy - B_slope * xx
	Ai = total( ww * y_Bx )/sumw

return, total( ww * (y_Bx - Ai)^2 ) - offs
end
;-------------------------------------------------------------------------------
pro fitexy, x, y, A_intercept, B_slope, sigma_A_B, chi_sq, TOLERANCE=Tol, $
					X_SIGMA=x_sigma, Y_SIGMA=y_sigma
;+
; NAME:
;	fitexy
; PURPOSE:
;	Linear Least-squares approximation in one-dimension (y = a + b*x),
;		when both x and y data have errors (e.g. Gaussian noise).
; CALLING EXAMPLE:
;	fitexy, x, y, A, B, X_SIG=sigx, Y_SIG=sigy, [sigmas, covar, chi_sq]
; INPUTS:
;	x = array of values for independent variable.
;	y = array of data values assumed to be linearly dependent on x.
; KEYWORDS:
;	X_SIGMA = scalar or array specifying the standard deviation of x data.
;	Y_SIGMA = scalar or array specifying the standard deviation of y data.
;	TOLERANCE = desired accuracy of minimum & zero location, default=1.e-3.
; OUTPUTS:
;	A_intercept = constant parameter result of linear fit,
;	B_slope = slope parameter, so that:
;			( A_intercept + B_slope * x ) approximates the y data.
; OPTIONAL OUTPUT:
;	sigma_A_B = two element array giving standard deviation of 
;			A_intercept and B_slope parameters, respectively.
;	chi_sq = resulting minimum Chi-Square of Linear fit.
; COMMON:
;	common fitexy, communicates the data for computation of chi-square.
; CALLS:
;	function chisq_fitexy
;	pro minf_bracket
;	pro minf_parabolic
;	function zbrent
; PROCEDURE:
;	From "Numerical Recipes" column: Computer in Physics Vol.6 No.3
; MODIFICATION HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-
  common fitexy, xx, yy, sigx, sigy, ww, Ai, offs

	scale = sqrt( stdev( x )/stdev( y ) )
	xx = x
	yy = y * scale
	sigx = x_sigma
	sigy = y_sigma * scale

;Compute first guess for B_slope using standard 1-D Linear Least-squares fit,
; where the non-linear term involving errors in x are ignored.
; (note that Tx is a transform to reduce roundoff errors)

	ww = sigx^2 + sigy^2
	if N_elements( ww ) EQ 1 then sumw = ww * N_elements( xx ) $
				 else sumw = total( ww )
	Sx = total( xx * ww )
	Tx = x - Sx/sumw
	B = total( ww * yy * Tx ) / total( ww * Tx^2 )

;Find the minimum chi-sq while including the non-linear term (B * sigx)^2
; involving variance in x data (computed by function chisq_fitexy):

	offs = 0
	ang = [ 0, atan( B ), 1.57 ]
	chi = fltarr( 3 )
	for j=0,2 do chi(j) = chisq_fitexy( ang(j) )	;this is for later...

	if N_elements( Tol ) NE 1 then Tol=1.e-3
	a0 = ang(0)
	a1 = ang(1)
	minf_bracket, a0,a1,a2, c0,c1,c2, FUNC="chisq_fitexy"
	minf_parabolic, a0,a1,a2, Bang, chi_sq, FUNC="chisq_fitexy", TOL=Tol

	A_intercept = Ai	;computed in function chisq_fitexy
	ang = [a0,a1,a2,ang]
	chi = [c0,c1,c2,chi]

;Now compute the variances of estimated parameters,
; by finding roots of ( (chi_sq + 1) - chisq_fitexy ).
;Note: ww, Ai are computed in function chisq_fitexy.

	offs = chi_sq + 1
	wc = where( chi GT offs, nc )

	if (nc GT 0) then begin

		angw = [ang(wc)]
		d1 = abs( angw - Bang ) MOD !PI
		d2 = !PI - d1
		wa = where( angw LT Bang, na )

		if (na GT 0) then begin
			d = d1(wa)
			d1(wa) = d2(wa)
			d2(wa) = B
		   endif

		Bmax = zbrent( Bang,Bang+max(d1),F="chisq_fitexy",T=Tol ) -Bang
		Amax = Ai - A_intercept
		Bmin = zbrent( Bang,Bang-min(d2),F="chisq_fitexy",T=Tol ) -Bang
		Amin = Ai - A_intercept

		if N_elements( ww ) EQ 1 then r2 = 2/( ww * N_elements( x ) ) $
					 else r2 = 2/total( ww )

	  	sigma_A_B = [ Amin^2 + Amax^2 + r2 , Bmin^2 + Bmax^2 ]
	  	sigma_A_B = sqrt( sigma_A_B/2 ) / (scale*[1,cos(Bang)^2])

	  endif else sigma_A_B = [1.e33,1.e33]

;Finally, transform parameters back to orignal units.

	A_intercept = A_intercept/scale
	B_slope = tan( Bang )/scale
return
end
