pro LLsq1D, x, y, A_intercept, B_slope, sigma_A_B, covariance_A_B, $
					chi_sq, Y_SIGMA = y_sigma
;+
; NAME:
;	LLsq1D
; PURPOSE:
;	Linear Least-squares approximation in one-dimension (y = a + b*x).
; CALLING EXAMPLE:
;	LLsq1D, x, y, A, B, [ sigmas, covar, chi_sq, Y_SIGMA=sigmay ]
; INPUTS:
;	x = array of values for independent variable.
;	y = array of data values assumed to be linearly dependent on x.
; KEYWORDS:
;	Y_SIGMA = scalar or array specifying the standard deviation of y data,
;		if not given, assumed to be unity, and then
;		sqrt( chi_sq/Ndata ) can be an estimate of Y_SIGMA.
; OUTPUTS:
;	A_intercept = constant parameter result of linear fit,
;	B_slope = slope parameter, so that:
;			( A_intercept + B_slope * x ) approximates the y data.
; OPTIONAL OUTPUT:
;	sigma_A_B = two element array giving standard deviation of 
;			A_intercept and B_slope parameters, respectively.
;	covariance_A_B = co-variance of A_intercept and B_slope parameters.
;	chi_sq = resulting minimum Chi-Square of Linear fit.
; PROCEDURE:
;	Standard algorithm.
;	Uses transform to reduce roundoff errors (cf. Numerical Recipes).
; MODIFICATION HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;-
	Nx = N_elements( x )
	Ny = N_elements( y )

	if (Nx NE Ny) then begin
		message,"#x = "+strtrim(Nx,2)+" NE #y ="+strtrim(Ny,2),/INFO
		Nx = Nx < Ny
		print,"    using",Nx," # of data points"
	   endif

	if N_elements( y_sigma ) LE 0 then begin
		wy = 1
		wy2 = 1
		Sw = Nx
	 endif else if N_elements( y_sigma ) LT Nx then begin
		wy = 1./y_sigma(0)
		wy2 = wy^2
		Sw = Nx * wy2
	  endif else begin
		wy = 1./y_sigma(0:Nx-1)
		wy2 = wy^2
		Sw = total( wy2 )
	   endelse

	Sx = total( x * wy2 )
	Sy = total( y * wy2 )

	Tx = wy * ( x - Sx/Sw )		;Transform to reduce roundoff errors.
	Stt = total( Tx * Tx )

	B_slope = total( wy * y * Tx ) / Stt
	A_intercept = (Sy - B_slope * Sx) / Sw

	sigma_A_B = [ ( 1 + (Sx^2)/(Sw*Stt) )/Sw ,  1/Stt ]
	covariance_A_B = -Sx/(Sw*Stt)

	if N_params() GE 6 then begin
		chi_sq = total( wy * (y - (B_slope*x + A_intercept) )^2 )
		if N_elements( y_sigma ) LE 0 then y_sigma = sqrt( chi_sq/Nx )
	   endif
return
end
