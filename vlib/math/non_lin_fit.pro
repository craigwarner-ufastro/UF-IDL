;+
; NAME:
;       Non_Lin_Fit
; PURPOSE:
;       Fit a non-linear function to 1-D data set,
;	by minimizing the Least-square deviation from data,
;	using the Conjugate Gradient method.
;       Function may be any non-linear function where
;       the partial derivatives are known or can be approximated.
; CALLING:
;       Non_Lin_Fit, Xi, Yd, Parms, sigmas, FUNC_NAME="name of function"
; INPUTS:
;       Xi = vector of independent variables.
;       Yd = vector of dependent variable to be fit with func_name( Xi ), 
;                                                 same length as Xi.
;       Parms = vector of nterms length containing the initial estimate
;              for each parameter.  If Parms is double precision, calculations
;              are performed in double precision, otherwise in single prec.
;              The initial guess of the parameter values should be as close to
;              the actual values as possible or the solution may not converge.
; KEYWORDS:
;       FUNC_NAME = function name (string)
;              Calling mechanism should be:  F = func_name( Xi, Parms, Pderiv )
;         where:
;              F = vector of NPOINT values of function.
;              Xi = vector of NPOINT independent variables, input.
;              Parms = vector of NPARM function parameters, input.
;              Pderiv = array, (NPOINT, NPARM), of partial derivatives.
;                     Pderiv(I,J) = derivative of function at ith point with
;                     respect to jth parameter.  Optional output parameter.
;                     Pderiv should not be calculated if parameter is not
;                     supplied in call (Unless you want to waste some time).
;       WEIGHTS = vector of weights, same length as x and y.
;              For equal (Gaussian) weighting w(i) = 1. (this is default),
;              instrumental (Poisson) weighting w(i) = 1./y(i), etc.
;      /INFO causes Chi-Sq. to be printed each iteration,
;              INFO > 1 causes current parameter estimates to also print.
;       MAX_ITER = maximum # of gradient search iterations, default=20.
;       TOLERANCE = ratio of Chi-Sq. change to previous Chi-Sq. at which
;                     to terminate iterations ( default = 1.e-4 = 0.1% ).
; OUTPUTS:
;       Parms = vector of parameters giving best fit to the data.
; OPTIONAL OUTPUT PARAMETERS:
;       sigmas = Vector of standard deviations for parameters Parms.
;       chisq = final Chi-Square deviation of fit.
;       Yfit = resulting best fit to data.
;	CoVariance = covariance matrix for fit params.
; PROCEDURE:
;       Uses method of function minimization via Conjugate Gradient method,
;	copied from Numerical Recipes.
; HISTORY:
;       Written, Frank Varosi, Univ. of Florida, 2007
;-

function ChiSqr_FuncFit, Parms, gradient

  common Non_Lin_Fit, Xin, Ydata, funcName, wghts

	Nparm = N_elements( Parms )
	Ndata = N_elements( Ydata )
	Nfree = Ndata - Nparm

	if N_params() GT 1 then begin

		Yfit = call_function( funcName, Xin, Parms, Pderiv )
		fdelta = Ydata - Yfit
		if Nparm eq 1 then Pderiv = reform( Pderiv, Ndata, 1 )
		gradient = reform( (2*wghts*fdelta) # Pderiv )/Nfree

;;message,/INFO,"gradient:"+strconcat( string(gradient,FORM="(G11.3)") )

	 endif else begin

		Yfit = call_function( funcName, Xin, Parms )
		fdelta = Ydata - Yfit
	  endelse

	chisq = total( wghts * fdelta^2 )/Nfree

return, chisq
end
;-----------------------------------------------------------------------------------

pro Non_Lin_Fit, Xi, Yd, Parms, sigmas, chisq, Yfit, CoVariance, WEIGHTS=Wts, $
		FUNC_NAME=func_name, MAX_ITER=Maxit, INFO_PRINT=info, TOLERANCE=tol, NITER=nitdone

  common Non_Lin_Fit, Xin, Ydata, funcName, wghts

	Nparm = N_ELEMENTS( Parms )			;# of params.
	Ndata = N_elements( Yd )
	Nfree = ( Ndata < N_ELEMENTS(Xi) ) - Nparm     ;degrees of freedom

	IF Nfree LE 0 THEN begin
		message,"not enough data points",/INFO
		nitdone = 0
		return
	   endif

	if N_elements( Maxit ) NE 1 then begin
		Maxit=20
		notify=1
	  endif else notify=0

	if N_elements( tol ) NE 1 then tol = 1.e-5
	if N_elements( func_name ) NE 1 then func_name = "FUNC"
	if N_elements( Wts ) LT Ndata then Wts = replicate( 1, Ndata )

	;;Setup common block for function ChiSqr (see above):
	funcName = func_name
	wghts = Wts
	Xin = Xi
	Ydata = Yd

	Parms = double( Parms )
	iter = 0

	minF_conj_grad, Parms, chisq, conv_factor, FUNC_NAME="ChiSqr_FuncFit",/INITIALIZE

	message,/INFO," iter =" + strtrim(iter,2) $
		+ ", chisq =" + string( chisq, FORM="(G9.4)") $
		+ ", cf =" + string( conv_factor, FORM="(G9.3)") $
		+ ", Parms:" + strconcat(string(Parms,FORM="(G9.3)"))

	while (conv_factor gt tol) and (iter LT Maxit) do begin

		minF_conj_grad, Parms, chisq, conv_factor, FUNC_NAME="ChiSqr_FuncFit"
		iter = iter+1

		if (iter LT 15) or (iter MOD 10 eq 0 ) then $
			message,/INFO," iter =" + strtrim(iter,2) $
				+ ", chisq =" + string( chisq, FORM="(G9.4)") $
				+ ", cf =" + string( conv_factor, FORM="(G9.3)") $
				+ ", Parms:" + strconcat(string(Parms,FORM="(G9.3)"))
	  endwhile

	message,/INFO," iter =" + strtrim(iter,2) $
		+ ", chisq =" + string( chisq, FORM="(G9.4)") $
		+ ", cf =" + string( conv_factor, FORM="(G9.3)") $
		+ ", Parms:" + strconcat(string(Parms,FORM="(G9.3)"))

	nitdone = iter

	Yfit = call_function( func_name, Xi, Parms )
	chisq = TOTAL( Wts * ( Yd - Yfit )^2 )/Nfree

	if N_elements( ARRAY ) gt 0 then begin
		sigmas = SQRT( ARRAY(DIAG) / ALPHA(DIAG) )		;return sigma's
		if N_params() GE 7 then CoVariance = ARRAY/ALPHA
	 endif else begin
		sigmas = fltarr( Nparm )
		sigmas[*] = chisq
	  endelse
END
