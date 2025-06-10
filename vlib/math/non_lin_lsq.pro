;+
; NAME:
;       Non_Lin_Lsq
; PURPOSE:
;       Non-linear least squares fit of a function of an
;       arbitrary number of parameters, to a data array or any dimensionality.
;       Function may be any non-linear function, and can even be
;	a function of multi-dimensional independent variable, e.g. f(x,y) or f(x,y,z).
;       The partial derivatives must be known or can be approximated.
;	Calculations are always performed in double precision.
; CALLING:
;       Non_Lin_Lsq, Xi, dataVals, Parms, sigmas, chisq, funcVals, FUNC_NAME="name of function"
; INPUTS:
;       Xi = array of independent variables. Can be of any dimensionality.
;       dataVals = array of dependent variable to be fit with func_name( Xi ), 
;
;       Parms = vector containing the initial guess for each parameter.
;		Parms is always converted to double precision,
;		and calculations are always performed in double precision.
;              The initial guess of the parameter values should be as close to
;              the actual values as possible or the solution may not converge.
; KEYWORDS:
;       FUNC_NAME = function name (string)
;              Calling mechanism should be:  funcVals = func_name( Xi, Parms, Pderiv )
;         where:
;              funcVals = array of NPOINT values of function.
;              Xi = array of independent variables to generate funcVals, input.
;              Parms = vector of NPARM function parameters, input.
;              Pderiv = output array, (NPOINT, NPARM), of partial derivatives.
;                     Pderiv(i,j) = derivative of function at i-th point with
;                     respect to j-th parameter.  Optional output parameter.
;                     Pderiv should not be calculated if argument is not
;                     supplied in call (Unless you want to waste some time).
;       WEIGHTS = array of weights, same length as x and y.
;              For equal (Gaussian) weighting w(i) = 1. (this is default),
;              instrumental (Poisson) weighting w(i) = 1./y(i), etc.
;      /INFO causes Chi-Sq. to be printed each iteration,
;              INFO > 1 causes current parameter estimates to also print.
;       MAX_ITER = maximum # of gradient search iterations, default=50.
;       TOLERANCE = ratio of Chi-Sq. change to previous Chi-Sq. at which
;                     to terminate iterations ( default = 1.e-3 = 0.1% ).
;
;	PAR_FIX = optional indices of which parameters to keep fixed.
;	PAR_MIN = optional vector of minimum allowed values for parameters.
;	PAR_MAX = optional vector of maximum allowed values for parameters.
; OUTPUTS:
;       Parms = vector of parameters giving best fit to the data.
; OPTIONAL OUTPUT PARAMETERS:
;       sigmas = Vector of standard deviations for parameters Parms.
;       chisq = final Chi-Square deviation of fit.
;       funcVals = resulting best fit to data.
;	CoVariance = covariance matrix for fit params.
; PROCEDURE:
;       Copied from "CURFIT", least squares fit to a non-linear function, pages 237-239
;	of Bevington, Data Reduction and Error Analysis for the Physical Sciences,
;	which is based on the Levenberg-Marquadt method.
;       "This method is the Gradient-expansion algorithm which
;       combines the best features of the gradient search with
;       the method of linearizing the fitting function."
; HISTORY:
;       Written, David M. Stern, RSI, September, 1982.
;       Modified, Frank Varosi, NASA/GSFC, 1992, to use call_function, and
;	    added keywords: WEIGHTS, FUNC_NAME, MAX_ITER, INFO_PRINT, TOLERANCE.
;	Mod, F.V. 1998: reform Pderiv array to (Ndata,1) if Nparm=1.
;	Mod, F.V. 2007 at UF: added keyword options PMIN, PMAX, PFIX.
;	Mod, F.V. 2012 : improved inner Lambda iteration Loop (see comments about Lambda).
;	Mod, F.V. 2015 : fixed stupid mistake now keeping params > Pmin if specified.
;	Mod, F.V. 2016 : default Lambda change factor now 2 instead of 4.
;	Mod, F.V. 2016 : fixed kinda bug: if # param Limits >= # params use them anyway.
;-

pro Non_Lin_Lsq, Xin, dataVals, Parms, sigmas, chisq, funcVals, CoVariance, WEIGHTS=dWts, $
                 FUNC_NAME=func_name, MAX_ITER=Maxit, MAXTRY=maxTry, INFO_PRINT=info,NITER=nitdone,$
                 PAR_MINS=Pmin, PAR_MAXS=Pmax, PAR_FIX=Pfix, TOLERANCE=ftol, POISSON=wPois

	Nparm = N_elements( Parms )	;# of params.
	Npmin = N_elements( Pmin )
	Npmax = N_elements( Pmax )
	Npfix = N_elements( Pfix )	;# of params. to keep fixed

	if Npfix gt 0 then begin
		for ip = 0, Nparm-1 do begin
			w = where( Pfix eq ip, nw )
			if( nw eq 0 ) then begin
			  if N_elements( wpfit ) eq 0 then wpfit = [ip] else wpfit = [wpfit,ip]
			 endif
		  endfor
	   endif else wpfit = indgen(Nparm)

	Nparfit = N_elements( wpfit )	;# of params. to actually vary for fit.

	if Nparfit eq 0 then begin
		message,"why did you fix all parameters ?! ("+func_name+")",/INFO
		help,Pfix
		print,Pfix,Parms
                chisq = 1.0
                nitdone = 1
		return
	   endif

	Ndata = N_elements( dataVals )
	Nfree = Ndata - Nparfit     ;degrees of freedom

        IF Nfree LE 0 THEN begin
           if Nfree LT 0 then begin
              message,strtrim(Ndata,2)+" are not enough data points ("+func_name+")",/INFO
              return
           endif
           message,strtrim(Ndata,2)+" data points give zero degrees of freedom ("+func_name+")",/INFO
        endif

	if N_elements( Maxit ) NE 1 then begin
		Maxit=100
		notify=1
	  endif else notify=0

	;;max tries to reduce chi-sq by Lambda diagonal amplification in REPEAT loop.
	if N_elements( maxTry ) ne 1 then maxTry = 9
	if N_elements( ftol ) NE 1 then ftol = 1.e-3
	if N_elements( func_name ) NE 1 then func_name = "FUNC"

	if N_elements( dWts ) LT Ndata then begin
		;; if data weights are not given then use either uniform weighting,
		;; or Poisson weighting (1/data) with min threshold:
		if keyword_set( wPois ) then dWts = 1.0/( dataVals > wPois ) $
		else dWts = replicate( 1.0, Ndata )
	   endif

	Parms = double( Parms )	;make sure params are double prec.
        if( Npmin ge Nparm ) then Parms = Parms > Pmin
        if( Npmax ge Nparm ) then Parms = Parms < Pmax
	Pnew = Parms
	nitdone = 0
	;; make sure weights are used as a vector, in case data supplied as multi-D array:
	dWts = reform( dWts, Ndata )
        funcVals = call_function( func_name, Xin, Parms )
        chisq = TOTAL( dWts * reform( dataVals - funcVals, Ndata )^2 )/(Nfree>1)
        if N_elements( Lambfac ) ne 1 then Lambfac = 10
        Lambda = 0.001

	if keyword_set( info ) then begin
           message,/INFO,"# degrees of freedom = " + strtrim(Nfree,2)
           message,/INFO,"iter =" + strtrim(nitdone,2) $
                   + ", ntry =" + strtrim(0,2) $
                   + ", chisq =" + string( chisq, FORM="(G11.5)") $
                   + ", Lambda =" + string( Lambda, FORM="(G9.4)") $
                   + ", Parms:" + strconcat( string(Parms,FORM="(G10.3)") )
        endif

	dWtsMatrix = dWts # replicate( 1.0, Nparfit )
	wdiag = INDGEN( Nparfit )*(Nparfit+1)	;subscripts of diagonal elements
        chisqhv = fltarr( maxit+1 )
        chisqhv[0] = chisq

        FOR iter = 1, Maxit DO BEGIN ;main iteration Loop

		funcVals = call_function( func_name, Xin, Parms, Pderiv )

		if( Nparfit LT Nparm ) then begin
			Pdfit = dblarr( Ndata, Nparfit )
			for i=0,Nparfit-1 do Pdfit[*,i] = Pderiv[*,wpfit[i]]
			Pderiv = Pdfit
		   endif

		if Nparfit eq 1 then Pderiv = reform( Pderiv, Ndata, 1 )

		fdiff = reform( dataVals - funcVals, Ndata )
		BETA = (fdiff*dWts) # Pderiv
		ALPHA = TRANSPOSE( Pderiv ) # (dWtsMatrix * Pderiv)
		Cdiag = SQRT( ALPHA(wdiag) ) > 1e-45
		Cdiag = Cdiag # Cdiag
		chisq1 = TOTAL( dWts * fdiff^2 )/(Nfree>1)
                chisq = chisq1
                Pnew = Parms
; divide by Cdiag to scale values around unity for good inversion,
;  then after invert() must again divide by Cdiag to rescale back:
                curvMatrix = ALPHA/Cdiag   
                curvMatLam = curvMatrix
		ntry=0

; Lamda iteration Loop.
; invert modified curvature matrix to generate new parameters:

		REPEAT BEGIN
                   Psav = Pnew
                   curvMatLam[wdiag] = curvMatrix[wdiag] * (1 + Lambda)
                   Pnew[wpfit] = Parms[wpfit] + INVERT( curvMatLam )/Cdiag # TRANSPOSE( BETA )

                   if( Npmin ge Nparm ) then Pnew = Pnew > Pmin
                   if( Npmax ge Nparm ) then Pnew = Pnew < Pmax

                   funcVals = call_function( func_name, Xin, Pnew )
                   chisqp = chisq
                   chisq = TOTAL( dWts * reform( dataVals - funcVals, Ndata )^2 )/(Nfree>1)
                   ntry = ntry+1

                   if keyword_set( info ) then begin
                      if( info gt 1 ) then begin
                         message,/INFO,"iter =" + strtrim(iter,2) $
                                 + ", ntry =" + strtrim(ntry,2) $
                                 + ", chisq =" + string( chisq, FORM="(G11.5)") $
                                 + ", Lambda =" + string( Lambda, FORM="(G9.4)") $
                                 + ", Parms:" + strconcat( string(Pnew,FORM="(G10.3)") )
                      endif
                   endif

                   Lambda = (Lambda * Lambfac) < 1e11 ;;increase Lambda in case fit got worse

                   if NOT finite( chisq ) then begin
                      message,/INFO,"why is chi-sq. not finite ?"
                      if !DEBUG then stop
                      goto,DONE
                   endif

                ENDREP UNTIL (chisq LT chisq1) OR (ntry GT maxTry); or (chisq gt chisqp)

;; Decrease Lambda by factor,
;;  and if Lambda iteration loop failed to improved chisq go back to previous parameters:

                Lambda = (Lambda / Lambfac) > 1e-3
            ;    if( chisq gt chisqp ) then Pnew = Psav
		if( chisq LE chisq1 ) then Parms = Pnew	;use new parameter estimate.
                chisqhv[iter] = chisq
		if (chisq EQ 0) then goto,DONE
		if (abs( chisq1 - chisq )/chisq1 LE ftol) then goto,DONE
	  ENDFOR

	iter = iter-1
        if keyword_set( info ) then message,"Failed to converge ("+func_name+")",/INFO

DONE:	nitdone = iter

	funcVals = call_function( func_name, Xin, Parms )
	chisq = TOTAL( dWts * reform( dataVals - funcVals, Ndata )^2 )/(Nfree>0.1)
	sigmas = fltarr( Nparm )

	if N_elements( curvMatrix ) gt 0 then begin
		sigmas[wpfit] = SQRT( curvMatrix(wdiag) / ALPHA(wdiag) )	;return sigma's
		if N_params() GE 7 then CoVariance = curvMatrix/ALPHA
	   endif

	if keyword_set( info ) then $
		message,/INFO,"iter =" + strtrim(iter,2) $
			+ ",          chisq =" + string( chisq, FORM="(G11.5)") $
                        + ",                    Parms:" + strconcat(string(Parms,FORM="(G10.3)"))
END
