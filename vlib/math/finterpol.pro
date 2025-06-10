;+
; NAME:
;	FINTERPOL
; PURPOSE:
;	Linear interpolation of a table of function values onto a new grid.
;	Finterpol is faster than the old IDL lib routine interpol
;	when the arrays (xold & xnew) have more than about 10 elements,
;	and speedup factor increases to > 15 when number of elements > 100.
;	This is acheived by first calling function InterLeave( xold, xnew ),
;	which then allows subsequent computations to be vectorized.
;	Intermediate computations are stored in a common block so that
;	after the first call, further interpolations of different functions
;	evaluated on the same grids can be performed even faster (10 X)
;	by omitting the xold & xnew arguments in further calls. The array
;	of points at which function is known must be strictly increasing,
;	but the new interpolation points can be in any order. If the new
;	grid points are beyond the range of old points extrapolation is
;	performed, and a message is printed (unless /QUIET is set).
;	See keywords for options to interpolate exponential or power-law
;	functional behaviours.
; CALLING:
;	fxnew = finterpol( fxold, xold, xnew )
; INPUTS:
;	fxold =	array of function values at the points xold.
;
;	xold = array of points (strictly increasing) where func is evaluated.
;
;	xnew = array of new points at which linear interpolations are desired.
; KEYWORDS:
;	/QUIET : do not check or print any messages if extrapolation is done.
;		Default is to give warning messages about # points extrapolated.
;
;	/INITIALIZE : use only the xold & xnew arrays to compute arrays
;		that are kept in the common block for use in fast repetitive
;		interpolations of different functions from/to same grids.
;		No interpolation is performed and just status (0/1) is returned.
;
;	/EXPONENTIAL_INTERP: perform interpolation/extrapolation in Log-Linear
;		space, thereby giving correct result for exponential function.
;		(Function values are then assumed > 1e-37).
;
;	/POWER_LAW_INTERP: perform interpolation/extrapolation in Log-Log space,
;		thereby giving correct result for power-law function.
;		(Function and X coordinate values are then assumed > 1e-37).
;
;	/NOEXTRAPOLATION: do not extrapolate, just use nearest old values.
;
;	/REVERSE: convenience option to reverse all arrays and then call function,
;		 and then reverse the result before returning result.
;		 Usefull if [xold] array is decreasing order.
; OUTPUTS:
;	Function returns array of linear interpolates at new grid points,
;	or it returns just the number 1 if /INIT is set, or 0 if calling error.
; COMMON BLOCKS:
;	common finterpol
;	common finterpol2
; EXTERNAL CALLS:
;	function interLeave
;	function scalar
; PROCEDURE:
;	Call function interLeave to find old grid points which bracket
;	the new grid points, and then interpolate in vectorized form.
;	Take natural log of function values for exponential interpolation,
;	then exponentiating on return, take log of grid points and function
;	for interpolation of power-law behaviour.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1997.
;	F.V. 1998, added /EXPONENTIAL_INTERP and /POWER_LAW_INTERP options.
;	F.V. 1999, /EXP and /POWER options are remebered for repeat calls,
;		and scalar value is returned if result has just one element.
;	F.V. 2020 @UFastro: added keyword option /NOEXTRAPOLATION
;	F.V. 2020 @UFastro: added keyword option /REVERSE
;-

function finterpol, fxold, xold, xnew, QUIET=quiet, INITIALIZE=init, NOEXTRAPOLATION=noext,$
				POWER_LAW_INTERP=ipow, EXPONENTIAL_INTERP=iexp, REVERSE=revit

  common finterpol, iLw, iLw1, xfrac
  common finterpol2, LogFlag, nextrap

   if keyword_set( revit ) then begin
      return, rotate( finterpol( rotate( fxold, 2 ), rotate( xold, 2 ), rotate( xnew,2 ), $
                                 QUI=quiet, INIT=init, NOEX=noext, POW=ipow, EXP=iexp ), 2)
   endif

   Nxo2 = N_elements( xold )-2

	if (Nxo2 ge 0) and (N_elements( xnew ) gt 0) then begin

		iLw = interLeave( xold, xnew )
		wex = where( (iLw LT 0) or (iLw GT Nxo2), nextrap )

		if nextrap GT 0 then begin
			if iLw[wex[nextrap-1]] eq (Nxo2+1) then begin
				w = wex[nextrap-1]
				if xnew[w] eq xold[iLw[w]] then nextrap=nextrap-1
			   endif
			iLw[wex] = ( iLw[wex] > 0 ) < Nxo2
		   endif

                LogFlag = 0  ;;must always first reset to zero to handle normal interp
                iLw1 = iLw+1
		if keyword_set( ipow ) or keyword_set( iexp ) then LogFlag = 1

                if keyword_set( ipow ) then begin
			z = aLog( xold > 1e-37 )
			xfrac = ( aLog(xnew>1e-37) - z[iLw] )/( z[iLw1] - z[iLw] )
		 endif else xfrac = ( xnew - xold[iLw] )/( xold[iLw1] - xold[iLw] )

	 endif else if (Nxo2 LT 0) and (N_elements( xnew ) gt 0) then begin
		message,"need more than 1 point in the Xold grid",/INFO
		return,0
	  endif

	if keyword_set( init ) then return,1

	if (N_elements( fxold ) LE 0) or (N_elements( xfrac ) LE 0) then begin
		if N_elements( fxold ) LE 0 then $
			message,"missing parameter fxold",/INFO
		if N_elements( xfrac ) LE 0 then $
			message,"missing parameters xold or xnew",/INFO
		print,"syntax:
		print," first call:	fxnew = finterpol( fxold, xold, xnew )"
		print," repeat calls:	fxnew = finterpol( fxold )
		return,0
	  endif

        if keyword_set( LogFlag ) then begin

		z = aLog( fxold > 1e-37 )
		result = exp( ( z[iLw1] - z[iLw] ) * xfrac + z[iLw] )

	 endif else result = ( fxold[iLw1] - fxold[iLw] ) * xfrac + fxold[iLw]

        if (nextrap GT 0) then begin
           if keyword_set( noext ) then begin
              result[wex] = fxold[iLw[wex]]
           endif else if NOT keyword_set( quiet ) then begin
              message,"extrapolated at " + strtrim( nextrap, 2 ) + $
                      " out of the "+ strtrim( N_elements( xfrac ), 2 ) +" new points",/INFO
           endif
        endif

	if N_elements( result ) eq 1 then return, scalar( result ) else return, result
end
