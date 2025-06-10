;+
;	chisq_fit
;
;Example to show how to find the parameters of a nonlinear function
; which minimizes the chi-sq of fit to some (x,y) data, using minf_conj_grad.
;
;Frank Varosi NASA/GSFC 1992.
;-

function chisq_fit, parms, gradient, FUNC_NAME=func_nam, WEIGHTS=wd,  $
						X_DATA=xd, Y_DATA=yd

  common chisq_fit, xdata, ydata, wdata, func_name

	if N_elements( func_nam ) EQ 1 then func_name = func_nam
	if N_elements( xd ) GT 0 then xdata = xd
	Nx = N_elements( xdata )
	if N_elements( yd ) EQ Nx then ydata = yd
	if N_elements( wd ) EQ Nx then wdata = wd
	if N_elements( wdata ) NE Nx then wdata = replicate( 1, Nx )
	Nfree = Nx - N_elements( parms )

	if N_params() GE 2 then begin

		ydif = ydata - call_function( func_name, xdata, parms, pderiv )

		gradient = (-2./Nfree) * ( (wdata * ydif) # pderiv )

	  endif else  ydif = ydata - call_function( func_name, xdata, parms )

return, total( wdata * ydif * ydif )/Nfree
end
