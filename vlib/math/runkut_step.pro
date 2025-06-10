pro RunKut_step, Xsol, Tx, Tstep, F_DERIVATIVE = FdXdT, $
				DXDT_FIRST=dXdT, PASTIME=past
;+
; NAME:
;	RunKut_step
;
; PURPOSE:
;	Applies the Runge-Kutta method to solve ODE system,
;	giving a single step in evolution of the ODE solution trajectory.
;
; CALLING:
;	RunKut_step, Xsol, Tx, Tstep, F_DERIV=
;
; INPUTS:
;	Xsol = array of initial conditions for ODE at time Tx.
;	Tx = scalar, time of initial conditions
;	Tstep = time step desired.
;
; KEYWORDS:
;	F_DERIVATIVE = string, name of the function giving derivative array,
;			the right hand side of ODE system (default is "FdXdT").
;		Form is:
;			dXdT = FdXdT( T, X )
;		or:
;			dXdT = FdXdT( T, X, Tpast )	if /PAST.
;		The resulting array dXdT must have one-to-one correspondence
;		with elements of Xsol.
;
;	DXDT_FIRST = optional, the derivative array from a previous call,
;			(just to save time, default is to compute it).
;
;	/PAST option causes the F_DERIVATIVE to be passed a third argument
;		containing the previous step time.
; OUTPUTS:
;	Xsol = solution of ODE at new time Tx.
;	Tx = new time of solution ( = Tx + Tstep )
;
; PROCEDURE:
;	The 4-th order Runge-Kutta method, e.g. Numerical Recipes, sec.15.1.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-
	if N_elements( FdXdT ) NE 1 then FdXdT = "FdXdT"
	h = Tstep
	h2 = h/2.
	Ts = Tx

if keyword_set( past ) then begin

	if N_elements( dXdT ) NE N_elements( Xsol ) then $
			dXdT = call_function( FdXdT, Tx, Xsol, Ts )
	dxsum = dXdT

	Tx = Ts + h2
	dXdT = call_function( FdXdT, Tx, Xsol + h2 * dXdT, Ts )
	dxsum = dxsum + 2 * dXdT

	dXdT = call_function( FdXdT, Tx, Xsol + h2 * dXdT, Ts )
	dxsum = dxsum + 2 * dXdT
	Tx = Ts + h

	Xsol = Xsol + $
		(h/6.)*( dxsum + call_function( FdXdT, Tx, Xsol + h*dXdT, Ts ) )

  endif else begin

	if N_elements( dXdT ) NE N_elements( Xsol ) then $
			dXdT = call_function( FdXdT, Tx, Xsol )
	dxsum = dXdT

	Tx = Ts + h2
	dXdT = call_function( FdXdT, Tx, Xsol + h2 * dXdT )
	dxsum = dxsum + 2 * dXdT

	dXdT = call_function( FdXdT, Tx, Xsol + h2 * dXdT )
	dxsum = dxsum + 2 * dXdT
	Tx = Ts + h

	Xsol = Xsol + $
		(h/6.)*( dxsum + call_function( FdXdT, Tx, Xsol + h*dXdT ) )
   endelse
END
