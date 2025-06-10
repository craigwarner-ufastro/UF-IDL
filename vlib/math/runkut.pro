pro RunKut, xsol, TSTART=tstart, TSTOP=tstop, NSTEP=nstep, F_DERIV=FdXdT
;+
; NAME:
;	RunKut
;
; PURPOSE:
;	Runge-Kutta fixed step solution of ODE system.
;
; CALLING:
;	RunKut, xsol, TSTART=, TSTOP=, NSTEP=, F_DERIV=
;
; INPUT and OUTPUT:
;	xsol = array giving initial conditions at time TSTART,
;		replaced by solution of ODE at TSTOP.
; KEYWORDS:
;	TSTART=
;	TSTOP=
;	NSTEP=
;	F_DERIV = string, name of the function giving derivative array,
;		the right hand side of ODE system (default is "FdXdT").
;		Form is:
;			dXdT = FdXdT( T, X )
;
;		The resulting array dXdT must have one-to-one correspondence
;		with elements of Xsol.
;
; HISTORY:
;	Written, Frank Varosi U.Md. 1987.
;-
	if N_elements( FdXdT ) NE 1 then FdXdT = "FdXdT"
	if N_elements( nstep ) NE 1 then nstep=20
	if N_elements( tstart ) NE 1 then tstart=0
	if N_elements( tstop ) NE 1 then tstop=1

      h = float( tstop - tstart )/nstep
      h2 = h/2.
      h6 = h/6.
      t = tstart
                
      for i=1,nstep do begin

	dx = call_function( FdXdt, t, xsol )
	t = t + h2

	xdot = call_function( FdXdt, t, xsol + h2 * dx )
	dx = dx + 2*xdot

	xdot = call_function( FdXdt, t, xsol + h2 * xdot )
	dx = dx + 2*xdot
	t = t + h2

	xsol = xsol + h6 * ( dx + call_function( FdXdt, t, xsol + h * xdot ) )

      endfor
END
