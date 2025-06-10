pro RunKut_fixed, xsol, TSTART=tstart, TSTOP=tstop, NSTEP=nstep, F_DERIV=FdXdT
;+
; NAME:
;	RunKut_fixed
; PURPOSE:
;	Runge-Kutta fixed step solution of ODE system.
;
; CALLING:
;	RunKut_fixed, xsol, TSTART=, TSTOP=, NSTEP=, F_DERIV=
;
; INPUT and OUTPUT:
;	xsol = array giving initial conditions at time TSTART,
;		replaced by solution of ODE at TSTOP.
; KEYWORDS:
;	TSTART=
;	TSTOP=
;	NSTEP=
;	F_DERIV =
;
; EXTERNAL CALLS:
;	function RunKut_step
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-
	if N_elements( nstep ) NE 1 then nstep=20
	if N_elemenst( tstart ) NE 1 then tstart=0
	if N_elemenst( tstop ) NE 1 then tstop=1

	h = float( tstop - tstart )/nstep
	t = tstart

	for i=1,nstep do RunKut_step, xsol, t, h
END
