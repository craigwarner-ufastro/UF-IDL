;+
; NAME:
;	expect
; PURPOSE:
;	Compute expectation (mean, first moment) of a probability distribution.
; CALLING:
;	mean = expect( x, Px )
; INPUTS:
;	x = array of independent variable values, strictly increasing.
;	Px = array of probability distribution values (positive) at points x.
; KEYWORDS:
;	/EXP : integrate in Linear-Log space,
;		increases accuracy for exponentially varying functions.
;	/POW : integrate in Log-Log space,
;		increases accuracy for functions with power-law variation.
; OUTPUTS:
;	Function returns scalar mean value of probability distribution.
; EXTERNAL CALLS:
;	function Trap_Int
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1998.
;-

function expect, x, Px, EXPONENTIAL=vex, POWER_LAW=vp

return, Trap_Int( x, x*Px, EXP=vex, POW=vp )/Trap_Int( x, Px, EXP=vex, POW=vp )
end
