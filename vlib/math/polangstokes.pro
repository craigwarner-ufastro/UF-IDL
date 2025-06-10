;; Compute polarization angle from Stokes q and u vectors based on Poincare circle.
;; Reference is the x-axis: output range is -45 to 135 deg.
;; Use keyword /REFYAXIS to switch to y-axis reference.

function polangStokes, stkq, stku, REFYAXIS=refyaxis, PAMAX=pamax

  npa = N_elements( stkq )

  if N_elements( stku ) ne npa then begin
     message,/INFO,"input Stokes q and u must have same # of elements."
     return,0
  endif

  pang = fltarr( npa )

  wqp = where( stkq gt 0, nqp )
  if( nqp gt 0 ) then pang[wqp] = !RADEG * atan( stku[wqp] / stkq[wqp] )/2

  wqz = where( stkq eq 0, nqz )

  if( nqz gt 0 ) then begin
     wugz = where( stku gt 0, nugz )
     if( nugz gt 0 ) then pang[wqz[wugz]] = 45
     wun = where( stku LT 0, nun )
     if( nun gt 0 ) then pang[wqz[wun]] = -45
  endif

  wqn = where( stkq LT 0, nqn )
  if( nqn gt 0 ) then pang[wqn] = (180 + !RADEG * atan( stku[wqn] / stkq[wqn] ))/2

  if keyword_set( refyaxis ) then pang += 90

  if N_elements( pamax ) ne 1 then begin
     if keyword_set( refyaxis ) then pamax = 180 else pamax = 135
  endif

  wg = where( pang ge pamax, ng )
  if( ng gt 0 ) then pang[wg] -= 180

  return, pang
end
