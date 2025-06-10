function interpolate, V, U, X=x
;+
; NAME:
;	interpolate
;
; PURPOSE:
;	Linearly interpolate vectors with a regular or irregular grid.
;
; CALLING SEQUENCE:
;	Result = interpolate( V, U )
;
; INPUTS:
;	V:	The input vector can be any type except string.
;
;	U:	The absicissae values for the result.  The result will have 
;		the same number of elements as U, does not need to be monotonic.
; KEYWORDS:
;	X = the grid of V, default is x = findgen( N_elements( V ) ).
; OUTPUTS:
;	Interpolate returns a floating-point vector of N points determined
;	by linearly interpolating the input vector.
;
; MODIFICATION HISTORY:
;	Written, DMS, October, 1982.
;	Modified, Rob at NCAR, February, 1991.  Made larger arrays possible 
;		and corrected by using long indexes into the array.
;	Mod by F.V. 1993 to emulate the interpolate function for sun386i.
;-
	m = N_elements(v)	;# of input pnts
	if n_elements(x) ne m then x = findgen( m )

	n= n_elements(u)	;# of output points
	m2=m-2			;last subs in v and x
	r= fltarr(n)+V(0)	;floating, dbl or cmplx result

	if x(1) - x(0) ge 0 then s1 = 1 else s1=-1 ;Incr or Decr X
	ix = 0L			;current point

	for i=0L,n-1 do begin	;point loop
		d = s1 * (u(i)-x(ix))	;difference
		if d LE 0. then r(i)=v(ix) else begin  ;at point
		  if d gt 0 then $
		     while (s1*(u(i)-x(ix+1)) gt 0) and (ix lt m2) do ix=ix+1 $
		   else while (s1*(u(i)-x(ix)) lt 0) and (ix gt 0) do ix=ix-1
		  r(i) = v(ix) + (u(i)-x(ix))*(v(ix+1)-v(ix))/(x(ix+1)-x(ix))
		 endelse
	  endfor
return,r
end
