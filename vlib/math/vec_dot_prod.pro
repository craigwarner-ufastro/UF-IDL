function Vec_Dot_Prod, vecf1, vecf2, NORMALIZE=norm

;return dot (inner) product of vector(s) array( Nvec, Vdim )
;(assume 1st index is vector #, 2nd index is vector component #)
;(if only one vector, then return its dot procuct).
;Frank Varosi U.MD.1988

	s = size( vecf1 )
	fcomplex = s(s(0)+1) EQ 6

	if ( s(0) LT 2 ) then begin
		if (fcomplex) then vd = total( vecf1*conj( vecf2 ) ) $
			      else vd = total( vecf1*vecf2 )
		if keyword_set( norm ) then $
			return,abs( vd )/(vec_norm( vecf1 )*vec_norm( vecf2 )) $
		   else return, vd
	   endif

	s = size( vecf1 )
	nvec = s(1)
	dim = s(2)
	vd = fltarr( nvec )

	if ( dim LE nvec ) then begin	;Choose the most vectorized computation.

		if (fcomplex) then $
		   for k=0,dim-1 do vd = vd + vecf1(*,k) * conj( vecf2(*,k) ) $
		  else for k=0,dim-1 do vd = vd + vecf1(*,k) * vecf2(*,k)
			
	  endif else begin

		if (fcomplex) then for i=0,nvec-1 do $
			      vd(i) = total( vecf1(i,*) * conj( vecf2(i,*) ) ) $
		else for i=0,nvec-1 do vd(i) = total( vecf1(i,*) * vecf2(i,*) )
	   endelse

	if keyword_set( norm ) then $
		return, abs( vd )/(vec_norm( vecf1 )*vec_norm( vecf2 )) $
	   else return,vd
end
