function Vec_Norm ,vecf

;return norm of vector(s) array( Nvec , Vdim )
;(assume 1st index is vector #, 2nd index is component #)
;(if only one vector, then return its norm).
;Frank Varosi U.MD.1988

	s = size( vecf )
	fcomplex = s(s(0)+1) EQ 6

	if ( s(0) LT 2 ) then begin
		if (fcomplex) then $
		   return, sqrt( total( float( vecf*conj(vecf) ) ) ) $
	      else return, sqrt( total( vecf*vecf ) )
	   endif

	nv = s(1)
	dim = s(2)
	vn = fltarr( nv )

	if ( dim LE nv ) then begin	;Choose the most vectorized computation.

		if (fcomplex) then begin

			for k=0,dim-1 do begin
				v = vecf(*,k)
				vn = vn + v * conj( v )
			  endfor
		  endif else begin
			for k=0,dim-1 do begin
				v = vecf(*,k)
				vn = vn + v*v
			  endfor
		   endelse

	 endif else begin

		if (fcomplex) then begin

			for i=0,nv-1 do begin
				v = vecf(i,*)
				vn(i) = total( v * conj( v ) )
			  endfor
		  endif else begin
			for i=0,nv-1 do begin
				v = vecf(i,*)
				vn(i) = total( v*v )
			  endfor
		   endelse
	   endelse

return, sqrt( float( vn ) )
end
