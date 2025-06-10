function phase, psi

	sz = size( psi )
	ang = make_array( DIM=sz(1:sz(0)),/FLOAT )

	r = float( psi )
	i = imaginary( psi )

	w = where( (r NE 0) OR (i NE 0), nw )
	if (nw GT 0) then ang(w) = atan( i(w), r(w) )

return, ang
end
