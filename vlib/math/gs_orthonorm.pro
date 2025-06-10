pro gs_OrthoNorm ,vectors ,renorm

;Gram-Schmidt Re-OrthoNormalization of basis vectors.
;Frank Varosi U.MD.1988

	s = size( vectors )
	dim = s(2)
	renorm = dblarr( dim )

	v0 = vectors(*,0)			;do first vector of basis.
	renorm(0) = sqrt( total( v0*v0 ) )
	vectors(0,0) = v0/renorm(0)

	for i=1,dim-1 do begin			;recursively do rest of vectors

	   vi = vectors(*,i)
	   visav = vi

	   for k=0,i-1 do  vi = vi - vectors(*,k) * total( visav*vectors(*,k) )

	   renorm(i) = sqrt( total( vi*vi ) )
	   vectors(0,i) = vi/renorm(i)
	endfor
return
end
