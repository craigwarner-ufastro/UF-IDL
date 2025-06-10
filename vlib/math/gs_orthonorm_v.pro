pro gs_OrthoNorm_V, vectors ,renorm

;Gram-Schmidt Re-OrthoNormalization of many vector-bases.
;Frank Varosi U.MD.1988

	s = size( vectors )	;is actually array of vectors (3D matrix)
	dim = s(2)
	renorm = dblarr( s(1),dim )
	vip = dblarr( s(1) )

	v0 = vectors(*,*,0)			;do first vector of each basis.
	renorm(0,0) = Vnorm( v0 )
	for j=0,dim-1 do vectors(0,j,0) = v0(*,j)/renorm(*,0)

	for i=1,dim-1 do begin			;recursively do rest of vectors

		vi = vectors(*,*,i)
		visav = vi

		for k=0,i-1 do begin

			vk = vectors(*,*,k)
			vip(*) = 0

			for j=0,dim-1 do  vip = vip + visav(*,j)*vk(*,j)

			for j=0,dim-1 do  vi(0,j) = vi(*,j) -  vk(*,j) * vip

		 endfor

		renorm(0,i) = Vnorm( vi )
		for j=0,dim-1 do  vectors(0,j,i) = vi(*,j) / renorm(*,i)
	endfor
return
end
