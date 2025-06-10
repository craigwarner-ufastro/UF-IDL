pro image_sections, size_image, Nxsect, Nysect, ix, Lx, iy, Ly

	Nxpix = size_image(1)
	Nypix = size_image(2)

	sxpix = Nxpix/Nxsect
	sypix = Nypix/Nysect

	Nsect = Nxsect * Nysect
	ix = intarr( Nsect )
	iy = ix
	isect = 0

	for sy=0,Nysect-1 do begin

		for sx=0,Nxsect-1 do begin

			iy(isect) = sy * sypix
			ix(isect) = sx * sxpix
			isect = isect + 1
		  endfor
	  endfor

	Lx = (ix + sxpix -1) < (Nxpix-1)
	Ly = (iy + sypix -1) < (Nypix-1)
return
end
