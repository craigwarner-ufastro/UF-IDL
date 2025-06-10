pro tv_sections, image, Nxsect, Nysect

	image_sections, size( image ), Nxsect, Nysect, ix, Lx, iy, Ly

	for k=0,N_elements( ix )-1 do begin

		erase
		tv, image( ix(k):Lx(k) , iy(k):Ly(k) )
		empty
	  endfor
return
end
