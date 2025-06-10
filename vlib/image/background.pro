function background, image, SMOOTH_WIDTH=smwid, NITER=nit

;Frank Varosi NASA/GSFC 1992.

	if N_elements( smwid ) NE 1 then smwid=3
	smwid = smwid > 3
	imbg = filter_image( image, SMO=smwid,/ALL )

	sim = size( image )
	if N_elements( nit ) NE 1 then nit = sim(1)*sim(2)

	for i=1,nit do begin
		imbg = smooth( imbg, smwid )
		if (i MOD sim(1)) EQ 0 then print,i,FORM="($,i9)"
	  endfor

return, filter_image( imbg, SMO=smwid,/ALL )
end
