function modify_image, image, SMOOTH_WID=smwid, THRESHOLD=threshold, $
			      POWER=power, NORMALIZE=normalize, BACKGROUND=back

;Smooth all pixels using iteration (approaches gaussian convolution),
; apply requested normalizing (max=1), thresholding and/or raising to power.
;Frank Varosi 1991.

	imagef = filter_image( image, SMOOTH=smwid,/ALL_PIXELS,/ITERATE )

	if keyword_set( back ) then  imagef = imagef - back
	if keyword_set( normalize ) then  imagef = imagef * (1.0/max( imagef ))

	if N_elements( threshold ) EQ 1 then begin

		w = where( imagef LT threshold, nlt )
		if (nlt GT 0) then imagef(w) = 0
	   endif

	if keyword_set( power ) then  imagef = imagef^power

return, imagef
end
