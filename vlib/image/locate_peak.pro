pro Locate_peak, image, x_peak, y_peak, maxim, minim, REGION=region

;Frank Varosi STX @ NASA/GSFC 1991.

	if N_elements( region ) EQ 4 then begin

		Locate_peak, image[ region[0]:region[1], region[2]:region[3] ], xmax, ymax
		x_peak = xmax + region[0]
		y_peak = ymax + region[2]

	 endif else begin

		maxim = max( image, imax, MIN=minim )
		s = size( image )
		x_peak = imax MOD s(1)
		y_peak = imax/s(1)
	  endelse
return
end
