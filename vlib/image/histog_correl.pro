function histog_correl, densities, values, WEIGHT_BY_VAL=weight, NORM=norm

;Compute simple correlation between histograms (must be all on same scale).
;First one is correlated with all of them, /WEIGHT uses values as weighting.
; Keyword NORM allows re-using the  total( density(*,0)^2 )  computation.
;Frank Varosi STX @ NASA/GSFC 1991.

	sd = size( densities )
	sv = size( values )

	if (sd(0) NE 2) then begin
		message,"need more than one histogram to correlate",/INFO
		return,(-1)
	   endif

	correls = fltarr( sd(2) )
	correls(0) = 1
	if (sv(0) NE 2) then weight=0

	den0 = float( densities(*,0) )
	if keyword_set( weight ) then den0 = den0 * values(*,0)
	if N_elements( norm ) NE 1 then norm = total( den0^2 )

	for i=1,sd(2)-1 do begin
		dens = float( densities(*,i) )
		if keyword_set( weight ) then dens = dens * values(*,i)
		correls(i) = total( dens * den0 )/sqrt( norm * total( dens^2 ) )
	  endfor

return, correls
end
