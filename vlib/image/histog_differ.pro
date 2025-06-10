function histog_differ, coefs, ABSVAL=absval, NORM=norm, TOTAL=total, $
			NBIN=nbin, FIXED_IMAGE=imfixed, MATCH_IMAGE=immatch

;Frank Varosi STX @ NASA/GSFC 1992.

  common histog_differ, image_match, vacc_fix, hacc_fix, binsize, nbins

	if (N_elements( imfixed ) GT 0) AND $
	   (N_elements( immatch ) GT 0) then begin
		image_match = immatch
		hacc_fix = accumulate( histo( imfixed, vacc_fix, NBIN=nbin ) )
		binsize = vacc_fix(1) - vacc_fix(0)
		nbins = nbin
		return,(1)
	   endif

	hacc = accumulate( histo( image_match*coefs(1) + coefs(0), vacc, $
							BINSIZE=binsize ) )
	match, vacc_fix, vacc, mfix, mm, RESOL=binsize/2
	hdif = hacc_fix(mfix) - hacc(mm)

	if !DEBUG GT 0 then begin
		print,total( hdif )/nbins,total( abs( hdif ) )/nbins
		if !DEBUG GE 2 then begin
			plot, vacc_fix, hacc_fix
			oplot, vacc, hacc, LINE=1
		   endif
		if !DEBUG GE 3 then stop
	   endif

	if keyword_set( absval ) then hdif = abs( hdif )
	if keyword_set( norm ) then hdif = hdif*hdif
	if keyword_set( total ) then return, total( hdif )/nbins $
				else return, hdif
end
