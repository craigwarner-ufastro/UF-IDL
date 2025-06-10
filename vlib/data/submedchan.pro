function subMedChan, image, correction, OFF_MEDIAN=offmed, NCHANNELS=nchan, WHICH_CHANNELS=wchan

	chanStack = stack_channels( image, NCHAN=nchan, WHICH=wchan )
	sort_stack, chanStack

	sz = size( chanStack )
	nstack = sz[3]
	imedian = nstack/2
	if nstack MOD 2 eq 0 then imedian = imedian-1

	sz = size( image )
	nx = sz[1]/nchan
	imcorr = image
	if keyword_set( offmed ) then imedian = imedian + offmed
	correction = chanStack[*,*,imedian]
	nwch = N_elements( wchan )

	if nwch LE 1 then begin
		nwch = nchan
		wchan = indgen( nchan )
	   endif

	for ic = 0,nwch-1 do begin
		ix = wchan[ic]*nx
		Lx = ix + nx - 1
		imcorr[ ix:Lx, * ] = image[ ix:Lx, * ] - correction
	  endfor

return, imcorr
end
