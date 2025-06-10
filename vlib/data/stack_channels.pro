function stack_channels, image, NCHANNELS=nchan, WHICH_CHANNELS=wchan

	if N_elements( nchan ) ne 1 then nchan=16
	sz = size( image )
	nx = sz[1]/nchan
	nwch = N_elements( wchan )

	if nwch LE 1 then begin
		nwch = nchan
		wchan = indgen( nchan )
	   endif

	chan_stack = fltarr( nx, sz[2], nwch )

	for ic = 0,nwch-1 do begin
		kc = wchan[ic]
		chan_stack[*,*,ic] = image[ kc*nx:(kc+1)*nx-1, * ]
	  endfor

return, chan_stack
end
