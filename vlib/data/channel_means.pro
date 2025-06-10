pro channel_means, data, means, maxes, mins, NCHANNEL=nchan, NVREGION=nvreg, COADDS=coadds, $
                   HEADER=fhd, MEDIAN=useMedian, ROWSTART=rowstart

	if N_elements( fhd ) gt 1 then coadds = sxpar(fhd,"FRMCOADD")
	if N_elements( coadds ) ne 1 then coadds=1
	if N_elements( nchan) ne 1 then nchan=16
	if N_elements( nvreg ) ne 1 then nvreg=1

        sz = size(data)
	npixch = sz[1]/nchan
	npixvr = sz[2]/nvreg
	ndat = sz[3]
	if sz[0] eq 2 then ndat = 1

        if( nvreg gt 1 ) then begin
           means = fltarr( nvreg, nchan, ndat )
           rowstart = 0
        endif else begin
           means=fltarr( nchan, ndat )
           if N_elements( rowstart ) ne 1 then rowstart = sz[2]/2
        endelse

        maxes = means
	mins = means

	for j=0,ndat-1 do begin

           if keyword_set( useMedian ) then datj = median( data[*,*,j], 3 ) else datj = data[*,*,j]

           for i=0,nchan-1 do begin

			chandat = float( datj[ i*npixch:(i+1)*npixch-1, rowstart:* ] )

			if nvreg gt 1 then begin

				for k=0,nvreg-1 do begin
					regdat = chandat[ *, k*npixvr:(k+1)*npixvr-1 ]
					means[k,i,j] = total( regdat )/N_elements( regdat )
					maxes[k,i,j] = max( regdat, MIN=m )
					mins[k,i,j] = m
				 endfor

			 endif else begin

				means[i,j] = total( chandat )/N_elements( chandat )
				maxes[i,j] = max( chandat, MIN=m )
				mins[i,j] = m
			  endelse
		 endfor
	 endfor

	if coadds gt 1 then begin
		means = means/coadds
		maxes = maxes/coadds
		mins = mins/coadds
	   endif
end
