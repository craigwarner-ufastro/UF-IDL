pro chnoise, data, sigmas, means, COADDS=coadds, SUBTRACTS=subs, $
				PLOT=plotit, HEADER=fhd

	if N_elements( fhd ) gt 1 then coadds = sxpar(fhd,"FRMCOADD")
	if N_elements( coadds ) ne 1 then coadds=1
	if N_elements( subs ) ne 1 then subs=1
	sz = size(data)
	ndat = sz[3]
	if sz[0] eq 2 then ndat = 1
	sigmas = fltarr(16,ndat)
	means = sigmas	

	for j=0,ndat-1 do begin
	  for i=0,15 do begin
	    sigmas[i,j] = sky_noise(float(data[i*20+1:(i+1)*20-2,*,j]),sky)
	    means[i,j] = sky
	   endfor
	 endfor

	means = means/coadds
	sigmas = sigmas/sqrt(coadds)/sqrt(subs)

	if keyword_set( plotit ) then plotchans, sigmas, means, HEAD=fhd
end
