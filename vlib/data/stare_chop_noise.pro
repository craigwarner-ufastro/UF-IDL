pro stare_chop_noise, data, fhd, mstdevs, chopfreqs, SPECTRUM=spectrum, STDEVS=stdevs, MEANS=means, NCHOPS=nchops

  savefreq = sxpar(fhd,"SAVEFREQ")
  help,savefreq
  if N_elements( nchops ) ne 1 then nchops = 10
  chopfreqs = savefreq/(findgen(nchops)+1)/2
  print,sxpar(fhd,"FILENAME")
  print,sxpar(fhd,"OBSMODE")
  print,sxpar(fhd,"READMODE")
  print,sxpar(fhd,"FrmTime")
  print,sxpar(fhd,"WELLDPTH")
  print,sxpar(fhd,"WLMINMIN")
  print,sxpar(fhd,"WLMAXMAX")
  print,sxpar(fhd,"FILTER1")
  print,sxpar(fhd,"FILTER2")

  sz = size( data )
  nxw = sz[1]
  nim = sz[sz[0]]
  nst = nim - 1 - 2*nchops

  if keyword_set( spectrum ) then begin

     stdevs = fltarr( nxw, nst, nchops )
     mstdevs = fltarr( nxw, nchops )
     means = stdevs
     help,stdevs,mstdevs

     for i=0,nst-1 do begin
        ddif = float( data[*,*,i+1] - data[*,*,i] )
        for j=0,nxw-1 do begin
           stdevs[j,i,0] = stdev( ddif[j,*], m )
           means[j,i,0] = m
        endfor
     endfor

     for k=1,nchops-1 do begin
        print,k
        for i=0,nst-1 do begin
           di = total( data[*,*,i:i+k], 3 )
           dik = total( data[*,*,i+k+1:i+2*k+1], 3 )
           ddif = dik - di
           for j=0,nxw-1 do begin
              stdevs[j,i,k] = stdev( ddif[j,*], m )
              means[j,i,k] = m
           endfor
        endfor
        stdevs[*,*,k] = stdevs[*,*,k]/sqrt(k+1)
        means[*,*,k] = means[*,*,k]/(k+1)
     endfor

     for k=0,nchops-1 do begin
        for j=0,nxw-1 do begin
           mstdevs[j,k] = avg( reform(stdevs[j,*,k]) )
        endfor
     endfor

  endif else begin

     stdevs = fltarr( nst, nchops )
     mstdevs = fltarr( nchops )
     means = stdevs

     for i=0,nst-1 do begin
        stdevs[i,0] = stdev( float( data[*,*,i+1] - data[*,*,i] ) , m )
        means[i,0] = m
     endfor

     for k=1,nchops-1 do begin
        print,k
        for i=0,nst-1 do begin
           di = total( data[*,*,i:i+k], 3 )
           dik = total( data[*,*,i+k+1:i+2*k+1], 3 )
           stdevs[i,k] = stdev( dik - di, m )
           means[i,k] = m
        endfor
        stdevs[*,k] = stdevs[*,k]/sqrt(k+1)
        means[*,k] = means[*,k]/(k+1)
     endfor

     for k=0,nchops-1 do mstdevs[k] = avg( stdevs[*,k] )
  endelse

end
