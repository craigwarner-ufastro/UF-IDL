; IDL Version 7.1, Mac OS X (darwin x86_64 m64)
; Journal File for varosi@trantor.astro.ufl.edu
; Working directory: /Users/varosi/mosaic
; Date: Mon Mar 23 19:36:34 2015

function read_NIR_MEF, file

  message,/INFO,"Reading file: " + file
  fits_open, file, fcb
  mfhd = fcb.hmain
  print, sxpar( mfhd,"filename")
  print, sxpar( mfhd,"date")
  ngroups = sxpar( mfhd,"ngroups")
  nramps = sxpar( mfhd,"nramps")
  nreads = sxpar( mfhd,"nreads")
  nresets = sxpar( mfhd,"nresets")
  fastResets = sxpar( mfhd,"FastRsts")
  expmode = sxpar( mfhd,"ExpMode")
  exptime = sxpar( mfhd,"ExpTime")
  nfexpect = nreads * ngroups * nramps
  help,nreads,ngroups,nramps,nfexpect,nresets,fastResets,expmode,exptime

  fits_read, fcb, img, xhd1, exten_no=1
  sim = size( img )
  nxp = sim[1]
  nyp = sim[2]
  help,nxp,nyp
  membytes = 2.0 * nxp * nyp * nreads * ngroups * nramps

  if( membytes gt 1e9 ) then begin
     message,"requested memory allocation "+strtrim(membytes,2)+" > 1 GB ?"
  endif

  imgs = uintarr( nxp, nyp, nreads, ngroups, nramps )
  help,imgs
  kx=0

  for kr=0,nramps-1 do begin
     for kg=0,ngroups-1 do begin
        for kf=0,nreads-1 do begin
           ++kx
           fits_read, fcb, img, xhd, Exten_No=kx
           imgs[*,*,kf,kg,kr] = img
           print,kx,kf,kg,kr
           if( kx eq 1 ) then xtenhdrs = strarr( N_elements( xhd1 ), nreads, ngroups, nramps )
           xtenhdrs[0,kf,kg,kr] = xhd
        endfor
     endfor
  endfor

  fits_close,fcb

  nir = { images:reform(imgs), xhds:reform( xtenhdrs ), fhdr:fcb.hmain, fcb:fcb }
  return, nir
end

