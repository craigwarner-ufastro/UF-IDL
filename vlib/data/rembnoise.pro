;; Function to create template of excess vertical noise from border columns
;; and then subtract from all columns, but with optional fitting to
;; each channel with LLsq1D.    FV Sept 2019

function rembnoise, image, LEFT_BORDER=bLx, RIGHT_BORDER=brx, FITCHANS=fitchans

  szim = size(image)

  if szim[0] gt 2 then begin
     imsubn = image
     for j=0,szim[3]-1 do imsubn[*,*,j] = rembnoise( image[*,*,j], LEFT=bLx, RIGHT=brx )
     return, imsubn
  endif

  nxpix = szim[1]
  if N_elements( bLx ) ne 1 then bLx = 9
  if N_elements( brx ) ne 1 then brx = 10
  bLx >= 2
  brx >= 2
  imsubn = image
  bLavg = total( image[0:bLx,*],1) / (bLx+1)
  bRavg = total( image[nxpix-brx:*,*],1) / brx

  if keyword_set( fitchans ) then begin

     imsct = total( sepchans( image ), 1 )/20
     szsc = size( imsct )
     fitchans <= 1
     nypix = szsc[1] * fitchans -1
     nchan = szsc[2]

     for k = 0, nchan/2-1 do begin
        stdvs = fltarr(41)
        LLsq1D, bLavg[0:nypix], imsct[0:nypix,k], af, bf
        if !DEBUG then print,k, af, bf
        for i = 0,19 do imsubn[i+k*20,*] -= (bf * bLavg + af)
     endfor
     
     for k = nchan/2, nchan-1 do begin
        stdvs = fltarr(41)
        LLsq1D, bravg[0:nypix], imsct[0:nypix,k], af, bf
        if !DEBUG then print,k, af, bf
        for i = 0,19 do imsubn[i+k*20,*] -= (bf * bRavg + af)
     endfor
     
  endif else begin

     for i = 0,     nxpix/2-1 do imsubn[i,*] -= bLavg
     for i = nxpix/2, nxpix-1 do imsubn[i,*] -= bRavg

  endelse

  if !DEBUG then stop
  
return, imsubn
end
