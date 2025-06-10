; IDL Version 8.0, Mac OS X (darwin x86_64 m64)
; Journal File for varosi@trantor.local
; Working directory: /Users/varosi/isim
; Date: Fri Dec 14 03:32:55 2018
;; Compute Encircled Energy versus radius around a point source PSF.
 
function psfee, img, cx, cy, THRESHOLD=thresh, ZERO_MEAN=zeromv, VERBOSE=verbose,SHOW=show,OFFSET=offset

  szim = size( img )
  nx = szim[1]
  ny = szim[2]

  if szim[0] ne 2 then begin
     message,/INFO,"input arg. must be an image."
     return,0
  endif

  if N_elements( cx ) ne 1 OR N_elements( cy ) ne 1 then begin
     centroid, img, cx, cy
     if keyword_set( verbose ) then help,cx,cy
  endif

  snoise = float( sky_noise( img, mv, PLOT=show ) )
  mv = float( mv )
  if keyword_set( verbose ) then help, snoise, mv

  if N_elements( thresh ) eq 1 then begin
     thresh <= max( img )/2
     totimg = total( img[ where( img gt thresh, nw )] )
  endif else totimg = total ( img )

  if keyword_set( verbose ) then help, thresh, totimg

  if keyword_set( zeromv ) then begin
     if N_elements( offset ) ne 1 then offset = -mv
     totimg += (nx * ny * offset)
     if keyword_set( verbose ) then help, offset, totimg
  endif else offset = 0

  gxy = gridxy( nx, ny,/VEC ) + 0.5         ;; add 0.5 because my centroid middle is 0.5
  nk = ((nx - cx) < cx) < ((ny - cy) < cy)
  ee = fltarr( nk )

  for kr=1,nk-1 do begin
     w = where( sqrt( (gxy[*,0]-cx)^2 + (gxy[*,1]-cy)^2 ) LE (kr+0.1), nw )
     if( nw gt 0 ) then ee[kr] = ( total( img[gxy[w,0],gxy[w,1]] ) + nw * offset )/totimg
  endfor

  return, ee
end
