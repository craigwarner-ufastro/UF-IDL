;+
;  Corrects the e- ghosts of the CRC774 detector using
;  a degradation model and an inverse filter convolution in Fourier space
;  CFACTOR = factor that allows to play with the pre-defined ghosts amplitudes (default=0.7)
; input : an image, or array of images, or matrix of images.
; output : the corrected image(s)
;
; Written by Eric Pantin, 2017, modified by F.V. 2020.
;-

Function corr_eghosts_deconv, img, CFACTOR=cfactor

  szim = size( img )
  sx = szim[1]
  sy = szim[2]

  if( szim[0] LT 2 ) or (szim[0] gt 4) then begin
     message,/INFO,"input should be an image or array / matrix of images"
     return,0
  endif

  p_resp = CRC774_eghosts_resp( sx, sy, Cfactor=cfactor )

  if( szim[0] gt 2 ) then begin
     nim = szim[3]
     if( szim[0] eq 4 ) then n4 = szim[4] else n4=1
     rs = 1/float(sx*sy)
     sxm = sx/2
     sym = sy/2
     imgs = img
     ftpr = fft( p_resp )
     for j=0,n4-1 do begin
        for i=0,nim-1 do begin
           imgs[*,*,i,j] = rs * shift( float( fft( fft(img[*,*,i,j])/ftpr,/inv)), sxm, sym)
        endfor
     endfor
     return, imgs
  endif

  return, shift( float( fft( fft(img)/fft(p_resp),/inv)), sx/2, sy/2) / float(sx*sy) 
End
