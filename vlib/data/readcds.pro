; IDL Version 8.0, Mac OS X (darwin x86_64 m64)
; Journal File for varosi@trantor
; Working directory: /Users/varosi/procc
; Date: Wed Jul  7 21:38:11 2021

function readCDS, file

  message,/INFO,"Reading file: " + file
  fits_open, file, fcb
  fits_read,fcb,im1,xhd1,exten_no=1
  fits_read,fcb,im2,xhd2,exten_no=2
  imdiff = im2-float(im1)
  fits_close,fcb

  cds = { image:imdiff, img1:im1, img2:im2, xhd1:xhd1, xhd2:xhd2, fhdr:fcb.hmain, fcb:fcb }
  return, cds
end
