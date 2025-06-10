;+
;  Builds a electronic ghosts response function (PSF) from measuring
;  the ghosts fluxes of a std star,
;  and assuming a 20 pixels period (width of one channel in CRC774).
;
;  "left" side ghosts seem to have a slightly different response than right side ones.
; 
;from file /Volumes/Ext_BouloData/Data/CanariCam/Errors/847027/DL/847027_oe.sav
;
; Written by Eric Pantin, 2017, modified by F.V. 2020.
;
;-

Function CRC774_eghosts_resp, xsize, ysize, CFACTOR=cfactor

  If not keyword_set(cfactor) then cfactor=0.7   ;0.7 is experimental on corrected data
  
  gpxperiod = 20  ;period of e- ghosts in pixels 

  l_ratio = 0.010470 * cfactor
  r_ratio = 0.010470 * cfactor
  
  output = fltarr(xsize,ysize)
  xmid = xsize/2
  ymid = ysize/2
  output[xmid,ymid]=1.0

  for i1= xmid-gpxperiod,      0, -gpxperiod Do output[i1,ymid] = -l_ratio
  for i2= xmid+gpxperiod, xsize-1, gpxperiod Do output[i2,ymid] = -r_ratio

  for i1= xmid-gpxperiod+10,      0, -gpxperiod Do output[i1,ymid] = -l_ratio/4
  for i2= xmid+gpxperiod-10, xsize-1, gpxperiod Do output[i2,ymid] = -r_ratio/4

  for i1= xmid-gpxperiod+5,      0, -gpxperiod Do output[i1,ymid] = -l_ratio/4
  for i2= xmid+gpxperiod+5, xsize-1, gpxperiod Do output[i2,ymid] = -r_ratio/4
  
  return, output
End
