;+
; NAME:
;    CIRCINT
; PURPOSE:
;    Integrate flux in circular apertures.  User responsible for 
;    subtracting sky first.  See notes (1) and (2) below.
; CALLING SEQUENCE:
;    circint, image, x0, y0, radii, totdif, npixdif, t8, n8
;
; INPUT PARAMETERS:
;    image      2-D image array
;    x0, y0     center of circular apertures (need not be integral)
;    radii     vector of aperture radii
; OUTPUT PARAMETERS (see note 2):
;    totdif     total raw flux for each annulus (difference of successive
;               apertures)
;    npixdif    number of pixels in each annulus
;    t8         like totdif, but each annulus is divided into eight sections
;               (X dimension = 8, Y dimension = number of annuli).  Allows
;               assessment of error due to lumpiness of distribution, by
;               method of Djorgovski & King
;    n8         number of pixels in each octant of annulus.
; OPTIONAL INPUT KEYWORDS:
;    MASK       Must be same size as image.  If a pixel in
;               the mask is 1, the corresponding pixel in the image is
;               counted.  If 0, the corresponding pixel in the image is
;               ignored (in all results).  See note (3).
;    /VERBOSE   causes routine to tell you what stage it's at.  See note (1).
; COMMON BLOCKS:
;    none
; NOTES:
;   (1) If you aren't sure you've set up right, use the /verbose keyword,
;       because the routine is fairly slow.
;   (2) For a surface brightness profile, plot totdif/npixdif vs. radii.
;       For an aperture growth curve, plot tot vs. radii.
;   (3) Mask is intended to mask out stars or garbage.  Depending on your
;       application, you might be better off modifying the input image.
; SIDE EFFECTS:  none
; PROCEDURE:   Similar to IDL/UIT/DAOPHOT aperture routine.
; MODIFICATION HISTORY:
;  Integrate in circular aperture.
;  RSH - STX - 20 Aug 1990
;  Modified to ignore some pixels according to mask image.
;  RSH - STX - 19 Sept 90
;  Totals done for annuli rather than discs.
;  RSH - STX - 20 Sept 90
;  Use of mask corrected.  RSH - STX - 27 Sept 90
;  Small change to conserve array space.  RSH - STX - 5 Nov 90
;  Fractional-pixel approximation adopted from Wayne Landsman's version
;     of DAOPHOT APER.  Annuli computed from discs.  RSH - STX - 17 July 91
;  Annuli divided into octants for subsequent error estimate due to random
;     distribution of sources.  RSH - STX - 22 July 91
;  Several bugs fixed.  RSH - STX - 3 Oct 91
;  Spiffed up for UIT IDL library.  RSH - Hughes STX - 11 June 92
;  Speeded up initializaion.  RSH - HSTX - 5 August 1993
;  Minor change to allow a scalar radius.  JWmP - HSTX - 1995 May 12
;  Speeded up by computing totals for annuli rather than discs. F.V. - 1995.
;-

PRO circint, image, x0,y0, radii, totdif, npixdif, t8, n8, $
			MASK=mask, VERBOSE=verbose      

IF n_params(0) LT 1 THEN BEGIN
   message,'syntax:  circint, image, x0, y0,' $
          +' radii, totdif, npixdif, t8, n8,' $
          +' mask=mask,/verbose',/INFO
   return
 ENDIF

sz       = size(image)
bell     = string(07b)

IF keyword_set(mask) THEN BEGIN
   szm = size(mask)
   IF (szm(1) NE sz(1)) OR (szm(2) NE sz(2)) THEN BEGIN
      message,'Mask image must be same size as flux image.'
   ENDIF
ENDIF

IF keyword_set( verbose ) THEN $ 
   message,/inf,'Beginning routine to integrate in circular apertures ...'
nrad = n_elements( radii )

IF (nrad gt 1) then begin
   order_test = radii(1:nrad-1) - radii(0:nrad-2)
   ww         = where((order_test LE 0),count)
   IF count GT 0 THEN $                             
      message,'Radii must be specified in ascending order.'
ENDIF

ww  = where((radii LT 0.5), count)
IF count GT 0 THEN $
   message,'All radii must be 0.5 pixels or greater.'

maxrad   = float(max(radii))
maxrad2  = maxrad^2
distance = replicate(2.0*maxrad2, sz(1), sz(2)) ; Like FLTARR with init
xdist2   = (findgen(sz(1)) - x0)^2
ydist2   = (findgen(sz(2)) - y0)^2
xmin = min(where(xdist2 LE maxrad2)) - 1
xmax = max(where(xdist2 LE maxrad2)) + 1
ymin = min(where(ydist2 LE maxrad2)) - 1
ymax = max(where(ydist2 LE maxrad2)) + 1

FOR iy=ymin,ymax DO distance(xmin:xmax,iy) = xdist2(xmin:xmax) + ydist2(iy)
distance = sqrt(distance)-0.5
totdif   = fltarr( nrad )
npixdif  = fltarr( nrad )

;  Sections for error estimates a la Djorgovski and King

if N_params() GE 7 then begin

 t8 = fltarr(8,nrad)
 n8 = t8
 octants  = replicate( 255b, sz(1), sz(2) )

 FOR iy=ymin,ymax DO BEGIN
    FOR ix=xmin,xmax DO BEGIN

	iyp = iy-y0
	ixp = ix-x0

	if (ixp GT 0) then begin
		if (iyp GE 0) then begin
			octants(ix,iy) = (iyp GE ixp)
		 endif else begin
			octants(ix,iy) = 6b + (iyp GE -ixp)
		  endelse
	 endif else begin
		if (iyp GT 0) then begin
			octants(ix,iy) = 2b + (iyp LE -ixp)
		 endif else begin
			octants(ix,iy) = 4b + (iyp LE ixp)
		  endelse
	  endelse
    ENDFOR
 ENDFOR
      octants(x0,y0)=99b  ;center pixel not included an any octant
endif
;
IF keyword_set( verbose ) THEN message,/info,'Init. complete, now integrating.'

FOR ir=0, nrad-1 DO BEGIN

   IF keyword_set( verbose ) THEN message,/inf,'Now doing aperture of ' $
                  		 + strtrim(radii(ir),2)+' pixel radius'
   within = distance LE radii(ir)
   if (ir GT 0) then within = within AND (distance GT radii(ir-1))
   IF keyword_set(mask) THEN within = within AND mask

   thisapr = distance( where( within ) )
   fractn = ( (radii(ir)-thisapr) < 1 ) > 0	;Fraction of pixels to count
   totdif(ir) = total( image( where( within ) ) * fractn )
   npixdif(ir) = total( fractn )

   if N_elements( octants ) GT 1 then begin
    FOR ioct = 0,7 DO BEGIN
      section = where( within AND (octants EQ byte(ioct) ), ns )
      if (ns GT 0) then begin
        thisoct = distance(section)
        fractn = ( (radii(ir)-thisoct) < 1 ) > 0
        IF keyword_set( verbose ) THEN message,/inf, $
         'Octant '+strtrim(ioct,2)+' with '+strtrim(n_elements(section),2) $
         +' pixels'
        t8(ioct,ir) = total( image(section) * fractn )
        n8(ioct,ir) = total( fractn )
       endif
     ENDFOR
    endif

ENDFOR

IF keyword_set( verbose ) THEN message,/inf,bell+'Done.'
RETURN
END
