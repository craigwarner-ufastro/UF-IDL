;+
; NAME: 
;     centroid
; PURPOSE:
;     Compute centroid coordinates of a stellar image as in DAOPHOT FIND.
; CALLING SEQUENCE: 
;     centroid, image, XCEN, YCEN, XY_PEAK=[x,y], [FWHM=fwhm], [/PEAK_LOC]
; INPUTS:     
;     image - Two dimensional image array
; KEYWORDS:
;     XY_PEAK = [x,y] vector of 2 integers giving approximate stellar center.
;		Pixel at maximum in subimage will then be used to start.
;		If not specified then max pixel of whole image is used.
;     FWHM - floating scalar or 2 elements (for x & y), default FWHM = 3 pixels.
;		Centroid computed in box of half width = 1.5*sigma = 0.637*FWHM
;    /PEAK_LOCATE - causes the peak (maximum pixel) of image to be used
;		as approximate stellar center (overrides XY_PEAK).
; OUTPUTS:   
;     XCEN - floating scalar, giving the computed X centroid position
;     YCEN - floating scalar, giving the computed Y centroid position
;          Values for XCEN and YCEN will not be computed if the
;          centroid falls outside of the box, or if the computed derivatives!
;          are non-decreasing.   If the centroid cannot be computed, then a 
;          message is displayed and XCEN and YCEN are set to -1.
; RESTRICTIONS: 
;    Program does not check if the input X and Y position falls within image.
; SYSTEM VARIABLES:
;    !DEBUG - If set, the subarray used to compute the centroid is printed
; EXTERNAL CALLS:
;		pro Locate_Peak, image, xmax, ymax
; PROCEDURE: 
;    Maximum pixel within distance from input pixel X, Y  determined 
;    by FHWM is found and used as the center of a square, within 
;    which the centroid is computed as the value (XCEN,YCEN) at which 
;    the derivatives of the partial sums of the input image over (y,x),
;    with respect to (x,y), equal zero.
; MODIFICATION HISTORY:
;    Written 2/25/86, by J. K. Hill, S.A.S.C., following
;    			algorithm used by P. Stetson in DAOPHOT.
;    Added vector subscripting, W. Landsman   5/21/87
;    Mod: F.Varosi 1991-92, changed name from CNTRD to CENTROID, optimized,
;		changed SUM calls to SUM_ARRAY, added keyword XY_PEAK=[x,y],
;		option /PEAK_LOC to locate and use maximum,
;		add 0.5 to centroid x & y values so it is in center of pixel.
;    Edit: FV 1999, added more comments explaining keywords in header info.
;-      

pro centroid, image, Xcen,Ycen, errcode, XY_PEAK=xy_peak, FWHM=fwhm, REGION=region, $
				PEAK_LOCATE=peak_Loc, PLOT_PDERIV=plotd

IF N_PARAMS() LT 1 THEN BEGIN
	PRINT,STRING(7B),$
	"CALL: centroid ,image, xcen, ycen, [ XY_PEAK=[x,y], FWHM=fwhm, /PEAK_LOCATE]"
	print,"CALL: centroid ,image, xcen, ycen, REGION=[ xmin, xmax, ymin, ymax ]"
	RETURN
   ENDIF

errcode = 0
if N_elements( fwhm ) LE 0 then fwhm = [3,3]
if N_elements( fwhm ) eq 1 then fwhm = replicate( fwhm[0], 2 )

NHALF_xy =  fix( 0.637 * fwhm + 0.5 ) > 2
NBOX_xy = 2*NHALF_xy+1	      ;Width of box to be used to compute centroid
sim = size( image )
L = sim-1

if keyword_set( peak_Loc ) then begin

	Locate_peak, image, xmax, ymax
	xy_peak = [xmax,ymax]

 endif else if N_elements( region ) EQ 4 then begin

	Locate_peak, image, xmax, ymax, REGION=region
	xy_peak = [xmax,ymax]

 endif else if N_elements( xy_peak ) EQ 2 then begin

	X = xy_peak[0]		;Locate maximum pixel in subimage
	Y = xy_peak[1]		;  around the given (X,Y) pixel.
	NHALFBIG = NHALF_xy +3	;Extend box 3 pixels on each side
	rX = ( [ X-NHALFBIG[0] , X+NHALFBIG[0] ] > 0 ) < L[1]
	rY = ( [ Y-NHALFBIG[1] , Y+NHALFBIG[1] ] > 0 ) < L[2]
	Locate_Peak, image( rX[0]:rX[1], rY[0]:rY[1] ), xmax, ymax
	XMAX = rX[0] + xmax  	;X coordinate in original image array
	YMAX = rY[0] + ymax  	;Y coordinate in original image array

  endif else begin

	Locate_Peak, image, xmax, ymax
	xy_peak = [xmax,ymax]
	if !DEBUG gt 1 then print," using (x,y) peak: ",xy_peak
   endelse
				;extract subimage centered on maximum pixel
rx = [ XMAX-NHALF_xy[0], XMAX+NHALF_xy[0] ]
ry = [ YMAX-NHALF_xy[1], YMAX+NHALF_xy[1] ]

if (min( [rx,ry] ) LT 0) OR max( ([rx[1],ry[1]] GT L[1:2]) ) then begin
	message,"not enough pixels around maximum to determine centroid",/INFO
        xcen=xmax & ycen=ymax
        errcode = 1
	return
   endif else STARBOX = double( image( rx[0]:rx[1], ry[0]:ry[1] ) )

if !DEBUG gt 1 then begin
	PRINT,' Maximum pixel value is ',image(xmax,ymax),'  at',xmax,ymax
	PRINT,'Subarray used to compute centroid:'
	PRINT,STARBOX
   endif

; Find X centroid

NBOX = NBOX_xy[0]
NHALF = NHALF_xy[0]

DD = findgen( NBOX-1 ) + 0.5 - NHALF	; Weighting factor W unity in center,
W = 1 - (ABS(DD)-0.5)/(2*NHALF-1)	; 0.5 at end, and linear in between
wdfactor = total( W*DD^2 )/total( W )

PDERIV = SHIFT( STARBOX,-1,0 ) - STARBOX    ;get partial derivatives resp. to X
NHALF = NHALF_xy[1]
IR = (NHALF-1) > 1 
PDERIV = PDERIV( 0:NBOX-2, NHALF-IR:NHALF+IR )  ;eliminate edges of the array

PDERIV = W * total( PDERIV, 2 )	     ;Sum X derivatives over Y direction
SUMD   = total( PDERIV )
SUMXD  = total( DD * PDERIV )

IF (SUMXD GE 0) THEN BEGIN
	message,' X derivative not decreasing, could not find centroid',/INFO
	XCEN=-1
  ENDIF else begin
	DX = wdfactor * (SUMD / SUMXD)
	IF (ABS(DX) GT NHALF_xy[0]) THEN $
				message,'computed X centroid outside box',/INFO
	XCEN = XMAX - DX + 0.5		;X centroid in original array
	if keyword_set( plotd ) then begin
		get_window,0,/SHOW
		plot, PDERIV, PS=-4
		oplot, [DX,DX],[-1,1]*1e33
	   endif
   endelse

; Find Y Centroid

NBOX = NBOX_xy[1]
NHALF = NHALF_xy[1]

DD = findgen( NBOX-1 ) + 0.5 - NHALF	; Weighting factor W unity in center,
W = 1 - (ABS(DD)-0.5)/(2*NHALF-1)	; 0.5 at end, and linear in between
wdfactor = total( W*DD^2 )/total( W )

PDERIV = SHIFT( STARBOX,0,-1 ) - STARBOX
NHALF = NHALF_xy[0]
IR = (NHALF-1) > 1 
PDERIV = PDERIV( NHALF-IR:NHALF+IR, 0:NBOX-2 )

PDERIV = W * total( PDERIV, 1 )	     ;Sum Y derivatives over X direction
SUMD =   total( PDERIV )
SUMXD =  total( DD * PDERIV )

IF (SUMXD GE 0) THEN BEGIN
	message,' Y derivative not decreasing, could not find centroid',/INFO
	YCEN=-1
  ENDIF else begin
	DY = wdfactor * (SUMD / SUMXD)
	IF (ABS(DY) GT NHALF_xy[1]) THEN $
				message,'computed Y centroid outside box',/INFO
	YCEN = YMAX - DY + 0.5
	if keyword_set( plotd ) then begin
		get_window,1,/SHOW
		plot, PDERIV, PS=-4
		oplot, [DY,DY],[-1,1]*1e33
		print,dy
	   endif
   endelse

IF !DEBUG THEN PRINT,'  X Y POS. OF CENTROID = ',XCEN,YCEN
if !DEBUG GT 5 then stop

END
