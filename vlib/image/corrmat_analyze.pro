;+
; NAME:
;	corrmat_analyze
; PURPOSE:
;	Analyze the 2-D cross-correlation function of two images
;	and find the optimal (x,y) pixel offsets.
;	Intended to analyze to result of function CORREL_IMAGES,
;	and called by pro CORREL_OPTIMIZE.
; CALLING:
;	corrmat_analyze, correl_mat, xoffset_optimum, yoffset_optimum, [...]
; INPUTS:
;	correl_mat = cross-correlation matrix of 2 images.
;		(as computed by function CORREL_IMAGES( imA, imB ) or
;		 function CONVOLVE( imA, imB, /CORREL ) ).
; NOTE:
;	If correl_mat[*,*,1] is the number of pixels for each correlation,
;	(the case when /NUMPIX was specified in call to CORREL_IMAGES)
;	then (# overlap pixels)^(1/4) is used as correlation weighting factor.
; KEYWORDS:
;	XOFF_INIT = initial X pixel offset of image_B relative to image_A.
;	YOFF_INIT = Y pixel offset, (both as specified to correl_images).
;	/PRINT causes the result of analysis to be printed.
;	CROPCM = # pixels to ignore (crop) from edges of correlation matrix,
;	        to avoid bad maxima ( if single # then used for both x & y, or specify 2 : [x,y])
; OUTPUTS:
;	xoffset_optimum = optimal X pixel offset of image_B relative to image_A.
;	yoffset_optimum = optimal Y pixel offset.
;	max_correl = the maximal correlation corresponding to optimal offset.
;	edge = 1 if maximum is at edge of correlation domain, otherwise=0.
; EXTERNAL CALLS:
;	IDL system variable !DEBUG should be defined.
; PROCEDURE:
;	Find point of maximum cross-correlation and calc. corresponding offsets.
;	Uses DAOPHOT style function centroid to find the max correlation.
; HISTORY:
;	Written: Frank Varosi, NASA/GSFC, 1991.
;	Mod, FV, UF-astro, 2015, use centroid method to find max correlation offsets.
;	F.V. UF. 2015: option to crop the correlation matrix to	ignore values at edges.
;	F.V. UF. 2022: bug fix: compute x_shift and y_shift as floating point (divide by 2.0)
;-

pro corrmat_analyze, correl_mat, xoffset_optimum, yoffset_optimum, max_correl, edge, $
				XOFF_INIT = xoff_init, $
				YOFF_INIT = yoff_init, PRINT = print, CROPCM=crop, $
				REDUCTION = reducf, MAGNIFICATION = Magf
	scm = size( correl_mat )

        if (scm[0] GE 3) then begin ;weight by # of overlap pixels:
           npix_mat = correl_mat[*,*,1]
           corrmat_analyze, correl_mat[*,*,0] * sqrt( sqrt( npix_mat / max( npix_mat ) ) ), $
                            xoffset_optimum, yoffset_optimum, max_correl, edge, $
                            XOFF_INIT = xoff_init, $
                            YOFF_INIT = yoff_init, PR=print, RED=reducf, MAG=Magf
           return
        endif

        if (scm[0] LT 2) then begin
           message,"first argument must be at least 2-D matrix",/INFO,/CON
           return
        endif

	Nx = scm[1]
	Ny = scm[2]
	x_shift = Nx/2.0
	y_shift = Ny/2.0
	if N_elements( xoff_init ) NE 1 then xoff_init=0
        if N_elements( yoff_init ) NE 1 then yoff_init=0

        if N_elements( crop ) gt 0 then begin
           if N_elements( crop ) eq 1 then crop = [crop,crop]
           corrmat_analyze, correl_mat[crop[0]:Nx-crop[0]-1,crop[1]:Ny-crop[1]-1], $
                            xoffset_optimum, yoffset_optimum, max_correl, edge, $
                            XOFF_INIT = xoff_init, $
                            YOFF_INIT = yoff_init, PR=print, RED=reducf, MAG=Magf
           xoffset_optimum += crop[0]
           yoffset_optimum += crop[1]
           return
        endif

        ; now analyze the cropped correlation matrix:
        max_correl = max( correl_mat, maxLoc, MIN=mincm )
        szcmc = size( correl_mat )
	ximx = (maxLoc MOD szcmc[1])
	yimx = (maxLoc/szcmc[1])
	edge=0
	if N_elements( Magf ) NE 1 then Magf=1
	if N_elements( reducf ) NE 1 then reducf=1

        centroid, correl_mat - mincm, cx, cy, errcode  ;; find centroid to find max correlation

;;        fpm =  fitPSF( correl_mat - mincm,/GAUSS )

        if N_struct( fpm ) eq 1 then begin
           if sqrt( total( ([cx,cy] - [fpm.cx,fpm.cy])^2 )) LT 1 then begin
              cx = fpm.cx
              cy = fpm.cy
           endif
        endif

        if errcode then begin
           xpk = ximx
           ypk = yimx
        endif ; else begin
           xpk = float( cx - 0.5 )
           ypk = float( cy - 0.5 )
;        endelse

        if ( reducf GT 1 ) then begin

           xoffset_optimum = ( xpk - x_shift + xoff_init/reducf ) * reducf
           yoffset_optimum = ( ypk - y_shift + yoff_init/reducf ) * reducf
           format = "(2i5)"

        endif else if ( Magf GT 1 ) then begin

           xoffset_optimum = xoff_init + float( xpk - x_shift )/Magf
           yoffset_optimum = yoff_init + float( ypk - y_shift )/Magf
           format = "(2f9.3)"

        endif else begin

           xoffset_optimum = xpk - x_shift + xoff_init
           yoffset_optimum = ypk - y_shift + yoff_init
           format = "(2f9.3)"
        endelse

	if (xpk EQ 0) OR (xpk EQ Nx-1) OR $
	   (ypk EQ 0) OR (ypk EQ Ny-1) then edge=1

	if keyword_set( print ) then begin

           print," Correlation min = ", strtrim( mincm, 2 ), $
                 " : MAX = ", strtrim( max_correl, 2 ), $
                 " : at (x,y) offset:", $
                 string( [ xoffset_optimum, yoffset_optimum ], FORM=format )

           if (edge) AND (!DEBUG) then begin
              print," Maximum is at EDGE of shift range, result is inconclusive"
              print," try larger shift or new starting offset"
           endif
        endif

        return
end
