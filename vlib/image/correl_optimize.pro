;+
; NAME:
;	correl_optimize
; PURPOSE:
;	Assuming the images are two detections of the same scene,
;	find the optimal (x,y) pixel offset of image_B relative to image_A,
;	by means of maximizing the cross-correlation function.
;	Note that bad pixels or extremely noisy data will bias the
;	cross-correlation function and give possibly erroneous results.
;	Small amount of noise is no problem, pre-filtering is recommended,
;	returned variable max_correl can be used to decide validity.
;	Cross-correlation is computed directly by default, but if
;	keyword /FFT is set it will be computed using FFTs of images.
; CALLING:
;	correl_optimize, image_A, image_B, xoffset, yoffset 
; INPUTS:
;	image_A, image_B = the two images to be analyzed, the offset of
;			image_B relative to image_A is determined.
; KEYWORDS:
;	XOFF_INIT = initial X pixel offset of image_B relative to image_A,
;	YOFF_INIT = Y pixel offset, (default offsets are 0 and 0).
;	MAX_REDUCTION = maximum reduction factor allowed, otherwise default is
;		the first reduction rebins image into about 8 x 8 pixels.
;	MAGNIFICATION = option to determine offsets up to fractional pixels,
;			(example: MAG=2 means 1/2 pixel accuracy, default=2).
;	/NUMPIX : (area fraction of overlap)^(1/4) used as correlation weighting factor.
;	/MONITOR causes the progress of computation to be briefly printed.
;	/PRINT causes the results of analysis to be printed to standard output.
;
;	/FFT : use FFT to compute correlation of images, usually faster,
;		but may not be always accurate because periodic nature of FT
;		may cause confusion (in this case /NUMPIX has no effect).
;		But if data is well inside image boundaries it is accurate.
;
;	/PAD : for FFT option embed images into zero images that are twice size.
;
;	CROPCM = # pixels to ignore (crop) from edges of correlation matrix,
;	        to avoid bad maxima ( if single # then used for both x & y, or specify 2 : [x,y])
; OUTPUTS:
;	xoffset = optimal X pixel offset of image_B relative to image_A.
;	yoffset = optimal Y pixel offset.
;	max_correl = the maximal correlation corresponding to optimal offset.
;	edge = 1 if maximum is at edge of correlation domain, otherwise=0.
;	plateau = 1 if maximum is in a plateau of correlation function, else=0.
; EXTERNAL CALLS:
;	function  correl_images( image_A, image_B )
;	function  convolve( image_A, image_B,/CORREL )	if /FFT is set.
;	pro  corrmat_analyze
;	IDL system variable !DEBUG should be defined.
; PROCEDURE:
;	The combination of function correl_images( image_A, image_B ) and
;	pro corrmat_analyze is used to obtain the (x,y) offset
;	yielding maximal correlation. The combination is first executed at
;	large REDUCTION factor (to speed up computation) and with
;	all possible shifts to find the approximate optimal (x,y) offsets.
;	The reduction factor is then decreased by factors of 2 and
;	the x & y shifts are kept small, thus zooming into the optimal offsets.
;	Finally, the MAGNIFICATION option (if specified)
;	is executed to determine the (x,y) offset up to fractional pixels.
;	If /FFT is set then just use function convolve( image_A,image_B,/CORR )
;	and pro corrmat_analyze to obtain the optimal (x,y) offset,
;	rebinning the images first if magnification is greater than one.
; NOTES:
;	Recommend that the images be normalized so that maximim=1,
;	and thresholded at some uniform percent of max=1.
;	See for example function modify_image.
; HISTORY:
;	Written: Frank Varosi, NASA/GSFC, 1991.
;	Mod, FV, UF, 2012, fixed descript of /NUMPIX, changed (*) to [*].
;	F.V. UF. 2015: added option to crop the correlation matrix to avoid edges.
;	F.V. UF. 2021: pad/crop images to always have even # pixels per dimension.
;	F.V. UF. 2021: if images not same size embed in maximal zero images.
;	F.V. UF. 2021: keyword /PAD embeds image in larger zero image twice size.
;-

pro correl_optimize, image_A, image_B, xoffset, yoffset, max_correl, edge, FFT=use_FFT, $
					XOFF_INIT = xoff_init, $
					YOFF_INIT = yoff_init, $
					PRINT=print, MONITOR=monitor, PAD=pad, $
					NUMPIX=numpix, MAGNIFICATION=Magf, CROPCM=cropcm, $
					MAX_REDUCTION = maxreducf
	szimA = size( image_A )
	szimB = size( image_B )

	if (szimA[0] ne 2) OR (szimB[0] ne 2) then begin
		message,"ERR: first two input arguments must be images.",/INFO
		return
	   endif

	if N_elements( xoff_init ) NE 1 then xoff_init=0
	if N_elements( yoff_init ) NE 1 then yoff_init=0
	xoff = xoff_init
	yoff = yoff_init

;This is option to use FFT and then return:

        if keyword_set( use_FFT ) then begin

           if max( abs( szimA[1:2] - szimB[1:2] ) ) ne 0 then begin

              ;; embed images into images of same size padded even bigger:
              npix = 2 * max( szimA[1:2] > szimB[1:2] )
              imgAsup = fltarr( npix, npix )
              imgBsup = fltarr( npix, npix )
              xia = (npix - szimA[1]) / 2
              yia = (npix - szimA[2]) / 2
              imgAsup[xia,yia] = image_A
              xib = (npix - szimB[1]) / 2
              yib = (npix - szimB[2]) / 2
              imgBsup[xib,yib] = image_B
              xoff += (xib - xia)
              yoff += (yib - yia)
              correl_optimize, imgAsup, imgBsup, xoffset, yoffset, max_correl, edge, /FFT, $
                               XOFF_IN=xoff, YOFF_IN=yoff, PRIN=print, CROP=cropcm
           endif else begin
              
              ;; Always use even number pixel sized images
              ;; (note that at this point images are always same size)
              ;;  if not then increase or reduce by one row or column, and call recursively:

              if keyword_set( pad ) then begin
                 npx = 2 * (szimA[1] + (szimA[1] mod 2))
                 npy = 2 * (szimA[2] + (szimA[2] mod 2))
                 imgApad = fltarr( npx, npy )
                 imgBpad = fltarr( npx, npy )
                 imgApad[npx/4,npy/4] = image_A
                 imgBpad[npx/4,npy/4] = image_B
                 correl_optimize, imgApad, imgBpad, xoffset, yoffset, max_correl, edge, /FFT, $
                                  XOFF_IN=xoff, YOFF_IN=yoff, PRIN=print, CROP=cropcm
                 return
              endif

              if( max( szimA[1:2] mod 2 ) ne 0 ) then begin
                 Lpx = szimA[1] - (szimA[1] mod 2) -1
                 Lpy = szimA[2] - (szimA[2] mod 2) -1
                 correl_optimize, image_A[0:Lpx,0:Lpy], image_B[0:Lpx,0:Lpy], xoffset, yoffset,$
                                  max_correl, edge, /FFT, PAD=pad, $
                                  XOFF_IN=xoff, YOFF_IN=yoff, PRIN=print, CROP=cropcm
                 return
              endif

              corrmat_analyze, convolve( image_A, image_B, /CORREL,/NORM ), CROP=cropcm,$
                               xoffset, yoffset, max_correl, edge, PR=print, XOFF=xoff, YOFF=yoff
           endelse

           return
        endif

;This is old method of direct correlations,
; starting at rebinned reduction factors and then higher resolutions:

	reducf = min( [ szimA[1:2], szimB[1:2] ] ) / 8	;Bin average to about 8 by 8 pixel image.
	reducf = 2^round( aLog( reducf )/aLog(2) )	; make it power of 2.
	if N_elements( maxreducf ) EQ 1 then reducf = reducf < maxreducf

	if N_elements( Magf ) NE 1 then Magf=2
	xsiz = szimA[1] > szimB[1]
	ysiz = szimA[2] > szimB[2]
	xshift = xsiz
	yshift = ysiz		;shift over the whole images first correlation.

	while (reducf GT 1) do begin

           corrmat_analyze, correl_images( image_A, image_B, NUM=numpix, $
                                           XOFF=xoff, YOFF=yoff, $
                                           XS=xshift, YS=yshift, $
                                           REDUC=reducf, MONIT=monitor ), $
                            xoff, yoff, XOFF=xoff, YOFF=yoff, PR=print, REDUC=reducf

           xshift = 2*reducf
           yshift = 2*reducf      ;shift over coarse pixel grid to refine
           reducf = reducf/2      ; correlations at increasing resolution.
        endwhile

	if keyword_set( monitor ) then print,"Magnification = 1"

	corrmat_analyze, correl_images( image_A, image_B, XSHIFT=3, YSHIFT=3, $
                                        XOFF=xoff,YOFF=yoff, MON=monitor,NUM=numpix ),$
                         xoffset, yoffset, max_correl, edge, XOFF=xoff, YOFF=yoff, PR=print

	if (Magf GE 2) then begin

           xoff = xoffset      ;refine offsets to
           yoff = yoffset      ; fractional pixels.

           corrmat_analyze, correl_images( image_A, image_B, XOFF=xoff, $
                                           YOFF=yoff, MAG=Magf, MON=monitor ),  $
                            xoffset, yoffset, max_correl, edge, $
                            XOFF=xoff,YOFF=yoff,PR=print, MAG=Magf
        endif
end
