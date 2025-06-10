;+
; NAME:
;
; PURPOSE:
;	Scale image into bytes and rebin the image for purpose of displaying.
;
; CALLING:
;	imscaled = scale_image( image, Magf, MIN=, MAX=, ROT=, /LOG )
;
; INPUTS:
;	image = 2-D array.
;	Magf = magnification factor, integer if greater than one,
;		floating point if less than one (reduction), default = 1.
; KEYWORDS:
;	MINIMUM = default is use the miximum of image.
;	MAXIMUM = default is use the maximum of image.
;	NSIGMA_RANGE = # of standard deviations around mean of image,
;		to use as minimum & maximum range for scaling,
;		used only of non-zero, sets and returns new MIN and MAX values.
;	TOPVAL = default is !D.table-size-2.
;	BOTTOM_VAL = default = 0.
;	LOG10 = display aLog10( image > LOG10 ).
;	POWER = display image^POWER.
;	ROTATION = integer, 0 to 7, passed to the IDL rotate function.
;	SMOOTH = box width of moving average.
;	/ALL_PIXELS : apply smooth to all pixels by extending image first.
;	/ITERATE : apply smooth(*,3) for (width-1)/2 times, and this iteration
;		approaches convolution by Gaussian of FWHM=sqrt( (width-1)/2 ).
; OUTPUTS:
;	Function returns scaled, optionally magnified, rotated, etc. byte image.
;
; EXTERNAL CALLS:
;	function filter_image
;
; PROCEDURE:
;	Straightforward.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1991, added option to de-magnify with binning if Magf < 1.
;	F.V. 1991, added smoothing option using filter_image.
;	F.V. 1992, added geometric scaling (image^power) with keyword POWER=
; 		and keyword option BOTTOM= to limit min of bytscl.
;       F.V. 2009,  separated the Magf < 1 and >= 1 cases,
;               if keyword SMOOTH is used, call scale_image after filtering.
;       F.V. 2010, fixed bug introduced in 2009: forgot Magf after filtering.
;       F.V. 2011, added keyword option REDUCF for reduction factor.
;       F.V. 2012, added keyword option MAGNIF if arg Magf not given.
;       F.V. 2012, added keyword option /ZSCALE, used if MIN or MAX not given.
;       F.V. 2016, added maximum Limit on Magf to avoid using too much memory.
;	F.V. 2017: added median filter option.
;-

function scale_image, image, Magf, REDUCF=reducf, MINIMUM=mink, MAXIMUM=maxk, LOG10=Logmin, $
				TOPVAL=topval, BOTTOM_VAL=bottom, RANGE_MEDIAN=ranmed, $
				SMOOTH=smooth, ALL_PIXELS=all, MEDIAN=medfbox,      $
				ITERATE_SMOOTH=iter, ROTATION=rotim, ZSCALE=zscale, $
				POWER_SCALE=power, NSIGMA_RANGE=nsigma, MAGNIFICATION=magnif

        sim = size( image )

        if( sim[0] LT 1 ) then begin
           message,/INFO,"First arg. should be an array."
           return,0
        endif

	if N_elements( mink ) EQ 1 then mini = mink
	if N_elements( maxk ) EQ 1 then maxi = maxk

	if N_elements( Magf ) NE 1 then begin
           if keyword_set( magnif ) then Magf = magnif else Magf=1
        endif

	if N_elements( Magf ) eq 1 then begin
           maxMagf =  (5000./max(sim[1:sim[0]]) ) > 2
           if( Magf gt maxMagf ) then Magf = maxMagf
        endif

	if N_elements( topval ) NE 1 then topval = !D.table_size-2
	topval = (topval>0) < 255
	if N_elements( bottom ) NE 1 then bottom = 0
	bottom = byte( (bottom>0) < (topval-1) )

        if keyword_set( nsigma ) then begin
           delta = nsigma * stdev( image, mean )
           mini = mean - delta
           maxi = mean + delta
           mink = mini
           maxk = maxi
        endif else if keyword_set( ranmed ) then begin
           maxi = max( median( image, ranmed > 3 ), MIN=minv )
           if N_elements( mini ) NE 1 then mini = minv
           mink = mini
           maxk = maxi
        endif

        if keyword_set( smooth ) OR keyword_set( medfbox ) then begin

           return, scale_image( filter_image( image,SMO=smooth,AL=all,IT=iter,MED=medfbox ),$
                                Magf, MIN=mink, MAX=maxk, LOG10=Logmin, ROT=rotim, $
                                TOP=topval, BOT=bottom, POW=power,NSIG=nsigma,ZSCAL=zscale )

        endif else if keyword_set( sigmafbox ) then begin

           return, scale_image( sigma_filter( image, sigmafbox, ALL=all, ITER=iter ), Magf,$
                                MIN=mink, MAX=maxk, LOG10=Logmin, ROT=rotim, $
                                TOP=topval, BOT=bottom, POW=power,NSIG=nsigma,ZSCAL=zscale )
        endif


        if (Magf LT 1) or keyword_set( reducf ) then begin

           if N_elements( reducf ) ne 1 then reducf = round( 1.0/(Magf > 0.1) )
           sr = fix( sim/reducf )
           L = sr * reducf

            if max( abs( L - sr ) ) gt 0 then begin
                L = L-1
                ims = rebin( image[ 0:L[1], 0:L[2] ], sr[1], sr[2] )
            endif else ims = rebin( image, sr[1], sr[2] )

            if N_elements( maxi ) NE 1 then begin
               if keyword_set( zscale ) then begin
                  zscran = Zscale_Range( ims, NIT=(zscale>1) )
                  maxi = zscran[1]
                  if N_elements( mini ) NE 1 then mini = zscran[0]
               endif else begin
                  maxi = max( ims, MIN=minv )
                  if N_elements( mini ) NE 1 then mini=minv
               endelse
            endif else if N_elements( mini ) NE 1 then begin
               if keyword_set( zscale ) then begin
                  zscran = Zscale_Range( ims, NIT=(zscale>1) )
                  mini = zscran[0]
               endif else mini = min( ims )
            endif

            mink = mini
            maxk = maxi

            if keyword_set( power ) then begin

                if (power NE 1) then begin
                    ims = ( ims - mini )/float( maxi - mini )
                    imdisp = bytscl( (ims>0)^power, MIN=0, MAX=1, TOP=topval-bottom )
                endif else imdisp = bytscl( ims, MIN=mini, MAX=maxi, TOP=topval-bottom )

            endif else if keyword_set( Logmin ) then begin

                Logmin = Logmin > 1.e-39
                mink = (mini > Logmin)
                imdisp = bytscl( aLog10( ims>Logmin ), TOP=topval-bottom, $
						      MIN=aLog10(mini>Logmin), $
						      MAX=aLog10(maxi>Logmin)  )

            endif else imdisp = bytscl( ims, MIN=mini, MAX=maxi, TOP=topval-bottom )

        endif else begin

            if N_elements( maxi ) NE 1 then begin
               if keyword_set( zscale ) then begin
                  zscran = Zscale_Range( image, NIT=(zscale>1) )
                  maxi = zscran[1]
                  if N_elements( mini ) NE 1 then mini = zscran[0]
               endif else begin
                  maxi = max( image, MIN=minv )
                  if N_elements( mini ) NE 1 then mini=minv
               endelse
            endif else if N_elements( mini ) NE 1 then begin
               if keyword_set( zscale ) then begin
                  zscran = Zscale_Range( image, NIT=(zscale>1) )
                  mini = zscran[0]
               endif else mini = min( image )
            endif

            mink = mini
            maxk = maxi

            if keyword_set( power ) then begin

                if (power NE 1) then begin
                    ims = ( image - mini )/float( maxi - mini )
                    imdisp = bytscl( (ims>0)^power, MIN=0, MAX=1, TOP=topval-bottom )
                endif else imdisp = bytscl( image, MIN=mini, MAX=maxi, TOP=topval-bottom )

            endif else if keyword_set( Logmin ) then begin

                Logmin = Logmin > 1.e-39
                mink = (mini > Logmin)
                imdisp = bytscl( aLog10( image>Logmin ), TOP=topval-bottom, $
                                                      MIN=aLog10(mini>Logmin), $
						      MAX=aLog10(maxi>Logmin)  )

            endif else imdisp = bytscl( image, MIN=mini, MAX=maxi, TOP=topval-bottom )

        endelse

	if (bottom GT 0) then   imdisp = imdisp + bottom   else begin
		if max( imdisp ) LE 0 then imdisp = imdisp + 1b
	   endelse

	if (Magf GT 1) then begin
            Magf = round( Magf )
            s = size( imdisp )
            imdisp = rebin( imdisp, s[1]*Magf,s[2]*Magf, /SAMPLE )
        endif

	if keyword_set( rotim ) then return, rotate( imdisp, rotim ) $
				else return, imdisp
end
