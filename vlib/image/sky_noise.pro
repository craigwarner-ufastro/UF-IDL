;+
; NAME:
;	sky_noise
;
; PURPOSE:
;	Estimate the standard deviation of Gaussian noise in background dominated image,
;	(for example ground based mid-infrared images for which SNR < 1000 ).
;	Estimate is robust even if object signal is thoughout the image,
;	as long as Poisson noise of signal is less than Gaussian noise of background.
;	Method is to compute the sums of Local Variances for moving box of pixels,
;	then histogram the array of local variance sums (iterate to get optimal binsize),
;	and fit with Chi-Square distribution to get estimate of the std. dev. of Guassian noise.
;	( Sum of squares of Gaussian random variables has Chi-square distribution,
;	with mean param. r = # of RVs summed, and mean and std. dev. params. of Gaussians.
;	If std. dev. of bkgd. Gaussian noise = 1 then sum of variances in a box
;	has Chi-Square dist. with mean = # pixels in box, i.e. 3x3 = 9, 5x5 = 25, etc.)
;	After getting std. dev., the sky level (average background) is estimated by
;	fitting a Gaussian with fixed st.dev. (to value found) to the histogram of the image.
;
; CALLING:
;	sigma = sky_noise( image, sky_Level, BOX_WIDTH= )
;
; INPUTS:
;	image = 2D array of data.
;
; OUTPUTS:
;	sky_Level = the average value of the background in image,
;		optional, computed only if parameter is present.
;
;	Function returns the most probable standard deviation of noise in image.
;
; KEYWORDS:
;
;	BOX_WIDTH = the width of box around each image pixel
;		in which to compute local variances, default = 3.
;
;	SIGMA_SET = if specficied, this value is used as st.dev. of noise
;		and routine skips to determine the sky_Level.
;
;	/PLOT : create windows with graphs of Histogram of Local Deviations
;		and Histogram of Data, and overplot with derived results.
;		(wait time between plots is PLOT/10 seconds).
;
;	/VERBOSE : print information during computations.
;
; KEYWORD OPTIONAL OUTPUTS:
;
;	LOCAL_VARIANCE = the image of local variance sums of data image (no sqrt).
;	HISTO_VARIANCE = histogram of local variance image
;	VALUE_VARIANCE = values corresponding to local variance histogram
;	IMAGE_HISTOGRAM = histogram of image
;	IMAGE_VALUES = values corresponding to image histogram
;
; EXTERNAL CALLS:
;	function Local_Variance
;	function histo
;	function chisqdist
;	function gaussian
;	pro non_Lin_Lsq
;
; SYSTEM VARIABLES:
;	if !DEBUG GT 3 then histogram plots are produced in new windows.
; COMMON BLOCKS:
;	common sky_noise, winp1, winp2  ;windows for plots of histograms and fits.
; PROCEDURE:
;	Compute the sums of Local Variances in moving box of pixels,
;	then histogram the array of local variance sums (iterate to get optimal binsize).
;	Fit with Chi-Square distribution to get estimate for the std. dev. of Guassian noise.
;	( Sum of squares of Gaussian random variables has Chi-square distribution,
;	with mean param. r = # of RVs summed, and mean and std. dev. params. of Gaussians.
;	If std. dev. of bkgd. Gaussian noise = 1 then sum of variances in a box
;	has Chi-Square dist. with mean = # pixels in box, i.e. 3x3 = 9, 5x5 = 25, etc.)
;	After getting std. dev., the sky level (average background) is estimated by
;	fitting a Gaussian with fixed st.dev. (to value found) to the histogram of the image.
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992 (first time modeling local std. devations)
;	F.V. 1995, added !DEBUG and /PLOT options.
;	F.V.@UF 2012 converted to model sum of variances with Chi-Square distribution.
;	F.V.@UF 2016 handle bad case of flat image, handle input of array of images.
;	F.V.@UF 2017 fixed mistake in histogram of data, for rare case of really strange image.
;	F.V.@UF 2022 fixed another bug (ivstop) in iter to get histogram of data to be fit.
;-

function sky_noise, image, sky_Level, BOX_WIDTH=box_width, SIGMA_SET=sigma_set, PLOT=hplots, $
					LOCAL_VARIANCE=imvars, VERBOSE=verbose, COADDS=coadds,$
					HISTO_VARIANCE=imvh, VALUE_VARIANCE=imvv,  $
					IMAGE_HISTOGRAM=imh, IMAGE_VALUES=imv, NAME=dname
  common sky_noise, winp1, winp2

  szim = size( image )

  if szim[0] eq 3 then begin
     nim = szim[3]
     stdevs = fltarr(nim)
     sky_Level = fltarr(nim)
     for i=0,nim-1 do begin
        stdevs[i] = sky_noise( image[*,*,i], sky, BOX=box_width, SIG=sigma_set, $
                               PL=hplots, VER=verbose, NAM=dname, COADDS=coadds )
        sky_Level[i] = sky
     endfor
     return, stdevs
  endif

        winsav = !D.window
        if vartype( dname,/CODE ) ne 7 then dname="" else dname = dname + ": "

        if szim[0] ne 2 then begin
           message,/INFO,"First arg. must be image data (2D array)"
           sky_Level = -1
           return,(-1)
        endif

	if keyword_set( sigma_set ) then begin
		sigma_noise = sigma_set
		goto,SKY
	   endif

	if N_elements( box_width ) NE 1 then box_width = 3
	box_width = ( 2 * (fix( box_width )/2) + 1 ) > 3   ;;make sure it is odd number
        npixbox = box_width * box_width

        if keyword_set( coadds ) then begin
           imvars = Local_Variance( image/float(coadds), BOX_WID=box_width,/TRIM ) * npixbox
        endif else imvars = Local_Variance( image, BOX_WID=box_width,/TRIM ) * npixbox

        nbin = 100
	imvh = histo( imvars, imvv, NBIN=nbin, MIN=0 )
	imvh[0]=0
	maxh = max( imvh, imax )
        maxv = imvv[nbin-1]
	imok = 11
	npf = 0.01
        npixim = N_elements( imvars )
	if N_elements( hplots ) ne 1 AND (!DEBUG gt 3 ) then hplots = !DEBUG - 3
        iter=0

	if keyword_set( hplots ) then begin
           get_window, winp1, XIS=500,YIS=400,/SHOW
           if( hplots gt 1 ) then begin
              if keyword_set( verbose ) then print,0b,": imax=",fix(imax),", mode=",imvv[imax],$
                                                   ", Hmax=",maxh,", Vran=",maxv, $
                                                   ", nbin=",nbin,", bfac=",1
              np = ( 4*imax ) < (N_elements( imvv )-1)
              if( np gt 120 ) then psym=4 else psym=10
              plot,imvv[0:np],imvh[0:np],PSYM=psym
              wait,hplots/10.
           endif
        endif

	if (maxh LT npixim*npf) OR (imax LT imok) then begin

           ;; iterate changing nbins and max to get accurate histrogram of Local variance sums:
           maxv = imvv[nbin-1]
           nbinp = 0

           while ( (maxh LT npixim*npf) OR (imax LT imok) ) $
              AND (iter LT 22) AND (nbinp NE nbin) do begin

              iter = iter+1
              bfac = 1 + 3/aLog( nbin>2 )
              nbinp = nbin
              if (imax LT imok) then nbin = bfac*nbin $
              else if (maxh LT npixim*npf) then nbin = nbin/bfac
              nbin = Long( nbin < 200000 ) > 20
              if( imax LT 3 ) then maxv = imvv[(imax+11) < (N_elements(imvv)-1)]
              imvh = histo( imvars, imvv, NBIN=nbin, MIN=0, MAX=maxv )
              imvh[0]=0
              maxh = max( imvh, imax )

              if keyword_set( hplots ) then begin
                 if( hplots gt 1 ) then begin
                    np = ( 4*imax ) < (N_elements( imvv )-1)
                    if( np gt 120 ) then psym=4 else psym=10
                    plot,imvv[0:np],imvh[0:np],PSYM=psym,/XSTY,XRAN=[0,imvv[np]]
                    if keyword_set( verbose ) then print,byte(iter),": imax=",fix(imax),$
                                                         ", mode=",imvv[imax],$
                                                         ", Hmax=",maxh,", Vran=",maxv,$
                                                         ", nbin=",nbin,", bfac=",bfac
                    wait,hplots/10.
                 endif
              endif
           endwhile
        endif

	if keyword_set( hplots ) then begin
           np = ( 4*imax ) < (N_elements( imvv )-1)
           if( np gt 120 ) then begin
              psym=4
              symsiz=0.3
           endif else psym=10
           plot,imvv[0:np],imvh[0:np],PSYM=psym,SYMSIZ=symsiz, XTIT="Local Variance Sums", $
                YTIT="Frequency", TIT=dname + "Histogram of Variances (Box=" + $
                strtrim( box_width, 2 ) + ") + Chi-square Fit",/XSTY,XRAN=[0,imvv[np]]
           if keyword_set( verbose ) then print,byte(iter),": imax=",fix(imax),$
                                                ", mode=",imvv[imax],$
                                                ", Hmax=",maxh,", Vran=",maxv,", nbin=",nbin
           if !DEBUG GT 4 then stop
        endif

;;Fit Chi-Square distribution to histogram of Local variance sums:

	if (imax gt 1) then begin

           rm2 = double( npixbox - 2 )               ;; chi-sq dist. attains max at npixbox - 2 
           parmg = [ 1, npixbox, npixbox/imvv[imax] ]
           np = ( 4*imax ) < (N_elements( imvv )-1)  ;;use only relevant part of histogram
           vx = imvv[0:np]
           parmg[0] = maxh/max( chisqdist( vx, parmg ) )   ;;scale by max's to match histogram
           if !DEBUG GT 4 then stop
           npf = ( 2*imax ) < (N_elements( imvv )-1)  ;;use smaller part of histogram for fit.

           non_Lin_Lsq, vx[0:npf], imvh[0:npf], parmg, pgsig, chisqfit, $
                        FUNC="chisqdist", PAR_FIX=1, INFO=verbose

           if keyword_set( verbose ) then begin
              print,"Chi-square dist. fit to Local variance sums:",parmg
              print,"goodness of fit = ",chisqfit
           endif

           if (parmg[2] GT 0) then begin
              sigma_noise = 1/sqrt(parmg[2])
              if keyword_set( hplots ) then begin
                 oplot, vx, chisqdist( vx, parmg ), LINE=3
                 oplot, [vx[npf],vx[npf]], !y.crange, LINE=1
              endif
           endif else sigma_noise = sqrt(imvv[imax]/npixbox)

           if keyword_set( hplots ) then begin
              if keyword_set(verbose) then $
                 print,dname+"Estimate of bkgnd. Gaussian noise st.dev.=",sigma_noise
              oplot, replicate( (npixbox-2) * sigma_noise^2, 2 ), !y.crange, LINE=2
              if !DEBUG GT 4 then stop
              if (winsav GE 0) then wset,winsav
           endif

        endif else begin  ;; could not find any noise

           if keyword_set( hplots ) then begin
              if (winsav GE 0) then wset,winsav
              if keyword_set( verbose ) then $
                 print,"Failed to estimate of bkgnd. Gaussian noise st.dev."
           endif
           sigma_noise = 0
           sky_Level = 0
           return, sigma_noise
        endelse

        if keyword_set( coadds ) then sigma_noise *= sqrt( coadds )
        if N_params() LT 2 then return, sigma_noise
SKY:
        if keyword_set( coadds ) then begin
           imh = histo( image/float(coadds), imv, BINSIZE=(sigma_noise/7), NBIN=22000, MAX=vmax)
        endif else imh = histo( image, imv, BINSIZE=(sigma_noise/7), NBIN=22000 )

        vmax = imv[N_elements( imv )-1]
        hmax = max( imh, imax )
        ivstop = median( image ) + avg( image ) + 10 * sigma_noise + (imv[imax] > 0)
        w = where( imv ge ivstop, nw )

        if( nw gt 0 ) then mmax = w[0] else mmax = N_elements(imv)-1
        if !DEBUG GT 2 then help,vmax,hmax,imax,ivstop,mmax
        iter=0
        if !DEBUG GT 4 then stop

        while (iter LT 22) and (vmax gt ivstop) do begin
           ;; iterate changing range of data to get accurate histrogram of data noise:
           vmax = vmax/2
           if keyword_set( coadds ) then begin
              imh = histo( image/float(coadds), imv, BINSIZ=(sigma_noise/7),NBIN=22000,MAX=vmax)
           endif else imh = histo( image, imv, BINSIZE=(sigma_noise/7), NBIN=22000, MAX=vmax )
           hmax = max( imh, imax )
           w = where( imv ge ivstop, nw )
           if( nw gt 0 ) then mmax = w[0] else mmax = N_elements(imv)-1
           if !DEBUG GT 2 then help,vmax,ivstop,mmax,hmax,imax,imv,imh
           iter = iter + 1
           if keyword_set( hplots ) then begin
              get_window, winp2, XIS=500,YIS=400,/SHOW
              np = ( 2*mmax ) < (N_elements( imv )-1)
              plot, imv[0:np], imh[0:np]>1, /YLOG,$
                    XTIT="Value", YTIT="Frequency",TIT="Histogram of Data", PS=10
              wait,hplots/10.
           endif
           if !DEBUG GT 4 then stop
        endwhile

;;Fit Gaussian distribution to histogram of image data:

        if( imax LE 3 ) then imax = mmax
	parmg = [ hmax, imv[imax], sigma_noise ]	;guess factor and mean,
        np = ( 3 * imax ) < (N_elements( imv )-1)	; fit to histogram around peak.

	non_Lin_Lsq, imv[0:np], imh[0:np], parmg, pgsig, chisqfit, FUNC="gaussian", PAR_FIX=2
	sky_Level = parmg[1]	;estimate of gaussian mean (sky background).

	if keyword_set( hplots ) then begin
           if keyword_set( verbose ) then begin
              print,"Gaussian fit to image histogram gives bkgnd.=",sky_Level
              print,"goodness of fit = ",chisqfit
              help,ivstop
           endif
           get_window, winp2, XIS=500,YIS=400,/SHOW
           plot, imv[0:np], imh[0:np] > 1, $
                 XTIT="image Values", YTIT="Frequency", /XSTYLE, PS=10,/YLOG,$
                 TIT=dname+"Data Histogram + Gaussian Fit: Sigma="+$
                 strtrim(string(sigma_noise,FORM="(G9.3)"),2)
           oplot, imv[0:np], gaussian( imv[0:np], parmg ), LINE=3
           oplot, replicate( sky_Level, 2 ), 10^!y.crange, LINE=1
           if (winsav GE 0) then wset,winsav
        endif

        if !DEBUG GT 4 then stop

        return, sigma_noise
end
