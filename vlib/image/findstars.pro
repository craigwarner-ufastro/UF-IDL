;+
; NAME:
;	findstars
; PURPOSE:
;	Find positive brightness perturbations (i.e stars) in an image 
; EXPLANATION:
;	Also returns centroids and shape parameters (roundness & sharpness).
;	Adapted from 1991 version of DAOPHOT, but does not allow for bad pixels
;       and uses a slightly different centroid algorithm.
;
;       Modified in March 2008 to use marginal Gaussian fits to find centroids       
; CALLING SEQUENCE:
;	findstars, image, x, y, flux, sharp, round, THRESH=,FWHM=,ROUNDLIM=,SHARPLIM=,/PRINT,/SILENT
;
; INPUTS:
;	image - 2 dimensional image array (integer or real) for which one
;		wishes to identify the stars present (or some kind of point-like sources)
;
; INPUT KEYWORDS:
;
;	(procedure will prompt for these parameters if not supplied)
;
;	THRESHOLD = Threshold intensity for a point source (hmin) - should generally 
;		be 3 or 4 sigma above background noise RMS (not the bkgnd value)
;
;	FWHM = full-width half-max to be used in the convolve filter
;
;	SHARPLIM = 2 element vector giving low and high cutoff for the
;		sharpness statistic (Default = [0.2,1.0] ).   Change this
;		default only if the stars have significantly larger or 
;		or smaller concentration than a Gaussian
;
;	ROUNDLIM = 2 element vector giving low and high cutoff for the
;		roundness statistic (Default = [-1.0,1.0] ).   Change this 
;		default only if the stars are significantly elongated.
;
; OPTIONAL KEYWORDS:
;
;	/ASK - ask for param values even if specified.
;
;	/SILENT - Normally, FIND will print out each star that meets all
;		selection criteria.   If the SILENT keyword is set and 
;		non-zero, then this printout is suppressed.
;
;	/PRINT - if set and non-zero then FIND will also write its results to
;		a file find.prt.   Also one can specify a different output file 
;		name by setting PRINT = 'filename'.
; OUTPUTS:
;	x - vector containing x position of all stars identified by FIND
;	y-  vector containing y position of all stars identified by FIND
;	flux - vector containing flux of identified stars as determined
;		by a Gaussian fit.  Fluxes are NOT converted to magnitudes.
;	sharp - vector containing sharpness statistic for identified stars
;	round - vector containing roundness statistic for identified stars
;
; NOTES:
;	(1) The sharpness statistic compares the central pixel to the mean of 
;       the surrounding pixels.   If this difference is greater than the 
;       originally estimated height of the Gaussian or less than 0.2 the height of the
;	Gaussian (for the default values of SHARPLIM) then the star will be rejected. 
;
;       (2) More recent versions of FIND in DAOPHOT allow the possibility of
;       ignoring bad pixels.    Unfortunately, to implement this in IDL
;       would preclude the vectorization made possible with the CONVOL function
;       and would run extremely slowly
;       (you can remove bad pixels with function sigma_filter or pro outlier_filter).
;
;       (3) Modified in March 2008 to use marginal Gaussian distributions to 
;       compute centroid.   (Formerly, find.pro determined centroids by locating
;       where derivatives went to zero -- see cntrd.pro for this algorithm.   
;       This was the method used in very old (~1984) versions of DAOPHOT. )   
;       As discussed in more  detail in the comments to the code, the  centroid
;       computation here is the same as in IRAF DAOFIND but differs slightly 
;       from the current DAOPHOT.
; PROCEDURE CALLS:
;	GETOPT()
; REVISION HISTORY:
;	Written W. Landsman, STX  February, 1987
;	ROUND now an internal function in V3.1   W. Landsman July 1993
;	Change variable name DERIV to DERIVAT    W. Landsman Feb. 1996
;	Use /PRINT keyword instead of TEXTOUT    W. Landsman May  1996
;	Changed loop indices to type LONG       W. Landsman Aug. 1997
;       Replace DATATYPE() with size(/TNAME)   W. Landsman Nov. 2001
;       Fix problem when PRINT= filename   W. Landsman   October 2002
;       Fix problems with >32767 stars   D. Schlegel/W. Landsman Sep. 2004
;       Fix error message when no stars found  S. Carey/W. Landsman Sep 2007
;       Rewrite centroid computation to use marginal Gaussians W. Landsman Mar 2008
;       F.V. at UF 2011 : changed name to findstars and added convenient keywords.
;       F.V. at UF 2018 : do not ask to try another threshold unless /ASK is specified.
;       F.V. at UF 2019 : fixed my stupid bug if flux array is also in params list. 
;-

pro findstars, image, fx, fy, flux, sharp, roundness, THRESHOLD=hmin, FWHM=fwhm, MAXBOX=maxbox, $
               ROUNDLIM=roundlim, SHARPLIM=sharplim, PRINT=printit, SILENT=silent, ASK=askp, $
               VERBOSE=verbose

 compile_opt idl2

 npar   = N_params()
 if npar EQ 0 then begin
    print,'Syntax - FINDSTARS, image,' + $
          '[ x, y, flux, sharp, round, THRESH=, FWHM=, ROUNDLIM=, SHARPLIM=, /ASK,/PRINT,/SILENT]'
    return
 endif

 if N_elements( maxbox ) ne 1 then maxbox = 13 	; max size of convolution box in pixels 

; Get information about the input image 

 type = size(image)
 if ( type[0] NE 2 ) then message,'ERROR - image array (first parameter) must be 2 dimensional'
 n_x  = type[1] & n_y = type[2]
 if keyword_set( verbose ) then $
    message,'image size is '+strtrim(n_x,2) +' by '+ strtrim(n_y,2),/INFO

;Determine if hardcopy output is desired
 doprint = keyword_set( printit )
 if NOT keyword_set( silent ) then silent = 0
 if ( N_elements(fwhm) NE 1 ) then read, 'Enter approximate FWHM: ', fwhm

 radius = 0.637*FWHM > 2.001             ;Radius is 1.5 sigma
 radsq = radius^2
 nhalf = fix(radius) < (maxbox-1)/2
 nbox = 2*nhalf + 1	;# of pixels in side of convolution box 
 middle = nhalf          ;Index of central pixel

 lastro = n_x - nhalf
 lastcl = n_y - nhalf
 sigsq = ( fwhm/2.35482 )^2
 mask = bytarr( nbox, nbox )   ;Mask identifies valid pixels in convolution box 
 g = fltarr( nbox, nbox )      ;g will contain Gaussian convolution kernel

 dd = indgen(nbox-1) + 0.5 - middle	;Constants need to compute ROUND
 dd2 = dd^2

 row2 = (findgen(Nbox)-nhalf)^2

 for i = 0, nhalf do begin
	temp = row2 + i^2
	g[0,nhalf-i] = temp         
        g[0,nhalf+i] = temp                           
 endfor

 mask = fix(g LE radsq)     ;MASK is complementary to SKIP in Stetson's Fortran
 good = where( mask, pixels)  ;Value of c are now equal to distance to center

;  Compute quantities for centroid computations that can be used for all stars
 g = exp(-0.5*g/sigsq)

;  In fitting Gaussians to the marginal sums, pixels will arbitrarily be 
; assigned weights ranging from unity at the corners of the box to 
; NHALF^2 at the center (e.g. if NBOX = 5 or 7, the weights will be
;
;                                 1   2   3   4   3   2   1
;      1   2   3   2   1          2   4   6   8   6   4   2
;      2   4   6   4   2          3   6   9  12   9   6   3
;      3   6   9   6   3          4   8  12  16  12   8   4
;      2   4   6   4   2          3   6   9  12   9   6   3
;      1   2   3   2   1          2   4   6   8   6   4   2
;                                 1   2   3   4   3   2   1
;
; respectively).  This is done to desensitize the derived parameters to 
; possible neighboring, brighter stars.

 xwt = fltarr(nbox,nbox)
 wt = nhalf - abs(findgen(nbox)-nhalf ) + 1
 for i=0,nbox-1 do xwt[0,i] = wt
 ywt = transpose(xwt) 
 sgx = total(g*xwt,1)
 p = total(wt)
 sgy = total(g*ywt,2)
 sumgx = total(wt*sgy)
 sumgy = total(wt*sgx)
 sumgsqy = total(wt*sgy*sgy)
 sumgsqx = total(wt*sgx*sgx)
 vec = nhalf - findgen(nbox) 
 dgdx = sgy*vec
 dgdy = sgx*vec
 sdgdxs = total(wt*dgdx^2)
 sdgdx = total(wt*dgdx) 
 sdgdys = total(wt*dgdy^2)
 sdgdy = total(wt*dgdy) 
 sgdgdx = total(wt*sgy*dgdx)
 sgdgdy = total(wt*sgx*dgdy)

 ck = g*mask          ;Convolution kernel now in ck      
 sumc = total(ck)
 sumcsq = total(ck^2) - sumc^2/pixels
 sumc = sumc/pixels
 ck[good] = (ck[good] - sumc)/sumcsq
 c1 = exp(-.5*row2/sigsq)
 sumc1 = total(c1)/nbox
 sumc1sq = total(c1^2) - sumc1
 c1 = (c1-sumc1)/sumc1sq
 if keyword_set( verbose ) then print,'RELATIVE ERROR computed from FWHM',sqrt(total(ck[good]^2))
 if N_elements(hmin) NE 1 then read,'Enter value above background for detection threshold: ',hmin

 if N_elements(sharplim) NE 2 then begin
    if keyword_set( askp ) then begin
       print,'Enter low and high cutoffs, press [RETURN] for defaults:'
GETSHARP:   
       ans = ''
       read, 'Image Sharpness Statistic (DEFAULT = 0.2,1.0): ', ans   
       if ans EQ '' then sharplim = [0.2,1.0] else begin
          sharplim = getopt(ans,'F')
          if N_elements(sharplim) NE 2 then begin  
             message, 'ERROR - Expecting 2 scalar values',/CON
             goto, GETSHARP     
          endif
       endelse                                                      
    endif else sharplim = [0.2,1.0]
 endif

 if N_elements(roundlim) NE 2 then begin
    if keyword_set( askp ) then begin
GETROUND: 
       ans = ''
       read, 'Image Roundness Statistic [DEFAULT = -1.0,1.0]: ',ans
       if ans EQ '' then roundlim = [-1.,1.] else begin
          roundlim = getopt( ans, 'F' )
          if N_elements( roundlim ) NE 2 then begin
             message,'ERROR - Expecting 2 scalar values',/CON
             goto, GETROUND   
          endif
       endelse
    endif else roundlim = [-1.,1.]
 endif

 if keyword_set( verbose ) then message,'Beginning convolution of image', /INF

 h = convol(float(image),ck)    ;Convolve image with kernel "c"

    h[0:nhalf-1,*] = 0 & h[n_x-nhalf:n_x-1,*] = 0
    h[*,0:nhalf-1] = 0 & h[*,n_y-nhalf:n_y-1] = 0

  if keyword_set( verbose ) then message,'Finished convolution of image', /INF

 mask[middle,middle] = 0	;From now on we exclude the central pixel
 pixels = pixels -1      ;so the number of valid pixels is reduced by 1
 good = where(mask)      ;"good" identifies position of valid pixels
 xx= (good mod nbox) - middle	;x and y coordinate of valid pixels 
 yy = fix(good/nbox) - middle    ;relative to the center
 offset = yy*n_x + xx
SEARCH: 			    ;Threshold dependent search begins here

 index = where( h GE hmin, nfound )  ;Valid image pixels are greater than hmin
 ix = index mod n_x              ;X indices 
 iy = index/n_x                  ;Y indices

 if nfound EQ 0 then begin          ;Any maxima found?
    message,'ERROR - No maxima exceed input threshold of ' + string(hmin,'(F9.1)'),/CON
    goto,FINISH    
 endif

 for i= 0L, pixels-1 do begin                             

	stars = where( h[index] GE h[index+offset[i]], nfound )

        if nfound EQ 0 then begin  ;Do valid local maxima exist?
             message,'ERROR - No maxima exceed input threshold of ' + string(hmin,'(F9.1)'),/CON
             goto,FINISH  
        endif
	index = index[stars]
 endfor 
 
 ix = index mod n_x              ;X index of local maxima
 iy = index/n_x                  ;Y index of local maxima
 ngood = N_elements(index)
 if keyword_set( verbose ) then $
    message,strtrim(ngood,2)+' Local maxima located above threshold',/INFO

 nstar = 0L       	;NSTAR counts all stars meeting selection criteria
 badround = 0L & badsharp=0L  &  badcntrd=0L

 if (npar GE 2) or (doprint) then begin 	;Create output X and Y arrays? 
    fx = fltarr( ngood )
    fy = fx
 endif

 if (npar GE 4) or (doprint) then begin   ;Create output flux,sharpness arrays?
 	flux = fx & sharp = fx & roundness = fx
 endif

 if doprint then begin	;Create output file?

    if ( size(print,/TNAME) NE 'STRING' ) then file = 'find.prt' else file = printit
    message,'Results will be written to a file ' + file,/INF
    openw,lun,file,/GET_LUN
	printf,lun,' Program: FIND '+ systime()
	printf,lun,format='(/A,F7.1)',' Threshold above background:',hmin
	printf,lun,' Approximate FWHM:',fwhm
	printf,lun,format='(2(A,F6.2))',' Sharpness Limits: Low', $
                sharplim[0], '  High',sharplim[1]
	printf,lun,format='(2(A,F6.2))',' Roundness Limits: Low', $
                roundlim[0],'  High',roundlim[1]
	printf,lun,format='(/A,i6)',' No of sources above threshold',ngood
 endif                      

 if NOT keyword_set( silent ) then $
    print,FORM='(13x,a)','STAR        X        Y      FLUX     SHARP    ROUND'

;  Loop over star positions; compute and check statistics

 for i = 0L,ngood-1 do begin   

     temp = float(image[ix[i]-nhalf:ix[i]+nhalf,iy[i]-nhalf:iy[i]+nhalf])
     d = h[ix[i],iy[i]]                  ;"d" is actual pixel intensity        

;  Compute Sharpness statistic

     sharp1 = (temp[middle,middle] - (total(mask*temp))/pixels)/d

     if ( sharp1 LT sharplim[0] ) or ( sharp1 GT sharplim[1] ) then begin
	badsharp = badsharp + 1
	goto, REJECT             ;Does not meet sharpness criteria
     endif

;   Compute Roundness statistic

     dx = total( total(temp,2)*c1)   
     dy = total( total(temp,1)*c1)

     if (dx LE 0) or (dy LE 0) then begin
         badround = badround + 1
	 goto, REJECT           ;Cannot compute roundness
     endif

     around = 2*(dx-dy) / ( dx + dy )    ;Roundness statistic
     if ( around LT roundlim[0] ) or ( around GT roundlim[1] ) then begin
	badround = badround + 1
	goto,REJECT           ;Does not meet roundness criteria
     endif
;
; Centroid computation:   The centroid computation was modified in Mar 2008 and
; now differs from DAOPHOT which multiplies the correction dx by 1/(1+abs(dx)). 
; The DAOPHOT method is more robust (e.g. two different sources will not merge)
; especially in a package where the centroid will be subsequently be 
; redetermined using PSF fitting.   However, it is less accurate, and introduces
; biases in the centroid histogram.   The change here is the same made in the 
; IRAF DAOFIND routine (see http://iraf.net/article.php?story=7211&query=daofind )
;    
     sd = total(temp*ywt,2)

     sumgd = total(wt*sgy*sd)
     sumd = total(wt*sd)
     sddgdx = total(wt*sd*dgdx)

     hx = (sumgd - sumgx*sumd/p) / (sumgsqy - sumgx^2/p)

; HX is the height of the best-fitting marginal Gaussian.   If this is not
; positive then the centroid does not make sense 

     if (hx LE 0) then begin
        badcntrd = badcntrd + 1
        goto, REJECT
     endif

     skylvl = (sumd - hx*sumgx)/p
     dx = (sgdgdx - (sddgdx-sdgdx*(hx*sumgx + skylvl*p)))/(hx*sdgdxs/sigsq)

     if abs(dx) GE nhalf then begin 
        badcntrd = badcntrd + 1
        goto, REJECT
     endif

     xcen = ix[i] + dx          ;X centroid in original array

; Find Y centroid                 

     sd = total(temp*xwt,1)
 
     sumgd = total(wt*sgx*sd)
     sumd = total(wt*sd)

     sddgdy = total(wt*sd*dgdy)

     hy = (sumgd - sumgy*sumd/p) / (sumgsqx - sumgy^2/p)

     if (hy LE 0) then begin
        badcntrd = badcntrd + 1
        goto, REJECT
     endif

     skylvl = (sumd - hy*sumgy)/p
     dy = (sgdgdy - (sddgdy-sdgdy*(hy*sumgy + skylvl*p)))/(hy*sdgdys/sigsq)

     if abs(dy) GE nhalf then begin 
        badcntrd = badcntrd + 1
        goto, REJECT
     endif
      
     ycen = iy[i] +dy           ;Y centroid in original array
 
;  This star has met all selection criteria.  Print out and save results

     if NOT keyword_set( silent ) then $
        print,FORM='(a,i5,2f9.1,g10.3,2f9.2)',"Loc+stats:  ", nstar, xcen, ycen, d, sharp1, around

     if (npar GE 2) or (doprint) then begin
        fx[nstar] = xcen
        fy[nstar] = ycen
     endif

     if ( npar GE 4 ) or (doprint) then begin
        flux[nstar] = d
        sharp[nstar] = sharp1
        roundness[nstar] = around
     endif
   
     nstar = nstar+1

REJECT: 
 endfor

 nstar = nstar-1		;NSTAR is now the index of last star found

 if doprint then begin
  printf,lun,' No. of sources rejected by SHARPNESS criteria',badsharp
  printf,lun,' No. of sources rejected by ROUNDNESS criteria',badround
  printf,lun,' No. of sources rejected by CENTROID  criteria',badcntrd
 endif
 
 if keyword_set( verbose ) OR $
    (NOT keyword_set( silent ) AND ((badsharp + badround + badcntrd) gt 0)) then begin 
    print,' No. of sources rejected by SHARPNESS criteria',badsharp
    print,' No. of sources rejected by ROUNDNESS criteria',badround
    print,' No. of sources rejected by CENTROID  criteria',badcntrd
 endif 

  if nstar LT 0 then return               ;Any stars found?

  if (npar GE 2) or (doprint) then begin
     fx = fx[0:nstar]
     fy = fy[0:nstar]
  endif

  if (npar GE 4) or (doprint) then begin
	flux= flux[0:nstar] & sharp=sharp[0:nstar]  
        roundness = roundness[0:nstar]
  endif

 if doprint then begin                
   printf,lun, $
      format = '(/8x,a)','     STAR       X       Y     FLUX     SHARP    ROUND'
	for i = 0L, nstar do $
	   printf,lun,format='(12x,i5,2f8.2,f9.1,2f9.2)', $
	              i+1, fx[i], fy[i], flux[i], sharp[i], roundness[i]
        free_lun, lun
 endif

FINISH:

 if keyword_set( silent ) then return

 if keyword_set( askp ) then begin
    print,form='(A,F8.1)',' Threshold above background for this pass was',hmin
    ans = ''
    on_ioerror,SKIP
    read,'Enter new threshold or [RETURN] to exit: ',ans
    hmin = float( ans )              
    if hmin GT 0 then goto, SEARCH
 endif

SKIP:
end
