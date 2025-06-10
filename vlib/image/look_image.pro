;+
; NAME:
;	Look_image
;
; PURPOSE:
;	Compute and optionally print mean & standard deviation of pixel values
;	in a box or circle centered at (xi,yi) of image array.
;	(Usually called by pro Look_images).
;
; CALLING:
;	Look_image, image, xi,yi, box_size, mean, stdev, imtot, npix, fmin, fmax
;
; INPUTS:
;	image = 2D array
;	xi, yi = pixel Location of box/circle center.
;	box_size = width of square box (diameter of circle), in pixels, def=5.
;
; OUTPUTS:
;	mean = the mean of pixel values in box/circular region.
;	stdev = the standard deviation of pixel values in box/circular region.
;	imtot = total of pixel values in box/circular region.
;	npix = # of pixels in box/circular region.
;
; KEYWORDS:
;	/CIRCULAR : use circular region instead of box.
;	ANNULAR = inner radius of annular region instead of box,
;		and box_size or RADIUS or BOX_WIDTH specifies outer circle.
;	/PRINT : causes mean and st.dev. to be printed, default = 0,
;		if PRINT=2 then pixel values in box are also printed.
;	/FWHM_PRINT : print the Full-Width-Half-Max of data in box.
;	/LOG10 : assume input image is in Log10 units, so print 10.^image vals.
;
;	ARCS_PIXEL = area of a pixel in squared arcsecs (prints total flux).
;
;	BOX_WIDTH = optional way to specify size of box,
;		1 integer for square box, 2 integers for rectangular box.
;	RADIUS = optional way to specify box/circle in terms of it's radius.
;	BORDER = # of pixels at edge of image to ignore, default = 0.
;	XOFF, YOFF = optional offset of image in some bigger image,
;			used when printing pixel locations only, default=(0,0).
; EXTERNAL CALLS:
;	function FullWid_HalfMax	(if /FWHM_PRINT)
;	function Disk_Region		(if /CIRC)
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1989, added keyword BORDER to limit (xi,yi) within image.
;	F.V. 1990, added keyword CIRCULAR for optional circular region.
;	F.V. 1991, fixed bug with printing pixvals when box is at top edge.
;	F.V. 1996, fixed bug with /CIRCULAR option, off by one pixel.
;	F.V. 1997, use function Disk_Region to generate map for /CIRCULAR option.
;	F.V. 1999, added /LOG10 option.
;	F.V. 2011 @ UF, use function frac_pix_extrac( image...) to get more accurate.
;	F.V. 2016, added option CONTAINER_BOX_FACTOR to also display a larger region.
;	F.V. 2021, changed keyword to CONTAINER_RADIUS to specify exact size.
;	F.V. 2022, improved the print format clarity.
;	F.V. 2024, added option GAUSS_BEAM_FWHM= beam to multiply image to sim obs.
;-

pro Look_image, image, xi,yi, box_size, mean, stdev, imtot, npix, fmin, fmax, $
		BOX_WIDTH=box_width, RADIUS=radius, CIRCULAR=circular,$
		FWHM_PRINT=fwhmpr, ANNULAR=innerRadius, GAUSS_BEAM_FWHM=beam_fwhm,$
		XOFFSET=xoff, YOFFSET=yoff, MORE_DIGITS=more_digits, MAGF=magf, $
		LOG10=Log10, PRINT=print, ARCS_PIXEL=arcs_pixel, BORDER=border,$
		DNUMBER=dnumber, DNAME=dname, DSIZE=dsize, CONTAINER_RADIUS=containRadius

  common Look_image1, image_subset, xmin, ymin

	szim = size( image )

	if (szim[0] NE 2) or (N_elements( xi ) ne 1) or (N_elements( yi ) ne 1) then begin
		print,"syntax:	Look_image, image, xi,yi, box_size, $"
		print,"			mean, stdev, imtot, npix, fmin, fmax,"
		print,"			BOX_WIDTH=, /CIRCULAR, BORDER=, /LOG10, $"
                print,"			/FWHM, /PRINT, RADIUS=, XOFFSET=, YOFFSET=, $"
                print,""
		print,"	outputs:	mean, stdev, imtot, npix, fmin, fmax"
		return
	   endif

	if keyword_set( beam_fwhm ) then begin
           gauss_beam = psf_gaussian( NPIX=szim[1:2], FWHM=beam_fwhm,CEN=[xi,yi])
           image_subset = gauss_beam * image
           imtot = total( image_subset )
           npix = N_elements( image_subset )
           stdev = stdev( image_subset, mean )
           fmin = min( image_subset, MAX=fmax )
           szimsub = size( image_subset )
           nx = szimsub[1]
           ny = szimsub[2]
           if keyword_set( magf ) then imscl = scale_image( image_subset>0, magf )
           goto,DISPLAY
        endif

	if N_elements( xoff ) NE 1 then xoff=0
	if N_elements( yoff ) NE 1 then yoff=0

	if N_elements( box_width ) GT 0 then box_size = box_width
	if N_elements( radius ) GT 0 then box_size = 2*radius+1
	if N_elements( box_size ) LT 1 then box_size = 5

	if (box_size LT 3) then begin
		mean = image[xi,yi]
		stdev = 0
		imtot = mean
		npix = 1
		fmin = mean
		fmax = mean
		nx=1 & ny=1 & szimsub=size(mean)
		goto,PRINT
	   endif

	boxh = box_size/2.0
	if N_elements( boxh ) LT 2 then boxh = [boxh,boxh]
	if N_elements( border ) EQ 1 then border =(border>0)< min(szim[1:2]/4) else border=0

	Lim = szim[1:2] -1
	Limit = Lim -border
        xshift = xi - fix(xi)
        yshift = yi - fix(yi)
        if ( xshift eq 0 ) and (yshift eq 0) then begin
           xi = fix(xi)
           yi = fix(yi)
        endif
        xmin = (( xi - boxh[0] ) > border) < Lim[0]
        ymin = (( yi - boxh[1] ) > border) < Lim[1]
        xmax = (( xi + boxh[0] ) < Limit[0]) > xmin
        ymax = (( yi + boxh[1] ) < Limit[1]) > ymin
        image_subset = frac_pix_extrac( image, xmin, ymin, xmax-xmin, ymax-ymin )

        if keyword_set( containRadius ) and keyword_set( magf ) then begin
           subconsize = magf * (2 * containRadius + 1)
           imsubcon = bytarr( subconsize, subconsize )
           xcmin = (( xi - containRadius ) > border) < Lim[0]
           ycmin = (( yi - containRadius ) > border) < Lim[1]
           xcmax = (( xi + containRadius ) < Limit[0]) > xcmin
           ycmax = (( yi + containRadius ) < Limit[1]) > ycmin
           ccx = xcmin - xi + containRadius
           ccy = ycmin - yi + containRadius
           xcsize = (2 * containRadius + 1) - ceil( ccx )
           ycsize = (2 * containRadius + 1) - ceil( ccy )
           ccx = round( ccx * magf )
           ccy = round( ccy * magf )
           imsubcon[ccx,ccy] = scale_image( frac_pix_extrac( smooth(image,3)>0, $
                                                             xcmin, ycmin, $
                                                             xcsize, ycsize ), $
                                            magf, MAX=max( image_subset ), LOG=Log10 )
        endif

	if keyword_set( Log10 ) then image_subset = 10.^image_subset
	szimsub = size( image_subset )
	nx = szimsub[1]
	if (szimsub[0] EQ 2) then ny = szimsub[2] else ny=1

	if keyword_set( innerRadius ) then circular=1 else innerRadius=0

        if keyword_set( circular ) then begin

           dsiz = [nx,ny]
           dcen = dsiz/2
           radius = max( dsiz )/2.0
           wcsav = Disk_Region( dcen, SIZE=dsiz, RADIUS=radius, IN=innerRadius )

           if keyword_set( magf ) then begin
              imdisp = image_subset
              imdisp[*] = 0
              imdisp[wcsav] = image_subset[wcsav]
              imscl = scale_image( imdisp > 0, magf )
           endif

           image_subset = image_subset[wcsav]

        endif else if keyword_set( magf ) then imscl = scale_image( image_subset>0, magf )
DISPLAY:
        s = size( imscl )

	if s[0] eq 2 then begin

           cxm = round( 0.5 * s[1])
           cym = round( 0.5 * s[2])
           imscl[cxm,*]=0
           imscl[*,cym]=0
           if N_elements( dnumber ) ne 1 then dnumber=0
           if N_elements( dsize ) ne 1 then dsize=120
           xoff = (dsize + magf) * dnumber
           if keyword_set( dname ) then printw,dname,PIXOFF=xoff,LINE=-3
           sc = size( imsubcon )

           if sc[0] eq 2 then begin
              ccxm = sc[1]/2
              ccym = sc[2]/2
              imsubcon[ccxm,*]=0
              imsubcon[*,ccym]=0
              bxmin = (ccxm - magf*boxh[0]) > 0
              bxmax = (ccxm + magf*boxh[0]) < (sc[1]-1)
              bymin = (ccym - magf*boxh[1]) > 0
              bymax = (ccym + magf*boxh[1]) < (sc[2]-1)
              imsubcon[ bxmin, bymin:bymax ] = 0
              imsubcon[ bxmax, bymin:bymax ] = 0
              imsubcon[ bxmin:bxmax, bymin ] = 0
              imsubcon[ bxmin:bxmax, bymax ] = 0
              tv, imsubcon, xoff,0
              tv, imscl, xoff + ccxm - cxm, dsize+1

              if keyword_set( fwhmpr ) then begin
                 fwhmxy = FullWid_HalfMax( imsubcon, PEAK=pxy, $
                                           MOFFAT_FIT=(fwhmpr EQ 3), $
                                           GAUSSIAN_FIT=(fwhmpr EQ 2), $
                                           LORENTZIAN_FIT=(fwhmpr EQ 4) )
                 printw,"FWHM= "+ string( fwhmxy, FORM="(2F7.1)"),LIN=0,PIXOFF=xoff,/ERAS
              endif

           endif else tv, imscl, xoff, 0
        endif

	imtot = total( image_subset )
	npix = N_elements( image_subset )
	mean = imtot/npix
	stdev = sqrt( total( (image_subset - mean)^2 ) / (npix-1) )
	fmin = min( image_subset, MAX=fmax )

	if (fmin LT 0) and (fmax gt fmin) then begin
           wmin = where( image_subset eq fmin, nmin )
           if( nmin gt 1 ) then begin
              imsubw = image_subset[ where( image_subset gt fmin ) ]
              imtot = total( imsubw )
              npix = N_elements( imsubw )
              mean = imtot/npix
              if( npix LE 1 ) then stdev=0.0 else stdev= sqrt(total((imsubw - mean)^2)/npix)
              fmin = min( imsubw, MAX=fmax )
           endif
        endif

	szimsub = size( image_subset )

	if keyword_set( fwhmpr ) AND (szimsub[0] eq 2) then begin
           if N_elements( fwhmxy ) ne 2 then begin
		fwhmxy = FullWid_HalfMax( image_subset, PEAK=pxy, $
                                          MOFFAT_FIT=(fwhmpr EQ 3), $
                                          GAUSSIAN_FIT=(fwhmpr EQ 2), $
                                          LORENTZIAN_FIT=(fwhmpr EQ 4) )
             endif
           print,"FWHM = ", fwhmxy
           if N_elements( pxy ) EQ 2 then print," peak Loc =", pxy + [xmin,ymin]
        endif
PRINT:
	if keyword_set( print ) then begin

           format = '2x,"mean=",'

           if keyword_set( more_digits ) and $
              (abs( mean ) LT 10) and (abs( mean ) GE 0.01) then $
                 format +='F8.4,",",2x,"stdev=",G7.2,",",2x,"min=",F8.4,",",2x,"max=",F8.4'$
           else  format +='G8.3,",",2x,"stdev=",G7.2,",",2x,"min=",G8.3,",",2x,"max=",G8.3'

           format +=',",",2x,"sum(",i6,")=",G8.3)'

           if keyword_set( arcs_pixel ) then begin
              format = '(",  flux=",G8.3,",",' + format
              print, imtot * arcs_pixel, mean, stdev, fmin, fmax, npix, imtot, FORM=format
           endif else begin
              format = '(' + format
              print, mean, stdev, fmin, fmax, npix, imtot, FORM=format
           endelse

           if (print GE 2) AND (szimsub[0] EQ 2) then begin
              js = szimsub[2]-1
              print, format="(4x,7i10)", indgen(szimsub[1]) + xmin + xoff
              jy = ymin+yoff
              for j = js, 0, -1 do print, j+jy, image_subset[*,j], FORM="(i4,7G10.3)"
           endif
        endif
end
