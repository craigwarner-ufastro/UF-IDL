;+
; NAME:
;	psf_Lorentzian
; PURPOSE:
;	Return a generalized Lorentzian (also called Cauchy function)
;	point spread function (PSF),
;	as either a 1D vector, a 2D image, or 3D volume-data.
; CALLING EXAMPLES:
;	psf = psf_Lorentzian( NPIXEL=31, FWHM=4.3, /NORMALIZE )
;	psf = psf_Lorentzian( parameters )
; KEYWORDS:
;	NDIMEN	=
;	NPIXEL	=
;	FWHM	=
;	CENTROID=
;	RADII	=
;	POWER	=
;	PSCALE	=
;	XY_CORREL=
;	/NORMALIZE
;	/DOUBLE
; INPUTS (optional):
;	parameters = an Ndim by 5 array giving for each dimension:
;			[ maxval, center, radius, pscale, power ]
;	Or a 2 + 3*ndim vector: [ maxval, [centers], [radii], [pscales], power ]
;	This argument is usually obtained from the FIT_PARAMS=parameters
;		keyword result of function FullWid_HalfMax or function fitPSF.
; EXTERNAL CALLS:
;	function Lorentzian  (for 1D case only).
; PROCEDURE:
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;	Mod, FV @ UF-astro 2017: compute more reasonable params when only FWHM is specified.
;	Mod, FV @ UF-astro 2017: fixed mistake: 1-D param array is now used correctly.
;	Mod, FV @ UF-astro 2019: keyword OFFSET= or = -parameters[5] if present.
;-

function psf_Lorentzian, parameters, NDIMEN=ndim, NPIXEL=npix, FWHM=fwhm, OFFSET=offset, $
				CENTROID=cntrd, RADII=radii, PSCALE=pscale, $
				POWER=power, NORMALIZE=normalize, DOUBLE=doublv

	szp = size( parameters )

	if (szp[0] gt 1) then begin
		if (szp[szp[0]] GE 5) then begin
			ndim = szp[0]
			factor = parameters[0,0]
		;;add half-pixel here to stay with center convention, subtracted below.
			cntrd = parameters[*,1] + 0.5
			radii = parameters[*,2]
			pscale = parameters[*,3]
			power = parameters[0,4]
		  endif else begin
			message,"need 5 parameters for each dimension",/INFO
			return,(-1)
		   endelse
	   endif

	if N_elements( ndim ) NE 1 then begin
		ndim = 2
		if N_elements( npix ) gt 0 then ndim = N_elements( npix )
		if N_elements( fwhm ) gt ndim then ndim = N_elements( fwhm )
	   endif

	ndim = fix( ndim>1 )

        if (szp[0] eq 1) then begin
           if( ndim gt 1 ) then begin
              factor = parameters[0]
              ;;add half-pixel here to stay with center convention, subtracted below.
              cntrd = parameters[1:ndim] + 0.5
              radii = parameters[1+ndim:2*ndim]
              pscale = parameters[1+2*ndim:3*ndim]
              power = parameters[1+3*ndim]
           endif else begin
              factor = parameters[0]
              ;;add half-pixel here to stay with center convention, subtracted below.
              cntrd = parameters[1] + 0.5
              radii = parameters[2]
              pscale = parameters[3]
              power = parameters[4]
              if N_elements( parameters ) GE 6 then offset = -parameters[5]
           endelse
        endif
        
        if N_elements( npix ) LE 0 then begin
           message,"must specify size of result with NPIX=",/INFO
           return,(-1)
        endif else if N_elements( npix ) LT ndim then npix = replicate( npix[0], ndim )

	if (N_elements( cntrd ) LT ndim) AND $
	   (N_elements( cntrd ) GT 0) then cntrd = replicate( cntrd[0], ndim )

	if N_elements( cntrd ) LE 0 then cntrd=(npix-1)/2. else cntrd = cntrd - 0.5

	if N_elements( fwhm ) GT 0 then begin
		radii = fwhm/2.
                if N_elements( power ) LE 0 then power = 2.0
		if N_elements( pscale ) LE 0 then pscale = 1e-3/fwhm
	   endif

        if N_elements( radii ) LE 0 then begin
           message,"must specify RADII= or FWHM=",/INFO
           return,(-1)
        endif else if N_elements( radii ) LT ndim then radii = replicate( radii[0], ndim )

	if (N_elements( pscale ) LT ndim) AND $
           (N_elements( pscale ) GT 0) then pscale = replicate( pscale[0], ndim )

	if N_elements( power ) LE 0 then power = 2.0

	CASE ndim OF

	1: BEGIN
                if keyword_set( doublv ) then x=dindgen(npix[0]) else x=findgen(npix[0])
                if N_elements( pscale ) LT 1 then pscale = 0
		psf = Lorentzian( x, [1,cntrd[0],radii[0],pscale[0],power[0]] )
	     END

	2: BEGIN
		if keyword_set( doublv ) then begin
                   psf = make_array( DIM=npix[0:ndim-1], /DOUBLE )
                   x = dindgen( npix[0] ) - cntrd[0]
                   y = dindgen( npix[1] ) - cntrd[1]
                endif else begin
                   psf = make_array( DIM=npix[0:ndim-1], /FLOAT )
                   x = findgen( npix[0] ) - cntrd[0]
                   y = findgen( npix[1] ) - cntrd[1]
                endelse

		x2 = (x/radii[0])^2
		y2 = (y/radii[1])^2
                gamma = 0  ;;default value if pscale not specified.

		if N_elements( pscale ) EQ 2 then begin
			pscale = pscale > 0
			xs = x*x*pscale[0]
			ys = y*y*pscale[1]
		   endif

		for j=0,npix[1]-1 do begin
			alpha = sqrt( x2 + y2[j] )
			if N_elements( xs ) eq npix[0] then gamma = sqrt( xs + ys[j] )
			beta = power * (1 + gamma)
			psf[0,j] = 1 / ( 1 + alpha^beta )
		  endfor
	     END

	3: BEGIN
		if keyword_set( doublv ) then begin
                   psf = make_array( DIM=npix[0:ndim-1], /DOUBLE )
                   x = dindgen( npix[0] ) - cntrd[0]
                   y = dindgen( npix[1] ) - cntrd[1]
                   z = dindgen( npix[2] ) - cntrd[2]
                endif else begin
                   psf = make_array( DIM=npix[0:ndim-1], /FLOAT )
                   x = findgen( npix[0] ) - cntrd[0]
                   y = findgen( npix[1] ) - cntrd[1]
                   z = findgen( npix[2] ) - cntrd[2]
                endelse

		x2 = (x/radii[0])^2
		y2 = (y/radii[1])^2
		z2 = (z/radii[2])^2
                gamma = 0  ;;default value if pscale not specified.

		if N_elements( pscale ) EQ 3 then begin
			pscale = pscale > 0
			xs = x*x*pscale[0]
			ys = y*y*pscale[1]
			zs = z*z*pscale[2]
		   endif

		for k=0,npix[2]-1 do begin
		    for j=0,npix(1)-1 do begin
			alpha = sqrt( x2 + y2[j] + z2[k] )
			if N_elements( xs ) eq npix[0] then $
					gamma = sqrt( xs + ys[j] + zs[k] )
			beta = power * (1 + gamma)
			psf[0,j,k] = 1 / ( 1 + alpha^beta )
		      endfor
		 endfor
	     END

	ENDCASE

	if keyword_set( normalize ) then return, psf/total( psf )

	if N_elements( factor ) EQ 1 then begin
           if (factor NE 1) then psf *= factor
           if keyword_set( offset ) then psf += offset
        endif

        return, psf
end
