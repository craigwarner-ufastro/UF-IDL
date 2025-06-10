;+
; NAME:
;	psf_Moffat
; PURPOSE:
;	Return a generalized Moffat (also called Cauchy function)
;	point spread function (PSF),
;	as either a 1D vector, a 2D image, or 3D volume-data.
; CALLING EXAMPLES:
;	psf = psf_Moffat( NPIXEL=31, FWHM=4.3, /NORMALIZE )
;	psf = psf_Moffat( parameters )
; KEYWORDS:
;	NDIMEN	=
;	NPIXEL	=
;	FWHM	=
;	CENTROID=
;	ALPHA	=
;	BETA	=
;	XY_CORREL=
;	/NORMALIZE
;	/DOUBLE
; INPUTS (optional):
;	parameters = an Ndim by 4 array giving for each dimension:
;			[ maxval, center, alpha, beta ]
;	Or a 2 + 2*ndim vector: [ maxval, [centers], [alphas], beta ]
;	This argument is usually obtained from the FIT_PARAMS=parameters
;		keyword result of function FullWid_HalfMax or function fitPSF.
; EXTERNAL CALLS:
;	function Moffat  (for 1D case only).
; PROCEDURE:
; HISTORY:
;	Written, Frank Varosi UFastro 2007.
;	Mod. FV 2017: fixed mistake in computing alpha from given FWHM.
;	Mod, FV 2017: fixed mistake: 1-D param array is now used correctly.
;-

function psf_Moffat, parameters, NDIMEN=ndim, NPIXEL=npix, FWHM=fwhm, $
				CENTROID=cntrd, ALPHA=alpha, BETA=beta, $
				NORMALIZE=normalize, XY_CORREL=xy_corr, DOUBLE=doublv

	szp = size( parameters )

	if (szp[0] gt 1) then begin
		if (szp[szp[0]] GE 4) then begin
			ndim = szp[0]
			factor = parameters[0,0]
		;;add half-pixel here to stay with center convention, subtracted below.
			cntrd = parameters[*,1] + 0.5
			alpha = parameters[*,2]
			beta = parameters[0,3]
		  endif else begin
			message,"need 4 parameters for each dimension",/INFO
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
              alpha = parameters[1+ndim:2*ndim]
              beta = parameters[1+2*ndim]
           endif else begin
              factor = parameters[0]
              ;;add half-pixel here to stay with center convention, subtracted below.
              cntrd = parameters[1] + 0.5
              alpha = parameters[2]
              beta = parameters[3]
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
                if N_elements( beta ) LE 0 then beta = 2.0
		alpha = fwhm/2./sqrt(2^(1./beta) - 1)
	   endif

	if N_elements( alpha ) LE 0 then begin
           message,"must specify ALPHA= or FWHM=",/INFO
           return,(-1)
        endif	else if N_elements( alpha ) LT ndim then alpha = replicate( alpha[0], ndim )

	if N_elements( beta ) LE 0 then beta = 2.5
	beta = total( beta )/N_elements( beta )

	CASE ndim OF

	1: BEGIN
                if keyword_set( doublv ) then x=dindgen(npix[0]) else x=findgen(npix[0])
		psf = Moffat( x, [1,cntrd[0],alpha[0],beta[0]] )
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

		x2 = (x/alpha[0])^2
		y2 = (y/alpha[1])^2

		for j=0,npix[1]-1 do begin
			radxy2 = x2 + y2[j]
			psf[*,j] = ( 1 + radxy2 )^(-beta)
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

		x2 = (x/alpha[0])^2
		y2 = (y/alpha[1])^2
		z2 = (z/alpha[2])^2

		for k=0,npix[2]-1 do begin
		    for j=0,npix[1]-1 do begin
				radxyz2 = x2 + y2[j] + z2[k]
				psf[*,j,k] = ( 1 + radxyz2 )^(-beta)
		      endfor
		 endfor
	     END

	ENDCASE

	if keyword_set( normalize ) then return, psf/total( psf )

	if N_elements( factor ) EQ 1 then begin
		if (factor NE 1) then  return, factor*psf  else  return,psf
	   endif else return, psf
end
