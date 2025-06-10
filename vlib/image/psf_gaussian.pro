;+
; NAME:
;	psf_Gaussian
;
; PURPOSE:
;	Return a point spread function having Gaussian profiles,
;	as either a 1D vector, a 2D image, or 3D volume-data.
;
; CALLING:
;	psf = psf_Gaussian( NPIXEL=31, FWHM=[4.3,5.2], /NORMALIZE )
; or:
;	psf = psf_Gaussian( parameters )
;
; KEYWORDS:
;	NDIMEN = dimension of result: 1 (vector), 2 (image), or 3 (volume),
;		default = 2 (an image result).
;
;	NPIXEL = number pixels for each dimension, specify as an array,
;		or just one number to make all sizes equal.
;
;	FWHM = the desired Full-Width Half-Max (pixels) in each dimension,
;		specify as an array, or single number to make all the same.
;
;	CENTROID = pixels numbers of PSF maximum ( 0.5 is center of a pixel ),
;		default is exact center of requested vector/image/volume.
;
;	STDEV = optional way to specify width by standard deviation param.
;	/NORMALIZE causes resulting PSF to be normalized so Total( psf ) = 1.
;	/DOUBLE
;
; INPUTS (optional):
;
;	parameters = an NDIMEN by 3 array giving for each dimension:
;			[ maxval, center, stdev ],  overrides other keywords.
;
;		This argument is obtained from the FIT_PARAMS=parameters
;		keyword result of function FullWid_HalfMax.
;
; EXTERNAL CALLS:
;	function Gaussian
; PROCEDURE:
;	Straight-forward.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1991.
;-

function psf_gaussian, parameters, NPIXEL=npix, NDIMENSION=ndim, FWHM=fwhm,  $
                       CENTROID=cntrd, ST_DEV=st_dev, NORMALIZE=normalize, DOUBLE=doublv

	szp = size( parameters )

	if (szp[0] GT 1) then begin
		ndim = szp[0]
		factor = parameters[0,0]
		;;add half-pixel here to stay with center convention, subtracted below.
		cntrd = parameters[*,1] + 0.5
		st_dev = parameters[*,2]
	   endif

	if N_elements( ndim ) NE 1 then begin
		ndim = 2
		if N_elements( npix ) gt 0 then ndim = N_elements( npix )
		if N_elements( fwhm ) gt ndim then ndim = N_elements( fwhm )
	   endif

	ndim = ndim > 1

	if (szp[0] eq 1) and (ndim gt 1) then begin
		factor = parameters[0]
		;;add half-pixel here to stay with center convention, subtracted below.
		cntrd = parameters[1:ndim] + 0.5
		st_dev = parameters[1+ndim:2*ndim]
	   endif

	if N_elements( npix ) LE 0 then begin
           message,"must specify size of result with NPIX=",/INFO
           return,(-1)
        endif else if N_elements( npix ) LT ndim then npix = replicate( npix[0], ndim )

	if (N_elements( cntrd ) LT ndim) AND (N_elements( cntrd ) GT 0) then $
			cntrd = replicate( cntrd[0], ndim )

	if N_elements( cntrd ) LE 0 then cntrd=(npix-1)/2. else cntrd = cntrd - 0.5
	if N_elements( fwhm ) GT 0 then st_dev = fwhm/( 2* sqrt( 2* aLog(2) ) )

	if N_elements( st_dev ) LE 0 then begin
		message,"must specify ST_DEV= or FWHM=",/INFO
		return,(-1)
	  endif

	if N_elements( st_dev ) LT ndim then st_dev = replicate( st_dev[0], ndim )
	sigfac = 1 / (2. * st_dev^2 )

	CASE ndim OF

	1: BEGIN
                if keyword_set( doublv ) then begin
                   x = dindgen(npix[0]) - cntrd[0]
                endif else begin
                   x = findgen( npix[0] ) - cntrd[0]
                endelse
		psf = gaussian( x, [1, 0, st_dev] )
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

		psfx = gaussian( x, [ 1, 0, st_dev[0] ] )
		psfy = gaussian( y, [ 1, 0, st_dev[1] ] )
		for j = 0, npix[1]-1 do psf[0,j] = psfx * psfy[j]
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

		psfx = gaussian( x, [ 1, 0, st_dev[0] ] )
		psfy = gaussian( y, [ 1, 0, st_dev[1] ] )
		psfz = gaussian( z, [ 1, 0, st_dev(2) ] )
		for k=0,npix(2)-1 do begin
		    for j=0,npix[1]-1 do psf[0,j,k] = psfx * psfy[j] * psfz[k]
		 endfor
	     END

	ENDCASE

	if keyword_set( normalize ) then return, psf/total( psf )

	if N_elements( factor ) EQ 1 then begin
		if (factor NE 1) then return,factor*psf else return,psf
	   endif else return, psf
end
