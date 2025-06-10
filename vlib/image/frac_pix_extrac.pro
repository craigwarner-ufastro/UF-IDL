;+
; NAME:
;	frac_pix_extrac
; PURPOSE:
;	Extract a part of image with region specified by fractional pixel offsets,
;	by calling function frac_pix_shift( image ) with x & y shifts,
;	which resamples image onto fractionally shifted grid
;	using the bilinear interp feature of intrinsic IDL function poly_2d.
; CALLING:
;	imgsubset = frac_pix_extrac( image, xBegin, yBegin, xSize, ySize )
; INPUTS:
;	image = 2D array from which to extract subset .
;	xBegin, yBegin =
;	xSize, ySize =
; KEYWORDS:
;	BEGIN_PIXEL = optional way to specify x & y beginning region as 2 element array.
;	SIZE_EXTRACT = optional way to specify x & y region size as 2 element array.
; OUTPUTS:
;	The extracted image subset is returned.
; EXTERNAL CALLS:
;	function frac_pix_shift
; PROCEDURE:
;	Setup and call function frac_pix_shift, return subimage.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1992.
;	FV : UF-astro 2015 : fixed mistake and now use only the needed subset of image.
;	FV : UF-astro 2016 : fixed stupid bug about size checking.
;-

function frac_pix_extrac, image, xBegin, yBegin, xSize, ySize, RENORMALIZE=renorm, $
                          BEGIN_PIXEL=begin_xy, SIZE_EXTRACT=sex

	sim = size( image )

	if (sim[0] NE 2) then begin
		message,"ERR: must input an image (2-D array)",/INFO
		return,[-1]
	   endif

	if N_elements( sex ) NE 2 then begin
           if N_params() LT 5 then begin
              message,"ERR: must specify size to extract,",/INFO
              message,"either with args. # 4 and 5,",/INFO
              message,"or with keyword SIZE=[xsize,ysize]",/INFO
              return,[-1]
           endif
           sex = [xSize, ySize]
        endif 

	if N_elements( begin_xy ) NE 2 then begin
		if N_params() LT 3 then begin
			message,"must specify start pixel for extraction",/INFO
			return,[-1]
		   endif
		begin_xy = [xBegin, yBegin]
	   endif

;; use only the needed subset of image to perform pixel shifting via poly_2D interpolation:

        sex = round( sex )
        if( sex[0] LT 2 ) then sex[0]=2
        if( sex[1] LT 2 ) then sex[1]=2
        ixy = ( fix( begin_xy ) < (sim[1:2]-sex-1) ) > 0
        exy = (( sex + 1 + ixy ) < (sim[1:2]-1)) > 0

        imshift = frac_pix_shift( image[ixy[0]:exy[0],ixy[1]:exy[1]], $
                                  SHIFT=(ixy - begin_xy), REN=renorm )
        sims = size( imshift )-1
        sexi = sex - 1

        if( max( sexi - sims[1:2] ) ge 0 ) then begin

           imsex = fltarr( sex[0], sex[1] )
           ss = (sexi < sims[1:2])
           imsex[0,0] = imshift[ 0:ss[0], 0:ss[1] ]
           return, imsex

        endif else if( min( sims[1:2] - sexi ) ge 0 ) then begin

           return, imshift[ 0:sexi[0], 0:sexi[1] ]

        endif else return, imshift
end
