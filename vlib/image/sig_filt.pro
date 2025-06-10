Function Sig_filt, image, radius, sigma
;+
; NAME:
;	Sig_filt
; PURPOSE:
;	Performs the Sigma filter algorithm on the input
;	image array using a box of side 2*radius+1.
;	This filter will remove noisy areas without removing isolated peaks.
;	The technique smooths additive image noise by checking statistics
;	in local neighborhoods and averaging only those pixels with values
;	within 2 * sigma * (central pixel value) range of central pixel.
; CATEGORY:
;	E3 Smoothing (image)
; CALLING SEQUENCE:
;	Result = Sig_filt( image, radius, sigma )
; INPUTS:
;	image = Input image array or one dimensional vector
;	radius : Side of filter box = 2*radius+1.  Default value = 1.
;	sigma = standard deviation FACTOR applied to each pixel value. Def=0.1
; PROCEDURE:
;	The loops over image pixels and differences are vectorized and
;	executed within loops of window shifts (which are the shorter loops).
;	The pixels within range are accumulated as wells as # of pixels used,
;	then the average is computed after loops.
;	This order of loops is faster but uses more memory.
; MODIFICATION HISTORY:
;	Written, Jan.1991, Frank Varosi, STX @ NASA/GSFC
;					based on info from Houra Raiz.
;-
	if N_elements( radius ) NE 1 then radius = 1
	width = (2 * radius + 1) > 3

	if N_elements( sigma ) NE 1 then sigma = 0.1
	sigma = sigma > 0

	sim = size( image )
	n1 = sim(1)-width+1
	n2 = sim(2)-width+1
	sum = fltarr( n1, n2 )
	count = intarr( n1, n2 )

	L1 = sim(1)-radius-1
	L2 = sim(2)-radius-1
	cim = image( radius:L1, radius:L2 )	;central part of image processed
	delta = (2. * sigma) * cim

	for sj = 0, width-1 do begin		;vertical window shifting...

		Lj = sj + n2 - 1

		for si = 0, width-1 do begin	;horizontal window shifting...

			Li = si + n1 - 1
			weight = abs( image(si:Li,sj:Lj) - cim ) LT delta

			count = count + weight
			sum = sum + weight * image(si:Li,sj:Lj)
		  endfor
	  endfor

	result = image
	result( radius:L1, radius:L2 )	= sum/count

return, result
end
