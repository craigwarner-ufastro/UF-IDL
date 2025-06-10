function histog_match, image_fixed, image_match, NBIN=nbin

	stat = histog_differ( NBIN=nbin, FIXED_IMAGE=image_fixed, $
						MATCH_IMAGE=image_match )

	offset = ( total( image_fixed )/N_elements( image_fixed ) $
		 - total( image_match )/N_elements( image_match ) ) > 100

	coefmat = [ [      0, 1  ], $
		    [ offset, 1.5], $
		    [-offset, 0.5]  ]

	hdifs = fltarr(3)
	for i=0,2 do hdifs(i) = histog_differ( coefmat(*,i),/ABS,/TOTAL )

	minf_simplex, coefmat, hdifs, niter, FUNC_NAME="histog_differ"

	minhd = min( hdifs, imin )

return, coefmat(*,imin)
end
