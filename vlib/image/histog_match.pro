function histog_match, image_fixed, image_match, NBIN=nbin, TOLERANCE=toler

	stat = histog_differ( NBIN=nbin, FIXED_IMAGE=image_fixed, $
					 MATCH_IMAGE=image_match  )

	offset = ( total( image_fixed )/N_elements( image_fixed ) $
		 - total( image_match )/N_elements( image_match ) )

	if N_elements( toler ) NE 1 then toler=1.0
	off1 =  offset
	off2 = -offset

	hdif1 = histog_differ( [ off1, 1 ], /TOTAL )
	iter=0
ITER_OFF:
	iter = iter+1
	hdif2 = histog_differ( [ off2, 1 ], /TOTAL )

	if (abs( hdif2 ) GT toler) AND (hdif2 NE hdif1) then begin
		derr = ( off2 - off1 ) / ( hdif2 - hdif1 )
		offset = off1 - derr * hdif1
		if !DEBUG GE 1 then begin
			print,iter
			print,off1,off2
			print,hdif1,hdif2
			print,derr,offset
			if !DEBUG GE 3 then stop
		   endif
		off1 = off2
		off2 = offset
		if (abs( derr ) GT 1.e-5) AND (iter LT 55) then goto,ITER_OFF
	   endif

	fac1 = 0.9
	fac2 = 1.1

	hdif1 = histog_differ( [ offset, fac1 ], /ABS,/TOTAL )
	hdif2 = histog_differ( [ offset, fac2 ], /ABS,/TOTAL )
	iter=0
ITER_FAC:
	if (hdif2 GT toler) AND (hdif2 NE hdif1) then begin

		factor = (fac1 + fac2)/2

		if !DEBUG GE 1 then begin
			print,iter
			print,hdif1,hdif2
			print,fac1,fac2,factor
			if !DEBUG GE 3 then stop
		   endif

		hdif = histog_differ( [ offset, factor ], /ABS,/TOTAL )
		iter = iter+1
		fac1 = fac2
		fac2 = factor
		if (abs( derr ) GT 1.e-5) AND (iter LT 55) then goto,ITER_FAC
	   endif

return, [offset,factor]
end
