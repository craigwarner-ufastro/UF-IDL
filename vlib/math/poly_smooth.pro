;+
; NAME:
;	poly_smooth
;
; PURPOSE:
;	Reduce noise in 1-D data (e.g. time-series, spectrum)
;	but retain dynamic range of variations in the data by
;	applying a moving Least-squares fitted smoothing polynomial filter,
;	also called the Savitzky-Golay smoothing filter. Filter is
;	applied via convolution by a kernel derived from polynomials,
;	and the kernel does NOT depend on input data, so can be reused.
;
;	Default is to extend the data array padding with NLEFT and NRIGHT values,
;	using the average of two leftmost and two rightmost data values respectively, 
;	then apply function, and then return inner part of resulting array which is
;	the same size as original data array, thus filtering all data values.
;	Specify keyword /NOEXTEND to not pad the ends of data for filtering.
;
;	Low-pass filter coefficients are computed by effectively
;	least-squares fitting a polynomial in a moving window,
;	centered on each data point, so the new value will be the
;	zero-th coefficient of the polynomial. Approximate first derivates
;	of the data can be computed by using first degree coefficient of
;	each polynomial, and so on.
;
;	The filter coefficients for a specified	polynomial degree
;	and window width are computed independent of any data,
;	and stored in a common block. The stored filter is then convolved
;	with any data array to result in smoothed data with reduced noise,
;	but retaining higher order variations (better than averaging).
;	Stored filtered coefficients are automatically reused if
;	requested parameters are the same as what created the coefficients.
;	Note that odd values of poly. degree have same convolution
;	coefficients as degree-1, but do provide one extra derivative order.
;
; CALLING SEQUENCE:
;
;	spectrum = poly_smooth( data, width, DEGREE= )
;
; INPUTS:
;	data = 1-D array, such as a spectrum or time-series.
;
;	width = total number of data points to use in filter convolution,
;		(default = 5, using 2 past and 2 future data points),
;		must be larger than DEGREE of polynomials, and a guideline is 
;		make WIDTH at least 2 times the FWHM of desired features.
; KEYWORDS:
;
;	DEGREE = degree of polynomial to use in designing the filter.
;		Higher degrees will preserve sharper features. (default DEGREE = 2), 
;
;	NLEFT = # of past data points to use in convolution (if	width is not given),
;		excluding current point, overrides width parameter,
;		so that width = NLEFT + NRIGHT + 1. (default = NRIGHT or 2)
;
;	NRIGHT = # of future data points to use (default = NLEFT or 2).
;
;	DERIV_ORDER = order of derivative desired (default = 0, no derivative).
;
;	COEFFICIENTS = optional output of the filter coefficients applied,
;		but they are all stored in common block for reuse anyway.
;
;	/NOEXTEND = does NOT extend (pad) data array with extra	begin/end values,
;	        so NLEFT datapoints on left and NRIGHT values will not be filtered.
;
;	/RESET = force function to recompute convolution coefficients,
;	        thus not re-using coefficients stored in common.
; RESULTS:
;	Function returns the data convolved with polynomial filter coefs.
; COMMON BLOCKS:
;	common poly_smooth, degc, nlc, nrc, coefs, ordermax
; PROCEDURE:
;	As described in Numerical Recipies, sec.14.8, Savitsky-Golay filter.
;	Matrix of normal eqs. is formed by starting with small terms
;	and then adding progressively larger terms (powers).
;	The filter coefficients of up to derivative ordermax are stored
;	in common, until the specifications change, then recompute coefficients.
;	Coefficients are stored in convolution order, zero lag in the middle.
; MODIFICATION HISTORY:
;	Written, Frank Varosi NASA/GSFC 1993.
;	Frank Varosi UF-astro 2015, fixed up comments.
;	Mod: F.V. 2020, default is to extend (pad) data array to filter all values.
;	Mod: F.V. 2020, added keywords /NOEXTEND and /RESET options.
;	Mod: F.V. 2021, fixed small bug: needed 1 more element for right part of extension
;-

function poly_smooth, data, width, DEGREE=degree, NLEFT=nl, NRIGHT=nr, RESET=reset, $
                      DERIV_ORDER=order, COEFFICIENTS=filter_coef, NOEXTEND=noextend

  common poly_smooth, degc, nlc, nrc, coefs, ordermax

	if N_elements( degree ) NE 1 then begin
           if N_elements( degc ) eq 1 then begin
              if( degc ge 2 ) then degree = degc else degree = 2
           endif else degree = 2
        endif

	if N_elements( order ) NE 1 then order = 0
	order = ( order < (degree-1) ) > 0

	if N_elements( width ) EQ 1 then begin
		width = fix( width ) > 3
		if (N_elements(nr) NE 1) AND (N_elements(nl) NE 1) then begin
			nl = width/2
			nr = width - nl -1
		   endif
	   endif

	if N_elements( nr ) NE 1 then begin
		if N_elements( nl ) EQ 1 then  nr = nl  else begin
                   if N_elements( nrc ) eq 1 then nr = nrc  else  nr = 2
                endelse
	   endif

	if N_elements( nl ) NE 1 then begin
		if N_elements( nr ) EQ 1 then  nl = nr  else begin
                   if N_elements( nlc ) eq 1 then nl = nlc  else  nl = 2
                endelse
	   endif

;; Default is to extend (pad) data array so all values in middle get filtered.
;; Left and right parts of extended data array are set to average of 3 elements.
;; Accomplished by recursive calling poly_smooth with extended array and return middle section:

        if NOT keyword_set( noextend ) then begin

           ndat = N_elements( data )
           datex = fltarr( ndat + nL + nr + 1 )
           datex[0:nL-1] = avg( data[0:3] )
           datex[nL:ndat+nL-1] = data
           datex[ndat+nL:*] = avg( data[ndat-3:*] )

           datps = poly_smooth( datex, DEG=degree, NL=nL,NR=nr,RESET=reset,DERIV=order,/NOEX )

           return, datps[ nL : ndat+nL-1 ]
        endif

;; Below is normal poly_smooth algorithm:

        if N_elements( coefs ) LE 1 then begin
		degc = 0
		nlc = 0
		nrc = 0
		ordermax = 3
	   endif

;; if saved coefficients degrees do not match requested params,
;;  or reset is requested then compute full alogrithm,
;;   otherwise re-use saved filter coefficient below.

        if keyword_set( reset ) OR (degree NE degc) OR $
           (nl NE nlc) OR (nr NE nrc) OR (order GT ordermax) then begin

		degree = degree > 2
		ordermax = ( ordermax < 3 ) > order
		nj = degree+1
		nl = nl > 0
		nr = nr > 0
		nrl = nr + nl + 1

		if (nrl LE degree) then begin
			message,"# of points in filter must be > degree",/INFO
			return, data
		   endif

		ATA = fltarr( nj, nj )
		ATA[0,0] = 1
		iaj = indgen( nj ) # replicate( 1, nj )
		iaj = iaj + transpose( iaj )
		m1_iaj = (-1)^iaj

		for k = 1, nr>nl do begin
		    k_iaj = float( k )^iaj
		    CASE 1 OF
			( k LE nr<nl ):	ATA = ATA + ( k_iaj + k_iaj*m1_iaj )
			( k LE nr ):	ATA = ATA + k_iaj
			( k LE nl ):	ATA = ATA + k_iaj * m1_iaj
		     ENDCASE
		  endfor

		LUdcmp, ATA, LUindex

		Bmat = fltarr( nj, degree<(ordermax+1) )
		B = fltarr( nj )

		for m = 0, (degree-1)<ordermax do begin
			B[*] = 0
			B[m] = 1
			LUbksb, ATA, LUindex, B
			Bmat[0,m] = B
		  endfor

		kvec = [0]
		if (nl GT 0) then kvec = [ rotate( -indgen( nl )-1, 2 ), kvec ]
		if (nr GT 0) then kvec = [ kvec, indgen( nr )+1 ]
		Kmat = fltarr( nrl, nj )
		Kmat[*,0] = 1
		for m = 1,degree do Kmat[0,m] = Kmat[*,m-1] * kvec

		coefs = Kmat # Bmat
		degc = degree
		nlc = nl
		nrc = nr

		if (nr GT nl) then begin
			sc = size( coefs )
			coefs = [  fltarr( nr-nl, sc[2] ),  coefs ]
		  endif else if (nl GT nr) then begin
			sc = size( coefs )
			coefs = [ coefs,  fltarr( nl-nr, sc[2] )  ]
		   endif
	   endif

	filter_coef = coefs[*,order]

return, convol( data, filter_coef )
end
