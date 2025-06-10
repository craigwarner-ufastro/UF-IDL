;+
; NAME:
;	Bin_Scat
; PURPOSE:
;	Given two arrays of data that are in one-to-one correspondence
;	(such as a functional relationship), calculate averages and
;	standard devations of the second array within bins that
;	are determined by the histogram of the first array.
;	Using this technique, a scatter plot of y versus x can be
;	displayed instead as a graph of bin averages with error bars.
;	Results are returned via keywords.
;
; CALLING:
;	Bin_Scat, x, y
;
; INPUTS:
;	x = this array is binned using histogram
;	y = usually a function of x, this array is averaged over bins
;		corresponing to the x array.
;
; KEYWORD INPUTS:
;
;	NBIN = # of bins desired, default=100.
;	BINSIZE = the desired binsize, overrides NBIN,
;		but NBIN will still limit the maximum # of bins.
;
;	MIN = minimum data value to include in histogram.
;	MAX = maximum data value to include in histogram
;	     (defaults are actual min or max of data).
;
;	/SUM : setting this keyword causes total of y-values in each bin
;		to be computed (instead of the default mean, st.dev., median).
;
; KEYWORD OUTPUTS:
;
;	XBINS = array of histogram bins between min & max.
;	XHISTOGRAM = number of x values in each bin.
;	MEAN = array of means of y values corresponding to x values in each bin.
;	STDEV = array of standard deviations of y values corresponding to means.
;	MEDIAN = array of medians of y values in each x-bin.
;	TOTAL = array of totals of y values in each x-bin (if /SUM is set).
;
; EXTERNAL CALLS:
;	function Histo	(this sets up and calls IDL histogram function)
;	function stdev
; PROCEDURE:
;	Use the reverse indices keyword of the IDL histogram function
;	(called via function Histo for which third argument is reverse indices).
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1997.
;-

pro Bin_Scat, x, y, XBINS=xbins, XHISTOGRAM=hx, SUM=sum, TOTAL=ytotal, $
			MEAN=ymean, STDEV=ystdev, MEDIAN=ymedian, $
			NBIN=nbin, MIN=minv, MAX=maxv, BINSIZE=binsize

	hx = Histo( x, xbins, r, NBIN=nbin, MIN=minv, MAX=maxv, BIN=binsize )
	nh = N_elements( hx )

	if keyword_set( sum ) then begin

		ytotal = fltarr( nh )
		w = where( hx, nwh )

		if (nwh GT 0) then begin
			rw = r(w)
			rw1 = r(w+1)-1
		   endif

		for k=0,nwh-1 do ytotal(w(k)) = total( y( r( rw(k):rw1(k) ) ) )

	 endif else begin

		ymean = fltarr( nh )
		ymedian = fltarr( nh )
		ystdev = fltarr( nh )
		w = where( hx GT 1, nwh )

		if (nwh GT 0) then begin
			rw = r(w)
			rw1 = r(w+1)-1
		   endif

		for k=0,nwh-1 do begin
			yr = y( r( rw(k):rw1(k) ) )
			i = w(k)
			ystdev(i) = stdev( yr, m )
			ymean(i) = m
			ymedian(i) = median( yr )
		  endfor

		w = where( hx EQ 1, nwh )

		if (nwh GT 0) then begin
			yr = y(r(r(w)))
			ymean(w) = yr
			ymedian(w) = yr
	 	  endif
	  endelse
end
