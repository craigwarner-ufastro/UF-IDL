;+
; NAME:
;	Histo
;
; PURPOSE:
;	Provides a convient interface to IDL histogram function:
;	return histogram with requested number of bins (or specified binsize),
;	and also output the vector of bin values (useful for plotting).
;	If keyword BOX_STDEV is set, this specifies the size of moving box
;	for which local variances of data are computed and then the
;	histogram of all local standard deviations is returned.
;
; CALLING EXAMPLES:
;
;	hist_data = Histo( data, BinVals, Revix, NBIN=70, MIN=.1, MAX=50 )
;
;	hist_stdev = Histo( data, BinVals, BOX_STDEV=3 )
;
;	plot, binvals, Histo( data, binvals )
;
; INPUT:
;	data = array of numbers (vector, image, etc.)
;
; KEYWORDS:
;	NBIN = # of bins desired, default=100.
;	BINSIZE = the desired binsize, overrides NBIN,
;		but NBIN can still limit the maximum # of bins.
;
;	MIN = minimum data value to include in histogram.
;	MAX = maximum data value to include in histogram
;	     (defaults are actual min or max of data).
;
;	/NORMALIZE : normalize the histogram so that its integral over bins
;		is unity, making it a probability density.

;	BOX_STDEV = the width in pixels of moving box
;	            in which to compute local variances,
;	            and then histogram of standard deviations is returned.
;
;	LOCAL_VARIANCE = optional output, the array of local variances
;	                 of data (before sqrt), if BOX_STDEV is specified.
;
; OUTPUTS:
;	BinVals = the data values at center of each bin (x-axis for plotting).
;
;	Revix = array of reverse indices from the IDL histogram function
;		(optional, computed only if argument is present).
;
;	Function returns a vector giving the histogram of data.
;	If BOX_STDEV is specified then
;	the histogram of local standard deviations of the data is returned.
;
; EXTERNAL CALLS:
;	function vartype
;	function trapez	(if /NORM)
; PROCEDURE:
;	Determine binsize (and bins) and call the IDL histogram function.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V.1991: added BINSIZE option and then NBIN limits the Max # bins.
;	F.V.1992: added option BOX_STDEV = width of moving box in which to
;	   compute Local standard deviations of data and return histogram of it.
;	F.V.1996: fixed bug where sometimes last bin element is dropped,
;		because IDL histogram returns one more than requested NBIN.
;	F.V.1997: added optional output argument "Revix" (reverse indices),
;		and keyword option /NORMALIZE.
;	F.V.2006: use binsize = 1 for integers.
;	F.V.2008: added default MaxBins = 66000 to limit histo of integers.
;	F.V.2012: added MBINS keyword for MaxBins.
;	F.V.2012: fixed bug if ceil( range/binsize ) exceeds 32 bits.
;	F.V.2017: if data is integer force maxv >= minv + 2.
;-

function Histo, data, BinVals, Revix, NBIN=Nbin, MBINS=MaxBins, MINDATA=minv, MAXDATA=maxv, $
                BOX_STDEV=boxdev, LOCAL_VARIANCE=var, NORMALIZE=norm, BINSIZE=binsize

	if keyword_set( boxdev ) then begin
		boxdev = ( 2 * (fix( boxdev )/2) + 1 ) > 3
		sz = size( data )
		if (sz[0] eq 0) or (min( sz[1:(sz[0]>1)] ) LT boxdev) then begin
			message,"expecting array for data ",/INFO
			print," with dimensions >", replicate( boxdev, sz[0]>1 )
			goto,SYNTAX
		   endif
		bw2 = boxdev^sz[0]
		fact = float( bw2 )/(bw2-1)
		var = smooth( (data - smooth( data, boxdev ))^2, boxdev ) *fact
		wv = where( var GT 0, nw )
		if( nw LE 0 ) then wv = where( var GE 0 )
		return, Histo( sqrt( var(wv) ), BinVals, Revix, $
				BIN=binsize, NBIN=Nbin, MIN=minv, MAX=maxv )
	   endif

	if N_elements( data ) LE 0 then begin
	SYNTAX:	print," "
		print,"syntax:   hd = Histo( data, BinVals, Revix, NBIN= )"
		print,"input:	data = array"
		print,"output:	BinVals = vector or bin values"
		print,"		Revix = optional reverse indices"
		print,"keywords:	NBIN=  or  BINSIZE= ,  MIN= , MAX="
		print,"options:		BOX_STDEV= , LOCAL_VARIANCE="
		BinVals = [0,1]
		return,[0,0]
	   endif

	if N_elements( data ) eq 1 then data = [data]

	if N_elements( maxv ) NE 1 then begin
		maxv = max( data, MIN=mind )
		if N_elements( minv ) NE 1 then  minv = mind
	   endif

	if N_elements( minv ) NE 1 then  minv = min( data )
	if (maxv LE minv) then maxv = minv + 1
	range = double( maxv ) - double( minv )

	dtype = varType( data,/CODE )
	if N_elements( MaxBins ) ne 1 then MaxBins = 66000
        MaxBins = MaxBins > 10

	if N_elements( binsize ) EQ 1 then begin
		if (dtype LE 3) or ( dtype ge 12 and dtype LE 15 ) then begin
                   if( binsize LT 1 ) then binsize = 1
                endif else binsize = double( binsize )
		if N_elements( Nbin ) EQ 1 then MaxBins = Nbin > 3
		Nbin = ceil( range/binsize )
		if N_elements( MaxBins ) EQ 1 then begin
                   if (Nbin GT MaxBins) or (Nbin LT 3) then begin
                      Nbin = MaxBins
                      binsize = range/Nbin
                   endif
                endif
		if (dtype LE 3) or ( dtype ge 12 and dtype LE 15 ) then begin
                   if( binsize LT 1 ) then binsize = 1
                endif
	  endif else if N_elements( Nbin ) EQ 1 then begin
		Nbin = Nbin > 3
		binsize = range/Nbin
		if (dtype LE 3) or ( dtype ge 12 and dtype LE 15 ) then begin
                   if( binsize LT 1 ) then binsize = 1
                endif
	   endif else begin
		if N_elements( Nbin ) NE 1 then Nbin = 100
		Nbin = Nbin > 3
		binsize = range/Nbin
		if (dtype LE 3) or ( dtype ge 12 and dtype LE 15 ) then binsize = 1
	    endelse

	Nbin = round( range/binsize )

	if( Nbin gt MaxBins ) then begin
		binsize = range/MaxBins
		Nbin = ceil( range/binsize )
	   endif

        if (dtype LE 3) or ( dtype ge 12 and dtype LE 15 ) then begin
           if( binsize LT 1 ) then binsize = 1
           if( range LT 2 ) then maxv = minv + 2
           range = maxv - minv
           Nbin = ceil( range/binsize )
        endif else begin
           if( binsize LT 1e-34 ) then binsize = 1e-34
           if( range LT 1e-33 ) then maxv = minv + 1e-33
        endelse

	if N_params() GE 3 then begin
           hfreq = histogram( data, BIN=binsize, MAX=maxv, MIN=minv, R=Revix )
        endif else hfreq = histogram( data, BIN=binsize, MAX=maxv, MIN=minv )

	nb = (N_elements( hfreq ) < Nbin) > 2
	BinVals = float( binsize * findgen( nb ) + minv + binsize/2 )

	if N_elements( hfreq ) GT Nbin then begin
		hfreq[Nbin-1] = hfreq[Nbin-1] + hfreq[Nbin]
		if N_elements( Revix ) GT 0 then Revix[Nbin] = Revix[Nbin+1]
		if keyword_set( norm ) then hfreq = hfreq/trapez( hfreq, BinVals )
		return, hfreq[0:Nbin-1]
	 endif else begin
		if keyword_set( norm ) then hfreq = hfreq/trapez( hfreq, BinVals )
		return, hfreq
	  endelse
end
