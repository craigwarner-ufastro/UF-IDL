;+
; NAME:
;	MinMax_Histo
; PURPOSE:
;	Determine suitable min & max range which rejects requested fraction
;	of data values that are at the low and high extremes.
; CALLING:
;	MinMax_Histo, data, hmin, hmax
; INPUTS:
;	data
; KEYWORDS:
;	REJECT_FRACTION = fraction of extremes to reject, default = 0.001
;	NBIN = default is 2000.
;	VALUES = optional returned histogram bin values.
;	HACCUMULATE = optional returned accumulated histogram
; OUTPUTS:
;	hmin, hmax = min & max range which rejects requested fraction
;		of data values that are at the low and high extremes.
; EXTERNAL CALLS:
;	function Histo
;	function accumulate
; PROCEDURE:
;	Accumulate and normalize the histogram and use where function.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1995.
;-

pro MinMax_Histo, data, hmin, hmax, REJECT_FRACTION=reject, NBIN=nbin, $
					 VALUES=hv, HACCUMULATE=ha

	if N_elements( nbin ) ne 1 then nbin = 2000
	if N_elements( reject ) ne 1 then reject = 0.001
	if (reject GE 1) then reject = 0.001

	if (reject LE 0) then begin
		hmin = min( data, MAX=hmax )
		return
	   endif

	ha = accumulate( Histo( data, hv, NBIN=nbin ) )
	ha = float( ha )/max( ha )

	w = where( (ha LE 1-reject) and (ha GE reject), nw )

	if (nw GT 0) then begin
		hmin = hv(w(0))
		hmax = hv(w(nw-1))
	 endif else begin
		hmin = 0
		hmax = 0
	  endelse
end
