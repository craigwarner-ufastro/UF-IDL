; NAME:
;
; PURPOSE:
;
; CALLING:
;
; INPUTS:
;
; KEYWORDS:
;
; OUTPUTS:
;
; EXTERNAL CALLS:
;	function mod2pi
; PROCEDURE:
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.

function DFT_uneven, times, data, omega

	tw = omega * times
	t2 = mod2pi( 2 * tw )
	tt = mod2pi( tw - atan( total(sin(t2)) , total(cos(t2)) )/2 )
	ctt = cos( tt )
	stt = sin( tt )
	aw = sqrt( total( ctt*ctt ) )
	bw = sqrt( total( stt*stt ) )

return, complex( total( data * ctt )/aw , total( data * stt )/bw )
end

;+
; NAME:
;
; PURPOSE:
;
; CALLING:
;
; INPUTS:
;
; KEYWORDS:
;
; OUTPUTS:
;
; EXTERNAL CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function DFTune, times, data, max_freq, delta_freq, OVERSAMPLE=oversample

	nt = N_elements( times )

	if (nt LE 0) then begin
		message,"args: times, data, max_freq",/INFO
		return,0
	   endif

	if (N_elements( data ) NE nt) then begin
		message,"# of times and data points do NOT agree",/INFO
		return,0
	   endif

	if min( times(1:*) - times ) LT 0 then begin
		message,"sorting the data into time order",/INFO
		sot = sort( times )
		times = times( sot )
		data = data( sot )
	   endif

	tran = double( times(nt-1) ) - times(0)
	if N_elements( delta_freq ) NE 1 then delta_freq = (nt-1)/(2*tran*nt)
	if N_elements( max_freq ) NE 1 then max_freq = nt/tran/2

	nf = round( max_freq/delta_freq ) + 1
	omega = 2 * !DPI * dindgen( nf ) * delta_freq
	DFT = complexarr( nf )

	for i=1,nf-1 do DFT(i) = DFT_uneven( times, data, omega(i) )

;	ntr = sqrt( nt )
;	ic = complex( 0, 1 )
;	DFT = DFT * exp( -ic * omega * times(0) ) * (ntr/sqrt(2))
	DFT(0) = total( data )/nt

	nfo = 2^ceil( aLog(nf)/aLog(2) )
	if keyword_set( oversample ) AND ((nfo-nf) LT (nfo/4)) then nfo = 2*nfo

return, [ DFT, complexarr( 2*(nfo-nf)+1 ), conj( rotate( DFT(1:*), 2 ) ) ]
end
