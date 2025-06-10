;+
; NAME:
;	InterLeave
; PURPOSE:
;	Determine the interleaving of two arrays of numbers,
;	by returning subscripts of the interleaving elements,
;	basically binning the array B into bins defined by array A.
;	Note that the input arrays can be in any order (non-sorted), but
;	for using result of this function you probably want A sorted first.
; CALLING:
;	Locs = InterLeave( A, B )
; INPUTS:
;	A = the reference grid array of numbers.
;	B = array of numbers to interleave with A.
; OUTPUT:
;	Function returns an integer array of same length as input B,
;	containing for each element of B the subscript of the element in A
;	which is less than (or equal) and closest to that element of B.
;	If there is no element of A less than or equal to an element of B
;	that subscript is set to -1.
; KEYWORD:
;	NBINS = optional output, # how many bins of grid A were occupied.
; EXTERNAL CALLS:
;	function Fsort	(to sort and keep original order if equal).
; PROCEDURE:
;	Proceed as in the beginning of pro match (by D.Lindler),
;	but then find interleave locations by observing jumps in flag array.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1995.
;-

function InterLeave, a, b, NBINS=nstart

	na = N_elements( a )
	nb = N_elements( b )

	if (na LE 0) OR (nb LE 0) then begin
		message,"must supply two arrays for interleaving",/INFO
		print,"syntax:  B_in_A = interleave( A, B )"
		return,[0]
	   endif

	if min( B ) LT min( A ) then begin
		if (na GT 32768) then Locs = Lonarr( nb )  $
				 else Locs = intarr( nb )
		Locs(*) = -1
		w = where( B GE min( A ), nw )
		if (nw GT 0) then Locs(w) = InterLeave( A, B(w), NBINS=nstart )
		return, Locs
	   endif

	sub = Fsort( [a,b] )			;combine a and b and sort.
	ind = [ Lindgen( na ), Lindgen( nb ) ]	;combined list of indices.
	ind = ind(sub)				;same sort order

	flag = [ bytarr( na ), replicate( 1B, nb ) ]	;to indicate which array
	flag = flag(sub)				;same sort order.
	sub = 0
	flags1 = shift( flag, -1 )

; Find interleave locations by observing jumps in flag array:

	wstart = where( flags1 GT flag, nstart )
	wstop = where( flags1 LT flag, nstop )

	if (nstart NE nstop) then begin
		message,"possible problem:   nstart NE nstop",/INFO
		print, nstart, nstop, string( 7B )
	   endif

	flag = 0
	flags1 = 0

	if (na GT 32768) then  Locs = Lonarr( nb )  else  Locs = intarr( nb )
	ws1 = wstart + 1
	for i=0,nstop-1 do Locs(ind(ws1(i):wstop(i))) = ind(wstart(i))

return, Locs
end
