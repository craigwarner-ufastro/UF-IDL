;+
; NAME:
;	accumulate
; PURPOSE:
;	Perform running accumulation of one of the dimension in 2D array,
;	for example, a matrix of histograms. 
; CALLING:
;	haccum = accumulate( histogs )
; INPUTS:
;	histogs = matrix ( Nbins, Nhistog ) of histograms to accumulate,
;		or any 2D array to be accumulated in first dimension.
; KEYWORDS:
;	/NO_TRANSPOSE : assume input matrix of histograms is already transposed,
;		accumulation is performed on second dimension of input matrix.
; HISTORY:
;	Frank Varosi NASA/GSFC 1992.
;-

function accumulate, histogs, NO_TRANSPOSE=notrans

	sh = size( histogs )

	if (sh(0) EQ 1) then begin
		haccum = histogs
		for i=1L,sh(3)-1 do haccum(i) = haccum(i) + haccum(i-1)
		return, haccum
	   endif

	if (sh(0) NE 2) then begin
		message,"bad arg: expecting 1-D or 2-D array",/INFO
		return, sh
	   endif

	if keyword_set( notrans ) then haccum = histogs $
				else haccum = transpose( histogs )
	sh = size( haccum )
	for i=1L,sh(2)-1 do haccum(*,i) = haccum(*,i) + haccum(*,i-1)

return, transpose( haccum )
end
