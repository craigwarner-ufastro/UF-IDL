function imagHeq ,image ,Pflag ,Nbins ,Heq,Hist, maxim, TOPVAL=topval

; 	Imb = imagHeq( IMAGE, Pflag, Nbins, Heq,Hist )
; Returns byte histogram equalization of image with weighting IH(x)*(x^Pflag)
;	where IH(x) is the integrated histrogram.
; Pflag = 0 or omitted gives the usual histogram equalization .
; IMAGE input can be byte, int, Long, float,
;   values are temporarily scaled to get # histogram bins = Nbins.
; Default for Nbins is 1000 (unless image is allready BYTE, INTEGER, LONG)
; Frank Varosi U.Md. 1988.
; F.V. 1990, mod for IDL-V2.

	s = size( image )
	imtyp = s(s(0)+1)

	if (imtyp GE 4) then begin		;if floating point then scale.

		if N_elements( Nbins ) LE 0 then  Nbins = 1000
		maxim = max( image )
		imscaled = (Nbins/maxim) * image
		Hist = histogram( imscaled ) 
		Nh = N_elements( Hist )

	  endif else begin			;if integer just get histogram.

		Hist = histogram( image )
		Nh = N_elements( Hist )
		Nbins = Nh
	   endelse

	if N_elements( topval ) NE 1 then  topval=!D.table_size-1
	if N_elements( Pflag ) NE 1 then  Pflag=0

	     if (Pflag LE 0) then Histint = Hist                          $
	else if (Pflag EQ 1) then Histint = Hist * indgen( Nh )           $
	else if (Pflag GT 0) then Histint = Hist * findgen( Nh )^Pflag

	Histint(0)=0
	for i=1,Nh-1 do Histint(i) = Histint(i) + Histint(i-1)
	Hmax = Histint(Nh-1)
	higz = where( Histint )
	Hmin = Histint(higz(0))
	Heq = bytarr( Nh )

	if (Hmin GE Hmax) then  Heq(higz)=topval  else begin
		fact = topval/float( Hmax - Hmin ) 	;map Hmin to color #1
		Heq(higz) = byte( fact*(Histint(higz)-Hmin) + 1 )
	  endelse

	if (imtyp GE 4) then  return, Heq(imscaled)	$
			else  return, Heq(image)
end
