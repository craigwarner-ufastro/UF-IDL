function imagHeqZ ,image ,Pflag ,Heq,Hist ,grzero,gzim, TOPVAL=topval

; Returns byte histogram equalization of image with weighting IH(x)*(x^Pflag)
;	where IH(x) is the integrated histrogram.
; Pflag = 0 or omitted gives the usual histogram equalization .
; IMAGE input can be byte, int, Long, float, but values must be greater than 2.
; Imb = imagHeq( IMAGE,Pflag,Heq,Hist )            GRZERO,GZIM are NOT computed
; Imb = imagHeq( IMAGE,Pflag,Heq,Hist, GRZERO,GZIM )     GRZERO,GZIM are OUTPUT
; Imb = imagHeq( SIZE(image),Pflag,Heq,Hist, GRZERO,GZIM )  use previous values
; Frank Varosi, U.of MD., 1988.
; F.V. 1990, modif. for IDL-V2.

	if N_elements( topval ) NE 1 then  topval=!D.table_size-1
	if N_elements( Pflag ) LE 0 then  Pflag=0
	s = size (image)

	if (s(0) LT 2) then begin
	  s = image		  	;assume this is "size" of an image.
	   if (s(0) LT 2) then begin
		print,' must supply image or its "size"'
		return,s
	     endif
	   if ((N_elements(grzero) LE 0) OR (N_elements(gzim) LE 0)) then begin
		print," must supply where image > 0"
		return,s
	     endif
	  endif else if (N_params(0) GE 6) then begin
		grzero = where( image )
		gzim = image(grzero)
	   endif

	if N_elements(gzim) GT 0 then  Hist = histogram( gzim ) $
				 else  Hist = histogram( image ) 
	Nh = N_elements( Hist )

	if (Nh LE 2) then begin
		print," Image values need to be greater, multiply by factor"
		return,image
	  endif

	if (Pflag NE 0) then  Histint = Hist * findgen( Nh )^Pflag $
			else  Histint = Hist
	Histint(0)=0
	for i = 1L, Nh-1 do Histint(i) = Histint(i-1) + Histint(i)

	Hmax = Histint(Nh-1)
	higz = where( Histint )
	Hmin = Histint(higz(0))
	Heq = bytarr( Nh )

	if (Hmin GE Hmax) then  Heq(higz)=topval  else begin
		fact = topval/float( Hmax - Hmin ) 	;map Hmin to color #1
		Heq(higz) = byte( fact*(Histint(higz)-Hmin) + 1 )
	  endelse

	if N_elements(gzim) GT 0 then begin

		imagbyt = bytarr( s(1), s(2) )
		imagbyt(grzero) = Heq(gzim)

	   endif  else  imagbyt = Heq(image)

return ,imagbyt
end
