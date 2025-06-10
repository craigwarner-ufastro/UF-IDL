; Convenient procedure for plotting arrays of x-y points.
; Frank Varosi NASA/GSFC 1987, fixed by also adding _EXTRA

pro pxy, gxy, PSYM=psym, OVERPLOT=overplot, _EXTRA=extra, TRANPOSE=mtranspose


	if N_elements( psym ) NE 1 then psym=3
	s = size( gxy )

CASE s[0] OF

   2: BEGIN

      if keyword_set( mtranspose ) then begin
         pxy, transpose( gxy ), PSYM=psym, OVERPLOT=overplot, _EXTRA=extra
         return
      endif

      if keyword_set( overplot ) then oplot, gxy[*,0], gxy[*,1], PSYM=psym, _EXTRA=extra  $
				 else  plot, gxy[*,0], gxy[*,1], PSYM=psym, _EXTRA=extra
     END

   3: BEGIN
	if keyword_set( overplot ) then oplot, gxy[*,*,0], gxy[*,*,1],PSYM=psym, _EXTRA=extra $
				   else  plot, gxy[*,*,0], gxy[*,*,1],PSYM=psym, _EXTRA=extra

     END

   ELSE:   message,/INFO,"expecting either 2D or 3D array to plot"

 ENDCASE

return
end
