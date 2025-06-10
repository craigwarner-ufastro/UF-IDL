function ext_frac_image, image, xbot,xtop, ybot,ytop, MAGF=Magf, IMSUB=imsub

; Extract a part of image specified up to fractional pixels
; by resampling image on fractionally shifted grid, using REBIN twice.
;
; ( xbot:xtop , ybot:ytop ) is sub-image location in MAGNIFIED image
;				(virtual magnified grid coordinates),
; 	thus performing fractional pixel extraction when MAGF=Magf > 1.
; Sub-image is returned at normal magnification (=1).
;
; Frank Varosi NASA/GSFC 1990
; F.V. 1991, fixed bugs, use /SAMPLE for Magnify and averaging for De-Magnify.

	if N_elements( Magf ) NE 1 then Magf=1
	Magf = fix( Magf ) > 1
	if (Magf LE 1) then return, image(xbot:xtop,ybot:ytop)

	s = size( image ) - 1		;compute Loc of bigger approx. image.
	xb = fix( xbot/Magf ) > 0
	xt = fix( 1 + xtop/Magf ) < s(1)	
	yb = fix( ybot/Magf ) > 0
	yt = fix( 1 + ytop/Magf ) < s(2)	

	imsub = image(xb:xt,yb:yt)			;approximate subimage.
	ss = size( imsub ) * Magf
	imsub = rebin( imsub, ss(1), ss(2), /SAMPLE )	;Magnify it by sampling.

	xsiz = xtop - xbot +1
	xsiz = xsiz - (xsiz MOD Magf)	;make sure result is whole pixels.
	ysiz = ytop - ybot +1
	ysiz = ysiz - (ysiz MOD Magf)

	ss = ss - 1
	xb = (xbot - xb*Magf) > 0	;compute Loc of exact in approximate.
	xt = (xb + xsiz -1) < ss(1)
	yb = (ybot - yb*Magf) > 0
	yt = (yb + ysiz -1) < ss(2)

   ;DE-magnify the subimage by AVERAGING over neighborhoods (REBIN default).

return, rebin( imsub(xb:xt,yb:yt), xsiz/Magf, ysiz/Magf )
end
