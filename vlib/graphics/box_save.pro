pro box_save, RESTORE=restore

;Frank Varosi NASA/GSFC 1989

  common box_draw, Lox,Loy, Hix,Hiy, $
		   Horiz_B, Horiz_T, $
		   Vert_L, Vert_R, Rectangle, box_window

  common box_save, xLo,yLo, xHi,yHi, $
		   saveHB, saveHT, saveVL, saveVR, saveRect, saveWindow

	if keyword_set( restore ) then begin

		if N_elements( xLo ) GT 0 then begin
			Lox = xLo
			Loy = yLo
			Hix = xHi
			Hiy = yHi
			box_window = saveWindow
		   endif

		if N_elements( saveHB ) GT 1 then begin

			Horiz_B = saveHB
			Horiz_T = saveHT
			Vert_L = saveVL
			Vert_R = saveVR
			saveHB = 0

		  endif else if N_elements( saveRect ) GT 1 then begin

			Rectangle = saveRect
			saveRect = 0
		   endif

	 endif else begin

		if N_elements( Lox ) GT 0 then begin
			xLo = Lox
			yLo = Loy
			xHi = Hix
			yHi = Hiy
			saveWindow = box_window
		   endif

		if N_elements( Horiz_B ) GT 1 then begin

			saveHB = Horiz_B
			saveHT = Horiz_T
			saveVL = Vert_L
			saveVR = Vert_R
			Horiz_B = 0

		  endif else if N_elements( Rectangle ) GT 1 then begin

			saveRect = Rectangle
			Rectangle = 0
		   endif

	  endelse

return
end
