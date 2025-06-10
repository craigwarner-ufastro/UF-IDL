function menus, fcn, choices, help_str, INITSEL=initsel
;+
; NAME:
;	Menus
; PURPOSE:
;	Implement a crude menu facility using character strings
;	at the bottom of a window.
; CALLING SEQUENCE:
;	Choice = Menus( fcn, choices, help_str )
; INPUTS:
;	fcn = 0 to draw choices on bottom of window, fcn is then set = 0.
;	fcn = 1 to select a choice, and to "unhighlight" previous choice.
;
;	Choices = string array containing text for each choice.
;	Help_str = string array of same # elements as choices.  Help text is
;	    displayed on top of the window if the corresponding choice is made.
;	INITSEL = choice # to have initially pre-selected before user selects.
; OUTPUTS:
;	Function output = choice, from 0 to # of elements in string arrays -1.
;	If the right button is pressed, -1 is returned to indicate done.
; COMMON BLOCKS:
;		menus_common = our common
; SIDE EFFECTS:
;	Text is written on the current window display.
; MODIFICATION HISTORY:
;	Dms, Dec, 1987.
;	DMS, April, 1989,  added check for button debouncing (see repeat until).
;	FV, 1990, added INITSEL & Location of choices based on string Lengths.
;-
common menus_common, isel, choice_Locs, ych, boxx, boxy

on_error,2	;Return to caller if an error occurs

if (fcn EQ 0) then begin	; Output original choices....

	nst = n_elements( choices )
	choice_Locs = [ 0, strlen( choices ) ]
	for i=1,nst do choice_Locs(i) = choice_Locs(i-1) + choice_Locs(i)
	choice_Locs = choice_Locs/float( choice_Locs(nst) )
	nst1 = 1./nst
	ych =  1.0 * !d.y_ch_size / !d.y_vsize	;Char ht in normal units
	boxy = [0., ych, ych, 0.]
	boxx = [0., 0., nst1-0.05, nst1-0.05]	;Box for highlight

	for i=0,nst-1 do xyouts, choice_Locs(i), 0., choices(i),/norm,/noclip
	fcn = 1
	if N_elements( initsel ) EQ 1 then begin
		if (initsel GE 0) AND (initsel LT nst) then begin
			isel = initsel
			polyfill, choice_Locs(isel) + boxx, boxy, col=255,/norm
			xyouts,0,1.0-ych,help_str(isel),/norm,/nocl
			xyouts, choice_Locs(isel), 0, choices(isel),  $
							/norm, col=0, /noclip
			return,isel
		  endif else begin
			isel = -1
			return,-1
		   endelse
	  endif else begin
		isel = -1
		return,-1
	   endelse

  endif else begin

	if isel ge 0 then begin		;first Remove old choice
	  xyouts,0,1.0-ych,help_str(isel),/norm,col=0,/noclip    ;remove instr
	  polyfill,choice_Locs(isel) + boxx,boxy,col=0,/norm   ;remove highlight
	  xyouts, choice_Locs(isel), 0, choices(isel), /norm,col=255,/noclip
	 endif	

	isel = -1
	y = 1000
	repeat  cursor,x,y,/nowait,/norm  until (!err eq 0)   ;until no buttons
	!err = 0
	while (y gt ych) and (!err ne 4) do cursor,x,y,/norm  
					;Button hit on bottom or right button.
	if (!err eq 4) then return,-1

	w = where( choice_Locs GE x, nc )
	if (nc LE 0) then isel=0  else isel = (w(0)-1) > 0

	polyfill, choice_Locs(isel) + boxx,boxy,col=255,/norm	;highlight name
	xyouts,0,1.0-ych,help_str(isel),/norm,/nocl	;instructions
	xyouts, choice_Locs(isel), 0, choices(isel), /norm,col=0,/noclip
	return,isel

   endelse

end
