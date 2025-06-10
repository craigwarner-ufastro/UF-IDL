pro pause_mouse, Location, WINDOW=window

   common pause, pause_text

	if N_elements( pause_text ) LE 0 then $
	   pause_text = [ " "," ","click HERE to continue..."," "," "," "]

	if N_elements( Location ) EQ 1 then			$
		pause_text(0) = "PAUSING in " + Location	$
	   else pause_text(0) = "PAUSING..."

	printw, pause_text, LINE=-3, /ERASE, WINDOW=window

	tvcrs,.5,.5,/NORM
	cursor,/DEV,x,y

	printw, replicate( " ", N_elements( pause_text )  ), LINE=-3, /ERASE
return
end
