pro blink_cursor, Nblink, blink_sec

	if N_elements( Nblink ) NE 1 then Nblink = 10
	if N_elements( blink_sec ) NE 1 then blink_sec = .05

	for i=1,Nblink do begin

		tvcrs,0
		wait, blink_sec
		tvcrs,1
		wait, blink_sec
	  endfor
return
end
