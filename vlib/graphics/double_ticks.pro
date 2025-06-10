pro double_ticks, data, tickvals, ticknames, NEGATIVE=negative

;Double the number of tick marks on an axis,
; but Label only the current <tickvals>  (assumed equally spaced, increasing).
; Use <tickvals_new> to specify ![XYZ].TICKV
;  and <nticks>  to specify ![XYZ].TICKS
;  and <ticknames>  to specify ![XYZ].TICKNAME
; (thus every other major tick will be labeled, in between will be blank).
; /NEG will reverse order of labels, so values will be descending.
;Frank Varosi STX @ NASA/GSFC 1991.

	if N_elements( tickvals ) LT 2 then begin
		if N_elements( tickvals ) GT 0 then tickvals_new = tickvals
		ticknames = strtrim( fix( tickvals ), 2 )
		message," need more than one existing tick mark",/INFO
		return
	   endif

	tmin = min( tickvals )
	tmax = max( tickvals )
	tint = tickvals(1) - tickvals(0)

	if (tint LE 0) then begin
		tickvals_new = tickvals
		ticknames = strtrim( fix( tickvals ), 2 )
		message," tick interval is zero",/INFO
		return
	   endif

	tv = [ tmin-tint, tickvals, tmax+tint ]
	tval = transpose( [ [tv], [ tv + tint/2. ] ] )

	if keyword_set( negative ) then sign=-1 else sign=1
	tname = strtrim( sign * fix( tval ), 2 )
	tname(1,*) = " "

	dmax = max( data, MIN=dmin )
	w = where( (tval GE dmin) AND (tval LE dmax) )
	tickvals = tval(w)
	ticknames = tname(w)
	if !DEBUG then stop
return
end
