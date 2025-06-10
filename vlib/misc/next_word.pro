function next_word, text

; extract the first word of text string and return it,
;  the input string text is also returned without the first word.
;  text = string with words delimited by blanks.
; Frank Varosi NASA/Goddard 1989

	if N_elements( text ) GT 1 then text = strconcat( text + " " )
	Len = strlen( text )
	if (Len LE 0) then return,""
AGAIN:
	blank_pos = strpos( text, " " )
	if (blank_pos LT 0) then  blank_pos = Len

	if (blank_pos EQ 0) then begin
		text = strmid( text, 1, Len )
		goto,AGAIN
	   endif

	word = strmid( text, 0, blank_pos )

	text = strmid( text, blank_pos+1, Len )

return, word
end
