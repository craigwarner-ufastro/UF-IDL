;+
; NAME:
;	get_words
; PURPOSE:
;	Separate text string(s) into array of words and return it.
; CALLING:
;	words = get_words( text )
; EXAMPLE:
;	dirs = get_words( !path, DELIMIT=":" )
; INPUTS:
;	text = string or string array. An array of strings is considered
;		to have a delimeter at end of each array element.
; KEYWORDS:
;	DELIMITERS = string(s) of single characters to search for in text,
;		and then use to separate text into words,
;		default = null, tab, comma and blank characters.
;		Note that multi-character strings will be just
;		converted to array of single characters since the
;		the code is restricted to finding single char. delimeters.
;
;	WNUMS = optional integer scalar/array of subscript(s) to select
;		and return out of the words array.
; OUTPUTS:
;	Function returns arrays of strings: the words in text.
; PROCEDURE:
;	Convert string to byte array and find where delimiters are.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1989.
;	F.V.1990 : generalized and added DELIMITERS keyword.
;	F.V.2017 @ UF : added keyword COUNT= giving # words returned.
;-

function get_words, text, DELIMITERS=delimiters, WNUMS=wnums, COUNT=nword

        nword = 0
	Nst = N_elements( text )
	if (Nst LE 0) then return,""

	if N_elements( delimiters ) GT 0 then begin
		delimb = byte( delimiters )
		sz = size( delimb )
		Ndelim = sz(sz[0]+2)
		if (sz[0] GT 1) then delimb = reform( delimb, Ndelim )
	 endif else begin
		delimb = [ 0b, 9b, byte( ", " ) ]
		Ndelim = N_elements( delimb )
	  endelse 

	textb = byte( text )
	if (Nst GT 1) then textb = [ textb, replicate( delimb[0], 1, Nst ) ]

        Len = N_elements( textb )
        dpos = 0

        if (Len LE 1) then begin
           nword = 1
           return, string( textb )
        endif

	for i=0,Ndelim-1 do begin
		wpos = where( textb EQ delimb[i], npos )
		if (npos GT 0) then dpos = [ dpos, wpos+1 ]
	  endfor

        if N_elements( dpos ) LE 1 then begin
           nword = 1
           return,text
        endif

        dpos = dpos( sort( dpos ) )
	wend = [ dpos[1:*]-2, Len-1 ]

	w = where( wend GE dpos, nword )
	if (nword LE 0) then return,""

	dpos = dpos[w]
	wend = wend[w]
	words = strarr( nword )

	for i=0,nword-1 do  words[i] = string( textb( dpos[i]:wend[i] ) )

        if N_elements( wnums ) gt 0 then begin
           nword = N_elements( wnums )
           return,words[wnums]
        endif else return, words
end
