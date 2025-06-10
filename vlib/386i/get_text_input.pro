function get_text_input, question, DEFAULT_INPUT=default
;+
; NAME:
;	get_text_input
;
; PURPOSE:
;	Prompt the user for input with a question and
;	return the text string typed by user.
;	Input is in terminal window, with a beep to alert user.
;	This version kept separate for Sun386i only.
; CALLING:
;	text = get_text_input( question )
; INPUTS:
;	question = string, prompt for input, default is null string.
; KEYWORDS:
;	DEFAULT_INPUT = optional string, setting the default response.
; OUTPUTS:
;	Function returns the text string entered by user,
;	with leading and trailing blanks removed.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1993.
;-
	sz = size( question )
	if sz(sz(0)+1) NE 7 then question = ""
	sz = size( default )
	if sz(sz(0)+1) NE 7 then default = ""

	text = ""
	if strlen( default ) LE 0 then prompt = question + " : " $
		else prompt = question + " (DEFAULT=" + default + ") : "
	read, string(7b) + " " + prompt, text
	if strlen( text ) LE 0 then text = default

return, strtrim( text, 2 )
end
