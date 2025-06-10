;+
; NAME:
;	get_text_input
;
; PURPOSE:
;	Prompt the user for input with a question and
;	return the text string typed by user.
;	For X-windows the input is thru a text widget, in middle of screen,
;	and the text widget waits until carriage return is entered.
;	Otherwise input is in terminal window, with a beep to alert user.
;
; CALLING:
;	text = get_text_input( question )
;
; INPUTS:
;	question = string(s), prompt for input, default is null string.
;		If an array is passed, each element is shown on its own line.
;
; KEYWORDS:
;	DEFAULT_INPUT = optional string, setting the default response.
;
;	WINDOW = window number to place menu next to.
;
; OUTPUTS:
;	Function returns the text string entered by user,
;	with leading and trailing blanks removed.
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1993.
;	F.V.1997, question can be a string array, shown as multiple lines.
;	F.V. 2011, keyword WINDOW to popup next to window.
;-

function get_text_input, question, DEFAULT_INPUT=default, WINDOW=mwind, FLOATP=floatp

	sz = size( question )
	if sz[sz[0]+1] NE 7 then question = ""
	sz = size( default )
	if sz[sz[0]+1] NE 7 then default = ""

	if ( !D.flags AND 65536 ) ne 0 then begin

           base = WIDGET_BASE( TIT="for Text input...", $
                               XOFF=400, YOFF=400, /COLUMN, SPACE=10 )
           for i=0,N_elements( question )-1 do Labid = WIDGET_LABEL( base, VAL=question[i] )
           Textid = WIDGET_TEXT( base, /EDIT, /FRAME, VALUE=default )
           WIDGET_CONTROL, base,/REALIZE
           WIDGET_CONTROL, Textid,/INPUT_FOCUS

           if N_elements( mwind ) eq 1 then begin
              if( mwind ge 0 ) then begin
                 owind = !D.window
                 wset, mwind
                 device,GET_WINDOW_POS=wpos
                 xpos = (wpos[0] - 100) > 0
                 ypos = (!DEVY - wpos[1] - 3*!D.y_vsize/4) > 0
                 if( owind ge 0 ) then wset, owind
                 widget_Location, base, XPOS=xpos, YPOS=ypos
              endif
           endif

           event = WIDGET_EVENT( Textid )
           WIDGET_CONTROL, event.id, GET_VALUE=text
           WIDGET_CONTROL, base, /DESTROY

           if keyword_set( floatp ) then begin
              return, float( strtrim( text[0], 2 ) )
           endif else return, strtrim( text[0], 2 )

        endif else begin

           text = ""
           if strlen( default ) LE 0 then prompt = question + " : " $
			else prompt = question + " (DEFAULT=" + default + ") : "
           read, string(7b) + " " + prompt, text
           if strlen( text ) LE 0 then text = default
           if keyword_set( floatp ) then begin
              return, float( strtrim( text, 2 ) )
           endif else return, strtrim( text, 2 )
        endelse
end
