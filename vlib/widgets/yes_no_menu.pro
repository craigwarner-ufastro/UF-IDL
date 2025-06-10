;+
; NAME:
;	yes_no_menu
;
; PURPOSE:
;	Provide a simple way to ask the interactive user a Yes/No question.
;	A yes/no menu is created and widget/menu waits for mouse selection.
; CALLING:
;	answer = yes_no_menu( question )
;
; INPUTS:
;	question = string, the question to be answer with yes/no,
;		note that question mark "?" is automatically tacked on the end.
; KEYWORDS:
;	/BINARY	: causes result of function to be binary integer, 0=no, 1=yes.
;		(default is string result).
;
;	/NO_DEFAULT : causes mouse to be positioned over the NO selection.
;		(default is mouse  positioned over YES).
;
;	WINDOW = window number to place menu next to.
;
; OUTPUTS:	Function returns string YES or NO, or if /BINARY
;		it returns an integer 1 or 0, indicating user selection.
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1990.
;	F.V. 1993, added use of X widget menu.
;	F.V. 2011, keyword WINDOW to popup next to window.
;-

function yes_no_menu, question, NO_DEFAULT=no_init, BINARY=binary, WINDOW=mwind

  sz = size( question )
  if sz[sz[0]+1] NE 7 then question = "?"

  if ( !D.name EQ "SUN" ) then begin

     menu = [ question + "?", "NO", "YES" ]
     if keyword_set( no_init ) then init=1 else init=2

     sel = wmenu( menu, INIT=init, TIT=0 ) > 1

     if keyword_set( binary ) then  return, sel-1  else  return, menu[sel]

  endif else begin

     base = WIDGET_BASE( TIT="decisions...", XOFF=!DEVX/3, YOFF=!DEVY/4, /COLUMN )
     Label = WIDGET_LABEL( base, VAL=question + " ?" )
     bbase = WIDGET_BASE( base, /ROW, SPACE=20, XPAD=20, YPAD=20, /FRAME )

     bwid = Lonarr(2)
     bval = [ "NO", "YES" ]

     for i=0,1 do bwid[i] = WIDGET_BUTTON( bbase, VAL=bval[i], UVAL=i )
     WIDGET_CONTROL, base,/REALIZE

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

     if keyword_set( no_init ) then WIDGET_CONTROL, bwid[0],/INPUT_FOCUS $
     else WIDGET_CONTROL, bwid[1],/INPUT_FOCUS

     event = WIDGET_EVENT( base )

     if keyword_set( binary ) then begin
        WIDGET_CONTROL, event.id, GET_UVAL=answer
     endif else begin
        WIDGET_CONTROL, event.id, GET_VAL=answer
     endelse

     WIDGET_CONTROL, base, /DESTROY
     return, answer

  endelse
end
