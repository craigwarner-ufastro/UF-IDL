;+
; NAME:
;	CW_Draw_Axes
;
; PURPOSE:
;	This compound widget merges a central scrolling draw widget,
;	for displaying an image or some user defined graphics,
;	with two smaller scrolling draw widgets for the X and Y axes
;	of the image/graphics. The scroll bars of all the draw widgets
;	are ganged so that moving any one will move the others, thus
;	all viewport events are swallowed. Button events in the X & Y axes
;	draw widgets cause all viewports to scroll and are then swallowed.
;	All motion/button events occuring in the central draw widget
;	are reported to the parent or higher level widgets.
;
; CATEGORY:
;	Compound widgets.
;
; CALLING:
;	Widget = CW_Draw_Axes( Parent )
;
; INPUTS:
;       Parent = the ID of the parent widget.
;		If not specified, a base is created just for this widget.
;
; KEYWORDS:
;	RETAIN:	Controls the setting for backing store for both windows.
;		If backing store is provided, a window which was obscured
;		will be redrawn when it becomes exposed. Set RETAIN=0 for
;		no backing store. Set RETAIN=1 to "request backing store
;		from server" (this is the default). Set RETAIN=2 for IDL
;		to provide backing store.
;
;	/FRAME:	causes frame to be drawn around the widget,default is no frame.
;	UVALUE = the user value for the widget.
;
;	SIZE_XY	= two integer array specifying the width and height of
;		the draw window (in pixels), default = [ 2048, 2048 ].
;
;	SCROLL_SIZE_XY = two integer array specifying the width and height of
;			the visible part of the draw window, default =[512,512].
;
; OUTPUTS:
;       The ID of the created widget is returned (widget is not realized).
;
; PROCEDURE:
;
;	WIDGET_CONTROL, id, SET_VALUE=value
;		can be used to change the display.
;
;	Action depends on the "value" variable type:
;
;		string: name of user provided procedure to be called.
;			The argument passed to procedure is that state of CW,
;			which has window numbers and other info, so that
;			graphics can be directed into the draw widgets.
;
;		two integers:  sets all draw viewports to the (x,y) specified.
;
;		single integer equaling zero:  erases all 3 draw windows.
;
;		image (matrix):  it is displayed in central draw window.
;
;		structure:  tags should be handles pointing to images which are
;			then displayed in central, x-axis, y-axis windows, resp.
;
;	WIDGET_CONTROL, id, GET_VALUE=state
;		can be used to obtain the state (structure var.) of widget.
;
; HISTORY:
;	written: Frank Varosi 1994 HSTX @ NASA/GSFC.
;-

;-----------------------------------------------------------------------------

pro Draw_Axes_Set_V, id, value

  stash = WIDGET_INFO( id, /CHILD )
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  if (state.draw.window LT 0) then begin      ;Get window #s from draw widgets.

	WIDGET_CONTROL, state.draw.id, GET_VALUE=win_temp
	state.draw.window = win_temp(0)

	WIDGET_CONTROL, state.xaxis.id, GET_VALUE=win_temp
	state.xaxis.window = win_temp(0)

	WIDGET_CONTROL, state.yaxis.id, GET_VALUE=win_temp
	state.yaxis.window = win_temp(0)
    endif

  sv = size( value )

; if value is a string, assume it is a user provided procedure to be called.
; if value is an image (matrix), display it in central draw window.
; if value is a structure, assume that its tags are handles pointing to
;  images which will then be displayed in central, x-axis, y-axis draw windows.
; if value is two integers:  set all draw viewports to the [x,y] specified.
; if value is single integer equaling zero:  erase all 3 draw windows.

  CASE sv(sv(0)+1) OF

	1:	if (sv(0) EQ 2) then begin
			save_win = !D.WINDOW
			WSET, state.draw.window
			TV, value
			if (save_win GE 0) then wset, save_win
		   endif

	7:	Call_Procedure, value, state

	8: BEGIN
		save_win = !D.WINDOW

		for i=0,2 do begin
			handle_value, value.(i), image,/NO_COPY
			WSET, state.(i).window
			TV, image
			handle_value, value.(i), image,/NO_COPY,/SET
		  endfor

		if (save_win GE 0) then wset, save_win
	     END

	else:	if (sv(0) EQ 2) then begin
			save_win = !D.WINDOW
			WSET, state.draw.window
			TVSCL, value
			if (save_win GE 0) then wset, save_win
		 endif else if N_elements( value ) EQ 2 then begin
			Widget_Control, state.draw.id, SET_DRAW_VIEW=value
			Widget_Control, state.xaxis.id, SET_DRAW_V=[value(0),0]
			Widget_Control, state.yaxis.id, SET_DRAW_V=[0,value(1)]
			state.draw.view = value
		  endif else if N_elements( value ) EQ 1 then begin
			if (value EQ 0) then begin
				save_win = !D.WINDOW
				for i=0,2 do begin
					WSET, state.(i).window
					erase
				  endfor
				if (save_win GE 0) then wset, save_win
			   endif
		   endif
   ENDCASE

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
END

;-----------------------------------------------------------------------------

function Draw_Axes_Get_V, id

  ; Retrieve and return the state

  stash = WIDGET_INFO( id, /CHILD )
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  Widget_Control, state.draw.id, GET_DRAW_VIEW=vxy
  state.draw.view = vxy
  ret = state
  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
  RETURN, ret
END

;-----------------------------------------------------------------------------

function Draw_Axes_Event, event

;Retrieve the structure from the child that contains the sub ids

  stash = WIDGET_INFO( event.handler, /CHILD )
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY

  if (event.type EQ 3) then begin	;Connect (gang) all the viewports.
					;and Swallow all viewport events

  	Widget_Control, event.id, GET_DRAW_VIEW=vxy

	if (event.id NE state.draw.id) then begin
		Widget_Control, state.draw.id, GET_DRAW_VIEW=vxyd
		CASE event.id OF
			state.xaxis.id:	vxy(1) = vxyd(1)
			state.yaxis.id:	vxy(0) = vxyd(0)
		 ENDCASE
		Widget_Control, state.draw.id, SET_DRAW_VIEW=vxy
	   endif

	Widget_Control, state.xaxis.id, SET_DRAW_VIEW=[vxy(0),0]
	Widget_Control, state.yaxis.id, SET_DRAW_VIEW=[0,vxy(1)]

	state.draw.view = vxy
	WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
	return,0

   endif else if (event.id NE state.draw.id) then begin		;swallow these

	if (event.type EQ 0) then begin		;do only on the button press.

		CASE event.id OF
		  state.xaxis.id: BEGIN
			vxy = state.draw.view
			vxy(0) = event.x - state.draw.scroll(0)/2
			Widget_Control, state.xaxis.id, SET_DRAW_VIEW=[vxy(0),0]
			END
		  state.yaxis.id: BEGIN
			vxy = state.draw.view
			vxy(1) = event.y - state.draw.scroll(1)/2
			Widget_Control, state.yaxis.id, SET_DRAW_VIEW=[0,vxy(1)]
			END
		 ENDCASE

		Widget_Control, state.draw.id, SET_DRAW_VIEW=vxy
		state.draw.view = vxy
	   endif

	WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
	return,0

   endif else begin	;return only the central draw motion/button events.

	ret_ev = { Draw_Axes_EVENT,	ID: 	event.handler,	$
					TOP:	event.top,	$
					HANDLER:0L,		$
					TYPE:	event.type,	$
					X:	event.x,	$
					Y:	event.y,	$
					PRESS:	event.press,	$
					RELEASE:event.release,	$
					WINFO:	state.draw	}

	WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY
	return, ret_ev
    endelse
END

;-----------------------------------------------------------------------------

function CW_Draw_Axes, parent, FRAME=frame, RETAIN=retain, UVALUE=uval, $
				SIZE_XY=size_xy, SCROLL_SIZE_XY=scroll_xy, $
				XAX_LINES=xax_Lines, YAX_CHARS=yax_chars
  on_error,2
  if N_elements( frame ) EQ 0 THEN frame = 0
  if N_elements( retain ) EQ 0 THEN retain = 1
  if N_elements( uval ) EQ 0  THEN uval = 0
  if N_elements( size_xy ) NE 2 THEN size_xy = [ 2048, 2048 ]
  if N_elements( scroll_xy ) NE 2 THEN scroll_xy = [ 512, 512 ]
  if N_elements( xax_Lines ) EQ 0 THEN xax_Lines = 3
  if N_elements( yax_chars ) EQ 0 THEN yax_chars = 9
  width_xax = xax_Lines * !D.y_ch_size
  width_yax = yax_chars * !D.x_ch_size

  if N_elements( parent ) NE 1 then begin

	base = WIDGET_BASE( TIT="CW_Draw_Axes", /ROW, UVALUE=uval, $ 
				FUNC_GET_VALUE="Draw_Axes_Get_V", $
				PRO_SET_VALUE="Draw_Axes_Set_V", $
				EVENT_FUNC = "Draw_Axes_Event" )
   endif else begin

	base = WIDGET_BASE( parent, FRAME=frame, /ROW, UVALUE=uval, $ 
				FUNC_GET_VALUE="Draw_Axes_Get_V", $
				PRO_SET_VALUE="Draw_Axes_Set_V", $
				EVENT_FUNC = "Draw_Axes_Event" )
    endelse
  
  Lcol = WIDGET_BASE( base, /COLUMN )
  Rcol = WIDGET_BASE( base, /COLUMN )

  winfo = { WINFO, name:"", id:0L, window:-1, $
		size:[0,0], scroll:[0,0], view:[0,0] }
  draw = winfo
  xaxis = winfo
  yaxis = winfo

  yaxis.id = WIDGET_DRAW( Lcol, RETAIN=retain, $
			/BUTTON_EVENTS, $
			/VIEWPORT_EVENTS, $
			XSIZE = width_yax, X_SCROLL = width_yax-1, $
			YSIZE = size_xy(1), Y_SCROLL = scroll_xy(1) )
  yaxis.size = [ width_yax, size_xy(1) ]
  yaxis.scroll = [ width_yax-1, scroll_xy(1) ]

  draw.id = WIDGET_DRAW( Rcol, RETAIN=retain, $
			/BUTTON_EVENTS, $
			/MOTION_EVENTS, $
			/VIEWPORT_EVENTS, $
			XSIZE = size_xy(0), X_SCROLL = scroll_xy(0), $
			YSIZE = size_xy(1), Y_SCROLL = scroll_xy(1) )
  draw.size = size_xy
  draw.scroll = scroll_xy

  xaxis.id = WIDGET_DRAW( Rcol, RETAIN=retain, $
			/BUTTON_EVENTS, $
			/VIEWPORT_EVENTS, $
			XSIZE = size_xy(0), X_SCROLL = scroll_xy(0), $
			YSIZE = width_xax, Y_SCROLL = width_xax-1 )
  xaxis.size = [ size_xy(0), width_xax ]
  xaxis.scroll = [ scroll_xy(0), width_xax-1 ]

  state = {	draw:	draw,	$
		xaxis:	xaxis,	$
		yaxis:	yaxis,	$
		base:	base,	$
		retain:	retain	}

  tags = Tag_names( state )
  for i=0,2 do state.(i).name = tags(i)

  WIDGET_CONTROL, WIDGET_INFO( base, /CHILD ), SET_UVALUE=state, /NO_COPY
  RETURN, base
END
