;+
; NAME:
;	Widget_Lab_Info
;
; PURPOSE:
;	Create 2 Label widgets in a row, one which is just at title
;	to the Left of a framed Label to which informational text can be set.
;
; CALLING:
;	wid = Widget_Lab_Info( base )
;
; INPUTS:
;	base = longword id of the parent base widget.
;
; KEYWORDS:
;	TITLE = string, label widget on left of row (default = nothing).
;	VALUE = string to be displayed in framed widget (default = space).
;	/DYNAMIC: if IDL version > 4.0 then set /DYNAMIC_RESIZE on second Label.
;
; OUTPUTS:
;	Function returns the id of the framed Label widget.
;
; EXAMPLE:
;	wid = Widget_Lab_Info( base, L="Status:", VALUE="OK" )
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1995.
;-

function Widget_Lab_Info, base, TITLE=title, VALUE=value, DYNAMIC_RESIZE=dynamic

	brow = Widget_Base( base, /ROW )
	if N_elements( title ) EQ 1 then wid = Widget_Label( brow, VALUE=title )
	if N_elements( value ) NE 1 then value = " "
	wid = Widget_Label( brow, /FRAME, VAL=value )

	if since_version( '4.0.1' ) and keyword_set( dynamic ) then $
		Widget_Control, wid, /DYNAMIC_RESIZE
return, wid
end
