;+
; NAME:
;	Widget_Lab_Text, base
;
; PURPOSE:
;	Create an editable text widget that has Labels to Left and
;	right of framed text, all in a row.
;
; CALLING:
;	wid_text = Widget_Lab_Text( base )
;
; INPUTS:
;	base = longword id of the parent base widget.
;
; KEYWORDS:
;	UVALUE = user value of text widget(s) must be specified.
;		If it is a matrix then an text widget
;		is created for each row of UVALUE matrix.
;
;	LEFT_LABEL = string, first label widget on left of row (default = null).
;	RIGHT_LABEL = string, last label on right of row (default = null).
;	FIELD_LABELS = optional, extra label to left of each text widget field.
;
;	VALUE = (default = null).
;	XSIZE =	integer, # of chars in text widget(s)
;		(default = strlen( value ) or 9 if value not spec.).
; OUTPUTS:
;	Function returns the id(s) of text widget(s).
; EXAMPLES:
;   Create single text field:
;
;	wid = Widget_Lab_Text( base, L="Time Range =", UVAL="TRAN", R="Hours", $
;				VALUE=string( Trange, F="(F9.1)" ) )
;
;   Create two text fields for min & max of an axis:
;
;	wids = Widget_Lab_Text( base, L="Y-axis: ", F=["MIN ="," MAX ="], $
;				VAL=string( yrange(*,0), F="(I6)" ), $
;				UVAL=[ ["YRANGE",'0'], ["YRANGE",'1'] ] )
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function Widget_Lab_Text, base, LEFT_LABEL=Lab_Left, RIGHT_LABEL=Lab_Right, $
				UVALUE=uvalue, VALUE=value, XSIZE=xsize,  $
				FIELD_LABELS=Lab_fields
	suv = size( uvalue )

	CASE suv[0] OF
		0:	Ntext = N_elements( uvalue )
		1:	Ntext = 1
		else:	Ntext = suv(2)
	 ENDCASE

	if (Ntext LE 0) then begin
		message,"must specify UVALUE=",/INFO
		return,0
	  endif else widt = Lonarr( Ntext )

	if N_elements( xsize ) LE 0 then begin
		if N_elements( value ) GT 0 then $
			xsize = strlen( value )  else  xsize = 9
	   endif

	if N_elements( value ) LE 0 then value = ""
	if N_elements( xsize ) LT Ntext then xsize = replicate( xsize[0],Ntext )
	if N_elements( value ) LT Ntext then value = replicate( value[0],Ntext )

	brow = Widget_Base( base, /ROW )
	wver = Widget_Info( /VERSION )
	if (wver.style EQ "Motif") then frame=0 else frame=1

	if N_elements( Lab_Left ) GT 0 then $
		wid = Widget_Label( brow, VALUE = Lab_Left )

	for i=0,Ntext-1 do begin

		if N_elements( Lab_fields ) GT i then $
			wid = Widget_Label( brow, VALUE = Lab_fields[i] )

		widt[i] = Widget_Text( brow, /EDIT, FRAME=frame, $
				UVAL=uvalue(*,i), VAL=value[i], XS=xsize[i] )
	  endfor

	if N_elements( Lab_Right ) GT 0 then $
		wid = Widget_Label( brow, VALUE = Lab_Right )

	if (Ntext EQ 1) then  return, widt[0]  else  return, widt
end
