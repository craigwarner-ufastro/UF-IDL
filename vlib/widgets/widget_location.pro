;+
; NAME:
;	widget_Location
; PURPOSE:
;	To set the Location of a top-level-base widget on the graphics device.
;	Calls WIDGET_CONTROL, allows flexibility to specify
;	either X or Y offsets alone, or both at once.
; CALLING:
;	widget_Location, wid, XPOS=xpos, YPOS=ypos
; INPUTS:
;	wid = Longword integer, the widget ID of the base or a child.
; KEYWORDS:
;	XPOS, YPOS = offset in device pixels from top-left corner of screen,
;			both x & y or either one alone can be specified.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1993.
;-

pro widget_Location, wid, XPOS=xpos, YPOS=ypos

  if N_elements( xpos ) EQ 1 then begin

     if N_elements( ypos ) EQ 1 then begin

        WIDGET_CONTROL, wid, TLB_SET_XOFF=xpos, TLB_SET_YOFF=ypos

     endif else WIDGET_CONTROL, wid, TLB_SET_XOFF=xpos, TLB_SET_YOFF=22

  endif else if N_elements( ypos ) EQ 1 then WIDGET_CONTROL, wid, TLB_SET_YOFF=ypos
end
