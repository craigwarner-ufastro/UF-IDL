pro zoomxy, window_num
;+
; NAME:
;	zoomxy
; PURPOSE:
;	Interactive zoom of 2-D plotting region by cursor selection,
;	works for windowing systems and on old graphics terminals.
; CALLING EXAMPLES:
;	zoomxy
;	zoomxy, window_num
; INPUTS:
;	window_num = IDL window number, default is current window.
; RESULTS:
;	X-Y range for plot is set to user defined box.
; EXTERNAL CALLS:
;	function box_create
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1987.
;	F.V.1993 mod to use function box_create for Sunview and X-windows.
;-
if (!D.name EQ "TEK") OR (!D.name EQ "REGIS") then begin

     	cursor,xw1,yw1,/DATA
	oplot,[xw1,xw1],[yw1,yw1],PSYM=1
	
	cursor,xw2,yw2,/DATA
	xmin = xw1 < xw2
	xmax = xw2 > xw1
	ymin = yw1 < yw2
	ymax = yw2 > yw1

  endif else begin

	if N_elements( window_num ) EQ 1 then begin
		wset,window_num
		wshow,window_num
	   endif

	tvcrs, 0.5, 0.5 ,/NORM
	status = 4
	while (status EQ 4) do status = box_create( xmin,ymin, xmax,ymax,/DATA )
	wait,0.1
	box_erase
	if (status LE 0) then return
   endelse

oplot,  [xmin,xmax,xmax,xmin,xmin], $
	[ymin,ymin,ymax,ymax,ymin], PSYM=0, LINE=2	

set_xy ,xmin,xmax,ymin,ymax
end
