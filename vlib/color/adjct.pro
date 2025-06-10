;+
; NAME:
;	adjct
; PURPOSE:
;	My interface to replace standard adjct with adjctmap (my version)
; CALLING:
;	adjct
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1993.
;-

pro adjct, REDRAW=redraw, WINDISP=windisp

  common adjct, redrawMap

  if N_elements( redraw ) eq 1 then redrawMap = redraw
  if N_elements( redrawMap ) ne 1 then redrawMap = 1

	adjctmap, XPOS=200, YPOS=300, REDRAW=redrawMap, WINDISP=windisp
return
end
