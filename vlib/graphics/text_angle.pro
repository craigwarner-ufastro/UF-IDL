;+
; NAME:
;
; PURPOSE:
;	Returns the angle in degrees for drawing text on a graph,
;	between two points given by input arrays (xt,yt).
; CALLING:
;	angle = text_angle( xt, yt )
; INPUTS:
;	xt, yt = 2 elements arrays for x and y coordinates resp.
; PROCEDURE:
;	Convert to device coordinates and use inverse tangent function.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1996.
;-

function Text_Angle, xt, yt

	devxy = transpose( convert_coord( xt, yt, /TO_DEVICE ) )

return, !RADEG * atan( total( [-1,1]*devxy(*,1) ), total( [-1,1]*devxy(*,0) ) )
end
