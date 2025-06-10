;+
; NAME:
;	vecLinterp
; PURPOSE:
;	Compute a sequence of Linear interpolations between any two arrays.
;	Arrays can be vectors, images, 3-D matrices, etc., but of same size.
;
; CALLING:
;	Zinterp = vecLinterp( Z1, Z2, Xp, X1, X2, NPOINT=Np )
;
; INPUTS:
;	Z1, Z2 = vectors, images, etc. (dependent variable).
;	X1, X2 = scalars (independent variable)
;	Xp = vector of interpolation points: X1 <= Xp <= X2
;
; KEYWORDS:
;	NPOINT = specifies # of equally spaced X values to interpolate,
;		overrides Xp.
; OUTPUTS:
;	If Npoints>1 then function result is an array of dimension one more than
;	input arrays (giving a sequence of arrays), else if Npoints=1 result is
;	same size as input arrays.
; PROCEDURE:
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1984.
;	F.V.1991 added keyword NPOINT.
;	F.V.2016 at UF: use brackets for arrays, and cleaned up.
;-

function vecLinterp, Z1, Z2, Xp, X1, X2, NPOINT=Np

	if N_elements( Np ) EQ 1 then factor = ( findgen( Np ) + 1 )/(Np+1)  else begin
           Np = N_elements( Xp )
           factor = [(Xp-X1)/(X2-X1)]
        endelse

	sz1 = size( Z1 )
	sz2 = size( Z2 )

	if (sz2[0] NE sz1[0]) then begin
		message,"input arrays are not same size",/INFO
		print,sz1,sz2
		return,0
	   endif

	if (sz2[sz2[0]+2] NE sz1[sz1[0]+2]) then begin
		message,"input arrays are not same size",/INFO
		print,sz1,sz2
		return,0
	   endif

	if (sz1[0] GT 1) then begin

           Nelem = sz1[sz1[0]+2]

           if (Np GT 1) then Dims = [ sz1[1:sz1[0]], Np ] else Dims = sz1[1:sz1[0]]

           return, reform( factor # reform( Z2-Z1, Nelem ) + replicate(1,Np) # reform( Z1, Nelem ), Dims )

        endif else return, factor # (Z2-Z1)  +  replicate(1,Np) # Z1
end
