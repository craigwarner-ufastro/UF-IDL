;+
; NAME:
;	Vec_Norm
; PURPOSE:
;	Return norms of an array of vectors, real or complex.
; CALLING:
;	vnorms = Vec_Norm( vectors )
; INPUTS:
;	vectors = 2-D array : ( Nvec , Vdim )
;		(1st index is vector #, 2nd index is component # e.g. x,y,z)
; OUTPUTS:
;	Function returns 1-D array of Euclidean norms.
;	(If only one vector, then returns its norm, a single number).
; HISTORY:
;	Written: Frank Varosi, U.MD.1988.
;	F.V. 1994, mod to use new feature of total function.
;	F.V. 2022, total function handles case of just one vector.
;-

function Vec_Norm, vectors

	s = size( vectors )

	if( s[0] LT 1 ) then begin
           message,"input should be a vector or an array of vectors",/INFO
           return,0
        endif

        if( s[s[0]+1] EQ 6) then begin
           return, sqrt( total( float( vectors * conj(vectors) ), s[0] ) )
        endif else return, sqrt( total( vectors * vectors, s[0] ) )
end
