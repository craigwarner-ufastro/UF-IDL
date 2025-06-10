;+
; NAME:
;	VecNorm
; PURPOSE:
;	Return norms of an array of vectors, real or complex.
; CALLING:
;	vnorms = VecNorm( vectors )
; INPUTS:
;	vectors = 2-D array : ( Nvec , Vdim )
;		(1st index is vector #, 2nd index is component # e.g. x,y,z)
; OUTPUTS:
;	Function returns 1-D array of Euclidean norms.
;	(If only one vector, then returns its norm, a single number).
; HISTORY:
;	F.V. 2022, added shorter simpler function name option.
;-

function VecNorm, vectors
  return, Vec_Norm( vectors )
end
