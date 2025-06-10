;+
; NAME:
;	TRP3D
; PURPOSE:
;		Transpose a 3D Array (permute order of dimensions)
; CATEGORY:
;		Math.
; CALLING:
;		Arrayout = Trp3D( Arrayin, Transpose_Type )
; INPUTS:
;	Arrayin = Array with up to 3 dimensions (3-D matrix).
;
;	Transpose_Type = integer vector of length 3
;			specifying permutation of dimensions
;	     examples:	[1,2,3] is NO tranpositions (identity),
;			[2,1,3] first two dimensions are transposed,
;       		[3,1,2] last dimension becomes first dimension.
; OUTPUTS:
;	Function returns transposed 3D array (dimensions permuted)
;	or usual transpose if input is 2D array.
; HISTORY:
;	Frank Varosi, written at U.of Md. 1988.
;	F.V. at NASA/GSFC 1990, adapted to IDL-v.2.
;	F.V.1992, simplified code using IDL intrinsic function reform( array ).
;	F.V.1995, mod to work on structures and 2D arrays (usual transpose).
;-

function Trp3D, Arrayin, Transpose_Type

	s = size( Arrayin )

	if (s(0) EQ 2) then begin

		if s(s(0)+1) LT 8 then  return,transpose( Arrayin )  else begin

			Arrayout = replicate( Arrayin(0), s(2), s(1) )
			for i=0,s(1)-1 do begin
				for j=0,s(2)-1 do Arrayout(j,i) = Arrayin(i,j)
			  endfor
			return,Arrayout
		 endelse

	  endif else if (s(0) EQ 1) then return,Arrayin

	Nel = s(s(0)+2)

	if N_elements( Transpose_Type ) LT 3 then begin
		print," "
		print,"usage:  Arrayout = Trp3d( Arrayin, Transpose_Type )
		print," "
		print," Transpose_Type = permutation of [1,2,3] specifying"
		print,"                  which dimensions get interchanged"
		print," "
		print,"examples: [1,2,3] is NO tranpositions (identity)"
		print,"          [2,1,3] first two dimensions are transposed"
		print,"          [3,1,2] last dimension becomes first dimension"
		if (Nel GT 0) then  return,Arrayin  else return,[1,2,3]
	   endif

	if (s(0) NE 3) then begin
		message,"first arg. should be 3D Array",/INFO
		if (Nel GT 0) then  return,Arrayin  else  return,s
	   endif

	tt = fix( Transpose_Type < 3 ) > 1

	if (tt(0) EQ tt(1)) OR (tt(0) EQ tt(2)) OR (tt(1) EQ tt(2)) then begin
		print,"* error:",Transpose_Type
		message,"second arg. must be permutation of [1,2,3]",/INFO
		if (Nel GT 0) then  return,Arrayin  else  return,tt
	   endif

	if s(4) EQ 8 then begin
		st = s(tt)
	 	Arrayout = replicate( Arrayin(0), st(0), st(1), st(2) )
	 endif else Arrayout = make_array( DIM=s(tt), TYPE=s(4) )

	IF ( tt(0) LT tt(1) ) THEN BEGIN             ;do not need 2D transpose.

		CASE tt(2) OF

		1: for i=0L,s(1)-1 do Arrayout(0,0,i) = reform( Arrayin(i,*,*) )

		2: for i=0L,s(2)-1 do Arrayout(0,0,i) = reform( Arrayin(*,i,*) )

		3:	Arrayout = Arrayin

		ENDCASE

	 ENDIF ELSE BEGIN	;need 2D Transpose function, so call Trp3D
				; recursively in case Arrayin is a structure.
		CASE tt(2) OF

		1: for i=0L,s(1)-1 do $
			Arrayout(0,0,i) = Trp3D( reform( Arrayin(i,*,*) ) )

		2: for i=0L,s(2)-1 do $	
			Arrayout(0,0,i) = Trp3D( reform( Arrayin(*,i,*) ) )

		3: for i=0L,s(3)-1 do $
			Arrayout(0,0,i) = Trp3D( Arrayin(*,*,i) )

		ENDCASE
	 ENDELSE

return, Arrayout
end
