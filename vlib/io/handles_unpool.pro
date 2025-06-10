;+
; NAME:
;	Handles_Unpool
; PURPOSE:
;	Extract all the handle-values that were lumped in a pool array
;	by function Handle_Pool and then create a new array of handles
;	storing the original values (variable types are also original).
;	Besides the pool array, caller must also provide the value sizes matrix.
;
; CALLING:
;	handles = Handles_Unpool( pool, SIZES=vsizes )
;
; INPUTS:
;	pool = an array which is the "pool" containing all elements
;		that were stored in the handles lumped together.
;		Obtained as result of function Handles_Pool.
;
; KEYWORDS:
;
;	SIZES = required input, matrix of variable sizes to be put in handles.
;		This matrix is created when using Handles_Pool and contains all
;		the information needed to extract the variables from the pool.
;		Matrix can also be obtained using function Handle_Sizes.

;	NPVEC = optional output, vector of # elements in each returned handle
;		that is extracted from pool.
;
; OUTPUTS:
;	Function returns an array of IDL handles (pointers to IDL variables).
;
; EXTERNAL CALLS:
;	function Map_Var_Type
;	function N_bytes_vtype
; PROCEDURE:
;	Get and set each handle value with /NO_COPY in loop of pool insertions.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1995.
;-

function Handles_Unpool, pool, SIZES=vsizes, NPVEC=npvec, VERBOSE=verbose

	sz = size( vsizes )

	if (sz(0) LE 0) OR N_elements( pool ) LE 0 then begin
		print,"syntax:	handles = handles_unpool( pool, SIZES=vsizes )"
		return,0
	   endif

	Locs = reform( vsizes(0,*) ) + 1 + sz(1) * Lindgen( sz(2) )
	vtypes = vsizes(Locs)
	npvec = vsizes(Locs+1) * ( (vtypes GT 0) AND (vtypes LT 7) )
	handles = Lonarr( N_elements( vtypes ) )
	wh = where( npvec GT 0, nwh )

	if (nwh LE 0) then begin
		if keyword_set( verbose) then message,"sizes are zero!",/INFO
		return,handles
	   endif

	sz = size( pool )
	npvec(wh) = ceil( float( npvec(wh) ) * N_bytes_vtype( vtypes(wh) ) / $
						N_bytes_vtype( sz(sz(0)+1) ) )
	Loc = 0L

	for i=0,nwh-1 do begin
		j = wh(i)
		L = Loc + npvec(j) - 1
		handles(j) = $
		  handle_create( VAL=Map_Var_Type( pool(Loc:L), TY=vtypes(j) ) )
		Loc = L + 1
	  endfor

return, handles
end
