;+
; NAME:
;	Handles_Pool
; PURPOSE:
;	Copy the handle values from and array of handles and lump them
;	all together into a single array. This array can then be saved
;	along with the handle-value sizes matrix and then later restored
;	and the handles recreated using function Handles_Unpool.
;	In the case of handle values with different variable types,
;	the variable type of the pool array is determined by
;	the first handle value, and all other handle values are
;	memory mapped to that same type (no conversion, data is unchanged).
;
; CALLING:
;	pool = Handles_Pool( handles, SIZES=vsizes )
;
; INPUTS:
;	handles = array of IDL handles (pointers to IDL variables).
;
; KEYWORDS:
;
;	SIZES = output, matrix of sizes of the variables stored in handles.
;		This matrix contains all the information needed to extract the
;		original variables from the pool using function Handles_Unpool.
;
;	NPVEC = output, vector of # elements from each handle actually pooled.
;		Note that for those handles storing strings & structures
;		npvec will be zero since those types cannot be pooled.
;
; OUTPUTS:
;	Function returns an array which is the "pool" containing all
;	elements stored in the handles lumped together.
;
; EXTERNAL CALLS:
;	function Handle_Sizes
;	function Map_Var_Type
;	function N_bytes_vtype
; PROCEDURE:
;	Get and set each handle value with /NO_COPY in loop of pool insertions.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1995.
;-

function Handles_Pool, handles, SIZES=vsizes, NPVEC=npvec, VERBOSE=verbose

	vsizes = Handle_Sizes( handles )
	sz = size( vsizes )
	Locs = reform( vsizes(0,*) ) + 1 + sz(1) * Lindgen( sz(2) )
	vtypes = vsizes(Locs)
	npvec = vsizes(Locs+1) * ( (vtypes GT 0) AND (vtypes LT 7) )
	wh = where( npvec GT 0, nwh )

	if (nwh LE 0) then begin
		if keyword_set( verbose) then message,"handles are empty!",/INFO
		return,0
	   endif

	vtp = vtypes(wh(0))
	npvec(wh) = ceil( float( npvec(wh) ) * N_bytes_vtype( vtypes(wh) ) / $
						N_bytes_vtype( vtp ) )
	Loc = 0L
	pool = make_array( total( npvec(wh) ), TYPE=vtp )

	for i=0,nwh-1 do begin
		j = wh(i)
		handle_value, handles(j), array,/NO_COPY
		pool(Loc) = Map_Var_Type( array, nout, TYPE_CODE_OUT=vtp )
		handle_value, handles(j), array,/NO_COPY,/SET
		Loc = Loc + nout
	  endfor

return, pool
end
