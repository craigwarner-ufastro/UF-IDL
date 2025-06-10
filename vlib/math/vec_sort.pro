function Vec_Sort, matrix, ordering, DECREASING=decr

; Return a matrix of subscripts which Sorts on Last index (columns) of matrix.
; For 2-D matrix, columns are matrix(i,*), for 3-D (image stack): matrix(i,j,*).
; Sort is increasing order, unless /DECREASING to get decreasing order.
; Usual statement: IDL> matrix( vec_sort( matrix ) ) will perform the sort.
; <ordering> is returned (optional) order indices for each column seperately.
; Calls external function trp3d() when sorting stack and # images > 9.
; Frank Varosi U.Md.1988
; F.V. NASA/GSFC 1991, adapted to IDL-v.2, and for stack of images (3-D matrix).

	sm = size( matrix )
	ndim = sm(0)
	nrow = sm(ndim)

	if (ndim LT 2) OR (ndim GT 3) then begin
		message,"expecting 2-D or 3-D matrix for 1st arg.",/INFO
		return, sm
	   endif

if (nrow LT 8) then begin	     ;faster to bubble-sort with gather/scatter

	oindex = make_array( DIM=sm(1:ndim),/INDEX,/LONG )

	for i = 0, nrow-2 do begin

		if (ndim EQ 3) then oi=oindex(*,*,i) else oi=oindex(*,i)

		for j = i+1, nrow-1 do begin

			if (ndim EQ 3) then oj=oindex(*,*,j) else oj=oindex(*,j)

			if keyword_set( decr) then $
				w = where( matrix(oi) LT matrix(oj), nw ) $
			  else 	w = where( matrix(oi) GT matrix(oj), nw )

			if (nw GT 0) then begin		; if out of order,
							;   exchange pointers
				ot = oi
				oi(w) = oj(w)
				oj(w) = ot(w)
				if (ndim EQ 3)  then oindex(0,0,j) = oj $
						else oindex(0,j) = oj
			   endif
		   endfor

		if (ndim EQ 3) then oindex(0,0,i)=oi else oindex(0,i)=oi
	  endfor

	if N_params() GE 2 then begin
		if (ndim EQ 3) then ncol=sm(1)*sm(2) else ncol=sm(1)
		ordering = oindex / ncol
	   endif

  endif else begin			;in this case faster to call IDL sort.

	if (ndim EQ 3) then begin

		oindex = Lonarr( sm(3), sm(1), sm(2) )

		for j=0L,sm(2)-1 do begin
		    for i=0L,sm(1)-1 do oindex(0,i,j) = sort( matrix(i,j,*) )
		  endfor

		oindex = trp3d( oindex, [2,3,1] )

		if keyword_set( decr ) then begin
			ordering = oindex
			nk = sm(3)-1
			for k=0L,nk do oindex(0,0,nk-k) = ordering(*,*,k)
		   endif
		if N_params() GE 2 then ordering = oindex

		oindex = ( sm(1) * sm(2) ) * oindex
		index = Lindgen( sm(1), sm(2) )
		for k=0L,sm(3)-1 do  oindex(0,0,k) = oindex(*,*,k) + index

	  endif else begin

		oindex = Lonarr( sm(2), sm(1) )
		for i=0L,sm(1)-1 do  oindex(0,i) = sort( matrix(i,*) )

		oindex = transpose( oindex )
		if keyword_set( decr ) then oindex = reverse( oindex ,2 )
		if N_params() GE 2 then ordering = oindex

		oindex = oindex * sm(1)
		index = Lindgen( sm(1) )
		for k=0L,sm(2)-1 do  oindex(0,k) = oindex(*,k) + index
	    endelse
   endelse

return, oindex
end
