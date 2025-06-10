;+
; NAME:
;	copy_struct_inx
; PURPOSE:
; 	Copy all fields with matching tag names (except for "except_Tags")
;	from one structure array to another structure array of different type.
;	This allows copying of tag values when equating the structures of
;	different types is not allowed, or when not all tags are to be copied.
;	Can also recursively copy from/to structures nested within structures.
;	This procedure is same as copy_struct with option to
;	specify indices (subscripts) of which array elements to copy from/to.
; CATEGORY:
;			Structures
; CALLING EXAMPLES:
;
;	copy_struct_inx, struct_From, struct_To, NT_copied, INDEX_FROM=subf
;
;	copy_struct_inx, struct_From, struct_To, INDEX_FROM=subf, INDEX_TO=subto
;
; INPUTS:
;	struct_From = structure array to copy from.
;	struct_To = structure array to copy values to.
;
; KEYWORDS:
;
;	INDEX_FROM = indices (subscripts) of which elements of array to copy.
;		(default is all elements of input structure array)
;
;	INDEX_TO = indices (subscripts) of which elements to copy to.
;		(default is all elements of output structure array)
;
;	EXCEPT_TAGS = string array of Tag names to ignore (to NOT copy).
;		Used at all levels of recursion.
;
;	SELECT_TAGS = Tag names to copy (takes priority over EXCEPT).
;		This keyword is not passed to recursive calls in order
;		to avoid the confusion of not copying tags in sub-structures.
;
;	/RECUR_FROM = search for sub-structures in struct_From, and then
;		call copy_struct recursively for those nested structures.
;
;	/RECUR_TO = search for sub-structures of struct_To, and then
;		call copy_struct recursively for those nested structures.
;
;	/RECUR_TANDEM = call copy_struct recursively for the sub-structures
;		with matching Tag names in struct_From and struct_To
;		(for use when Tag names match but sub-structure types differ).
;
; OUTPUTS:
;	struct_To = structure array to which new tag values are copied.
;	NT_copied = incremented by total # of tags copied (optional)
;
; INTERNAL:
;	Recur_Level = # of times copy_struct_inx calls itself.
;		This argument is for internal recursive execution only.
;		The user call is 1, subsequent recursive calls increment it,
;		and the counter is decremented before returning.
;		The counter is used just to find out if argument checking
;		should be performed, and to set NT_copied = 0 first call.
; EXTERNAL CALLS:
;	pro match	(when keyword SELECT_TAGS is specified)
; PROCEDURE:
;	Match Tag names and then use corresponding Tag numbers,
;	apply the sub-indices during = and recursion.
; HISTORY:
;	adapted from copy_struct: 1991 Frank Varosi STX @ NASA/GSFC
;	mod Aug.95 by F.V. to fix match of a single selected tag.
;	mod Mar.97 by F.V. do not pass the SELECT_TAGS keyword in recursion,
;		and check validity of INDEX_FROM and INDEX_TO in more detail.
;	F.V.1999, correctly handle copying tag arrays of different # elements.
;-

pro copy_struct_inx, struct_From, struct_To, NT_copied, Recur_Level,        $
						EXCEPT_TAGS  = except_Tags, $
						SELECT_TAGS  = select_Tags, $
						INDEX_From   = index_From,  $
						INDEX_To     = index_To,    $
						RECUR_From   = recur_From,  $
						RECUR_To     = recur_To,    $
						RECUR_TANDEM = recur_tandem

	if N_elements( Recur_Level ) NE 1 then Recur_Level = 0

	Ntag_from = N_tags( struct_From )
	Ntag_to = N_tags( struct_To )

	if (Recur_Level EQ 0) then begin	;check only at first user call.

		NT_copied = 0

		if (Ntag_from LE 0) OR (Ntag_to LE 0) then begin
			message,"two arguments must be structures",/INFO
			print," "
			print,"syntax:  copy_struct_inx, struct_From, struct_To"
			print," "
			print,"keywords:	INDEX_From= , INDEX_To="
			print,"		EXCEPT_TAGS= , SELECT_TAGS=,  "
			print,"		/RECUR_From,  /RECUR_To,  /RECUR_TANDEM"
			return
		   endif

		N_from = N_elements( struct_From )
		N_to = N_elements( struct_To )

		if N_elements( index_From ) LE 0 then index_From = $
								indgen( N_from )
		Ni_from = N_elements( index_From )
		if N_elements( index_To ) LE 0 then index_To = indgen( Ni_from )
		Ni_to = N_elements( index_To )

		if (Ni_from LT Ni_to) then begin

			message," # elements (" + strtrim( Ni_to, 2 ) + $
					") in output TO indices",/INFO
			message," decreased to (" + strtrim( Ni_from, 2 ) + $
					") as in FROM indices",/INFO
			index_To = index_To[0:Ni_from-1]

		  endif	else if (Ni_from GT Ni_to) then begin

			message," # elements (" + strtrim( Ni_from, 2 ) + $
					") of input FROM indices",/INFO
			message," decreased to (" + strtrim( Ni_to, 2 ) + $
					") as in TO indices",/INFO
			index_From = index_From[0:Ni_to-1]
		   endif

		Mi_to = max( [index_To] )
		Mi_from = max( [index_From] )

		if (Mi_to GE N_to) then begin

			message," # elements (" + strtrim( N_to, 2 ) + $
					") in output TO structure",/INFO
			message," increased to (" + strtrim( Mi_to, 2 ) + $
					") as max value of INDEX_To",/INFO
			struct_To = [ struct_To, replicate( struct_To[0], Mi_to-N_to ) ]
		  endif

 		if (Mi_from GE N_from) then begin

			w = where( index_From LT N_from, nw )

			if (nw GT 0) then begin
				index_From = index_From[w]
				message,"max value (" + strtrim( Mi_from, 2 ) +$
					") in FROM indices",/INFO
				message,"decreased to " + strtrim( N_from,2 ) +$
					") as in FROM structure",/INFO
			 endif else begin
				message,"all FROM indices are out of bounds",/IN
				return
			  endelse
		  endif
	   endif

	Recur_Level = Recur_Level + 1		;go for it...

	Tags_from = Tag_names( struct_From )
	Tags_to = Tag_names( struct_To )
	wto = indgen( Ntag_to )

;Determine which Tags are selected or excluded from copying:

	Nseltag = N_elements( select_Tags )
	Nextag = N_elements( except_Tags )

	if (Nseltag GT 0) then begin

		match, Tags_to, [strupcase( select_Tags )], mt, ms,COUNT=Ntag_to

		if (Ntag_to LE 0) then begin
			message," selected tags not found",/INFO
			return
		   endif

		Tags_to = Tags_to[mt]
		wto = wto[mt]

	  endif else if (Nextag GT 0) then begin

		except_Tags = [strupcase( except_Tags )]

		for t=0,Nextag-1 do begin
			w = where( Tags_to NE except_Tags[t], Ntag_to )
			Tags_to = Tags_to[w]
			wto = wto[w]
		  endfor
	   endif

;Now find the matching Tags and copy them...

	for t = 0, Ntag_to-1 do begin

		wf = where( Tags_from EQ Tags_to[t] , nf )

		if (nf GT 0) then begin

			from = wf[0]
			to = wto[t]

			if keyword_set( recur_tandem ) AND		$
			   ( N_tags( struct_To.(to) ) GT 0 ) AND	$
			   ( N_tags( struct_From.(from) ) GT 0 ) then begin

				struct_tmp = struct_To[index_To].(to)

				copy_struct, struct_From(index_From).(from),  $
						struct_tmp,                   $
						NT_copied, Recur_Level,       $
						EXCEPT=except_Tags,           $
						/RECUR_TANDEM,                $
						RECUR_FROM = recur_From,      $
						RECUR_To   = recur_To

				struct_To[index_To].(to) = struct_tmp

			  endif else begin


				sz_to = size(struct_To[0].(to))
				sz_from = size(struct_From[0].(from))

				if sz_to[0] eq sz_from[0] then begin

				   ndim = sz_to[0]

				   if ndim gt 0 then begin
					net = sz_to[1:ndim]
					nef = sz_from[1:ndim]
					ndim = max( abs( net - nef ) )
				    endif

				   if ndim eq 0 then begin

                                      struct_To[index_To].(to) = struct_From[index_From].(from)
                                      NT_copied = NT_copied + 1

                                   endif else begin ;take apart and rebuild:

					vto = struct_To[index_To].(to)
					vfrom = struct_From[index_From].(from)
					vto(*) = 0
					L = ( net < nef )-1
					L = [ L, replicate( 0, 7 ) ]
					vto[0,0,0,0,0,0,0]   =   vfrom(	0:L[0],$
									0:L[1],$
									0:L[2],$
									0:L[3],$
									0:L[4],$
									0:L[5],$
									0:L[6] )
					struct_To[index_To].(to) = vto
					NT_copied = NT_copied + 1

				     endelse

				 endif else begin
					message,"TO and FROM arrays not same "+$
						"dimension:" + string(7b),/INFO
					print, sz_to, sz_from
					print," tag  ",Tags_to[t],"  not copied"
				  endelse
			   endelse
		  endif
	  endfor

;Handle request for recursion on FROM structure:

	if keyword_set( recur_From ) then begin

		wfrom = indgen( Ntag_from )

		if (Nextag GT 0) then begin

			for t=0,Nextag-1 do begin
			    w = where( Tags_from NE except_Tags[t], Ntag_from )
			    Tags_from = Tags_from[w]
			    wfrom = wfrom[w]
			  endfor
		   endif

		for t = 0, Ntag_from-1 do begin

		     from = wfrom[t]

		     if N_tags( struct_From.(from) ) GT 0 then begin

			copy_struct_inx, struct_From.(from), struct_To,        $
						NT_copied, Recur_Level,    $
						EXCEPT=except_Tags,        $
						/RECUR_FROM,               $
						INDEX_From   = index_From, $
						INDEX_To     = index_To,   $
						RECUR_To     = recur_To,   $
						RECUR_TANDEM = recur_tandem
			endif
		  endfor
	  endif

;Handle request for recursion on TO structure:

	if keyword_set( recur_To ) then begin

		for t = 0, Ntag_to-1 do begin

		   to = wto[t]

		   if N_tags( struct_To.(to) ) GT 0 then begin

			struct_tmp = struct_To[index_To].(to)

			copy_struct_inx, struct_From, struct_tmp,          $
						NT_copied, Recur_Level,    $
						EXCEPT=except_Tags,        $
						/RECUR_To,                 $
						INDEX_From   = index_From, $
						RECUR_FROM = recur_From,   $
						RECUR_TANDEM = recur_tandem
			struct_To[index_To].(to) = struct_tmp
		     endif
		  endfor
	  endif

   Recur_Level = Recur_Level - 1
end
