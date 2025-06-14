;+
; NAME:
;	compare_struct
; PURPOSE:
; 	Compare all matching Tag names (except for "except_Tags")
;	between two structure arrays (may be different structure defs.),
;	and return a structured List of tags found different.
; CATEGORY:
;		Structures
; CALLING:
;	diff_List = compare_struct( struct_A, struct_B )
; INPUTS:
;	struct_A, struct_B : the two structure arrays to compare.
;	Struct_Name : for internal recursion use only.
; KEYWORDS:
;	/PERCENT : also compute min, avg, max percent differences.
;	EXCEPT = string array of Tag names to ignore (NOT to compare).
;	/BRIEF : number of differences found for each matching tag
;		of two structures is printed while executing.
;	/FULL : option to print even if zero differences found.
;	/RECUR_A : option to search for Tag names in sub-structures of struct_A,
;		then call compare_struct recursively for those sub-structures.
;	/RECUR_B : search for sub-structures of struct_B, and then
;		call compare_struct recursively for those nested sub-structures.
;	Note:
;		compare_struct is automatically called recursively
;		for those sub-structures that are in both struct_A and struct_B
;		and have matching names (otherwise cannot take difference).
; OUTPUT:
;	Returns a structure array describing differences found,
;	which can be examined by the following examples:
;		help,/st, diff_List.
;		print, diff_List
;		print_struct, diff_List
;		print_struct, compare_struct( struct_A, struct_B )
;		print_struct, compare_struct( struct_A, struct_B,/PERCEN )
; PROCEDURE:
;	Match Tag names and then use where function on tags.
; HISTORY:
;	written: 1990 Frank Varosi STX @ NASA/GSFC (based on copy_struct)
;	modif Aug.90 by F.V. to check and compare same # of elements only.
;	mod 2020 by F.V. @UFastro: added keyword option /PERCENT
;-

function compare_struct, struct_A, struct_B, Struct_Name, EXCEPT=except_Tags, FULL=full,$
                         BRIEF=brief, RECUR_A=recur_A, RECUR_B=recur_B, PERCENT=percdiff

   common compare_struct1, diffLv1
   common compare_struct2, diffLv2

	if N_struct( diffLv1 ) NE 1 or N_struct( diffLv2 ) NE 1 then begin

           diffLv1 = { DIFF_LIST, Tag_Num_A:0, Tag_Num_B:0, Tag_Name:"", Ndiff:0L }
           diffLv2 = { DIFF_LIST2, Tag_Num_A:0, Tag_Num_B:0, Tag_Name:"",Ndiff:0L,$
                       dmin:0.0, dmax:0.0, davg:0.0, vavg:0.0, $
                       percentmin:0.0, percentavg:0.0, percentmax:0.0 }
        endif

        if keyword_set( percdiff ) then begin
           diff_List = replicate( {DIFF_LIST2}, 1 )
        endif else diff_List = replicate( {DIFF_LIST}, 1 )

	Ntag_A = N_tags( struct_A )
	if (Ntag_A LE 0) then begin
		message," 1st argument must be a structure variable",/INFO
		return,diff_List 
	   endif
	Ntag_B = N_tags( struct_B )
	if (Ntag_B LE 0) then begin
		message," 2nd argument must be a structure variable",/INFO
		return,diff_List 
	   endif

	N_A = N_elements( struct_A )
	N_B = N_elements( struct_B )

	if (N_A LT N_B) then begin

		message,"comparing "+strtrim(N_A,2)+" of first structure",/INFO
		message,"to first "+strtrim(N_A,2)+" of "+strtrim(N_B,2)+ $
			" in second structure",/INFO

                diff_List = compare_struct( struct_A, struct_B(0:N_A-1), $
                                            EXCEPT=except_Tags, PERCENT=percdiff,$
                                            RECUR_A = recur_A, $
                                            RECUR_B = recur_B, FULL=full, BRIEF=brief )
 		return,diff_List 

	  endif else if (N_A GT N_B) then begin

		message,"comparing first "+strtrim(N_B,2)+" of "+ $
			strtrim(N_A,2)+" in first structure",/INFO
		message,"to "+strtrim(N_B,2)+" in second structure",/INFO

		diff_List = compare_struct( struct_A(0:N_B-1), struct_B, $
                                            EXCEPT=except_Tags, PERCENT=percdiff,$
                                            RECUR_A = recur_A, $
                                            RECUR_B = recur_B, FULL=full, BRIEF=brief )
 		return,diff_List 
	   endif

	Tags_A = tag_names( struct_A )
	Tags_B = tag_names( struct_B )
	wB = indgen( N_elements( Tags_B ) )
	Nextag = N_elements( except_Tags )

	if (Nextag GT 0) then begin

		except_Tags = [strupcase( except_Tags )]

		for t=0,Nextag-1 do begin
			w = where( Tags_B NE except_Tags(t), Ntag_B )
			Tags_B = Tags_B(w)
			wB = wB(w)
		  endfor
	   endif

	if N_elements( struct_name ) NE 1 then sname = "." $
					  else sname = struct_name + "." 

	for t = 0, Ntag_B-1 do begin

		wA = where( Tags_A EQ Tags_B(t) , nf )

		if (nf GT 0) then begin

		     tA = wA(0)
		     tB = wB(t)

		     NtA = N_tags( struct_A.(tA) )
		     NtB = N_tags( struct_B.(tB) )

		     if (NtA GT 0 ) AND (NtB GT 0) then begin

                        if keyword_set( full ) OR $
                           keyword_set( brief ) then print, sname + Tags_A(tA), " :"

                        diffs = compare_struct( struct_A.(tA), struct_B.(tB), $
                                                sname + Tags_A(tA), PERCENT=percdiff,$
						EXCEPT=except_Tags, FULL=full, BRIEF=brief)
                        diff_List = [ diff_List, diffs ]

                     endif else if (NtA LE 0) AND (NtB LE 0) then begin

                        w = where( struct_B.(tB) NE struct_A.(tA) , Ndiff )

                        if (Ndiff GT 0) then begin
                           if keyword_set( percdiff ) then begin
                              diff = replicate( {DIFF_LIST2}, 1 )
                              vta = vartype( struct_A.(tA),/CODE)
                              vtb = vartype( struct_B.(tB),/CODE)
                              if (vta ne 7) and (vta ne 8) and $
                                 (vtb ne 7) and (vtb ne 8) then begin
                                 savbig = avg( struct_B.(tB) ) > avg( struct_A.(tA) )
                                 sdiff = struct_B.(tB) - struct_A.(tA)
                                 sdmax = max( sdiff, MIN=sdmin )
                                 diff.dmin = sdmin
                                 diff.dmax = sdmax
                                 diff.davg = avg( sdiff )
                                 diff.vavg = savbig
                                 diff.percentmax = 100 * sdmax / savbig
                                 diff.percentmin = 100 * sdmin / savbig
                                 diff.percentavg = 100 * diff.davg/ savbig
                              endif
                           endif else diff = replicate( {DIFF_LIST}, 1 )
                           diff.Tag_Num_A = tA
                           diff.Tag_Num_B = tB
                           diff.Tag_Name = sname + Tags_A(tA) 
                           diff.Ndiff = Ndiff
                           diff_List = [ diff_List, diff ]
                        endif

                        if (keyword_set( brief ) AND (Ndiff GT 0)) OR $
                           keyword_set( full ) then print, Tags_A(tA), Ndiff,FORM="(15X,A15,I9)"

                     endif else print, Tags_A(ta), " not compared"
		endif
	  endfor

	if keyword_set( recur_A ) then begin

		for tA = 0, Ntag_A-1 do begin

		   if N_tags( struct_A.(tA) ) GT 0 then begin

			diffs = compare_struct( struct_A.(tA), struct_B, $
						sname + Tags_A(tA), PERCENT=percdiff,$
						EXCEPT=except_Tags, $
						RECUR_A = recur_A, $
						RECUR_B = recur_B, FULL=full, BRIEF=brief )
			diff_List = [ diff_List, diffs ]
		     endif
		  endfor
	  endif

	if keyword_set( recur_B ) then begin

		for tB = 0, Ntag_B-1 do begin

		   if N_tags( struct_B.(tB) ) GT 0 then begin

			diffs = compare_struct( struct_A, struct_B.(tB), $
						sname + Tags_B(tB), $
						EXCEPT=except_Tags, PERCENT=percdiff,$
						RECUR_A = recur_A, $
						RECUR_B = recur_B, FULL=full, BRIEF=brief )
			diff_List = [ diff_List, diffs ]
		     endif
		  endfor
	  endif

	w = where( [diff_List.Ndiff] GT 0, np )
	if (np LE 0) then w = [0]

return, diff_List[w]
end
