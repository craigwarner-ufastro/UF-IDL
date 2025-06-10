;+
; NAME:
;	print_struct
;
; PURPOSE:
;	Print the tag values of an array of structures in nice column format,
;	with header line of tag names.
;
; CALLING:
;	print_struct, structure, Tags_to_print [ , title, string_matrix ]
;
; INPUTS:
;	structure = array of structured variables
;
;	Tags_to_print = string array specifying the names of tags to print.
;			Default is to print all tags which are not arrays.
; KEYWORDS:
;	FILE = string, optional file name to which output will then be written.
;	LUN_OUT = Logical unit number for output to an open file,
;		default is to print to standard output.
;	TNUMS = tag numbers to print (alternative to specifying tag names).
;	TRANGE = [beg,end] tag number range to print.
;	FRANGE = same as TRANGE.
;	WHICH = optional array of subscripts to select which elements to print.
;	FORM_FLOAT = string array of three elements specifying
;		floating point format, ex: FORM=['f','9','2'] means "(F9.2)",
;		(default float format is G12.4).
;	MAX_ELEMENTS = positive integer, print only tags that have less than
;			this number of elements (default is no screening).
;	/STRINGS : instead of printing, return the array of strings in
;		fourth argument of procedure: string_matrix.
;	/TWILD : use the `wildcard' approach to matching tag names,
;		so that all tag names whose first characters match
;		the characters in the requested tag are printed.
; OUTPUTS:
;	title = optional string, list of tags printed/processed.
;	string_matrix = optional output of string matrix of tag values,
;			instead of printing to terminal or file, if /STRINGS.
; EXTERNAL CALLS:
;	function N_struct
;	function unique
; PROCEDURE:
;	Check the types and lengths of fields to decide formats,
;	then loop and form text string from requested fields, then print.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1991.
;	F.V.1993, fixed up the print formats.
;	F.V.1994, added more keyword options (TNUMS, TRANGE, /STRINGS,...).
;	F.V.1997, added WHICH and MAX_ELEM keyword options.
;	F.V.1998, added /COLUMN keyword option (needs more work).
;	F.V.1999, added /TWILD keyword option and improved /COLUMN.
;	Mod 2008 by FV at UF: handle the unsigned data types.
;	Mod 2011 by FV at UF: skip pointers and obj. refs.
;-

pro print_struct, structure, Tags_to_print, title, string_matrix, TNUMS=tagi, $
			FRANGE=fran, TRANGE=tran, FIL=filout, LUN_OUT=Lun, $
	STRINGS=strings, FORM_FLOAT=formf, NO_TITLE=no_tit, TWILD=wild, MAXCHAR=maxChar,$
	WHICH_TO_PRINT=which, MAXELEMENTS=max_elements, COLUMN=column, DELIMITER=delim

	Nstruct = N_struct( structure, Ntag )

	if (Nstruct LE 0) then begin
		message,"expecting a structure",/INFO
		return
	   endif

	if (Nstruct EQ 1) then structure = [structure]

	tags = [tag_names( structure )]
	Ntpr = N_elements( Tags_to_print )
	if N_elements( tran ) EQ 2 then fran = tran
        if N_elements( delim ) ne 1 then delim = ""

	if N_elements( tagi ) GT 0 then begin

		tagi = ( tagi > 0 ) < (Ntag-1)
		tagi = tagi( unique( tagi,/SORT,/ORIGINAL ) )

	 endif else if N_elements( fran ) EQ 2 then begin

		fran = ( fran > 0 ) < (Ntag-1)
		nf = abs( fran[1] - fran[0] )+1
		tagi = indgen( nf ) + min( fran )

	  endif else if (Ntpr LE 0) then begin

		if keyword_set( max_elements ) then maxe = max_elements else begin
  			if keyword_set( column ) then maxe=1 else maxe=2
		endelse

		for i=0,Ntag-1 do begin

                   sze = size( structure[0].(i) )
                   
                   if ( sze[sze[0]+2] LE maxe ) AND $
                      ( sze[sze[0]+1] ne 8 ) AND $
                      ( sze[sze[0]+1] ne 10 ) then begin
                      if N_elements( tagi ) LE 0 then tagi = [i]  else tagi = [ tagi, i ]
                   endif
                endfor

	   endif else begin

		ptags = [strupcase( Tags_to_print )]

		for i=0,Ntpr-1 do begin

		    if keyword_set( wild ) then begin
			if( wild gt 1 ) then begin
			   nt = N_elements( tags )
			   p = intarr( nt )
			   for j=0,nt-1 do p[j] = strpos( tags[j], ptags[i] )
			   w = where( p ge 0, nf )
			 endif else begin
		           w = where( strmid( tags, 0, strlen( ptags[i] ) ) EQ ptags[i], nf )
			  endelse
		     endif else  w = where( tags EQ ptags[i], nf )

		    if (nf GT 0) then begin

                       if N_elements( tagi ) LE 0 then tagi = w  else tagi = [ tagi, w ]

		      endif else message,"Tag <"+ptags[i]+"> not found",/INFO
		  endfor

		if keyword_set( wild ) and $
                   (N_elements( tagi ) gt 0) then tagi = tagi( unique( tagi, Ntag,/SORT,/ORIG ) )
	    endelse

	if N_elements( tagi ) LE 0 then begin
		message,"requested Tags are not in structure",/INFO
		return
	   endif

	if keyword_set( max_elements ) then begin

		Ntag = N_elements( tagi )
		Ntel = Lonarr( Ntag )
		Ntst = intarr( Ntag )

		for i=0,Ntag-1 do begin
			Ntel[i] = N_elements( structure[0].(tagi[i]) )
			Ntst[i] = N_tags( structure[0].(tagi[i]) )
		  endfor

		w = where( (Ntel LE max_elements) and (Ntst LE 0), nw )

		if (nw GT 0) then  tagi = tagi[w]  else begin
			message,"requested Tags have too many elements",/INFO
			return
		   endelse
	   endif

	if (Nstruct GE 1000) then begin
		title = "   #"
		iform = "(I4)"
	 endif else begin
		title = "  #"
		iform = "(I3)"
	  endelse

	Tags_to_print = tags[tagi]
	Ntpr = N_elements( tagi )

	vtypes = intarr( Ntpr )
	sLens = intarr( Ntpr )
	formats = strarr( Ntpr )
	ncht = strlen( Tags_to_print ) + 2
	minch = [ 0, 5, 8, 12, 12, 14, 12, 0, 0, 20, 0, 0, 8, 12, 12, 12 ]

	if N_elements( formf ) EQ 3 then begin
		formf = strtrim( formf, 2 )
		ndig = fix( formf[1] )
		minch[4] = ndig
	   endif

	if keyword_set( column ) then minch[*] = minch[4]

	for i=0,Ntpr-1 do begin

		st = size( structure[0].(tagi[i]) )
		vtypes[i] = st[st[0]+1]

		CASE vtypes[i] OF
		1:	formats[i] = "I" + strtrim( ncht[i]>minch[1], 2 ) + ")"
		2:	formats[i] = "I" + strtrim( ncht[i]>minch[2], 2 ) + ")"
		3:	formats[i] = "I" + strtrim( ncht[i]>minch[3], 2 ) + ")"
		7: BEGIN
			sLens[i] = max( strlen( structure.(tagi[i]) ) ) + 2
			if NOT keyword_set( column ) then sLens[i] = sLens[i] > ncht[i]
                        if keyword_set( maxChar ) then sLens = sLens < maxChar
			formats[i] = "(X,A" + strtrim( sLens[i], 2 ) + "))"
		     END
		else: BEGIN
                   if( vtypes[i] ne 10 AND vtypes[i] ne 11 ) then begin
                      if N_elements( ndig ) EQ 1 then begin
                         formf = strtrim( formf, 2 )
                         ndig = fix( formf[1] )
                         minch[4] = ndig
                         if strpos(formf[0],"F") eq 0 then begin
                            ndec = fix( formf[2] )
                            if( ndec LE 1 ) then begin
                               minv = min( structure.(tagi[i]), MAX=maxv )
                               mpow = ceil( aLog10( minv ) )
                               if( mpow LE 0 ) then ndec = 1-mpow
                               if( maxv LT 100 ) then ndec = ndec > 1
                               if( maxv LT 10 ) then ndec = ndec > 2
                            endif
                            fmat = strtrim(ncht[i]>ndig,2) + "." + strtrim(ndec,2) + ")"
                         endif else fmat = strtrim(ncht[i]>ndig,2) + "." + formf(2) + ")"

                         formats[i] = formf[0] + fmat

                      endif else begin

                         if( vtypes[i] eq 5 or vtypes[i] eq 10 ) then begin
                            formats[i] = "G"+strtrim( ncht[i]>minch[5],2 )+".7)"
                         endif else formats[i] = "G"+strtrim( ncht[i]>minch[4],2 )+".4)"
                      endelse
                   endif
                   END
		ENDCASE

                ;; avoid pointers and object refs.:

                if( vtypes[i] eq 10 OR vtypes[i] eq 11 ) then tagi[i] = -1 else begin
                   nelem = st[st[0]+2]
                   formats[i] = "(" + strtrim( nelem, 2 ) + formats[i]
                   minch[7] = sLens[i]
                   nb = nelem * ( ncht[i] > minch[vtypes[i]] ) - ncht[i] + 2
                   if( vtypes[i] eq 7 ) then nb = nb + 1
                   title = title + delim + string( replicate( 32b, nb ) ) + Tags_to_print[i]
                endelse
	  endfor

        if min( tagi LT 0 ) then begin

           wok = where( tagi ge 0, Ntpr )

           if( Ntpr gt 0 ) then begin
              tagi = tagi[wok]
              vtypes = vtypes[wok]
              formats = formats[wok]
              Tags_to_print = Tags_to_print[wok]
           endif else begin
              message,/INFO,"why are all Tags pointers or object refs.???"+string(7b)
              return
           endelse
        endif

	if N_elements( which ) GT 0 then begin
		w = where( (which GE 0) AND (which LT Nstruct), nw )
		if (nw LE 0) then begin
			message,"keyword WHICH subscripts out of range",/INFO
			return
		   endif
		which = which[w]
		Nprint = nw
	 endif else begin
		which = indgen( Nstruct )
		Nprint = Nstruct
	  endelse

;; Now do the actual printing:

  if keyword_set( column ) then begin

     if keyword_set( filout ) then openw, Lun, filout,/GET_LUN
     maxch = max( strlen( Tags_to_print ) )
     blanks = string( replicate( 32b, maxch ) )
     Tags = strmid( Tags_to_print + blanks, 0, maxch+1 )

     for i=0,Ntpr-1 do begin
        text = Tags[i]
        for n=0,Nprint-1 do text = text + string( structure[which[n]].(tagi[i]), FORM=formats[i] )
        if N_elements( Lun ) EQ 1 then printf,Lun,text else print,text
     endfor

  endif else begin

     pr_tit = keyword_set( no_tit ) EQ 0

     if keyword_set( strings ) then begin
        string_matrix = strarr( Ntpr, Nprint )
        title = strmid( title, 3, 999 )
     endif else begin
        if keyword_set( filout ) then openw, Lun, filout,/GET_LUN
        if (pr_tit) then begin
           if (Nstruct LE 3) then title = strmid( title, 3, 999 )
           if N_elements( Lun ) EQ 1 then printf,Lun,title else print,title
        endif
     endelse

     for n=0,Nprint-1 do begin

        wp = which[n]

        if keyword_set( strings ) then begin

           for i=0,Ntpr-1 do string_matrix[i,n] = string( structure[wp].(tagi[i]), FORM=formats[i] )

        endif else begin

           if (pr_tit) AND (Nstruct GT 3) then text = string( wp,FORM=iform )  else text=""

           for i=0,Ntpr-1 do text = text + delim + string( structure[wp].(tagi[i]), FORM=formats[i] )

           if N_elements( Lun ) EQ 1 then printf,Lun,text else print,text
        endelse
     endfor
  endelse

  if keyword_set( filout ) and (N_elements( Lun ) EQ 1) then begin
     free_Lun, Lun
     message,"structure printed into file: " + filout,/INFO
  endif
end
