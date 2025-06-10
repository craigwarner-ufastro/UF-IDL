function DDL_struct, DDL_Lun, Lun_out, DDL_NAME = DDL_name, ABBREVIATE=abbrev,$
							VARIANT_NUMBER=variant,$
						     TRUNCATE_FILENAME=truncate
;+
; NAME:
;	DDL_struct
; PURPOSE:
;	Convert a DDL record-structure definition file
;			into IDL structure(s) code, packaged as a function.
; CATEGORY:
;			Structures
; CALLING SEQUENCE:
;		status = DDL_struct( DDL="DDL_name", /ABBREV,/TRUNC,VARIANT=# )
;	examples:
;			print, ddl_struct( ddl="FDQ_SDF", /ABBREV, /TRUNC )
;			print, ddl_struct( ddl="BET_LT_ET", VARIANT=2 )
; INPUTS/OUTPUTS:
;   keywords:
;		DDL_NAME = string giving DDL file name (type .DDL is assumed )
;   optional:
;		/ABBREV performs Field name abbreviations as defined 
;							by DDL_abbrev.pro
;		VARIANT = variant number to choose for structure definition,
;							(default variant = 1)
;		/TRUNC causes the output file name (not including .PRO)
;				to be truncated to 15 characters,
;			so IDL will compile it automatically when invoked.
;
;   inputs used recursively:
;		DDL_Lun = Logical unit number for reading DDL file.
;		Lun_out = Logical unit number for writing .PRO code.
;		ABBREV = keyword to indicate use of abbreviations.
;		VARIANT = variant number to choose for structure definition.
;
;   function returns a string which is not important to user
;		(it is just the final return from chain of recursion).
; SIDE EFFECTS:
;		"DDL_name".DDL file is read,
;		"DDL_name"_struct.PRO is created (IDL function code).
; PROCEDURE:
;  The DDL file (given by "DDL_name".DDL) is read one line at a time,
;
;  if declaration STRUCTURE is encountered,
;		then DDL_struct is called recursively,
;    		obtaining the complete IDL structure definition,
;		which is then printed to file "DDL_name"_struct.pro ,
;
;  otherwise the field type and size is used to construct IDL definition,
;	(calling DDL_abbrev,Tag_Name for name abbreviations if /ABBREV),
;	which is then concatenated with the other IDL definitions,
;   	and returned when declaration END STRUCTURE is encountered in DDL file.
;
; EXTERNAL CALLS:
;		pro DDL_ABBREV		(should be in same directory)
;		function GET_WORDS	(from [varosi.idl.lib]VAROSI.TLB) 
; MODIFICATION HISTORY:
;	written June,1990 Frank Varosi STX @ NASA/GSFC
;	modif. Aug.1990 by F.V. to process field variants in DDL.
;	modif. Sep.1990 by F.V. to handle multi-dimensional array declarations,
;				and option to truncate filename to 15 chars.
;-
	if N_elements( variant ) NE 1 then variant = 1
	struct_Tags = ""
	DDL_rec = ""
	DDL_rec2 = ""

;Setup is executed just the first call, skipped for all recursive calls:

	if ( N_elements( DDL_name ) EQ 1 ) AND $
	   ( N_elements( DDL_Lun ) NE 1 ) then begin

		DDL_name = strupcase( DDL_name )
		DDL_file = DDL_name + ".DDL"

		on_ioerror, IOERROR
		openr, DDL_Lun, DDL_file, /get_Lun, /SHARE

		if N_elements( DDL_Lun ) NE 1 then begin
	IOERROR:	print,!ERR_STRING
			print,!SYSERR_STRING
			message,"aborting",/CONTIN,/INFORM
			on_ioerror, NULL
			retall
		  endif else begin
			on_ioerror, NULL
			message, "reading "+DDL_file, /CONTIN
		   endelse

		while ( strpos( DDL_rec, "DEFINE RECORD" ) LT 0 ) do begin
			readf, DDL_Lun, DDL_rec
			DDL_rec = strupcase( DDL_rec )
		   endwhile

		DDL_words = get_words( DDL_rec )
		Rec_Name = DDL_words(2)

		pos = strpos( Rec_Name, "." )
		if (pos GE 0) then Rec_Name = strmid( Rec_Name, 0, pos )

		func_struct = Rec_Name + "_struct"

		func_code =  [  "function " + func_struct + ", Nstruct"      ,$
				" "					     ,$
				"  common "  + func_struct + ", defined"     ,$
				" "					     ,$
				"  if N_elements( defined ) NE 1 then begin" ,$
				" " ]


		func_end =  [   "       defined = 1 "			     ,$
				"    endif"				     ,$
				" "					     ,$
			"  if N_elements( Nstruct ) NE 1 then Nstruct = 1"   ,$
			" "						     ,$
			"return, replicate( {" + Rec_Name + "}, Nstruct )"   ,$
			"end" ]

		Nline = N_elements( func_code )

		file_struct = func_struct
		if keyword_set( truncate ) then $
				file_struct = strmid( file_struct, 0, 15 )
		file_struct = file_struct + ".PRO"
		openw, Lun_out, file_struct, /get_Lun
		message, "creating file " + file_struct, /CONTIN

		for i=0,Nline-1 do printf, Lun_out, func_code(i)
	   endif

;End of Setup.
;-----------------------------------------------------------------------------
;Now process the DDL file and write out IDL structure code:

;Read DDL file until a period is encountered:

	readf, DDL_Lun, DDL_rec

	while (strpos( DDL_rec, "." ) LT 0) do begin
		readf, DDL_Lun, DDL_rec2
		DDL_rec = DDL_rec + DDL_rec2
	  endwhile

	DDL_rec = strupcase( DDL_rec )

;Process structures or variants or field definitions:

   while ( strpos( DDL_rec, "END STRUCTURE" ) LT 0 ) AND $
         ( strpos( DDL_rec, "END VARIANT" ) LT 0 ) do begin

	if ( strpos( DDL_rec, "STRUCTURE" ) GT 0 ) then begin

		DDL_rec = strmid( DDL_rec, 0, strlen(DDL_rec)-1 )
		DDL_words = get_words( DDL_rec )
		struct_name = DDL_words(0)
		struct_namLc = strlowcase( struct_name )

		structure = struct_namLc + " = { " + struct_name

		struct_def = DDL_struct( DDL_Lun, Lun_out, $
						ABBREV=abbrev, VARIANT=variant )

		blanks = replicate( 32B, 80 )
		structure = string( blanks(0:6) ) + structure

		bpad = strpos( structure, "{" ) + 1
		struct_def = string( blanks(0:bpad) ) + struct_def

		structure = [ structure, struct_def ]

		bpad = strlen( structure )
		bpad = (max( bpad ) - bpad) < 70
		Nstr = N_elements( structure )

		for i=0,Nstr-2 do structure(i) = structure(i) + $
					string( blanks(0:bpad(i)) ) + ", $"

		structure(Nstr-1) = structure(Nstr-1) + $
					string( blanks(0:bpad(i)+4) ) + "}"

		for i=0,Nstr-1 do printf, Lun_out, structure(i)
		printf, Lun_out, " "
		print, struct_name + ": ", Nstr-1, " fields in structure", $
			FORM="(A30,I6,(A))"

		if (strpos( DDL_rec, "ARRAY" ) GT 0) then begin

			wa = where( DDL_words EQ "ARRAY" )
			dims = DDL_words( wa(0)+1 : * )
			dim = dims(0)
			Ndim = N_elements( dims )

			if (Ndim GT 1) then begin
				for i=1,Ndim-1 do dim = dim + ", " + dims(i)
			   endif

			struct_Tags = [ struct_Tags, struct_name + $
			  ": replicate( " + struct_namLc + ", " + dim + " )" ]

		  endif else  struct_Tags = $
			      [ struct_Tags, struct_name + ":" + struct_namLc ]

	 endif else if ( strpos( DDL_rec, "VARIANTS") GT 0 ) then begin

		message,"encountered VARIANTS, using VARIANT # " $
						+ strtrim( variant, 2 ),/CONT
		variant_count = 0

		while (variant_count LT variant) do begin

			readf, DDL_Lun, DDL_rec
			DDL_rec = strupcase( DDL_rec )
			DDL_words = get_words( DDL_rec )

			CASE DDL_words(0) OF

			"VARIANT.":	variant_count = variant_count + 1

			"END": BEGIN
				if (DDL_words(1) EQ "VARIANTS.") then begin

				   	message,"requested variant # " + $
						strtrim( variant, 2 ) + $
						" exceeds total " + $
						strtrim( variant_count, 2 ) + $
						" variants",/CONT
					struct_Tags = [ struct_Tags , $
							"non-existing variant" ]
					goto,BREAK_VARIANTS
				   endif
				END
			else:
			ENDCASE

		  endwhile

		struct_Tags = [ struct_Tags , DDL_struct( DDL_Lun, Lun_out, $
					      ABBREV=abbrev, VARIANT=variant ) ]

		while (strpos( DDL_rec, "END VARIANTS" ) LT 0) do begin
			readf, DDL_Lun, DDL_rec
			DDL_rec = strupcase( DDL_rec )
		  endwhile

		BREAK_VARIANTS:

	  endif else begin			;Process Field definition:

		DDL_rec = strmid( DDL_rec, 0, strlen(DDL_rec)-1 )
		DDL_words = get_words( DDL_rec )
		Tag_Name = DDL_words(0)
		if (Tag_Name EQ "*") then Tag_Name = "FILL"
		Tag_Type = DDL_words( N_elements( DDL_words )-1 )

		if (strpos( DDL_rec, "TEXT" ) GT 0) then begin

			w = where( DDL_words EQ "SIZE" )
			dim = DDL_words(w(0)+2)

			if (dim EQ '1') then			$
				Tag_def = ": 0B"			$
			  else	Tag_def = ": bytarr( " + dim + " )"

		 endif else if (strpos( DDL_rec, "ARRAY" ) GT 0) then begin

			wa = where( DDL_words EQ "ARRAY" )
			wd = where( DDL_words EQ "DATATYPE" )
			dims = DDL_words( wa(0)+1 : wd(0)-1 )
			dim = dims(0)
			Ndim = N_elements( dims )

			if (Ndim GT 1) then begin
				for i=1,Ndim-1 do dim = dim + ", " + dims(i)
			   endif

			CASE Tag_Type OF

			"BYTE":	      Tag_def = ": bytarr( " + dim + " )"
			"WORD":       Tag_def = ": intarr( " + dim + " )"
			"LONGWORD":   Tag_def = ": Lonarr( " + dim + " )"
			"F_FLOATING": Tag_def = ": fltarr( " + dim + " )"
			"D_FLOATING": Tag_def = ": dblarr( " + dim + " )"
			"COMPLEX":    Tag_def = ": complexarr( " + dim + " )"
			else: BEGIN
				message,"unknown type [" + Tag_Type + $
					"] of field <" + Tag_Name + ">", /CONT
				Tag_def = ": unknown( " + dim + " )"
				END
			ENDCASE

		  endif else begin

			CASE Tag_Type OF

			"BYTE":		Tag_def = ": 0B"
			"WORD":		Tag_def = ": 0"
			"LONGWORD":	Tag_def = ": 0L"
			"F_FLOATING":	Tag_def = ": 0.0"
			"D_FLOATING":	Tag_def = ": 0.D0"
			"COMPLEX":	Tag_def = ": complex(0)"
			else: BEGIN
				message,"unknown type [" + Tag_Type + $
					"] of field <" + Tag_Name + ">", /CONT
				Tag_def = ": unknown"
				END
			ENDCASE
	           endelse

		if keyword_set( abbrev ) then  DDL_abbrev, Tag_Name

		struct_Tags = [ struct_Tags , Tag_Name + Tag_def ]

	   endelse

;Read the next DDL records until period:

	readf, DDL_Lun, DDL_rec

	while (strpos( DDL_rec, "." ) LT 0) do begin
		readf, DDL_Lun, DDL_rec2
		DDL_rec = DDL_rec + DDL_rec2
	   endwhile

	DDL_rec = strupcase( DDL_rec )

	if strpos( DDL_rec, "END RECORD" ) GT 0 then begin

		free_Lun, DDL_Lun
		Nline = N_elements( func_end )

		for i=0,Nline-1 do printf, Lun_out, func_end(i)
		free_Lun, Lun_out

		message,"finished IDL structure code for "+Rec_Name,/CONTIN
		Nstr = N_elements( struct_Tags )
		return, struct_Tags(Nstr-1)
	   endif

   endwhile

;End of processing current structure (or variant), return its definition:

	w = where( strlen( struct_Tags ) )

return, struct_Tags(w)
end
