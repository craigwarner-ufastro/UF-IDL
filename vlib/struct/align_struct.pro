pro align_struct, struct_name, struct, Max_Field_Size, struct_size, Lun_out
;+
; NAME:
;	ALIGN_STRUCT
; PURPOSE:
;	Check an IDL structure for standard memory alignment of fields,
;	insert any byte padding required to align fields on size boundaries,
;	write out new code to define the aligned version of structure.
; CATEGORY:
;			Structures
; CALLING SEQUENCE:
;			align_struct, structure_name
;	examples:
;			align_struct, "fdq_sdf"
; INPUTS:
;		structure_name = string giving the structure name
;				as known by IDL (help,/struct,variable).
;
; inputs/outputs used recursively:
;		struct = the structure variable currently analyzed.
;		Max_Field_Size = size of the largest field found in structure.
;		struct_size = total size of all fields in structure.
;		Lun_out = Logical unit number for writing .PRO code.
;
; SIDE EFFECTS:
;		"structure_name"_ST_AL.PRO is created (IDL function code).
; PROCEDURE:
;		Strategy is to call align_struct recursively.
;
;  otherwise the field type and size is used to construct IDL definition,
;	which is then concatenated with the other IDL definitions,
;
; MODIFICATION HISTORY:
;	written 1990 Frank Varosi STX @ NASA/GSFC
;-
	if N_tags( struct ) LE 0 then begin

		s = size( struct_name )
		
		if ( s(s(0)+1) NE 7 ) then begin
			message,"specify STRUCTURE NAME in first arg.",/CONTIN
			return
		   endif

		struct_name = strupcase( struct_name )
		status = execute( "struct=replicate({" + struct_name + "},1)" )

		func_struct = struct_name + "_ST_AL"

		func_code =  [  "function " + func_struct + ", Nstruct"      ,$
				" "					     ,$
				"  common "  + func_struct + ", defined"     ,$
				" "					     ,$
				"  if N_elements( defined ) NE 1 then begin" ,$
				" " ]

		struct_align = strmid( struct_name, 0, 12 ) + "_AL"

		func_end =  [   "       defined = 1 "			     ,$
				"    endif"				     ,$
				" "					     ,$
			"  if N_elements( Nstruct ) NE 1 then Nstruct = 1"   ,$
			" "						     ,$
			"return, replicate( {" + struct_align + "}, Nstruct )",$
			"end" ]

		Ncode = N_elements( func_code )

		file_struct = func_struct + ".PRO"
		openw, Lun_out, file_struct, /get_Lun
		message, "creating file " + file_struct, /CONTIN

		for i=0,Ncode-1 do printf, Lun_out, func_code(i)

		print," Structure Name    Size   Max Field Size"
	   endif

	Tags = Tag_Names( struct )
	Ntag = N_tags( struct )
	Max_Field_Size = 1
	struct_size = 0
	struct_def = ""

	for t=0,Ntag-1 do begin

		s = size( struct.(t) )
		N_elem = s( s(0)+2 )
		N_els = strtrim( N_elem, 2 )
		Tag_Name = Tags(t)
		Field_Type = s( s(0)+1 )

		if (Field_Type EQ 8) then begin

			bytpad = struct_size MOD 8     ;req. align. for structs?

			if (bytpad NE 0) then begin
				pn = strtrim( t, 2 )
				Nb = strtrim( bytpad, 2 )
				pad_def = "PAD_" + pn + ": bytarr( " + Nb + " )"
				struct_def = [ struct_def, pad_def ]
				struct_size = struct_size + bytpad
				print, "Post-Padding", bytpad, " bytes",$
					FORM="(A15,I8,(A))"
			   endif

		   align_struct, Tags(t), struct.(t), MaxF_siz, ST_siz, Lun_out

			if (N_elem GT 1) then begin
				Tag_def = ": replicate( " + Tag_Name + $
							", " + N_els + " )"
			  endif else Tag_def = ": " + strlowcase( Tag_Name )

			struct_def = [ struct_def , Tag_Name + Tag_def ]
			struct_size = struct_size + ST_siz * N_elem
			Max_Field_Size = Max_Field_Size > MaxF_siz

		  endif else begin

			CASE Field_Type OF
			1: BEGIN
				Field_Size = 1
				if (N_elem EQ 1) then Tag_def = ": 0B"	$
				   else	Tag_def = ": bytarr( " + N_els + " )"
				END
			2: BEGIN
				Field_Size = 2
				if (N_elem EQ 1) then Tag_def = ": 0"	$
				   else Tag_def = ": intarr( " + N_els + " )"
				END
			3: BEGIN
				Field_Size = 4
				if (N_elem EQ 1) then Tag_def = ": 0L"	$
				   else Tag_def = ": Lonarr( " + N_els + " )"
				END
			4: BEGIN
				Field_Size = 4
				if (N_elem EQ 1) then Tag_def = ": 0.0"	$
				   else Tag_def = ": fltarr( " + N_els + " )"
				END
			5: BEGIN
				Field_Size = 8
				if (N_elem EQ 1) then Tag_def = ": 0.D0" $
				   else Tag_def = ": dblarr( " + N_els + " )"
				END
			6: BEGIN
				Field_Size = 8
				if (N_elem EQ 1) then Tag_def = ": complex(0)" $
				 else Tag_def = ": complexarr( " + N_els + " )"
				END
			7: BEGIN
				Tag_def = ""
				Tag_Name = ""
				END
			else: BEGIN
				Tag_def = ""
				Tag_Name = ""
				END
			ENDCASE

			bytpad = struct_size MOD Field_Size

			if (bytpad NE 0) then begin
				pn = strtrim( t, 2 )
				Nb = strtrim( bytpad, 2 )
				pad_def = "PAD_" + pn + ": bytarr( " + Nb + " )"
				struct_def = [ struct_def, pad_def ]
				struct_size = struct_size + bytpad
			   endif

			struct_def = [ struct_def , Tag_Name + Tag_def ]
			struct_size = struct_size + Field_Size * N_elem
			Max_Field_Size = Max_Field_Size > Field_Size

		   endelse
	  endfor

	bytpad = struct_size MOD Max_Field_Size

	if (bytpad NE 0) then begin
		Nb = strtrim( bytpad, 2 )
		pad_end = "PAD_END" + ": bytarr( " + Nb + " )"
		struct_def = [ struct_def, pad_end ]
		struct_size = struct_size + bytpad
	   endif

;write out the aligned structure definition IDL code:

	struct_namLc = strlowcase( struct_name )
	struct_name = strmid( struct_name, 0, 12 ) + "_AL"

	print, struct_name, struct_size, Max_Field_Size, FORM="(A15,I8,I8)"

	structure = "       " + struct_namLc + " = { " + struct_name

	blanks = replicate( 32B, 80 )
	Lpad = strpos( structure, "{" ) + 1
	struct_def = string( blanks(0:Lpad) ) + struct_def(1:*)

	structure = [ structure, struct_def ]

	Lpad = strlen( structure )
	Lpad = (max( Lpad ) - Lpad) < 70
	Nstr = N_elements( structure )

	for i=0,Nstr-2 do structure(i) = structure(i) + $
					string( blanks(0:Lpad(i)) ) + ", $"

	structure(Nstr-1) = structure(Nstr-1) + $
					string( blanks(0:Lpad(i)+4) ) + "}"

	for i=0,Nstr-1 do printf, Lun_out, structure(i)
	printf, Lun_out, ""

	Nend = N_elements( func_end )

	if (Nend GT 0) then begin
		for i=0,Nend-1 do printf, Lun_out, func_end(i)
		free_Lun, Lun_out
		message,"finished aligned structure code for "+struct_name,/CONT
	   endif
return
end
