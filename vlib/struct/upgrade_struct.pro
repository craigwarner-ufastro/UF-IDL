;+
; NAME:
;	upgrade_struct
; PURPOSE:
;	Copy tags from a structure to a newer version of structure,
;	checking first if such an upgrade is really needed.
; CALLING:
;	upgrade_struct, struct_array, REFERENCE_STRUCT=ref_struct
; INPUT:
;	struct_array = array of structures.
; KEYWORDS:
;	REFERENCE_STRUCT = structure to which input should be upgraded.
; OUTPUT:
;	struct_array = array with same structure as the reference structure,
;		if input structure name does not match the reference structure
;		name, or if both are anonymous structures (null names).
;		In such case the tags of original input structure are copied.
;		If names are the same, the input is not changed at all.
; EXTERNAL CALLS:
;	function N_struct
;	pro copy_struct
; PROCEDURE:
;	Compare structure names and call copy_struct if upgrade is needed,
;	otherwise do nothing in which case input is not changed.
; HISTORY:
;	written: 1997 Frank Varosi HSTX @ NASA/GSFC.
;	FV @ UF 2011: simply replicate ref_struct[0].
;-

pro upgrade_struct, struct_array, REFERENCE_STRUCT=ref_struct, VERBOSE=verbose

  Nsin = N_struct( struct_array )
  Nsref = N_struct( ref_struct )

  if (Nsin LE 0) OR (Nsref LE 0) then begin
     print," "
     print,"syntax:    upgrade_struct, struct_array, REF=ref_struct"
     return
  endif

  name_in = tag_names( struct_array, /STRUCTURE_NAME )
  name_ref = tag_names( ref_struct, /STRUCTURE_NAME )

  if (name_in NE name_ref) OR ((name_in EQ "") AND (name_ref EQ "")) then begin

     if keyword_set( verbose ) then help,struct_array
     message,"upgrading structure " + name_in + " to " + name_ref,/INFO

     new_struct = replicate( ref_struct[0], Nsin )

     copy_struct, struct_array, new_struct,/RECUR_TANDEM
     struct_array = new_struct

     if keyword_set( verbose ) then help,struct_array
  endif
end
