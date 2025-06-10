;+
; NAME:
;	add_struct
;
; PURPOSE:
;	Add the tag values of two structures in nice column format.
;	Tags are chosen by wildcard matching.
;
; CALLING:
;	add_struct, struct_in_out, struct_add, TAG_MATCH=tag_match
;
; INPUTS:
;	struct_in_out = structure to which values are added.
;	struct_add = tag values of this structure are added to struct_in_out.
;
; KEYWORDS:
;
; OUTPUTS:
; EXTERNAL CALLS:
;	function N_struct
;	function unique
; PROCEDURE:
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1999.
;-

pro add_struct, struct_in_out, struct_add, TAG_MATCH=tag_match

	if (N_struct( struct_in_out ) LE 0) or $
		(N_struct( struct_add ) LE 0) then begin
		print,"syntax:	add_struct, struct_in_out, struct_add, TAG_MATCH="
		return
	   endif

	tags = tag_names( struct_in_out )
	tag_match = strupcase( tag_match )
	wadd = where( strpos( tags, tag_match(0) ) eq 0, nadd )

	for i=1,N_elements( tag_match )-1 do begin
		wadd = [ wadd, where( strpos( tags, tag_match(i) ) eq 0, na ) ]
		nadd = nadd + na
	  endfor

	if nadd LE 0 then begin
		message,"no matching tag strings found",/INFO
		return
	   endif

	wadd = wadd( where( wadd GE 0, nadd ) )

	for i=0,nadd-1 do begin

		struct_in_out.(wadd(i)) = struct_in_out.(wadd(i)) + struct_add.(wadd(i))
	  endfor
end
