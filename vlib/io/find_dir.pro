;+
; NAME:
;       find_dir
; PURPOSE:
;       Check for existence of a sub-directory (independent of op.sys.)
;	and return the name with correct op.sys. syntax.
; CALLING:
;       dirspec = find_dir( dir )
; INPUT: 
;       dir = string, the directory name to look for
;                 do not specify wildcards (*) or op.sys. syntax.
; KEYWORD:
;      /LINKS = assume all Links (UNIX only) are actually directories
;                                      and include them in the search.
; OUTPUT:
;       Returns the directory name with appropriate op.sys. syntax ([] or /).
; EXTERNAL CALLS:
;       function find_dirs
; PROCEDURE:
;       Use function findfile in VMS, but otherwise use function find_dirs,
;	and then see if requested directory is in list.
; HISTORY:
;       Written, Frank Varosi NASA/GSFC 1990.
;	F.V. 1994, handle case for MacOS and Win32.
;-

function find_dir, dir, LINKS=Links

	if N_elements( dir ) ne 1 then begin
		print,"syntax:	dir_spec_string = find_dir( dir [, /LINK ] )"
		return,""
	   endif
		
	if ( !VERSION.OS  EQ "vms" ) then begin

		ff = findfile( dir+".DIR" )

		if (ff(0) NE "") then dirnam = "[." + dir + "]"  $
				 else dirnam = ""

	  endif else begin

		dirs = find_dirs( LINKS=Links )

		w = where( strlen( dirs ) GT 0, Ndir )
		if (Ndir LE 0) then return,""

		w = where( dirs EQ dir , nf )

		if (nf GT 0) then begin
			CASE !version.os OF
			  "MacOS":	dlim = ":"
			  "Win32":	dlim = "\"
			  else:		dlim = "/"
			 ENDCASE
			dirnam = dir + dlim
		  endif else dirnam = ""

	   endelse

return, dirnam
end
