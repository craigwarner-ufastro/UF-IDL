;+
; NAME:
;       check_dir
; PURPOSE:
;	Check if a directory actually exists on a UNIX system,
;	for VMS this function checks whether a sub-directory exits.
; CALLING:
;       dir_existing = check_dir( directory )
; INPUT: 
;       directory = string, full name of directory (sub-directory on VMS).
; OUTPUT:
;       Function returns the directory name if it exists,
;	else if directory does not exist it returns null string.
;	On UNIX: directory name is appended with / ,
;	on VMS if is string of form: [.sub-dir] .
; RESTRICTIONS:
;	Have not tested on MacOS or Win32, probably does not work.
; HISTORY:
;       Written: Frank Varosi NASA/GSFC 1994.
;-

function check_dir, directory

	if N_elements( directory ) NE 1 then return,""
	dir = strtrim( directory, 2 )

	nch = strlen( dir )
	if (nch LE 0) then return,""

	if ( !VERSION.OS  EQ "vms" ) then begin

		ff = findfile( dir+".DIR" )
		if (ff(0) NE "") then return,"[." + dir + "]"  else return, ""

	  endif else begin

		if strpos( dir, "/", nch-1 ) NE nch-1 then dir = dir + "/"
		f = findfile( dir + "..", COUNT=nf )
		if (nf LE 0) then return,"" else return,dir
	   endelse
end
