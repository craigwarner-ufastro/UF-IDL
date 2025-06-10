pro make_help, DIR_ROOT=root_dir
;+
; NAME:
;	make_help
; PURPOSE:
;	Create IDL help files for routines in all sub-directories,
;				one help file for each sub-directory.
; CALLING SEQUENCE:
;	make_help, DIR_ROOT="main_directory"
; KEYWORDS:
;	DIR_ROOT= string giving name of directory in which sub-directories,
;		containing IDL routines, reside.
; RESULT:
;	An IDL help file will be created for each sub-directory,
;	the name of each help file is name of sub-directory + ".help".
; EXTERNAL CALLS:
;		function find_dirs
;		pro mk_Library_Help
; PROCEDURE:
;	Get sub-directory names and
;	call mk_Library_Help for each sub-directory.
; MODIFICATION HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-
	if N_elements( root_dir ) NE 1 then root_dir = "."
	if (!version.os NE "vms") then root_dir = root_dir + "/"

	dirs = find_dirs( DIR=root_dir, NDIR=ndir )

	if (Ndir LE 0) then begin
		message, root_dir + " has no sub-directories",/INFO
		return
	   endif
	
	for i=0,Ndir-1 do  mk_Library_Help, root_dir + dirs(i),  $
						dirs(i) + ".help", /VERBOSE
return
end
