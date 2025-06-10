;+
; NAME:
;	substwid
; PURPOSE:
;	Substitute the actual home directory of user for "~/" (twiddle char.)
;	in a file name or directory name, on a Unix system.
;	Note that the form ~username is not altered.
; CALLING:
;	filename_full = substwid( filename )
; INPUTS:
;	filename = string, name of file preceeded with directory
;		or just a Unix directory specification.
; OUTPUTS:
;	Function returns string with home directory substituted
;	if "~/" was in file directory specification, else just the input string.
; COMMON BLOCKS:
;	common substwid, home_dir	(to save the home directory string)
; PROCEDURE:
;	Spawn a shell and execute the "echo ~" command to get home directory,
;	and this home-dir is saved in common block for further calls.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1993.
;-

function substwid, filename

  common substwid, home_dir

	if N_elements( filename ) LE 0 then return,""
	if (!Version.Arch EQ "vms") then return, filename

	sz = size( filename )
	if (sz(0) GT 0) then begin
		for i=0,sz(sz(0)+2)-1 do filename(i) = substwid( filename(i) )
		return, filename
	   endif

	if strpos( filename, "~/" ) EQ 0 then begin

		if N_elements( home_dir ) NE 1 then spawn,"echo ~",home_dir
		fname = home_dir + strmid( filename, 1, strlen( filename ) )
		return, fname(0)

	  endif else return, filename
end
