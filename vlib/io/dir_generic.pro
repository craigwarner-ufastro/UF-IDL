;+
; NAME:
;       dir_generic
; PURPOSE:
;       Convert string specifying a subdirectory into a string array
;	of sub-directories which determine a path in generic fashion.
;       (result is thus independent of operating systems).
; CALLING:
;       direct_array = dir_generic( directory )
; INPUT: 
;       directory = string, full name of directory.
; OUTPUT:
;       Function returns string array giving a path of sub-directories,
;	which do not include the OS specific syntax for directories,
;	that is, leaving out the "/" or ".".
; EXTERNAL CALLS
;	function get_words
; HISTORY:
;       Written, Frank Varosi NASA/GSFC 1994.
;	F.V. 1995, handle case for MacOS and Win32.
;-

function dir_generic, directory

	if N_elements( directory ) NE 1 then return,""
	directory = strtrim( directory, 2 )

	if ( !version.OS  EQ "vms" ) then begin

		return, get_words( directory, DELIM=[ "[", ".", "]" ] )

	 endif else if ( !version.OS  EQ "MacOS" ) then begin

		return, get_words( directory, DELIM=":" )

	 endif else if ( !version.OS  EQ "Win32" ) then begin

		return, get_words( directory, DELIM="\" )

	  endif else begin

		dirpath = get_words( directory, DELIM="/" )
		if strpos( directory, "/" ) EQ 0 then dirpath = ["/",dirpath]
		return, dirpath
	   endelse
end
