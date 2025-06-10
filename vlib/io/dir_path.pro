;+
; NAME:
;       dir_path
; PURPOSE:
;       Construct and return a string specifying a directory (with OS syntax)
;       from a string array of sub-directories (with no OS syntax)
;	which determine a path in generic fashion, thus independent of OS.
; CALLING:
;       directory = dir_path( direct_array, dir_delim )
; INPUT: 
;       direct_array = string array giving a path of sub-directories, which
;                     upon concatenation specifies the subdirectory.
;                                               (default is current directory).
;                     Do not include the operating system specific syntax for
;                     directories, that is, leave out the "/" or ".",
;                     they are automatically added depending on op.sys.
;                     Leading & trailing blanks are trimmed, default ="".
; OUTPUT:
;       dir_delim = optional, string used to delimit (end) the directory name.
;                     (this is "]" on VMS, otherwise uses "/" on UNIX, etc.).
;
;       Function returns string specifying the directory on current OS.
;
; EXTERNAL CALLS:
;	function strconcat
; HISTORY:
;       Written: Frank Varosi NASA/GSFC 1989.
;	F.V. 1994, simplified by using function strconcat.
;	F.V. 1995, handle case for MacOS and Win32.
;-

function dir_path, direct_array, dir_delim

	CASE !version.os OF
	  "vms":	dir_delim = "]"
	  "MacOS":	dir_delim = ":"
	  "Win32":	dir_delim = "\"
	  else:		dir_delim = "/"
	 ENDCASE

	Ndir = N_elements( direct_array )
	if (Ndir LE 0) then return,""

	dir_array = strtrim( direct_array, 2 )
	w = where( [strlen( dir_array )] GT 0, Ndir )
	if (Ndir LE 0) then return,""

	if !version.os EQ "vms" then begin

		return, "[" + strconcat( "." + dir_array(w) ) + "]"

	 endif else begin

		return, strconcat( dir_array(w) + dir_delim )
	  endelse
end
