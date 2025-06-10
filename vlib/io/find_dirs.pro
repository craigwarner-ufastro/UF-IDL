;+
; NAME:
;       find_dirs
; PURPOSE:
;       Return a list of sub-directories (and links) in specified directory.
;       Can also wildcard search with specific characters in directory names.
; CALLING EXAMPLE:
;       subdirs = find_dirs( dirspec, DIR=["data","images"], NDIR=Ndir )
; INPUT:
;       dirspec = characters to look for in directory name (default = *),
;                 do not specify wildcards (*) because
;                 they are added automatically, ( search is on "*dirspec*" ).
; KEYWORDS:
;       DIRECTORY_PATH = string array giving a path of directories, which
;                      upon concatenation specifies the subdirectory to search.
;                                               (default is current directory).
;                      Do not include the operating system specific syntax for
;                      directories, that is, leave out the "/" and ".",
;                      they are automatically added depending on op.sys.
;       NDIR = returned number of directories found (not including # Links)
;      /LINKS = assume all Links (UNIX only) are actually directories
;                     and include them at the end of the List.
;       NLINK = number of Links found, if /LINKS was set.
; OUTPUT:
;       Returns array of directory names (minus directory delimeters ] or /).
; RESTRICTIONS:
;	Have not tested on Win32, probably does not work.
; EXTERNAL CALLS:
;       function dir_path
;       function VarType
; PROCEDURE:
;       Use function findfile in VMS, but otherwise use "spawn ls -F" on UNIX.
; HISTORY:
;       Written, Frank Varosi NASA/GSFC 1990.
;	F.V. 1994, handle case for MacOS.
;	F.V. 1995, fixed for case of no dirs but found Links.
;	F.V. 1999, handle case for OS = Win32.
;-

function find_dirs, dirspec, DIRECTORY_PATH=dirpath, NDIR=Ndir, $
					LINKS=Links, NLINK = NLink

	direct = dir_path( dirpath )

	if ( !VERSION.OS  EQ "vms" ) then begin

		if VarType( dirspec ) EQ "STRING" then dirspec = "*" + dirspec

		ff = [ findfile( direct + dirspec + "*.DIR", COUNT=Ndir ) ]

		if (Ndir LE 0) then return,""

		dirs = ff
		pos = strpos( ff, "]" ) + 1
		Len = strpos( ff, ".DIR" ) - pos

		for i=0,Ndir-1 do dirs(i) = strmid( ff(i), pos(i), Len(i) )

	 endif else if ( !version.OS  EQ "MacOS" ) then begin

		ff = findfile( direct + "*", COUNT=Nf )
		if (Nf LE 0) then return,""

		fb = byte( ff )
		sz = size( fb )
		sl = strlen( ff )-1 + indgen( sz(2) ) * sz(1)
		w = where( fb(sl) EQ byte(":"), Ndir )
		if (Ndir LE 0) then return,""

		dirs = ff(w)

		if VarType( dirspec ) EQ "STRING" then begin
			w = where( strpos( dirs, dirspec ) GT 0, Ndir )
			if (Ndir GT 0) then dirs = dirs(w)  else dirs=[""]
		   endif

	  endif else if ( !version.OS  EQ "Win32" ) then begin

		ff = findfile( "*", COUNT=Nf )
		if (Nf LE 0) then return,""

		fb = byte( ff )
		sz = size( fb )
		sl = strlen( ff )-1 + indgen( sz(2) ) * sz(1)
		w = where( fb(sl) EQ scalar( byte("\") ), Ndir )
		if (Ndir LE 0) then return,""

		dirs = ff(w)

		if VarType( dirspec ) EQ "STRING" then begin
			w = where( strpos( dirs, dirspec ) GE 0, Ndir )
			if (Ndir GT 0) then dirs = dirs(w)  else dirs=[""]
		   endif

	   endif else begin		;default is to assume UNIX

		if strlen( direct ) LE 0 then direct = "."
		spawn, ["ls","-F",direct], ff, /NOSHELL

		pos = strpos( ff, "/" )
		w = where( [pos] GT 0, Ndir )
		if (Ndir GT 0) then dirs = ff(w)  else dirs=[""]

		if keyword_set( Links ) then begin

			pos = strpos( ff, "@" )
			w = where( pos GT 0, NLink )
			if (NLink GT 0) then dLinks = ff(w)

		  endif else NLink = 0

		if VarType( dirspec ) EQ "STRING" then begin

			w = where( strpos( dirs, dirspec ) GT 0, Ndir )
			if (Ndir GT 0) then dirs = dirs(w)  else dirs=[""]

			if (NLink GT 0) then begin
			   w = where( strpos( dLinks, dirspec ) GT 0, NLink )
			   if (NLink GT 0) then dLinks = dLinks(w)
			  endif
		   endif

		if (Ndir GT 0) AND (NLink GT 0) then dirs = [ dirs, dLinks ] $
		  else	if (Ndir LE 0) AND (NLink GT 0) then dirs = dLinks

		Ndl = Ndir + NLink
		Len = strlen( dirs ) -1

		for i=0,Ndl-1 do dirs(i) = strmid( dirs(i), 0, Len(i) )

	   endelse

return, dirs
end
