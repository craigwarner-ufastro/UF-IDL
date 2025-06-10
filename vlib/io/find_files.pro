;+
; NAME:
;       find_files
; PURPOSE:
;	Search for files with specified file-extension in specified directory.
; CALLING EXAMPLE:
;	find_files, files, filnams, FILEXT=".fits", DIR=["data","images"]
; KEYWORDS:
;	FILEXT = the file-extension (name ending) to look for (e.g. ".dat"),
;		do not use wildcard, because * is automatically placed in front.
;       DIRECTORY_PATH = string array giving a path of directories, which
;                      upon concatenation specifies the subdirectory to search.
;                                               (default is current directory).
;                      Do not include the operating system specific syntax for
;                      directories, that is, leave out the "/" and ".",
;                      they are automatically added depending on op.sys.
; OUTPUT:
;	files = array of complete file names found (nothing if none found)
;	filnams = array file names (without the directory spec in name).
; EXTERNAL CALLS:
;       function dir_path
; PROCEDURE:
;       Use function findfile and then manipulate the strings.
; HISTORY:
;       Written, Frank Varosi NASA/GSFC 1989.
;-

pro find_files, files, filnams, FILEXTENSION=filext, DIRECTORY_PATH=dirpath

	direct = dir_path( dirpath, dir_delim )

	if N_elements( filext ) EQ 1 then begin
		if !Version.Release eq '5.3' then begin
			files_found = [ direct + findfile( direct ) ]
		 endif else files_found = [ findfile( direct + "*" + filext ) ]
	 endif else files_found = [ direct + findfile( direct ) ]

	w = where( strlen( files_found ) , Nfil )
	if (Nfil LE 0) then return

	files_found = files_found(w)

	if ( !VERSION.OS EQ "vms" ) then filext = strupcase( filext )

	if N_elements( filext ) EQ 1 then begin

		posext = strpos( files_found , filext )
		w = where( posext GT 0, Nfil )
		if (Nfil LE 0) then return
		files = files_found[w]
		posext = posext[w]

	  endif else files = files_found

	fnam = byte( files(0) )
	delim = byte( dir_delim )
	wpos = where( fnam EQ delim(0), npos )
	if (npos GT 0) then posf = wpos[npos-1]+1  else posf=0

	if N_elements( posext ) gt 0 then Len = posext - posf else Len=strlen(files)
	filnams = files

	for i=0,Nfil-1 do  filnams(i) = strmid( files(i), posf, Len(i) )
end
