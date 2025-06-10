pro doc_menu, name, print=printflg, directory = direct, multi = multi, $
	PATH = path, FILE=file
;+
; NAME:
;	Doc_menu
; PURPOSE:
;	Extract the documentation template of one or more procedures.
; CATEGORY:
;	Help, documentation.
; CALLING SEQUENCE:
;	Doc_menu	;For prompting.
;	Doc_menu, Name ;Extract documentation for procedure Name using
;		the current !PATH.
; OPTIONAL INPUT PARAMETERS:
;	Name = string containing the name of the procedure.
;	Under Unix, Name may be "*" for all modules.
;
;	If Name is not passed, then Doc_menu will go into an interactive
;	mode to prompt the user for the directory or library to search, and
;	then for the routine to get information on.
;	
; KEYWORDS:
;	PRINT = keyword parameter which, if set to 1, sends output
;		of doc_menu to the default printer. Under Unix, if PRINT
;		is a string, it is a shell command used for output with
;		its standard input set to the documentation
;		(i.e. PRINT="cat > junk")
; Unix KEYWORDS:
;	DIRECTORY = directory to search.  If omitted, use  current directory
;		and !PATH.
;	MULTI = flag to allow printing of more than one file if the module
;		exists in more than one directory in the path + the current
;		directory.
; VMS KEYWORDS:
;	FILE - If present and non-zero, the output is left in the file
;		userlib.doc, in the current directory.
;	PATH = optional directory/library search path.  Same format
;		and semantics as !PATH.  If omitted, !PATH is used.
;
; OUTPUTS:
;	Documentation is sent to the standard output unless /PRINT
;	is specified.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	Output is produced on terminal or printer.
; RESTRICTIONS:
;	The DIRECTORY and MULTI keywords are ignored under VMS. The
;	FILE and PATH keywords are ignored under Unix.
; PROCEDURE:
;	If NAME is not passed, then this procedure will first display a list of
;	the directories in !PATH, and ask for the user to select one.  Then a
;	list of procedures in the selected directory are displayed, and the
;	user is again asked to select one of them.  Finally, the routine
;	DOC_LIB_xxx, where xxx represents the operating system, is called to
;	display the documentation in the file between the lines containing the
;	characters ";+" and ";-".
;
;	VMS text libraries can also be searched by this routine.
;
;	If a file named "aaareadme.txt" is also found in the selected
;	directory, then this can be selected as the topic "*INFO*".
;
; MODIFICATION HISTORY:
;	Written, DMS, Sept, 1982.
;	Added library param, Jul 1987.
;	Unix version, DMS, Feb, 1988.
;	New VMS version, DMS, Dec. 1989
;	Wrapper procedure to call the correct version
;		under Unix and VMS, AB, Jan 1990
;       Added support for DOS, SNG, Dec, 1990
;	Added interactive capabilities, William Thompson, July 1991.
;-

on_error,2                        ;Return to caller if an error occurs
;
;  Get initial values of keywords.
;
IF N_ELEMENTS(DIRECT) NE 0 THEN DIRECTORY   = DIRECT
IF N_ELEMENTS(PATH)   NE 0 THEN SEARCH_PATH = PATH
;
;  Set the path that will be searched.
;
IF N_PARAMS() EQ 0 THEN BEGIN
	TEMP = !PATH
	IF !VERSION.OS EQ 'vms' THEN BEGIN
		IF N_ELEMENTS(PATH) EQ 1 THEN TEMP = PATH
	END ELSE BEGIN
		IF N_ELEMENTS(DIRECT) EQ 1 THEN TEMP = DIRECT
	ENDELSE
;
;  Expand any VMS logical names.
;
	IF !VERSION.OS EQ 'vms' THEN TEMP = EXPAND_PATH(TEMP)
;
;  Get a listing of all the directories in the path.
;
	IF !VERSION.OS EQ "vms" THEN BEGIN
        	SEP = ','
		DIRSEP = ''
	END ELSE BEGIN
        	SEP = ':'
		DIRSEP = '/'
	ENDELSE
	DIRS = "Current directory"
	WHILE TEMP NE '' DO DIRS = [DIRS,GETTOK(TEMP,SEP)]
	N_DIRS = N_ELEMENTS(DIRS)
;
;  Ask the user which directory should be looked at.
;
	IF N_DIRS EQ 1 THEN BEGIN
		INDEX = 1
	END ELSE IF (!D.NAME EQ 'X' OR (!D.WINDOW NE -1)) THEN BEGIN	; Use WMENU.
		INDEX = WMENU(['Directories',DIRS],TITLE=0,INITIAL=1)
	END ELSE BEGIN
		OPENW, LUN, /GET_LUN, FILEPATH(/TERMINAL), /STREAM, /MORE
		PRINTF, LUN, FORMAT = '(/,"Directories:",/)'
		FOR I = 0,N_DIRS-1 DO		$
			PRINTF, LUN, FORMAT = '(I3,".  ",A)', I+1, DIRS(I)
		FREE_LUN, LUN
		PRINT, FORMAT='(/,/)'
		READ, 'Enter NUMBER for desired directory: ', INDEX
	ENDELSE
;
;  Decide whether a directory/library or "Current Directory" was selected.
;
	IF (INDEX GT 1) AND (INDEX LE N_DIRS) THEN BEGIN
		DIR = DIRS(INDEX-1)+DIRSEP
	END ELSE IF !VERSION.OS EQ "vms" THEN BEGIN
		DIR = '[]'
	END ELSE BEGIN
		DIR = ''
	ENDELSE
;
;  If the directory starts with the tilde character, then change this to home.
;
	FIRST = STRMID(DIR,0,1)
	IF FIRST EQ '~' THEN DIR = GETENV("HOME") + STRMID(DIR,1,32767)
;
;  Set the keywords DIRECTORY and PATH to reflect the selected routine.  Set
;  the directory name.
;
	SEARCH_PATH = DIR
	IF DIR EQ '' THEN BEGIN
		DIRECTORY = '.'
		DIR_NAME = 'Current directory'
	END ELSE BEGIN
		DIRECTORY = STRMID(DIR,0,STRLEN(DIR)-STRLEN(DIRSEP))
		DIR_NAME = DIRECTORY
	ENDELSE
;
;  If the directory starts with the "at" character, then this is a VMS text
;  library.  Spawn the VMS LIBRARY command to get a directory of all the
;  modules in this text library.
;
	IF FIRST EQ '@' THEN BEGIN
		LIBRARY = STRMID(DIR,1,32767)
		SPAWN,'LIBRARY /TEXT /LIST=SYS$SCRATCH:USERLIB.LIS ' + LIBRARY
		OPENR, LUN, /GET_LUN, 'SYS$SCRATCH:USERLIB.LIS'
		LINE = ''
		FOR I = 1,8 DO READF, LUN, LINE		;Skip header
		FILES = ''
		COUNT = 0
		WHILE NOT EOF(LUN) DO BEGIN
			READF, LUN, LINE
			FILES = [FILES,LINE]
			COUNT = COUNT + 1
		ENDWHILE
		FREE_LUN, LUN
		SPAWN,'DELETE /NOLOG /NOCONFIRM  SYS$SCRATCH:USERLIB.LIS;'
		IF COUNT EQ 0 THEN BEGIN
			PRINT,'*** No modules found in selected library.'
			RETURN
		ENDIF
		FILES = FILES(1:*)
		N_FILES = N_ELEMENTS(FILES)
;
;  Otherwise, find all the .PRO files in the selected directory.
;
	END ELSE BEGIN
		FILES = FINDFILE(DIR+'*.pro',COUNT=COUNT)
		IF COUNT EQ 0 THEN BEGIN
			PRINT,'*** No IDL procedure files found in selected directory.'
			RETURN
		ENDIF
;
;  Strip off the .pro part from the procedure names.
;
		IF !VERSION.OS EQ 'vms' THEN TAIL = STRPOS(FILES, '.PRO;') ELSE $
			TAIL = STRPOS(FILES, '.pro')
		N_FILES = N_ELEMENTS(FILES)
		FOR I = 0,N_FILES-1 DO FILES(I) = STRMID(FILES(I), 0, TAIL(I))
;
;  Strip off the path part from the procedure names.
;
		FOR I = 0,N_FILES-1 DO BEGIN            
				IF (!VERSION.OS EQ 'vms') THEN BEGIN
				J = STRPOS(FILES(I), ']') + 1
				FILES(I) = STRMID(FILES(I), J, 32767)
			ENDIF ELSE BEGIN
				J = STRPOS(FILES(I), '/')
				WHILE (J NE -1) DO BEGIN
					FILES(I) = STRMID(FILES(I), J+1, 32767) 
					J = STRPOS(FILES(I), '/')
				ENDWHILE
			ENDELSE
		ENDFOR
;
;  Find any file called "aaareadme.txt" in the selected directory.
;
		README = FINDFILE(DIR+'aaareadme.txt',COUNT=COUNT)
		IF COUNT NE 0 THEN BEGIN
			FILES = ['*INFO*',FILES]
			N_FILES = N_FILES + 1
		ENDIF
	ENDELSE
;
;  Prompt the user for the procedure name to get the documentation for.
;
	FILES = STRUPCASE(FILES)
	IF (!D.NAME EQ 'X' OR (!D.WINDOW NE -1)) THEN BEGIN	; Use WMENU.
		INDEX = WMENU([DIR_NAME,FILES],TITLE=0,INITIAL=1)
		NAME = FILES(INDEX-1)
	END ELSE BEGIN
		OPENW, LUN, /GET_LUN, FILEPATH(/TERMINAL), /STREAM, /MORE
		PRINTF, LUN, FORMAT = '(/,A,":",/)', DIR_NAME
		PRINTF, LUN, FORMAT = '(4A19)', FILES
		FREE_LUN, LUN
		PRINT, FORMAT='(/,/)'
		NAME = ''
		READ, 'Enter NAME of desired procedure: ', NAME
		NAME = STRUPCASE(NAME)
	ENDELSE
ENDIF
;
;  If the file 'aaareadme.txt' was selected, then type it out.  Otherwise, call
;  the version of DOC_LIB for the current operating system.
;
IF NAME EQ '*INFO*' THEN BEGIN
	OPENR, INFILE, /GET_LUN, DIR + 'aaareadme.txt'
	OPENW, LUN, /GET_LUN, FILEPATH(/TERMINAL), /STREAM, /MORE
	LINE = ''
	WHILE NOT EOF(INFILE) DO BEGIN
		READF,INFILE,LINE
		PRINTF,LUN,LINE
	ENDWHILE
	FREE_LUN, LUN
	FREE_LUN, INFILE
END ELSE BEGIN
	CASE !VERSION.OS OF 
		'vms':  DL_VMS2, NAME, FILE=file, PRINT=printflg,	$
				PATH=search_path
		'DOS':  DL_DOS, NAME, DIRECTORY=directory, PRINT=printflg
		 else:  DL_UNIX, NAME, print=printflg,		$
				directory = directory, multi = multi
	ENDCASE
ENDELSE
;
end
