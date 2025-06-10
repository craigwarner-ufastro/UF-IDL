	FUNCTION GET_LIB,PATH
;+
; NAME:
;	GET_LIB
; PURPOSE:
;	Place library and directory elements of !PATH into a string array.
;	Used by SCLIB.
; CALLING SEQUENCE:
;	LIB = GET_LIB()
; INPUTS:
;       path = path name (default is !path)
; OUTPUTS:
;	Function result is a string array of path elements.
; PROCEDURE:
;	Reads !PATH and keys on commas (VMS) or colons (UNIX).
; MODIFICATION HISTORY:
;       Written DMZ (ARC) April 1991
;	William Thompson, Dec 1991, modified to be compatible with UNIX, and
;				    with VMS logical names.  Also, to be
;				    compatible with changes in SCANPATH
;-
;
       if n_elements(path) eq 0 then TEMP = !PATH else TEMP=path
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
        CD,CURRENT=CDIR
        DIRS=''
	WHILE TEMP NE '' DO DIRS = [DIRS,GETTOK(TEMP,SEP)]
        NDIRS=N_ELEMENTS(DIRS) & DIRS=DIRS(1:NDIRS-1)
        FIND=WHERE(CDIR EQ DIRS,COUNT)
        IF COUNT EQ 0 THEN DIRS=[CDIR,DIRS]   ;-- ADD CURRENT DIRECTORY
;
	RETURN,DIRS
	END
