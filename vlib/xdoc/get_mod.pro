	FUNCTION GET_MOD,LIBRARY
;+
; NAME:
;	GET_MOD
; PURPOSE:
;	Extract list of procedure modules from a library or directory.  Used by
;	SCPATH.
; CALLING SEQUENCE:
;	MODS = GET_MOD(LIB)
; INPUTS:
;	LIB  = Library or directory name.
; OUTPUTS:
;	Result of function is a string array with each module name.
; PROCEDURE:
;	Spawns a LIBRARY/LIST or uses FINDFILE.
; MODIFICATION HISTORY:
;       Written DMZ (ARC) May 1991
;	William Thompson, Dec 1991, modified to be compatible with UNIX.
;       DMZ (DEC'92), fixed bug in FINDFILE with long argument lists.
;	F.V.1993, fixed up the UNIX part of code (spawn, etc...).
;-
;
        if !version.os eq 'vms' then dirsep='' else dirsep='/'
	LIB = STRTRIM(LIBRARY,2)
;
;  Is it a text library or a directory?  If the first character is a "@", then
;  it must be a library.
;
	TCHAR = STRMID(LIB,0,1)
	IF TCHAR EQ "@" THEN BEGIN
		LIB = STRMID(LIB,1,STRLEN(LIB)-1)
		BREAK_FILE,LIB,DSK,DIREC,NAME,EXT
		LNAME = 'SYS$LOGIN:'+NAME+'._MOD'
		FIND= FINDFILE(LNAME,COUNT=FC)
		IF FC EQ 0 THEN BEGIN
			STATEMENT = '$LIBRARY/LIST=' + LNAME + ' ' + LIB
			SPAWN,STATEMENT
		ENDIF
;
;  Read lines from file created above.
;
                ON_IOERROR,OOPS
		MODS=''
		OPENR,LUN,LNAME,/GET_LUN
		I=0
		WHILE NOT EOF(LUN) DO BEGIN
			LINE = ''
			READF,LUN,LINE
			I = I+1
			IF (LINE NE '') AND (I GT 7) THEN	$
				MODS = [MODS,STRTRIM(LINE,2)]
		ENDWHILE
		CLOSE,LUN
		FREE_LUN,LUN
		MODS=MODS(1:*) 
                ON_IOERROR,NULL
;
;  Otherwise, it's just a directory.
;
	ENDIF ELSE BEGIN
		IF !VERSION.OS EQ "vms" THEN begin

                  MODS = FINDFILE(LIB+'*.pro',count=fcount)

                endif else begin

                  spawn, "ls "+lib, mods

		  w = where( strpos( mods, "@" ) GE 0, nat )
	 	  for i=0,nat-1 do begin
			modi = mods(w(i))
			mods(w(i)) = strmid( modi, 0, strlen(modi)-1 )
		   endfor

		  w = where( strlen( mods )-strpos( mods,".pro" ) EQ 4, fcount )
		  if (fcount GT 0) then  mods = mods(w)  else mods=""

                endelse

                if fcount eq 0 then return,''
;
;  Find any "aaareadme.txt" files.
;
		README = FINDFILE(LIB+DIRSEP+'aaareadme.txt',COUNT=COUNT)
		IF COUNT NE 0 THEN MODS = ['*INFO*',MODS]
		FOR I=0,N_ELEMENTS(MODS)-1 DO BEGIN
;
;  Strip off the path part from the procedure names.
;
			IF (!VERSION.OS EQ 'vms') THEN BEGIN
				J = STRPOS(MODS(I), ']') + 1
				MODS(I) = STRMID(MODS(I), J, 32767)
			ENDIF ELSE BEGIN
				J = STRPOS(MODS(I), '/')
				WHILE (J NE -1) DO BEGIN
					MODS(I) = STRMID(MODS(I), J+1, 32767) 
					J = STRPOS(MODS(I), '/')
				ENDWHILE
			ENDELSE
		ENDFOR
	ENDELSE
;
;	IF !VERSION.OS EQ 'vms' THEN MODS = STRUPCASE(MODS)
OOPS:   RETURN,MODS
	END
