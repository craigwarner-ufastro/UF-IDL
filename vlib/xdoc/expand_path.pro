	FUNCTION EXPAND_PATH, PATH
;+
; NAME:
;	EXPAND_PATH
; PURPOSE:
;	Expands any logical names in an IDL search PATH (e.g. !PATH) into the
;	directories defined by that logical name.   *** VMS only. ***
; CALLING SEQUENCE:
;	Result = EXPAND_PATH(PATH)
; INPUTS:
;	PATH = Valid IDL search path, e.g. !PATH.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	The variable PATH must be in the proper format for !PATH.
; PROCEDURE:
;	TRNLOG is called with the /FULL_TRANSLATION switch to translate the
;	logical names.
; MODIFICATION HISTORY:
;	William Thompson
;-
;
	TEMP = PATH
	EXP_PATH = ''
	WHILE TEMP NE '' DO BEGIN
		THISPATH = GETTOK(TEMP,',')
		LASTCHAR = STRMID(THISPATH,STRLEN(THISPATH)-1,1)
		IF LASTCHAR EQ ':' THEN BEGIN
			LOGNAME = STRMID(THISPATH,0,STRLEN(THISPATH)-1)
			IF NOT TRNLOG(LOGNAME,TRANS,/FULL_TRANSLATION) THEN $
				TRANS = THISPATH
		END ELSE TRANS = THISPATH
;
	        FOR ITRANS=0,N_ELEMENTS(TRANS)-1 DO BEGIN
		        IF EXP_PATH EQ '' THEN BEGIN
				EXP_PATH = TRANS(ITRANS)
			END ELSE BEGIN
			        EXP_PATH = EXP_PATH + ',' + TRANS(ITRANS)
			ENDELSE
		ENDFOR
	ENDWHILE
;
	RETURN, EXP_PATH
	END
