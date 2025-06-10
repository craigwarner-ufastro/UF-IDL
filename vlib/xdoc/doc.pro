	PRO DOC, NAME
;+
; NAME:
;	DOC
; PURPOSE:
;	Provides online documentation for IDL procedures found in the IDL
;	search path.
; CATEGORY:
;	Help, documentation.
; CALLING SEQUENCE:
;	DOC	  ;For prompting.
;	DOC, NAME ;Extract documentation for procedure NAME.
; OPTIONAL INPUT PARAMETERS:
;	NAME = String containing the name of the procedure.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Decides whether the graphics device supports X-windows widgets, in
;	which case it calls SCANPATH, else it calls DOC_LIBRARY.
;-
;
        SZ=SIZE(NAME)
        IF SZ(N_ELEMENTS(SZ)-2) EQ 7 THEN INPUT=1 ELSE INPUT=0
        IF ((!D.FLAGS AND 65536) EQ 65536) AND (!D.NAME EQ 'X') THEN SCANPATH,NAME ELSE BEGIN
          IF INPUT THEN DOC_MENU,NAME ELSE DOC_MENU
   	ENDELSE
	RETURN
	END
