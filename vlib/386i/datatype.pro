FUNCTION DATATYPE,VAR, FLAG
;+
; NAME: 
;    DATATYPE
; PURPOSE: 
;     Returns data type as a string.  Makes it easier to do
;     data type dependent operations.
; CALLING SEQUENCE: 
;     TYP = DATATYPE(VAR, [FLAG])
; INPUTS: 
;     VAR = a variable.
;     [FLAG] = Output format flag: 0 (def) gives 3 char type, else
;              type is spelled out.
; OUTPUTS: 
;    TYP = a string indicating the data type of VAR.
;    TYP is 3 char upper case: UND, STR, BYT, INT, FLO, LON, DOU, COM
;    unless FLAG = 1 (non-zero).
; Procedure: 
;    Uses the SIZE function to determine the datatype.
; MODIFICATION HISTORY: 
;       Written by R. Sterner, 29 June, 1988.
;	Johns Hopkins University Applied Physics Laboratory.
;       Modified for version 2 IDL B. PFarr, STX, 27 Jan, 1990.
;	Structures added.  M. Greason, STX, 14 August 1990.
;-
IF N_PARAMS(0) LT 2 THEN FLAG = 0	; Default flag.
;
S = SIZE(VAR)
;
IF FLAG EQ 0 THEN BEGIN
	  CASE S(S(0)+1) OF
   0:	    TYP = 'UND'
   7:       TYP = 'STR'
   1:       TYP = 'BYT'
   2:       TYP = 'INT'
   4:       TYP = 'FLO'
   3:       TYP = 'LON'
   5:       TYP = 'DOU'
   6:       TYP = 'COM'
   8:       TYP = 'STC'
ELSE:       PRINT,'Error in DATATYPE'
	  ENDCASE
ENDIF ELSE BEGIN
	  CASE S(S(0)+1) OF
   0:	    TYP = 'Undefined'
   7:       TYP = 'String'
   1:       TYP = 'Byte'
   2:       TYP = 'Integer'
   4:       TYP = 'Float'
   3:       TYP = 'Long'
   5:       TYP = 'Double'
   6:       TYP = 'Complex'
   8:       TYP = 'Structure'
ELSE:       PRINT,'Error in DATATYPE'
	  ENDCASE
ENDELSE
;
RETURN, TYP
END
