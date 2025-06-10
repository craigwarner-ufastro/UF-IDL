FUNCTION HEADFITS,FILENAME
;+
; NAME:
;	HEADFITS
; CALLING SEQUENCE:
;	Result = headfits( filename )
; PURPOSE:
;	READ A FITS FILE HEADER RECORD
; INPUTS:
;	FILENAME = String containing the name of the file to be read.
; OUTPUTS:
;	Result of function = header record.
; MODIFICATION HISTORY:
;	adapted by Frank Varosi from READFITS by Jim Wofford, January, 24 1989
;-
 
; Open file and read header information

	GET_LUN,UNIT
  	OPENR,UNIT,FILENAME

	HDR = ASSOC( UNIT, BYTARR(2880) )
	HEADER = STRARR(80,360)			;Up to 10 records
	Y = INDGEN(36*8)
	Y2 = Y - 8*(Y/8) + 80*(Y/8)
	R = 0

; Read header one record at a time

LOOP:
	X = HDR(R)
	TEMP = STRING( BYTE(X,0,80,36) )
	HEADER(R*36) = TEMP
	NAME = STRING( X(Y2) )		;Get first 8 char of each line
	POS = STRPOS( NAME,'END' )
	IF (POS LT 0) THEN BEGIN
		R = R + 1
		GOTO,LOOP
	ENDIF 

	LASTLINE = 36*R + POS / 8
	HEADER = HEADER(0:LASTLINE)

	FREE_LUN,UNIT

RETURN, HEADER
END
