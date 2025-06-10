FUNCTION READFITS, FILENAME, HEADER, UNSIGN = unsign, NOSCALE = noscale, $
			     SILENT = silent, BLANK_VALUE = Blank_value
;+
; NAME:
;	READFITS
; CALLING SEQUENCE:
;	Result = readfits(filename,[header])
; PURPOSE:
;	Read a FITS file into IDL data and optionally header variables.
; INPUTS:
;	FILENAME = String containing the name of the file to be read.
; OPTIONAL KEYWORDS:
; 	UNSIGN - A keyword which, if present, will cause INTEGER*2 data
;               (BITPIX = 16) to be intepreted as unsigned.  Note that
;                the same effect will occur if the optional FITS keyword 
;                DATATYPE has a a value of UNSIGNED*2, and that a LONG
;                array will result.
;	NOSCALE: If present and non-zero, then the ouput data will not be
;                scaled using the optional BSCALE and BZERO keywords in the 
;                FITS header.   Default is to scale.
;       BLANK_VALUE - The optional BLANK keyword in the FITS header defines 
;                the data value that was assigned to unknown pixel values.
;                For floating point data, the BLANK_VALUE keyowrd can be used
;                to assign a different value to these pixels.
;       SILENT - Normally, READFITS will display the size the array at the
;                terminal.  The SILENT keyword will suppress this
; OUTPUTS:
;	Result of function = array constructed from designated record.
; OPTIONAL OUTPUT:
;	HEADER = String array containing the header from the FITS file.
; EXAMPLE:
;	Read a FITS file TEST.FITS into an IDL image array, IM and FITS 
;       header array, H.   Do not scale the data with BSCALE and BZERO.
;
;               IM = READFITS('TEST.FITS',H,/NOSCALE)
; LIMITATIONS:
;	Cannot handle FITS extensions (NAXIS = 0)
;	Cannot handle GROUPS
;       Cannot handle REAL*8 data on a non-IEEE machine (e.g. VAX)
; PROCEDURES USED:
;       SXPAR	(and SXPAR calls STRNUMBER)
; MODIFICATION HISTORY:
;	WRITTEN, Jim Wofford, January, 24 1989
; 	MODIFIED, Kanav Bhagat, July 1990  convert to Version 2
; 	MODIFIED, Frank Varosi, Oct, 1990
;		added calls to IDL intrinsic ByteOrder for byte swapping,
;		single call to MAKE_ARRAY using IDL to FITS type mapping,
;		handle floating point data with optional blank replacement.
;       MODIFIED, Wayne Landsman   December, 1990
;               corrected ByteOrder call for REAL*4 data
;-
  ON_ERROR,2                    ;Return to user
;
; Check for filename input
;
	N = N_PARAMS(0)		;# OF PARAMETERS WE HAVE
	IF N LT 1 THEN READ,'FILE NAME: ',FILENAME
;
; Check for keywords
;
	IF KEYWORD_SET(UNSIGN) THEN SIGN = 0 ELSE SIGN = 1
;
; Open file and read header information
;
  	OPENR,UNIT,FILENAME,/BLOCK,/GET_LUN
;
	HDR = ASSOC(UNIT,BYTARR(2880))
	HEADER = STRARR(360)		; Start at 10 records
;
	R = 0
	LINMAX = 360
	Y = INDGEN(36*8)
	Y2 = Y - 8*(Y/8) + 80*(Y/8)	;To Get first 8 char of each line
;
; Read header one record at a time
;
LOOP:
	X = HDR(R)
	Z = BYTE(X,0,80,36)			;Force header lines to be 80
	INDEX = WHERE(Z EQ 0B,NFOUND)		; characters long
	IF NFOUND GT 0 THEN Z(INDEX) = 32B      ; Make sure no 0b values
	TEMP = STRING(Z)
	HEADER(R*36) = TEMP
	NAME = STRING(X(Y2))
	POS = STRPOS(NAME,'END   ')
	IF POS EQ -1 THEN BEGIN
		R = R + 1
		IF (R*36) EQ LINMAX THEN BEGIN		;Check length of
			HEADER = [HEADER,STRARR(360)]	; header, and 
			LINMAX = LINMAX + 360		; lengthen if
		END					; necessary
		GOTO,LOOP
	ENDIF 
;
	LASTLINE = 36*R + POS / 8
	HEADER = HEADER(0:LASTLINE)
;
; Set defaults
;
	EXTEND = 'F'
	GROUPS = 'F'
;
; Get parameter values

NAXIS = SXPAR(HEADER,'NAXIS')

; Check for dummy extension header
;
IF NAXIS EQ 0 THEN $
	MESSAGE,"Cannot read a file with NAXIS = 0 (FITS extensions)"

NAX = SXPAR(HEADER,'NAXIS*')			;Read NAXES

Blank = sxpar( HEADER, "BLANK" )		;check for Blank repl. value
if (Blank NE 0) AND $
   (N_elements( Blank_value ) EQ 1) then Blank_Repl =1  else Blank_Repl =0

	NAMES  = STRMID( HEADER,  0,  8 )	;PARAM NAMES
	VALUES = STRMID( HEADER, 10, 20 )	;PARAM VALUES

	w = where( NAMES EQ 'BITPIX  ', nw )
	if (nw GT 0) then BITPIX = FIX( VALUES(w(0)) )

	w = where( NAMES EQ 'BSCALE  ', nw )
	if (nw GT 0) then BSCALE = FLOAT( VALUES(w(0)) )

	w = where( NAMES EQ 'BZERO   ', nw )
	if (nw GT 0) then BZERO	= FLOAT( VALUES(w(0)) )

	w = where( NAMES EQ 'EXTEND  ', nw )
	if (nw GT 0) then EXTEND = STRTRIM( VALUES(w(0)), 2 )

	w = where( NAMES EQ 'GROUPS  ', nw )
	if (nw GT 0) then GROUPS = STRTRIM( VALUES(w(0)), 2 )

		CASE BITPIX OF
			 8:	TYPE = 1
			16:	TYPE = 3
			32:	TYPE = 5
			-32:    TYPE = 2
                        -64:    TYPE = 7
		ENDCASE

IF (BITPIX EQ 16) OR (BITPIX EQ 32) THEN BEGIN       ;Check for unsigned datatypes
	w = where( NAMES EQ 'DATATYPE', nw )       ;Datatype keyword?

	if (nw GT 0) then BEGIN
		V = STRTRIM( VALUES(w(0)), 2 )		;Process data type
		V = STRMID( V, 1, STRLEN(V)-2 ) 	;Remove apostrophes
		V = STRTRIM( V )	            	;Trim blanks
		IF (BITPIX EQ 16) AND (V EQ 'UNSIGNED*2') THEN TYPE = 4
                IF (BITPIX EQ 32) AND (V EQ 'UNSIGNED*4') THEN TYPE = 6
	ENDIF
ENDIF
;
; Check for FITS extensions, GROUPS
;
	IF EXTEND EQ 'T' THEN $
		MESSAGE,'WARNING: File contains FITS extensions ',/INFORM
	IF GROUPS EQ 'T' THEN $
		MESSAGE,'Cannot handle GROUPS'
;
        IF NOT KEYWORD_SET(SILENT) THEN BEGIN   ;Print size of array being read
              SNAX = STRTRIM(NAX,2)
              ST = SNAX(0)
              IF NAXIS GT 1 THEN FOR I=1,NAXIS-1 DO ST = ST + ' by '+SNAX(I) $
                            ELSE ST = ST + ' element'
              MESSAGE,'Now reading ' + ST + ' array',/inform
        ENDIF
;
;
	IDL_types = [0, 1, 4, 2, 3, 3, 3, 5, 6 ]	;corresponding to TYPE

	DATA = make_array( DIM=NAX, TYPE=IDL_types(TYPE) )
;
; Read Data
;              
	R = R + 1
	NBYTES = abs( BITPIX )/8
	NPIX = 2880 / NBYTES	;Pixels per record
	POS = LONG(0)
	s = size( DATA )
	LIM = s(s(0)+2)
	REC = ASSOC( UNIT, BYTARR(2880) )
        if (Blank_Repl) then $
          If !VERSION.ARCH EQ "vax" then Blank_value = Blank_value/4.

	WHILE POS LT LIM DO BEGIN

	   LINE = REC(R)
	   Ndat = NPIX<(LIM-POS)

	   CASE TYPE OF
	       	1:	DATA(POS) = LINE(0:(2879<(LIM-POS-1)))
		2: BEGIN
			DataL = LONG( LINE, 0, Ndat )
			ByteOrder, DataL, /NtoHS
			if (Blank_Repl) then wb = where( DataL EQ Blank, nb ) $
					else nb=0
			DataF = FLOAT( DataL, 0, Ndat )
			if (nb GT 0) then DataF(wb) = Blank_value
			if !VERSION.ARCH EQ "vax" then DataF = 4. * DataF
			DATA(POS) = DataF
		     END
		3:	DATA(POS) = FIX( LINE, 0, Ndat )
		4: 	DATA(POS) = LONG( LINE, 0, Ndat )
		5:	DATA(POS) = LONG( LINE, 0, Ndat )
		6:	DATA(POS) = LONG( LINE, 0, Ndat )
		7: BEGIN
			DataL = LONG( LINE, 0, Ndat*2 )
			ByteOrder, DataL, /NtoHL
			if (Blank_Repl) then wb = where( DataL EQ Blank, nb ) $
					else nb=0
			DataF = DOUBLE( DataL, 0, Ndat )
			if !VERSION.ARCH EQ "vax" then DataF = 4. * DataF
			if (nb GT 0) then DataF(wb) = Blank_value
			DATA(POS) = DataF
		     END
	     ENDCASE

	   POS = POS + NPIX
	   R = R + 1       
	ENDWHILE

	FREE_LUN,UNIT

; Perform any required byte swapping for NON-floating point data:

	CASE TYPE OF
		3:	ByteOrder, DATA, /NtoHS
		4:	ByteOrder, DATA, /NtoHS
		5:	ByteOrder, DATA, /NtoHL
		6:	ByteOrder, DATA, /NtoHL
		else:
	   ENDCASE
;
	IF keyword_set(UNSIGN) THEN BEGIN  ;Interpet Integer as unsigned?
		IF TYPE EQ 3 THEN DATA = DATA + 65535l
	ENDIF
;
;
; Scale data if appropriate
;
	if NOT keyword_set( noscale ) then begin

		IF N_ELEMENTS( BSCALE ) EQ 1  THEN BEGIN
			if (BSCALE NE 1) then DATA = DATA * BSCALE
		  endif

		IF N_ELEMENTS( BZERO ) EQ 1  THEN BEGIN
			if (BZERO NE 0) then DATA = DATA + BZERO
		   endif
	   endif
; Return array
;
	RETURN,DATA
END
