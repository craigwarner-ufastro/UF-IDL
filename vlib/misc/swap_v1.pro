	PRO SWAP,INT,TYPE
;+
; NAME:
;	SWAP
; PURPOSE:
;	To swap alternating bytes in a byte, integer, longword integer array
;	or scalar, for conversion from VAX data types to Sun data types.
; CALLING SEQUENCE:
;	SWAP, INT  [, TYPE ]
; INPUT PARAMETER:
;	INT  = Variable in which bytes will be swapped.
; OPTIONAL INPUT PARAMETER:
;	TYPE = Data type.  If not specified, then determined by IDL command
;	       SIZE.
; SIDE EFFECTS:
;	The original vector or scalar is replaced by the swapped data.
; RESTRICTIONS:
;	If INT is a scalar byte, or a byte array with an odd number of bytes,
;	or any other data type other than byte, integer, or longword integer,
;	the procedure returns to the main level.
; PROCEDURE:
;	For byte arrays, the odd and even elements are simply reversed.  For
;	integers, ANDs & ORs and the mask 'FF'X (a byte with all bits on) are
;	used with the intrinsic command ISHFT to interchange bytes.
;*MODIFICATION HISTORY:
;	Apr. 25 1980 D.J. Lindler   initial program.
;	Mar. 21 1988 CAG add VAX RDAF-style prolog, add procedure
;	    call listing, and check for parameters.
;	Feb. 09 1988 RWT change suggested by D. Lindler to handle
;	    longword integers.
;	Jun. 14 1989 RWT modify for SUN IDL, add optional parameter,
;	    and allow swapping of bytes in a byte array.
;	Jan. 08 1989 William Thompson check for odd number of elements.
;-
;
;  Check the number of parameters passed.
;
	NPAR = N_PARAMS(0)
	IF NPAR EQ 0 THEN BEGIN
	    PRINT,'*** SWAP must be called with 1-2 parameters:'
	    PRINT,'               INT  [, TYPE ]'
	    RETALL
	END ELSE IF NPAR LT 2 THEN BEGIN
	    S = SIZE(INT)
	    TYPE = S(S(0)+1)
	ENDIF
;
;  Handle each data type individually.
;
	CASE TYPE OF
;
	    2:  BEGIN				; Byte
	        NPT  = N_ELEMENTS(INT)
	        IF (NPT MOD 2) EQ 0 THEN BEGIN
	            TMP  = INT
	            EVEN = INDGEN(LONARR(NPT/2L)) * 2L
	            ODD  = EVEN + 1L
	            TMP(EVEN) = INT(ODD)
	            TMP(ODD)  = INT(EVEN)
	            INT = TMP
	        END ELSE BEGIN
	            PRINT,'*** Unable to swap odd number of bytes, routine SWAP.'
	            RETALL
	        ENDELSE
	        END
;
	    4:  BEGIN				; Integer
	        I2 = INT AND 'FF'X
	        INT = (ISHFT(INT,-8) AND 'FF'X) OR ISHFT(I2,8)
	        END
;
	   16:  BEGIN				; Longword Integer
	        I1 = ISHFT(INT,-24) AND 'FF'X
	        I2 = ISHFT(INT,-16) AND 'FF'X
	        I3 = ISHFT(INT,-8) AND 'FF'X
	        I4 = INT AND 'FF'X
	        INT = ISHFT(I4,24) OR ISHFT(I3,16) OR ISHFT(I2,8) OR I1
	        END
	    ELSE:  BEGIN
	        PRINT,'*** SWAP only supports Byte, Integer, or Longword Integer variables.'
	        RETALL
	        END
	ENDCASE
;
	RETURN
	END  ;	SWAP
