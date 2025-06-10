PRO fitsbyte, im, HOST=host
;+
; NAME:
;	FITSBYTE
; CALLING SEQUENCE:
;	fitsbyte, im [, host=conversion_flag]
; PURPOSE:
;	To convert an image array from FITS byte ordering to the ordering
;	used by the computer.
; INPUT/OUTPUT:
;	im  The image array (both input and output).
; OPTIONAL KEYWORDS:
; 	HOST  If present and non-zero, the image is converted to "host" 
;	      form.  Otherwise, it is converted to "network", or FITS,
;	      form.
; COMMON BLOCKS:
;	None
; PROCEDURE:
;	The byteorder procedure is used to perform the conversion.  The
;	form of this conversion depends upon the datatype of the array.
;	If it is of I*2 format, a short integer swap is performed.  If it 
;	is of I*4 format, a long integer swap is performed.  Otherwise, 
;	nothing is done.
; MODIFICATION HISTORY:
;	Written by Michael R. Greason, STX, 21 August 1990.
;	Mod F.Varosi Oct.1991, do short integer byte swap for floating point.
;-
;			Initialize.
;
s = size(im)
dtype = s(s(0) + 1)			; The datatype.
h = keyword_set(host)			; Is the host keyword set?
i2 = 2					; Size type code for I*2.
i4 = 3					; Size type code for I*4.
float = 4				; Size type code for Floating point.
;
;			If I*2, do a short integer swap.
;
IF (dtype EQ i2) THEN BEGIN
	IF (h) THEN byteorder, im, /ntohs $	; Convert to Host format.
	       ELSE byteorder, im, /htons	; Convert to Network format.
ENDIF ELSE BEGIN
;
;			If I*4, do a long integer swap.
;
	IF (dtype EQ i4) THEN BEGIN
		IF (h) THEN byteorder, im, /ntohl $ ; Convert to Host format.
		       ELSE byteorder, im, /htonl ; Convert to Network format.
	   ENDIF
;
;	If Floating point, do a short integer swap (as for Sun386i and VAX).
;
	IF (dtype EQ float) THEN BEGIN
		IF (h) THEN byteorder, im, /ntohs $ ; Convert to Host format.
		       ELSE byteorder, im, /htons ; Convert to Network format.
	   ENDIF
ENDELSE
;
;			Done.
;
RETURN
END
