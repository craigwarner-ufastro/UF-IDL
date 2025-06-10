FUNCTION sxpar,HDR,NAME,ABORT,COUNT=MATCHES
;+
; NAME:
;    SXPAR
; PURPOSE:
;    Obtain the value of a parameter in a FITS header
; CALLING SEQUENCE:
;    RESULT = sxpar(HDR,NAME,ABORT,COUNT=COUNT)
; INPUTS:
;    Hdr =  FITS header array, (e.g. as returned by SXOPEN)  
;	    string array, each element should have a length of 80
;           characters	
;    Name = String name of the parameter to return.   If Name is of 
;           the form 'keyword*' then an array is returned containing 
;           values of keywordN where N is an integer.  The value
;	    of keywordN will be placed in RESULT(N-1).  The data type 
;           of RESULT will be the type of the first valid match of keywordN 
;          found.
; OPTIONAL INPUTS:
;	ABORT - string specifying that SXPAR should do a RETALL
;		if a parameter is not found.  ABORT should contain
;               a string to be printed if the keyword parameter is not found.
;		If not supplied SXPAR will return with a negative
;		!err if a keyword is not found.
; KEYWORDS:
;       COUNT - Optional keyword to return a value equal to the number of 
;               parameters found by sxpar.
; OUTPUTS:
;	Function value = value of parameter in header.
;	    If parameter is floating, long or string, the
;	    result is of that type.  Apostrophes are stripped
;	    from strings.  If the parameter is logical, 1 is
;	    returned for T, and 0 is returned for F.
;	    If Name was of form 'keyword*' then a vector of values
;	    are returned.
; SIDE EFFECTS:
;       Keyword COUNT returns the number of parameters found.
;	!err is set to -1 if parameter not found, 0 for a scalar
;	value returned.  If a vector is returned it is set to the
;	number of keyword matches found.
;
;       If a keyword occurs more than once in a header, a warning is given,
;       and the first occurence is used.
; PROCEDURE:
;	The first 8 chacters of each element of Hdr are
;	searched for a match to Name.  The value from
;	the last 20 characters is returned.  An error occurs
;	if there is no parameter with the given name.
; MODIFICATION HISTORY:
;	DMS, May, 1983, Written.
;   D. Lindler Oct 86 Returns !err=-1 if name not found,
;			and allows longer string values
;   D. Lindler Aug, 87 added 'keyword*' capability
;   D. Lindler Jan 90 added ABORT input parameter
;   J. Isensee Jul,90 added COUNT keyword
;-
;----------------------------------------------------------------------
VALUE = 0
	if n_params(0) le 2 then begin
		abort_return = 0
		abort = 'FITS Header'
	  end else abort_return = 1
if abort_return then on_error,1 else on_error,2
;
;       check for valid header
;
	S = SIZE(HDR)		;Check header for proper attributes.
	IF (S(0) NE 1) OR (S(2) NE 7) THEN $
		MESSAGE,'FITS Header (first parameter) must be a string array'
;
	NAM = STRTRIM(STRUPCASE(NAME))	;Copy name, make upper case     
;
; Determine if name is of form 'keyword*'
;
	IF STRPOS(NAM,'*') EQ STRLEN(NAM)-1 THEN BEGIN
		NAM=STRMID(NAM,0,STRLEN(NAM)-1)
		VECTOR=1			;Flag for vector output  
		NAME_LENGTH=STRLEN(NAM)		;Length of name 
		NUM_LENGTH=8-NAME_LENGTH	;Max length of number portion  
		IF NUM_LENGTH LE 0 THEN  $
			MESSAGE,'Keyword length must be 8 characters or less'
	   END ELSE BEGIN
		WHILE STRLEN(NAM) LT 8 DO NAM = NAM + ' ' ;Make 8 chars long
		VECTOR=0
	END
;
; Loop on lines of the header 
;
KEYWORD = STRMID(HDR,0,8)
IF VECTOR THEN BEGIN
           NFOUND = WHERE(STRPOS(KEYWORD,NAM) GE 0,MATCHES)
	   IF MATCHES GT 0 THEN BEGIN
                   NUMST= STRMID(HDR(NFOUND),NAME_LENGTH,NUM_LENGTH)
                   NUMBER = INTARR(MATCHES)-1
                   FOR I=0,MATCHES-1 DO $
	            IF STRNUMBER(NUMST(I),NUM) THEN NUMBER(I) = NUM
                   IGOOD = WHERE(NUMBER GE 0,MATCHES)
                   IF MATCHES GT 0 THEN BEGIN
                        NFOUND = NFOUND(IGOOD) & NUMBER = NUMBER(IGOOD)
                   ENDIF
           ENDIF
ENDIF ELSE BEGIN
       NFOUND = WHERE(KEYWORD EQ NAM,MATCHES)
       IF MATCHES GT 1 THEN MESSAGE,$
         'WARNING- Keyword '+NAM +'located more than once in '+abort,/inform
ENDELSE
;
; Process string parameter 
;
IF MATCHES GT 0 THEN BEGIN
 LINE = HDR(NFOUND)
 SVALUE =STRTRIM( STRMID(LINE,9,70),2)
 FOR I=0,MATCHES-1 DO BEGIN
      IF STRMID(SVALUE(I),0,1) EQ "'" THEN BEGIN   ;Is it a string?
		  VALUE=STRMID(SVALUE(I),1,STRLEN(SVALUE(I))-1)
		  ENDAP=STRPOS(VALUE,"'")      ;Ending apostrophe  
		  IF ENDAP LT 0 THEN $
			    MESSAGE,'Value of '+name+' invalid in '+abort
		  VALUE=STRMID(VALUE,0,ENDAP)
	     END ELSE BEGIN
;
; Process non-string value  
;
		  VALUE = STRTRIM(STRMID(LINE(I),10,20),2);Extract value    
		  IF VALUE EQ 'T' THEN VALUE=1 ELSE $
		  IF VALUE EQ 'F' THEN VALUE=0 ELSE $
		  IF STRPOS(VALUE,'.') GE 0 THEN BEGIN
		      if (strpos(value,'E') gt 0) or $
			       (strlen(value) lt 8) then value=float(value) $
						    else value=double(value)
		       END ELSE value = long(value)
	     ENDELSE; if c eq apost
;
;  Add to vector if required
;
	     IF VECTOR THEN BEGIN
                      MAXNUM = MAX(NUMBER)
		      IF I EQ 0 THEN BEGIN
                          SZ_VALUE = SIZE(VALUE)
                          RESULT = MAKE_ARRAY(MAXNUM,TYPE=SZ_VALUE(1),/NOZERO)
		      ENDIF 
                      RESULT(NUMBER(I)-1) =  VALUE
		ENDIF
  ENDFOR
  IF VECTOR THEN BEGIN
         !ERR = MATCHES 
         RETURN,RESULT
  ENDIF ELSE !ERR = 0
ENDIF  ELSE  BEGIN
     if abort_return then message,'keyword '+nam+' not found in '+abort
     !ERR = -1
ENDELSE
RETURN,VALUE
END                 
