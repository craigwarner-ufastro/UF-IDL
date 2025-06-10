function gammln, xx
;+
; NAME
;    GAMMLN
; PURPOSE:
;    Return the natural log of the gamma function.   Adapted from the
;    algorithm GAMMLN in Section 6.1 in "Numerical Recipes " (Second
;    Edition) by Press  et al. 1992.    This function became obsolete 
;    in IDL 2.4.0 when the equivalent LNGAMMA intrinsic function was 
;    introduced.
; CALLING SEQUENCE:
;    result = gammln ( xx )
; INPUTS:
;    xx - numeric scalar or vector for which the log of the gamma function
;         will be evaluated.    Must be > 0
; OUTPUT:
;    result = alog ( gamma(xx) ).   The result will double precision if xx
;             is double, otherwise the result is floating point.
; NOTES:
;     IDL has an intrinsic gamma function, GAMMA, but overflow occurs in
;     GAMMA for X > 34.5.    By computing the log of the gamma function, one
;     can deal with much larger input values.   GAMMLN also allows double 
;     precision computation, not available with GAMMA.
; EXAMPLE:
;     Compare the output of GAMMA with GAMMLN
;
;       IDL> x = findgen(15)+0.5
;       IDL> print, alog(gamma(x))
;       IDL> print,gammln(x)
; METHOD:
;      Uses the expansion of Lanczos as described in Press et al. (1986)
; REVISION HISTORY:
;       Written,   W. Landsman            July, 1992
;       Double Precision update           October, 1992
;-
 On_error,2
 
 if N_params() EQ 0 then begin
      print,'Syntax -- result = gammln( xx )
      return, -1
 endif
 
 cof =  [ 76.18009172947146D0, -86.50532032941677D0, 24.01409824083091D0, $
        -1.231739572450155D0, 0.1208650973866179D-2, -0.5395239384953D-5 ]

 sqrt_2pi = 2.5066282746310005D0           ;Square root of 2*!PI

 if N_elements( xx ) EQ 0 then $
        message,'ERROR - First parameter is undefined'

 y = xx
 tmp = xx + 5.5D0
 tmp = ( xx + 0.5D0) * alog(tmp) - tmp

 ser = 1.000000000190015d0 
 
 for j = 0,5 do begin
     y = y + 1.D0
     ser = ser +  cof(j) / y
 endfor

;Return floating point unless input is double

 if datatype(xx) EQ 'DOU' then $
      return, tmp + alog( sqrt_2pi * ser / xx )    $
 else return, float( tmp + alog ( sqrt_2pi * ser / xx) )

 end
