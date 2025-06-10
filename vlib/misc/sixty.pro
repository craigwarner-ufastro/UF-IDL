;+
;   NAME:
;      SIXTY
;   PURPOSE:
;      Converts decimal number to sexigesimal.
;      Reverse of TEN function.
;   CALLING SEQUENCE:
;      X = SIXTY( DECIMAL ) 
;   INPUTS:
;      DECIMAL -- Decimal scalar or vector quantity (usually degrees RA or DEC).
;   OUTPUTS:
;      Function value returned = double real vector of three elements, 
;      sexigesimal equivalent of input decimal quantity.
;      A negative number is signified by making the first non-zero
;      element of the output vection negative.
;   PROCEDURE:
;      Mostly involves checking arguments and setting the sign.
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, STX, 19-OCT-87         
;      Output changed to single precision.  RSH, STX, 1/26/88
;	modified for SUN-IDL by FV 1989 (added RA keyword)
;	modified to process vector input by FV 2022.
;-

FUNCTION sixty, decimal, RA=ra

  nd = N_elements( decimal )
  if (nd LE 0) then goto,arg_error

  if keyword_set( ra ) then  x = double( decimal )/15. else  x = double( decimal )

      ss = abs( 3600 * x )
      mm = abs( 60 * x ) 
      dd = abs( x )
      result = fltarr(nd,3)
      result[*,0] = float( fix(dd) )
      result[*,1] = float( fix(mm - 60.0d0 * result[*,0]))
      result[*,2] = float( ss -3600.d0 * result[*,0] - 60.0d0 * result[*,1])

      wn = where( decimal LT 0, nn )

      if( nn gt 0 ) then begin
         result[wn,0] = -result[wn,0]
         result[wn,1] = -result[wn,1]
         result[wn,2] = -result[wn,2]
      endif

      return, transpose( result )

arg_error:  
      print,'One input argument needed: scalar or vector'                           
      return,replicate(100.0e0,3)
end
