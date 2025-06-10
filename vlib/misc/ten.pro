;+
;   NAME:
;      TEN
;   PURPOSE:
;      Converts sexigesimal number to decimal. Inverse of SIXTY function.
;   CALLING SEQUENCES:
;      X=TEN([HOUR_OR_DEG,MIN,SEC])
;      X=TEN( HOUR_OR_DEG,MIN,SEC )
;      X=TEN([HOUR_OR_DEG,MIN])
;      X=TEN( HOUR_OR_DEG,MIN )
;      X=TEN([HOUR_OR_DEG])      <--  Trivial cases
;      X=TEN(HOUR_OR_DEG)        <--
;   INPUTS:
;      VECTOR -- Sexigesimal quantity.  Elements represent units      
;                in order from largest to smallest.
;   OUTPUTS:
;      Function value returned = double real scalar, decimal equivalent of
;      input sexigesimal quantity.  A minus sign on any element
;      of the input vector causes all the elements to be taken as < 0.
;   PROCEDURE:
;      Mostly involves checking arguments and setting the sign.
;   MODIFICATION HISTORY:
;      Written by R. S. Hill, STX, 21 April 87       
;      Modified to allow non-vector arguments.  RSH, STX, 19-OCT-87
;	modified for SUN-IDL by FV 1989 (added RA keyword)
;	FV 2011, fixed stupid mistake for the /RA option.
;-

FUNCTION ten,dd,mm,ss, RA=ra

      np = n_params(0)
      if (np lt 1) or (np gt 3) then goto,bad_args

      if (np eq 1) then begin            
         vector = double( dd )
      endif else begin
         vector=dblarr(3)
         vector[0]=dd
         vector[1]=mm
         if (np gt 2) then vector[2]=ss
      endelse

	if keyword_set( ra ) then vector[0] = 15. * vector[0]

      sz = size(vector)
      ndim = sz(0)
      if (ndim eq 0) then return, vector

      nel = sz(1)
      signflag = total( vector lt dblarr(nel) )
      if (signflag gt 0.0d0) then sign = -1  else sign = +1

      vector = abs( vector ) / [ 1., 60., 3600. ]

      return, sign * total( vector )

bad_args:    
      print,"Argument(s): hours/degrees, minutes (optional), seconds (optional)"
      print," should be in vector or as separate arguments."
      print,"If any one number negative, all taken as negative."
      return, 0.0d0     
end
