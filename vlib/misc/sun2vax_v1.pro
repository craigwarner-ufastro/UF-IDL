	pro SUN2VAX, variable
;****************************************************************************
;+
;
;*NAME:
;    SUN2VAX
;
;*PURPOSE:
;    To convert Sun IDL data types to VAX IDL data types.
;
;*CALLING SEQUENCE:
;    SUN2VAX, variable
;
;*PARAMETERS:
;    variable (REQ) (IO) (BIFDC) (012)
;        The data variable to be converted.  This may be a scalar
;	 or an array.  Valid datatypes are integer, longword,
;	 floating point, and double precision. The result of the 
;        conversion is passed back in the original variable.
;
;*COMMON BLOCKS:
;	none
;
;*SIDE EFFECTS:
;	none
;
;*RESTRICTIONS:
;	Data types passed here must be in the set listed in the "INPUTS"
;	section.
;
;*EXAMPLE:
;	Read a 100 by 100 matrix of floating point numbers from a data
;	file created on a Sun.  Then convert the matrix values into
;	VAX format.
;
;	IDL> openr,1,'vax_float.dat
;	IDL> data = fltarr(100,100)
;	IDL> forrd,1,data
;	IDL> SUN2VAX,data
;
;*MODIFICATION HISTORY:
;	Version 1	By John Hoegy		13-Jun-88
;	21-Oct-88 - JAH:  Fixed problem where it wouldn't convert float
;			  and double scalars.
;	24-Oct-88 - JAH:  Fixed problem with converting integer arrays.
;       20-Jun-89 - RWT:  use SWAP.PRO for byte swapping, remove GOTO's
;                         and modify prolog entries
;	09-Jan-90 - WTT:  Changed name to VAX2SUN.  Added checks for undefined 
;			  variable, complex, structure, unknown type.
;	04-May-90 - WTT:  Created SUN2VAX from VAX2SUN, reversing floating
;			  point procedure.
;-
;****************************************************************************
;
;  Check to see if VARIABLE is defined.
;
if n_elements(variable) eq 0 then begin
	print,'*** VARIABLE not defined, routine SUN2VAX.'
	retall
endif
;
var_chars = size(variable)
var_type = var_chars(var_chars(0)+1)
;
case var_type of
  2: return			; byte
  4: swap,variable		; integer
 16: swap,variable		; longword
  8: begin         		; floating point
        scalar = (var_chars(0) eq 0)
        var_elems = long(var_chars(var_chars(0)+2))
        byte_elems = var_elems*4L
        if scalar then begin
	    tmp = fltarr(1)
            tmp(0) = variable
            byte_eq = byte(tmp, 0, byte_elems)
            endif else byte_eq = byte(variable, 0, byte_elems)
    ;
        i1 = indgen(lonarr(byte_elems/4L))*4L
        i2 = i1 + 1L
        biased = byte((byte_eq(i1) AND '7F'X) * 2) OR byte(byte_eq(i2)/128L)
        i = where(biased ne 0)
        if (i(0) ne -1) then biased(i) = byte(biased(i) + 2)
        byte_eq(i1) = byte(byte_eq(i1) AND '80'X) OR byte(biased/2)
        byte_eq(i2) = byte(byte_eq(i2) AND '7F'X) OR byte(biased*128)
    ; 
    ; swap bytes
    ;
        byte_elems = byte_elems + 3L
        swap,byte_eq
;
        if scalar then begin
           tmp = fltarr(1)
           tmp(0) = float(byte_eq, 0, var_elems)
           variable = tmp(0)
           endif else variable(0) = float(byte_eq, 0, var_elems)
        return & end
 32: begin    	         	; double precision
        var_elems = long(var_chars(var_chars(0)+2))
        byte_elems = var_elems*8L
	scalar = (var_chars(0) eq 0)
        if scalar then begin
             tmp = dblarr(1)
	     tmp(0) = variable
      	     byte_eq = byte(tmp, 0, byte_elems)
             endif else byte_eq = byte(variable, 0, byte_elems)
    ;
    ;  Bring it up to at least a double-precision level.
    ;
       byte_elems = byte_elems + 7L
       i1 = indgen(lonarr(byte_elems/8L))*8L
       i2 = i1 + 1L
       i3 = i2 + 1L
       I4 = i3 + 1L
       i5 = i4 + 1L
       i6 = i5 + 1L
       i7 = i6 + 1L
       i8 = i7 + 1L
       exponent = fix( ((byte_eq(i1) AND '7F'X)*16) OR $
 		    ((byte_eq(i2) AND 'F0'X)/16) )
       i = where(exponent ne 0)
       if (i(0) ne -1) then exponent(i) = exponent(i) + 128 - 1022
       tmp1 = byte_eq(i8)
       byte_eq(i8) = ((byte_eq(i7) and '1f'x)*8) or ((tmp1 and 'e0'x)/32)
       tmp2 = byte_eq(i7)
       byte_eq(i7) = (tmp1 and '1f'x)*8
       tmp3 = byte_eq(i6)
       byte_eq(i6) = ((byte_eq(i5) and '1f'x)*8) or ((tmp3 and 'e0'x)/32)
       tmp1 = byte_eq(i5)
       byte_eq(i5) = ((tmp3 and '1f'x)*8) or ((tmp2 and 'e0'x)/32)
       tmp2 = byte_eq(i4)
       byte_eq(i4) = ((byte_eq(i3) and '1f'x)*8) or ((tmp2 and 'e0'x)/32)
       tmp3 = byte_eq(i3)
       byte_eq(i3) = ((tmp2 and '1f'x)*8) or ((tmp1 and 'e0'x)/32)
       tmp1 = byte_eq(i2)
       byte_eq(i2) = (byte_eq(i1) and '80'x) or byte((exponent and 'fe'x)/2)
       byte_eq(i1) = byte((exponent and '1'x)*128) or ((tmp1 and 'f'x)*8) or $
             ((tmp3 and 'e0'x)/32)
;
       if scalar then begin
           tmp = dblarr(1)
           tmp(0) = double(byte_eq, 0, var_elems)
           variable = tmp(0)
           endif else variable(0) = double(byte_eq, 0, var_elems)
       return & end
 64: begin			; complex
       print,'*** SUN2VAX does not support COMPLEX conversion explicitly.'
       print,'*** Decompose into real and imaginary parts and convert seperately.'
       retall
       end
  1: return                     ; string
  else: begin			; unknown
       print,'*** Data type ' + trim(var_type) + ' unknown, routine SUN2VAX.'
       retall
       end
  endcase
return
end
