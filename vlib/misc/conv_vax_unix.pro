function  conv_vax_unix, variable, TARGET_ARCH=target
;+
; NAME:
;   CONV_VAX_UNIX     
; PURPOSE:
;    To convert VAX IDL data types to UNIX (Sun,MIPS,etc.) IDL data types.
;    The architecture is obtained from IDL sys.var. !VERSION.ARCH.   
;    (Derived from the IUE procedure VAX2SUN)
;
; CALLING SEQUENCE:
;    		var_unix = conv_vax_unix( var_vax )
; PARAMETERS:
;    variable (REQ) (IO) (BIFDC) (012)
;        The data variable to be converted.  This may be a scalar
;	 or an array.  All IDL datatypes are valid (including structures).
;	The result of the conversion is returned by the function.
; KEYWORD:  
;	TARGET_ARCH = name (string) of desired target architecture
;			if using this function on a VAX.
;		otherwise !VERSION.ARCH is used to determine the conversion.
; EXAMPLE:
;	Read a 100 by 100 matrix of floating point numbers from a data
;	file created on a VAX.  Then convert the matrix values into Sun format.
;
;	IDL> openr,1,'vax_float.dat'
;	IDL> data = fltarr(100,100)
;	IDL> readu,1,data
;	IDL> data = conv_vax_unix( data )
;
; MODIFICATION HISTORY:
;       Written   F. Varosi               August 1990
;       Modified  P. Keegstra             April 1992
;           			Implemented MIPSEL architecture
;-                                   
;****************************************************************************
;
;  Check to see if VARIABLE is defined.

if n_elements( variable ) eq 0 then begin
	message,'*** VARIABLE not defined',/CONTIN
	retall
endif

if N_elements( target ) EQ 1 then arch = target  else arch = !VERSION.ARCH 

CASE arch OF

"sparc": BEGIN
	swap_ints = 1
	swap_float = 1
	END

'386i': BEGIN
	swap_ints = 0
	swap_float = 1
	END

"mipsel": BEGIN
	swap_ints = 0
	swap_float = 2
	END

"vax": BEGIN
	message,"architecture is VAX, no need to convert",/CONTIN
	return,variable
	END

else: BEGIN
;	message,"NOT tested on "+!VERSION.ARCH+" architecture, " + $
;		"will swap bytes and go IEEE float.point",/CONTIN
	swap_ints = 1
	swap_float = 1
	END
ENDCASE

svar = size( variable )
var_type = svar(svar(0)+1)
scalar = (svar(0) eq 0)


CASE var_type OF

  1: return, variable						; byte

  2: BEGIN							; integer
	if (swap_ints GT 0) then begin

		var_out = variable
		byteorder, var_out, /Sswap
		return, var_out

	  endif else return, variable
	END

  3: BEGIN							; longword
	if (swap_ints GT 0) then begin

		var_out = variable
		byteorder, var_out, /Lswap
		return, var_out

	  endif else return, variable
	END

  4: BEGIN         					; floating point
        var_elems = long( svar(svar(0)+2) )
        byte_elems = var_elems*4L

	var_out = byte( [variable], 0, byte_elems )
	if (swap_float GT 0) then byteorder, var_out, /Sswap

        byte_elems = byte_elems + 3L
        i1 = Lindgen( byte_elems/4L )*4L
        i2 = i1 + 1L
        biased = byte( (var_out(i1) AND '7F'X) * 2 ) OR byte( var_out(i2)/128L )
        i = where(biased ne 0)
        if ((size(i))(0) ne 0) then biased(i) = byte(biased(i) - 2)
        var_out(i1) = byte( var_out(i1) AND '80'X ) OR byte( biased/2 )
        var_out(i2) = byte( var_out(i2) AND '7F'X ) OR byte( biased*128 )
	if (swap_float GT 1) then byteorder, var_out, /Lswap

        if scalar then begin

             vout = float( var_out, 0, var_elems )
             return, vout(0)

           endif else begin

		vout = make_array( SIZE=svar )
		vout(0) = float( var_out, 0, var_elems )
		return,vout

	    endelse
     END

  5: BEGIN    	         				; double precision
        var_elems = long( svar(svar(0)+2) )
        byte_elems = var_elems*8L

	var_out = byte( [variable], 0, byte_elems )
	if (swap_float GT 1) then var_out2 = bytarr( byte_elems )

       byte_elems = byte_elems + 7L
       i1 = Lindgen(byte_elems/8L)*8L
       i2 = i1 + 1L
       i3 = i2 + 1L
       I4 = i3 + 1L
       i5 = i4 + 1L
       i6 = i5 + 1L
       i7 = i6 + 1L
       i8 = i7 + 1L
       vout = var_out(i2) AND '80'X
       exponent = fix( ((var_out(i2) AND '7F'X)*2) OR $
 		    ((var_out(i1) AND '80'X)/128) )
       i = where(exponent ne 0)
       if ((size(i))(0) ne 0) then exponent(i) = exponent(i) - 128 + 1022
       vout = vout OR ((exponent AND '7F0'X)/16)
       var_out(i2) = (exponent AND '00F'X)*16
       vout2 = var_out(i8)
       var_out(i8) = ((var_out(i8) AND '07'X)*32) OR ((var_out(i7) AND 'F8'X)/8)
       vout3 = var_out(i7)
       var_out(i7) = ((var_out(i5) AND '07'X)*32) OR ((vout2 AND 'F8'X)/8)
       vout2 = var_out(i6)
       var_out(i6) = ((var_out(i6) AND '07'X)*32) OR ((var_out(i5) AND 'F8'X)/8)
       vout3 = var_out(i5)
       var_out(i5) = ((var_out(i3) AND '07'X)*32) OR ((vout2 AND 'F8'X)/8)
       vout2 = var_out(i4)
       var_out(i4) = ((var_out(i4) AND '07'X)*32) OR ((var_out(i3) AND 'F8'X)/8)
       vout3 = var_out(i3)
       var_out(i3) = ((var_out(i1) AND '07'X)*32) OR ((vout2 AND 'F8'X)/8)
       var_out(i2) = var_out(i2) OR ((var_out(i1) AND '78'X)/8)
       var_out(i1) = vout

	if (swap_float GT 1) then begin
	     var_out2(i1) = var_out(i8)
	     var_out2(i2) = var_out(i7)
	     var_out2(i3) = var_out(i6)
	     var_out2(i4) = var_out(i5)
	     var_out2(i5) = var_out(i4)
	     var_out2(i6) = var_out(i3)
	     var_out2(i7) = var_out(i2)
	     var_out2(i8) = var_out(i1)
             var_out      = var_out2
        endif

        if scalar then begin

             vout = double( var_out, 0, var_elems )
             return, vout(0)

           endif else begin

		vout = make_array( SIZE=svar )
		vout(0) = double( var_out, 0, var_elems )
		return,vout

	    endelse
     END

  6: return, complex( conv_vax_unix( float( variable ), TARGET=target ),  $
		      conv_vax_unix( imaginary( variable ), TARGET=target ) )

  7: return,variable            	; string

  8: BEGIN				; structure
	var_out = variable
	Ntag = N_tags( variable )

	for t=0,Ntag-1 do  var_out.(t) = $
				conv_vax_unix( variable.(t), TARGET=target )
	return, var_out
       END

  else: BEGIN
	message,'*** Data type ' + strtrim(var_type,2) + ' unknown',/CONTIN
	return,variable
       END

  ENDCASE

end
