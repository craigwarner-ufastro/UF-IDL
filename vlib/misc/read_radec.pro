;+
; NAME:
;	read_radec
; PURPOSE:
;	Read R.A. and DEC from user, convert and return as degrees.
;	User input can be delimeted with commas, spaces or ":".
; CALLING:
;	read_radec, rasc,dec
; INPUTS:
;	rasc = default value of Right Ascension.
;	dec = default value of Declination.
; OUTPUTS:
;	rasc = degrees Right Ascension.
;	dec = degrees Declination.
; EXTERNAL CALLS:
;	function Ten
;	function sixty
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1989, based "radec.pro" in astro-Lib.
;	F.V. 1991, added option to use default values.
;-

pro read_radec, rasc,dec

  if (N_elements( rasc ) EQ 1) AND (N_elements( dec ) EQ 1) then begin
     radec = [ sixty( rasc,/RA ), sixty( dec ) ]
     format = "(I6,' : ',I2,' : ',F6.3, I6,' : ',I2,' : ',F5.2)"
     print," current RA and DEC =", string( radec, FORM=format )
     print," enter return to use current values, or"
  endif

READ:
   input = ""
   print," enter RA and DEC as  HR,MIN,SEC,  DEG,MIN,SEC"
   read,input

   if (input NE "") then radec = float( get_words( input,DELIM=[","," ",":"] ) )

   CASE N_elements( radec ) OF 
	2: BEGIN 
		rasc = radec(0)
		dec = radec(1)
	     END
	6: BEGIN
		rasc = ten( radec(0:2), /RA )
		dec = ten( radec(3:5) )
	     END
     else: BEGIN
		print,string(7b)," ERROR - Illegal Format"
		goto, READ
	     END
    ENDCASE 
end
