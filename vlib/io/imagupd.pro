pro imagupd, image, Outfile, V1=v1

; Add an image to existing file.
; Frank Varosi, U.of MD., 1988.
; F.V. 1990, mod. for IDL-V2,
; and added keywork /V1 to convert to V1 vartype codes (not fully consistent).
;(reminder: should use make_array instead of case statement).

	s = size( image )

	if (s(0) GT 2) then begin
		print," data given is not an image, nothing written"
		return
	   endif

	if (s(0) EQ 1) then begin
		print,' data is a vector, consider as single row matrix'
		s = [ 2, s(1), 1, s(2) ]
	   endif

	Nx = s(1)
	Ny = s(2)
	ityp = s(3)
	if keyword_set( v1 ) then ityp = vmscode( ityp )

	get_Lun ,Lun
	openU ,Lun ,Outfile
	Lrec = 128

	record = assoc( Lun, Lonarr( Lrec ) )
	if keyword_set( v1 ) then header = conv_vax_unix( record(0) ) $
			     else header = record(0)
	Mx= header(0)
	My= header(1)
	ftyp = header(2)
	nheader = header(3)
	Lim = header(4)

	if ( ityp NE ftyp ) then begin
		print,' type of image incompat. with file:',ityp,ftyp
		goto, quit
	endif

	if ( Nx NE Mx ) then begin
		print,'X size of image incompat. with file:',Nx,Mx
		goto, quit
	endif

	if ( Ny NE My ) then begin
		print,'Y size of image incompat. with file:',Ny,My
		goto ,quit
	endif

	Nhrbyt = (1 + Nheader)*512

	CASE ftyp OF

	2:	imagfil = assoc ( Lun ,bytarr(Mx,My) ,Nhrbyt )
	4:	imagfil = assoc ( Lun ,intarr(Mx,My) ,Nhrbyt )
	8:	imagfil = assoc ( Lun ,fltarr(Mx,My) ,Nhrbyt )
	16:	imagfil = assoc ( Lun ,lonarr(Mx,My) ,Nhrbyt )
	ELSE: begin
		print,' Image must be byte/int/float/long'
		goto,quit
	      end
	ENDCASE

	Lim = Lim + 1
	imagfil( Lim ) = image

	header(4) = Lim
	print,header(0:4)
	if keyword_set( v1 ) then conv_unix_vax, header
	record(0) = header

quit:	free_Lun ,Lun

return
end
