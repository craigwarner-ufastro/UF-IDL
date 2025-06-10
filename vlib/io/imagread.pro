function imagread ,file ,header ,Kimag, V1=v1

;Read image # Kimag from file.
; result is image data, and  header is returned :
;	[ #X_pixels, #Y_pixels, IDL_data_type, #extra_header_recs, #images-1 ]
; Frank Varosi, U.of MD., 1988
; F.V. 1990, mod. for IDL-V2,
; and added keywork /V1 to read V1 type files.
;(reminder: should use make_array instead of case statement).

	if (N_params(0) LT 3) then  Kimag=0

	get_Lun,Lun
	openr ,Lun ,file

	record = assoc (Lun,lonarr(128))
	if keyword_set( v1 ) then header = conv_vax_unix( record(0) ) $
			     else header = record(0)
;;	print,header(0:5)
	Nx = header(0)
	Ny = header(1)
	ftyp = header(2)
	Nheader = header(3)
	Lim = header(4)
	Nhrbyt = (1 + Nheader)*512

	if (Kimag GT Lim) then begin
		print,' Last image is #',Lim
		Kimag = Lim
	endif

	if keyword_set( v1 ) then ftyp = vmscode( ftyp, /FROM_V1VMS )

	if (Ny EQ 1) then begin

		CASE ftyp OF

		1:	imagfil = assoc( Lun ,bytarr(Nx) ,Nhrbyt )
		2:	imagfil = assoc( Lun ,intarr(Nx) ,Nhrbyt )
		3:	imagfil = assoc( Lun ,lonarr(Nx) ,Nhrbyt )
		4:	imagfil = assoc( Lun ,fltarr(Nx) ,Nhrbyt )

		ELSE:	print,' Image type not supported'
		ENDCASE

	 endif else begin

		CASE ftyp OF

		1:	imagfil = assoc( Lun ,bytarr(Nx,Ny) ,Nhrbyt )
		2:	imagfil = assoc( Lun ,intarr(Nx,Ny) ,Nhrbyt )
		4:	imagfil = assoc( Lun ,fltarr(Nx,Ny) ,Nhrbyt )
		3:	imagfil = assoc( Lun ,lonarr(Nx,Ny) ,Nhrbyt )

		ELSE:	print,' Image type not supported'
		ENDCASE

	  endelse

	image = imagfil(Kimag)

	free_Lun ,Lun

return ,image
end
