pro imagwrit ,image ,Outfile ,Nheader, V1=v1

;Create file and write image
; First header records is always:
;	[ #X_pixels, #Y_pixels, IDL_data_type, #extra_header_recs, #images-1 ]
; (specify  #extra_header_recs = Nheader for later use).
;Frank Varosi U.MD.1988
; F.V. 1990, mod. for IDL-V2,
; and added keywork /V1 to write header so V1 routines can read file.
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

	get_Lun,Lun
	openw ,Lun ,Outfile

	if (N_elements(Nheader) LE 0) then Nheader = 0
	record = assoc (Lun,lonarr(128))
	header = [ s(1:3) , Nheader ,0 ]

	if keyword_set( v1 ) then begin
		header(2) = vmscode( header(2) )
		print,header
		conv_unix_vax, header
	  endif else print,header

	record(0) = header
	ityp = s(3)
	Nhrbyt = (1 + Nheader)*512

	CASE ityp OF

	1: 	imagfil = assoc ( Lun ,bytarr(s(1),s(2)) ,Nhrbyt )
	2: 	imagfil = assoc ( Lun ,intarr(s(1),s(2)) ,Nhrbyt )
	3: 	imagfil = assoc ( Lun ,lonarr(s(1),s(2)) ,Nhrbyt )
	4: 	imagfil = assoc ( Lun ,fltarr(s(1),s(2)) ,Nhrbyt )
	5: 	imagfil = assoc ( Lun ,dblarr(s(1),s(2)) ,Nhrbyt )

	ELSE:	begin
			print,' Image must be byte/int/float/long/dbl'
			goto,quit
		end
	ENDCASE

	imagfil(0) = image

quit:	free_Lun ,Lun

return
end
