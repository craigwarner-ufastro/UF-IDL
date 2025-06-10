pro write_images, images, waveLens, FORMATTED=form, F77=f77, $
				FITS=fits, NAMES=names
;+
; NAME:
;	write_images
; PURPOSE:
;	Write a 3-D array of images to either plain binary file,
;	F77 binary file, FORMATTED file, or FITS file.
; CALLING:
;	write_images, images, waveLens
; INPUTS:
;	images = 3-D array of data:  array( nx, ny, Nimage )
;	waveLens = vector of wavelenghs corresponding to images
; KEYWORDS:
;	/FITS : write to FITS file, put wavelengths and names in header.
;	/F77 : write to F77 binary file, put wavelengths in first record.
;	/FORMAT : write formatted to file, put wavelengths in first record.
;	NAMES = string array of names corresponding to images
; OUTPUTS:
;	none.
; EXTERNAL CALLS:
;	pro mkhdr
;	pro sxaddpar
;	pro writefits
;	function Trp3D	(not used for /FITS)
; HISTORY:
;	Frank Varosi NASA/GSFC 1991.
;	F.V.1993, added /FITS option.
;-
	filename = ""
	read," output filename ? ",filename

	if keyword_set( fits ) then begin

		filename = filename + ".fits"
		mkhdr, fhdr, images
		if N_elements( waveLens ) GT 0 then $
			for i=0,N_elements(waveLens)-1 do $
			sxaddpar, fhdr, "waveLen" + strtrim(i+1,2), waveLens(i)
		if N_elements( names ) GT 0 then $
			for i=0,N_elements(names)-1 do $
			sxaddpar, fhdr, "name-" + strtrim(i+1,2), names(i)
		writefits, filename, images, fhdr
		print," 3-D stack of images written to: ",filename
		return

	 endif else if keyword_set( form ) then begin
		filename = filename + ".form"
		f77=0
	  endif else if keyword_set( f77 ) then begin
		filename = filename + ".F77"
		f77=1
	   endif else begin
		filename = filename + ".unf"
		f77=0
	    endelse

	openw, Lun, filename, /GET_LUN, F77=f77
	print," creating file: ",filename

	images = Trp3D( images, [3,1,2] )
	sim = size( images )
	Nstack = sim(1)
	Nwave = N_elements( waveLens )
	print,sim(1:sim(0))

	if keyword_set( form ) then begin

		printf, Lun, sim, FORM="(7I10)"
		if (Nwave GT 0) then $
		  printf, Lun, waveLens, FORM="(" + strtrim( Nwave, 2 ) +"F8.1)"
		format = "(" + strtrim( Nstack, 2 ) + "G10.3)"
		print," writing data in format: ",format

		for j=0,sim(3)-1 do begin
		    for i=0,sim(2)-1 do printf, Lun, images(*,i,j), FORM=format
		  endfor

	 endif else if keyword_set( f77 ) then begin

		writeu, Lun, sim
		writeu, Lun, waveLens

		for j=0,sim(3)-1 do begin
			for i=0,sim(2)-1 do  writeu, Lun, images(*,i,j)
		  endfor

	  endif else begin

		writeu, Lun, sim
		writeu, Lun, waveLens
		writeu, Lun, images
	   endelse

	print," 3-D stack of images written to: ",filename
	free_Lun,Lun
end
