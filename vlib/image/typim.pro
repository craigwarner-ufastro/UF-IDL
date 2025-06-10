function typim, nx, ny, LONG_ARRAY=Long
;+
; NAME:
;	typim
; PURPOSE:
;	Read values of all image pixels from keyboard entries.
;
; CALLING:
;	image = typim( nx, ny )
; INPUTS:
;	nx, ny = desired size of image, # rows, # columns.
;
; KEYWORDS:
;	/LONG_ARRAY causes result to be a longword array, default is floating.
;
; EFFECTS:
;	User is prompted for each pixel by (i,j) = (row,column).
;	Function returns the entries as a matrix
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1991.
;-
	if keyword_set( Long ) then begin
		data = 0L
		image = Lonarr( nx, ny )
	  endif else image = fltarr( nx, ny )

	for j=0,ny-1 do begin

		for i=0,nx-1 do begin

			print," pixel pos:",i+1,j+1,FORM="($,(A),2i4)"
			read,"   ? ",data
			image(i,j) = data
		   endfor
	   endfor

return, image
end
