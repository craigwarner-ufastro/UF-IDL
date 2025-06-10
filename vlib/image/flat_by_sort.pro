function flat_by_sort, image, imback, TRANSPOSE=transp, MIN_SORT=mins
;+
; NAME:
;	flat_by_sort
; PURPOSE:
;	Estimate sky or row/column pattern background in an image by
;	sorting rows (or columns) and then subtract the background
;	to return flat fielded image.
; CALLING EXAMPLE:
;	image_flat = flat_by_sort( image, imback, /TRANSPOSE, MIN_SORT=9 )
; INPUTS:
;	image = 2D array of data
; KEYWORDS:
;	/TRANSPOSE : work on transpose so columns are sorted to get background.
;	MIN_SORT = index of which element of sorted row to use at background,
;		MIN=1 causes 1st value above minimum to be the background,
;		MIN = #pixels_per_row/2 causes median to be used,
;		MIN=0 (default) causes the first statistically favorable value
;			above minimum to be selected as the row background.
; OUTPUTS:
;	imback = the estimated sky or row/column pattern background.
; RESULT:
;	Function returns the flat fielded image (background subtracted).
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;-
	sim = size( image )
	
	if (sim(0) NE 2) then begin
		message,"expecting an image (2-D matrix)",/INFO
		return, image
	   endif

	if keyword_set( transp ) then imback = transpose( image ) $
				 else imback = image
	sim = size( imback )
	nx = sim(1)
	ny = sim(2)
	nx2 = nx/2

; reorder pixels within rows into ascending order

	for i=0,ny-1 do imback(*,i) = imback( sort( imback(*,i) ), i )

; now estimate the background:

	if keyword_set( mins ) then begin	;use m-th value of each row

		m = ( (mins-1) > 0 ) < nx2
		for i=0,ny-1 do imback(*,i) = imback(m,i)

	  endif else begin	;use first statistically favorable value

		for i=0,ny-1 do begin
			row = imback(0:nx2,i)
			rowd = row(1:*) - row
			w = where( rowd LT stdev( rowd ) )
			imback(*,i) = imback(w(0),i)
		  endfor
	   endelse

	if keyword_set( transp ) then imback = transpose( imback )

return, image - imback
end
