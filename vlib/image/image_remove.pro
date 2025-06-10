;+
; NAME:
;	image_remove
;
; PURPOSE:
;	Remove an image (or array of any dimensions or type)
;	from pool-array (a 1-D array for storing data) and update pointers.
;	The image (array) must have been inserted using pro image_insert.
;
; CALLING:
;	image_remove, inum, im_pool, im_sizes
;
; INPUTS:
;	inum = integer, the insertion slot # of image in pool-array.
;
;	im_pool = 1-D pool-array for storing data using pointers.
;
;	im_sizes = 2-D array, each row contains the standard IDL size info
;		plus pointer into pool-array (beginning and ending Locations).
;
;	Variables im_pool & im_sizes are created/modified by pro image_insert
;	and pro image_remove, and should NOT be altered any other way.
;
; OUTPUTS:
;	im_pool : 1-D pool-array is modified.
;
;	im_sizes : info and pointer of inum slot is deleted.
;
; RELATED ROUTINES:
;	pro image_insert
;	pro image_replace
;	function image_extract
; PROCEDURE:
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1990 modified to handle > 2-D image.
;	F.V. 1994 modified to handle data of any mixed types.
;-

pro image_remove, inum, im_pool, im_sizes

	if (inum LT 0) then return

	ss = size( im_sizes )
	Lpim = ss(1)-2		;Location of pointer in each row of im_sizes.
	Maxim = ss(2)-1

	if (inum GT Maxim) then begin
		message,"requested slot # " + strtrim(inum,2) + $
			" does not exist, MAX=" + strtrim(Maxim,2),/INFO
		return
	   endif

	sim = im_sizes(*,inum)
	start = sim(Lpim)
	Lpim1 = Lpim+1
	Last = sim(Lpim1)
	Npix = Last - start + 1
	if (Npix LE 0) OR (sim(sim(0)+2) LE 0) then return

	im_sizes(*,inum) = 0
	Npixall = N_elements(im_pool)

	if Npixall EQ Npix then		im_pool = 0			$
	 else if start LE 0 then	im_pool = im_pool(Last+1:*)	$
	 else if Npixall LE Last+1 then im_pool = im_pool(0:start-1)	$
	 else im_pool = [ im_pool(0:start-1), im_pool(Last+1:*) ]

	w = where( im_sizes(Lpim,*) GT start, Nim )

	for i=0,Nim-1 do im_sizes(Lpim:*,w(i)) = im_sizes(Lpim:*,w(i)) - Npix
end
