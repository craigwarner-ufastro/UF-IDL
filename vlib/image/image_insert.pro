;+
; NAME:
;	image_insert
;
; PURPOSE:
;	Insert an image (or array of any dimensions or type)
;	into pool-array (a 1-D array for storing data) and update pointers.
;	The image (array) can be extracted using function image_extract.
;	In this way, many different sized images can be stored in single array,
;	and there is no type conversion, just straight memory mapping.
;
; CALLING:
;	image_insert, image, inum, im_pool, im_sizes
;
; INPUTS:
;	image = array of any size or type.
;
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
; KEYWORDS:
;	/REPLACE : option to replace an existing image in pool-array.
;
; OUTPUTS:
;	im_pool : 1-D pool-array contains new data.
;
;	im_sizes : updated to contain new info and pointer into pool-array.
;
; RELATED ROUTINES:
;	pro image_remove
;	pro image_replace
;	function image_extract
; EXTERNAL CALLS:
;	pro image_replace
;	function map_var_type
; PROCEDURE:
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1990 modified to handle > 2-D image.
;	F.V. 1994 modified to handle data of any mixed types.
;-

pro image_insert, image, inum, im_pool, im_sizes, REPLACE=replace

	if keyword_set( replace ) then begin
		image_replace, image, inum, im_pool, im_sizes
		return
	   endif

	if (inum LT 0) then return

	ss = size( im_sizes )
	Maxdim = ss(1)-5
	Maxim = ss(2)-1

	if (inum GT Maxim) then begin
		message,"slot # " + strtrim(inum,2) + $
			" > " + strtrim(Maxim,2) + " (MAX allocated)",/INFO
		return
	   endif

	if (im_sizes(0,inum) GT 0) then begin
		message,"slot # " + strtrim(inum,2) + " already occupied",/INFO
		return
	   endif

	sim = size( image )
	Ndim = sim(0)
	Npix = sim(Ndim+2)

	if (Ndim GT Maxdim) then begin
		message,"array dimensionality " + strtrim(Ndim,2) + $
			" > " + strtrim(Maxdim,2) + " (MAX allocated)",/INFO
		return
	   endif

	Loc = N_elements( im_pool )
	if (Loc EQ 1) then Loc = 0		;the case if reset by im_pool=0

	if (Loc LE 1) then  im_pool = reform( image, Npix )  else begin
		s = size( im_pool )
		im_pool = [ im_pool, map_var_type( image, Npix, TY=s(s(0)+1) ) ]
	  endelse

	Pointer = [ Loc, Loc + Npix - 1 ]	;added to end of im_pool.

	if (Ndim LT Maxdim) then begin		;put new pointer in im_sizes.
		zeros = Lonarr( Maxdim - Ndim )
		im_sizes(*,inum) = [ sim, zeros, Pointer ]
	  endif else  im_sizes(*,inum) = [ sim, Pointer ]
end
