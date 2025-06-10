;+
; NAME:
;	image_extract
;
; PURPOSE:
;	Extract an image (or array of any dimensions or type)
;	from pool-array (a 1-D array for storing data) using pointers.
;	The image (array) must have been inserted using pro image_insert.
;	In this way, many different sized images can be stored in single array,
;	and there is no type conversion, just straight memory mapping.
;
; CALLING:
;	image = image_extract( inum, im_pool, im_sizes )
;
; INPUTS:
;	inum = integer, the insertion slot # of image in pool-array.
;
;	im_pool = 1-D pool-array for storing data using pointers,
;
;	im_sizes = 2-D array, each row contains the standard IDL size info
;		plus pointer into pool-array (beginning and ending Locations).
;
;	Variables im_pool & im_sizes are created/modified by pro image_insert
;	and pro image_remove, and should NOT be altered any other way.
;
; OUTPUTS:
;	Function returns the image (array) requested.
;
; RELATED ROUTINES:
;	pro image_insert
;	pro image_remove
;	pro image_replace
; EXTERNAL CALLS:
;	function map_var_type
; PROCEDURE:
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1990 modified to handle > 2-D image.
;	F.V. 1994 modified to handle data of any mixed types.
;-

function image_extract, inum, im_pool, im_sizes

	if (inum LT 0) then return, replicate(-1,2,2)

	ss = size( im_sizes )
	Lpim = ss[1]-2		;Location of pointer in each row of im_sizes.
	Maxim = ss[2]-1

	if (inum GT Maxim) then begin
		message,"requested slot # " + strtrim(inum,2) + $
			" does not exist, MAX=" + strtrim(Maxim,2),/INFO
		return, replicate(-1,2,2)
	   endif

	sim = im_sizes(*,inum)
	Ndim = sim[0]
	Lsiz = Ndim+2
	Npix = sim[Lsiz]

	if (Npix LE 0) then begin
		message,"requested slot # " + strtrim(inum,2) +" is empty",/INFO
		return, replicate(-1,2,2)
	   endif

	start = sim[Lpim]
	Last = sim[Lpim+1]
	vtype = sim[sim[0]+1]

return, reform( map_var_type( im_pool[start:Last], TYPE=vtype ), sim[1:ndim] )
end
