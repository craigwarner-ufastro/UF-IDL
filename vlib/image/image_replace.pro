;+
; NAME:
;	image_replace
;
; PURPOSE:
;	Replace an image (or array of any dimensions or type)
;	in pool-array (a 1-D array for storing data) and update pointers.
;	The image (array) must have been inserted using pro image_insert.
;	In this way, many different sized images can be stored in single array.
;
; CALLING:
;	image_replace, image, inum, im_pool, im_sizes
;
; INPUTS:
;	image = array of same size & type as already inserted into im_pool.
;
;	inum = integer, the insertion slot # of image in pool-array.
;
;	im_pool = 1-D pool-array for storing data using pointers.
;
;	im_sizes = 2-D array, each row contains the standard IDL size info
;		plus pointer into pool-array (beginning and ending Locations).
;
;	Variables im_pool & im_sizes are created / modified by pro image_insert
;	and pro image_remove, and should NOT be altered any other way.
;
; OUTPUTS:
;	im_pool : 1-D pool-array contains new data.
;
; RELATED ROUTINES:
;	pro image_insert
;	pro image_remove
;	function image_extract
; PROCEDURE:
;
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	F.V. 1990 modified to handle > 2-D image.
;	F.V. 1994 modified to handle data of any mixed types.
;-

pro image_replace, image, inum, im_pool, im_sizes

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
	simr = size( image )
	w = where( sim(0:2) - simr(0:2), ndif )

	if (ndif GT 0) then begin
		message,"cannot fit image into slot #"+strtrim(inum,2),/INFO
		print, sim, simr
		return
	  endif

	s = size( im_pool )		;re-insert it at same Loc.
	im_sizes(0:3,inum) = simr(0:3)

	im_pool(sim(Lpim)) =  map_var_type( image, TY=s(s(0)+1) )
end
