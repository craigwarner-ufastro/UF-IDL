;+
; NAME:
;	extract_region
;
; PURPOSE:
;	Interactively extract a sub-region of image array, using mouse.
;	Optionally, sub-region can be specified by entering position & size.
;	To active this option, press the MIDDLE mouse button first, then
;	widget will appear into which region position & size can be typed.
;
; CALLING:
;	imsub = extract_region( image )
;
; INPUTS:
;	image = 2D array.
;
; KEYWORDS:
;	VALUE_FILL = value to use in filling empty pixels of result, default=0.
;	SIZE_REGION = 2 integers, desired size of extracted region,
;			default is variable (interactive).
;	MAGNIFICATION = magification factor of displayed image.
;	WINDOW = IDL window number for displayed image.
;	WPOS_XY = 2 integers, position of displayed image in window coordinates.
;
;	/LIMIT_REGION : selection of region is limited to actual image size.
;	/ERASE_BOX : erase the region selection box upon completion (def=no).
;	/DISPLAY : automatically create a window and display the image.
;	/LOG10 : display ( image > 1 ) in Log base 10 scaling.
;	LOG10 = ( image > LOG10 ) is displayed in Log base 10 scaling.
;
; KEYWORD OUTPUTS:
;	SIZE_REGION = 2 integers, actual size of extracted region.
;	START_XY = 2 integers, begining Location of extracted region.
;	LAST_XY = 2 integers, ending Location of extracted region.
;
; OUTPUT:
;	Function returns the extracted image array,
;	which is a subset, or possibly an extension, of input image.
;
; EXTERNAL CALLS:
;	pro get_window
;	pro box_cursor
;	function box_create
;	pro box_erase
; COMMON BLOCKS:
;	common extract_region, xy_region	;saved position & size.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1992.
;	F.V. 1994: options to fix sub-region and edit position & size.
;	F.V. 2014: magnification factor computed from screen size.
;-

function extract_region, image, SIZE_REGION=sxy, START_XY=b, LAST_XY=L, $
			MAGNIFICATION=magf, LOG10=Log10, VALUE_FILL=minim,  $
			LIMIT_REGION=Limit, ERASE_BOX=erase_box, $
			DISPLAY_IMAGE=dispim, WINDOW=wtmp, WPOS_XY=wpos

   common extract_region, xy_region

	sim = size( image )
	sim = sim[1:2]

	if N_elements( sxy ) LE 0 then begin
		sxy = sim
		fixed = 0
	  endif else begin
		if N_elements( sxy ) EQ 1 then sxy = [sxy,sxy]
		fixed = 1
	   endelse

	if keyword_set( dispim ) then begin

		sizw = sim > sxy
		if N_elements( magf ) NE 1 then begin
			magf = min( float([!DEVX,!DEVY])/sizw ) > 0.1
			if (magf LT 1) then begin
                           redf = ceil( 1/magf )
                           magf = 1.0/redf
                        endif else magf = fix( magf )
		   endif
		sizw = sizw * magf
		get_window, wtmp, XSIZ=sizw[0], YSIZ=sizw[1], $
				TITLE="Select subregion:"
		wpos = ( ( (sxy-sim) > 0 ) * magf )/2
		tvs, image, wpos[0], wpos[1], MAG=magf, LOG=Log10

	  endif else if N_elements( wtmp ) EQ 1 then begin

		wset,wtmp
		wshow,wtmp,ICON=0
	   endif

	if N_elements( magf ) NE 1 then begin
		message,"using default Magnification = 1",/INFO
		magf=1
	   endif

	if N_elements( wpos ) NE 2 then wpos = [0,0]

	if (fixed) then begin

		b = ( sim/2 - sxy/2 ) * magf
		bx=b[0] & by=b[1]
		s = sxy * magf
		box_cursor, bx,by, s[0],s[1], /ADJACENT,/CONTIN,/INIT, $
					KEEP=(keyword_set( erase_box ) NE 1)
		b = ( [bx,by] - wpos )/magf
		L = b + sxy -1

	  endif else begin

		if N_struct( xy_region ) NE 1 then $
		   xy_region = { x_min:0, y_min:0, x_size:9, y_size:9, mode:0 }

		bstat = 0
		tvcrs,0.5,0.5,/NORM
		print," "

	SELECT:	if (xy_region.mode EQ 1) then begin
			message,"in FIXED region mode:",/INFO
			print," Left button   = fixed region extraction"
			print," Middle button = menu options"
			cursor, bx, by,/DEV
			bstat = -!mouse.button
		 endif else begin
			message,"in VARIABLE region mode:",/INFO
			print," select first point with Left button, drag mouse"
			print," then Middle button to define box, Right = retry"
			print," or: Middle for menu options"
			while (abs(bstat) NE 2) do $
				bstat = box_create( bx, by, Tx, Ty )
		  endelse

		if (bstat EQ -2) then begin

			cursor, bx, by,/DEV,/NOWAIT
			bstat = 0
			menu = ["Options:", " "		,$
				"Fix xy-region"		,$
				"Edit xy-region", " "	,$
				"Un-fix: keep xy-region",$
				"Un-fix: variable xy-region"]
			task = menu[ wmenux( menu,INIT=2,TIT=0 ) > 0 ]

			CASE next_word( task ) OF
			  "Fix":	xy_region.mode = 1
			  "Edit": BEGIN
					X_Var_Edit, xy_region
					xy_region.mode = 1
				END
			  "Un-fix:": BEGIN
					CASE next_word( task ) OF
					  "variable":	xy_region.mode = 0
					  "keep":	xy_region.mode = -1
					 ENDCASE
				END
			  else:
			 ENDCASE

			tvcrs, bx, by,/DEV
			goto,SELECT
		   endif

		if (xy_region.mode EQ 1) then begin
			b = [ xy_region.x_min, xy_region.y_min ]
			sxy = [ xy_region.x_size, xy_region.y_size ]
			L = sxy + b -1
			box_draw, POS=b*magf+wpos, SIZE=sxy*magf
		 endif else begin
			b = ( [bx,by] - wpos )/magf
			L = ( [Tx,Ty] - wpos )/magf
			sxy = L-b+1
			if (xy_region.mode EQ 0) then begin
				xy_region.x_min = b[0]
				xy_region.y_min = b[1]
				xy_region.x_size = sxy[0]
				xy_region.y_size = sxy[1]
			   endif
		  endelse

		if keyword_set( erase_box ) then begin
			wait,0.2
			box_erase
		   endif
	   endelse

	if keyword_set( dispim ) then wdelete,wtmp
	if keyword_set( Limit ) then sxy = sxy < sim

	b = round( b > 0 ) < (sim-1)
	L = round( L > 0 ) < (sim-1)

	if max( sim LT sxy ) then begin

		if N_elements( minim ) NE 1 then minim=0
		imextract = make_array( DIM=sxy, /FLOAT, VAL=minim )
		sid = ( (L-b+1) - sim ) > 0
		L = L - sid
		sim = L-b+1
		Loc = ( sxy/2 - sim/2 ) > 0
		imextract(Loc[0],Loc[1]) = image(b[0]:L[0],b[1]:L[1])
		return, imextract

	   endif else  return, image(b[0]:L[0],b[1]:L[1])
end
