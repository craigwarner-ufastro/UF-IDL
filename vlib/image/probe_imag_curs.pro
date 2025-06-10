;+
; NAME:
;	probe_imag_curs
; PURPOSE:
;	Display the values in probe_image profiles at cursor location.
; CALLING:
;	probe_imag_curs
; INPUTS:
;	No explicit inputs, all info in common blocks.
; OUTPUTS:
;	No explicit outputs, info is written to profile plot window.
; EXTERNAL CALLS:
; 	function Trp3D
; 	function strconcat
; 	function intersection
; COMMON BLOCKS:
;	common probe_image, winprof
;	common probe_image3, save_P, save_X, save_Y
; PROCEDURE:
;	Loop over cursor query code.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

pro probe_imag_curs

  common probe_image, winprof
  common probe_image3, save_P, save_X, save_Y

  if N_struct( save_P ) LE 0 then begin
     message,/INFO,"save_P structure for plot info does not exist"
     return
  endif


	ppos = Trp3D( reform( save_P.clip[0:3], 2,2,2 ), [2,1,3] )
	xran = transpose( save_X.crange )
	xscl = ( xran[*,1] - xran[*,0] ) / reform( ppos[1,0,*] - ppos[0,0,*] )
	yran = transpose( save_Y.crange )
	yscl = ( yran[*,1] - yran[*,0] ) / reform( ppos[1,1,*] - ppos[0,1,*] )
	scl = transpose( [[xscl],[yscl]] )
	rmin = transpose( [[xran[*,0]],[yran[*,0]]] )
	winimage = !D.window
	wset, winprof
	wshow, winprof, ICON=0
	zline = bytarr( !D.x_ch_size * 40, !D.y_ch_size * 2 )
	tvcrs,0.5,0.5,/NORM

	REPEAT BEGIN

		cursor,x,y, /DEV, /CHANGE
		cxy = [[x,x],[y,y]]

		if intersection( ppos[*,*,0], cxy, rpos ) then begin

			rpos = scl[*,0] * reform( rpos[0,*] ) + rmin[*,0]
			if (save_Y[0].type) then rpos[1] = 10^rpos[1]
			x = ppos[0,0,0]
			y = ppos[1,1,0] + 2
			tv, zline, x,y
			xyouts, x, y+2, /DEV,FONT=0, strconcat( string(rpos) )

		 endif else if intersection( ppos[*,*,1], cxy, rpos ) then begin

			rpos = scl[*,1] * reform( rpos[0,*] ) + rmin[*,1]
			if (save_Y[1].type) then rpos[1] = 10^rpos[1]
			x = ppos[0,0,1]
			y = ppos[1,1,1] + 2
			tv, zline, x,y
			xyouts, x, y+2, /DEV,FONT=0, strconcat( string(rpos) )
		  endif
		
	   ENDREP UNTIL (!mouse.button EQ 4)

	wset, winimage
	wshow, winimage
end
