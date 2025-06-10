;+
; NAME:
;	view_FITS_header
; PURPOSE:
;	Display in a text widget the strings of a FITS header.
; CALLING:
;	view_FITS_header, header, XPOS=xpos, YPOS=ypos
; INPUTS:
;	header = string array of items to display.
; KEYWORDS:
;	XPOS, YPOS = x,y Location on device screen, default is top-center.
;	WINDOW = window number to place menu next to (overrides XPOS & YPOS).
; OUTPUTS:
;	none.
; EXTERNAL CALLS:
;	pro widget_Location
;	pro hprint
; HISTORY:
;	Written: Frank Varosi at UF, Jan. 2012.
;-

pro view_FITS_header, header, TITLE=title, WINDOW=mwind, XPOS=xpos, YPOS=ypos, STRUCT=struct, WBASE=wbase

   if N_struct( struct ) gt 0 then begin

      tags = tag_names( struct )
      if max( tags eq "NAME" ) then title = struct[0].name
      if max( tags eq "WINAME" ) then title = struct[0].winame
      if max( tags eq "WINDO" ) then mwind = struct[0].windo

      if max( tags eq "PFHD" ) then begin
         if Ptr_Valid( struct[0].pfhd ) then begin
            view_FITS_header, *struct[0].pfhd, TITLE=title, WINDOW=mwind, XPOS=xpos, YPOS=ypos
            return
         endif  else message,/INFO,"FITS header not in structure: NULL pointer."
      endif else message,/INFO,"FITS header not in structure."
      if N_elements( header ) LE 0 then return
   endif

   if N_elements( mwind ) eq 1 then begin
      if( mwind ge 0 ) then begin
         owind = !D.window
         wset, mwind
         device,GET_WINDOW_POS=wpos
         xpos = fix(wpos[0] - 1.6*81*!D.x_ch_size) > 0
         ypos = 0
         if( owind ge 0 ) then wset, owind
      endif
   endif

   sz = size( header)

   if sz[sz[0]+1] NE 7 then begin
      message,/INFO,"expecting an array of strings."
      return
   endif

   if (!D.flags AND 65536) NE 0 then begin

      if N_elements( title ) ne 1 then title="FITS header"
      base = widget_base( TIT=title, XOFF=!DEVX/4, YOFF=!DEVY/4, /COLUMN )
      wbase = base

      wid = widget_text( base, VALUE=header, XSIZ=81, YSIZ=55 ,/SCROLL )
      widget_control, base,/REALIZE

      if N_elements( xpos ) eq 1 and N_elements( ypos ) eq 1 then $
         widget_Location, base, XPOS=xpos, YPOS=ypos

   endif else hprint, header

end
