;+
; NAME:
;	wmenux
; PURPOSE:
;	Select from a widget-List type of menu if device supports widgets,
;	or the old "wmenu" if SUN device, or from numerical list if neither.
; CALLING:
;	sel = wmenux( menu, INIT=init, TIT=0, XPOS=xpos, YPOS=ypos )
; INPUTS:
;	menu = string array of items to select from.
; KEYWORDS:
;	INITIAL_SELECTION = default is 0.
;	INSTRUCTIONS = string, displayed as window title,
;			default = "Select item from List:"
;	TITLE = string, for title of List,
;		or index in menu of list title, usually 0, default is none.
;	FONT = font to use for text in widget (default = '9x15bold')
;	XPOS, YPOS = x,y Location on device screen, default is top-center.
;	WINDOW = window number to place menu next to (overrides XPOS & YPOS).
;	MLIST = max # items to fit in 1 column, otherwise use 2	columns (default = 42)
;	/NO_SELECT : creates a button at top that when pressed aborts the selection.
;	/NO_DEFAULT : implies /NO_SELECT and focusses the pointer on button.
; OUTPUTS:
;	Function returns index of selected item in menu array.
; EXTERNAL CALLS:
;	pro widget_Location
;	pro hprint
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;	FV @ UF 2011 added feature that make 2 columns if list is long.
;	FV @ UF 2020 fixed bug with 2 columns selection in 2nd column.
;-

function wmenux, menu, INITIAL_SELECTION=sinit, TITLE=title, NO_DEFAULT=nodef, WINDOW=mwind, $
                 INSTRUCTION=instruct,XPOS=xpos,YPOS=ypos,NO_SELECT=nosel,MLIST=maxList,FONT=font

   common wmenux, old_wmenu

   if keyword_set( old_wmenu ) then begin
      if (!D.name eq "X") or (!D.name eq "SUN") then begin
         wait,0.1
         return, wmenu( menu, INIT=sinit, TIT=title )
      endif else begin
         hprint, string( indgen( N_elements( menu ) ) ) + " " + menu
         read,"select by number: ", sel
         return, fix( sel )
      endelse
   endif

   if N_elements( mwind ) eq 1 then begin
      if( mwind ge 0 ) then begin
         owind = !D.window
         wset, mwind
         device,GET_WINDOW_POS=wpos
         xpos = (wpos[0] - 100) > 0
         ypos = (!DEVY - wpos[1] - 3*!D.y_vsize/4) > 0
         if( owind ge 0 ) then wset, owind
      endif
   endif

	if N_elements( ypos ) NE 1 then ypos = 100
	if N_elements( sinit ) NE 1 then sinit=0
	sz = size( menu )
	if sz[sz[0]+1] NE 7 then return,(-1)

	if (!D.flags AND 65536) NE 0 then begin

		if N_elements( instruct ) NE 1 then instruct = "Select item from List:"
		base = widget_base( TIT=instruct, XOFF=!DEVX/4, YOFF=!DEVY/4, /COLUMN )

		if keyword_set( nosel ) OR keyword_set( nodef ) then $
			abort_id = widget_button( base, VAL="NO selection" ) $
		  else  abort_id = -1

		if N_elements( title ) EQ 1 then begin
                   st = size( title )
                   if st[st[0]+1] NE 7 then begin
                      inxz = 1
                      Lab = widget_Label( base, VAL=menu[0] )
                      menu1 = menu[1:*]
                   endif else begin
                      inxz = 0
                      Lab = widget_Label( base, VAL=title )
                      menu1 = menu
                   endelse
                endif else begin
                   inxz = 0
                   menu1 = menu
                endelse

                if N_elements( font ) NE 1 then begin
                   if( !D.name eq "X") then font = '9x15bold'
                endif

                if N_elements( maxList ) NE 1 then maxList=42
                maxList = maxList < 42
		Lysiz = sz[sz[0]+2]

		if( Lysiz gt maxList ) then begin
                   base2 = widget_base( base, COLUMN=2,/GRID_LAYOUT )
                   Lysiz = Lysiz/2
                   menu2 = menu1[Lysiz:*]
                   menu1 = menu1[0:Lysiz-1]
                   Lid = widget_List( base2, VALUE=menu1, YSIZ=Lysiz<maxList, FONT=font )
                   Lid2 = widget_List( base2, VALUE=menu2, YSIZ=Lysiz<maxList, FONT=font )
                endif else Lid = widget_List( base, VALUE=menu1, YSIZ=Lysiz, FONT=font )

		widget_control, base,/REALIZE
		widget_Location, base, XPOS=xpos, YPOS=ypos

                if N_elements( Lid2 ) eq 1 and (sinit-inxz) gt Lysiz then begin
                   widget_control, Lid2, SET_LIST_SEL=sinit-Lysiz-inxz,/INPUT_FOCUS
                endif else widget_control, Lid, SET_LIST_SEL=sinit-inxz,/INPUT_FOCUS

		if keyword_set( nodef ) then widget_control, abort_id,/INPUT_FOCUS

		event = widget_event( base )
		widget_control, base,/DESTROY
		wait,0.1

		if (event.id EQ abort_id) then return,(-1) else begin
                   ioff = inxz
                   if N_elements( Lid2 ) eq 1 then begin
                      if (event.id EQ Lid2) then ioff += Lysiz
                   endif
                   return, event.index + ioff
                endelse

	 endif else if !D.name EQ "SUN" then begin

		if N_elements( title ) EQ 1 then $
			return, wmenu( menu, INIT=sinit, TIT=title ) $
		  else	return, wmenu( menu, INIT=sinit )

	  endif else begin

		hprint, string( indgen( N_elements( menu ) ) ) + " " + menu
		read,"select by number: ", sel
		return, fix( sel )
	   endelse
end
