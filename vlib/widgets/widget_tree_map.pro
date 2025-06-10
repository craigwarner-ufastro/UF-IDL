;+
; NAME:
;	widget_Tree_Map
;
; PURPOSE:
;	To get id's and info of all children of a widget (base),
;	returned as structure. If a child is a base or has children, 
;	function is called recursively, thus giving a map of the widget tree.
;
; CALLING:
;	wmap = widget_Tree_Map( base_id )
;
; INPUT:
;	base_id = integer, id of a widget that has children, usually a base.
;		If not given,
;		function returns empty structure for use as template.
;
; OUTPUT:
;	Function returns an array of structures with tags for widget id's,
;	parent id's, type flags, values and user values (if they exist and
;	are strings):
;		{ id:0L, parent:0L, type:-1, value:"", uvalue:"" }
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1993.
;-

function widget_Tree_Map, base_id

	wdesrc = { WIDGET_MAP_v1, id:0L, parent:0L, type:-1,value:"",uvalue:"" }
	wmap = wdesrc

	if N_elements( base_id ) LE 0 then return, wmap

	base_id = Long( base_id(0) )
	wmap.id = base_id

	if NOT widget_info( base_id, /VALID ) then begin
		message,"widget base id is INVALID" + string(7b),/INFO
		return, wmap
	   endif

	wmap.type = widget_info( base_id, /TYPE )
	child_id = widget_info( base_id, /CHILD )

	while (child_id GT 0) do begin
		wdc = wdesrc
		wdc.id = child_id
		wdc.type = widget_info( child_id, /TYPE )
		wmap = [ wmap, wdc ]
		child_id = widget_info( child_id, /SIBLING )
	  endwhile

	if N_elements( wmap ) LE 1 then begin
		message,"widget base has NO children!" + string(7b),/INFO
		return, wmap
	   endif

	wmap = wmap(1:*)
	wmap.parent = base_id
	wo = where( (wmap.type NE 0) AND (wmap.type NE 6), nonbase )
	has_child = bytarr( nonbase > 1 )

	for i=0,nonbase-1 do begin
		iw = wo(i)
		widget_control, wmap(iw).id, GET_VAL=value, GET_UVAL=uval
		sv = size( value )
		su = size( uval )
		if sv(sv(0)+1) EQ 7 then wmap(iw).value = value(0)
		if su(su(0)+1) EQ 7 then wmap(iw).uvalue = uval(0)
		has_child(i) = widget_info( wmap(iw).id,/CHILD ) GT 0
	  endfor

	w = wo( where( has_child, npar ) > 0 )
	for i=0,npar-1 do wmap = [ wmap, widget_Tree_Map( wmap(w(i)).id ) ]

	w = where( wmap.type EQ 0, nbase )
	for i=0,nbase-1 do wmap = [ wmap, widget_Tree_Map( wmap(w(i)).id ) ]

return, wmap
end
