;+
; NAME:
;	Force_Events
;
; PURPOSE:
;	To set timers on widgets that match the requested values,
;	causing them to generate events in sequence.
;
; CALLING:
;	Force_Events, widget_map, event_values
;
; INPUTS:
;	widget_map = array of structures with at least the following tags:
;		{ id:0L, value:"" } for each widget of interest.
;		This can be obtained from function widget_Tree_Map.
;
;	event_values = array of strings, corresponding to widget values.
;			The matches need not be exact but must be unique.
;
; KEYWORD:
;	TIMER_INCREMENT = seconds between each sequential event, default = 0.1.
;
; OUTPUT:	none.
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

pro Force_Events, widget_map, event_values, TIMER_INCREMENT=tinc

	if N_struct( widget_map ) LE 0 then return
	if N_elements( tinc ) NE 1 then tinc = 0.1

	sz = size( event_values )
	if sz(sz(0)+1) NE 7 then return

	for i = 0, N_elements( event_values )-1 do begin

		w = where( strpos( widget_map.value, event_values(i) ) GE 0, n )

		if (n EQ 1) then $
		   widget_control, widget_map(w(0)).id, BAD=bid, TIM=tinc*(i+1)
	  endfor
end
