;+
; NAME:
;	it_get_color
; PURPOSE:
;	Returns the color index associated with an item,
;	so that:
;			plot,x,y,COLOR = it_get_color( "data" )
;	will make a graph using with cyan lines/symbols.
;
;	The first 7 default items are the colors themselves, so that:
;			plot,x,y,COLOR = it_get_color( "green" )
;	will make a graph using with green lines/symbols.
;
;	Item colors are assigned with pro it_set_color,
;	and pro Seven_Colors must be called first.
;
; CALLING:
;	color_index = it_get_color( item, color )
;
; INPUTS:
;	item = string, users name of item (default = "white")
;
; OUTPUTS:
;	color = string (optional), the name of primary color.
;
;	Function returns the index in lookup color table associated with item.
;	If item is not found then the index for white is returned.
;
; COMMON BLOCKS:
;	common seven_color_it, it_color
; EXTERNAL CALLS:
;	function N_struct
; PROCEDURE:
;	Straightforward.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

function it_get_color, item, color

   common seven_color_it, it_color

	if N_struct( it_color ) LE 0 then seven_colors

        if N_elements( item ) NE 1 then item="white"
        if( strlowcase( item ) eq 'black') then return,0

	wi = where( it_color.name EQ strlowcase( item ), ni )
	if (ni LE 0) then wi=6  else wi=wi[0]
	color = it_color[wi].color

return, it_color[wi].index
end
