;+
; NAME:
;	it_set_color
; PURPOSE:
;	Associate an item with any of the 7 primary colors:
;	"red","yellow","green","cyan","blue","violet","white".
;
;	Examples:
;		it_set_color, "model", "green"
;		it_set_color, "data", "cyan"
;	This info is saved in common block.
;
;	Then function it_get_color returns the color index associated with
;	any item, so that:
;			plot,x,y,COLOR = it_get_color( "data" )
;	will make a graph using with cyan lines/symbols.
;
;	Note that pro Seven_Colors must be called first.
;
; CALLING:
;	it_set_color, item, color
;
; INPUTS:
;	item = string, users name of item.
;	color = string, one of:
;		"red","yellow","green","cyan","blue","violet","white".
; KEYWORDS:
;	None.
; OUTPUTS:
;	None.
; COMMON BLOCKS:
;	common seven_colors, colors
;	common seven_color_it, it_color
; EXTERNAL CALLS:
;	function N_struct
; PROCEDURE:
;	Straightforward.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;-

pro it_set_color, item, color

   common seven_colors, colors
   common seven_color_it, it_color

	if N_struct( colors ) LE 0 then seven_colors

	wc = where( colors.color EQ strlowcase( color ), nc )
	if (nc LE 0) then return

	wi = where( it_color.name EQ strlowcase( item ), ni )

	if (ni LE 0) then begin
		wi = N_elements( it_color )
		it_color = [ it_color, it_color[0] ]
		it_color[wi].name = strlowcase( item )
	   endif

	wc = wc[0]
	wi = wi[0]

	it_color[wi].color = colors[wc].color
	it_color[wi].index = colors[wc].index
end
