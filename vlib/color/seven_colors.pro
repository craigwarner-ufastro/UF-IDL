;+
; NAME:
;	Seven_Colors	
; PURPOSE:
;	Set the top 7 entries in Lookup Color Table (LCT) to be the seven
;	primary colors: "red","yellow","green","cyan","blue","violet","white".
;	Creates a structure array in common containing these lookup
;	color table indices and the names of the colors.
;
;	Pro it_set_color can associate an item with any of the 7 colors:
;		it_set_color, "model", "green"
;		it_set_color, "data", "cyan"
;	This info is saved in common block.
;
;	Then function it_get_color returns the color index associated with
;	any item, so that:
;			plot,x,y,COLOR = it_get_color( "data" )
;		will make a graph using with cyan lines/symbols.
;
;	The first 7 default items are the colors themselves, so that:
;			plot,x,y,COLOR = it_get_color( "green" )
;		will make a graph using with green lines/symbols.
;
; CALLING:
;	Seven_Colors, new_topval
;
; INPUTS:
;	None.
; KEYWORDS:
;	None.
; OUTPUTS:
;	new_topval = optional, the index just below where 7 colors start in LCT.
; COMMON BLOCKS:
;	common Seven_Colors, colors
;	common Seven_Color_it, it_color
; EXTERNAL CALLS:
;	function N_struct
; PROCEDURE:
;	Straightforward.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;	Frank Varosi UF-Astro 2014 : added keywords /SAVE and /RESTORE.
;	F.V. UFastro 2022: deprecated /SAVE & /RESTORE: done by /LOAD and /UNLOAD
;-

pro Seven_Colors, new_topval, SAVE=save, LOAD=Load, UNLOAD=unLoad, INITIALIZE=init

   common Seven_Colors, colors
   common Seven_Color_it, it_color
   common Seven_Colors0, cstart
   common Seven_Colors1, rsav, gsav, bsav
   common Seven_Colors2, waitsec

        if (!D.flags AND 256) GT 0 then begin
           if !D.window LT 0 then begin
              window, XS=2,YS=2,/FREE
              wdelete
           endif
        endif

	if keyword_set( init ) or N_struct( colors ) LT 7 then begin
           colors = replicate( { color:"", index:0, H:0., S:1., V:1. }, 7 )
           colors.H = 60 * findgen( 7 )
           colors[1].H = 50
           colors[6].S = 0
           colors.color = ["red","yellow","green","cyan","blue","violet","white"]
           it_color = replicate( { name:"", index:0, color:"" }, 7 )
           it_color.color = colors.color
           it_color.name = colors.color
           cstart = !D.table_size - 7
           colors.index = cstart + indgen( 7 )
           it_color.index = colors.index
        endif

        new_topval = cstart-1
        if N_elements( waitsec ) ne 1 then waitsec = 0.01

        if keyword_set( Load ) then begin

           empty
           wait, waitsec
           tvLct, r, g, b,/GET
           rsav = r[cstart:*]
           gsav = g[cstart:*]
           bsav = b[cstart:*]
           wait, waitsec
           tvLct, colors.H, colors.S, colors.V, cstart, /HSV

        endif else if keyword_set( unLoad ) then begin

           new_topval = !D.table_size-1
           empty
           wait, waitsec
           if N_elements( rsav ) gt 2 AND $
              N_elements( rsav ) gt 2 AND $
              N_elements( rsav ) gt 2 then tvLct, rsav, gsav, bsav, cstart
        endif
end
