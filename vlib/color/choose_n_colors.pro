;+
; NAME:
;	choose_N_colors
; PURPOSE:
;	Allow user to choose number of colors to use for color table,
;	or else take whatever number is available.
; CALLING:
;	choose_N_colors
; INPUTS:
;	None.
; KEYWORDS:
;	DEFAULT_NCOL = (default = 100).
;	/YES_CHOOSE : assume yes answer to first question, so as to skip
;			ahead to choosing # of colors.
; OUTPUTS:
;	None.
; EXTERNAL CALLS:
;	function yes_no_menu
;	function get_text_input
;	pro print_struct
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1997.
;-

pro choose_N_colors, DEFAULT_NCOL=defncol, YES_CHOOSE=choose

if ((!D.flags AND 256) GT 0) then begin

	if keyword_set( defncol ) then defs = strtrim(defncol,2) else defs='100'

	if N_elements( choose ) ne 1 then $
		choose = yes_no_menu( "choose # colors",/BIN )

	if( choose ) then $
	   window, COL=fix( get_text_input( "how many colors ?",DEF=defs ) ) $
	else window

	wdelete
	print_struct, !D, ["n_colors","table_size"]
   endif
end
