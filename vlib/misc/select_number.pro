;+
;Create menu of integer numbers and return selection.
;Frank Varosi NASA/GSFC 1989.
;F.V. 1990, added keyword SKIP= interval between numbers.
;F.V. 1991, allow first item (minN) in numbers menu to be floating.
;F.V. 1991, added keyword /ZERO to include 0 first in menu.
;F.V. 1991, added keyword FACTOR= scaling factor for numbers.
;F.V. 2011, use function wmenux and popup at specified menu_window.
;-

function select_number, title, minN, maxN, SKIP=skip, INIT=init, FACTOR=factor,$
			ZERO_START=zero, MORE_NUMBERS=more, MENU=menu_window

	if (!D.name EQ "SUN") then begin
           if N_elements( menu_window ) EQ 1 then begin
              wset,menu_window
              wshow,menu_window
           endif
        endif

	if N_elements( init ) EQ 1 then initsel=fix( init ) else initsel=1
	if N_elements( title ) NE 1 then title = "Select Number"
	if N_elements( minN ) NE 1 then minN = 1
	if N_elements( maxN ) NE 1 then maxN = 4

	if vartype( minN ) EQ "FLOATING" then begin

		mini = fix( minN )
		numbers = findgen( maxN-mini+1 ) + mini
		numbers[0] = minN
		format = "((F7.1))"

	  endif else begin

		numbers = indgen( maxN-minN+1 ) + minN
		format = "((I7))"
	   endelse

	if N_elements( skip ) EQ 1 then begin

		skip1 = (skip > 0) + 1
		N = N_elements( numbers )
		N1 = N-1
		N = N/float( skip1 )
		if (N GT fix( N )) then N = fix( N+1 ) else N = fix( N )
		i = skip1 * indgen( N )
		i = i < N1
		numbers = numbers[i]
		w = where( numbers GE initsel, nw )
		if (nw GT 0) then initsel = w[0]+1 $
			     else initsel = N_elements( numbers )

	  endif else initsel = initsel - minN +1

	if keyword_set( zero ) then begin
		initsel = initsel+1
		numbers = [0,numbers]
	   endif

	if keyword_set( factor ) then begin
		numbers = factor * numbers
		if (factor LT 0.1) then format = "((F7.2))" $
				   else format = "((F7.1))"
	   endif

	menu = [ title, string( numbers, FORMAT=format ) ]
	if keyword_set( more ) then menu = [ menu, "more" ]

	if (!D.name eq "X") or (!D.name eq "SUN") then begin
		sel = wmenux( menu, INIT=initsel, TITLE=0, WINDO=menu_window )
		if (sel LE 0) then sel = initsel>1
	 endif else sel = initsel>1

	if (sel GT N_elements( numbers )) then begin
           return, select_number( title, maxN, 2*maxN, SKIP=skip, $
                                  FACT=factor, MENU=menu_window, IN=maxN )
        endif else return, numbers[sel-1]
end
