function yes_no_menu, question, NO_DEFAULT=no_init, BINARY=binary

	menu = [ question + "?", "NO", "YES" ]

	if keyword_set( no_init ) then init=1 else init=2

	sel = wmenu( menu, INIT=init, TIT=0 ) > 1

	if keyword_set( binary ) then  return, sel-1  else  return, menu(sel)
end
