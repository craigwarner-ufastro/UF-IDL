;+
;  wss, window_number
;
;  Shortcut name call to pro window_set_show
;
;Frank Varosi UFastro 2012.
;-

pro wss, winum, DELAY=delay, CHECK_VALID=check, CURSOR_IN_CENTER=cen_cursor, ZERO_BUTTON_STATUS=zero_button

  window_set_show, winum, DELAY=delay, CHECK=check, CURSOR_IN_CEN=cen_cursor, ZERO_BUT=zero_button
end
