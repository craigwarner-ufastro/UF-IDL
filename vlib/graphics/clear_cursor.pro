pro clear_cursor, MAXSEC=maxsec, WAITSEC=wsec

  if N_elements( maxsec ) ne 1 then maxsec = 1.0
  if N_elements( wsec ) ne 1 then wsec = 0.01
  maxtry = fix( maxsec/wsec ) > 10
  ntry = 0

  while( !mouse.button ne 0 and ++ntry LT maxtry ) do begin
     cursor,xc,yc,/DEV,/NOWAIT
     wait,0.01
  endwhile

end
