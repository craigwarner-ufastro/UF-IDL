pro recall, cmd, TOKEN=toke

  help,/REC,OUT=hrec

  if keyword_set( toke ) then begin
     w = where(strpos(hrec,cmd) ge 0 and strpos(hrec,toke) ge 0 and strpos(hrec,'recall') LT 0,nw)
  endif else begin
     w = where( strpos( hrec, cmd ) ge 0 and strpos( hrec,'recall') LT 0, nw )
  endelse

  if( nw gt 0 ) then hprint, hrec[w]
end
