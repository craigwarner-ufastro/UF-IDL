function keep180, hang

  wn = where( hang LT -180, nn )
  if( nn gt 0 ) then hang[wn] += 360

  wp = where( hang gt 180, np )
  if( np gt 0 ) then hang[wp] -= 360

  return, hang
end

function HourAngle, FITSheader, AVERAGE=avgHA

  szfh = size( fitsheader )

  if( szfh[0] eq 2 ) then nfh = szfh(2) else nfh = 1

  ascale = fltarr( 3, nfh )
  ascale[0,*] = 1
  ascale[1,*] = 60
  ascale[2,*] = 3600
  ascale /= 15
  
  LST1 = reform( float( get_words( sxparv( fitsheader,'ST1'), DEL=':')), 3, nfh ) / ascale
  LST2 = reform( float( get_words( sxparv( fitsheader,'ST2'), DEL=':')), 3, nfh ) / ascale
  radeg = float( sxparv( fitsheader,"RAdeg"))

  if keyword_set( avgHA ) then begin
     
     return, keep180( (total( LST1, 1 ) + total( LST2, 1 ))/2 - radeg )
     
  endif else begin
     
     return, transpose([[ keep180( total( LST1, 1 ) - radeg)],$
                        [ keep180( total( LST2, 1 ) - radeg)]])
  endelse
end
