function the_date

  if( !version.OS_FAMILY eq "unix") then begin

     spawn,["date","+%D"],date,/NOSHELL
     date = date[0]
     if strpos( date, '0' ) EQ 0 then date = strmid( date, 1, 7 )
     
  endif else if( strpos( strupcase( !version.OS ),"VMS") ge 0 ) then begin

     spawn,"show time",date
     date = next_word( date[0] )

  endif else date = systime()
     
return, date
end
