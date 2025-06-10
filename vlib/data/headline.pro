;;-------------------------------------------------------------------------------------
;; Convenience function to put relevant parameters from FITS header into single string.
;;-------------------------------------------------------------------------------------

function headline, fitshdr, NOFT=noft, SIMPLE=simple, ENVIR=envir, $
                   SLIT=pslit, NOBS=nobs, SHORT=short, RADEC=radec, SKYINFO=skyinfo

  szfh = size( fitshdr )

  if szfh[0] eq 2 then begin
     nfh = szfh[2]
     headlines = strarr( nfh )
     for i=0,nfh-1 do headlines[i] = strconcat( headline( fitshdr[*,i], NOFT=noft,$
                                                          ENV=envir, SLIT=pslit, $
                                                          SIMP=simple, SHORT=short,$
                                                          RADEC=radec,NOBS=nobs,SKY=skyinfo))
     return, headlines
  endif

  fw1 = sxpar( fitshdr,"filter1")
  fw2 = sxpar( fitshdr,"filter2")
  if( strpos( fw1,"Open" ) ge 0 ) then filter = fw2 else filter = fw1

  grating = sxpar( fitshdr,"grating")
  if( strpos(grating,"Mirror") LT 0 ) then filter = grating

  welld =  sxpar( fitshdr,"welldpth")
  object =  sxpar( fitshdr,"object")
  if vartype( object,/CODE) ne 7 or keyword_set( skyinfo ) then object = ""

  if N_elements( nobs ) eq 1 then expfact = nobs else begin
     expfact = 1
     if strpos( sxpar(fitshdr,"COMPSTAT"),"STOP") ge 0 then begin
        cwords = get_words( sxpar(fitshdr,"COMPSTAT") )
        if N_elements( cwords ) ge 3 then begin
           nf = float( cwords[2] )
           if( nf gt 0 ) then begin
              totf = sxpar(fitshdr,"TOTFRMS")
              expfact = nf / totf
           endif
        endif
     endif
  endelse 

  exptime =  sxpar( fitshdr,"exptime") * expfact

  if( exptime gt 999 ) then begin
     sexptime =  string( exptime/60., FORM="(G9.3)") + "min"
  endif else sexptime =  string( exptime, FORM="(G9.3)") + "sec"

  if strlen( object) gt 1 then begin
     pfor = strpos( object,"_for_")
     if( pfor LT 0 ) then pfor = 20
     object = strmid( object, 0, pfor )
     hinfo = object +" : "
  endif else hinfo = ""

  if keyword_set( simple ) then begin

     hinfo += (sxpar( fitshdr,"READMODE") +" : " $
               + string( sxpar( fitshdr,"FRMTIME"),FORM="(F5.1)") +" ms : " $
               + sxpar( fitshdr,"DATE-OBS") +" : " $
               + sxpar( fitshdr,"UTSTART"))

  endif else if keyword_set( skyinfo ) then begin

     hinfo += (sxpar( fitshdr,"READMODE") +" : " $
               + string( sxpar( fitshdr,"FRMTIME"),FORM="(F5.1)") +" ms : " $
               + sxpar( fitshdr,"DATE-OBS"))

  endif else if keyword_set( noft ) then begin

     hinfo += (sxpar( fitshdr,"WINDOW") +" : " $
               + sxpar( fitshdr,"APERTURE") +" : "+ filter +" : " $
               + sxpar( fitshdr,"DATE-OBS"))

  endif else if keyword_set( short ) then begin

     hinfo +=(filter+" : "+sxpar(fitshdr,"DATE-OBS")+"  :  "+strmid(sxpar(fitshdr,"UT1"),0,5))

  endif else begin

     hinfo += (sxpar( fitshdr,"READMODE") +" : " $
               + string( sxpar( fitshdr,"FRMTIME"),FORM="(F5.1)") +" ms : "+ welld +" : " $
               + sxpar( fitshdr,"WINDOW") +" : " $
               + sxpar( fitshdr,"APERTURE") +" : "+ filter  +" : " $
               + sxpar( fitshdr,"DATE-OBS") +" : " $
               + strmid( sxpar( fitshdr,"UTSTART"),0,5))
  endelse

  if keyword_set( pslit ) then begin
     if( pslit gt 1 ) then hinfo += (" :" + sexptime)
     hinfo += (" : Slit = " + sxpar( fitshdr,"SLIT"))
  endif else hinfo += (" :" + sexptime)

  if keyword_set( envir ) then begin

     humid = (sxpar( fitshdr,"HUMOUT1") + sxpar( fitshdr,"HUMOUT2"))/2
     pwv1 = sxpar( fitshdr,"PWVMON1")
     pwv2 = sxpar( fitshdr,"PWVMON2")

     if( pwv1 gt 0 and pwv2 gt 0 ) then pwv = (pwv1 + pwv2)/2 else begin
        if( pwv1 gt 0 ) then pwv = pwv1 else pwv = pwv2 
     endelse

     envinfo = "Airmass = " + string( sxpar( fitshdr,"AIRMASS"),FORM="(F4.2)") + $
               " : Humidity = " + string( humid, FORM="(F4.1)") + " %" + $
               " : PWV = " + string( pwv, FORM="(F3.1)") + " mm"

     if keyword_set( radec ) then begin
        ra = sxpar( fitshdr,"RA")
        dec= sxpar( fitshdr,"DEC")
        if vartype( ra,/CODE ) eq 7 then  envinfo +=(" : RA=" + ra)
        if vartype( dec,/CODE ) eq 7 then envinfo +=(" : DEC=" + dec)
     endif

     Return,[hinfo+' ', envinfo]
  endif

  return, hinfo
end
