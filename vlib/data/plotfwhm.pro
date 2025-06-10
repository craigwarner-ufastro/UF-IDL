;+
; NAME
;       plotfwhm
;
; PURPOSE:
;       Plot FWHM stats from data_structs returned by function readaccsigs
;
; CALLING SEQUENCE:
;
;	plotfwhm, data_structs
;
; INPUTS:
;       data_structs = array of structures containing data & statistics (from function readaccsigs)
;
; KEYWORD PARAMETERS:
;
;	FILTER = name of Filter to plot
;
; OUTPUTS:
;       None.
;
; MODIFICATION HISTORY:
;
;  Written  June 2011, by:    Frank Varosi, Department of Astronomy, University of Florida.
;
;# $Name:  $ $Id: plotfwhm.pro,v 1.4 2011/06/09 18:32:08 varosi Exp $
;-

pro plotFWHM, data_structs, wf, FILTER=filter, CHOPPA=chopPAval, SYMPA=symPA, SNR=cutSNR, $
              XVY=xvy, _EXTRA=extra, HARDCOPY=psfile, MOFFAT=moffat

  if N_elements( filter ) ne 1 then filter = "Si2-8.7"
  if N_elements( cutSNR ) ne 1 then cutSNR = 900

  wf = where( (strpos( data_structs.filter, filter ) ge 0) and (data_structs.PeakSNR gt cutSNR) and $
              (data_structs.cammode eq "Imaging" or data_structs.cammode eq "TEST") and $
              (data_structs.obsmode eq "chop" or data_structs.obsmode eq "chop-nod"), nf ) 

  if( nf LE 0 ) then begin
     message,/INFO,"None found."
     return
  endif

  save_pxyz
  !P.title = data_structs[wf[0]].filter

  if N_elements( psfile ) eq 1 then begin
     !P.thick = 3
     psLand, FILE = psfile
  endif

  chopPAd = data_structs[wf].chopPA

  if N_elements( chopPAval ) eq 1 then begin
     help,nf
     if keyword_set( symPA ) then begin
        chopPAsym = chopPAval - 180
        wc = where( (chopPAd ge (chopPAval-1) and chopPAd LE (chopPAval+1)) or $
                    (chopPAd ge (chopPAsym-1) and chopPAd LE (chopPAsym+1)), nf )
        !P.title = filter + "  :  " + "chopPA = " + strtrim( chopPAval, 2 ) + " or " + strtrim( chopPAsym, 2 )
     endif else begin
        wc = where( chopPAd ge (chopPAval-1) and chopPAd LE (chopPAval+1), nf )
        !P.title = filter + "  :  " + "chopPA = " + strtrim( chopPAval, 2 )
     endelse
     if( nf gt 0 ) then wf=wf[wc]
  endif else print,"Unique Chop PA = ", chopPAd( unique( chopPAd,/SORT ) )

  if( nf LE 0 ) then begin
     message,/INFO,"None found with chop PA."
     return
  endif

  help,nf
  if( nf eq 1 ) then wf = [wf,wf]

  date1 = data_structs[wf[0]].date
  date2 = data_structs[wf[nf-1]].date

  if keyword_set( moffat ) then begin
     psffit = data_structs[wf].PSFfitM
  endif else psffit = data_structs[wf].PSFfitL

  if keyword_set( xvy ) then begin
     get_window,1,/SHOW
     !P.charsize = 1.6
     !y.Tickinterval = 1
     !x.Tickinterval = 1
     !x.range = [0, max( psffit.fwhmx )]
     !y.range = [0, max( psffit.fwhmy )]
     plot, psffit.fwhmx, psffit.fwhmy, PS=4, YSTYLE=16, $
           XTIT="FWHM - X (Horiz. Pixels)", YTIT="FWHM - Y (Vert. Pixels)", SYMSIZ=2, _EXTRA=extra
     xyouts,/NORM,0.03,0.02,date1 + " -> " + date2,SIZE=1.4
  endif else begin
     !P.multi = [0,2,1]
     !x.title = "Chop Throw (arcsec)"
     !x.range = [0,35]
     !x.style = 1
     !y.range = [0, max( psffit.fwhmx ) > max( psffit.fwhmy ) ]
     !y.Tickinterval = 1
     if( !D.name eq "PS") then begin
        !P.charsize = 1.3
        xmarx=[5,1]
        xmary=[6,0]
     endif else begin
        !P.charsize = 2
        xmarx=[6,1]
        xmary=[6,1]
     endelse
     get_window,0,/SHOW
     plot, data_structs[wf].chopthrow, psffit.fwhmx, PS=4, YTIT="FWHM - X  (Horiz. Pixels)", SYMSIZ=2, XMAR=xmarx, _EXTRA=extra
     plot, data_structs[wf].chopthrow, psffit.fwhmy, PS=4, YTIT="FWHM - Y  (Vert. Pixels)", SYMSIZ=2, XMAR=xmary, _EXTRA=extra
     xyouts,/NORM,0.44,0.01,date1 + " -> " + date2,SIZE=1
  endelse

  save_pxyz,/RESTORE
  psclose
end
