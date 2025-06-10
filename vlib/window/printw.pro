;+
; NAME:
;	printw
; PURPOSE:
;	Use window as a non-scrolling text screen and print one string per line.
; CALLING:
;	printw, strings
; INPUT:
;	strings = string array to be printed in window.
; KEYWORDS:
;	LINE = starting point as # lines from top of window if negative,
;		# lines from bottom of window if non-negative.
;		(LINE=-1 is at top and LINE=0 is at bottom, default value is -1)
;
;	XOFFSET = horizontal shift by # characters to right each line (deflt=1).
;	COLOR = color table index to use for graphics.
;	WINDOW = window number to set and show before output (default=current).
;	/ERASE : first erase the region to be occupied by text,
;	SIZE = character size: default = 1 with FONT=0, if specificied then FONT=1
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;	Mod, Frank Varosi UFastro 2020: added keyword SIZE=
;	Mod, Frank Varosi UFastro 2021: added keyword LOCXY=[x,y]
;-

pro printw, strings, LINE=Line, COLOR=color, SIZE=csize, XOFFSET=xoffset, PIXOFFSET=pixoff, $
				WINDOW=window, ERASE=erase, LOCXY=xyLoc, MAXLEN=maxLen

  nLine = N_elements( strings )
  if (nLine LE 0) then return

  ychsize = !D.y_ch_size + 2
  xchsize = !D.x_ch_size

  if keyword_set( csize ) then begin
     cfont=1
     ychsize = csize * !D.y_ch_size + 2
     xchsize *= csize
  endif else cfont=0

  sz = size( strings )
;;	if (sz[0] EQ 0) then strings = [strings]	;for old sun386i
  if N_elements( Line ) NE 1 then Line = -1

  if N_elements( window ) EQ 1 then begin
     wset, window
     wshow, window, ICON=0
  endif

  if N_elements( xyLoc ) eq 2 then begin
     pixoff = xyLoc[0]
     ystart = xyLoc[1]
  endif else begin
     ;; normally Line is negative from top of window:
     ystart = Line * ychsize + !D.y_vsize-1
     if (Line GE 0)  then  ystart = (nLine-1+Line) * ychsize + 6
  endelse

  if keyword_set( pixoff ) then xoff = pixoff else begin

     Nxoff = N_elements( xoffset )

     if (Nxoff LE 0) then xoff = replicate( !D.x_ch_size, nLine ) else begin
        if (Nxoff EQ 1) then xoff = replicate( xoffset, nLine ) else begin
           if (Nxoff LT nLine) then xoff = [ xoffset, replicate( xoffset[Nxoff-1], nLine-Nxoff ) ]
        endelse
        xoff = xoff * xchsize
     endelse
  endelse
  
  xoff = ( xoff > 0 ) < !D.x_vsize
  yLine = (( ystart - indgen( nLine ) * ychsize ) > 0 ) < !D.y_vsize

  if keyword_set( erase ) then begin
     if (nLine GT 1) then xLeft = min( xoff ) else xLeft = xoff[0]
     if (Line EQ -1) then Neras = nLine + .3 else Neras = nLine
     nx = !D.x_vsize - xLeft
     if nx GT 1 then begin
        zero = bytarr( nx, Neras * ychsize + 3 )
        ybot = fix( ystart - (nLine-.75) * ychsize - 1 ) > 0
        tv, zero, xLeft, ybot < (!D.y_vsize-1)
     endif
  endif

  if N_elements( maxLen ) ne 1 then maxLen = 198
  wbig = where( strlen( strings ) gt maxLen, nbig )

  if( nbig gt 0 ) then begin
     wok = where( strlen( strings ) LE maxLen, nok )
  endif else begin
     wok = indgen( nLine )
     nok = nLine
  endelse

  if N_elements( xoff ) ne nLine then xoff = replicate( xoff[0], nLine )
  if N_elements( yLine ) ne nLine then yLine = replicate( yLine[0], nLine )

  if N_elements( color ) EQ 1 then begin
     for i=0,nok-1 do begin
        w = wok[i]
        xyouts,/DEV,xoff[w],yLine[w],strings[w],FONT=cfont,COL=color, SIZE=csize
     endfor
     for i=0,nbig-1 do begin
        w = wbig[i]
        xyouts,/DEV,xoff[w],yLine[w],strmid(strings[w],0,maxLen),FONT=cfont,COL=color, SIZE=csize
     endfor
  endif else begin
     for i=0,nok-1 do begin
        w = wok[i]
        xyouts,/DEV, xoff[w], yLine[w], strings[w],FONT=cfont, SIZE=csize
     endfor
     for i=0,nbig-1 do begin
        w = wbig[i]
        xyouts,/DEV, xoff[w], yLine[w], strmid(strings[w],0,maxLen),FONT=cfont, SIZE=csize
     endfor
  endelse

  empty
  return
end
