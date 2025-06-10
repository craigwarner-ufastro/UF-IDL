pro ChanSpec, frame, FTMS=frameTime, COLUMN=icol, _EXTRA=extra, $
			NCHAN=nchan, ALLCOLUMNS=allc, CHANNEL=ichan

	if N_elements( nchan ) ne 1 then nchan=16
	if N_elements( icol ) ne 1 then icol = 10
	if( icol LT 0 ) then allc=1

	sz = size( frame )
	nxp = sz[1]
	nyp = sz[2]
	ncc = nxp/nchan
	if( icol ge ncc ) then icol = ncc-1
	ymargin = !y.margin

	if keyword_set( allc ) then begin
		freq = 1000*findgen(ncc*nyp/2)/frameTime
		!x.title = "Frequency (Hz)"
	 endif else begin
		freq = 1000*findgen(nyp/2)/frameTime
		!x.title = "Frequency (Hz)  Column = " + strtrim(icol,2)
	  endelse

	if keyword_set( ichan ) then  nchan = ichan  else begin
		ichan=1
		!p.multi=[0,1,nchan]
		ymargin=[0,0]
	   endelse

	if keyword_set( allc ) then begin
		for ic=ichan,nchan do begin
			data = frame[ncc*(ic-1):ncc*ic-1,*]
			plot, freq, abs( fft( data[indgen(N_elements(data))], -1 ) ), $
			  ps=10, ymarg=ymargin, ytit="Chan = " +strtrim(ic,2), _EXTRA=extra
		  endfor
	 endif else begin
		for ic=ichan,nchan do begin
			plot, freq, abs( fft( frame[ncc*(ic-1)+icol,*], -1 ) ), $
			  ps=10, ymarg=ymargin, ytit="Chan = " +strtrim(ic,2), _EXTRA=extra
		  endfor
	  endelse

	!p.multi = 0
	!x.title = ""
end
