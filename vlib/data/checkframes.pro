function CheckFrames, testname, DIRECTORY=dir, SWAPBYTES=swapbytes, NSIGMA=nsig, DELTA=delta

	if N_elements( dir ) ne 1 then dir = "/data/Test"
	if N_elements( testname ) ne 1 then testname = "chop_test"
	if N_elements( nsig ) ne 1 then nsig = 11
	if N_elements( delta ) ne 1 then delta = 2000

	files = findfile( dir + "/" + testname + "*" )
	help, files

	Nframe = 0L
	Nbad = 0
	Fvdev = 0 & Fvav = 0 & Fvmed=0 & maxnmax=0 & ymin = 0 & ymax = 0
	fh = bytarr(80,36)
	frame = Lonarr( 320, 240 )

	for n=0,N_elements( files )-1 do begin

		openr,Lun,files(n),/GET_LUN
		readu,Lun,fh
		print,files(n)

		while NOT EOF(Lun) do begin

			readu,Lun,frame
			Nframe = Nframe+1
			if keyword_set( swapbytes ) then byteorder,frame,/LSWAP
			frsub = frame[*,1:238]
			w = where( frsub gt min(frsub)+delta, nmax )
			nmax = fix(nmax)

			if nmax gt 0 then begin
				frsub[w] = 0
				frsub[w] = total( frsub )/N_elements( frsub )
				ymin = fix( min( (w/320)+1, MAX=ymax ) )
				ymax = fix(ymax)
				maxnmax = maxnmax > nmax
			 endif

			FrmVar = Frame_Variation( frsub )
			if FrmVar gt 1e7 then message,"you may need to set /SWAPBYTES keyword!"+string(7b)
			frold = frsub

			if N_elements( FvarVec ) gt 0 then    $
				FvarVec = [ FvarVec, FrmVar ] $
			  else $
				FvarVec = FrmVar			  
		  endwhile

		free_Lun,Lun
		Fvdev = stdev( FvarVec, Fvav )
		Fvmed = median( FvarVec )
		wb = where( FvarVec gt nsig*Fvmed, Nbad )
		print, n, Nframe, FrmVar, Fvdev, Fvmed, max(FvarVec), $
			fix(Nbad), Nframe/float(Nbad), maxnmax, ymin,ymax
	  endfor

return, FvarVec
end
