pro ftspecread, fitsfile, DMEANS=dmeans, FREQUENCY=freq, FFT=dmmft, MEAN=mean, $
	    FHEADER=fhd, DMLEFT=dml, DMRIGHT=dmr, WBAD=wbad, DISPLAY=display

	print," reading: ",fitsfile
	fhds = sheadfits( fitsfile, HEAD=fhd )

help,/st,fhds
nd= fhds.totalFrames
nx= sxpar(fhd,"NAXIS1")
ny= sxpar(fhd,"NAXIS2")
npix= nx*ny
help,nd,npix
dmeans=fltarr(nd)
dml=dmeans
dmr=dmeans
dnoise=fltarr(nd)

lyot = sxpar(fhd,"LYOT")
tdet = sxpar(fhd,"DETARRAY")
help,lyot,tdet

nfc=sxpar(fhd,"FRMCOADD")
sf=sxpar(fhd,'savefreq')
help,nfc,sf

	on_ioerror,EOF
	openr,Lun,fitsfile,/GET
	nhl = 1 + N_elements(fhd)/36
	hb = bytarr(80,nhl*36)
	readu,Lun,hb
	datfrm = Lonarr(nx,ny)

	for i=0,nd-1 do begin
		readu,Lun,datfrm
		dmeans[i] = total(datfrm)/npix/nfc
		print,i,FORM="($,i5)"
		if keyword_set( display ) then tvs,datfrm,/col
		ip = (i-1)>0
	  endfor
EOF:
	free_Lun,Lun

	if( i LT nd ) then begin
		nd = i
		dmeans = dmeans[0:nd-1]
	   endif

print," "
help,dmeans,lyot,tdet

plot,dmeans
std=stdev(dmeans,mean)
print,std,mean
wbad=where( (dmeans gt mean+7*std) or (dmeans lt mean-7*std), nbad )
help,nbad

if nbad GT 0 then begin
	dmeans[wbad]=(dmeans[wbad-1]+dmeans[wbad+1])/2
	if wbad[0] eq 0 then dmeans[0] = dmeans[1]
	std=stdev(dmeans,mean)
	print,std,mean
  endif

plot,dmeans-mean,TIT=fitsfile
wait,1
dmm=dmeans-mean

if dmeans[0] LT (mean-3*std) then begin
	dmeans[0]=mean
	std=stdev(dmeans,mean)
	dmm=dmeans-mean
	print,std,mean
   endif

dmmft=abs(fft(dmm,-1))

freq=sf*findgen(nd/2)/(nd/2-1)/2

plot,freq,dmmft,XRAN=[0,3],TIT=fitsfile

print,fitsfile,max(dmmft)
p= strpos( fitsfile,".fits")
savfil = strmid( fitsfile,0,p ) + ".means"
help,savfil

save,fhd,fhds,mean,dmeans,fitsfile,freq,dmmft,/VERB,FIL=savfil

end
