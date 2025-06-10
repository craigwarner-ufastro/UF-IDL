pro ftspec, fitsfile, DMEANS=dmeans, DNOISE=dnoise, FREQUENCY=freq, FFT=dmmft, MEAN=mean, $
	    FHEADER=fhd, DATA=datfrms, DMLEFT=dml, DMRIGHT=dmr, WBAD=wbad

sz = size(datfrms)

if sz[0] ne 3 and N_elements(fitsfile) eq 1 then begin
	print," reading: ",fitsfile
	datfrms = reform( readfits( fitsfile, fhd ) )
	help,datfrms
	sz=size(datfrms)
   endif

nd=sz[sz[0]]
nx=sz[1]
npix=sz[1]*sz[2]
help,nd,npix
dmeans=fltarr(nd)
dml=dmeans
dmr=dmeans
dnoise=fltarr(nd)

lyot = sxpar(fhd,"LYOT")
tdet = sxpar(fhd,"DETARRAY")
help,lyot,tdet

fc=sxpar(fhd,"FRMCOADD")
sf=sxpar(fhd,'savefreq')
help,fc,sf

for i=0,nd-1 do begin
	dmeans[i] = total(datfrms[*,*,i])/npix
	dml[i] = 2*total(datfrms[0:nx/2-1,*,i])/npix
	dmr[i] = 2*total(datfrms[nx/2:*,*,i])/npix
	ip = (i-1)>0
	dnoise[i] = stdev((datfrms[*,*,i]-datfrms[*,*,ip])-dmeans[i]-dmeans[ip])
  endfor

dmeans=dmeans/float(fc)
dml=dml/float(fc)
dmr=dmr/float(fc)
dnoise=dnoise/sqrt(fc)/sqrt(2)
help,lyot,tdet

plot,dmeans
std=stdev(dmeans,mean)
print,std,mean
print,stdev(dmr,mr),mr
print,stdev(dml,ml),ml
wbad=where(dmeans gt mean+7*std,nbad)
help,nbad

if nbad GT 0 then begin
	dmeans[wbad]=(dmeans[wbad-1]+dmeans[wbad+1])/2
	dml[wbad]=(dml[wbad-1]+dml[wbad+1])/2
	dmr[wbad]=(dmr[wbad-1]+dmr[wbad+1])/2
	dnoise[wbad]=dnoise[wbad-1]
	dnoise[wbad+1]=dnoise[wbad-1]
	std=stdev(dmeans,mean)
	print,stdev(dmr,mr),mr
	print,stdev(dml,ml),ml
	print,std,mean
  endif

plot,dmeans
plot,dmeans-mean
dmm=dmeans-mean
plot,dmm[0:100],ps=-4

if dmeans[0] LT (mean-3*std) then begin
	dmeans[0]=mean
	dml[0]=ml
	dmr[0]=mr
	std=stdev(dmeans,mean)
	print,stdev(dmr,mr),mr
	print,stdev(dml,ml),ml
	dmm=dmeans-mean
	print,std,mean
   endif

dmmft=abs(fft(dmm,-1))

freq=sf*findgen(nd/2)/(nd/2-1)/2
plot,freq,dmmft

end
