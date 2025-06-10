if N_elements( knodset ) ne 1 then knodset = 0
if N_elements( knodbeam ) ne 1 then knodbeam = 0
if N_elements( ksav ) ne 1 then ksav = 0
  
ncoadds = float( sxpar(fhd,"FRMCOADD") * sxpar(fhd,"CHPCOADD") )
help,ncoadds,knodset,knodbeam,ksav

r1 = fdat[*,*,1,ksav,knodbeam,knodset]/ncoadds
r2 = fdat[*,*,1,ksav+1,knodbeam,knodset]/ncoadds

s1 = fdat[*,*,0,ksav,knodbeam,knodset]/ncoadds
s2 = fdat[*,*,0,ksav+1,knodbeam,knodset]/ncoadds

dr = r2 - r1
ds = s2 - s1

if N_elements( nxe ) ne 1 then nxe = 100
if N_elements( nye ) ne 1 then nye = 90

dre = dr[0:nxe-1,0:nye-1]
dse = ds[0:nxe-1,0:nye-1]
npix = N_elements( dre )

sdr = stdev( dre, mdr )
sds = stdev( dse, mds )
print,"Ref sigma =", sdr*sqrt(ncoadds/2), mdr
print,"Src sigma =", sds*sqrt(ncoadds/2), mds

sdsr = stdev( dse - dre, mdsr )
print,"Diff sigma =", sdsr*sqrt(ncoadds)/2, mdsr

if keyword_set( hardcopy ) then psport,/sq,fil='Noise_Time.ps'

get_window,0,/SHOW

plot, hv, histo( dse-dre, hv, bins=1)>1, ps=10,/ylog,xran=[-30,30],yran=[1,2000],/ysty, $
	xtit="ADU / FrameTime", ytit='Freq.', tit='dS-dR, dR, dS'

oplot, hv, npix * gaussian( hv, [1,mdsr,sdsr] )/sqrt(2*!pi)/sdsr, LINE=2

oplot,hv,histo( dre, hv,bins=1)>1,ps=10
oplot, hv, npix * gaussian( hv, [1,mdr,sdr] )/sqrt(2*!pi)/sdr, LINE=1

oplot,hv,histo( dse, hv,bins=1)>1,ps=10
oplot, hv, npix * gaussian( hv, [1,mds,sds] )/sqrt(2*!pi)/sds, LINE=1

psclose
