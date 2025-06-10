pro plotNoise, fcv, WELL_DEPTH=welldepth, HARDCOPY=hardcopy

readmode = "S1"
if( min( fcv.sigma_clamp ) gt 0 ) then readmode = "S1R1_CR"
if( min( fcv.sigma_oclmp_max ) gt 0 ) then readmode = "S1R3"

ftit = "Avg. & Min & Max "
ptit = " for Readout = " + readmode
if N_elements( welldepth ) eq 1 then ptit = ptit + " : Well = " + welldepth

nt = N_elements( fcv )
secs = fcv[0].savePeriod * findgen(nt)
!x.title = "Seconds"

if keyword_set( hardcopy ) then begin
    fname = "Noise-" + readmode
    if N_elements( welldepth ) eq 1 then fname = fname + "-" + welldepth
    fname = fname + ".ps"
    message,/INFO,"Plotting to file : " + fname
    psLand, FILE = fname
    !p.thick = 3
endif

get_window,0
samp = "WELL  Noise"
plot, secs, fcv.sigma_well,/ysty,/xsty,yran=[2,8], YTIT=samp+" (ADU)", TIT=ftit+samp+ptit
oplot, secs, fcv.sigma_well_min
oplot, secs, fcv.sigma_well_max

if( min( fcv.sigma_clamp ) gt 0 ) then begin 
    get_window,1
    samp = "Column Clamp Ref. Noise"
    plot, secs, fcv.sigma_clamp,/ysty,/xsty,yran=[4,11], YTIT=samp+" (ADU)", TIT=ftit+samp+ptit
    oplot, secs, fcv.sigma_clamp_min
    oplot, secs, fcv.sigma_clamp_max
endif

if( min( fcv.sigma_oclmp_max ) gt 0 ) then begin 
    get_window,2
    samp = "Output Clamp Ref. Noise"
    plot, secs, fcv.sigma_oclmp_min,/ysty,/xsty,yran=[1,3], YTIT=samp+" (ADU)", TIT=ftit+samp+ptit
    oplot, secs, fcv.sigma_oclmp_max
endif

if keyword_set( hardcopy ) then psClose

!x.title = ""
!p.thick = 1
return
end
