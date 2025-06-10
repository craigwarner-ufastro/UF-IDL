deltd = 0.001

dgghi = ( deltd*( cfhi[1] + 2*cfhi[2]*td ) + deltd^2 * 2*cfhi[2]/2 )/poly(td,cfhi)

dggmed = ( deltd*( cfmed[1] + 2*cfmed[2]*td ) + deltd^2 * 2*cfmed[2]/2 )/poly(td,cfmed)

dggdat = 2 * 1.1 * [5.7533e-05, 4.1165e-05, 2.5076e-05, 1.5699e-05]

tdat = [8.6, 9, 9.5, 10]

save_pxyz
!X.thick=2
!Y.thick=2
!X.charsize=1.3
!Y.charsize=1.5
!X.margin=[17,3]

get_window,0,/SHOW
if keyword_set( hardcopy ) then psport,/sq,FILE="CC2006-250-deltaGain.ps",FONT=-1
if keyword_set( hardcopy ) then pthick=5 else pthick=2

plot,td,2*dgghi,xran=[8.4,10.2],/xsty,XTIT="!8T !3 (K)", YTIT="!4D!8G / G !3", $
    LINE=2, THICK=pthick, TIT="Predicted and Measured 2 Hz Oscillation of Gain", CHARTHICK=pthick

oplot, td, 1.5*dgghi, THICK=pthick, LINE=1
oplot, td, dgghi, THICK=pthick

xyouts,/NORM, 0.4, 0.8, "!4D!8T !3= 2 mK", SIZ=1.7
xyouts,/NORM, 0.4, 0.3, "!4D!8T !3= 1 mK", SIZ=1.7

tdatm = transpose([[tdat],[tdat]])
dggdm = transpose([[dggdat],[dggdat]])
psyms = [2,4,5,6]

for i=0,3 do oplot,tdatm[*,i],dggdm[*,i],ps=psyms[i],SYMSIZ=2,THICK=pthick

psclose

save_pxyz,/RESTORE

