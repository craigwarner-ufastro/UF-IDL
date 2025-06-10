sigT_medb = { V_detgrv:-5.8, Filter_um:9.8, $
		Td:double([ 7.90000,      8.00000,      9.00000,      10.0000 ]), $
		ADU:double([ 19689.2,      20283.1,      25200.3,      28264.8 ]) }

sigT_hib = { V_detgrv:-6.5, Filter_um:7.8, $
		Td:double([ 9.0,      9.5,      10.0 ]), $
		ADU:double([ 22561.0,       23123.0,       23551.0 ]) }

if keyword_set( hardcopy ) then psport,/sq,fil='CC2006-248+250-avgsig-Td.ps'

save_pxyz
if keyword_set( hardcopy ) then !P.thick=5 else !P.thick=2
!X.thick=2
!Y.thick=2
!X.charsize=1.3
!Y.charsize=1.3
!X.margin=[14,3]

get_window,0,/SHOW

plot, sigT_medb.Td, sigT_medb.ADU, ps=6, symsiz=2, xran=[7.8,10.1],/xsty,yran=[19e3,29e3],/ysty, $
					TIT="CC2006-248  and  CC2006-250", $
			XTIT = 'Detector Temperature  ( K )', YTIT='Average  Signal  per  Frame  ( ADU )'

oplot, sigT_hib.Td, sigT_hib.ADU, ps=4,symsiz=2

xyouts,/NORM, 0.3, 0.80, "Det. groove bias = " + string( sigT_medb.V_detgrv, FORM="(F4.1)" )+" V", SIZ=1.3
xyouts,/NORM, 0.3, 0.73, "Filter = " + string( sigT_medb.Filter_um, FORM="(F3.1)" ) + " um", SIZ=1.3

xyouts,/NORM, 0.55, 0.27, "Det. groove bias = " + string( sigT_hib.V_detgrv, FORM="(F4.1)" )+" V", SIZ=1.3
xyouts,/NORM, 0.55, 0.20, "Filter = " + string( sigT_hib.Filter_um, FORM="(F3.1)" ) + " um", SIZ=1.3

cfmed = poly_fit( sigT_medb.Td, sigT_medb.ADU, 2 )
print,cfmed
Td = 7 + findgen(40)/10
oplot, Td, poly( Td, cfmed )

cfhi = poly_fit( sigT_hib.Td, sigT_hib.ADU, 2 )
print,cfhi
oplot, Td, poly( Td, cfhi )

save_pxyz,/RESTORE

if keyword_set( hardcopy ) then psclose
