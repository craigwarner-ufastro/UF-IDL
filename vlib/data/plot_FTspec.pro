save_pxyz
!X.thick=2
!Y.thick=2
!X.charsize=1.3
!Y.charsize=1.3
!X.margin=[11,3]

get_window,0,/SHOW

if keyword_set( hardcopy ) then psport,/sq,fil="CC2006-254-ftspec-8_9_10K-relog.ps
if keyword_set( hardcopy ) then pthick=5 else pthick=2

ftspec,sd8,/ylog,/xsty,/rel,yran=[1e-6,1e-4],xran=[1.985,2.02],ps=-2,THICK=pthick, SYMSIZ=1.5

ftspec,sd9,/ylog,/xsty,ps=-4,/rel,/ov,THICK=pthick, SYMSIZ=1.5

ftspec,sd95,/ylog,/xsty,ps=-5,/rel,/ov,THICK=pthick, SYMSIZ=1.5

ftspec,sd10,/ylog,/xsty,ps=-6,/rel,/ov,THICK=pthick, SYMSIZ=1.5

psclose


if keyword_set( hardcopy ) then psport,/sq,fil="CC2006-254-ftspec-8_9_10K-relin.ps

ftspec,sd8,/xsty,yran=[0,6e-5],/rel,xran=[1.985,2.02],ps=-2,THICK=pthick, SYMSIZ=1.5

ftspec,sd9,/ylog,/xsty,ps=-4,/rel,/ov,THICK=pthick, SYMSIZ=1.5

ftspec,sd95,/ylog,/xsty,ps=-5,/rel,/ov,THICK=pthick, SYMSIZ=1.5

ftspec,sd10,/ylog,/xsty,ps=-6,/rel,/ov,THICK=pthick, SYMSIZ=1.5

psclose

get_window,1,/SHOW

ftspec,sd8,/xsty,yran=[1e-7,1e-3],/ylog,/rel,xran=[1e-2,3],/xlog,symsiz=.3,ps=-2,HARD=hardcopy

ftspec,sd9,/xsty,yran=[1e-7,1e-3],/ylog,/rel,xran=[1e-2,3],/xlog,symsiz=.4,ps=-4,HARD=hardcopy

ftspec,sd95,/xsty,yran=[1e-7,1e-3],/ylog,/rel,xran=[1e-2,3],/xlog,symsiz=.4,ps=-5,HARD=hardcopy

ftspec,sd10,/xsty,yran=[1e-7,1e-3],/ylog,/rel,xran=[1e-2,3],/xlog,symsiz=.4,ps=-6,HARD=hardcopy


if keyword_set( hardcopy ) then psport,/sq,fil="CC2006-254-ftspec-8.6K_9.5K-full-relog.ps

ftspec,sd8,/xsty,yran=[1e-7,1e-3],/ylog,/rel,xran=[1e-2,3],/xlog,symsiz=.3,ps=-2

ftspec,sd95,/ylog,/xsty,ps=-5,/rel,/ov,/xlog,symsiz=.4

psclose

save_pxyz,/RESTORE

