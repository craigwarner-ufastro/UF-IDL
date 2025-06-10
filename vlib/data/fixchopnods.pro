;+
; NAME
;       fixchopnods
;
; PURPOSE:
;       If chopper throw is less than FOV,
;	combine the two nod beam data images intelligently by averaging
;	in regions where pixels are the same to within a small factor of noise level,
;	and in other regions use the greater of the two pixel values.
;	This will eliminate the negative chop regions of the usual nod sum image.
;
; CALLING SEQUENCE:
;
;	nodavg_image = fixchopnods( nodimA, nodimB )
;
; INPUTS:
;	nodimA = 2-D matrix of the image in nod beam A.
;	nodimB = 2-D matrix of the image in nod beam B.
;
; KEYWORD PARAMETERS:
;
;	FACTOR_SIG = (default is 3)
;	SIGMA_NOISE = (default is compute using function sky_noise)
;	/VERBOSE
;	/DISPLAY : display before & after using pro tvs.
;	LOG=minLog : min values for Log scale display.
;
; OUTPUTS:
;	Single image of intelligently combined nod beam images
;
; MODIFICATION HISTORY:
;
;	Written Dec. 2004, by: Frank Varosi, Department of Astronomy, University of Florida.
;
;# $Name:  $ $Id: fixchopnods.pro,v 1.6 2012/02/14 21:37:06 varosi Exp $
;-

function fixchopnods, nodimA, nodimB, FACTOR_SIG=sigfac, SIGMA_NOISE=sigman, $
                      VERBOSE=verbose, DISPLAY=disp, LOG=minLog

	if N_elements( sigman ) ne 1 then sigman = ( sky_noise( nodimA ) + sky_noise( nodimB ) )/2
	if NOT keyword_set( sigfac ) then sigfac=3
        maxDev = sigfac * sigman

	wbadA = where( nodimB - nodimA gt maxDev, nbadA )
	wbadB = where( nodimA - nodimB gt maxDev, nbadB )

	nodavg = (nodimA + nodimB)/2
	nodfix = nodavg
	if( nbadB gt 0 ) then nodfix[wbadB] = nodimA[wbadB]
	if( nbadA gt 0 ) then nodfix[wbadA] = nodimB[wbadA]

	if keyword_set( verbose ) then begin
           help,sigman,nbadA,nbadB
           signold = sky_noise(nodavg,skyold)
           signfix = sky_noise(nodfix,skyfix)
           print," Avgd. noise sigma=",signold,",  sky=",skyold,",  SNR=",max(nodavg)/signold
           print," Fixed noise sigma=",signfix,",  sky=",skyfix,",  SNR=",max(nodfix)/signfix
        endif

	if keyword_set( disp ) then begin
		sz = size( nodavg )
		get_window,0,XSIZ=sz[1],YSIZ=3*sz[2],/SHOW
		tvs,nodavg,LOG=minLog,MAG=1
		tvs,nodfix,0,sz[2],LOG=minLog,MAG=1
		tvs,(nodfix-nodavg)/(nodfix>1e-4),0,2*sz[2],/COLOR_BAR,max=.15,min=0,MAG=1
	   endif

return, nodfix
end
