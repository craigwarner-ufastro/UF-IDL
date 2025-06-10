;+
; NAME:
;	patch_image
; PURPOSE:
;	Create a new image by patching a region in an image
;	using Vertical and/or Horizontal interpolation,
;	with optional averaging and/or Gaussion noise added.
; CALLING:
;	patched_image = patch_image( image, patch_coordin, direction )
; INPUTS:
;	image = 2D array to patch.
;	patch_coordin = 4 element integer array specifying region to patch:
;		       [ xcorner, xsize, ycorner, ysize ]
;	direction = string, H,V, or HV.
; KEYWORDS:
;	AVERAGE = # of pixels to average outside of patch region for values
;		between which interpolation is computed, default=0.
;	GAUSSIAN_NOISE = standard deviation of Gaussian noise to add, default=0.
;	DIAGONAL_WIDTH = width (pixels) of diagonal of rectangle to patch instead.
;	/FLIP = patch the flipped diagonal of rectangular region.
; OUTPUTS:
;	Function returns array of same size as input image, and same values
;	except changes as requested: horizontal and/or vertical interpolation.
; EXTERNAL CALLS:
;	function vecLinterp
; PROCEDURE:
;
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1990.
;	Mod 2024: F.V. UFastro: added option to patch along Diagonal Line.
;-

function patch_image, image, patch_coordin, direction, AVERAGE=aver,$
                      GAUSSIAN_NOISE=st_dev, DIAGONAL_WIDTH=diagwidth,FLIP=flip

	sim = size( image )
        pc = patch_coordin

        if N_elements( pc ) LT 4 then begin
           message,/INFO,"ERR: patch region coordinates should be 4 numbers."
           return, image
        endif

	xL = pc[0] > 0
	xsiz = pc[1] > 1
	xR = ( xL + xsiz -1 ) < (sim[1]-1)
	xsiz = xR - xL + 1

	yB = pc[2] > 0
	ysiz = pc[3] > 1
	yT = ( yB + ysiz -1 ) < (sim[2]-1)
	ysiz = yT - yB + 1

	xL1 = xL > 1
	xR1 = xR < (sim[1]-2)
	yB1 = yB > 1
	yT1 = yT < (sim[2]-2)
	patched_image = image

        if keyword_set( diagwidth ) then begin

           patch = image[ xL:xR, yB:yT ]
           if keyword_set( flip ) then patch = rotate( patch, 5 )
           szp = size( patch )
           nxp = szp[1]
           nyp = szp[2]
           dw = diagwidth > 3
           dwh = dw/2
           slope = float(nyp)/float(nxp)

           if( nyp gt nxp ) then begin
              dy = indgen( nyp )
              dx = fix( dy/slope )
              dxb = (dx - dwh) > 0
              dxt = (dx + dwh) < (nxp-1)
              dxb1 = dxb+1
              dxt1 = dxt-1
              np = dxt - dxb -1
              wp = where( np gt 0, nw )
              for iw=0,nw-1 do begin
                 iy = wp[iw]
                 hrow = patch[*,iy]
                 fe = [hrow[dxb[iy]],hrow[dxt[iy]]]
                 xe = [0,np[iy]+1]
                 xi = indgen(np[iy])+1
                 hrow[dxb1[iy]:dxt1[iy]] = finterpol( fe, xe, xi )
                 patch[*,iy] = hrow
              endfor
           endif else begin
              dx = indgen( nxp )
              dy = fix( slope * dx )
              dyb = (dy - dwh) > 0
              dyt = (dy + dwh) < (nyp-1)
              dyb1 = dyb+1
              dyt1 = dyt-1
              np = dyt - dyb -1
              wp = where( np gt 0, nw )
              for iw=0,nw-1 do begin
                 ix = wp[iw]
                 vcol = reform( patch[ix,*] )
                 fe = [vcol[dyb[ix]],vcol[dyt[ix]]]
                 ye = float([0,np[ix]+1])
                 yi = findgen(np[ix])+1
                 vcol[dyb1[ix]:dyt1[ix]] = finterpol( fe, ye, yi )
                 patch[ix,*] = vcol
              endfor
           endelse

           if keyword_set( flip ) then patch = rotate( patch, 5 )

        endif else begin

           if N_elements( aver ) NE 1 then aver=0

           if (aver GT 1) then begin

		avH = fix( min( [ aver, xL, sim[1]-xR-1 ] ) ) > 1
		pL = image[ (xL1-avH):(xL1-1), yB:yT ]
		pR = image[ (xR1+1):(xR1+avH), yB:yT ]
		pL = transpose( pL )
		pR = transpose( pR )

		avV = fix( min( [ aver, yB, sim[2]-yT-1 ] ) ) > 1
		pB = image[ xL:xR, (yB1-avV):(yB1-1) ]
		pT = image[ xL:xR, (yT1+1):(yT1+avV) ]

		print," # pixels averaged (H,V) =", avH,avV

		unity = replicate( 1, avH )	;to average over widths.
		pL = ( pL # unity )/avH
		pR = ( pR # unity )/avH

		unity = replicate( 1, avV )
		pB = ( pB # unity )/avV
		pT = ( pT # unity )/avV

           endif else begin

		pL = image[ xL1-1, yB:yT ]
		pR = image[ xR1+1, yB:yT ]
		pL = transpose( pL )
		pR = transpose( pR )
		pB = image[ xL:xR, yB1-1 ]
		pT = image[ xL:xR, yT1+1 ]
	   endelse

           direction = strupcase( direction )
           print," interpolation direction : ",direction

           CASE direction OF
		"H":	patch = vecLinterp( pL, pR, NP=xsiz )
		"V":	patch = transpose( vecLinterp( pB, pT, NP=ysiz ) )
		"HV": BEGIN
                   piH = vecLinterp( pL, pR, NP=xsiz )
                   piV = vecLinterp( pB, pT, NP=ysiz )
                   patch = ( transpose( piV ) + piH )/2
                END
		else: BEGIN
                   message,"must give direction: H,V, or HV",/INFO
                   return, image
                END
	   ENDCASE
        endelse

        if (!DEBUG GT 2) then stop

        if keyword_set( st_dev ) then begin
           print," adding Gaussian noise with standard devation =",st_dev
           patch = patch + st_dev * randomn( seed, xsiz, ysiz )
        endif

	patched_image[xL,yB] = patch

return, patched_image
end
