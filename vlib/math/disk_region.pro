;+
; NAME:
;	Disk_Region
; PURPOSE:
;	Returns a set of indices mapping the inside of circle or annulus.
; CALLING:
;	disk_indices = Disk_Region( center, SIZE=sz, RADIUS=radius, COUNT=npix )
; INPUTS:
;	center =
; KEYWORDS:
;	SIZE =
;	RADIUS =
;	INNER_RADIUS =
;	COUNT =
; OUTPUTS:
;
; PROCEDURE:
;
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1997.
;	Mod: F.V. at U.F., 2007: added keyword option INNER_RADIUS=
;	Mod: F.V. at U.F., 2017: fixed accuracy: shift by 0.5 (half pixel)
;-

function Disk_Region, center, SIZE=sz, RADIUS=radius, COUNT=npix, INNER_RADIUS=inrad

	if N_elements( center ) ne 2 then begin
           npix = 0
           message,/INFO,"must input 1st arg. as two element array = center"
           return,(-1)
        endif

	if N_elements( sz ) ne 2 then begin
           npix = 0
           message,/INFO,"must specify two element array SIZE=[nx,ny]"
           return,(-1)
        endif

	if N_elements( sz ) ne 2 then sz = 2 * center

        d2 = [(findgen( sz[0] ) - center[0])^2] # replicate( 1, sz[1] ) $
             + replicate( 1, sz[0] ) # [(findgen( sz[1] ) - center[1])^2]

	if keyword_set( inrad ) then begin

           radin = inrad < (radius-1)
           return, where( (d2 LT radius^2) and (d2 ge radin^2), npix )

        endif else return, where( d2 LT radius^2, npix )
end
