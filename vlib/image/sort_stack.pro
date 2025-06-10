;+
; NAME:
;	sort_stack
; PURPOSE:
;	Given a stack of images (3-D matrix),
;		sort each stack of pixels into ascending order.
; CALLING:  
;	sort_stack, images
; INPUT:
;	images = stack of images (3-D matrix) to be sorted.
; OUTPUT:
;	images = result with each vertical stack of pixels ordered.
; METHOD:
;	When number of images <= 7 apply bubble-sort with gather/scatter
;	(using MSORT function), otherwise just loop over all pixel stacks.
; HISTORY:
;	F.Varosi NASA/GSFC 1992
;	F.Varosi UF-astro 2015 :  cleaned up code (use [*] for indices)
;-

pro sort_stack, images

  sm = size( images )

  if (sm[0] NE 3) then begin
     message,"expecting stack of images (3-D matrix)",/INFO
     return
  endif

  nx = sm[1]
  ny = sm[2]
  nstack = sm[3]

  if (nstack LE 7) then begin

     if (nx GE ny) then begin
        for j=0,ny-1 do images[0,j,0] = reform( msort( reform( images[*,j,*] ), /SORT ), nx,1,nstack )
     endif else begin
        for i=0,nx-1 do images[i,0,0] = reform( msort( reform( images[i,*,*] ), /SORT ), 1,ny,nstack )
     endelse

  endif else begin              ;in this case faster to Loop and call IDL sort.

     for j=0,ny-1 do begin
        for i=0,nx-1 do images[i,j,0] = images[ i,j, sort( images[i,j,*] ) ]
     endfor

  endelse

end
