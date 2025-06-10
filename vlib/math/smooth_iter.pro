;+
; NAME:
;	smooth_iter
; PURPOSE:
;	Approximate convolution with a Gaussian acheived by iteration of
;	the IDL moving boxcar filter ( smooth( x, 3 ) ).
;	Equivalent Gaussian kernel FWHM is approximately 2 * sqrt( niter ) if niter >> 1.
;	Default is to also smooth the edges of images by splicing on rows & columns 
;	outside edges by reflecting values from adjacent inner rows & columns,
;	and then applying smooth function iteratively to the bigger image,
;	finally returning just the inner image.
;
;	Default is to reflect inner row & columns, but keywords provide other options.
; CALLING:
;	xs = smooth_iter( x, niter )
; INPUTS:
;	x = data array(s) (can be vector, vectors, image, or images)
;	niter = # of smooth( x, 3 ) iterations, default = 3.
; KEYWORDS:
;	/USEAVERAGE : sets the values of extra edges to be the average of actual edges
;	/DUPLICATE : sets the extra edges to be duplicates of actual edges
;	/NOEDGES : causes the edges NOT to be also filtered (so they do not change)
; OUTPUTS:
;	Function returns filtered data array of same size.
;	Note that the default action of reflecting the 2 edge rows/columns outward
;	creates the most stable iteration of smoothing.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1994.
;       2020 March, FV@UF: made filtering the edges the default.
;       2020 July,  FV@UF: reform input of (1,n) array to vector
;       2020 Oct, FV@UF: fixed bug in 1-D right edge wrong index.
;       2023 Oct, FV@UF: added option /VECTORS to smooth each 1-D vector of array
;-

function smooth_iter, x, niter, VECTORS=vecs, NOEDGES=noedges, USEAVERAGE=useav, DUPLICATE=dupe

	if N_elements( niter ) NE 1 then niter=3
	if (N_elements( x ) LT 5) OR (niter LE 0) then return,x

        sz = size( x )

        if( sz[0] eq 3 ) then begin
           imgs = x
           for i=0,sz[3]-1 do imgs[*,*,i] = smooth_iter( x[*,*,i], niter, $
                                                         NOE=noedges, USE=useav, DUP=dupe)
           return, imgs
        endif

        if( sz[0] eq 2 ) and keyword_set( vecs ) then begin
           vecs = x
           for i=0,sz[2]-1 do vecs[*,i] = smooth_iter( x[*,i], niter, $
                                                       NOE=noedges, USE=useav, DUP=dupe)
           return, vecs
        endif

        if NOT keyword_set( noedges ) then begin

           if (sz[0] eq 2) then begin

              if( sz[1] eq 1 ) then begin
;;                 message,/INFO,"First dimension size = 1, so reforming to vector..."
                 return, smooth_iter( reform(x),niter,NOEDG=noedges,USEA=useav,DUP=dupe )
              endif

              img = fltarr( sz[1]+2, sz[2]+2 )
              img[1,1] = x
              Lxr = sz[1]
              Lyr = sz[2]
              Lx = Lxr-1
              Ly = Lyr-1

              if keyword_set( useav ) then begin
                 
                 avgL = avg( x[ 0,*] )
                 avgR = avg( x[Lx,*] )
                 avgB = avg( x[*, 0] )
                 avgT = avg( x[*,Ly] )
                 avgBL = (avgB + avgL)/2
                 avgBR = (avgB + avgR)/2
                 avgTL = (avgT + avgL)/2
                 avgTR = (avgT + avgR)/2
                 Lx2 = Lxr+1
                 Ly2 = Lyr+1

                 for i=1,niter do begin
                    img[  0,*] = avgL
                    img[Lx2,*] = avgR
                    img[*,  0] = avgB
                    img[*,Ly2] = avgT
                    img[  0,0] = avgBL
                    img[Lx2,0] = avgBR
                    img[  0,Ly2] = avgTL
                    img[Lx2,Ly2] = avgTR
                    img = smooth( img, 3 )
                 endfor

              endif else if keyword_set( dupe ) then begin

                 Lx2 = Lxr+1
                 Ly2 = Lyr+1

                 for i=1,niter do begin
                    img[  0,0] = img[1, *]  ;Left
                    img[Lx2,0] = img[Lxr,*] ;right
                    img[0,  0] = img[*, 1]  ;bottom
                    img[0,Ly2] = img[*,Lyr] ;top
                    img = smooth( img, 3 )
                 endfor

              endif else begin

                 for i=1,niter do begin
                    img[  0,0] = rotate( img[1:2,   *], 5 ) ;Left
                    img[Lxr,0] = rotate( img[Lx:Lxr,*], 5 ) ;right
                    img[0,  0] = rotate( img[*,   1:2], 7 ) ;bottom
                    img[0,Lyr] = rotate( img[*,Ly:Lyr], 7 ) ;top
                    img = smooth( img, 3 )
                 endfor

              endelse

              return, img[1:Lxr,1:Lyr]

           endif else if (sz[0] eq 1) then begin

              curve = fltarr( sz[1]+2 )
              curve[1] = x
              Lxr = sz[1]
              Lx2 = Lxr+1
              Lx = Lxr-1

              for i=1,niter do begin
                 curve[0] = curve[1]
                 curve[Lx2] = curve[Lxr]
                 curve = smooth( curve, 3 )
              endfor

              return, curve[1:Lxr]

           endif
        endif

        ;; if /NOEDGES then use apply smooth iteratively:
        xs = x
        for i=1,niter do xs = smooth( xs, 3 )
        return, xs
end
