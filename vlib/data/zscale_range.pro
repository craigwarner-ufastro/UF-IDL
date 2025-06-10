;
;+
; NAME:
;
;       ZSCALE_RANGE
;
; PURPOSE:
;	Computes best range for displaying astronomical images using IRAF zscale algorithm.
;
;
; DESCRIPTION:
;
;		Returns the best low_cut and high cut values for the display
;		visualization of an image, on the base of the IRAF Zscale algorithm.
;
; CATEGORY:
;
;       Image Visualization.
;
; CALLING SEQUENCE:
;
;		Result = ZSCALE_RANGE(image, CONSTRAST=contrast, POINTS=npoints )
;
; INPUTS:
;		Image: 1-D or 2-D array.
;
; KEYWORDS:
;
;       POINTS: Number of sample points along each axis to use for the determination
;               of the range: total sample points = POINTS^2 for an image (default = 100).
;
;       NITER = # iterations of Linear fit, default = 1.
;
;	CONSTRAST = Constrast for the Zscale algorithm; default = 1.
;
;       /VERBOSE : causes sorted data values and linear fits to be plotted.
;
; OUTPUTS:
;
;       2-element vector of the form [low_cut, high-cut].
;
; PROCEDURE:
;
;	Makes a linear fit to the sorted values of the sample points and takes
;	the minumum and maximum	values of the fit.
;
; MODIFICATION HISTORY:
;
;       Feb 2004 - 	Gianluca Li Causi, INAF - Rome Astronomical Observatory
;					licausi@mporzio.astro.it
;					http://www.mporzio.astro.it/~licausi/
;
;	2012: mod by FV at UF, added /VERBOSE option, and made CONTRAST a keyword input.
;-

FUNCTION zscale_range, im, CONTRAST=contrast, POINTS=points, NITER=niter, VERBOSE=verbose

  if N_elements( im ) LT 1 then begin
     print,"Calling seq:"
     print,"[ rmin, rmax ] = Zscale_range( image_data, CONTRAST=, POINTS=, NITER=, /VERB )"
     return,[0,1]
  endif

  IF n_elements(contrast) ne 1 THEN contrast = 1.

  IF contrast LE 0 THEN contrast = 1.

  maxim = max( im, MIN=minim )

  IF minim EQ maxim THEN RETURN, [minim, maxim]
  if N_elements( im ) LT 3 then RETURN, [minim, maxim]

  IF n_elements(points) ne 1 THEN points = 100

  siz = size(im)

INIZIO:

  CASE siz[0] OF

	1: BEGIN
		lx = siz[1]
		np = points < lx	;numero di punti per la griglia
		xx = findgen(np) * (lx-1)/(np-1)
		imp = im[xx]
	END

	2: BEGIN
		lx = siz[1]
		ly = siz[2]

		np = points < min([lx, ly])	;numero di punti per la griglia

		xx = findgen(np) # replicate(1, np) * (lx-1)/(np-1)
		yy =  replicate(1, np) # findgen(np) * (ly-1)/(np-1)

		imp = im[xx,yy]
         END
        else: BEGIN
           message,/INFO,"not implemented yet for higher dimensions"
           RETURN, [minim, maxim]
        END
     ENDCASE

  IF min(imp) EQ max(imp) THEN BEGIN
	points = points * 5.
	GOTO, INIZIO
     ENDIF

  s = sort(imp)
  intens = imp[s]

  x = findgen(np^siz[0])

  if keyword_set( verbose ) then $
	plot, x, intens, ps=4, symsiz=0.5, TIT="Zscale: Linear fit to sorted data values"

;fit lineare

  coef = Linfit( x, intens, yfit=yfit )

  if keyword_set( verbose ) then oplot, x, yfit
  if N_elements( niter ) ne 1 then niter = 1
  soglia = 3.

  FOR i = 1, niter-1 DO BEGIN
	diff = ABS(intens - yfit)
	IF n_elements(diff) LE 1 THEN BREAK
	sig = stdev(diff)
	ok = where(diff LT soglia*sig, count)
	IF count LE 1 THEN BREAK
	x = x[ok]
	intens = intens[ok]
	coef = Linfit(x, intens, yfit=yfit)
	if keyword_set( verbose ) then oplot, x, yfit, LINE=i
     ENDFOR

  zmed = (min(yfit) + max(yfit))/2.
  xmed = (min(x) + max(x))/2.
  zmax = zmed + coef[1] * (max(x)-xmed) / contrast < maxim
  zmin = zmed + coef[1] * (min(x)-xmed) / contrast > minim

  if keyword_set( verbose ) then begin
     help,minim,maxim,contrast,niter,zmed,xmed,x,zmin,zmax,coef
     print,coef
     ix = [ 0, N_elements(x)-1 ]
     oplot, x[ix], [zmin,zmin],LIN=4
     oplot, x[ix], [zmax,zmax],LIN=4
  endif

  IF zmin EQ zmax THEN BEGIN
	zmin = minim
	zmax = maxim
     ENDIF

RETURN, [zmin, zmax]
END
