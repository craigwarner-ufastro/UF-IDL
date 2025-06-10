;+
; NAME:
;	Bin3D
; PURPOSE:
;	Create a volume density (3D histogram) from arrays of (x,y,z) points,
;	or create a voxel matrix from arrays of ( x,y,z, f(x,y,z) ) data.
;	In first case each voxel counts # of (x,y,z) points falling into
;	a 3D bin (box), thus forming an image of counters. In optional case,
;	each voxel value is the average of f(x,y,z) data falling in box.
;	Boxes are determined by dividing the (x,y,z) range into a uniform grid.
; CALLING:
;	imh = Bin3D( x, y, z, NVOX=64, XRAN=[0,20], YRAN=[-5,5] )
;
;	imz = Bin3D( x, y, z, FXYZ=z, NVOX=[200,100,50] )
;
; INPUTS:
;	x = array (any dimension) of x values.
;	y = array of y values, should correspond to x array.
;	z = array of z values, corresponding to x array.
;
;	Optionally, x can be of the form [[x],[y],[z]]
;		containing all x, y and z coordinates as rows of matrix,
;		and then arguments y and z should not be specified.
; KEYWORDS:
;	XRAN, YRAN, ZRAN : specify the x,y,z range to be mapped into the image.
;			otherwise the defaults = min-max ranges of x and y.
;	NVOXELS = 1 or 3 element integer array specifying size of result,
;			(single value means square image), default = [64,64,64].
;      /NOCLIP : do not bother checking if (x,y,z) are within range (faster).
;	TYPE_VAR = type code specifying the IDL variable type of result,
;		(1=byte, 2=short, 3=Long, 4=float,... default=2, short integer).
;
; KEYWORDS (optional):
;	VOXEL_DENSITY = an existing image of counters (3D histogram)
;			to which the result is added (overrides NVOX=).
;	FXYZ = array giving f(x,y,z) for the purpose of binning into voxels,
;		however, bins with no (x,y,z) data points are left = zero.
;		(NOTE: must specify XRAN, YRAN, ZRAN, or set /NOCLIP).
;   if /BOTH is set and FXYZ=f is given, then the binned image of z=f(x,y,z) is
;		returned by function, and voxel density of (x,y,z) is
;		returned via the keyword VOXEL_DENSITY.
; OUTPUTS:
;	Result of function is voxel density of (x,y,z) points, or an
;	scalar field of voxels if f(x,y,z) values are given.
; PROCEDURE:
;	Binning is performed by finding number of (x,y,z) duplicates
;	at each voxel,  using the IDL histogram, sort and where functions.
; HISTORY:
;	written Frank Varosi, U.of MD., 1988.
;		F.V. 1990, modif. for IDL-V2.
;-

function Bin3D, x, y, z, FXYZ=fxyz, SUMF=sumf, NVOXELS=npix, LOCATIONS=Loc, $
			XRAN=xran, YRAN=yran, ZRAN=zran, $
			TYPE_VARIABLE=vtype, NOCLIP=noclip, JUST_LOCS=just, $
			VOXEL_DENSITY=voxel_density, BOTH=both

  common Bin3D, xminc, xmaxc, yminc, ymaxc, zminc, zmaxc

	sim = size( voxel_density )

	if (sim(0) EQ 3) then  npix = sim(1:3)  else begin
		if N_elements( npix ) LE 0 then npix = [64,64,64]
		if N_elements( npix ) EQ 1 then npix = replicate( npix(0), 3 )
		npix = Long( npix ) < Long( 2.^15-1 )
		if N_elements( vtype ) NE 1 then vtype=2
		voxel_density = 0
	 endelse

	sx = size( x )
	XYZcombined = (sx(0) EQ 2) AND (N_params() LT 2)
	if (XYZcombined) then np = sx(1) $
		else np = N_elements(x) < N_elements(y) < N_elements(z)

	if (np LE 0) then begin
		message,"no (x,y,z) points",/INFO
		return, voxel_density
	   endif

	if N_elements( xran ) EQ 2 then begin
		xmin = xran(0)
		xmax = xran(1)
	  endif else if (N_elements( xmaxc ) EQ 1) AND $
	   		(N_elements( xminc ) EQ 1) then begin
		xmin = xminc
		xmax = xmaxc
	   endif else begin
		if (XYZcombined) then xmax = max( x(*,0), MIN=xmin ) $
				else xmax = max( x, MIN=xmin )
	    endelse

	if N_elements( yran ) EQ 2 then begin
		ymin = yran(0)
		ymax = yran(1)
	  endif else if (N_elements( ymaxc ) EQ 1) AND $
	   		(N_elements( yminc ) EQ 1) then begin
		ymin = yminc
		ymax = ymaxc
	   endif else begin
		if (XYZcombined) then ymax = max( x(*,1), MIN=ymin ) $
				else ymax = max( y, MIN=ymin )
	    endelse

	if N_elements( zran ) EQ 2 then begin
		zmin = zran(0)
		zmax = zran(1)
	  endif else if (N_elements( zmaxc ) EQ 1) AND $
	   		(N_elements( zminc ) EQ 1) then begin
		zmin = zminc
		zmax = zmaxc
	   endif else begin
		if (XYZcombined) then zmax = max( x(*,1), MIN=zmin ) $
				else zmax = max( y, MIN=zmin )
	    endelse

;Data scaling and clipping:

	xsiz = float( xmax - xmin )/npix(0)
	ysiz = float( ymax - ymin )/npix(1)
	zsiz = float( zmax - zmin )/npix(2)

	if (XYZcombined) then begin
		ix = fix( (x(*,0)-xmin)/xsiz )
		iy = fix( (x(*,1)-ymin)/ysiz )
		iz = fix( (x(*,2)-zmin)/zsiz )
	  endif else begin
		ix = fix( (x-xmin)/xsiz )
		iy = fix( (y-ymin)/ysiz )
		iz = fix( (z-zmin)/zsiz )
	    endelse

	if NOT keyword_set( noclip ) then begin

		wp = where ( (ix GE 0) AND (ix LT npix(0)), n )
		if (n LE 0) then return, voxel_density

		if (n LT N_elements( ix )) then begin
			ix = ix(wp)
			iy = iy(wp)
			iz = iz(wp)
		   endif

		wp = where ( (iy GE 0) AND (iy LT npix(1)), n )
		if (n LE 0) then return, voxel_density

		if (n LT N_elements( iy )) then begin
			ix = ix(wp)
			iy = iy(wp)
			iz = iz(wp)
		   endif

		wp = where ( (iz GE 0) AND (iz LT npix(2)), n )
		if (n LE 0) then return, voxel_density

		if (n LT N_elements( iz )) then begin
			ix = ix(wp)
			iy = iy(wp)
			iz = iz(wp)
		   endif
	   endif

;Perform Binning of data:

	Loc = ix + iy*npix(0) + iz*(npix(0)*npix(1))	;Location indices.

	if N_elements( Loc ) GT 1 then begin

		soL = sort( Loc )
		Loc = Loc(soL)				;find unique Locations.
		Wuniq = where( shift(Loc,-1) NE Loc, Nuniq )
		if (Nuniq LE 0) then Wuniq = N_elements( Loc )-1
		Loc = Loc(Wuniq)
		if keyword_set( just ) then return, Loc
		Kdup = Wuniq(0)+1			;get # duplicates
		if (Nuniq GT 1) then Kdup = [ Kdup, Wuniq(1:*)-Wuniq ]

	 endif else begin

		soL = [0]
		Wuniq = [0]
		Kdup = [1]
	  endelse

	if N_elements( fxyz ) GE N_elements( soL ) then begin

		sz = size( fxyz )
		fvoxels = make_array( DIM=npix, TYPE=sz(sz(0)+1) )
		fvoxels(Loc) = fxyz(soL(Wuniq))

		if !DEBUG then stop
		kd = Kdup-1
		w = where( kd GT 0, ndup )	;if there are duplicates,    
						; total up the scalar values.
		while ( ndup GT 0 ) do begin
 			Wuniq = Wuniq - 1
			fvoxels(Loc(w)) = fvoxels(Loc(w)) + fxyz(soL(Wuniq(w)))
			kd = kd-1
			w = where( kd GT 0, ndup )  ;check for more duplicates.
		 endwhile

		if NOT keyword_set( sumf ) then $
			fvoxels(Loc) = fvoxels(Loc) / Kdup

		if keyword_set( both ) then begin
			if N_elements( vtype ) NE 1 then vtype=2
			voxel_density = make_array( DIM=npix, TYPE=vtype )
			voxel_density(Loc) = Kdup
		   endif

		return, fvoxels

	  endif else begin

		if (sim(0) NE 3) then begin

			voxel_density = make_array( DIM=npix, TYPE=vtype )
			voxel_density(Loc) = Kdup

		 endif else voxel_density(Loc) = voxel_density(Loc) + Kdup

		return, voxel_density

	   endelse
end
