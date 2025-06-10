;+
; NAME:
;	match
;
; PURPOSE:
;	Routine to match values in two arrays,
;	by returning subscripts of the matching values.
;	The subscripts can be returned in 3 kinds of original orders
;	(see keyword /ORIGINAL for options).
;	Duplicates can be ignored in the matching process,
;	or they can be accounted for completely in the first array,
;	(see keywords). If desired option is not specified,
;	duplicates may or may not be matched by the straight algorithm.
;
; CALLING SEQUENCE:
;	match, a, b, suba, subb
;
; INPUTS:
;	a, b - two arrays in which to find matching elements
;
; KEYWORDS:
;	COUNT = number of matches found (optional output).
;	/UNIQUE_ONLY : match only the unique values (discard duplicates).
;	/DUPLICATES_IN_A : account for duplicates in first input array (a),
;		so that duplicates get the same subscripts and each group
;		gets mapped to a single element of second input array (b).
;	RESOLUTION = maximum distance defining a match (default=0),
;		option used to find numbers which are almost equal,
;		i.e. matching up to the specified resolution.
;		(default is resolution=0 so only exact equality is a match).
;	/ORIGINAL causes matching subscripts to give elements in original order
;		of the first array (a), and a(suba) = b(subb).
;		If ORIGINAL=2 the order of b(subb) is preserved.
;		If ORIGINAL>2 or <0 the orders are preserved independently.
;		However, if /DUP then ORIGINAL>1 or <0 means just /ORIGINAL.
;	/INFO causes informational messages on how many matches were found.
;
; OUTPUTS:
;	suba = subscripts of elements in vector a with a match in vector b.
;	subb = subscripts of the elements in vector b with matches in vector a.
;
;	suba and subb are ordered such that a(suba) = b(subb)
;	with increasing values (unless /ORIGINAL_ORDER options are invoked).
;
; EXTERNAL CALLS:
;	function unique		(if keywords /UNIQUE or /DUPLICATES are set)
;	function remix		(if keyword /DUPLICATES is set)
; PROCEDURE:
;	Find matches by finding duplicates in sorted combined list,
;	keeping track of which element comes from which list.
;	For the /DUPLICATES_IN_A option, repeatedly match and remove duplicates.
; HISTORY:
;	D. Lindler 1986.
;	F. Varosi, 1990, optimized storage: flag = byte array, dup is reused.
;			added option to find matches within RESOLUTION bin size.
;	F. Varosi, 1992, added /ORIGINAL_ORDER and COUNT= options.
;	F. Varosi, 1995, added /UNIQUE_ONLY and /DUPLICATES_IN_A options.
;-

pro match, a,b, suba, subb, COUNT=count, RESOLUTION=resolution, $
				ORIGINAL_ORDER=original_order, INFO=info, $
				UNIQUE_ONLY=uniq_only, DUPLICATES_IN_A=dups_ina

;First check for special cases and options:

	na = N_elements( a )
	nb = N_elements( b )

	if (na LE 0) OR (nb LE 0) then begin
		message,"must supply two arrays for comparison",/INFO
		return
	   endif

	if keyword_set( uniq_only ) then begin

		ua = unique( a,/SORT, ORIG=original_order, RES=resolution )
		ub = unique( b,/SORT, ORIG=original_order, RES=resolution )
		match, a(ua), b(ub), suba, subb, COUNT=count, INFO=info, $
					ORIG=original_order, RES=resolution

		if count GT 0 then begin
			suba = ua(suba)
			subb = ub(subb)
		   endif

		return
	   endif

	if (na EQ 1) OR (nb EQ 1) then begin
		if (nb GT 1) then begin
			if keyword_set( resolution ) then $
				subb = where( abs(a(0)-b) LE resolution, nw ) $
			  else	subb = where( b EQ a(0), nw )
			if (nw GT 0) then suba = replicate(0,nw) else suba=[-1]
		  endif else begin
			if keyword_set( resolution ) then $
				suba = where( abs(b(0)-a) LE resolution, nw ) $
			  else	suba = where( a EQ b(0), nw )
			if (nw GT 0) then subb = replicate(0,nw) else subb=[-1]
		   endelse
		count = nw
		if keyword_set( info ) then message,/INFO,"found " + $
				strtrim( count, 2 ) + " matches"
		return
	   endif

	if keyword_set( dups_ina ) then begin

		ua = unique( a,/SORT, RES=resolution )
		ub = unique( b,/SORT, RES=resolution )
		bb = b(ub)
		match, a(ua), bb, suba, subb, COUNT=count, RES=resolution

		if count LE 0 then begin
			if keyword_set(info) then message,"found 0 matches",/INF
			return
		   endif

		suba = ua(suba)
		subb = ub(subb)
		inx = Lindgen( N_elements( a ) )
		ar = a

		while N_elements( ar ) GT N_elements( ua ) do begin
			ix = remix( REMOVE=ua, NINDEX=N_elements(ar) )
			ar = ar(ix)
			inx = inx(ix)
			ua = unique( ar,/SORT, RES=resolution )
			match, ar(ua), bb, sa, sb, COUNT=n, RES=resolution

			if n GT 0 then begin
				suba = [ suba, inx(ua(sa)) ]
				subb = [ subb, ub(sb) ]
				count = count + n
			  endif else ar=0
		  endwhile

		if keyword_set( original_order ) then begin
			sa = sort( suba )
			suba = suba(sa)
			subb = subb(sa)
		   endif

		if keyword_set( info ) then message,/INFO,"found " + $
				strtrim( count, 2 ) + " matches"
		return
	   endif

;Start the regular part of algorithm:

	c = [a,b]			;combined list of a and b and sort.
	sub = sort( c )
	c = c(sub)

	ind = [Lindgen( na ),Lindgen( nb )]	;combined list of indices
	ind = ind(sub)

	flag = [bytarr( na ),replicate( 1B, nb )]	;to indicate which array
	flag = flag(sub)
	sub = 0

;Find matches by finding duplicates in sorted combined list:

	if keyword_set( resolution ) then begin

		dup = where( ( abs( shift(c,-1) - c ) LE resolution ) $
					AND ( flag NE shift(flag,-1) ), count )
	  endif else begin

		dup = where( (c EQ shift( c,-1 )) AND $
				(flag NE shift( flag,-1 )), count )
	   endelse

	c = 0
	if keyword_set( info ) then message,"found " + $
				strtrim( count, 2 ) + " matches",/INFO

	if (count LT 1) then begin		;any found?
		suba = [-1]
		subb = [-1]
		return
	   endif

	dup = transpose( [[dup],[dup+1]] )	;Location of found duplicates
	ind = ind(dup)				;indices of duplicates
	flag = flag(dup)			;array id of duplicates
	suba = ind( where( flag EQ 0 ) )	; a subscripts matching with
	subb = ind( where( flag EQ 1 ) )	; b subscripts

	if keyword_set( original_order ) then begin
		if (original_order EQ 1) then begin
			sa = sort( suba )
			suba = suba(sa)
			subb = subb(sa)
		 endif else if (original_order EQ 2) then begin
			sb = sort( subb )
			suba = suba(sb)
			subb = subb(sb)
		  endif else begin
			suba = suba(sort( suba ))
			subb = subb(sort( subb ))
		   endelse
	   endif
end
