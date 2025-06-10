;+
; NAME:
;	astro_ticks
; PURPOSE:
;	Create nice Labels for R.A. or DEC given range of degrees.
; CALLING:
;	astro_ticks, degrees, nticin, ticvals, ticLabs, mtics
; INPUTS:
;	degrees = array of degrees.
;	nticin = suggested # of ticks.
; KEYWORDS:
;	/RA : to indicate that Labels are for R.A. axis. (hours,..etc.)
; OUTPUTS:
;	ticvals = float array, location of major ticks.
;	ticLabs = string array, Labels for major ticks.
;	mtics = # of minor tick marks.
; EXTERNAL CALLS:
;	function Ten
;	function sixty
; PROCEDURE:
;	Very many "if" statements.
; HISTORY:
;	Written: Frank Varosi NASA/GSFC 1989, based on
;		"tics.pro" and "ticlabels.pro" from IDL-astron-Library.
;	F.V. 1991, make seconds start on multiple of 5 or 2 if inc = 5 or 2 .
;-

pro astro_ticks, degrees, nticin, ticvals, ticLabs, mtics, RA=ra

	Np = N_elements( degrees )
	dmax = degrees(Np-1)
	dmin = degrees(0)

	if keyword_set( ra ) then  minut_deg = 4.  else  minut_deg = 60.
	minut_deg = double( minut_deg )

	minutes = minut_deg * abs( dmax - dmin )
	ntic = nticin
	incm = minutes/ntic

	CASE 1 OF			;determine increment (in minutes)

	(incm GE 180.0 ) : BEGIN incm=240.0   & mtics= 4 & END ; 4 hours
	(incm GE  90.0 ) : BEGIN incm=120.0   & mtics= 2 & END ; 2 hours
	(incm GE  40.0 ) : BEGIN incm= 60.0   & mtics= 2 & END ; 1 hour
	(incm GE  20.0 ) : BEGIN incm= 30.0   & mtics= 3 & END ; 30 minutes 
	(incm GE  12.0 ) : BEGIN incm= 15.0   & mtics= 3 & END ; 15 minutes
	(incm GE   7.0 ) : BEGIN incm= 10.0   & mtics=10 & END ; 10 minutes
	(incm GE   3.0 ) : BEGIN incm=  5.0   & mtics= 5 & END ;  5 minutes
	(incm GE   1.5 ) : BEGIN incm=  2.0   & mtics= 4 & END ;  2 minutes
	(incm GE   0.8 ) : BEGIN incm=  1.0   & mtics= 6 & END ;  1 minute
	(incm GE   0.4 ) : BEGIN incm=  0.5   & mtics= 3 & END ; 30 seconds
	(incm GE   0.2 ) : BEGIN incm=  0.25  & mtics= 3 & END ; 15 seconds
	(incm GE   0.1 ) : BEGIN incm= 10/60. & mtics=10 & END ; 10 seconds
	(incm GE   0.05) : BEGIN incm=  5/60. & mtics= 5 & END ;  5 seconds
	(incm GE   0.02) : BEGIN incm=  2/60. & mtics= 4 & END ;  2 seconds
	(incm GE   0.01) : BEGIN incm=  1/60. & mtics=10 & END ;  1 second
	(incm GT  .0035) : BEGIN incm= .5/60  & mtics= 5 & END ; .5 second
	else             : BEGIN incm= .2/60  & mtics= 4 & END ; .2 second
	ENDCASE

	ntic = fix( minutes/incm )+2
	ticvals = dblarr( ntic )
	ticLabs = strarr( ntic )

	if keyword_set( ra ) then begin		;convert to hours,minutes,secs

		rasc = sixty( dmin , /RA )
		minhd = fix( rasc(0) )
		minhd = (minhd + 24) MOD 24
		minm = fix( rasc(1) )
		mins = fix( rasc(2) )
		hord = "!Ah!N"
		m = "!Am!N"
		s = "!As!N"
		direcL = -1
		direcV = -1

	 endif else begin		;convert dmin to degrees, minutes, secs

		if (dmin LT 0) then  direcL = -1  else  direcL = 1
		direcV = 1
		dec = sixty( dmin )
		minhd = fix( dec(0) )
		minm = fix( dec(1) )
		mins = fix( dec(2) + ( direcL > 0 ) )
 		hord = "!Ao!N "
		m = "' "
		s = '"'
	  endelse

	if (minutes GT 2) AND (mins NE 0) then begin

		mins = 0
		minm = minm + direcL

		if (minm GE 60) OR (minm LT 0) then begin
			minm = 0
			minhd = minhd+1
		   endif

	 endif else if (incm GT .08) AND ( (mins MOD 5) NE 0) then begin

		mins = mins - (mins MOD 5) + 5 * ((direcL+1)/2)

		if (mins GE 60) OR (mins LT 0) then begin
			mins = 0
			minm = minm + direcL
		   endif

	  endif else if (incm GT .03) AND ( (mins MOD 2) NE 0) then begin

		mins = mins - (mins MOD 2) + 2 * ((direcL+1)/2)

		if (mins GE 60) OR (mins LT 0) then begin
			mins = 0
			minm = minm + direcL
		   endif
	   endif

	ticvals(0) = Ten( minhd, minm, mins, RA=ra )	;first Label position.

	if (incm lt 1/60.)  then begin		;fractional SECONDS

		inc = incm*60*direcL
		incd = direcV * abs( inc )/60./minut_deg

		if keyword_set( ra ) then minsec = rasc(2)  else minsec = dec(2)

		mins = fix( minsec/inc ) * inc
		ticvals(0) = Ten( minhd, minm, mins , RA=ra )

		ticLabs(0) = strtrim( minhd, 2 ) + hord  $
			   + strtrim( minm, 2 ) + m      $
			   + string( mins, FORM="(f4.1)" ) + s

		for i = 1,ntic-1 do begin

			mins = mins + inc
			ticvals(i) = ticvals(i-1) + incd

			if (mins GE 60) then begin

				mins = mins - 60
				minm = minm + 1
				ticLabs(i)= strtrim( minm, 2 ) + m  $
					+ string( mins, FORM="(f4.1)" ) + s

			 endif else if (mins LE 0) then begin

				if (mins LT 0) then mins = mins + 60
				minm = minm - 1
				ticLabs(i)= strtrim( minm, 2 ) + m  $
					+ string( mins, FORM="(f4.1)" ) + s

			   endif else $
				ticLabs(i) = string( mins,FORM="(f4.1)") + s
		  endfor

	endif else if (abs(mins) ge 1) or (incm lt 1.0)  then begin	;SECONDS

		inc = fix( incm*60 )*direcL
		incd = direcV * abs( inc )/60./minut_deg

		ticLabs(0) = strtrim( minhd, 2 ) + hord  $
			   + strtrim( minm, 2 ) + m      $
			   + strtrim( mins, 2 ) + s

		for i = 1,ntic-1 do begin

			mins = mins + inc
			ticvals(i) = ticvals(i-1) + incd

			if (mins GE 60) then begin

			    mins = mins - 60
			    minm = minm + 1

			    if (minm GE 60) then begin

				minm = minm - 60
				minhd = minhd + 1
				ticLabs(i)= strtrim( minhd, 2 ) + hord  $
					  + strtrim( minm, 2 ) + m	$
					  + strtrim( mins, 2 ) + s

			     endif else ticLabs(i)= strtrim( minm, 2 ) + m  $
						  + strtrim( mins, 2 ) + s

			 endif else if (mins LE 0) then begin

			    if (mins LT 0) then begin
				mins = mins + 60
				minm = minm - 1
				endif

			    if (minm LE 0) then begin

				if (minm LT 0) then begin
					minm = minm + 60
					minhd = minhd + 1
				   endif

				ticLabs(i)= strtrim( minhd, 2 ) + hord  $
					  + strtrim( minm, 2 ) + m	$
					  + strtrim( mins, 2 ) + s

			     endif else ticLabs(i)= strtrim( minm, 2 ) + m  $
						  + strtrim( mins, 2 ) + s

			    if (mins EQ 0) then mins = 60

			   endif else  ticLabs(i) = strtrim( mins, 2 ) + s
		  endfor

	 endif else if (abs(minm) ge 1) or (incm lt 60.0) then begin	
									;MINUTES
		inc = fix( incm )*direcL
		incd = direcV * abs( inc )/minut_deg

		ticLabs(0) = strtrim( minhd, 2 ) + hord  $
			   + strtrim( minm, 2 ) + m

		for i = 1,ntic-1 do begin

			minm = minm + inc
			ticvals(i) = ticvals(i-1) + incd

			if (minm GE 60) then begin

				minm = minm - 60
				minhd = minhd + 1
				ticLabs(i)= strtrim( minhd, 2 ) + hord  $
					  + strtrim( minm, 2 ) + m

			 endif else if (minm LE 0) then begin

				if (minm LT 0) then minm = minm + 60
				minhd = minhd + 1
				ticLabs(i)= strtrim( minhd, 2 ) + hord  $
					  + strtrim( minm, 2 ) + m

				if (minm EQ 0) then minm = 60

			  endif else  ticLabs(i) = strtrim( minm, 2 ) + m
		  endfor

	 endif else begin			;Hours/Degrees

		inc = fix( incm/60 )
		incd = direcV * abs( inc ) * 60/minut_deg

		for i = 0,ntic-1 do begin

			ticvals(i) = ticvals(i-1) + incd
			ticLabs(i) = strtrim( minhd, 2 ) + hord
			minhd = minhd + inc
		  endfor      
	   endelse

	if (mtics GT 3) AND ( (mtics MOD 2) EQ 0 ) then begin

		ticval2 = ticvals + incd/2
		ticLab2 = replicate( " ", ntic )
		ticvals = transpose( [ [ticvals],[ticval2] ] )
		ticLabs = transpose( [ [ticLabs],[ticLab2] ] )
		mtics = mtics/2
	   endif

	if keyword_set( ra ) then $
		w = where( (ticvals LE dmin) AND (ticvals GT dmax), ntic ) $
	 else $
		w = where( (ticvals LT dmax) AND (ticvals GE dmin), ntic )

	if (ntic GT 0) then begin
		ticvals = ticvals(w)
		ticLabs = ticLabs(w)
	   endif
end
