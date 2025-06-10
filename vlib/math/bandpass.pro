;+
; NAME:
;	bandpass
; PURPOSE:
;	Compute flux thru bandpass filter of given transmission at wavelengths.
;	Optionally, flux and/or transmission is integrated in Log-Log space
;	(see keyword POWER_LAW), default is integration in Linear-Linear space.
; CALLING:
;	Fobs = bandpass( Lambda, Flux, W=waveT, T=Trans )
; INPUTS:
;	Lambda = wavelengths in microns.
;	Flux = radiation spectrum at wavelengths of Lambda array.
;		Units are assumed to be energy/Hertz.
; KEYWORDS:
;	WAVELENGTH = wavelengths (microns) of bandpass filter transmission.
;	TRANSMISSION = transmission coefficients at wavelengths.
;	/MICRON : then assumes Flux is in units of energy/micron.
;	/POWER_LAW_INT : use power law interp for flux, linear interp for
;		transmission, and power law integral for flux * transmission.
;	POWER_LAW_INT=2 : use power law interp/integral for flux & transmission.
; OUTPUTS:
;	Returns the integral of the product of Flux times Transmission.
; EXTERNAL CALLS:
;	function unique
;	function finterpol
;	function Trapow
;	function Trapez
; PROCEDURE:
;	Apply reform to inputs just in case they were passed as Flux(i,*) etc.
;	Interpolate both flux and transmission to single merged wavelength grid
;	and integrate the product. Optionally, flux and/or transmission
;	is interpolated/integrated in Log-Log space (see keyword POWER_LAW),
;	whereas default is interpolation/integration in Linear-Linear space.
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1995.
;	FV, 1998, mod to skip interp if wavelength grids match.
;-

function bandpass, Lambda, Flux, WAVELENGTH_BAND=waveT, TRANSMISSION=Trans, $
				MICRON_UNITS=per_micron, POWER_LAW_INT=powint

	Lambda = reform( Lambda )
	Flux = reform( Flux )
	waveT = reform( waveT )
	Trans = reform( Trans )

	minw = min( waveT, MAX=maxw )
	w = where( (Lambda GE minw) AND (Lambda LE maxw), nw )

	if nw EQ N_elements( waveT ) then begin
		if min( waveT eq Lambda(w) ) then begin
			wave = waveT
			FT = Flux(w) * Trans
		   endif
	   endif

	if N_elements( wave ) LE 0 then begin
		wave = [ Lambda, waveT ]
		wave = wave( sort( wave ) )
		wave = wave( where( (wave GE minw) AND (wave LE maxw) ) )
		wave = wave( unique( wave ) )
	   endif

	if keyword_set( powint ) then begin

	   if N_elements( FT ) LE 0 then begin

		wl = aLog( wave )
		FT = exp( finterpol( aLog( Flux ), aLog( Lambda ), wl,/QUIET ) )

		if powint GE 2 then begin
		 	FT = FT * $
			 exp( finterpol( alog( Trans ), alog( waveT ), wl,/Q ) )
		 endif else FT = FT * finterpol( Trans, waveT, wave,/Q )
	    endif

	   if keyword_set( per_micron ) then $
				return, Trapow( FT > 1e-37, wave ) $
			  else 	return, -3e14 * Trapow( FT > 1e-37, 1/wave )

	 endif else begin

		if N_elements( FT ) LE 0 then $
			FT = finterpol( Flux, Lambda, wave,/QUIET ) * $
				finterpol( Trans, waveT, wave,/QUIET )

		if keyword_set( per_micron ) then $
				return, Trapez( FT, wave ) $
			  else 	return, -3e14 * Trapez( FT, 1/wave )
	  endelse
end
