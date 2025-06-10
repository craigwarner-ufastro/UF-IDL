;+
;Generate a model psf (Gaussian or Lorentzian type profiles).
;If npix is odd, psf peak is centered in single center pixel.
;If npix is even, psf peak is centered between 4 middle pixels.
;Frank Varosi NASA/GSFC 1992.
;-

function psf_model, npix

  text = ""
  print," PSF type ?"
  print,"  G = Gaussian  (default)"
  print,"  L = Lorentzian-type"
  read, text
  psf_type = strupcase( text )

  text = ""
  while strlen( text ) LE 0 do read," FWHM (pixels) ? ",text
  psf_fwhm = float( get_words( text ) )

  if (psf_type EQ "L") then begin

     if N_elements( npix ) LE 0 then npix = 2 * fix( 8 * max( [psf_fwhm] ) )/2 + 1
     text = ""
     read," Lorentzian power ( > 1 , or return for default ) ? ",text
     if strlen( text ) GT 0 then psf_power = float( get_words( text ) )

     if N_elements( psf_power ) GE 1 then begin

        text = ""
        read," power decay ( < 1 ) ? ",text
        if strlen( text ) LE 0 then psf_decay = 0 else psf_decay = float( get_words( text ) )
        return, psf_Lorentzian( RADII=psf_fwhm/2.,NPIX=npix,POW=psf_power,PSCAL=psf_decay,/NORM,NDIM=2 )

     endif else return, psf_Lorentzian( FWHM=psf_fwhm, NPIX=npix,/NORM, NDIM=2 )

  endif else begin

     if N_elements( npix ) LE 0 then npix = 2 * fix( 4 * max( [psf_fwhm] ) )/2 + 1
     return, psf_gaussian( FWHM=psf_fwhm, NPIX=npix, /NORM, NDIM=2 )
  endelse		
end
