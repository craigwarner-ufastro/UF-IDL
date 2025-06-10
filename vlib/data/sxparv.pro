;;-------------------------------------------------------------------------------------------------
;; Apply the function sxpar to an array of FITS headers to get vector of values for a keyword.
;; F.V @UF Oct.2020
;; Also handle array of pointers to FITS headers.
;; F.V.@UF Oct.2021
;;-------------------------------------------------------------------------------------------------

function sxparv, fitsheads, keyword

  if min( ptr_valid( fitsheads )) gt 0 then begin

     nfh = N_elements( fitsheads )
     value = sxpar( *fitsheads[0], keyword )
     values = replicate( value, nfh )
     for i=1,nfh-1 do values[i] = sxpar( *fitsheads[i], keyword )
     return, values

  endif else begin

     szf = size( fitsheads )

     if( szf[0] eq 2 ) then begin
        nfh = szf[2]
        value = sxpar( fitsheads[*,0], keyword )
        values = replicate( value, nfh )
        for i=1,nfh-1 do values[i] = sxpar( fitsheads[*,i], keyword )
        return, values
     endif else return, sxpar( fitsheads, keyword )

  endelse
end
