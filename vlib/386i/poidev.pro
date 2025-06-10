function poidev, xm, SEED = seed
;+
; NAME
;   POIDEV
; PURPOSE:
;   Return an integer random deviate drawn from a Poisson distribution with
;   a specified mean.    Adapted from procedure of the same name in 
;   "Numerical Recipes" by Press et al. (1986), Section 7.3
; CALLING SEQUENCE:
;    result = POIDEV( xm, [ SEED = ] )
; INPUTS:
;    xm - integer scalar or vector, specifying mean of the Poisson 
;         distribution
; OUTPUT:
;    result - integer scalar or vector, same size as xm
; OPTIONAL KEYWORD INPUT-OUTPUT:
;    SEED - floating point scalar, used as the seed for the random 
;           distribution.     This keyword can be used to have POIDEV
;           give identical results on consecutive runs.     
; EXAMPLE:
;    (1) Add Poisson noise to an integral image array, im
;          IDL> imnoise = POIDEV( im)
;    (2) Verify the expected mean  and sigma for an input value of 81
;          IDL> p = POIDEV( intarr(10000) + 81)       ;Test for 10,000 points
;          IDL> print,avg(p),sigma(p)
;        Average and sigma of the 10000 points should be close to 81 and 9
; METHOD: 
;    For small values (< 20) independent exponential deviates are generated 
;    until their sum exceeds the specfied mean, the number of events required 
;    is returned as the Poisson deviate.   For large (> 20) values, uniform
;    random variates are compared with a Lorentzian distribution function.
; NOTES:
;    Negative values in the input array will be returned as zeros.  
; PROCEDURES CALLED:
;    GAMMLN - returns log of the Gamma function
; REVISION HISTORY:
;    Version 1               Wayne Landsman        July  1992
;    Added SEED keyword                            September 1992
;-
  On_error,2

 Npts = N_elements( xm)
 if (Npts EQ 0) then message, $
      'ERROR - Poisson mean vector (first parameter) is undefined'

   index = where( xm LE 20, Nindex)

   if Nindex GT 0 then begin

   g = exp( -xm( index) )           ;To compare with exponential distribution
   em1 = replicate( -1, Nindex )    ;Counts number of events
   t = replicate( 1., Nindex )          ;Counts (log) of total time

  Ngood = Nindex
  good = lindgen( Nindex)                 ;GOOD indexes the original array
  good1 = good                         ;GOOD1 indexes the GOOD vector

 REJECT:  em1(good) = em1(good) + 1      ;Increment event counter
   t = t(good1)*randomu( seed, Ngood )   ;Add exponential deviate, equivalent
                                         ;to multiplying random deviate
   good1 = where( t GT g(good), Ngood1)  ;Has sum of exponential deviates 
                                         ;exceeded specified mean?
   if ( Ngood1 GE 1 ) then begin
           good = good( good1)
           Ngood = Ngood1
           goto, REJECT
   endif

 endif
     if ( Npts GT 1 ) then $
           output = make_array( SIZE = size(xm), /NOZERO )  else $
           output = lonarr(1)                      ;Output array
     if Nindex GT 0 then output( index) = em1
     if Nindex EQ Npts then return, output
; ***************************************

    big = where(xm GT 20, Nbig)
    xbig = xm(big)

    sq = sqrt( 2.*xbig )           ;Sq, Alxm, and g are precomputed
    alxm = alog( xbig )
    g = xbig * alxm - gammln( xbig + 1.)

    Ngood = Nbig  & Ngood1 = Nbig
    good = lindgen( Ngood)
    good1 = good
    y = fltarr(Ngood, /NOZERO ) & em = y


REJECT1:   y(good) = tan( !PI * randomu( seed, Ngood ) )  
   em(good) = sq(good)*y(good) + xbig(good)
   good2 = where( em(good) LT 0. , Ngood )
   if (Ngood GT 0) then begin
            good = good(good2)
            goto, REJECT1
   endif

   fixem = long( em(good1) )
   test = check_math( 0, 1)         ;Don't want overflow messages
   t = 0.9*(1. + y(good1)^2)*exp( fixem*alxm(good1) - $ 
               gammln( fixem + 1.) - g(good1) )
   good2 = where( randomu (seed, Ngood1) GT T , Ngood)
   if ( Ngood GT 0 ) then begin
            good1 = good1(good2)
            good = good1
            goto, REJECT1
   endif
   output( big ) = long(em)

 return, output

 end
