Function Lee_filt, image, radius, sigma, variance
;+
; NAME:
;	Lee_filt
; PURPOSE:
;	Performs the Lee filter algorithm on the input
;	image (or vector) using a box of side 2*radius+1.
; CATEGORY:
;	E3 Smoothing (image)
; CALLING SEQUENCE:
;	Result = Lee_filt( image, radius, sigma )
; INPUTS:
;	image = Input image array or one dimensional vector
;	radius : Side of filter box = 2*radius+1.  Default value = 1.
;	sigma = Estimate of the standard deviation. Def = average Var of image.
; OUTPUTS:
;	Function returns the result of the filtering,
;	optional output is variance of image, if requested in arg.list.
; PROCEDURE:
;	The LEE (Computer Graphics 197?) technique smooths additive
;	image noise by generating statistics in a local neighborhood
;	and comparing them to the expected values:
;		Var( image ) * image  +  Smooth( image ) * sigma
; filter   =	------------------------------------------------
;			Var( image )  +  sigma
;	Code reformulates above with adds & subtracts instead of multiplies.
; MODIFICATION HISTORY:
;	Written, 24-Nov-1982, R. A. Howard, Naval Research Lab, Wash.DC 20375
;	Modified, Jan.1991, Frank Varosi, STX @ NASA/GSFC, cleaned it up.
;-
	if N_elements( radius ) NE 1 then radius = 1
	width = (2 * radius + 1) > 3

	mean = smooth( float(image), width ) 		;mean of each box.
	imm = image - mean
	var = smooth( imm^2, width ) > 0		;variance of each box.

	if N_elements( sigma ) NE 1 then begin

		sigma = sqrt( total( var )/N_elements( var ) )
		message,"variance window size="+strtrim(width,2),/CON,/INF
		message,"estimating optimal Sigma="+strtrim(sigma,2),/CON,/INF

	  endif else  sigma = float( sigma )

	if N_params(0) GE 4 then variance = var
	var = var/( var + sigma^2 )

return, imm * var + mean
end
