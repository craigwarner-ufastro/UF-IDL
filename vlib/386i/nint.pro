function nint,x,LONG = long             ;Nearest Integer Function
;+
; NAME:
;   NINT
; PURPOSE:
;   Nearest integer function
; CALLING SEQUENCE:
;   result = nint(x, [ /LONG] )
; INPUT:
;   X - An IDL variable, scalar or vector, usually floating or double
;       Unless the LONG keyword is set, X must be between -32767.5 and 
;       32767.5 to avoid integer overflow
; OUTPUT
;   RESULT - Nearest integer to X
; OPTIONAL KEYWORD INPUT:
;   LONG - If this keyword is set and non-zero, then the result of NINT
;          is of type LONG.   Otherwise, the result is of type INTEGER
; EXAMPLE:
;   If X = [-0.9,-0.1,0.1,0.9] then NINT(X) = [-1,0,0,1]
; REVISION HISTORY:
;   Written W. Landsman        January 1989
;-
if keyword_set( LONG ) then return, long( x + 0.5 - (x LT 0) ) else $
                            return,  fix( x + 0.5 - (x lt 0) )
end
