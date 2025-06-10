function round_off, numbers, LONG=Long

;F.V.1991
	s = size( numbers )
	vtype = s(s(0)+1)
	if (vtype LT 4) OR (vtype GT 6) then return, numbers

if keyword_set( Long ) then  return, Long( 0.5 + numbers - (numbers LT 0) ) $
			else  return, fix( 0.5 + numbers - (numbers LT 0) )
end
