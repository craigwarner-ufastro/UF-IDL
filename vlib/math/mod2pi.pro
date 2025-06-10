function mod2pi, xin, ZERO_CENTERED=zero_centered

	TwoPi = 2*!PI

	if keyword_set( zero_centered ) then  x = ( xin + !PI ) MOD  TwoPi  $
					else  x = xin MOD TwoPi

	Lz = where ( x LT 0, nn )
	if (nn GT 0) then x(Lz) = x(Lz) + TwoPi

	if keyword_set( zero_centered ) then  x = x - !PI

return ,x
END
