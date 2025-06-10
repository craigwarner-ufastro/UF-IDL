;Frank Varosi NASA/GSFC 1992.()

function Poisson_Noise, X_mean, MINBINS=minbins, GAUSSIAN_CUT_IN=gausscut

	if N_elements( minbins ) NE 1 then minbins=200
	if N_elements( gausscut ) NE 1 then gausscut=40

	sx = size( X_mean )

	if (sx[0] LT 1) then begin
		message,"input should be an array of mean values",/INFO
		return, X_mean
	   endif

	if (sx(sx[0]+1) GT 3) then Xp = round_off( X_mean ) else Xp = X_mean
	wg = where( Xp GT gausscut, ng )

	hx = histogram( Xp, MIN=0, BIN=1, MAX=gausscut )
	wh = where( hx, nh )
	if (wh[0] GT 0) then fb=0 else fb=1
        
	if (nh GT fb) then begin

           nbin = 3*max( hx[ wh ] ) > minbins
           binsiz = 1./nbin
           bins = histogram( randomu( seed, wh[fb]*nbin ), MIN=0, MAX=1, BIN=binsiz )
           Lbb = N_elements( bins ) -1
           Xp[ where( Xp EQ wh[fb], nw ) ] = bins[0:nw-1]

           if (nh GT 1) then more = wh[1:*] - wh

           for i=fb+1,nh-1 do begin
              bins += histogram( randomu( seed, more[i-1]*nbin ), MIN=0,MAX=1,BIN=binsiz )
              Xp[ where( Xp EQ wh[i], nw ) ] = bins[0:nw-1]
           endfor
        endif

	if (ng GT 0) then Xp[wg] = Xp[wg] + randomn( seed, ng ) * sqrt( Xp[wg] )

return, Xp
end
