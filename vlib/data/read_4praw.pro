function read_4praw, filename, rawdata, dif1, dif2, sig

	rawdata = reform( readfits( filename, fhd ) )
	dif1 = reform( rawdata[*,*,1,0,*] - rawdata[*,*,0,0,*] )
	dif2 = reform( rawdata[*,*,1,1,*] - rawdata[*,*,0,1,*] )
	sig = dif1 + dif2

return, fhd
end
