function gLine ,xmin,xmax ,ymin,ymax ,npoint

	xLin = xmin + (xmax-xmin)*findgen(npoint)/npoint
	yLin = ymin + (ymax-ymin)*findgen(npoint)/npoint

return, [ [xLin], [yLin] ]
end
