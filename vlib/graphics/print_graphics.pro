;+
; NAME:
;	print_graphics
; PURPOSE:
;	Spawn UNIX lpr command to send file to printer.
; CALLING:
;	print_graphics, fileps, PS_mode
; INPUTS:
;	fileps = string, the file name.
;	PS_mode = optional string, "color" to indicate color printer queue.
; KEYWORDS:
;	N_COPY = integer, # of copies to print, default = 1.
;	QUEUE = string, send to a particular printer queue instead of default.
; OUTPUTS:
;	None.
; EXTERNAL CALLS:
;	function next_word
;	function VarType
; HISTORY:
;	Written: Frank Varosi, HSTX @ NASA/GSFC, 1990.
;	F.V.1996, added keyword QUEUE.
;	F.V.1997, use optional environment vars PR_QUE_COLOR and PR_QUE_BW.
;-

pro print_graphics, fileps, PS_mode, N_COPY=ncopy, QUEUE=queue

	if !version.os EQ "vms" then begin
		message,"not configured to print on VAX/VMS system",/INFO
		message," please print the file yourself...",/INFO
		return
	   endif

	spawn, ["ls","-s",fileps], fsize, /NOSHELL
	fsize = next_word( fsize(0) )

	if N_elements( PS_mode ) NE 1 then PS_mode = ""
	if N_elements( ncopy ) NE 1 then ncopy = 1
	ncopy = ( fix( ncopy ) > 1 ) < 9

	CASE PS_mode OF

	"color": BEGIN
		if N_elements( queue ) ne 1 then queue = getenv( "PR_QUE_COLOR")
		if VarType( queue,/NO_NULL ) NE "STRING" then queue = "color"
		command = [ "lpr", "-P"+queue, "-h", "-s", fileps ]
		ftmin = round( 1 + fsize/100. )
		quenam = "  queued to Color printer..."
		END

	  else: BEGIN
		if N_elements( queue ) ne 1 then queue = getenv( "PR_QUE_BW" )

		if VarType( queue,/NO_NULL ) EQ "STRING" then begin
			command = [ "lpr", "-P"+queue, "-h", "-s", fileps ]
			ftmin = 1
			quenam = "  queued to " + queue
		 endif else begin
			command = [ "lpr", "-h", "-s", fileps ]
			ftmin = round( 1 + fsize/100. )
			quenam = "  queued to Laser printer..."
		  endelse
		END
	ENDCASE

	for i=0,ncopy-1 do spawn, command, /NOSHELL

	if N_elements( quenam ) EQ 1 then begin
		if (ftmin EQ 1) then text = " minute"  else text = " minutes"
		ftmin = strtrim( ftmin, 2 )
		if (ncopy GT 1) then begin
		     print,ncopy," copies of graphics file  " + fileps + quenam
		     print," printing will take about " + ftmin + text + $
			" for each copy,"
		 endif else begin
			print," graphics file  " + fileps + quenam
			print," printing will take about " + ftmin + text + ","
		  endelse
		print," but you may resume the program..."
	   endif
end
