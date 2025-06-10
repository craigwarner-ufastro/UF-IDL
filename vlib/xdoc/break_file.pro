
pro break_file, file, disk_log, dir, filnam, ext, fversion, node
;
;+
;NAME:
;	break_file
;PURPOSE:
;	Given a file name, break the filename into the parts
;	of disk/logical, the directory, the filename, the
;	extension, and the file version (for VMS)
;INPUT:
;	file	- The file name
;OUTPUT:
;	disk_log- The disk or logical (looks for a ":")
;		  This is generally only valid on VMS machines
;	dir	- The directory
;	filnam	- The filename (excluding the ".")
;	ext	- The filename extension (including the ".")
;	fversion- The file version (only VMS)
;	node	- The Node name (only VMS)
;RESTRICTIONS:
;	VMS:
;		Assumes that : always precedes []
;	ULTRIX:
;		Right now it has trouble with the ultrix option of use
;		of "." or ".."
;HISTORY:
;	Written 1988 by M.Morrison
;	   Aug-91 (MDM) Changed to handle Unix filename convensions
;	28-Feb-92 (MDM) * Adjusted to handle arrays
;	11-Mar-92 (MDM) - Perform a STRTRIM(x,2) on input string before
;			  doing the "break-up"
;	 1-Dec-92 (MDM) - Moved code to do filename, extension and version
;			  number for both VMS and Unix (previously it
;			  did not do version number code for Unix)
;       29-Jan-93 (DMZ/MDM) - checked for node in file name
;-
;
qvms = 1
dummy = where(strpos(file, '/') ne -1, count)		;dummy is where filename has /
if (count ne 0) then qvms = 0		;if there is a /, then count ne 0, and therefore it is not VMS (lots of negatives there)
;
n = n_elements(file)
node     = strarr(n)
disk_log = strarr(n)
dir      = strarr(n)
filnam   = strarr(n)
ext      = strarr(n)
fversion = strarr(n)
;
for ifil=0,n-1 do begin
    file0 = file(ifil)
    file0 = strtrim(file0, 2)		;MDM added 11-Mar-92
    len=strlen(file0)
    ;
    ;-- node name present    ;DMZ added Jan'93
    ;  (if so then strip it off now and then add it back later)
    dcolon=strpos(file0,'::')
    if dcolon gt -1 then begin
	node(ifil)=strmid(file0,0,dcolon+2)
	file0=strmid(file0,dcolon+2,1000)
    endif
    ;
    if (qvms) then begin
	p=strpos(file0,':')
	if (p ne 1) then disk_log(ifil)=strmid(file0,0,p+1)	;includes :
	len=len-p+1
	file0=strmid(file0, p+1, len)
	;
	p=strpos(file0,']')
	if (p ne -1) then dir(ifil)=strmid(file0,0,p+1)		;includes ]
	len=len-p+1
	file0=strmid(file0, p+1, len)
    end else begin
	p = 0
	while (strpos(file0,'/', p+1) ne -1) do p = strpos(file0,'/',p+1)	;find last /
	dir(ifil) = strmid(file0, 0, p+1)
	file0 = strmid(file0, p+1, len-(p+1))
    end

    p=strpos(file0,'.')
    if (p eq -1) then begin
	    filnam(ifil) = strmid(file0,0,len) 
	    p=len
    end else filnam(ifil) = strmid(file0,0,p)		 ;not include .
    len=len-p
    file0=strmid(file0, p, len)
    ;
    p=strpos(file0,';')
    if (p eq -1) then begin
	    ext(ifil) = strmid(file0,0,len) 
	    p=len
    end else ext(ifil) = strmid(file0,0,p)			;includes . but not ;
    len=len-p
    file0=strmid(file0, p, len)
    ;
    fversion(ifil) = ''
    if (len ne 0) then fversion(ifil) = file0

    ;-- now prefix disk name with node name
    if node(ifil) ne '' then disk_log(ifil)=node(ifil)+disk_log(ifil)
end
;
if (n eq 1) then begin		;turn output into scalars
    disk_log = disk_log(0)
    dir      = dir(0)
    filnam   = filnam(0)
    ext      = ext(0)
    fversion = fversion(0)
    node     = node(0)
end
;
end

