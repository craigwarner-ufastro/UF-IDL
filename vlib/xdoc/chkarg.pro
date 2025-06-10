;+
; NAME:
;	chkarg
; PURPOSE:
;	determine calling arguments of procedure or function
; CATEGORY:
;	utility
; CALLING SEQUENCE:
;	chkarg
; INPUTS:
;	name  = string containing the name of the procedure. 
; KEYWORDS:
;	PATH  = optional directory/library search path.  Same format
;		and semantics as !PATH.  If omitted, !PATH is used.
; OUTPUTS:
;       name  = name of routine
;	proc  = string array with lines of procedure 
;       lname = libr/direc location of procedure
; PROCEDURE:
;       Based on DOC_LIBRARY
; RESTRICTIONS:
;       Cannot access built-in IDL procedures
; MODIFICATION HISTORY:
;       Written DMZ (ARC) Oct 1990
;       Converted to version 2 (DMZ Jul'92)
;-

pro chkarg,name,proc,lname,path=path

on_error,1

if datatype(name) ne 'STR' then begin	;Interactive query?
  name = ''
  read,'* Name of procedure? ',name 
endif

proc=name+' NOT FOUND'
if n_elements(path) eq 0 then path = !path

;-- strip off .pro ext

pname=name   
ext=strpos(pname,'.pro') 
if ext gt -1 then pname=strmid(pname,0,ext)

;-- do the easy UNIX case first 
;   (use DOC_LIB_UNIX and search for documentation header)

if !version.os ne 'vms' then begin
 islib=0 & out=pname
 dl_unix2,pname,out=out
 np=n_elements(out)
 for i=0,np-1 do begin
  dc=strpos(out(i),'Documentation')
  if dc gt -1 then goto,exit
 endfor
 message,proc,/info & return
 exit: fstart=strpos(out(i),'/')
 if fstart eq -1 then begin
  message,proc,/info & return
 endif
 fend=strpos(out(i),'pro')
 file=strmid(out(i),fstart-1,fend+3-fstart+1)
 break_file,file,dsk,lname,name,ext,version
 lname=strtrim(lname,2) & len=strlen(lname) 
 lname=strmid(lname,0,len-1)
endif else begin

;-- get library/directory names (VMS case)

 lnames=get_lib(path)
 nlib=n_elements(lnames)
 for i=0,nlib-1 do begin
  modules=get_mod(lnames(i))
  nmods=n_elements(modules)
  for j=0,nmods-1 do begin
   mname=modules(j)
   ext=strpos(strupcase(mname),'.PRO')        ;-- strip off .PRO extension
   if ext gt -1 then mname=strmid(mname,0,ext)
   if strupcase(mname) eq strupcase(pname) then goto,found
  endfor
 endfor
 message,pname+' not found in !PATH',/continue     ;-- exit if procedure not found
 return
 found:lname=lnames(i)
 islib=(strpos(lname,'@') gt -1)
endelse

if not islib then pname=pname+'.pro'    ;-- add back .PRO extension 
if lname eq '.' then cd,current=lname   ;-- expand current directory
proc=get_proc(lname,pname) 

if (strpos(proc(0),'NOT FOUND') gt -1) then begin
 message,pname+' not found in !PATH'     ;-- exit if procedure not found
endif

print,'---- Module: ', pname
print,'---- From:   ', lname

;-- read source code and search for call line identified by PRO or FUNCTION

valid=0 & nlines=n_elements(proc)
for i=0,nlines-1 do begin
 line=strtrim(proc(i),2)
 doll=strpos(line,'$')
 semi=strpos(line,';')
 procd=strpos(strupcase(line),'PRO')
 func=strpos(strupcase(line),'FUNCTION')
 found=((procd ne -1) or (func ne -1))
 if found then begin                       ;skip comment lines
  if semi eq -1 then valid=1 else begin
   if (procd ne -1) and (semi gt procd) then valid=1
   if (func ne -1) and (semi gt func) then valid=1
  endelse
  if valid then begin
   print,'---> Call: ',line
   repeat begin             ;print continuation lines
    doll=strpos(line,'$')
    if doll ne -1 then begin
     i=i+1
     line=strtrim(proc(i),2)
     print,'           ',line
    endif
   endrep until (doll eq -1)
  endif
 endif
 if valid then goto,quit
endfor

if not valid then message,pname+' is a MAIN program',/continue

;-- clean up extracted files from VMS libraries

quit:
if !version.os eq "vms" then begin
 modfile='sys$login:*._MOD;*'
 sclfile='sys$login:*._SCL;*'
 mods=findfile(modfile,count=mc)
 if mc ne 0 then spawn,'$delete/nolog/noconfirm '+modfile
 scls=findfile(sclfile,count=sc)
 if sc ne 0 then spawn,'$delete/nolog/noconfirm '+sclfile
endif

return & end


