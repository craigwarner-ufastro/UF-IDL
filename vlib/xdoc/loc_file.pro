function loc_file,file,path=path,loc=loc,all=all,count=count
;+
; NAME:
;       loc_file
; PURPOSE:
;       get files from a set of directories
; CATEGORY:
;       help
; CALLING SEQUENCE:
;       fname=loc_file(file,path=path,loc=loc,count=count,all=all)
; INPUTS:
;       file   : file name
; KEYWORDS:
;       loc    : directory locations of found files
;       path   : array or scalar string with directory name(s)
;       count  : number of found files
;       all    : search all directories
; PROCEDURE:
;       Uses FINDFILE
; HISTORY:
;       Written Dec'92 (DMZ,ARC)
;-
on_error,1

if datatype(file) ne 'STR' then $
 message,'usage --> FILES=LOC_FILE(FILE,PATH=PATH)'

cd,current=cdir

;-- decompose file

break_file, file, disk_log, direc, filnam, ext, vers
direc=disk_log+direc

case 1 of

 (direc eq '') and (n_elements(path) eq 0): path=cdir

 (direc ne '') and (n_elements(path) eq 0): path=direc

 (direc ne '') and (datatype(path) eq 'STR'): path=[direc,path]

 (direc eq '') and (datatype(path) eq 'STR'): path=path

 else: message,'do not understand request'
endcase

loc=''  & fname=''
ndir=n_elements(path)
for i=0,ndir-1 do begin
 bpath=path(i)
 if !version.os ne 'vms' then begin    ;-- expand possible ~ in path
  spawn, 'echo '+bpath,out 
  bpath=out(0)
 endif
 bname=concat_dir(bpath,filnam+ext+vers)
 go_find=findfile(bname,count=count)
 if count ne 0 then begin
  if fname(0) eq '' then fname=go_find else fname=[fname,go_find]
  if loc(0) eq '' then loc=bpath else loc=[loc,bpath]
  if not keyword_set(all) then goto,jump
 endif
endfor

jump: if fname(0) eq '' then count=0 else count=n_elements(fname)
if count eq 0 then message,'file not found',/contin
if n_elements(fname) eq 1 then fname=fname(0)
return,fname

end
