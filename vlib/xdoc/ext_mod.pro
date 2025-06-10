;+
; NAME:  
;             EXT_MOD
; PURPOSE:
;             to extract modules from VMS libraries
; CALLING SEQUENCE: 
;             ext_mod,lib_nam,targ_dir
; INPUTS:
;             lib_nam  = name of source library
;             targ_dir = target directory of modules
; HISTORY:                           
;             written Jan'93 by DMZ (ARC)
;-


pro ext_mod,lib_nam,targ_dir

on_error,1

if datatype(lib_nam) ne 'STR' then begin
 message,'enter name of library from which to extract modules ',/cont
 repeat begin
  lib_nam='' & read,'---> ',lib_nam
 endrep until lib_nam ne ''
endif

;-- valid library? 

break_file,lib_nam, disk_log, dirnam, filnam, ext, fversion
if (strpos(strupcase(ext),'TLB') eq -1) then $
message,'input is not a valid library'

;-- location?
 
find = findfile( substwid( lib_nam ), count=count )
if count eq 0 then begin
 message,'enter directory location of library ',/cont
 repeat begin
  dirnam='' & read,'---> ',dirnam
 endrep until dirnam ne ''
 dir_log=getenv(strupcase(dirnam))
 if dir_log ne '' then dirnam=dir_log
 lib_nam=dirnam+lib_nam
endif

;-- target?

if datatype(targ_dir) ne 'STR' then begin
 cd,current=cur_dir
 message,'enter target directory for extracted modules ',/cont
 message,'[def = '+cur_dir,/cont
 ans='' & read,'---> ',ans
 if ans eq '' then targ_dir=cur_dir else targ_dir=ans 
endif
dir_log=getenv(strupcase(targ_dir))
if dir_log ne '' then targ_dir=dir_log

mods=get_mod('@'+lib_nam)
nmods=n_elements(mods)
if nmods eq 0 then  message,'no modules in '+lib_nam

;-- now extract

message,'busy extracting...',/info
for i=0,n_elements(mods)-1 do begin
 sp_str='libr/extract='+mods(i)+' '+lib_nam+' /out='+targ_dir+mods(i)+'.pro'
 spawn,sp_str
endfor

message,'all done',/info

return & end

