;+
; NAME:
;	GET_PROC
; PURPOSE:
;	Extract procedure from a library or directory.  
; CALLING SEQUENCE:
;	PROC = GET_PROC(LIB,NAME,FNAME,TEXT, 
;			/DOC_ONLY,/EXTRACT,/SEARCH,/LINOS,/BUFFER)
; INPUTS:
;	LIB	= Library name.
;	NAME	= Procedure name.
;	TEXT 	= Search string.
; KEYWORDS:
;	DOC_ONLY= Logical switch to decide whether only the documentation
;		  (between ";+" and ";-") is to be read in, or the entire file.
;       EXTRACT = copy extracted library module to user's current directory
;	SEARCH  = Logical switch to decide whether to search for string in
;		  variable TEXT
; UNUSABLE KEYWORDS -- code is in place but modifications to DOC.PRO
;		       are needed to pass /LINOS or BUFFER=[n1,n2]
;       LINOS   = added line number option
;	BUFFER  = 2 element vector indicating display n1 lines before search 
;		  string and n2 lines after search string. Only valid if
;		  /SEARCH is set
; OUTPUTS:
;	FNAME	= File name.
;	PROC    = string array with each element being a line of code.
; PROCEDURE:
;	If necessary, then spawns a LIBRARY/EXTRACT command.
; MODIFICATION HISTORY:
;       Written DMZ (ARC) May 1991.
;	Modified WTT (ARC) Dec 1991, to support UNIX.
;       Modified DMZ (ARC) Jul 1992, to speed reading and add extract keyword
;       Modified EEE (HSTX) Oct 1992, 1) to find all occurrences of ;+/;_
;       			      2) to search for input string
;       			      3) to allow BUFFER keyword 
;-

function get_proc,library,name,fname,text, doc_only=doc_only, $
		  extract=extract,search=search,linos=linos, buffer=buffer

common procb,libs,names,procs

;-- in case there was a crash earlier

n_libs=n_elements(libs)
n_names=n_elements(names)
sz=size(procs)
if sz(0) eq 2 then n_procs=sz(2) else n_procs=sz(0)
if (n_procs ne n_names) or (n_libs ne n_names) or (n_libs ne n_procs) then begin
 message,'memory error; will fix',/info
 names='' & procs='' & libs=''
endif

;-- strip off .pro

ext=strpos(strupcase(name),'.PRO') 
if ext gt -1 then tname=strupcase(strmid(name,0,ext)) else $
                        tname=strupcase(name)

if keyword_set(doc_only) then doc_only=1 else doc_only=0
if !version.os eq "vms" then dirsep = '' else dirsep = '/'
lib = strtrim(library,2) & tchar = strmid(lib,0,1)
tlb = tchar eq "@"
if tlb then begin                        ;-- take off "@" sign
 lib = strmid(lib,1,strlen(lib)-1)
 fname='sys$login:'+tname+'._SCL' 
endif else begin
 ext=strpos(strupcase(name),'.PRO') 
 if ext gt -1 then fname=lib+dirsep+name else fname=lib+dirsep+name+'.pro'
endelse

;-  If the name is '*INFO*', then get the file "aaareadme.txt", and get the
;  entire file.

SAVE_DOC_ONLY = DOC_ONLY

IF NAME EQ '*INFO*' THEN BEGIN
 FNAME = LIB + DIRSEP + "aaareadme.txt"
 DOC_ONLY = 0
ENDIF

;-- extract option (copy selected procedure to current directory)

if keyword_set(extract) then begin
 if tlb then begin                        ;-- library modules already extracted?
  find = findfile( substwid( fname ), count=fc )        
  if fc eq 0 then begin
   statement='$LIBR/EXTRACT='+tname+' '+lib+' /OUT='+fname
   spawn,statement
  endif
 endif
 cd,current=def
 semic=strpos(strupcase(fname),';')
 if semic lt 0 then sname=fname else sname=strmid(fname,0,semic) ;-- strip version
 if name eq '*INFO*' then tname='aaareadme'
 back=sname+' '+def+dirsep+tname+'.txt'
 if !version.os eq 'vms' then front='copy/nolog/noconfirm ' else front='cp '
 spawn,front+back
 return,''
endif 

;-- in memory already?; if so, then retrieve it

PROC_IN_MEMORY=0
IF N_ELEMENTS(NAMES) NE 0 THEN BEGIN
 FIND=WHERE(TNAME EQ NAMES,COUNT)   
 IF COUNT NE 0 THEN BEGIN
  FOR ICOUNT = 0,COUNT-1 DO BEGIN
   IF LIBS(FIND(ICOUNT)) EQ LIB THEN BEGIN 
    message,'recalling '+name+' from memory',/info
    PROC=PROCS(*,FIND(ICOUNT)) & PROC_IN_MEMORY=1 & GOTO,NEXT
   ENDIF
  ENDFOR
 ENDIF
ENDIF

next:

if not PROC_IN_MEMORY then begin

;-- extract module from library or directory 
  
 if tlb then begin                      ;--library case
  find = findfile( substwid( fname ), count=fc )   ;--modules already extracted?
  if fc eq 0 then begin
   statement='$LIBR/EXTRACT='+tname+' '+lib+' /OUT='+fname
   spawn,statement
  endif
 endif else begin                       ;--directory case
  found = findfile( substwid( fname ), count=nf )
  if nf eq 0 then return,strupcase(fname)+' NOT FOUND'
 endelse

;-- now read procedure into memory by using SPAWN

 if !version.os eq 'vms' then cmd='type/nopage '+fname else cmd='cat '+fname
 spawn,cmd,proc

;-- add line numbers

if keyword_set(linos) then begin
 np=n_elements(proc) 
 lnums=(sindgen(np+1))(1:np)+': '
 proc=strtrim(lnums,1)+proc
endif

;--now save procedure into common memory to avoid re-reading

 if n_procs eq 0 then begin
  LIBS = LIB
  NAMES = TNAME
  PROCS = PROC
 endif else begin
  LIBS = [LIBS,LIB]
  NAMES = [NAMES,TNAME]
  BOOST_ARRAY,PROCS,PROC
  IF N_ELEMENTS(NAMES) GT 10 THEN BEGIN		;Save last 10
   LIBS = LIBS(1:*)
   NAMES = NAMES(1:*)
   PROCS = PROCS(*,1:*)
  ENDIF
 endelse
endif

;--  If the documentation only switch has been set, then only extract the parts
;   between the lines beginning with ";+" and ";-".  First look for the ";+"
;   line, then copy in lines until the ";-" line is found.

if doc_only then begin
 tproc='NO DOCUMENTATION FOUND' & np=n_elements(proc)
 BEGUN=0 & DONE=0 & i=-1
 repeat begin
  i=i+1 & line=proc(i)
  IF NOT BEGUN THEN BEGIN
   BEGUN = (STRPOS(LINE, ";+") NE -1)
  END ELSE IF (STRPOS(LINE,";-") NE -1) THEN BEGIN
   begun = 0				; start again and look for more
  END ELSE BEGIN
   TPROC=[TPROC,LINE]
  ENDELSE
 endrep until (i eq np-1)		; search whole file
 IF N_ELEMENTS(TPROC) GT 1 THEN TPROC=TPROC(1:*)
 PROC=TPROC
endif

DOC_ONLY = SAVE_DOC_ONLY


; Now have desired output in variable array PROC so just do a search on that.
;  If SEARCH is set, then show the specified lines above and below the line
;  that contains the search string.


if keyword_set(search) then search=1 else search=0
if not keyword_set(buffer) then buffer = [2,2] $ ; 2 lines before and after
                           else buffer=buffer

if search then begin			; search file for the given string
 if text eq '' then return,'NO SEARCH STRING ENTERED'

 tproc='NO MATCH FOUND FOR SEARCH STRING ' + text & np=n_elements(proc)-1
 textup = strupcase(text)			; find all occurrences

 case n_elements(buffer) of 		; is there a range to show above/below
   0 : begin above=0 & below=0   &  end 		; only 1 line
   1 : begin above=0 & below=buffer  &  end		; current down to input
   else : begin above=buffer(0) & below=buffer(1) & end	; expand both sides
 endcase

 i=-1  &   trail=-1
 repeat begin
   i=i+1 & line=strupcase(proc(i))
   if STRPOS(LINE, textup) ge 0 then begin		; found a match
        lead = (i-above-1) > trail+1 > 0		; don't rewrite lines
        if lead-1 ne trail then tproc = [tproc, '---LINE ' + strtrim(lead,2) + $
					        '---']
        trail = (i+below) < np
	TPROC=[TPROC, proc(lead: trail) ]
	i = trail 				; already have these lines
   end
 endrep until (i ge np)		; search whole file

 IF N_ELEMENTS(TPROC) GT 1 THEN TPROC=TPROC(1:*)
 PROC=TPROC
endif


return,proc & end

