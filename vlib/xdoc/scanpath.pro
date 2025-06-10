;+
; NAME:
;	SCANPATH
; PURPOSE:
;	Widget-based routine to read in the documentation from IDL procedures
;	in the search path.  Optionally, reads in the entire procedure.
; CATEGORY:
;	Widgets.
; CALLING SEQUENCE:
;	SCANPATH
; OPTIONAL INPUTS:
;       NAME:           Name of procedure to search and document
; OPTIONAL KEYWORD OUTPUT:
;       PROC:           String array with the text of the latest saved procedure
; BUTTONS:
;	QUIT:		Exits SCANPATH.
;	PRINT:		Prints selected procedure.  The entire procedure is
;			printed, even if "DOC ONLY" is selected.
;	DOC ONLY / ALL:	Toggles between showing only the documentation from a
;			file (the part between the "+" and "-" lines), and
;			the entire file.
;       EXTRACT         Extract and copy procedure from library and/or directory
;                       to current working directory with ".txt" extension.
; ENVIRONMENT VARIABLES:
;	IDL_PRINT_TEXT:	Environment variable (VMS: logical name) which contains
;			the print command to be used in printing files.
;	SCANPATH_FONT:	Optional font to use in the text widget.  Note that the
;			font is a function of the computer or terminal
;			providing the X-windows display server, not the host
;			computer for the IDL application.
; RESTRICTIONS:
;	Needs X-windows and widgets support (MOTIF or OPENLOOK).
; SIDE EFFECTS:
;	If "ALL" is selected to read in the entire file, then memory problems
;	may arise when reading very large procedure files.
; MODIFICATION HISTORY:
;	Written May'91 by DMZ (ARC).
;	Modified Dec 91 by WTT (ARC) to support UNIX, and add the following
;		features:
;			- Search current directory, as well as !PATH
;			- Allow for files "aaareadme.txt" containing more
;			  general information to also be searched.
;			- Only save last five procedures in memory.
;			- Add "documentation only" button.
;			- Use environment variable IDL_PRINT_TEXT
;			- Change extensions ".SCL", ".MOD" to "._SCL", "._MOD".
;       Modified Jan'92 by DMZ (ARC) to sense screen size and autosize widgets
;	Modified Feb'92 by WTT (ARC) to use SCANPATH_FONT environment variable.
;	Modified Feb'92 by DMZ (ARC) to include a message window
;	Modified Mar'92 by DMZ (ARC) to enable remote printing of files
;       Modified Jul'92 by DMZ (ARC) to improve DOCONLY switch and add EXTRACT button
;       Modified Oct'92 by DMZ (ARC) to accept procedure name as input
;       Modified Dec'92 by EEE (HSTX) to accept search strings
;-

PRO scanpath_event, event                         ;event driver routine

common scanpath,base,printb,doconly,search,tlist,mlist,flist,mlabel,$
                   comment,extractb,bdoc,text,cur_sear
common lib_stuff,lname,dname,fname,tlb
common procb,libs,names,procs
printcom = getenv("IDL_PRINT_TEXT")
if printcom eq "" then	$
	if !version.os eq "vms" then printcom = "print/setup=port" else printcom = "print"

;-- take care of different event names

widget_control, event.id, GET_UVALUE = USERVALUE
if (n_elements(uservalue) eq 0) then uservalue=''
wtype=strmid(tag_names(event,/structure_name),7,1000)

;-- take care of BUTTON widgets

if wtype eq  'BUTTON' then begin
 bname=strtrim(uservalue,2)
 case bname of 
  'QUIT'   :         begin         ;-- clean up and quit
                      if !version.os eq "vms" then begin
                       modfile='sys$login:*._MOD;*'
                       sclfile='sys$login:*._SCL;*'
                       mods = findfile( substwid( modfile ), count=mc )
                       if mc ne 0 then spawn,'$delete/nolog/noconfirm '+modfile
                       scls = findfile( substwid( sclfile ), count=sc )
                       if sc ne 0 then spawn,'$delete/nolog/noconfirm '+sclfile
                      endif
                      widget_control,base,/destroy
                     end
  'PRINT'  :         begin                         ;-- print file
                      printcom=printcom+" "+dname
                      dc=strpos(dname,'::')      ;-- for remote printing
                      if (dc gt -1) and (!version.os eq 'vms') then $
                       printcom=printcom+' /remote'
                      print,printcom & spawn,printcom
                     end
  'EXTRACT':         begin                        ;-- extract from library
                      cd,cur=def
                      m='EXTRACTING '+strupcase(fname)+' INTO '+strupcase(def)
                      widget_control,comment,set_value=m
                      widget_control,base,sensitive=0
                      dummy=get_proc(lname(0),fname,dname,/extract)
                      widget_control,base,sensitive=1
                      widget_control,comment,set_value=strupcase(fname)
                      return
                     end
  else:              begin
		      case bname of 
			  'DOCONLY'    	      : doconly=1 
			  'ALL'        	      : doconly=0
			  'Search'            : search=1
			  'No Search'  	      : search=0
		      endcase

                      if fname ne '' then begin
                       suffix=fname & prefix='M_' & goto,again
                      endif
                     end
 endcase
endif

if wtype eq 'LIST' then begin
 widget_control,printb,sensitive=0
 widget_control,extractb,sensitive=0
 widget_control,bdoc,sensitive=0
 ename=uservalue(event.index) & prefix=strmid(ename,0,2) & len=strlen(ename)
 suffix=strmid(ename,2,len)
 if !version.os eq 'vms' then suffix = strupcase(suffix)
again:
 case prefix of
  'L_':    begin                    ;--  determine module names 
            lname=suffix
            tchar = strmid(lname,0,1) & tlb = (tchar eq "@")
	    if !version.os eq 'vms' then lname = strupcase(lname)
            widget_control,comment,$
             set_value='GETTING PROCEDURE NAMES, STANDBY...'
            widget_control,base,sensitive=0
	    mods=get_mod(lname(0))
            widget_control,base,sensitive=1
            widget_control,mlabel,set_value=lname
            if n_elements(mods) eq 0 then mods=''
            if (n_elements(mods) eq 1) and (strtrim(mods(0),2) eq '') then begin
             widget_control,mlist,set_value=mods
             widget_control,mlist,sensitive=0
             widget_control,comment,set_value='NO PROCEDURES FOUND'
             return
            endif else begin
             widget_control,mlist,set_value=mods,set_uvalue='M_'+mods
             widget_control,mlist,sensitive=1
             widget_control,comment,set_value='PLEASE SELECT A PROCEDURE'
             return
            endelse
           end
   'M_':   begin
            widget_control,tlist,sensitive=0
            widget_control,mlist,sensitive=0
            fname=suffix
	    if !version.os eq 'vms' then fname = strupcase(fname)
            widget_control,comment,$
             set_value='READING '+strupcase(fname)+', STANDBY...'
            widget_control,flist,set_value=''
            widget_control,base,sensitive=0
	    proc=get_proc(lname(0),fname,dname,text, $
			                 doc_only=doconly,search=search)
            widget_control,base,sensitive=1
            if n_elements(proc) ne 0 then begin
             widget_control,comment,set_value=strupcase(dname)
             widget_control,flist,set_value=proc
             widget_control,printb,sensitive=1
             widget_control,extractb,sensitive=1
             widget_control,bdoc,sensitive=1
            endif
            widget_control,mlist,sensitive=1
            widget_control,tlist,sensitive=1
            return
           end
   else:   return
 endcase
endif

if wtype eq  'TEXT' then begin		; user entered text to search for
  widget_control, event.id, get_value=value, set_value=''
  text = value(0)
  if strlen(text) lt 10 then ellipse='' else ellipse='...' 
  widget_control, cur_sear, set_value='Current search text is : ' + $
				strmid(text,0,9) + ellipse
  if fname ne '' then begin
     suffix=fname & prefix='M_' & goto,again	; go do search
  endif
endif

return & end

;----------------------------------------------------------------------------

pro scanpath,name,proc=proc                     ;IDL path scanner

common scanpath,base,printb,doconly,search,tlist,mlist,flist,mlabel,$
                   comment,extractb,bdoc,text,cur_sear
common procb,libs,names,procs
common lib_stuff,lname,dname,fname,tlb

set_plot,'X'
if (!d.flags and 65536) eq 0 then message,'widgets are unavailable'
;widget_control,/reset,/clear_events
fname=''
font = getenv('SCANPATH_FONT')

;-- autosize screen

device,get_screen_size=sc
fspace=.0244*sc(0)/2.
fxpad=.0195*sc(0)/2.
fypad=.0195*sc(1)/2.
scx=sc(0)/1280. & scy=sc(1)/1024.

base = widget_base(TITLE ='SCANPATH', XPAD = fxpad, YPAD = fypad,$
                   SPACE = fspace, /row,/frame)


col1=widget_base(base,/column,space=fspace)

;-- top row of buttons

temp=widget_base(col1,/row,space=fspace,/frame,xpad=fxpad,ypad=fypad)   

;-- return button

quitb = widget_button(temp,value='Quit',uvalue='QUIT',/no_release)

;-- print button

printb=widget_button(temp,value='Print',uvalue='PRINT',/no_release)

;-- extract button

extractb=widget_button(temp,value='Extract',uvalue='EXTRACT',/no_release)

;-- DOC ONLY button

values=['All','Doc Only']
xmenu,values,temp,/column,/exclusive,/frame,/no_release,space=fspace,xpad=fxpad,ypad=fypad,$
      buttons=docb,uvalue=['ALL','DOCONLY'],base=bdoc

if n_elements(doconly) eq 0 then doconly=0
if doconly then widget_control,docb(1),set_button=1 else $
                widget_control,docb(0),set_button=1

choices = ['Search', 'No Search ']
xmenu,choices,temp,/column,/exclusive,/frame,/no_release,space=fspace,$
		xpad=fxpad,ypad=fypad,buttons=searb,uvalue=choices,base=bdoc

if n_elements(search) eq 0 then search=0		
if search then widget_control,searb(0),set_button=1 $	; turn on correct
          else widget_control,searb(1),set_button=1	;   button


;-- 2nd row

;
;  If you're having vertical sizing problems, try changing the following
;  parameter to 12.
;

ysz = 15

;-- 1st column contains list of libraries (or directories) and modules

lnames=get_lib()
temp=widget_base(col1,/column,/frame)
 tlabel=widget_label(temp,value='SELECT FROM THE FOLLOWING DIRECTORIES/LIBRARIES')
 tlist=widget_list(temp,ysize=ysz*scy,value=lnames,uvalue='L_'+lnames)

;-- list of modules in selected library

temp=widget_base(col1,/column,/frame)
 mlabel=widget_label(temp,value='SELECT FROM THE FOLLOWING PROCEDURES')
 mlist=widget_list(temp,ysize=ysz*scy)

;-- able to search for a string

if n_elements(text) eq 0 then text = ''
if strlen(text) lt 10 then ellipse='' else ellipse='...' 

sear = widget_base(col1, /column,/frame)

  cur_sear = widget_label(sear, value='Current search text is : ' + $
				strmid(text,0,9) + ellipse)

  newtext = widget_label(sear, value='Enter Search String and RETURN')
  entval = widget_text(sear, /editable, ysize=1, /frame)
 


;-- 2nd column will contain text of selected procedure

col2=widget_base(base,/column,space=fspace)

;-- message window

temp=widget_base(col2,/column)
 comment=widget_text(temp,xsize=80*scx,ysize=scy)
 widget_control,comment,set_value=''

;-- procedure window

temp=widget_base(col2,/column,/frame) 
flist=widget_text(temp,xsize=80*scx,ysize=50*scy,/scroll) 
widget_control,bdoc,sensitive=0
widget_control,extractb,sensitive=0
widget_control,printb,sensitive=0

;-- user provided name?

found=0
sz=size(name)
if sz(n_elements(sz)-2) eq 7 then begin
 fname=name & ext=strpos(name,'.pro')
 if ext eq -1 then fname=name+'.pro'

;-- in memory already?

 if n_elements(names) ne 0 then begin

  ext=strpos(strupcase(fname),'.PRO') 
  if ext gt -1 then tname=strupcase(strmid(fname,0,ext)) else $
                         tname=strupcase(fname)
  find=where(tname eq names,count)
  if count gt 0 then begin
   message,'recalling '+fname+' from memory',/info
   proc=procs(*,find) & lname=libs(find) & dname=lname+' '+fname
   found=1
  endif
 endif

;-- in path?

 if not found then begin
  message,'searching !PATH for '+fname,/info
  chkarg,fname,proc,lname
  ext=strpos(fname,'.pro')
  if ext eq -1 then fname=name+'.pro'
  found=(strpos(proc(0),'NOT FOUND') eq -1)
  if found then begin
   proc=get_proc(lname(0),fname,dname,doc_only=doconly)
  endif
 endif

;-- if found then list it 

 if found then begin
  mods=get_mod(lname(0))
  widget_control,mlist,set_value=mods,set_uvalue='M_'+mods
  widget_control,mlist,set_value=mods
  widget_control,flist,set_value=proc
  widget_control,printb,sensitive=1
  widget_control,extractb,sensitive=1
  widget_control,bdoc,sensitive=1
  if found then widget_control,comment,set_value=strupcase(dname)
 endif else widget_control,comment,set_value=strupcase(fname)+' NOT FOUND'

endif else widget_control,comment,set_value='PLEASE SELECT A LIBRARY/DIRECTORY'

widget_control,base,/realize
xmanager,'scanpath',base
xmanager

;-- return most recent procedure

sp=size(procs)
if (sp(0) eq 1) then proc=procs(*) else if (sp(0) eq 2) then proc=procs(*,sp(2)-1)

return & end


