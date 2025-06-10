; $Id: //depot/idl/IDL_71/idldir/lib/utilities/xvaredit.pro#1 $
;
; Copyright (c) 1991-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;+
; NAME:
;   X_VAR_EDIT
; PURPOSE:
;   This routine provides an editor for any IDL variable.
; CATEGORY:
;   Widgets
; CALLING SEQUENCE:
;   X_VAR_EDIT, VAR
; INPUTS:
;   VAR = The variable that is to be edited.
; KEYWORD PARAMETERS:
;   NAME = The NAME of the variable.  This keyword is overwritten with the
;       structure name if the variable is a structure.
;   GROUP = The widget ID of the widget that calls X_Var_Edit.  When this
;       ID is specified, a death of the caller results in a death of
;       X_Var_Edit.
;   X_SCROLL_SIZE = The X_SCROLL_SIZE keyword allows you to set
;       the width of the scrolling viewport in columns.
;       Default is 9.
;   Y_SCROLL_SIZE = The Y_SCROLL_SIZE keyword allows you to set
;       the height of the scrolling viewport in rows.
;       Default is 9.
; OUTPUTS:
;   VAR= The variable that has been edited, or the original if the user
;       selects the "Cancel" button.
; COMMON BLOCKS:
;   None.
; SIDE EFFECTS:
;   Initiates the XManager if it is not already running.
; RESTRICTIONS:
;   None known.
; PROCEDURE:
;   Display a table widget, a "cancel" button, and an "accept" button.
;   If the user clicks "accept", values in the table widget are
;   written to the variable being edited.
;
;   Note: Pointers and Object References are shown as blank cells.  To
;   edit Pointer or Object References, enter valid IDL expressions
;   such as OBJ_NEW('IDLgrModel'), PTR_VALID(123, /CAST), etc.  To
;   leave Ponters and Object References unchanged, leave their
;   corresponding cells blank.
;
; MODIFICATION HISTORY:
;   Written by: Steve Richards, February, 1991
;   Modified: September 96, LP - rewritten with TABLE widget
;   Modified: March 2000, PCS - Added support for new data types
;       such as Unsigned Integer.  Made Pointers and Object References
;       editable.
;   CT, RSI, April 2005: Disable editing in the IDL Virtual Machine.
;   FV, UF, Sept. 2011: copied to x_var_edit.pro and use COLUMN_WIDTHS.
;-

FUNCTION X_Var_Edit__n_elements, arg
;
;function X_VAR_EDIT__N_ELEMENTS: return the number of elements in arg.
;This function is similar to N_ELEMENTS, but counts recursively,
;where applicable.
;
ON_ERROR, 2 ;return to caller on error.

CASE 1 OF
    SIZE(arg, /TYPE) EQ 8: BEGIN    ;structure or array of structures.
        result = 0L

        FOR i=0L,N_ELEMENTS(arg)-1 DO BEGIN
            FOR j=0L,N_TAGS(arg)-1 DO BEGIN
                result = result + X_Var_Edit__n_elements((arg[i]).(j))
            END
        END

        RETURN, result
    END

    ELSE: $
        RETURN, N_ELEMENTS(arg)
ENDCASE

END
;------------------------------------------------------------------------------
;   procedure X_Var_Edit_event
;------------------------------------------------------------------------------
; This procedure processes the events being sent by the XManager.
;------------------------------------------------------------------------------
PRO X_Var_Edit_event, event

COMPILE_OPT hidden
WIDGET_CONTROL, event.id, GET_UVALUE = whichevent
IF N_ELEMENTS(whichevent) EQ 0 THEN RETURN
IF whichevent NE "THEBUTTON" THEN RETURN

CASE event.value OF

    0: BEGIN                                            ;the user chose the
        WIDGET_CONTROL, event.top, /DESTROY     ;return the initial
      END                       ;variable

    1: BEGIN                        ;the user chose accept
        WIDGET_CONTROL, event.top, GET_UVALUE = pEval, /HOURGLASS
        IF (*pEval).usetable THEN BEGIN
            edit_cell = WIDGET_INFO((*pEval).table, /TABLE_EDIT_CELL)
            if edit_cell[0] EQ -1 AND edit_cell[1] EQ -1 then begin
                (*pEval).modified = 1
                WIDGET_CONTROL, (*pEval).table, GET_VALUE = var
                if (SIZE((*pEval).var))[0] EQ 0 then begin
                  (*pEval).var = var[0]
                endif else begin
                  (*pEval).var = TEMPORARY(var)
                endelse
            endif else begin
                tmp = DIALOG_MESSAGE(['Please commit or cancel the edit',$
                                      'before pressing Accept.'])
                RETURN
            endelse
        ENDIF ELSE BEGIN
            i = 0LL
            ;so go ahead and modify the variable
            WIDGET_CONTROL, (*pEval).table, GET_VALUE = var
            WHILE(i LT N_ELEMENTS(var))DO BEGIN
                CASE (*pEval).entries[i].type OF
                    6: assign = '=COMPLEX'
                    9: assign = '=DCOMPLEX'
                    ELSE: assign = '='
                ENDCASE
                IF ((*pEval).entries[i].type EQ 10 && var[i] NE '') $
                || ((*pEval).entries[i].type EQ 11 && var[i] NE '') $
                || ((*pEval).entries[i].type NE 10 && $
                    (*pEval).entries[i].type NE 11) $
                THEN BEGIN
                    str = "(*pEval)." + $
                        (*pEval).entries[i].name + $
                        assign + $
                        ((*pEval).entries[i].type EQ 7 ? "var[i]" : var[i])
                    IF NOT EXECUTE(str, 1) THEN BEGIN
                        void = DIALOG_MESSAGE( $
                            [ $
                                'X_Var_Edit: error converting "' $
                                    + var[i] $
                                    + '" to ' $
                                    + (*pEval).entries[i].name, $
                                '', $
                                !error_state.msg, $
                                !error_state.sys_msg $
                                ], $
                            /ERROR $
                            )
                        conversion_err = 1
                    ENDIF
                ENDIF
                i = i + 1
            ENDWHILE
        ENDELSE
        IF NOT KEYWORD_SET(conversion_err) THEN BEGIN
            (*pEval).modified = 1
            WIDGET_CONTROL, event.top, /DESTROY
        ENDIF
    END
    ELSE:

ENDCASE

END ;============= end of X_Var_Edit event handling routine task =============


;------------------------------------------------------------------------------
;   procedure AddEditEntry
;------------------------------------------------------------------------------
; This procedure adds an entry to the list that contains the variables names
; and the widget id for the edit field corresponding to the variable name.
;------------------------------------------------------------------------------

PRO X_Var_Edit__AddEditEntry, $
    entries, $      ; IN/OUT
    n_ents, $       ; IN/OUT
    thename, $      ; IN
    thetype, $      ; IN
    value, $        ; IN
    n_elems         ; IN

COMPILE_OPT hidden

IF(NOT(KEYWORD_SET(entries))) THEN BEGIN
    entries = REPLICATE({entstr, $
        name: thename, $
        value: value, $
        type: thetype $
        }, n_elems)
    n_ents = 1L
ENDIF ELSE BEGIN
    IF (N_ELEMENTS(entries) LE n_ents) THEN BEGIN
        entries = [temporary(entries), REPLICATE({entstr}, n_elems > 100)]
    END
    entries[n_ents].name = thename
    entries[n_ents].value = value
    entries[n_ents].type = thetype
    n_ents = n_ents + 1
ENDELSE
END ;============== end of X_Var_Edit event handling routine task ===============


;------------------------------------------------------------------------------
;   procedure X_Var_EditField
;------------------------------------------------------------------------------
;  This routine is used to create the widget or widgets needed for a given
;  variable type.  It could call itself recursively if the variable was itself
;  a structure comprised of other IDL variables.
;------------------------------------------------------------------------------

FUNCTION X_Var_EditField, $
    base, $     ; IN
    val, $      ; IN
    usetable, $ ; OUT
    entries, $  ; IN/OUT
    nentries, $ ; IN/OUT
    TOTAL_N_ELEMENTS = total_n_elements, $  ; IN/OUT (opt)
    NAME = NAME, $
    READ_ONLY=readOnly, $
    RECNAME = RECNAME, $
    X_SCROLL_SIZE = X_SCROLL_SIZE, $
    Y_SCROLL_SIZE = Y_SCROLL_SIZE, COLUMN_MAJOR=usecolumns

COMPILE_OPT hidden
FORWARD_FUNCTION X_Var_EditField

typarr = [ $
    "Undefined", $                  ; 0
    "Byte", $                       ; 1
    "Integer", $                    ; 2
    "Longword Integer", $           ; 3
    "Floating Point", $             ; 4
    "Double Precision Floating", $  ; 5
    "Complex Floating", $           ; 6
    "String", $                     ; 7
    "Structure", $                  ; 8
    "Double Precision Complex", $   ; 9
    "Pointer", $                    ; 10
    "Object Reference", $           ; 11
    "Unsigned Integer", $           ; 12
    "Unsigned Longword Integer", $  ; 13
    '64-bit Integer', $             ; 14
    "Unsigned 64-bit Integer" $     ; 15
    ]

varsize = size(val)
vardims = N_ELEMENTS(varsize) - 2
type = varsize[vardims]
numelements = varsize[vardims + 1]

usetable = 0
IF (NOT(KEYWORD_SET(RECNAME)) $
AND (varsize[0] EQ 1 OR varsize[0] EQ 2)) THEN BEGIN
    IF(type EQ 8) THEN BEGIN
        IF varsize[0] EQ 1 THEN BEGIN
            IF !VERSION.OS_FAMILY EQ 'Windows' AND N_TAGS(val) GT 200 THEN $
                Goto, Cplx_Struct
            FOR i = 0, N_TAGS(val) - 1 DO BEGIN
                strsize = size(val.(i))
                strdims = N_ELEMENTS(strsize) - 2
                IF strsize[strdims] EQ 8 $  ; Structure
                OR strsize[strdims] EQ 10 $ ; Pointer
                OR strsize[strdims] EQ 11 $ ; Object Reference
                OR strsize[strdims + 1] NE varsize[vardims + 1] THEN $
                    Goto, Cplx_Struct
            ENDFOR
            usetable = 1
        ENDIF
    ENDIF ELSE BEGIN
        IF !VERSION.OS_FAMILY EQ 'Windows' AND varsize[1] GT 200 THEN $
            Goto, Cplx_Struct
        usetable = 1
    ENDELSE
ENDIF
Cplx_Struct:
recurse = KEYWORD_SET(RECNAME)

IF (NOT recurse) THEN $
  abase = WIDGET_BASE(base, /FRAME, /COLUMN, XPAD = 8, YPAD = 8)

IF(numelements GT 1) THEN BEGIN             ;if the variable is an
  suffix = " Array("                        ;array, then say so and
  FOR j = 1, varsize[0] DO BEGIN            ;show the array
    suffix = suffix + strtrim(varsize[j], 2)        ;dimensions.
    IF j NE varsize[0] THEN suffix = suffix + ", "
  ENDFOR
  suffix = suffix + ")"
ENDIF ELSE suffix = ""


IF(type EQ 8) THEN NAME = TAG_NAMES(val, /STRUCTURE) ;if the variable is a
                            ;structure, use its
                            ;name

;build up the name of variable with the type in parentheses
IF(NOT recurse) THEN BEGIN
    IF(KEYWORD_SET(NAME)) THEN $
      lbl = WIDGET_LABEL(abase, $
                         VALUE = NAME + " (" + typarr[type] + suffix + ")") $
    ELSE lbl = WIDGET_LABEL(abase, $
                            value = typarr[type] + suffix)
ENDIF

IF(NOT(KEYWORD_SET(RECNAME))) THEN BEGIN
    RECNAME = 'var'         ;establish the name
                            ;if not being called
                            ;recursively
END

IF(N_ELEMENTS(X_SCROLL_SIZE) EQ 0) THEN XSCROLL_SIZE = 9 ELSE XSCROLL_SIZE = X_SCROLL_SIZE
IF(N_ELEMENTS(Y_SCROLL_SIZE) EQ 0) THEN YSCROLL_SIZE = 9 ELSE YSCROLL_SIZE = Y_SCROLL_SIZE

IF (usetable) THEN BEGIN
   IF keyword_set( usecolumns ) THEN BEGIN
      IF(type EQ 8) THEN BEGIN
         RETURN, WIDGET_TABLE( abase, VALUE = val, /COLUMN_MAJOR, $
                               ROW_LABELS = tag_names( val ), $
                               COLUMN_WIDTHS = 20*!D.x_ch_size, $
                               FONT="-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1", $
                               /RESIZEABLE_COLUMNS, $
                               EDIT=~KEYWORD_SET(readOnly), $
                               X_SCROLL_SIZE = XSCROLL_SIZE, $
                               Y_SCROLL_SIZE = YSCROLL_SIZE )
      ENDIF ELSE BEGIN
         RETURN, WIDGET_TABLE( abase, VALUE = val, /COLUMN_MAJOR, $
                               /RESIZEABLE_COLUMNS, $
                               EDIT=~KEYWORD_SET(readOnly), $
                               X_SCROLL_SIZE = XSCROLL_SIZE, $
                               Y_SCROLL_SIZE = YSCROLL_SIZE )
      ENDELSE
   endif else begin
      IF(type EQ 8) THEN BEGIN
         column_Labels = TAG_NAMES(val)
         RETURN, WIDGET_TABLE( abase, VALUE = val, $
                               COLUMN_LABELS = tag_names( val ), $
                               COLUMN_WIDTHS = (strLen(column_Labels)>10)*!D.x_ch_size*2, $
                               /RESIZEABLE_COLUMNS, $
                               EDIT=~KEYWORD_SET(readOnly), $
                               X_SCROLL_SIZE = XSCROLL_SIZE, $
                               Y_SCROLL_SIZE = YSCROLL_SIZE )
      ENDIF ELSE BEGIN
         RETURN, WIDGET_TABLE( abase, VALUE = val, $
                               /RESIZEABLE_COLUMNS, $
                               EDIT=~KEYWORD_SET(readOnly), $
                               X_SCROLL_SIZE = XSCROLL_SIZE, $
                               Y_SCROLL_SIZE = YSCROLL_SIZE )
      ENDELSE
   endelse
ENDIF

IF(varsize[0] GT 1) THEN BEGIN
  moduli = LONARR(varsize[0]-1) + 1
  FOR i = varsize[0], 2,-1 DO BEGIN
    FOR j = 1,i-1 DO $
      moduli[i - 2] = moduli[i - 2] * varsize[j]
  ENDFOR
ENDIF

IF N_ELEMENTS(total_n_elements) EQ 0 THEN BEGIN
    total_n_elements = X_Var_Edit__n_elements(val)
END

FOR element = 0L, numelements - 1 DO BEGIN       ;for each array element

  IF(numelements NE 1) THEN BEGIN           ;use array subscripting
    indexname = "("
    indexname = indexname + $
        strtrim(element mod varsize[1],2)
    IF(varsize[0] GT 1) THEN BEGIN
      indexarr = lonarr(varsize[0] - 1)
      flatindex = element
      FOR i = varsize[0] - 2, 0, -1 DO BEGIN
    indexarr[i] = flatindex / moduli[i]
    flatindex = flatindex mod moduli[i]
      ENDFOR
      FOR i = 0, varsize[0] - 2 DO $
    indexname = indexname + ", " + $
        strtrim(indexarr[i], 2)
    ENDIF
    indexname = indexname + ")"
    thename = RECNAME + indexname
  ENDIF ELSE BEGIN
    thename = RECNAME
  ENDELSE

  ;depending on the type, build a string variable with proper formatting
  CASE type OF
    0: thevalue = "Undefined Variable"          ;Undefined

    1: thevalue = string(val[element], FORM='(I3)')        ;Byte
        

    7: thevalue = val[element]              ;String

    8: BEGIN                        ;Structure
        tags = TAG_NAMES(val[element])
        FOR i = 0, N_ELEMENTS(tags) - 1 DO BEGIN
           id = X_Var_EditField( abase, $
                                 val[element].(i), $
                                 usetable, $
                                 entries, $
                                 nentries, $
                                 COLUMN_MAJOR=usecolumns, $
                                 TOTAL_N_ELEMENTS = total_n_elements, $
                                 NAME = tags[i], $
                                 READ_ONLY=readOnly, $
                                 RECNAME = thename + "." + tags[i], $
                                 X_SCROLL_SIZE = XSCROLL_SIZE, $
                                 Y_SCROLL_SIZE = YSCROLL_SIZE )
        ENDFOR
    END
    10: thevalue = ''
    11: thevalue = ''
    ELSE: thevalue = strtrim(val[element], 2)
  ENDCASE

  IF(type NE 8) THEN BEGIN
    X_Var_Edit__AddEditEntry, entries, $
                              nentries, $
                              thename, $
                              type, $
                              thevalue, $
                              total_n_elements
  END

ENDFOR

table = 0
IF (NOT recurse) THEN BEGIN
    IF (N_ELEMENTS(entries.value) GT 1) THEN BEGIN
        table = WIDGET_TABLE( abase, $
                              VALUE = TRANSPOSE((entries.value)[0:nentries-1]), $
                              ROW_LABELS = TRANSPOSE((entries.name)[0:nentries-1]), $
                              COLUMN_LABELS = '', $
                              /RESIZEABLE_COLUMNS, $
                              EDIT=~KEYWORD_SET(readOnly), $
                              COLUMN_WIDTHS = 150, $
                              Y_SCROLL_SIZE = YSCROLL_SIZE )
    ENDIF ELSE BEGIN
        table = WIDGET_TABLE( abase, $
                              VALUE = [entries.value], $
                              ROW_LABELS = [entries.name], $
                              COLUMN_LABELS = '', $
                              /RESIZEABLE_COLUMNS, $
                              EDIT=~KEYWORD_SET(readOnly), $
                              COLUMN_WIDTHS=150, $
                              Y_SCROLL_SIZE = YSCROLL_SIZE )
    ENDELSE
ENDIF

return, table
END ;============= end of X_Var_Edit event handling routine task =============


;------------------------------------------------------------------------------
;   procedure X_Var_Edit
;------------------------------------------------------------------------------
; this is the actual routine that is called.  It builds up the variable editing
; fields by calling other support routines and then registers the widget
; heiarchy with the XManager.  Notice that the widget is registered as a MODAL
; widget so it will desensitize all other current widgets until it is done.
;------------------------------------------------------------------------------

PRO X_Var_Edit, var, GROUP = GROUP, NAME = NAME, COLUMN_MAJOR=usecolumns, $
                X_SCROLL_SIZE = X_SCROLL_SIZE, Y_SCROLL_SIZE = Y_SCROLL_SIZE

on_error, 2 ; Return to caller on error.

if(n_params() ne 1) THEN $
  MESSAGE, "Must have one parameter"

if n_elements(var) eq 0 then $
  MESSAGE, 'Argument is undefined.'

; Create parent of modal base if needed
if (N_ELEMENTS(GROUP) NE 1) then begin
  GROUP_ID = WIDGET_BASE(MAP=0)
endif else begin
  GROUP_ID = GROUP
endelse

X_Var_Editbase = WIDGET_BASE(TITLE = "X_Var_Edit", /COLUMN, $    ;create the main base
                             GROUP_LEADER=GROUP_ID, /MODAL, XOFF=!DEVX/5, YOFF=!DEVY/4)

menu = Cw_Bgroup( X_Var_Editbase, $
                  ['Cancel', 'Accept'], $
                  /ROW, $
                  IDS=IDS, $
                  UVALUE="THEBUTTON" )

if (LMGR(/VM)) then begin
   void = WIDGET_LABEL(X_Var_Editbase, $
                       VALUE='Values are not editable in the IDL Virtual Machine.')
endif

; Read-only if an expression was passed in,
; or we are in IDL Virtual Machine (because "execute" is unavailable).
readOnly = ~ARG_PRESENT(var) || LMGR(/VM)

if (readOnly) then begin
    widget_control, ids[1], sensitive=0
end

WIDGET_CONTROL, /HOURGLASS
entries = 0
nentries = 0
table = X_Var_EditField( X_Var_Editbase, var, usetable, entries, nentries, $
                         NAME = NAME, X_SCROLL_SIZE = X_SCROLL_SIZE, $
                         READ_ONLY=readOnly, COLUMN_MAJOR=usecolumns, $
                         Y_SCROLL_SIZE = Y_SCROLL_SIZE )

X_Var_EditStat = {var:var, $
                  entries:entries, $
                  modified:0, $
                  readOnly:readOnly, $
                  table: table, $
                  usetable: usetable}

pX_Var_EditStat = PTR_NEW(X_Var_EditStat, /NO_COPY)

WIDGET_CONTROL, X_Var_Editbase, SET_UVALUE=pX_Var_EditStat

WIDGET_CONTROL, X_Var_Editbase, /REALIZE

XManager, "X_Var_Edit", X_Var_Editbase

; Get the return value
IF ((*pX_Var_EditStat).modified) THEN var = (*pX_Var_EditStat).var
PTR_FREE, pX_Var_EditStat

; Destroy parent of modal base
if (N_ELEMENTS(GROUP) NE 1) then begin
  WIDGET_CONTROL, GROUP_ID, /DESTROY
endif

END ;================== end of X_Var_Edit main routine =======================

