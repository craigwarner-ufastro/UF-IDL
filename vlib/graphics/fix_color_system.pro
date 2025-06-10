pro fix_color_system,manu=manu


;try to detect itself the number of bits of the X system
spawn,'xdpyinfo',xdpyinfo
ind_start_screen = (where(strpos(xdpyinfo,'default') ne -1))
ind_start_screen=ind_start_screen(0) 
depths = (xdpyinfo(ind_start_screen:*))(where(strpos(xdpyinfo(ind_start_screen:*),'depths') ne -1))

main_depth=fix((str_sep(strcompress((str_sep(depths,','))(0)),' '))(3))

spawn,'uname -a',info_system
info_system=info_system((n_elements(info_system))-1)
syst_exp = (str_sep(info_system,' '))(0)

if keyword_set(manu) then begin
 read,'Color Depth ?',main_depth
 goto,skip1
endif

if syst_exp eq 'Linux' or syst_exp eq 'Darwin' then begin  ;darwin is for MacIntosh

skip1:

    case main_depth of

        24:  begin
            print,'Found 24 bits Linux color system'
            device,true_color=24 ; use this
            device,decompose=0  ; for 24 bits X systems
        end

       16:  begin
            print,'Found 16 bits Linux color system'
            device,true_color=16 ; use this
            device,decompose=0  ; for 16 bits X systems
        end


        8:    begin
            print,'Found 8 bits Linux color system'
            device,pseudo_color=8 ; use this
            device,decompose=0  ; for 8 bits X systems
        end
        else : res=dialog_message('No valid X system found, fix it by hands',/info)
    endcase

endif


end
