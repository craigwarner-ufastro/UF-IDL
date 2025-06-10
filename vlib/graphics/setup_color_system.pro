pro setup_color_system, DIRECT_COLOR=direct

  if( !D.name NE "X") then begin
     message,/INFO,"current device is not X windows."
     return
  endif

;detect the number of bits of the X system

  device,GET_VISUAL_DEPTH=visual_depth

  case visual_depth of

     24:  begin
            message,/INFO,'X device is 24 bits color system'
            if keyword_set( direct ) then device,DIRECT_color=24 else device,true_color=24
            device,decompose=0
        end

     16:  begin
            message,/INFO,'X device is 16 bits color system'
            if keyword_set( direct ) then device,DIRECT_color=16 else device,true_color=16
            device,decompose=0
        end

     8:    begin
            message,/INFO,'X device is 8 bits color system'
            device,pseudo_color=8 ; use this for 8 bits X systems
         end

     else : message,"Visual depth is NOT either 24, 16, or 8.",/info
    endcase
end
