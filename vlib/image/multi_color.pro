; pro multi_color,image_1, image_2, image_3, image_4
;
;+
;NAME:
;       MULTI_COLOR
;
; PURPOSE:
;       This provides an interactive means of adjusting display ranges and 
;       stretches for generating "true" color images from the combination 
;       of 1, 2, 3, or 4 input images.
;
; CALLING SEQUENCE:
;       MULTI_COLOR, IMAGE_1, [IMAGE_2, IMAGE_3, IMAGE_4]
;
; INPUT:
;       IMAGE_1 =  Reddest input image
;       IMAGE_2 =  Next reddest input image
;       IMAGE_3 =  Bluer input image
;       IMAGE_4 =  Bluest input image
;
; OUTPUT ARGUMENTS:
;       none.
;
; OPTIONAL INPUT KEYWORDS:
;       none.
;
; NOTES: 
;
;       If 1 image is specified, it is displayed in greyscale (white).
;       2 images are displayed as {orange, light blue}, {red, cyan}, 
;          or {magenta,green}.
;       3 images are displayed as {red, green, blue} respectively.
;       4 images are displayed as {red, yellow-green, cyan, violet},
;        {pink, yellow, blue-green, blue}, or {red, orange, green, blue}.
;
;       In each case the colors are balanced so that equal scaled intensities
;       in all images will appear grey. (For 4 images, the first and third 
;       pair and the second and fourth pair will also add up to grey.)
;       The third 4-color combination is an exception to this rule.
;
;       All input images do not need to have the same x,y-dimensions, but 
;       should be aligned at their lower left corners.
;
;       It is probably not helpful to BYTSCL the input images (that's done 
;       within the procedure), but in some cases it may be helpful to 
;       truncate the input images with < and/or > operators, e.g. 
;       IDL> multi_color, one_image>0, another_image>(-100)<1e3
;
;       A droplist allows the selection of {linear, logarithmic, square-root,
;       histogram-equalized} stretches. The non-linear stretches are intended
;       to help with display of high dynamic range images. I'm not sure if
;       histogram-equalized ever looks good, but it's there anyways.
;
;       With 4 input images a second droplist controls whether the images are
;       clipped or scaled as they are combined to form the RGB images. If 
;       clipped, then any single image is scaled to the full range of 
;       brightness, but a color channel (scaled 0:255) may saturate from the 
;       combination of two of the images. If scaled, it takes the full
;       scaled intensity of ALL input images to satureate all color channels,
;       but any single image will display over only 1/2 of the full range.
;       The scaled setting seems most useful for logarithmic stretches.
;
;       For 2 or 4 input images a third droplist allows the selection of 
;       alternate color assignments. The default sets provide somewhat more 
;       even brightness of the colors. 
;
;       The min and max sliders for each image can be used to adjust the 
;       ranges of the stretch. However, they are less useful for the non-linear
;       stretches, and you'll probably want to do fine tuning by entering 
;       values directly in the associated text boxes.
;
;       The "Save TIFF" button saves the displayed image as a TIFF file. There
;       is no overwrite protection so be careful. 
;
;       If you would rather do it yourself, the color combinations used are
;       2-colors (set 1):
;           R = image_1;  G = 0.50*image_1 + 0.50*image_2; B = image_2
;       2-colors (set 2):
;           R = image_1;  G = image_2; B = image_2
;       2-colors (set 3):
;           R = image_1;  G = image_2; B = image_1
;       4-colors (set 1):
;           R = 0.50*image_1 + 0.25*image_2 + 0.25*image_4
;           G = 0.50*image_2 + 0.50*image_3
;           B = 0.50*image_3 + 0.50*image_4
;       4-colors (set 2):
;           R = 0.50*image_1 + 0.50*image_2
;           G = 0.50*image_2 + 0.50*image_3
;           B = 0.25*image_1 + 0.25*image_3 + 0.50*image_4       
;       4-colors (set 3):
;           R = 0.50*image_1 + 0.50*image_2
;           G = 0.33*image_2 + 0.67*image_3
;           B = image_4       
;
; SUBROUTINE CALLS:
;       ADD_ARRAYS, (included in this file) adds different-sized arrays
;       MULTI_COLOR_EVENT, (included in this file) widget event handler
;
; REVISION HISTORY:
;       8/03 R. Arendt, SSAI, arendt@stars.gsfc.nasa.gov , original version.
;      12/05 R. Arendt, SSAI, arendt@milkyway.gsfc.nasa.gov , addded third 
;           options for 2- and 4-color combinations: magenta-green, and 
;           red-orange-green-blue (often used by SSC for IRAC images).
;-


;#############################################################################

function add_arrays, a, b
;
; This function adds the overlapping areas of two differently sized arrays.
; The returned array encompasses the maximum dimensions of the inputs.
;
size_a = size(a)
size_b = size(b)

c = fltarr(size_a[1]>size_b[1],size_a[2]>size_b[2])
c[0:size_a[1]-1,0:size_a[2]-1] = a
c[0:size_b[1]-1,0:size_b[2]-1] = c[0:size_b[1]-1,0:size_b[2]-1] + b

return,c
end

;#############################################################################

pro multi_color_event, ev
; Event-handler routine. This updates the display whenever widgets are changed.
COMPILE_OPT hidden

; Done button. Kill everything.
if (widget_info(ev.id,/uname) eq 'done') then begin
  widget_control,ev.top,/destroy
  return
endif

; Save button. Save a TIFF file and return.
if (widget_info(ev.id,/uname) eq 'save') then begin
  savefile = dialog_pickfile(/write,file='idl.tif')
  if (savefile ne '') then begin
    image = tvrd(/true)
    write_tiff,savefile,reverse(image,3)
  endif
  return
endif

; Read slider settings for min, max image scalings.
widget_control,widget_info(ev.top,find_by_uname='slide_min1'),get_value=min_1
widget_control,widget_info(ev.top,find_by_uname='slide_max1'),get_value=max_1
widget_control,widget_info(ev.top,find_by_uname='slide_min2'),get_value=min_2
widget_control,widget_info(ev.top,find_by_uname='slide_max2'),get_value=max_2
widget_control,widget_info(ev.top,find_by_uname='slide_min3'),get_value=min_3
widget_control,widget_info(ev.top,find_by_uname='slide_max3'),get_value=max_3
widget_control,widget_info(ev.top,find_by_uname='slide_min4'),get_value=min_4
widget_control,widget_info(ev.top,find_by_uname='slide_max4'),get_value=max_4

widget_control, ev.top, get_uvalue=stash

; Update stretch selection
if (widget_info(ev.id,/uname) eq 'stretch') then begin
  stash.sel_index = ev.index 
  widget_control, ev.top, set_uvalue=stash
endif
stretch = stash.sel_index

; Update 4-color addition type selection
if (widget_info(ev.id,/uname) eq 'type4') then begin
  stash.type4 = ev.index 
  widget_control, ev.top, set_uvalue=stash
endif
type4 = stash.type4

; Update alternate colors selection, and redraw color legend.
if (widget_info(ev.id,/uname) eq 'alt_color') then begin
  stash.alt_color = ev.index 
  widget_control, ev.top, set_uvalue=stash
  wset,stash.drawID_leg
  x16 = bindgen(16,16)
  case stash.n_im of
    1: 
    2: begin
      case (stash.alt_color) of 
        0: begin
         ; orange-blue
         erase
         tv,x16,0,ch=1
         tv,x16*0.5,0,ch=2
         tv,x16*0.5,1,ch=2
         tv,x16,1,ch=3
        end
       1: begin
         ; red-cyan
         erase
         tv,x16,0,ch=1
         tv,x16,1,ch=2
         tv,x16,1,ch=3
       end
       2: begin
         ; magenta-green
         erase
         tv,x16,0,ch=1
         tv,x16,1,ch=2
         tv,x16,0,ch=3
       end
      endcase
    end
    3:
    4: begin
      case (stash.alt_color) of 
        0: begin
         ; red-green-blue-violet (Red-oriented)
         erase
         tv,x16,0,ch=1
         tv,x16*0.5,1,ch=1
         tv,x16,1,ch=2
         tv,x16,2,ch=2
         tv,x16,2,ch=3
         tv,x16*0.5,3,ch=1
         tv,x16,3,ch=3
        end
       1: begin
         ; pink-yellow-aqua-blue (Blue-oriented)
         erase
         tv,x16,0,ch=1
         tv,x16*0.5,0,ch=3
         tv,x16,1,ch=1
         tv,x16,1,ch=2
         tv,x16,2,ch=2
         tv,x16*0.5,2,ch=3
         tv,x16,3,ch=3
       end
       2: begin
         ; red-orange-green-blue (SSC)
         erase
         tv,x16,0,ch=1
         tv,x16,1,ch=1
         tv,x16*0.5,1,ch=2
         tv,x16,2,ch=2
         tv,x16,3,ch=3
       end
      endcase
    end
  endcase
  wset,stash.drawID
endif

; Redraw images.
; A) Byte-scale images according desired stretch
case stretch of
  0: begin
       image_1 = bytscl(stash.im_1,min_1,max_1)
       image_2 = bytscl(stash.im_2,min_2,max_2)
       image_3 = bytscl(stash.im_3,min_3,max_3)
       image_4 = bytscl(stash.im_4,min_4,max_4)
     end
  1: begin
       m1 = min(stash.im_1[where(stash.im_1 gt 0)],/nan)
       m2 = min(stash.im_2[where(stash.im_2 gt 0)],/nan)
       m3 = min(stash.im_3[where(stash.im_3 gt 0)],/nan)
       m4 = min(stash.im_4[where(stash.im_4 gt 0)],/nan)
       image_1=bytscl(alog10(stash.im_1>m1),alog10(min_1>m1),alog10(max_1>m1))
       image_2=bytscl(alog10(stash.im_2>m2),alog10(min_2>m2),alog10(max_2>m2))
       image_3=bytscl(alog10(stash.im_3>m3),alog10(min_3>m3),alog10(max_3>m3))
       image_4=bytscl(alog10(stash.im_4>m4),alog10(min_4>m4),alog10(max_4>m4))
     end
  2: begin
       image_1 = bytscl(sqrt(stash.im_1>0),sqrt(min_1>0),sqrt(max_1>0))
       image_2 = bytscl(sqrt(stash.im_2>0),sqrt(min_2>0),sqrt(max_2>0))
       image_3 = bytscl(sqrt(stash.im_3>0),sqrt(min_3>0),sqrt(max_3>0))
       image_4 = bytscl(sqrt(stash.im_4>0),sqrt(min_4>0),sqrt(max_4>0))
     end
  3: begin
       image_1 = (min_1 ge max_1) ? 0*stash.im_1 : $
         hist_equal(stash.im_1,minv=min_1,maxv=max_1)
       image_2 = (min_2 ge max_2) ? 0*stash.im_2 : $
         hist_equal(stash.im_2,minv=min_2,maxv=max_2)
       image_3 = (min_3 ge max_3) ? 0*stash.im_3 : $
         hist_equal(stash.im_3,minv=min_3,maxv=max_3)
       image_4 = (min_4 ge max_4) ? 0*stash.im_4 : $
         hist_equal(stash.im_4,minv=min_4,maxv=max_4)
     end
endcase

; B) Combine/translate images to RGB color images
case stash.n_im of
  1:begin 
      image_r = image_1
      image_g = image_1
      image_b = image_1
    end
  2:begin 
      case (stash.alt_color) of
        0: begin
          ; orange-blue
          image_r = image_1
          image_g = add_arrays(image_1,image_2)/2.
          image_b = image_2
        end
        1: begin
          ; red-cyan
          image_r = image_1
          image_g = image_2
          image_b = image_2
        end
        2: begin
          ; magenta-green
          image_r = image_1
          image_g = image_2
          image_b = image_1
        end
      endcase
    end
  3:begin
      image_r = image_1
      image_g = image_2
      image_b = image_3
    end 
  4:begin 
      case (type4) of 
      0: begin ; Clipped colors
        case (stash.alt_color) of 
          0: begin
            ; red-green-blue-violet (Red-oriented)
            image_r=add_arrays(add_arrays(image_4,image_2)/2.,image_1)<255
            image_g=add_arrays(image_2,image_3)<255
            image_b=add_arrays(image_3,image_4)<255
          end
          1: begin
            ; pink-yellow-aqua-blue (Blue-oriented)
            image_r = add_arrays(image_1,image_2)<255
            image_g = add_arrays(image_2,image_3)<255
            image_b = add_arrays(add_arrays(image_1,image_3)/2.,image_4)<255
          end 
          2: begin
            ; red-orange-green-blue (SSC)
            image_r = add_arrays(image_1,image_2)<255
            image_g = add_arrays(image_2*(2./3),image_3*(4./3))<255
            image_b = (2*image_4)<255
          end 
        endcase
      end
      else: begin ; Scaled colors
        case (stash.alt_color) of 
          0: begin
            ; red-green-blue-violet (Red-oriented)
            image_r=add_arrays(add_arrays(image_4,image_2)/2.,image_1)/2.
            image_g=add_arrays(image_2,image_3)/2.
            image_b=add_arrays(image_3,image_4)/2.
          end
          1: begin
            ; pink-yellow-aqua-blue (Blue-oriented)
            image_r = add_arrays(image_1,image_2)/2.
            image_g = add_arrays(image_2,image_3)/2.
            image_b = add_arrays(add_arrays(image_1,image_3)/2.,image_4)/2.
          end 
          2: begin
            ; red-orange-green-blue (SSC)
            image_r = add_arrays(image_1,image_2)/2.
            image_g = add_arrays(image_2*(2./3),image_3*(4./3))/2.
            image_b = image_4
          end 
        endcase
      end
      endcase
    end
endcase

; C) Display new images for each channel
tv,channel=1,image_r
tv,channel=2,image_g
tv,channel=3,image_b

end

;###########################################################################

pro multi_color,image_1, image_2, image_3, image_4
; This procedure provides an interactive "true-color" display for 1 - 4 images.
; The displayed image can be saved as a TIFF file.

; create dummy images if any are unspecified
if (n_params() le 1) then image_2 = findgen(1,2)
if (n_params() le 2) then image_3 = findgen(1,2)
if (n_params() le 3) then image_4 = findgen(1,2)

; Create the base widget.
base = widget_base(/row)
base0 = widget_base(base,/column)
draw_legend = widget_draw(base0,xsize=16*n_params(),ysize=16)
dlist_stretch = widget_droplist(base0,uname='stretch',title='Stretch type:', $
  value=['linear','log','sqrt','hist_eq'])
dlist_type4 = widget_droplist(base0,value=['clipped','scaled'],$
  uname='type4',title='4-color type:')
dlist_alt = widget_droplist(base0,value=['colors 1','colors 2','colors 3'],$
  uname='alt_color',title='Alt. colors:')
base1 = widget_base(base0,/row)
slide_min1 = cw_fslider(base1,min=min(image_1,/nan),max=max(image_1,/nan),$
  value=min(image_1),/edit,uname='slide_min1',title='Min (image 1)')
slide_max1 = cw_fslider(base1,min=min(image_1,/nan),max=max(image_1,/nan),$
  value=max(image_1),/edit,uname='slide_max1',title='Max (image 1)')
base2 = widget_base(base0,/row)
slide_min2 = cw_fslider(base2,min=min(image_2,/nan),max=max(image_2,/nan),$
  value=min(image_2),/edit,uname='slide_min2',title='Min (image 2)')
slide_max2 = cw_fslider(base2,min=min(image_2,/nan),max=max(image_2,/nan),$
  value=max(image_2),/edit,uname='slide_max2',title='Max (image 2)')
base3 = widget_base(base0,/row)
slide_min3 = cw_fslider(base3,min=min(image_3,/nan),max=max(image_3,/nan),$
  value=min(image_3),/edit,uname='slide_min3',title='Min (image 3)')
slide_max3 = cw_fslider(base3,min=min(image_3,/nan),max=max(image_3,/nan),$
  value=max(image_3),/edit,uname='slide_max3',title='Max (image 3)')
base4 = widget_base(base0,/row)
slide_min4 = cw_fslider(base4,min=min(image_4,/nan),max=max(image_4,/nan),$
  value=min(image_4),/edit,uname='slide_min4',title='Min (image 4)')
slide_max4 = cw_fslider(base4,min=min(image_4,/nan),max=max(image_4,/nan),$
  value=max(image_4),/edit,uname='slide_max4',title='Max (image 4)')
base5 = widget_base(base0,/row)
button_save = widget_button(base5,value='Save TIFF',uname='save',xsize=96)
button_done = widget_button(base5,value='Done',uname='done',xsize=96)

; Create the main draw widget. 
base2 = widget_base(base,/row)
xsize = (size(image_1))[1]>(size(image_2))[1]> $
  (size(image_3))[1]>(size(image_4))[1]
ysize = (size(image_1))[2]>(size(image_2))[2]> $
  (size(image_3))[2]>(size(image_4))[2]
screenxy = get_screen_size()
draw = widget_draw(base2,xsize=xsize,ysize=ysize, $
  x_scroll_size=(screenxy[0]-384)<xsize+2,$
  y_scroll_size=(screenxy[1]-128)<ysize+2,/scroll)

; Realize the widgets.
widget_control, base, /realize

; Retrieve the window ID from the draw widget.
widget_control, draw, get_value=drawID
widget_control, draw_legend, get_value=drawID_leg

; save data to pass to event handler
stash = { drawID:drawID, drawID_leg:drawID_leg, n_im:n_params(), $
  sel_index:0, type4:0, alt_color:0, $
  im_1:image_1, im_2:image_2, im_3:image_3, im_4:image_4}
widget_control, base, set_uvalue=stash

; Draw the color legend for the first time.
wset, drawID_leg
x16 = bindgen(16,16)
case n_params() of
  1: tv,x16
  2: begin
       ; orange-blue
       tv,x16,0,ch=1
       tv,x16*0.5,0,ch=2
       tv,x16*0.5,1,ch=2
       tv,x16,1,ch=3
       end
  3: begin
       tv,x16,0,ch=1
       tv,x16,1,ch=2
       tv,x16,2,ch=3
     end
  4: begin
       ; red-green-blue-violet (Red-oriented)
       tv,x16,0,ch=1
       tv,x16*0.5,1,ch=1
       tv,x16,1,ch=2
       tv,x16,2,ch=2
       tv,x16,2,ch=3
       tv,x16*0.5,3,ch=1
       tv,x16,3,ch=3
     end
endcase

; Load the images for the first time.
wset, drawID
case n_params() of
  1:begin 
      image_r = bytscl(image_1)
      image_g = bytscl(image_1)
      image_b = bytscl(image_1)
      widget_control,slide_min2,sensitive=0
      widget_control,slide_max2,sensitive=0
      widget_control,slide_min3,sensitive=0
      widget_control,slide_max3,sensitive=0
      widget_control,slide_min4,sensitive=0
      widget_control,slide_max4,sensitive=0
      widget_control,dlist_alt,sensitive=0
      widget_control,dlist_type4,sensitive=0
    end
  2:begin 
      image_r = bytscl(image_1)
      image_g = add_arrays(bytscl(image_1),bytscl(image_2))/2.
      image_b = bytscl(image_2)
      widget_control,slide_min3,sensitive=0
      widget_control,slide_max3,sensitive=0
      widget_control,slide_min4,sensitive=0
      widget_control,slide_max4,sensitive=0
      widget_control,dlist_type4,sensitive=0
    end
  3:begin
      image_r = bytscl(image_1)
      image_g = bytscl(image_2)
      image_b = bytscl(image_3)
      widget_control,slide_min4,sensitive=0
      widget_control,slide_max4,sensitive=0
      widget_control,dlist_alt,sensitive=0
      widget_control,dlist_type4,sensitive=0
    end 
  4:begin 
      image_r=add_arrays(add_arrays(bytscl(image_4),bytscl(image_2))/2.,$
        bytscl(image_1))<255
      image_g=add_arrays(bytscl(image_2),bytscl(image_3))<255
      image_b=add_arrays(bytscl(image_3),bytscl(image_4))<255
    end
endcase
tv,image_r,ch=1
tv,image_g,ch=2
tv,image_b,ch=3

; Call XMANAGER to manage the widgets.
;
xmanager, 'multi_color', base, /no_block

end
