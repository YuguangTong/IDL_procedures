;+
;Procedure: ACE_PAD_LOAD
;
;Purpose:  Loads ACE electron pitch angle distribution (PAD)
;
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;
;Example:
;   ace_pad_load: load PAD in physical units (#/cm^2/sr/eV)
;
;Notes:
;  This routine is still in development.
; Author: Yuguang Tong 06/11/2016
;
;-
pro ace_pad_load,type,files=files,trange=trange, $
                 downloadonly=downloadonly, $
                 addmaster=addmaster,tplotnames=tn,source_options=source, $
                 reduce=reduce
                 

ace_sci_l3_init
if not keyword_set(source) then source = !ace_l3

; if data files are not defined
if not keyword_set(files) then begin
; URLs to ACE science center
   pathformat = 'swepam/data/YYYY/ace_swepam_pa272ev_YYYY-DOY_v?.dat'

   relpathnames = file_dailynames(file_format=pathformat, $
                                  trange=trange,addmaster=addmaster)

   files = file_retrieve(relpathnames, _extra=source, /last_version)
;   files = file_retrieve(relpathnames, _extra=source)
endif

if keyword_set(downloadonly) then return

format = {year:0, doy:0, hour:0, min:0, sec:0, sct:0l, pad:fltarr(20)}

dat = read_asc(files, format=format)

; convert day of year to month and date
doy_to_month_date, dat.year, dat.doy, month, day

nn = n_elements(dat.year)
times=dblarr(nn)

for i=0, nn-1 do begin
   yy = (dat.year)(i)
   mo = month(i)
   dd = day(i)
   hr = (dat.hour)(i)
   min = (dat.min)(i)
   sec = (dat.sec)(i)
;   tt={year:yy,month:mo,date:dd,hour:hr,min:min,sec:sec, fsec:0}
   tt={year:yy, month:mo, date:dd, hour:hr,min:min,sec:sec, fsec:0, tdiff:0}
   times(i) = time_double(tt)
endfor

; if reduce unit
if keyword_set(reduce) then begin
   max_pad = mean(dat.pad, dimension=1)
   ones = replicate(1, 20)
   pad_denom = ones#max_pad
   temp = dat.pad / pad_denom
   dat.pad  = temp
endif

; construct tplot for pad

angles = indgen(20) * 9 + 4.5
datastr = {x: times, y: transpose(dat.pad), v:angles}
yt = 'ACE_elec_PA_272eV'

if not keyword_set(tn) then tn = 'ace_swepam_pa272ev'

store_data, tn, data = datastr, $
            dlim={ylog:0, ytitle:yt, panel_size:1, $
                  zlog:0, spec:1, ystyle:1}

dprint, dlevel=3, 'tplotnames: ', dname
end


