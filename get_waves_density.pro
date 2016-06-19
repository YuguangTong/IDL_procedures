;+
;Procedure: get_waves_density
;
;Purpose:  load data products (temperarily only TNR electron density)
;from published L2 CDF files on cdaweb
;
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;   /VERBOSE : set to output some useful info
;Example:
;   get_waves_density
;Notes:
;  This routine is still in development.
; Author: Yuguang Tong
;
;-
pro get_waves_density, files=files,trange=trange,verbose=verbose, $
                  downloadonly=downloadonly, $
                  varformat=varformat, $
                  addmaster=addmaster,tplotnames=tn,source_options=source

datatype = 'h0'

istp_init
if not keyword_set(source) then source = !istp

;URL deprecated by reorg at SPDF
;if datatype eq 'k0'  then    pathformat =
;'wind/mfi/YYYY/wi_k0_mfi_YYYYMMDD_v0?.cdf'

pathformat = 'wind/waves/wav_h0/YYYY/wi_h0_wav_YYYYMMDD_v0?.cdf'

if not keyword_set(varformat) then begin
   varformat = '*'
endif

relpathnames = file_dailynames(file_format=pathformat,trange=trange,addmaster=addmaster)

files = file_retrieve(relpathnames, _extra=source, /last_version)

if keyword_set(downloadonly) then return

prefix = 'wi_'+datatype+'_wav_'
cdf2tplot,file=files,varformat=varformat,verbose=verbose,prefix=prefix ,tplotnames=tn    ; load data into tplot variables

; Set options for specific variables

dprint,dlevel=3,'tplotnames: ',tn

options,/def,tn+'',/lazy_ytitle          ; options for all quantities

end
