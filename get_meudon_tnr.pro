;+
;Procedure: GET_MEUDON_TNR
;
;Purpose:  Loads tnr spectra calibrated by Meudon
;
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;Examples:
;   get_medeudon_tnr
;
;Notes:
; Author: Yuguang
;
;-
pro get_meudon_tnr, trange, tn

meudon_tnr_dir = getenv('MEUDON_TNR_DIR')
pathformat = meudon_tnr_dir + '/' + 'YYYY/TNR_XY_ACE_YYYYMMDD.sav'
pathnames = file_dailynames(file_format = pathformat, trange = trange)

nn = n_elements(pathnames)
times = []
spectra = []
for i = 0, nn-1 do begin
   restore, pathnames[i]
   times = [times, ur8_to_sunseconds(data.timeur8)]
   spectra = [spectra, data.spectra]
endfor 

if not keyword_set(tn) then tn = 'wind_tnr_spec'

fbins = 4 * 2^((2*findgen(96)+1)/32.d)
datastr = {x: times, y: spectra, v: fbins}
yt = 'f[kHz]'
store_data, tn, data = datastr, $
            dlim={ylog:1, ytitle:yt, panel_size:1, $
                  zlog:0, spec:1, ystyle:1}
options,tn,'no_interp',1

end

