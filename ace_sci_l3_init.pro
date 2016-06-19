;+
;Procedure: ACE_SCI_L3_INIT
;
;Purpose: Initializes a setting for ace_pad_load, which loads level 3
;   data from Ace science cneter
; 
;Notes:
;
; Author: Yuguang Tong 6/11/2016
;
;-


pro ace_sci_l3_init, reset=reset, local_data_dir=local_data_dir, remote_data_dir=remote_data_dir

    defsysv,'!ace_l3',exists=exists
    if not keyword_set(exists) then begin
       defsysv,'!ace_l3',  file_retrieve(/structure_format)
    endif

    if keyword_set(reset) then !ace_l3.init=0

    if !ace_l3.init ne 0 then return

    !ace_l3 = file_retrieve(/structure_format)
    !ace_l3.local_data_dir = spd_default_local_data_dir() + 'ace' + path_sep()
    !ace_l3.remote_data_dir = 'http://www.srl.caltech.edu/ACE/ASC/DATA/level3/'

    if file_test(!ace_l3.local_data_dir+'.master') then !ace_l3.no_server=1  
    ; local directory is the master directory

    if keyword_set(name) then call_procedure,name

    !ace_l3.init = 1

    printdat, /values, !ace_l3, varname='!ace_l3
end
