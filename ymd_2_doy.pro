;+
;Procedure: ymd_2_doy
;
;Purpose:  convert [year, month, date] into [year, doy]
;
;Input: year, month, day
;
;Output: doy (day of the year)
;
;Example:
;   ymd_2_doy, year, month, day, doy
;Notes:
;
; Author: Yuguang Tong 
;
; Version:
;   1.0 --> 06/11/2016
;-

pro ymd_2_doy, year, month, day, doy

mdt = [[0, 31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334, 365], $
       [0, 31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335, 366]]

isleap = ((year mod 4) eq 0) - ((year mod 100) eq 0) + $
         ((year mod 400) eq 0) - ((year mod 4000) eq 0)

doy = mdt(month-1, isleap) + day

end
