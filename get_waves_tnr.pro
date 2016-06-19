
pro  get_waves_tnr,						$
	DT=dt,							$
	CHANNEL=channel,					$
	UNITS=units,						$
	BANDS=bands,						$
	TRANGE=trange,						$
	AGCFLAG=agcflag,					$
	BACKGROUND=background,					$
	NNAME=nname

;+
;
;	KEYWORDS
;		dt:		average interval, in seconds
;		channel:	TNR channel, A=1, B=2	
;		units:		data units, 'MICROVOLTS' or 'DBVOLTS'
;		bands:		TNR bands to use in calculating spectra
;
;	Stuart Bale+1997
;-

fillval = !values.f_nan
if not keyword_set(NNAME) then nname='tnr_ch'

@wind_com.pro

if keyword_set(TRANGE) then begin dtimes=trange
  times=time_string(dtimes,format=2)
endif
if not keyword_set(TRANGE) then begin
  get_timespan,tt
  times=time_string(tt,format=2)
endif

t0=str_sep(times(0),'_')
yymmdd=t0(0)
tstart_hr=long(t0(1))
tstop_hr=235959l
if (n_elements(times) gt 1) then begin
  t1=str_sep(times(1),'_')
  tstop_hr=long(t1(1))
  if(t0(0) ne t1(0)) then tstop_hr=235959l
endif

if not keyword_set(DT) then dt = 60
if not keyword_set(CHANNEL) then channel = 1
if not keyword_set(UNITS) then units = 'MICROVOLTS'
if not keyword_set(BANDS) then bands = [1,1,1,1,1]
bands=float(bands)

if ((channel lt 1) or (channel gt 2)) then channel=1
if ((units ne 'MICROVOLTS') and (units ne 'DBVOLTS')) then	$
	units = 'MICROVOLTS'

schan=strcompress(string(channel),/remove_all)
uitem = 'SPECTRA_'+schan+'_'+units+'_R4'
agcitem = 'AGC_'+schan

;	get the whole day

t_to_seconds,tstart_hr,secstart
t_to_seconds,tstop_hr,secstop

filename='wi_lz_wav_'+yymmdd+'*.dat'

ok=0l & ch = 0l & rs = 0l
ntimes=long((secstop-secstart)/abs(dt))
imax=ntimes
if (dt lt 0) then imax = 24*60*60l
dt1=findgen(imax)*dt + secstart
dt2=(findgen(imax)+1)*dt + secstart
scount=fltarr(imax)
nseconds=dblarr(imax)
agc=fltarr(imax)

ssize=96l
dsize=512l
zero = 0d
one = 1l
two = 2l
scetr8=0d
scet=lonarr(2)
four = 4l
state=0l
fpband=32l

ndatabuf=lonarr(512)
databuf=fltarr(512)
freqs=fltarr(512)
a=fltarr(96) & b=a & c=a & d=a & e=a

mspectrum=fltarr(ssize,imax)
evn = 0l & oevn = 0l

if (filename eq '') then filename = 'offline'
ver='AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' 
file = ver
gt20=indgen(76)+20

evn=0l & evnl = 1l
ant1=0l & antenna = 'AAAAAAAAAAAAAAAAAAA'
inttime = float(0)
exlength=float(0)
ur8_time=double(0)
ur8_stop=double(0)

ok=w_channel_open(ch,filename)
if (ok ne 1) then begin
  print,'File NOT opened'
  ok=w_channel_close(ch)
  return
endif
ok = w_version(ver)
ok = w_messages_off(ch)
ok = w_channel_filename(ch,file)
print,'reading : ',file
tmp=str_sep(file,'_')
sstring=strcompress(tmp(3),/remove_all)
tstart_day=long(sstring)

ok = w_ur8_from_ymd_i(ur8_stop,tstart_day,tstop_hr)
ok = w_ur8_from_ymd_i(ur8_time,tstart_day,tstart_hr)
ok = w_channel_position(ch,ur8_time)

seconds=float(0)
f0=float(0)
df=float(0)
spectra=fltarr(96)
calflag=0l

scets=dblarr(512)


bigscet=0l
iok=1l
jj=0l

s0=systime(1)
while((iok eq 1) and (scetr8 le ur8_stop)) do begin
  
  iok=w_event(ch,'TNR')

  item = 'EVENT_SCET'
  ok=w_item_i4(ch,item,scet,two,rs)
  t_to_seconds,scet(1),seconds

  item = 'EVENT_SCET_R8'
  ok=w_item_r8(ch,item,scetr8,one,rs)

  item = 'SPECTRA_1_SCET_R8'
  ok=w_item_r8(ch,item,scets,512l,rs)
  sst=ur8_to_sunseconds(scets)

  ;ok = w_channel_position(ch, ur8new)

  ;stop

  print, scetr8, scets[0], scets[0] - scetr8, $
         time_string(ur8_to_sunseconds(scetr8))
  
;  stop

  
  item = 'EVENT_INTEGRATE_SECS_R4'
  ok=w_item_r4(ch,item,inttime,one,rs)

  item = 'FREQUENCY_COVERAGE_HZ_R4'
  ok=w_item_r4(ch,item,freqs,dsize,rs)

  ok = w_item_r4(ch,uitem,databuf,dsize,rs)

  item = 'CAL_FLAG'
  ok = w_item_i4(ch,item,calflag,one,rs)

  if (calflag eq 1) then databuf(*) = fillval

  item = 'EVENT_STATE'		; 2 = ABCDE and 3 = ACE
  ok=w_item_i4(ch,item,state,1l,rs)

  at = 0.
  if keyword_set(agcflag) then begin
    ok = w_item_r4(ch,agcitem,at,1l,rs)
    print,at
  endif

  if (state eq 3l) then begin	; 3==ACE 
    for k=0,3 do begin
      sevent = ( databuf(96*k : 96*k + 95) )
      sec = seconds + k*inttime*3
      iii=where((sec ge dt1) and (sec le dt2))
      a(0:31) = sevent(0:31)*bands(0)
      c(32:63) = sevent(32:63)*bands(2)
      e(64:95) = sevent(64:95)*bands(4)
      spectra=sevent
      if (iii(0) ne -1) then begin
	   mspectrum(*,iii(0)) = mspectrum(*,iii(0)) + spectra(*)
	   scount(iii(0)) = scount(iii(0)) + 1
	   agc(iii(0)) = at
      endif
      bigscet=[bigscet,sec]	
      if ((dt lt 0) and (total(spectra) gt 0.))then begin
        mspectrum(*,jj) = spectra(*)
	    ;nseconds(jj) = ur8_to_sunseconds(scetr8)
	    nseconds(jj) = ur8_to_sunseconds(scets[96*k])
        agc(jj) = at
        jj = jj + 1
      endif
    endfor
  endif

  ;stop
  ; plot, databuf
  ; plot, (scets - scets[0]) * 86400.d

  if (state eq 2l) then begin	; 2==ABCDE 
    for k=0,2 do begin
      sevent=databuf(160*k:160*k+159)
      sec = seconds + k*inttime*5
      iii=where((sec ge dt1) and (sec le dt2))
      a(0:31) = sevent(0:31) & b(16:47) = sevent(32:63)
      c(32:63) = sevent(64:95) & d(48:79) = sevent(96:127)
      e(64:95) = sevent(128:159)
      spectra(0:15) = a(0:15)*bands(0)
      spectra(16:31) = (a(16:31)*bands(0) + b(16:31)*bands(1))/	$
	(bands(0) + bands(1))
      spectra(32:47) = (b(32:47)*bands(1) + c(32:47)*bands(2))/ $
	(bands(1) + bands(2))
      spectra(48:63) = (c(48:63)*bands(2) + d(48:63)*bands(3))/	$
	(bands(2) + bands(3))
      spectra(64:79) = (d(64:79)*bands(3) + e(64:79)*bands(4))/ $
	(bands(3) + bands(4))
      spectra(80:95) = e(80:95)*bands(4)
      if ((dt lt 0) and (total(spectra) gt 0.)) then begin
;      if ((dt lt 0)) then begin
        mspectrum(*,jj) = spectra(*)
	nseconds(jj) = ur8_to_sunseconds(scetr8)
	agc(jj) = at
        jj = jj + 1l
      endif
      if (iii(0) ne -1) then begin
	   mspectrum(*,iii(0)) = mspectrum(*,iii(0)) + spectra(*)
	   scount(iii(0)) = scount(iii(0)) + 1
	   agc(iii(0)) = at
      endif
      bigscet=[bigscet,sec]
    endfor
  endif


  item = 'ANTENNA_1'
  ok=w_item_i4(ch,item,ant1,one,rs)
  ok=w_item_xlate(ch,'TNR',item,ant1,antenna)

  item = 'EVENT_INTEGRATE_SECS_R4'
  ok=w_item_r4(ch,item,inttime,one,rs)
  
endwhile
ok=w_channel_close(ch)


if (dt ge 0) then begin
  if (max(scount) eq 0) then begin
    print, 'No data found'
    return
  endif
endif

if (dt ge 0) then begin
  for i=0,imax-1 do begin
    if (scount(i) ge 1 ) then begin
    mspectrum(*,i) = mspectrum(*,i)/float(scount(i))
    endif
    if (scount(i) eq 0 ) then begin
     ni=i
     while((scount(ni) eq 0) and (ni gt 0)) do begin
       ni = ni - 1
     endwhile
     while((scount(ni) eq 0) and (ni lt imax)) do begin
       ni = ni + 1
     endwhile
     mspectrum(*,i) = mspectrum(*,ni)
    endif
  endfor
  seconds = (dt1 + dt2)/2.
  nn=n_elements(seconds)
  times=dblarr(nn)
for i=0,nn-1 do begin
  yy=fix(strmid(t0(0),0,4))
  mo=fix(strmid(t0(0),4,2))
  dd=fix(strmid(t0(0),6,2))
  hr=fix(seconds(i)/3600.)
  min=fix(seconds(i)/60.) - hr*60
  sec=fix(seconds(i) mod 60)
  fsec = double((seconds(i) - floor(seconds(i))))
  tt={year:yy,month:mo,date:dd,hour:hr,min:min,sec:sec,fsec:fsec}
  times(i) = time_double(tt)
endfor
endif


if (dt lt 0) then begin
  nn=jj-1l
  times=nseconds(0:nn-1)
endif 
mspectrum=transpose(mspectrum(*,0:nn-1))
freqs=freqs(0:95)

if keyword_set(BACKGROUND) then begin
  ones=replicate(1.0,n_elements(times))
  bs = ones#background
  mspectrum = mspectrum - bs
endif

isort=sort(times)
mspectrum=mspectrum(isort,*)

yl=1
bands=fix(bands)
bname=''
if (total(bands) ne 5) then begin
  bname = '_'
  for i=0,4 do begin
    bname = bname + strcompress(string(bands(i)),/remove_all)
  endfor
endif
if (units eq 'DBVOLTS') then yl = 0
datastr={x:times,y:mspectrum,v:freqs}
yt='TNR : freq (Hz)'
dname=nname+schan + bname
if keyword_set(BACKGROUND) then dname=dname + '_bkg'
store_data,dname,data=datastr,			$
	dlim={ylog:yl,ytitle:yt,		$
	panel_size:2,zlog:1,spec:1,		$
	ystyle:1}

options,dname,'no_interp',0
if (dt lt 0) then options,dname,'no_interp',1

if keyword_set(agcflag) then begin
  ds={x:times,y:(agc)}
  store_data,agcitem,data=ds
endif


print,string(systime(1)-s0) + ' seconds execution time'
end
