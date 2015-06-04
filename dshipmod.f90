  module dship
  private
!  
  logical, save                           :: debug=.true.
  integer, save                           :: stdout=6
  real*8                                  :: rdays
  real, save                              :: missing
  integer, save                           :: nochannels, channel
  integer, save                           :: ncid, timeid, timedim
  integer, save                           :: nlines, prec

  integer, allocatable                    :: Length(:)    
  integer, allocatable                    :: Precision(:) 
  character*20, allocatable               :: Device(:)
  character*20, allocatable               :: Sensor(:)
  character*20, allocatable               :: Format(:)
  character*20, allocatable               :: Unit(:)
  character*20, allocatable               :: Calculation(:) 
  character*20, allocatable               :: Delivered_values(:) 

  integer, allocatable                    :: varid(:)
  include 'netcdf.inc'
 
  public process_header, get_data
      
  contains
      
      subroutine process_header(icnv, ncid_in, title, history)    
!-----------------------------------------------------------------------
!     This procedure analyzes the .cnv file header and writes the header
!     back to the output file 
!-----------------------------------------------------------------------
      character*255       :: linebuff
      integer, intent(in) :: icnv, ncid_in
      integer             :: in, is, ie, status 
      integer             :: i
      character*1         :: c1, c2
      character*64        :: title
      character*255       :: history 
!
! ...   read ASCII .dship-files
!
      in = 0
      ncid = ncid_in
!       
!-----------------------------------------------------------------------
!     read the description file
!-----------------------------------------------------------------------
      ivar = 0
      nrinf = 0
      ncinf = 0
      linebuff = ' '
      rdays_reise = 0.
      rdays_nmea = 0.
      nochannels = 0
      write(stdout,*) 'Analysing file structure'
      do while(index(linebuff,'END').eq.0)
        read(icnv,'(a)',end=1000) linebuff
        if (index(linebuff,'No of channels').gt.0) then
          in = index(linebuff,'=')+1
          read(linebuff(in+1:in+6),*) nochannels
!!          write(stdout,'(I4)') 'The file contains ',nochannels, 'variables' 
          write(stdout,'(a,I4,a)')  'The file contains ',nochannels, ' variables' 
          allocate(Device(nochannels))    
          allocate(Sensor(nochannels))    
          allocate(Format(nochannels))    
          allocate(Length(nochannels))    
          allocate(Precision(nochannels)) 
          allocate(Unit(nochannels))
          allocate(Calculation(nochannels))
          allocate(Delivered_values(nochannels))
        endif  
        if (index(linebuff,'Channel').gt.0) then
          in = index(linebuff,'Channel')+7
          read(linebuff(in:in+6),*) channel
          write(stdout,'(a,I4)') 'processing channel ', channel
        endif
        if (index(linebuff,'Device').gt.0) then
          is = index(linebuff,'"')+1
          ie = index(linebuff(is+1:100),'"')-1
          device(channel) = linebuff(is:is+ie)
        endif
        if (index(linebuff,'Sensor').gt.0) then
          is = index(linebuff,'"')+1
          ie = index(linebuff(is+1:100),'"')-1
          sensor(channel) = linebuff(is:is+ie)
          c1 = ' '
          c2 = '_'
          call replace_char(sensor(channel),c1,c2)
        endif
        if (index(linebuff,'Format').gt.0) then
          is = index(linebuff,'"')+1
          ie = index(linebuff(is+1:100),'"')-1
          format(channel) = linebuff(is:is+ie)
        endif
        if (index(linebuff,'Unit').gt.0) then
          is = index(linebuff,'"')+1
          ie = index(linebuff(is+1:100),'"')-1
          unit(channel) = linebuff(is:is+ie)
!!          c1 = '�'
!!          c2 = '_'
!!          call replace_char(unit(channel),c1,c2)
        endif
        if (index(linebuff,'Calculation').gt.0) then
          is = index(linebuff,'"')+1
          ie = index(linebuff(is+1:100),'"')-1
          calculation(channel) = linebuff(is:is+ie)
        endif
        if (index(linebuff,'Delivered values').gt.0) then
          is = index(linebuff,'=')+2
          read(linebuff(is:255),*) Delivered_values(channel)
        endif
        if (index(linebuff,'Length').gt.0) then
          is = index(linebuff,'=')+2
          read(linebuff(is:255),*) length(channel)
        endif
        if (index(linebuff,'Precision').gt.0) then
          is = index(linebuff,'=')+2
          read(linebuff(is:255),*) precision(channel)
        endif
      enddo
 1000  continue
      do channel=1,nochannels
!!        print*,device(channel)
          print*,channel,' ',sensor(channel),' ',format(channel)
!!        print*,format(channel)
!!        print*,unit(channel)
!!        print*,Delivered_values(channel)
!!        print*,length(channel)
      enddo  

      status = nf_def_dim(ncid, 'time', nf_unlimited, timedim)
      status = nf_def_var(ncid, 'time', nf_double, 1, timedim, timeid)
      status = nf_put_att_text(ncid, timeid, 'units',30, 'days since 2000-01-01 00:00:00')
      status = nf_put_att_text(ncid, timeid, 'time_origin', 19, '1-jan-2000 00:00:00')
      allocate(varid(nochannels))  
      do n=1,nochannels
        if (trim(format(n)) .eq. "Real"  &
        .or. trim(format(n)) .eq. "PosLon" &
        .or. trim(format(n)) .eq. "PosLat" &
        .or. trim(format(n)) .eq. "PosDeg") &
        then
          missing = 0.
          prec = precision(n)
          if (prec .gt. 0) prec = prec + 1
          do l=1, length(n) - prec
            missing = missing*10. + 9.
          enddo
          do l=1, precision(n)
            missing = missing + 9./10**l
          enddo 
          status = nf_def_var(ncid, trim(sensor(n)), nf_real, 1, timedim, varid(n))
          status = nf_put_att_real(ncid, varid(n), 'missing_value', NF_REAL, 1, missing)
        else
          status = nf_def_var(ncid, trim(sensor(n)), nf_char, 1, timedim, varid(n))
        endif
        status = nf_put_att_text(ncid, varid(n), 'units', len_trim(unit(n)),trim(unit(n)))
        status = nf_put_att_text(ncid, varid(n), 'long_name', len_trim(sensor(n)),trim(sensor(n)))
        status = nf_put_att_text(ncid, varid(n), 'device', len_trim(device(n)),trim(device(n)))
        status = nf_put_att_text(ncid, varid(n), 'calculation', len_trim(Calculation(n)),trim(Calculation(n)))
        status = nf_put_att_text(ncid, varid(n), 'delivered values', len_trim(Delivered_values(n)),trim(Delivered_values(n)))
      enddo  
      status = nf_put_att_text(ncid, nf_global, 'title', len_trim(title), trim(title))
      status = nf_put_att_text(ncid, nf_global, 'history', len_trim(history), trim(history))
!
      status = nf_enddef(ncid)
      write(stdout,*) 'nc-file header has been created with err ',status
!
      end subroutine process_header
		
      subroutine get_data(icnv) 
      character*1024      :: linebuff
      integer, intent(in) :: icnv
      integer             :: i, n, itab, status
      integer             :: ihour, imin, isec, iday, imon, iyear 
      real                :: temp, lat, lon
      character*20        :: ctemp, cgeod, cgeom
      character*1         :: cflag 
       
      do i=1, 4	
	read(icnv,*,end=100) linebuff
      enddo
      nlines=0
      do i=1, 4000000
        nlines = nlines + 1
	read(icnv,'(a)',end=100) linebuff
        rdays = days_since_2000(linebuff(1:19))
        status = nf_put_var1_double(ncid, timeid, nlines, rdays)
        linebuff = linebuff(21:len_trim(linebuff))
        do n=1, nochannels 
           itab = index(linebuff,'	')
           if (itab .eq. 0) itab = len_trim(linebuff)
           ctemp = linebuff(1:itab)
           if (trim(format(n)) .eq. "Real" &
          .or. trim(format(n)) .eq. "PosDeg") then
             read(ctemp,*) temp
             status = nf_put_var1_real(ncid, varid(n), nlines, temp)
           else
             if (trim(format(n)) .eq. "PosLon") then
                cgeod = ctemp(1:3)
                cgeom = ctemp(4:13)
                cflag = ctemp(14:14)
                read(cgeod,*) lon
                read(cgeom,*) temp
                lon = lon + temp/60.
                if (cflag .eq. 'W' ) lon = -lon
                status = nf_put_var1_real(ncid, varid(n), nlines, lon)
             elseif (trim(format(n)) .eq. "PosLat") then
                cgeod = ctemp(1:3)
                cgeom = ctemp(4:13)
                cflag = ctemp(14:14)
                read(cgeod,*) lat
                read(cgeom,*) temp
                lat = lat + temp/60.
                if (cflag .eq. 'S' ) lat = -lat
                status = nf_put_var1_real(ncid, varid(n), nlines, lat)
             else
!!               status = nf_put_vara_text(ncid, varid(n), nlines, nlines, cdata_temp(n))
             endif
           endif
           linebuff = linebuff(itab+1:len_trim(linebuff))
        enddo   
      enddo
      
100   continue          

      deallocate(Device)    
      deallocate(Sensor)    
      deallocate(Format)    
      deallocate(Length)    
      deallocate(Precision) 
      deallocate(Unit)
      deallocate(Calculation)
      deallocate(Delivered_values)
      deallocate(varid)

      write(stdout,*) nlines,' read!'
      return
      end subroutine get_data
      
      real*8 function days_since_2013(cd) 
      character*19, intent(in)    :: cd
      integer             :: ihr, imn, isc, idy, imon, iyr, days 

      integer             :: monlen(12)
      data (monlen(i),i=1,12) /31,28,31,30,31,30,31,31,30,31,30,31/
      
      read(cd(1:2),*) idy
      read(cd(4:5),*) imon
      read(cd(7:10),*) iyr
      read(cd(12:13),*) ihr
      read(cd(15:16),*) imn
      read(cd(18:19),*) isc
      if(int(iyr/4)*4.eq.iyr) monlen(2) = 29
      days = 0
      do n=1,imon-1
        days = days + monlen(n)
      enddo
      days_since_2013 = (iyr - 2013)*365 &
                    +dfloat(days + idy - 1) + (ihr+(imn+isc/60.)/60.)/24.
      return
      end function days_since_2013
      
      real*8 function days_since_2000(cd) 
      character*19, intent(in)    :: cd
      integer             :: ihr, imn, isc, idy, imon, iyr, days, nleap 

      integer             :: monlen(12)
      data (monlen(i),i=1,12) /31,28,31,30,31,30,31,31,30,31,30,31/
      
      read(cd(1:2),*) idy
      read(cd(4:5),*) imon
      read(cd(7:10),*) iyr
      read(cd(12:13),*) ihr
      read(cd(15:16),*) imn
      read(cd(18:19),*) isc
      if(int(iyr/4)*4.eq.iyr) monlen(2) = 29
      
      nleap=0
      if(iyr.gt.2000) nleap = int((iyr-2000)/4) + 1
      days = 0
      do n=1,imon-1
        days = days + monlen(n)
      enddo
      days_since_2000 = (iyr - 2000)*365 + nleap &
                    +dfloat(days + idy - 1) + (ihr+(imn+isc/60.)/60.)/24.
      return
      end function days_since_2000
      
      subroutine replace_char(string, old_char, new_char)
      character(*)    :: string
      character*1     :: old_char, new_char
      integer         :: isunit, ieunit
      do while (index(trim(string), old_char).ne.0) 
        isunit = index(trim(string), old_char) 
        ieunit = len_trim(string)
        string = string(1:isunit-1)//new_char//string(isunit+1:ieunit)
      enddo
!!      do while (index(string,"__").ne.0) 
!!        isunit = index(string,"__") 
!!        ieunit = len_trim(string)
!!        string = string(1:isunit-1)//'_'//string(isunit+1:ieunit)
!!      enddo
      return
      end subroutine replace_char
  end module dship

