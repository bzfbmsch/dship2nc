program corr_cnv
  
  use dship
  
  character*64  inputfile, inputtxt, inputdat, outputfile, title 
  character*255 history 
!
  character*64 filelist  
  data  filelist /'files.cmd'/

 
  parameter (maxcnv=1000,kmcnv=1000,ismoothanz=2)
  integer ::iolst=10, iunit=11, ocnv=12, stdout=6
  real, allocatable :: buff(:)
  
  include 'netcdf.inc'
  integer           :: ncid, status
  character*10      :: name  
!-----------------------------------------------------------------------
!     define allowable i/o units for MOM
!-----------------------------------------------------------------------
!      
  write(stdout,*)'Processing files in ',filelist
  open(iolst,file=filelist,form='formatted')
  read(iolst,'(a)',end=1111,err=5555,iostat=ierr) title
  read(iolst,'(a)',end=1111,err=5555,iostat=ierr) history
  do ncnv=1,maxcnv   
    read(iolst,'(a)',end=1111,err=5555,iostat=ierr) inputfile
!
! ...   read ASCII .cnv-files
!
!-----------------------------------------------------------------------
    inputtxt = trim(inputfile)//'.txt'
    inputdat = trim(inputfile)//'.dat'
    outputfile = trim(inputfile)//'.nc'       
    write(stdout,*) 'reading ... '//trim(inputtxt)
    write(stdout,*) 'writing ... '//trim(outputfile)
! -----------------------------------------------------------------------

    open(iunit,file=inputtxt,form='formatted')
    status = nf_create(outputfile, nf_clobber, ncid)
    write(stdout,*) 'file ',outputfile,' has been created with err ',status
    call process_header(iunit, ncid, title, history)
    close(iunit)
   
    open(iunit,file=inputdat,form='formatted')
    call get_data(iunit)
    close(iunit)
    status = nf_close(ncid)
10  continue
    close(iunit)

    write(stdout,*) 'file ',outputfile,' has been closed with err ',status
  enddo
1111 continue
5555 continue
  stop
  end

    

