program postproc_qsto
! compile using ifort
implicit none

integer :: i, irec, ii, jj, ll
character(len=2) :: cc
integer, parameter :: iip1 = 97, jjp1 =73, llm = 19
real(kind=8), dimension( iip1,jjp1,llm )     :: q
!real(kind=8), dimension( iip1,jjp1,llm, 12 ) :: qmoy

!qmoy = 0.
!do i = 1, 12

!  if (i < 10) then
!    write(cc,'(i1.1)') i
!    open(1,file='trajq2003-'//cc(1:1)//'.bin',form='unformatted',access='direct',recl=2*iip1*jjp1*llm)
!  else
!    write(cc,'(i2.2)') i
!    open(1,file='trajq2003-'//cc//'.bin',form='unformatted',access='direct',recl=2*iip1*jjp1*llm)
!  endif

open(1,file='../trajq1990-1.bin_saved',form='unformatted',access='direct',recl=2*iip1*jjp1*llm)

  irec = 1
  do 
    read(1,rec=irec,err=10) q
    irec = irec + 1
    WRITE(*,*)irec
    WRITE(*,*)q(:,:,1)
    stop
!    qmoy(:,:,:,i) = qm*shoy(:,:,:,i) + q
  enddo
10 close(1)

  irec = irec - 1
!  print*, irec
!  qmoy(:,:,:,i) = qmoy(:,:,:,i) / irec

!enddo


!open(2,file='dataq.bin',form='unformatted')
!  write(2) qmoy
!close(2)

end
