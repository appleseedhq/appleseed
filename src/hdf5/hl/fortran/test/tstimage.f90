! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!
! This file contains the FORTRAN90 tests for H5LT
!

program image_test

call make_image1()

end program image_test


!-------------------------------------------------------------------------
! make_image1
!-------------------------------------------------------------------------

subroutine make_image1()

use h5im ! module of H5IM
use hdf5 ! module of HDF5 library

implicit none

character(len=8), parameter :: filename = "f1img.h5" ! file name
character(len=4), parameter :: dsetname1 = "img1"    ! dataset name
character(len=4), parameter :: dsetname2 = "img2"    ! dataset name
character(len=15), parameter :: il ="INTERLACE_PIXEL"! dataset name
integer(hid_t) :: file_id                            ! file identifier
integer(hsize_t), parameter :: width  = 500          ! width of image
integer(hsize_t), parameter :: height = 200          ! height of image
integer, parameter :: pal_entries = 9                ! palette number of entries
integer, dimension(width*height) :: buf1             ! data buffer
integer, dimension(width*height) :: bufr1            ! data buffer
integer, dimension(width*height*3) :: buf2           ! data buffer
integer, dimension(width*height*3) :: bufr2          ! data buffer
integer(hsize_t) :: widthr                           ! width of image
integer(hsize_t) :: heightr                          ! height of image
integer(hsize_t) :: planesr                          ! color planes
integer(hsize_t) :: npalsr                           ! palettes
character(len=15) :: interlacer                      ! interlace
integer :: errcode                                   ! error flag
integer :: is_image                                  ! error flag
integer :: i, j, n                                   ! general purpose integers
!
! palette
! create a 9 entry palette
!
character(len=4), parameter :: pal_name = "pal1"     ! dataset name
integer(hsize_t), dimension(2) :: pal_dims = (/pal_entries,3/) ! palette dimensions
integer(hsize_t), dimension(2) :: pal_dims_out       ! palette dimensions
integer, dimension(pal_entries*3) :: pal_data_out    ! data buffer
integer(hsize_t) :: npals                            ! number of palettes
integer          :: pal_number                       ! palette number
integer          :: is_palette                       ! is palette
integer          :: space
integer, dimension(pal_entries*3) :: pal_data_in = (/&
 0,0,168,&      ! dark blue
 0,0,252,&      ! blue
 0,168,252,&    ! ocean blue
 84,252,252,&   ! light blue
 168,252,168,&  ! light green
 0,252,168,&    ! green
 252,252,84,&   ! yellow
 252,168,0,&    ! orange
 252,0,0/)      ! red


! create an 8bit image of 9 values divided evenly by the array
!
space = width*height / pal_entries;
n = 0; j = 0;
do i = 1, width*height
  buf1(i) = n
  if ( j > space ) then
   n = n + 1;
   j = 0;
  endif
  if (n>pal_entries-1) n=0;
  j = j +1;
end do

!
! create a 3 byte rgb image
!
n = 0; j = 0;
do i = 1, width*height*3
  buf2(i) = n;
  if (j == 3) then
   n = n + 1;
   j = 0;
  endif
  if (n>255) n=0;
  j = j +1;
end do


! Initialize FORTRAN predefined datatypes.
!
call h5open_f(errcode)

!
! Create a new file using default properties.
!
call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)


!-------------------------------------------------------------------------
! indexed image
!-------------------------------------------------------------------------

call test_begin(' Make/Read image 8bit           ')

!
! write image.
!
call h5immake_image_8bit_f(file_id,dsetname1,width,height,buf1,errcode)
!
! read image.
!
call h5imread_image_f(file_id,dsetname1,bufr1,errcode)
!
! compare read and write buffers.
!
do i = 1, width*height
 if ( buf1(i) /= bufr1(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr1(i), ' and ',   buf1(i)
   stop
  endif
end do

!
! get image info.
!
call h5imget_image_info_f(file_id,dsetname1,widthr,heightr,planesr,interlacer,npalsr,errcode)

if ( (widthr /= widthr) .or. (heightr /= height) .or. (planesr /= 1)) then
 print *, 'h5imget_image_info_f bad value'
 stop
endif

is_image = h5imis_image_f(file_id,dsetname1)
if ( is_image /= 1) then
 print *, 'h5imis_image_f bad value'
 stop
endif


call passed()

!-------------------------------------------------------------------------
! true color image
!-------------------------------------------------------------------------

call test_begin(' Make/Read image 24bit          ')

!
! write image.
!
call h5immake_image_24bit_f(file_id,dsetname2,width,height,il,buf2,errcode)

!
! read image.
!
call h5imread_image_f(file_id,dsetname2,bufr2,errcode)

!
! compare read and write buffers.
!
do i = 1, width*height*3
 if ( buf2(i) /= bufr2(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr2(i), ' and ',   buf2(i)
   stop
  endif
end do

!
! get image info.
!
call h5imget_image_info_f(file_id,dsetname2,widthr,heightr,planesr,interlacer,npalsr,errcode)

if ( (widthr /= widthr) .or. (heightr /= height) .or. (planesr /= 3)) then
 print *, 'h5imget_image_info_f bad value'
 stop
endif

is_image = h5imis_image_f(file_id,dsetname2)
if ( is_image /= 1) then
 print *, 'h5imis_image_f bad value'
 stop
endif



call passed()

!-------------------------------------------------------------------------
! palette
!-------------------------------------------------------------------------

call test_begin(' Make palette                   ')

!
! make palette.
!
call h5immake_palette_f(file_id,pal_name,pal_dims,pal_data_in,errcode)

call passed()


call test_begin(' Link/Unlink palette            ')

!
! link palette.
!
call h5imlink_palette_f(file_id,dsetname1,pal_name,errcode)


!
! read palette.
!
pal_number = 0
call h5imget_palette_f(file_id,dsetname1,pal_number,pal_data_out,errcode)

!
! compare read and write buffers.
!
do i = 1, pal_entries*3
 if ( pal_data_in(i) /= pal_data_out(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  pal_data_in(i), ' and ',   pal_data_out(i)
   stop
  endif
end do

!
! get number of palettes
!
call h5imget_npalettes_f(file_id,dsetname1,npals,errcode)

if ( npals /= 1) then
 print *, 'h5imget_npalettes_f bad value'
 stop
endif

!
! get palette info
!
pal_number = 0
call h5imget_palette_info_f(file_id,dsetname1,pal_number,pal_dims_out,errcode)

if ( (pal_dims_out(1) /= pal_dims(1)) .or. (pal_dims_out(2) /= pal_dims(2))) then
 print *, 'h5imget_palette_info_f bad value'
 stop
endif

!
! is palette
!
is_palette = h5imis_palette_f(file_id,pal_name)

if ( is_palette /= 1 ) then
 print *, 'h5imis_palette_f bad value'
 stop
endif

!
! unlink palette.
!
call h5imunlink_palette_f(file_id,dsetname1,pal_name,errcode)

!
! get number of palettes
!
call h5imget_npalettes_f(file_id,dsetname1,npals,errcode )

if ( npals /= 0) then
 print *, 'h5imget_npalettes_f bad value'
 stop
endif


!
! link palette again
!
call h5imlink_palette_f(file_id,dsetname1,pal_name,errcode)

call passed()


!-------------------------------------------------------------------------
! end
!-------------------------------------------------------------------------

!
! Close the file.
!
call h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
call h5close_f(errcode)

!
! end function.
!
end subroutine make_image1

!-------------------------------------------------------------------------
! test_begin
!-------------------------------------------------------------------------

subroutine test_begin(string)
character(len=*), intent(in) :: string
write(*, fmt = '(14a)', advance = 'no') string
write(*, fmt = '(40x,a)', advance = 'no') ' '
end subroutine test_begin

!-------------------------------------------------------------------------
! passed
!-------------------------------------------------------------------------

subroutine passed()
write(*, fmt = '(6a)')  'PASSED'
end subroutine passed
