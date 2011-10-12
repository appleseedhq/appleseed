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
! This file contains a FORTRAN90 example for the H5LT API
!
!
program lite_example


use H5LT ! module of H5LT
use HDF5 ! module of HDF5 library

implicit none

integer, parameter :: DIM1 = 4;                      ! Dimension of array
character(len=9), parameter :: filename = "exlite.h5"! File name
character(LEN=5), parameter :: dsetname1 = "dset1"   ! Dataset name
integer(HID_T) :: file_id                            ! File identifier
integer(HSIZE_T), dimension(1) :: dims = (/DIM1/)    ! Dataset dimensions
integer        :: rank = 1                           ! Dataset rank
integer, dimension(DIM1) :: buf1                     ! Data buffer
integer, dimension(DIM1) :: bufr1                    ! Data buffer
integer        :: errcode                            ! Error flag
integer        :: i                                  ! general purpose integer


!
! Initialize the data array.
!

do i = 1, DIM1
  buf1(i) = i;
end do

!
! Initialize FORTRAN predefined datatypes.
!

call h5open_f(errcode)

!
! Create a new file using default properties.
!

call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!
! write dataset.
!

call h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf1, errcode)

!
! read dataset.
!

call h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr1, dims, errcode)

!
! compare read and write buffers.
!

do i = 1, DIM1
 if ( buf1(i) .ne. bufr1(i) ) then
   print *, 'read buffer differs from write buffer'
   print *,  bufr1(i), ' and ',   buf1(i)
   stop
  endif
end do


!
! Close the file.
!

call h5fclose_f(file_id, errcode)


!
! Close FORTRAN predefined datatypes.
!

call h5close_f(errcode)

!
! end
!

end program lite_example



