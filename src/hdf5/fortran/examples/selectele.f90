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
!            This program creates two files, copy1.h5, and copy2.h5.
!            In copy1.h5, it creates a 3x4 dataset called 'Copy1',
!            and write 0's to this dataset.
!            In copy2.h5, it create a 3x4 dataset called 'Copy2',
!            and write 1's to this dataset.
!            It closes both files, reopens both files, selects two
!            points in copy1.h5 and writes values to them.  Then it
!            uses an H5Scopy to write the same selection to copy2.h5.
!            Program reopens the files, and reads and prints the contents of
!            the two datasets.
!

     PROGRAM SELECTEXAMPLE

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     CHARACTER(LEN=8), PARAMETER :: filename1 = "copy1.h5" ! File name
     CHARACTER(LEN=8), PARAMETER :: filename2 = "copy2.h5" !
     CHARACTER(LEN=5), PARAMETER :: dsetname1 = "Copy1"    ! Dataset name
     CHARACTER(LEN=5), PARAMETER :: dsetname2 = "Copy2"    !

     INTEGER, PARAMETER :: RANK = 2 ! Dataset rank

     INTEGER(SIZE_T), PARAMETER :: NUMP = 2 ! Number of points selected

     INTEGER(HID_T) :: file1_id       ! File1 identifier
     INTEGER(HID_T) :: file2_id       ! File2 identifier
     INTEGER(HID_T) :: dset1_id       ! Dataset1 identifier
     INTEGER(HID_T) :: dset2_id       ! Dataset2 identifier
     INTEGER(HID_T) :: dataspace1     ! Dataspace identifier
     INTEGER(HID_T) :: dataspace2     ! Dataspace identifier
     INTEGER(HID_T) :: memspace       ! memspace identifier

     INTEGER(HSIZE_T), DIMENSION(1) :: dimsm = (/2/)
                                                   ! Memory dataspace dimensions
     INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/3,4/)
                                                   ! File dataspace dimensions
     INTEGER(HSIZE_T), DIMENSION(RANK,NUMP) :: coord ! Elements coordinates
                                                      ! in the file

     INTEGER, DIMENSION(3,4) :: buf1, buf2, bufnew ! Data buffers
     INTEGER, DIMENSION(2) :: val = (/53, 59/) ! Values to write

     INTEGER :: memrank = 1  ! Rank of the dataset in memory

     INTEGER :: i, j

     INTEGER :: error  ! Error flag
     LOGICAL :: status
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims


   !
   ! Create two files containing identical datasets. Write 0's to one
   ! and 1's to the other.
   !

     !
     ! Data initialization.
     !
     do i = 1, 3
          do j = 1, 4
               buf1(i,j) = 0;
          end do
     end do

     do i = 1, 3
          do j = 1, 4
               buf2(i,j) = 1;
          end do
     end do

     !
     ! Initialize FORTRAN interface.
     !
     CALL h5open_f(error)

     !
     ! Create file1, file2  using default properties.
     !
     CALL h5fcreate_f(filename1, H5F_ACC_TRUNC_F, file1_id, error)

     CALL h5fcreate_f(filename2, H5F_ACC_TRUNC_F, file2_id, error)

     !
     ! Create the data space for the  datasets.
     !
     CALL h5screate_simple_f(RANK, dimsf, dataspace1, error)

     CALL h5screate_simple_f(RANK, dimsf, dataspace2, error)

     !
     ! Create the datasets with default properties.
     !
     CALL h5dcreate_f(file1_id, dsetname1, H5T_NATIVE_INTEGER, dataspace1, &
                      dset1_id, error)

     CALL h5dcreate_f(file2_id, dsetname2, H5T_NATIVE_INTEGER, dataspace2, &
                      dset2_id, error)

     !
     ! Write the datasets.
     !
     data_dims(1) = 3
     data_dims(2) = 4
     CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, buf1, data_dims, error)

     CALL h5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, buf2, data_dims, error)

     !
     ! Close the dataspace for the datasets.
     !
     CALL h5sclose_f(dataspace1, error)

     CALL h5sclose_f(dataspace2, error)

     !
     ! Close the datasets.
     !
     CALL h5dclose_f(dset1_id, error)

     CALL h5dclose_f(dset2_id, error)

     !
     ! Close the files.
     !
     CALL h5fclose_f(file1_id, error)

     CALL h5fclose_f(file2_id, error)

  !
  ! Open the two files.  Select two points in one file, write values to
  ! those point locations, then do H5Scopy and write the values to the
  ! other file.  Close files.
  !

     !
     ! Open the files.
     !
     CALL h5fopen_f (filename1, H5F_ACC_RDWR_F, file1_id, error)

     CALL h5fopen_f (filename2, H5F_ACC_RDWR_F, file2_id, error)

     !
     ! Open the  datasets.
     !
     CALL h5dopen_f(file1_id, dsetname1, dset1_id, error)

     CALL h5dopen_f(file2_id, dsetname2, dset2_id, error)

     !
     ! Get dataset1's dataspace identifier.
     !
     CALL h5dget_space_f(dset1_id, dataspace1, error)

     !
     ! Create memory dataspace.
     !
     CALL h5screate_simple_f(memrank, dimsm, memspace, error)

     !
     ! Set the selected point positions. Because Fortran array index starts
     ! from 1, so add one to the actual select points in C.
     !
     coord(1,1) = 1
     coord(2,1) = 2
     coord(1,2) = 1
     coord(2,2) = 4

     !
     ! Select the elements in file space.
     !
     CALL h5sselect_elements_f(dataspace1, H5S_SELECT_SET_F, RANK, NUMP,&
                               coord, error)

     !
     ! Write value into the selected points in dataset1.
     !
     data_dims(1) = 2
     CALL H5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, val, data_dims, error, &
                    mem_space_id=memspace, file_space_id=dataspace1)

     !
     ! Copy the daspace1 into dataspace2.
     !
     CALL h5scopy_f(dataspace1, dataspace2, error)

     !
     ! Write value into the selected points in dataset2.
     !
     data_dims(1) = 2
     CALL H5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, val, data_dims, error, &
                    mem_space_id=memspace, file_space_id=dataspace2)

     !
     ! Close the dataspace for the datasets.
     !
     CALL h5sclose_f(dataspace1, error)

     CALL h5sclose_f(dataspace2, error)

     !
     ! Close the memoryspace.
     !
     CALL h5sclose_f(memspace, error)

     !
     ! Close the datasets.
     !
     CALL h5dclose_f(dset1_id, error)

     CALL h5dclose_f(dset2_id, error)

     !
     ! Close the files.
     !
     CALL h5fclose_f(file1_id, error)

     CALL h5fclose_f(file2_id, error)

  !
  ! Open both files and print the contents of the datasets.
  !

     !
     ! Open the files.
     !
     CALL h5fopen_f (filename1, H5F_ACC_RDWR_F, file1_id, error)

     CALL h5fopen_f (filename2, H5F_ACC_RDWR_F, file2_id, error)

     !
     ! Open the  datasets.
     !
     CALL h5dopen_f(file1_id, dsetname1, dset1_id, error)

     CALL h5dopen_f(file2_id, dsetname2, dset2_id, error)

     !
     ! Read dataset from the first file.
     !
     data_dims(1) = 3
     data_dims(2) = 4
     CALL h5dread_f(dset1_id, H5T_NATIVE_INTEGER, bufnew, data_dims, error)

     !
     ! Display the data read from dataset "Copy1"
     !
     write(*,*) "The data in dataset Copy1 is: "
     do i = 1, 3
         print *, (bufnew(i,j), j = 1,4)
     end do

     !
     ! Read dataset from the second file.
     !
     CALL h5dread_f(dset2_id, H5T_NATIVE_INTEGER, bufnew, data_dims, error)

     !
     ! Display the data read from dataset "Copy2"
     !
     write(*,*) "The data in dataset Copy2 is: "
     do i = 1, 3
         print *, (bufnew(i,j), j = 1,4)
     end do

     !
     ! Close datasets.
     !
     CALL h5dclose_f(dset1_id, error)

     CALL h5dclose_f(dset2_id, error)

     !
     ! Close files.
     !
     CALL h5fclose_f(file1_id, error)

     CALL h5fclose_f(file2_id, error)

     !
     ! Close FORTRAN interface.
     !
     CALL h5close_f(error)

     END PROGRAM SELECTEXAMPLE
