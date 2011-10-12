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
!    Testing Selection-related Dataspace Interface functionality.
!

!
!    The following subroutines tests the following functionalities:
!    h5sget_select_npoints_f, h5sselect_elements_f, h5sselect_all_f,
!    h5sselect_none_f, h5sselect_valid_f, h5sselect_hyperslab_f,
!    h5sget_select_bounds_f, h5sget_select_elem_pointlist_f,
!    h5sget_select_elem_npoints_f, h5sget_select_hyper_blocklist_f,
!    h5sget_select_hyper_nblocks_f, h5sget_select_npoints_f
!

  SUBROUTINE test_select_hyperslab(cleanup, total_error)

    USE HDF5 ! This module contains all necessary modules

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: cleanup
    INTEGER, INTENT(OUT) :: total_error

    CHARACTER(LEN=7), PARAMETER :: filename = "tselect"
    CHARACTER(LEN=80) :: fix_filename

    !
    !dataset name is "IntArray"
    !
    CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray"

    INTEGER(HID_T) :: file_id       ! File identifier
    INTEGER(HID_T) :: dset_id       ! Dataset identifier
    INTEGER(HID_T) :: dataspace     ! Dataspace identifier
    INTEGER(HID_T) :: memspace      ! memspace identifier

    !
    !Memory space dimensions
    !
    INTEGER(HSIZE_T), DIMENSION(3) :: dimsm = (/7,7,3/)


    !
    !Dataset dimensions
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/5,6/)

    !
    !Size of the hyperslab in the file
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: count = (/3,4/)

    !
    !hyperslab offset in the file
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: offset = (/1,2/)

    !
    !Size of the hyperslab in memory
    !
    INTEGER(HSIZE_T), DIMENSION(3) :: count_out = (/3,4,1/)

    !
    !hyperslab offset in memory
    !
    INTEGER(HSIZE_T), DIMENSION(3) :: offset_out = (/3,0,0/)

    !
    !data to write
    !
    INTEGER, DIMENSION(5,6) :: data

    !
    !output buffer
    !
    INTEGER, DIMENSION(7,7,3) :: data_out


    !
    !dataset space rank
    !
    INTEGER :: dsetrank = 2

    !
    !memspace rank
    !
    INTEGER :: memrank = 3




    !
    !general purpose integer
    !
    INTEGER :: i, j

    !
    !flag to check operation success
    !
    INTEGER :: error
    INTEGER(HSIZE_T), DIMENSION(3) :: data_dims


    !
    !This writes data to the HDF5 file.
    !

    !
    !data initialization
    !
    do i = 1, 5
       do j = 1, 6
          data(i,j) = (i-1) + (j-1);
       end do
    end do
    !
    ! 0,  1,  2,  3,  4,  5
    ! 1,  2,  3,  4,  5,  6
    ! 2,  3,  4,  5,  6,  7
    ! 3,  4,  5,  6,  7,  8
    ! 4,  5,  6,  7,  8,  9
    !

    !
    !Initialize FORTRAN predifined datatypes
    !
!    CALL h5init_types_f(error)
!    CALL check("h5init_types_f", error, total_error)

    !
    !Create a new file using default properties.
    !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
    CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL check("h5fcreate_f", error, total_error)

    !
    !Create the data space for the  dataset.
    !
    CALL h5screate_simple_f(dsetrank, dimsf, dataspace, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    ! Create the dataset with default properties
    !
    CALL h5dcreate_f(file_id, dsetname, H5T_STD_I32BE, dataspace, &
         dset_id, error)
    CALL check("h5dcreate_f", error, total_error)

    !
    ! Write the dataset
    !
    data_dims(1) = 5
    data_dims(2) = 6
    CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Close the dataspace for the dataset.
    !
    CALL h5sclose_f(dataspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the dataset.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)

    !
    !This reads the hyperslab from the sds.h5 file just
    !created, into a 2-dimensional plane of the 3-dimensional array.
    !

    !
    !initialize data_out array
    !
    !     do i = 1, 7
    !          do j = 1, 7
    !              do k = 1,3
    !                  data_out(i,j,k) = 0;
    !              end do
    !          end do
    !     end do

    !
    !Open the file.
    !
    CALL h5fopen_f (fix_filename, H5F_ACC_RDONLY_F, file_id, error)
    CALL check("h5fopen_f", error, total_error)

    !
    !Open the  dataset.
    !
    CALL h5dopen_f(file_id, dsetname, dset_id, error)
    CALL check("h5dopen_f", error, total_error)

    !
    !Get dataset's dataspace handle.
    !
    CALL h5dget_space_f(dset_id, dataspace, error)
    CALL check("h5dget_space_f", error, total_error)

    !
    !Select hyperslab in the dataset.
    !
    CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
         offset, count, error)
    CALL check("h5sselect_hyperslab_f", error, total_error)
    !
    !create memory dataspace.
    !
    CALL h5screate_simple_f(memrank, dimsm, memspace, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    !Select hyperslab in memory.
    !
    CALL h5sselect_hyperslab_f(memspace, H5S_SELECT_SET_F, &
         offset_out, count_out, error)
    CALL check("h5sselect_hyperslab_f", error, total_error)

    !
    !Read data from hyperslab in the file into the hyperslab in
    !memory and display.
    !
    data_dims(1) = 7
    data_dims(2) = 7
    data_dims(3) = 3
    CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
         memspace, dataspace)
    CALL check("h5dread_f", error, total_error)

    !
    !Display data_out array
    !
    !do i = 1, 7
    !   print *, (data_out(i,j,1), j = 1,7)
    !end do

    ! 0 0 0 0 0 0 0
    ! 0 0 0 0 0 0 0
    ! 0 0 0 0 0 0 0
    ! 3 4 5 6 0 0 0
    ! 4 5 6 7 0 0 0
    ! 5 6 7 8 0 0 0
    ! 0 0 0 0 0 0 0
    !

    !
    !Close the dataspace for the dataset.
    !
    CALL h5sclose_f(dataspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the memoryspace.
    !
    CALL h5sclose_f(memspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the dataset.
    !
    CALL h5dclose_f(dset_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the file.
    !
    CALL h5fclose_f(file_id, error)
    CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
    RETURN

  END SUBROUTINE test_select_hyperslab

  !
  !Subroutine to test element selection
  !

  SUBROUTINE test_select_element(cleanup, total_error)

    USE HDF5 ! This module contains all necessary modules

    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(OUT) :: total_error

    !
    !the dataset1 is stored in file "copy1.h5"
    !
    CHARACTER(LEN=13), PARAMETER :: filename1 = "tselect_copy1"
    CHARACTER(LEN=80) :: fix_filename1

    !
    !the dataset2 is stored in file "copy2.h5"
    !
    CHARACTER(LEN=13), PARAMETER :: filename2 = "tselect_copy2"
    CHARACTER(LEN=80) :: fix_filename2
    !
    !dataset1 name is "Copy1"
    !
    CHARACTER(LEN=8), PARAMETER :: dsetname1 = "Copy1"

    !
    !dataset2 name is "Copy2"
    !
    CHARACTER(LEN=8), PARAMETER :: dsetname2 = "Copy2"

    !
    !dataset rank
    !
    INTEGER, PARAMETER :: RANK = 2

    !
    !number of points selected
    !
    INTEGER(SIZE_T), PARAMETER :: NUMP = 2

    INTEGER(HID_T) :: file1_id       ! File1 identifier
    INTEGER(HID_T) :: file2_id       ! File2 identifier
    INTEGER(HID_T) :: dset1_id       ! Dataset1 identifier
    INTEGER(HID_T) :: dset2_id       ! Dataset2 identifier
    INTEGER(HID_T) :: dataspace1     ! Dataspace identifier
    INTEGER(HID_T) :: dataspace2     ! Dataspace identifier
    INTEGER(HID_T) :: memspace       ! memspace identifier

    !
    !Memory space dimensions
    !
    INTEGER(HSIZE_T), DIMENSION(1) :: dimsm = (/2/)

    !
    !Dataset dimensions
    !
    INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/3,4/)

    !
    !Points positions in the file
    !
    INTEGER(HSIZE_T), DIMENSION(RANK,NUMP) :: coord

    !
    !data buffers
    !
    INTEGER, DIMENSION(3,4) :: buf1, buf2, bufnew

    !
    !value to write
    !
    INTEGER, DIMENSION(2) :: val = (/53, 59/)

    !
    !memory rank
    !
    INTEGER :: memrank = 1

    !
    !general purpose integer
    !
    INTEGER :: i, j

    !
    !flag to check operation success
    !
    INTEGER :: error
    INTEGER(HSIZE_T), DIMENSION(3) :: data_dims


    !
    !Create two files containing identical datasets. Write 0's to one
    !and 1's to the other.
    !

    !
    !data initialization
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
    !Initialize FORTRAN predifined datatypes
    !
!    CALL h5init_types_f(error)
!    CALL check("h5init_types_f", error, total_error)

    !
    !Create file1, file2  using default properties.
    !
          CALL h5_fixname_f(filename1, fix_filename1, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
    CALL h5fcreate_f(fix_filename1, H5F_ACC_TRUNC_F, file1_id, error)
    CALL check("h5fcreate_f", error, total_error)

          CALL h5_fixname_f(filename2, fix_filename2, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
    CALL h5fcreate_f(fix_filename2, H5F_ACC_TRUNC_F, file2_id, error)
    CALL check("h5fcreate_f", error, total_error)

    !
    !Create the data space for the  datasets.
    !
    CALL h5screate_simple_f(RANK, dimsf, dataspace1, error)
    CALL check("h5screate_simple_f", error, total_error)

    CALL h5screate_simple_f(RANK, dimsf, dataspace2, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    ! Create the datasets with default properties
    !
    CALL h5dcreate_f(file1_id, dsetname1, H5T_NATIVE_INTEGER, dataspace1, &
         dset1_id, error)
    CALL check("h5dcreate_f", error, total_error)

    CALL h5dcreate_f(file2_id, dsetname2, H5T_NATIVE_INTEGER, dataspace2, &
         dset2_id, error)
    CALL check("h5dcreate_f", error, total_error)

    !
    ! Write the datasets
    !
    data_dims(1) = 3
    data_dims(2) = 4
    CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, buf1, data_dims, error)
    CALL check("h5dwrite_f", error, total_error)

    CALL h5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, buf2, data_dims, error)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Close the dataspace for the datasets.
    !
    CALL h5sclose_f(dataspace1, error)
    CALL check("h5sclose_f", error, total_error)

    CALL h5sclose_f(dataspace2, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the datasets.
    !
    CALL h5dclose_f(dset1_id, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5dclose_f(dset2_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the files.
    !
    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f", error, total_error)

    CALL h5fclose_f(file2_id, error)
    CALL check("h5fclose_f", error, total_error)

    !
    !Open the two files.  Select two points in one file, write values to
    !those point locations, then do H5Scopy and write the values to the
    !other file.  Close files.
    !

    !
    !Open the files.
    !
    CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
    CALL check("h5fopen_f", error, total_error)

    CALL h5fopen_f (fix_filename2, H5F_ACC_RDWR_F, file2_id, error)
    CALL check("h5fopen_f", error, total_error)

    !
    !Open the  datasets.
    !
    CALL h5dopen_f(file1_id, dsetname1, dset1_id, error)
    CALL check("h5dopen_f", error, total_error)

    CALL h5dopen_f(file2_id, dsetname2, dset2_id, error)
    CALL check("h5dopen_f", error, total_error)

    !
    !Get dataset1's dataspace handle.
    !
    CALL h5dget_space_f(dset1_id, dataspace1, error)
    CALL check("h5dget_space_f", error, total_error)

    !
    !create memory dataspace.
    !
    CALL h5screate_simple_f(memrank, dimsm, memspace, error)
    CALL check("h5screate_simple_f", error, total_error)

    !
    !Set the selected point positions.Because Fortran array index starts
    ! from 1, so add one to the actual select points in C
    !
    coord(1,1) = 1
    coord(2,1) = 2
    coord(1,2) = 1
    coord(2,2) = 4

    !
    !Select the elements in file space
    !
    CALL h5sselect_elements_f(dataspace1, H5S_SELECT_SET_F, RANK, NUMP,&
         coord, error)
    CALL check("h5sselect_elements_f", error, total_error)

    !
    !Write value into the selected points in dataset1
    !
    data_dims(1) = 2
    CALL H5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, val, data_dims, error, &
         mem_space_id=memspace, file_space_id=dataspace1)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Copy the daspace1 into dataspace2
    !
    CALL h5scopy_f(dataspace1, dataspace2, error)
    CALL check("h5scopy_f", error, total_error)

    !
    !Write value into the selected points in dataset2
    !
    CALL H5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, val, data_dims, error, &
         mem_space_id=memspace, file_space_id=dataspace2)
    CALL check("h5dwrite_f", error, total_error)

    !
    !Close the dataspace for the datasets.
    !
    CALL h5sclose_f(dataspace1, error)
    CALL check("h5sclose_f", error, total_error)

    CALL h5sclose_f(dataspace2, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the memoryspace.
    !
    CALL h5sclose_f(memspace, error)
    CALL check("h5sclose_f", error, total_error)

    !
    !Close the datasets.
    !
    CALL h5dclose_f(dset1_id, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5dclose_f(dset2_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the files.
    !
    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f", error, total_error)

    CALL h5fclose_f(file2_id, error)
    CALL check("h5fclose_f", error, total_error)

    !
    !Open both files and print the contents of the datasets.
    !

    !
    !Open the files.
    !
    CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
    CALL check("h5fopen_f", error, total_error)

    CALL h5fopen_f (fix_filename2, H5F_ACC_RDWR_F, file2_id, error)
    CALL check("h5fopen_f", error, total_error)

    !
    !Open the  datasets.
    !
    CALL h5dopen_f(file1_id, dsetname1, dset1_id, error)
    CALL check("h5dopen_f", error, total_error)

    CALL h5dopen_f(file2_id, dsetname2, dset2_id, error)
    CALL check("h5dopen_f", error, total_error)

    !
    !Read dataset1.
    !
    data_dims(1) = 3
    data_dims(2) = 4
    CALL h5dread_f(dset1_id, H5T_NATIVE_INTEGER, bufnew, data_dims, error)
    CALL check("h5dread_f", error, total_error)

    !
    !Display the data read from dataset "Copy1"
    !
    !write(*,*) "The data in dataset Copy1 is: "
    !do i = 1, 3
    !   print *, (bufnew(i,j), j = 1,4)
    !end do

    !
    !Read dataset2.
    !
    CALL h5dread_f(dset2_id, H5T_NATIVE_INTEGER, bufnew, data_dims, error)
    CALL check("h5dread_f", error, total_error)

    !
    !Display the data read from dataset "Copy2"
    !
    !write(*,*) "The data in dataset Copy2 is: "
    !do i = 1, 3
    !   print *, (bufnew(i,j), j = 1,4)
    !end do

    !
    !Close the datasets.
    !
    CALL h5dclose_f(dset1_id, error)
    CALL check("h5dclose_f", error, total_error)

    CALL h5dclose_f(dset2_id, error)
    CALL check("h5dclose_f", error, total_error)

    !
    !Close the files.
    !
    CALL h5fclose_f(file1_id, error)
    CALL check("h5fclose_f", error, total_error)

    CALL h5fclose_f(file2_id, error)
    CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename1, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          if(cleanup) CALL h5_cleanup_f(filename2, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
     RETURN
  END SUBROUTINE  test_select_element


  SUBROUTINE test_basic_select(cleanup, total_error)
    USE HDF5 ! This module contains all necessary modules

    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(OUT) :: total_error

     !
     !the dataset is stored in file "testselect.h5"
     !
     CHARACTER(LEN=10), PARAMETER :: filename = "testselect"
     CHARACTER(LEN=80) :: fix_filename

     !
     !dataspace rank
     !
     INTEGER, PARAMETER :: RANK = 2

     !
     !select NUMP_POINTS points from the file
     !
     INTEGER(SIZE_T), PARAMETER :: NUMPS = 10

     !
     !dataset name is "testselect"
     !
     CHARACTER(LEN=10), PARAMETER :: dsetname = "testselect"

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier

     !
     !Dataset dimensions
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dimsf = (/5,6/)

     !
     !Size of the hyperslab in the file
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: count = (/2,2/)

     !
     !hyperslab offset in the file
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: offset = (/0,0/)

     !
     !start block for getting the selected hyperslab
     !
     INTEGER(HSIZE_T) :: startblock = 0

     !
     !start point for getting the selected elements
     !
     INTEGER(HSIZE_T)  :: startpoint = 0

     !
     !Stride of the hyperslab in the file
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: stride = (/3,3/)

     !
     !BLock size of the hyperslab in the file
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: block = (/2,2/)

     !
     !array to give selected points' coordinations
     !
     INTEGER(HSIZE_T), DIMENSION(RANK, NUMPS) :: coord


     !
     !Number of hyperslabs selected in the current dataspace
     !
     INTEGER(HSSIZE_T) :: num_blocks

     !
     !allocatable array for putting a list of hyperslabs
     !selected in the current file dataspace
     !
     INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: blocklist

     !
     !Number of points selected in the current dataspace
     !
     INTEGER(HSSIZE_T) :: num_points
     INTEGER(HSIZE_T) :: num1_points

     !
     !allocatable array for putting a list of points
     !selected in the current file dataspace
     !
     INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: pointlist

     !
     !start and end bounds in the current dataspace selection
     !
     INTEGER(HSIZE_T), DIMENSION(RANK) :: startout, endout

     !
     !data to write
     !
     INTEGER, DIMENSION(5,6) :: data

     !
     !flag to check operation success
     !
     INTEGER :: error
     INTEGER(HSIZE_T), DIMENSION(3) :: data_dims

     INTEGER :: i

     !
     !initialize the coord array to give the selected points' position
     !
     coord(1,1) = 1
     coord(2,1) = 1
     coord(1,2) = 1
     coord(2,2) = 3
     coord(1,3) = 1
     coord(2,3) = 5
     coord(1,4) = 3
     coord(2,4) = 1
     coord(1,5) = 3
     coord(2,5) = 3
     coord(1,6) = 3
     coord(2,6) = 5
     coord(1,7) = 4
     coord(2,7) = 3
     coord(1,8) = 4
     coord(2,8) = 1
     coord(1,9) = 5
     coord(2,9) = 3
     coord(1,10) = 5
     coord(2,10) = 5

     !
     !Create a new file using default properties.
     !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f", error, total_error)

     !
     !Create the data space for the  dataset.
     !
     CALL h5screate_simple_f(RANK, dimsf, dataspace, error)
     CALL check("h5screate_simple_f", error, total_error)

     !
     ! Create the dataset with default properties
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_STD_I32BE, dataspace, &
                      dset_id, error)
     CALL check("h5dcreate_f", error, total_error)

     !
     ! Write the dataset
     !
     data_dims(1) = 5
     data_dims(2) = 6
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error)
     CALL check("h5dwrite_f", error, total_error)

     !
     !Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)
     CALL check("h5sclose_f", error, total_error)

     !
     !Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f", error, total_error)

     !
     !Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)

     !
     !Open the file.
     !
     CALL h5fopen_f (fix_filename, H5F_ACC_RDONLY_F, file_id, error)
     CALL check("h5fopen_f", error, total_error)

     !
     !Open the  dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)
     CALL check("h5dopen_f", error, total_error)

     !
     !Get dataset's dataspace handle.
     !
     CALL h5dget_space_f(dset_id, dataspace, error)
     CALL check("h5dget_space_f", error, total_error)

     !
     !Select hyperslab in the dataset.
     !
     CALL h5sselect_hyperslab_f(dataspace, H5S_SELECT_SET_F, &
                                offset, count, error, stride, block)
     CALL check("h5sselect_hyperslab_f", error, total_error)

     !
     !get the number of hyperslab blocks in the current dataspac selection
     !
     CALL h5sget_select_hyper_nblocks_f(dataspace, num_blocks, error)
     CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
     IF (num_blocks .NE. 4) write (*,*) "error occured with num_blocks"
     !write(*,*) num_blocks
     !result of num_blocks is 4

     !
     !allocate the blocklist array
     !
     ALLOCATE(blocklist(num_blocks*RANK*2), STAT= error)
     if(error .NE. 0) then
         STOP
     endif

     !
     !get the list of hyperslabs selected in the current dataspac selection
     !
     CALL h5sget_select_hyper_blocklist_f(dataspace, startblock, &
                                          num_blocks, blocklist, error)
     CALL check("h5sget_select_hyper_blocklist_f", error, total_error)
!     write(*,*) (blocklist(i), i =1, num_blocks*RANK*2)
     !result of blocklist selected is:
     !1,  1,  2,  2,  4,  1,  5,  2,  1,  4,  2,  5,  4,  4,  5,  5

     !
     !deallocate the blocklist array
     !
     DEALLOCATE(blocklist)

     !
     !get the selection bounds in the current dataspac selection
     !
     CALL h5sget_select_bounds_f(dataspace, startout, endout, error)
     CALL check("h5sget_select_bounds_f", error, total_error)
     IF ( (startout(1) .ne. 1) .or. (startout(2) .ne. 1) ) THEN
        write(*,*) "error occured to select_bounds's start position"
     END IF

     IF ( (endout(1) .ne. 5) .or. (endout(2) .ne. 5) ) THEN
        write(*,*) "error occured to select_bounds's end position"
     END IF
     !write(*,*) (startout(i), i = 1, RANK)
     !result of startout is 0, 0

     !write(*,*) (endout(i), i = 1, RANK)
     !result of endout is 5, 5

     !
     !allocate the pointlist array
     !
!     ALLOCATE(pointlist(num_blocks*RANK), STAT= error)
     ALLOCATE(pointlist(20), STAT= error)
     if(error .NE. 0) then
         STOP
     endif

     !
     !Select the elements in file space
     !
     CALL h5sselect_elements_f(dataspace, H5S_SELECT_SET_F, RANK, NUMPS,&
                               coord, error)
     CALL check("h5sselect_elements_f", error, total_error)

     !
     !Get the number of selected elements
     !
     CALL h5sget_select_elem_npoints_f(dataspace, num_points, error)
     CALL check("h5sget_select_elem_npoints_f", error, total_error)
     IF (num_points .NE. 10) write(*,*) "error occured with num_points"
     !write(*,*) num_points
     ! result of num_points is 10

     !
     !Get the list of selected elements
     !
     num1_points = num_points
     CALL h5sget_select_elem_pointlist_f(dataspace, startpoint, &
                                          num1_points, pointlist, error)
     CALL check("h5sget_select_elem_pointlist_f", error, total_error)
     !write(*,*) (pointlist(i), i =1, num1_points*RANK)
     !result of pintlist is:
     !1,  1,  3,  1,  5,  1,  1,  3,  3,  3,  5,  3,  3,
     !4,  1,  4,  3,  5,  5,  5

     !
     !deallocate the pointlist array
     !
     DEALLOCATE(pointlist)

     !
     !Close the dataspace for the dataset.
     !
     CALL h5sclose_f(dataspace, error)
     CALL check("h5sclose_f", error, total_error)

     !
     !Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f", error, total_error)

     !
     !Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)

     RETURN
  END SUBROUTINE  test_basic_select

!/****************************************************************
!**
!**  test_select_point(): Test basic H5S (dataspace) selection code.
!**      Tests element selections between dataspaces of various sizes
!**      and dimensionalities.
!**
!****************************************************************/

SUBROUTINE test_select_point(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error
  INTEGER(HID_T) :: xfer_plist

  INTEGER, PARAMETER :: SPACE1_DIM1=3
  INTEGER, PARAMETER :: SPACE1_DIM2=15
  INTEGER, PARAMETER :: SPACE1_DIM3=13
  INTEGER, PARAMETER :: SPACE2_DIM1=30
  INTEGER, PARAMETER :: SPACE2_DIM2=26
  INTEGER, PARAMETER :: SPACE3_DIM1=15
  INTEGER, PARAMETER :: SPACE3_DIM2=26

  INTEGER, PARAMETER :: SPACE1_RANK=3
  INTEGER, PARAMETER :: SPACE2_RANK=2
  INTEGER, PARAMETER :: SPACE3_RANK=2

  ! /* Element selection information */
  INTEGER, PARAMETER :: POINT1_NPOINTS=10
  INTEGER(hid_t) ::fid1 ! /* HDF5 File IDs */
  INTEGER(hid_t) ::dataset !	/* Dataset ID */
  INTEGER(hid_t) ::sid1,sid2 ! /* Dataspace ID */
  INTEGER(hsize_t), DIMENSION(1:3) :: dims1 = (/SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3/)
  INTEGER(hsize_t), DIMENSION(1:2) :: dims2 = (/SPACE2_DIM1, SPACE2_DIM2/)
  INTEGER(hsize_t), DIMENSION(1:2) :: dims3 = (/SPACE3_DIM1, SPACE3_DIM2/)

  INTEGER(hsize_t), DIMENSION(1:SPACE1_RANK,1:POINT1_NPOINTS) :: coord1 !/* Coordinates for point selection */
  INTEGER(hsize_t), DIMENSION(1:SPACE1_RANK,1:POINT1_NPOINTS) :: temp_coord1 !/* Coordinates for point selection */
  INTEGER(hsize_t), DIMENSION(1:SPACE2_RANK,1:POINT1_NPOINTS) :: coord2 !/* Coordinates for point selection */
  INTEGER(hsize_t), DIMENSION(1:SPACE2_RANK,1:POINT1_NPOINTS) :: temp_coord2 !/* Coordinates for point selection */
  INTEGER(hsize_t), DIMENSION(1:SPACE3_RANK,1:POINT1_NPOINTS) :: coord3 !/* Coordinates for point selection */
  INTEGER(hsize_t), DIMENSION(1:SPACE3_RANK,1:POINT1_NPOINTS) :: temp_coord3 !/* Coordinates for point selection */
  INTEGER(hssize_t) :: npoints

!!$    uint8_t    *wbuf,       /* buffer to write to disk */
!!$               *rbuf,       /* buffer read from disk */
!!$               *tbuf;       /* temporary buffer pointer */
  INTEGER :: i,j;        !/* Counters */
!    struct pnt_iter pi;     /* Custom Pointer iterator struct */
  INTEGER :: error		!/* Generic return value		*/
  CHARACTER(LEN=9) :: filename = 'h5s_hyper'
  CHARACTER(LEN=80) :: fix_filename
  CHARACTER(LEN=1), DIMENSION(1:SPACE2_DIM1,1:SPACE2_DIM2) :: wbuf, rbuf

  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF
  xfer_plist = H5P_DEFAULT_F
!    MESSAGE(5, ("Testing Element Selection Functions\n"));

    !/* Allocate write & read buffers */
!!$  wbuf = HDmalloc(sizeof(uint8_t) * SPACE2_DIM1 * SPACE2_DIM2);
!!$  rbuf = HDcalloc(sizeof(uint8_t), (size_t)(SPACE3_DIM1 * SPACE3_DIM2));
!!$
  !/* Initialize WRITE buffer */

  DO i = 1, SPACE2_DIM1
     DO j = 1, SPACE2_DIM2
        wbuf(i,j) = 'a'
     ENDDO
  ENDDO

!!$  for(i=0, tbuf=wbuf; i<SPACE2_DIM1; i++)
!!$  for(j=0; j<SPACE2_DIM2; j++)
!!$  *tbuf++=(uint8_t)((i*SPACE2_DIM2)+j);

  !/* Create file */
  CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, fid1, error)
  CALL check("h5fcreate_f", error, total_error)

  !/* Create dataspace for dataset */
  CALL h5screate_simple_f(SPACE1_RANK, dims1, sid1, error)
  CALL check("h5screate_simple_f", error, total_error)

  !/* Create dataspace for write buffer */
  CALL h5screate_simple_f(SPACE2_RANK, dims2, sid2, error)
  CALL check("h5screate_simple_f", error, total_error)

  !/* Select sequence of ten points for disk dataset */
  coord1(1,1)=1; coord1(2,1)=11; coord1(3,1)= 6;
  coord1(1,2)=2; coord1(2,2)= 3; coord1(3,2)= 8;
  coord1(1,3)=3; coord1(2,3)= 5; coord1(3,3)=10;
  coord1(1,4)=1; coord1(2,4)= 7; coord1(3,4)=12;
  coord1(1,5)=2; coord1(2,5)= 9; coord1(3,5)=14;
  coord1(1,6)=3; coord1(2,6)=13; coord1(3,6)= 1;
  coord1(1,7)=1; coord1(2,7)=15; coord1(3,7)= 3;
  coord1(1,8)=2; coord1(2,8)= 1; coord1(3,8)= 5;
  coord1(1,9)=3; coord1(2,9)= 2; coord1(3,9)= 7;
  coord1(1,10)=1; coord1(2,10)= 4; coord1(3,10)= 9


  CALL h5sselect_elements_f(sid1, H5S_SELECT_SET_F, SPACE1_RANK, INT(POINT1_NPOINTS,size_t), coord1, error)
  CALL check("h5sselect_elements_f", error, total_error)

  !/* Verify correct elements selected */

  CALL h5sget_select_elem_pointlist_f(sid1, INT(0,hsize_t), INT(POINT1_NPOINTS,hsize_t),temp_coord1,error)
  CALL check("h5sget_select_elem_pointlist_f", error, total_error)

  DO i= 1, POINT1_NPOINTS
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord1(1,i)), INT(coord1(1,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord1(2,i)), INT(coord1(2,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord1(3,i)), INT(coord1(3,i)), total_error)
  ENDDO

  CALL H5Sget_select_npoints_f(sid1, npoints, error)
  CALL check("h5sget_select_npoints_f", error, total_error)
  CALL verify("h5sget_select_npoints_f", INT(npoints), 10, total_error)

  !/* Append another sequence of ten points to disk dataset */

  coord1(1,1)=1; coord1(2,1)=3; coord1(3,1)= 1;
  coord1(1,2)=2; coord1(2,2)=11; coord1(3,2)= 9;
  coord1(1,3)=3; coord1(2,3)= 9; coord1(3,3)=11;
  coord1(1,4)=1; coord1(2,4)= 8; coord1(3,4)=13;
  coord1(1,5)=2; coord1(2,5)= 4; coord1(3,5)=12;
  coord1(1,6)=3; coord1(2,6)= 2; coord1(3,6)= 2;
  coord1(1,7)=1; coord1(2,7)=14; coord1(3,7)= 8;
  coord1(1,8)=2; coord1(2,8)=15; coord1(3,8)= 7;
  coord1(1,9)=3; coord1(2,9)= 3; coord1(3,9)= 6;
  coord1(1,10)=1; coord1(2,10)= 7; coord1(3,10)= 14


  CALL h5sselect_elements_f(sid1, H5S_SELECT_APPEND_F, SPACE1_RANK, INT(POINT1_NPOINTS,size_t), coord1, error)
  CALL check("h5sselect_elements_f", error, total_error)
  ! /* Verify correct elements selected */

  CALL h5sget_select_elem_pointlist_f(sid1, INT(POINT1_NPOINTS,hsize_t), INT(POINT1_NPOINTS,hsize_t),temp_coord1,error)
  CALL check("h5sget_select_elem_pointlist_f", error, total_error)

  DO i= 1, POINT1_NPOINTS
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord1(1,i)), INT(coord1(1,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord1(2,i)), INT(coord1(2,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord1(3,i)), INT(coord1(3,i)), total_error)
  ENDDO

  CALL H5Sget_select_npoints_f(sid1, npoints, error)
  CALL check("h5sget_select_npoints_f", error, total_error)
  CALL verify("h5sget_select_npoints_f", INT(npoints), 20, total_error)

  ! /* Select sequence of ten points for memory dataset */
  coord2(1,1)=13; coord2(2,1)= 4;
  coord2(1,2)=16; coord2(2,2)=14;
  coord2(1,3)= 8; coord2(2,3)=26;
  coord2(1,4)= 1; coord2(2,4)= 7;
  coord2(1,5)=14; coord2(2,5)= 1;
  coord2(1,6)=25; coord2(2,6)=12;
  coord2(1,7)=13; coord2(2,7)=22;
  coord2(1,8)=30; coord2(2,8)= 5;
  coord2(1,9)= 9; coord2(2,9)= 9;
  coord2(1,10)=20; coord2(2,10)=18

  CALL h5sselect_elements_f(sid2, H5S_SELECT_SET_F, SPACE2_RANK, INT(POINT1_NPOINTS,size_t), coord2, error)
  CALL check("h5sselect_elements_f", error, total_error)


  !/* Verify correct elements selected */

  CALL h5sget_select_elem_pointlist_f(sid2, INT(0,hsize_t), INT(POINT1_NPOINTS,hsize_t),temp_coord2,error)
  CALL check("h5sget_select_elem_pointlist_f", error, total_error)

  DO i= 1, POINT1_NPOINTS
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord2(1,i)), INT(coord2(1,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord2(2,i)), INT(coord2(2,i)), total_error)
  ENDDO

!!$
!!$    /* Save points for later iteration */
!!$    /* (these are in the second half of the buffer, because we are prepending */
!!$    /*  the next list of points to the beginning of the point selection list) */
!!$    HDmemcpy(((char *)pi.coord)+sizeof(coord2),coord2,sizeof(coord2));
!!$

  CALL H5Sget_select_npoints_f(sid2, npoints, error)
  CALL check("h5sget_select_npoints_f", error, total_error)
  CALL verify("h5sget_select_npoints_f", INT(npoints), 10, total_error)

  !/* Append another sequence of ten points to memory dataset */
  coord2(1,1)=25; coord2(2,1)= 1;
  coord2(1,2)= 3; coord2(2,2)=26;
  coord2(1,3)=14; coord2(2,3)=18;
  coord2(1,4)= 9; coord2(2,4)= 4;
  coord2(1,5)=30; coord2(2,5)= 5;
  coord2(1,6)=12; coord2(2,6)=15;
  coord2(1,7)= 6; coord2(2,7)=23;
  coord2(1,8)=13; coord2(2,8)= 3;
  coord2(1,9)=22; coord2(2,9)=13;
  coord2(1,10)= 10; coord2(2,10)=19

  CALL h5sselect_elements_f(sid2, H5S_SELECT_PREPEND_F, SPACE2_RANK, INT(POINT1_NPOINTS,size_t), coord2, error)
  CALL check("h5sselect_elements_f", error, total_error)


  !/* Verify correct elements selected */
  CALL h5sget_select_elem_pointlist_f(sid2, INT(0,hsize_t), INT(POINT1_NPOINTS,hsize_t),temp_coord2,error)
  CALL check("h5sget_select_elem_pointlist_f", error, total_error)

  DO i= 1, POINT1_NPOINTS
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord2(1,i)), INT(coord2(1,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord2(2,i)), INT(coord2(2,i)), total_error)
  ENDDO

  CALL H5Sget_select_npoints_f(sid2, npoints, error)
  CALL check("h5sget_select_npoints_f", error, total_error)
  CALL verify("h5sget_select_npoints_f", INT(npoints), 20, total_error)

!!$    /* Save points for later iteration */
!!$    HDmemcpy(pi.coord,coord2,sizeof(coord2));

  ! /* Create a dataset */
  CALL h5dcreate_f(fid1, "Dataset1", H5T_NATIVE_CHARACTER, sid1, dataset, error)
  CALL check("h5dcreate_f", error, total_error)

  ! /* Write selection to disk */
  CALL h5dwrite_f(dataset, H5T_NATIVE_CHARACTER, wbuf, dims2, error, sid2, sid1, xfer_plist)
  CALL check("h5dwrite_f", error, total_error)

  ! /* Close memory dataspace */
  CALL h5sclose_f(sid2, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Create dataspace for reading buffer */
  CALL h5screate_simple_f(SPACE3_RANK, dims3, sid2, error)
  CALL check("h5screate_simple_f", error, total_error)

  ! /* Select sequence of points for read dataset */
  coord3(1,1)= 1; coord3(2,1)= 3;
  coord3(1,2)= 5; coord3(2,2)= 9;
  coord3(1,3)=14; coord3(2,3)=14;
  coord3(1,4)=15; coord3(2,4)=21;
  coord3(1,5)= 8; coord3(2,5)=10;
  coord3(1,6)= 3; coord3(2,6)= 1;
  coord3(1,7)= 10; coord3(2,7)=20;
  coord3(1,8)= 2; coord3(2,8)=23;
  coord3(1,9)=13; coord3(2,9)=22;
  coord3(1,10)=12; coord3(2,10)=7;

  CALL h5sselect_elements_f(sid2, H5S_SELECT_SET_F, SPACE3_RANK, INT(POINT1_NPOINTS,size_t), coord3, error)
  CALL check("h5sselect_elements_f", error, total_error)

  ! /* Verify correct elements selected */
  CALL h5sget_select_elem_pointlist_f(sid2, INT(0,hsize_t), INT(POINT1_NPOINTS,hsize_t),temp_coord3,error)
  CALL check("h5sget_select_elem_pointlist_f", error, total_error)
  DO i= 1, POINT1_NPOINTS
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord3(1,i)), INT(coord3(1,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord3(2,i)), INT(coord3(2,i)), total_error)
  ENDDO

  CALL H5Sget_select_npoints_f(sid2, npoints, error)
  CALL check("h5sget_select_npoints_f", error, total_error)
  CALL verify("h5sget_select_npoints_f", INT(npoints), 10, total_error)

  !/* Append another sequence of ten points to disk dataset */
    coord3(1,1)=15; coord3(2,1)=26;
    coord3(1,2)= 1; coord3(2,2)= 1;
    coord3(1,3)=12; coord3(2,3)=12;
    coord3(1,4)= 6; coord3(2,4)=15;
    coord3(1,5)= 4; coord3(2,5)= 6;
    coord3(1,6)= 3; coord3(2,6)= 3;
    coord3(1,7)= 8; coord3(2,7)=14;
    coord3(1,8)=10; coord3(2,8)=17;
    coord3(1,9)=13; coord3(2,9)=23;
    coord3(1,10)=14; coord3(2,10)=10

    CALL h5sselect_elements_f(sid2, H5S_SELECT_APPEND_F, SPACE3_RANK, INT(POINT1_NPOINTS,size_t), coord3, error)
    CALL check("h5sselect_elements_f", error, total_error)

  ! /* Verify correct elements selected */
  CALL h5sget_select_elem_pointlist_f(sid2, INT(POINT1_NPOINTS,hsize_t), INT(POINT1_NPOINTS,hsize_t),temp_coord3,error)
  CALL check("h5sget_select_elem_pointlist_f", error, total_error)
  DO i= 1, POINT1_NPOINTS
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord3(1,i)), INT(coord3(1,i)), total_error)
     CALL VERIFY("h5sget_select_elem_pointlist_f", INT(temp_coord3(2,i)), INT(coord3(2,i)), total_error)
  ENDDO

  CALL H5Sget_select_npoints_f(sid2, npoints, error)
  CALL check("h5sget_select_npoints_f", error, total_error)
  CALL verify("h5sget_select_npoints_f", INT(npoints), 20, total_error)

! F2003 feature
!!$ /* Read selection from disk */
!!$    ret=H5Dread(dataset,H5T_NATIVE_UCHAR,sid2,sid1,xfer_plist,rbuf);
!!$    CHECK(ret, FAIL, "H5Dread");
!!$
!!$    /* Check that the values match with a dataset iterator */
!!$    pi.buf=wbuf;
!!$    pi.offset=0;
!!$    ret = H5Diterate(rbuf,H5T_NATIVE_UCHAR,sid2,test_select_point_iter1,&pi);
!!$    CHECK(ret, FAIL, "H5Diterate");
!!$
! F2003 feature

  !/* Close memory dataspace */
  CALL h5sclose_f(sid2, error)
  CALL check("h5sclose_f", error, total_error)

  !/* Close disk dataspace */
  CALL h5sclose_f(sid1, error)
  CALL check("h5sclose_f", error, total_error)

  !/* Close Dataset */
  CALL h5dclose_f(dataset, error)
  CALL check("h5dclose_f", error, total_error)

  !/* Close file */
  CALL h5fclose_f(fid1, error)
  CALL check("h5fclose_f", error, total_error)


  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)

END SUBROUTINE test_select_point


!/****************************************************************
!**
!**  test_select_combine(): Test basic H5S (dataspace) selection code.
!**      Tests combining "all" and "none" selections with hyperslab
!**      operations.
!**
!****************************************************************/

SUBROUTINE test_select_combine(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error

  INTEGER, PARAMETER :: SPACE7_RANK = 2
  INTEGER, PARAMETER :: SPACE7_DIM1 = 10
  INTEGER, PARAMETER :: SPACE7_DIM2 = 10

  INTEGER(hid_t) :: base_id !      /* Base dataspace for test */
  INTEGER(hid_t) :: all_id !       /* Dataspace for "all" selection */
  INTEGER(hid_t) :: none_id !      /* Dataspace for "none" selection */
  INTEGER(hid_t) :: space1 !       /* Temporary dataspace #1 */
  INTEGER(hsize_t), DIMENSION(1:SPACE7_RANK) :: start ! /* Hyperslab start */
  INTEGER(hsize_t), DIMENSION(1:SPACE7_RANK) :: stride ! /* Hyperslab stride */
  INTEGER(hsize_t), DIMENSION(1:SPACE7_RANK) :: icount ! /* Hyperslab count */
  INTEGER(hsize_t), DIMENSION(1:SPACE7_RANK) :: iblock ! /* Hyperslab BLOCK */
  INTEGER(hsize_t), DIMENSION(1:SPACE7_RANK) :: dims = (/SPACE7_DIM1,SPACE7_DIM2/) !/* Dimensions of dataspace */
  INTEGER :: sel_type ! /* Selection type */
  INTEGER(hssize_t) :: nblocks   !/* Number of hyperslab blocks */
  INTEGER(hsize_t), DIMENSION(1:128,1:2,1:SPACE7_RANK) :: blocks ! /* List of blocks */
  INTEGER :: error, area

  !/* Create dataspace for dataset on disk */
  CALL h5screate_simple_f(SPACE7_RANK, dims, base_id, error)
  CALL check("h5screate_simple_f", error, total_error)

  ! /* Copy base dataspace and set selection to "all" */
  CALL h5scopy_f(base_id, all_id, error)
  CALL check("h5scopy_f", error, total_error)

  CALL H5Sselect_all_f(all_id, error)
  CALL check("H5Sselect_all_f", error, total_error)

  CALL H5Sget_select_type_f(all_id, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT(H5S_SEL_ALL_F), total_error)

  !/* Copy base dataspace and set selection to "none" */
  CALL h5scopy_f(base_id, none_id, error)
  CALL check("h5scopy_f", error, total_error)

  CALL H5Sselect_none_f(none_id, error)
  CALL check("H5Sselect_none_f", error, total_error)

  CALL H5Sget_select_type_f(none_id, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT(H5S_SEL_NONE_F), total_error)

  !/* Copy "all" selection & space */
  CALL H5Scopy_f(all_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  !/* 'OR' "all" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/)
  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_OR_F, start, &
                                icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  !/* Verify that it's still "all" selection */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT(H5S_SEL_ALL_F), total_error)

  !/* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  !/* Copy "all" selection & space */
  CALL H5Scopy_f(all_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'AND' "all" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/)
  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_AND_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  !/* Verify that the new selection is the same at the original block */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_HYPERSLABS_F), total_error)

  !/* Verify that there is only one block */
  CALL h5sget_select_hyper_nblocks_f(space1, nblocks, error)
  CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
  CALL VERIFY("h5sget_select_hyper_nblocks_f", INT(nblocks), 1, total_error)

  !/* Retrieve the block defined */
  CALL h5sget_select_hyper_blocklist_f(space1, INT(0, hsize_t), INT(nblocks,hsize_t), blocks, error)
  CALL check("h5sget_select_hyper_blocklist_f", error, total_error)

  !/* Verify that the correct block is defined */

  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(1,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(2,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(3,1,1)), 5, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(4,1,1)), 4, total_error)

  !/* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  !/* Copy "all" selection & space */
  CALL H5Scopy_f(all_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'XOR' "all" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/)

  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_XOR_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  ! /* Verify that the new selection is an inversion of the original block */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_HYPERSLABS_F), total_error)

  ! /* Verify that there are two blocks */
  CALL h5sget_select_hyper_nblocks_f(space1, nblocks, error)
  CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
  CALL VERIFY("h5sget_select_hyper_nblocks_f", INT(nblocks), 2, total_error)

  ! /* Retrieve the block defined */

  blocks = -1 ! /* Reset block list */
  CALL h5sget_select_hyper_blocklist_f(space1, INT(0, hsize_t), INT(nblocks,hsize_t), blocks, error)
  CALL check("h5sget_select_hyper_blocklist_f", error, total_error)

  ! /* Verify that the correct block is defined */

  ! No guarantee is implied as the order in which blocks are listed.
  ! So this will ONLY work for square domains iblock(1:2) = (/5,5/)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(1,1,1)), 1, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(2,1,1)), 5, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(3,1,1)), 5, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(4,1,1)), 10, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(5,1,1)), 6, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(6,1,1)), 1, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(7,1,1)), 10, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(8,1,1)), 10, total_error)

  ! Otherwise make sure the "area" of the block is correct
  area = (ABS(INT(blocks(1,1,1)-blocks(3,1,1)))+1)*(ABS(INT(blocks(2,1,1)-blocks(4,1,1)))+1)
  area = area + (ABS(INT(blocks(5,1,1)-blocks(7,1,1)))+1)*(ABS(INT(blocks(6,1,1)-blocks(8,1,1)))+1)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", area, 80, total_error)

  !/* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Copy "all" selection & space */
  CALL H5Scopy_f(all_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'NOTB' "all" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/) !5

  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_NOTB_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  ! /* Verify that the new selection is an inversion of the original block */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_HYPERSLABS_F), total_error)

  ! /* Verify that there are two blocks */
  CALL h5sget_select_hyper_nblocks_f(space1, nblocks, error)
  CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
  CALL VERIFY("h5sget_select_hyper_nblocks_f", INT(nblocks), 2, total_error)

  ! /* Retrieve the block defined */
  blocks = -1 ! /* Reset block list */
  CALL h5sget_select_hyper_blocklist_f(space1, INT(0, hsize_t), INT(nblocks,hsize_t), blocks, error)
  CALL check("h5sget_select_hyper_blocklist_f", error, total_error)

  ! /* Verify that the correct block is defined */

  ! No guarantee is implied as the order in which blocks are listed.
  ! So this will ONLY work for square domains iblock(1:2) = (/5,5/)

!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(1,1,1)), 1, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(2,1,1)), 5, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(3,1,1)), 5, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(4,1,1)),10, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(5,1,1)), 6, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(6,1,1)), 1, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(7,1,1)),10, total_error)
!!$  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(8,1,1)),10, total_error)

  ! Otherwise make sure the "area" of the block is correct
  area = (ABS(INT(blocks(1,1,1)-blocks(3,1,1)))+1)*(ABS(INT(blocks(2,1,1)-blocks(4,1,1)))+1)
  area = area + (ABS(INT(blocks(5,1,1)-blocks(7,1,1)))+1)*(ABS(INT(blocks(6,1,1)-blocks(8,1,1)))+1)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", area, 80, total_error)


  ! /* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)
  ! /* Copy "all" selection & space */
  CALL H5Scopy_f(all_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'NOTA' "all" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/) !5

  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_NOTA_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  !/* Verify that the new selection is the "none" selection */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_NONE_F), total_error)

  ! /* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Copy "none" selection & space */
  CALL H5Scopy_f(none_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'OR' "none" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/) !5

  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_OR_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  ! /* Verify that the new selection is the same as the original hyperslab */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_HYPERSLABS_F), total_error)


  ! /* Verify that there is only one block */
  CALL h5sget_select_hyper_nblocks_f(space1, nblocks, error)
  CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
  CALL VERIFY("h5sget_select_hyper_nblocks_f", INT(nblocks), 1, total_error)

  ! /* Retrieve the block defined */
  blocks = -1 ! /* Reset block list */
  CALL h5sget_select_hyper_blocklist_f(space1, INT(0, hsize_t), INT(nblocks,hsize_t), blocks, error)
  CALL check("h5sget_select_hyper_blocklist_f", error, total_error)

  ! /* Verify that the correct block is defined */
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(1,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(2,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(3,1,1)), 5, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(4,1,1)), 4, total_error)

  ! /* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Copy "none" selection & space */
  CALL H5Scopy_f(none_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'AND' "none" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/) !5

  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_AND_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  ! /* Verify that the new selection is the "none" selection */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_NONE_F), total_error)

  ! /* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Copy "none" selection & space */
  CALL H5Scopy_f(none_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'XOR' "none" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/) !5

  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_XOR_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  ! /* Verify that the new selection is the same as the original hyperslab */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_HYPERSLABS_F), total_error)


  ! /* Verify that there is only one block */
  CALL h5sget_select_hyper_nblocks_f(space1, nblocks, error)
  CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
  CALL VERIFY("h5sget_select_hyper_nblocks_f", INT(nblocks), 1, total_error)

  ! /* Retrieve the block defined */
  blocks = -1 ! /* Reset block list */
  CALL h5sget_select_hyper_blocklist_f(space1, INT(0, hsize_t), INT(nblocks,hsize_t), blocks, error)
  CALL check("h5sget_select_hyper_blocklist_f", error, total_error)
  ! /* Verify that the correct block is defined */
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(1,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(2,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(3,1,1)), 5, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(4,1,1)), 4, total_error)

  ! /* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Copy "none" selection & space */
  CALL H5Scopy_f(none_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'NOTB' "none" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/) !5

  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_NOTB_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  ! /* Verify that the new selection is the "none" selection */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_NONE_F), total_error)

  ! /* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Copy "none" selection & space */
  CALL H5Scopy_f(none_id, space1, error)
  CALL check("h5scopy_f", error, total_error)

  ! /* 'NOTA' "none" selection with another hyperslab */
  start(1:2) = 0
  stride(1:2) = 1
  icount(1:2) = 1
  iblock(1:2) = (/5,4/) !5
  CALL h5sselect_hyperslab_f(space1, H5S_SELECT_NOTA_F, start, &
       icount, error, stride, iblock)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  ! /* Verify that the new selection is the same as the original hyperslab */
  CALL H5Sget_select_type_f(space1, sel_type, error)
  CALL check("H5Sget_select_type_f", error, total_error)
  CALL VERIFY("H5Sget_select_type_f", INT(sel_type), INT( H5S_SEL_HYPERSLABS_F), total_error)

  ! /* Verify that there is ONLY one BLOCK */
  CALL h5sget_select_hyper_nblocks_f(space1, nblocks, error)
  CALL check("h5sget_select_hyper_nblocks_f", error, total_error)
  CALL VERIFY("h5sget_select_hyper_nblocks_f", INT(nblocks), 1, total_error)

  ! /* Retrieve the block defined */

  blocks = -1 ! /* Reset block list */
  CALL h5sget_select_hyper_blocklist_f(space1, INT(0, hsize_t), INT(nblocks,hsize_t), blocks, error)
  CALL check("h5sget_select_hyper_blocklist_f", error, total_error)


  ! /* Verify that the correct block is defined */

  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(1,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(2,1,1)), 1, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(3,1,1)), 5, total_error)
  CALL VERIFY("h5sget_select_hyper_blocklist_f", INT(blocks(4,1,1)), 4, total_error)

  ! /* Close temporary dataspace */
  CALL h5sclose_f(space1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /* Close dataspaces */

  CALL h5sclose_f(base_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5sclose_f(all_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5sclose_f(none_id, error)
  CALL check("h5sclose_f", error, total_error)

END SUBROUTINE test_select_combine

!/****************************************************************
!**
!**  test_select_bounds(): Tests selection bounds on dataspaces,
!**      both with and without offsets.
!**
!****************************************************************/

SUBROUTINE test_select_bounds(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error

  INTEGER, PARAMETER :: SPACE11_RANK=2
  INTEGER, PARAMETER :: SPACE11_DIM1=100
  INTEGER, PARAMETER :: SPACE11_DIM2=50
  INTEGER, PARAMETER :: SPACE11_NPOINTS=4

  INTEGER(hid_t) :: sid ! /* Dataspace ID */
  INTEGER(hsize_t), DIMENSION(1:SPACE11_RANK) :: dims = (/SPACE11_DIM1, SPACE11_DIM2/) !Dataspace dimensions
  INTEGER(hsize_t), DIMENSION(SPACE11_RANK, SPACE11_NPOINTS) :: coord !/* Coordinates for point selection
  INTEGER(hsize_t), DIMENSION(SPACE11_RANK) :: start ! /* The start of the hyperslab */
  INTEGER(hsize_t), DIMENSION(SPACE11_RANK) :: stride !/* The stride between block starts for the hyperslab */
  INTEGER(hsize_t), DIMENSION(SPACE11_RANK) :: count !/* The number of blocks for the hyperslab */
  INTEGER(hsize_t), DIMENSION(SPACE11_RANK) :: BLOCK !/* The size of each block for the hyperslab */
  INTEGER(hssize_t), DIMENSION(SPACE11_RANK) :: offset !/* Offset amount for selection */
  INTEGER(hsize_t), DIMENSION(SPACE11_RANK) :: low_bounds !/* The low bounds for the selection */
  INTEGER(hsize_t), DIMENSION(SPACE11_RANK) :: high_bounds !/* The high bounds for the selection */

  INTEGER :: error

  !/* Create dataspace */
  CALL h5screate_simple_f(SPACE11_RANK, dims, sid, error)
  CALL check("h5screate_simple_f", error, total_error)

  ! /* Get bounds for 'all' selection */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(1)), 1, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(2)), 1, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(1)), SPACE11_DIM1, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(2)), SPACE11_DIM2, total_error)

  !/* Set offset for selection */
  offset(1:2) = 1
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  !/* Get bounds for 'all' selection with offset (which should be ignored) */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL verify("h5sget_select_bounds_f", INT(low_bounds(1)), 1, total_error)
  CALL verify("h5sget_select_bounds_f", INT(low_bounds(2)), 1, total_error)
  CALL verify("h5sget_select_bounds_f", INT(high_bounds(1)), SPACE11_DIM1, total_error)
  CALL verify("h5sget_select_bounds_f", INT(high_bounds(2)), SPACE11_DIM2, total_error)

  !/* Reset offset for selection */
  offset(1:2) = 0
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  !/* Set 'none' selection */
  CALL H5Sselect_none_f(sid, error)
  CALL check("H5Sselect_none_f", error, total_error)

  !/* Get bounds for 'none' selection, should fail */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL VERIFY("h5sget_select_bounds_f", error, -1, total_error)

  !/* Set point selection */

  coord(1,1)=  3; coord(2,1)=  3;
  coord(1,2)=  3; coord(2,2)= 46;
  coord(1,3)= 96; coord(2,3)=  3;
  coord(1,4)= 96; coord(2,4)= 46;

  CALL h5sselect_elements_f(sid, H5S_SELECT_SET_F, SPACE11_RANK, INT(SPACE11_NPOINTS,size_t), coord, error)
  CALL check("h5sselect_elements_f", error, total_error)

  !/* Get bounds for point selection */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(1)), 3, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(2)), 3, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(1)), SPACE11_DIM1-4, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(2)), SPACE11_DIM2-4, total_error)

  ! /* Set bad offset for selection */

  offset(1:2) = (/5,-5/)
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  ! /* Get bounds for hyperslab selection with negative offset */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL VERIFY("h5sget_select_bounds_f", error, -1, total_error)

  ! /* Set valid offset for selection */
  offset(1:2) = (/2,-2/)
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  ! /* Get bounds for point selection with offset */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(1)), 5, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(2)), 1, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(1)), SPACE11_DIM1-2, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(2)), SPACE11_DIM2-6, total_error)

  ! /* Reset offset for selection */
  offset(1:2) = 0
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  ! /* Set "regular" hyperslab selection */
  start(1:2) = 2
  stride(1:2) = 10
  count(1:2) = 4
  block(1:2) = 5

  CALL h5sselect_hyperslab_f(sid, H5S_SELECT_SET_F, start, &
       count, error, stride, block)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  !/* Get bounds for hyperslab selection */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(1)), 3, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(2)), 3, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(1)), 37, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(2)), 37, total_error)

  !/* Set bad offset for selection */
  offset(1:2) = (/5,-5/)
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  ! /* Get bounds for hyperslab selection with negative offset */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL VERIFY("h5sget_select_bounds_f", error, -1, total_error)

  ! /* Set valid offset for selection */
  offset(1:2) = (/5,-2/)
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  !/* Get bounds for hyperslab selection with offset */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(1)), 8, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(2)), 1, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(1)), 42, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(2)), 35, total_error)

  !/* Reset offset for selection */
  offset(1:2) = 0
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  ! /* Make "irregular" hyperslab selection */
  start(1:2) = 20
  stride(1:2) = 20
  count(1:2) = 2
  block(1:2) = 10

  CALL h5sselect_hyperslab_f(sid, H5S_SELECT_OR_F, start, &
       count, error, stride, block)
  CALL check("h5sselect_hyperslab_f", error, total_error)

  !/* Get bounds for hyperslab selection */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(1)), 3, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(2)), 3, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(1)), 50, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(2)), 50, total_error)

  ! /* Set bad offset for selection */
  offset(1:2) = (/5,-5/)
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  ! /* Get bounds for hyperslab selection with negative offset */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL VERIFY("h5sget_select_bounds_f", error, -1, total_error)

  !/* Set valid offset for selection */
  offset(1:2) = (/5,-2/)
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  !/* Get bounds for hyperslab selection with offset */
  CALL h5sget_select_bounds_f(sid, low_bounds, high_bounds, error)
  CALL check("h5sget_select_bounds_f", error, total_error)

  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(1)), 8, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(low_bounds(2)), 1, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(1)), 55, total_error)
  CALL VERIFY("h5sget_select_bounds_f", INT(high_bounds(2)), 48, total_error)

  !/* Reset offset for selection */
  offset(1:2) = 0
  CALL H5Soffset_simple_f(sid, offset, error)
  CALL check("H5Soffset_simple_f", error, total_error)

  !/* Close the dataspace */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f", error, total_error)

END SUBROUTINE test_select_bounds
