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
! In this example we iterate through the members of the groups.
!


     PROGRAM GRPITEXAMPLE

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     CHARACTER(LEN=11), PARAMETER :: filename = "iteratef.h5" ! File name
     CHARACTER(LEN=7), PARAMETER :: groupname1 = "MyGroup"    ! Group name
     CHARACTER(LEN=15), PARAMETER :: groupname2 = "Group_A"   ! Group name
     CHARACTER(LEN=13), PARAMETER :: dsetname1 = "dset1"      ! Dataset name
     CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"       !

     CHARACTER(LEN=20) :: name_buffer ! Buffer to hold object's name
     INTEGER :: type ! Type of the object
     INTEGER :: nmembers ! Number of group members

     INTEGER(HID_T) :: file_id        ! File identifier
     INTEGER(HID_T) :: dataset1_id    ! Dataset1 identifier
     INTEGER(HID_T) :: dataset2_id    ! Dataset2 identifier
     INTEGER(HID_T) :: dataspace1_id  ! Data space identifier
     INTEGER(HID_T) :: dataspace2_id  ! Data space identifier
     INTEGER(HID_T) :: group1_id, group2_id ! Group identifiers

     INTEGER     ::  i, j

     INTEGER     ::   error ! Error flag

     INTEGER, DIMENSION(3,3) :: dset1_data  ! Arrays to hold data
     INTEGER, DIMENSION(2,10) :: dset2_data !


     INTEGER(HSIZE_T), DIMENSION(2) :: dims1 = (/3,3/) ! Dataset dimensions
     INTEGER(HSIZE_T), DIMENSION(2) :: dims2 = (/2,10/)!
     INTEGER     ::   rank = 2 ! Datasets rank
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

     !
     ! Initialize dset1_data array.
     !
     do i = 1, 3
          do j = 1, 3
               dset1_data(i,j) = j;
          end do
     end do


     !
     ! Initialize dset2_data array.
     !
     do i = 1, 2
          do j = 1, 10
               dset2_data(i,j) = j;
          end do
     end do

     !
     ! Initialize FORTRAN interface.
     !
     CALL h5open_f(error)

     !
     ! Create a new file using default properties.
     !
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Create group "MyGroup" in the root group using absolute name.
     !
     CALL h5gcreate_f(file_id, groupname1, group1_id, error)

     !
     ! Create group "Group_A" in group "MyGroup" using relative name.
     !
     CALL h5gcreate_f(group1_id, groupname2, group2_id, error)

     !
     ! Create the data space for the first dataset.
     !
     CALL h5screate_simple_f(rank, dims1, dataspace1_id, error)

     !
     ! Create a dataset in group "MyGroup" with default properties.
     !
     CALL h5dcreate_f(group1_id, dsetname1, H5T_NATIVE_INTEGER, dataspace1_id, &
                      dataset1_id, error)

     !
     ! Write the first dataset.
     !
     data_dims(1) = 3
     data_dims(2) = 3
     CALL h5dwrite_f(dataset1_id, H5T_NATIVE_INTEGER, dset1_data, data_dims, error)

     !
     ! Create the data space for the second dataset.
     !
     CALL h5screate_simple_f(rank, dims2, dataspace2_id, error)

     !
     ! Create the second dataset in group "Group_A" with default properties
     !
     CALL h5dcreate_f(group2_id, dsetname2, H5T_NATIVE_INTEGER, dataspace2_id, &
                     dataset2_id, error)

     !
     ! Write the second dataset
     !
     data_dims(1) = 2
     data_dims(2) = 10
     CALL h5dwrite_f(dataset2_id, H5T_NATIVE_INTEGER, dset2_data, data_dims, error)

     !
     ! Get number of members in the root group.
     !
     CALL h5gn_members_f(file_id, "/", nmembers, error)
     write(*,*) "Number of root group member is " , nmembers

     !
     ! Print each group member's name and type.
     !
     do i = 0, nmembers - 1
        CALL h5gget_obj_info_idx_f(file_id, "/", i, name_buffer, type, &
                                    error)
     write(*,*) name_buffer, type
     end do

     !
     ! Get number of members in MyGroup.
     !
     CALL h5gn_members_f(file_id, "MyGroup", nmembers, error)
     write(*,*) "Number of group MyGroup member  is ", nmembers

     !
     ! Print each group member's name and type in "MyGroup" group.
     !
     do i = 0, nmembers - 1
        CALL h5gget_obj_info_idx_f(file_id, groupname1, i, name_buffer, type, &
                                    error)
     write(*,*) name_buffer, type
     end do


     !
     ! Get number of members in MyGroup/Group_A.
     !
     CALL h5gn_members_f(file_id, "MyGroup/Group_A", nmembers, error)
     write(*,*) "Number of group MyGroup/Group_A member  is ", nmembers

     !
     ! Print each group member's name and type in "MyGroup/Group_A" group.
     !
     do i = 0, nmembers - 1
        CALL h5gget_obj_info_idx_f(file_id,"MyGroup/Group_A" , i, name_buffer, type, &
                                    error)
     write(*,*) name_buffer, type
     end do

     !
     ! Close the dataspace for the first dataset.
     !
     CALL h5sclose_f(dataspace1_id, error)

     !
     ! Close the first dataset.
     !
     CALL h5dclose_f(dataset1_id, error)

     !
     ! Close the dataspace for the second dataset.
     !
     CALL h5sclose_f(dataspace2_id, error)

     !
     ! Close the second dataset.
     !
     CALL h5dclose_f(dataset2_id, error)

     !
     ! Close the groups.
     !
     CALL h5gclose_f(group1_id, error)

     CALL h5gclose_f(group2_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN interface.
     !
     CALL h5close_f(error)

     END PROGRAM GRPITEXAMPLE
