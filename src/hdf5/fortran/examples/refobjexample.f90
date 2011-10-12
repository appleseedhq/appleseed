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
!    This program shows how to create and store references to the objects.
!    Program creates a file, two groups, a dataset to store integer data and
!    a dataset to store references to the objects.
!    Stored references are used to open the objects they are point to.
!    Data is written to the dereferenced dataset, and class type is displayed for
!    the shared datatype.
!
     PROGRAM OBJ_REFERENCES

        USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     CHARACTER(LEN=10), PARAMETER :: filename = "FORTRAN.h5"  ! File
     CHARACTER(LEN=8), PARAMETER :: dsetnamei = "INTEGERS"    ! Dataset with the integer data
     CHARACTER(LEN=17), PARAMETER :: dsetnamer = "OBJECT_REFERENCES" ! Dataset wtih object
                                                                     ! references
     CHARACTER(LEN=6), PARAMETER :: groupname1 = "GROUP1"            ! Groups in the file
     CHARACTER(LEN=6), PARAMETER :: groupname2 = "GROUP2"            !

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: grp1_id       ! Group identifiers
     INTEGER(HID_T) :: grp2_id       !
     INTEGER(HID_T) :: dset_id       ! Dataset identifiers
     INTEGER(HID_T) :: dsetr_id      !
     INTEGER(HID_T) :: type_id       ! Type identifier
     INTEGER(HID_T) :: space_id      ! Dataspace identifiers
     INTEGER(HID_T) :: spacer_id     !
     INTEGER     ::   error
     INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/5/)
     INTEGER(HSIZE_T), DIMENSION(1) :: dimsr= (/4/)
     INTEGER(HSIZE_T), DIMENSION(1) :: my_maxdims = (/5/)
     INTEGER :: rank = 1
     INTEGER :: rankr = 1
     TYPE(hobj_ref_t_f), DIMENSION(4) ::  ref
     TYPE(hobj_ref_t_f), DIMENSION(4) ::  ref_out
     INTEGER, DIMENSION(5) :: data = (/1, 2, 3, 4, 5/)
     INTEGER :: class
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims, ref_size
     !
     !  Initialize FORTRAN interface.
     !
     CALL h5open_f(error)
     !
     !  Create a file
     !
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
                                         ! Default file access and file creation
                                         ! properties are used.
     !
     !  Create a group in the file
     !
     CALL h5gcreate_f(file_id, groupname1, grp1_id, error)
     !
     ! Create a group inside the created gorup
     !
     CALL h5gcreate_f(grp1_id, groupname2, grp2_id, error)
     !
     ! Create dataspaces for datasets
     !
     CALL h5screate_simple_f(rank, dims, space_id, error, maxdims=my_maxdims)
     CALL h5screate_simple_f(rankr, dimsr, spacer_id, error)
     !
     ! Create integer dataset
     !
     CALL h5dcreate_f(file_id, dsetnamei, H5T_NATIVE_INTEGER, space_id, &
                      dset_id, error)
     !
     ! Create dataset to store references to the objects
     !
     CALL h5dcreate_f(file_id, dsetnamer, H5T_STD_REF_OBJ, spacer_id, &
                      dsetr_id, error)
     !
     ! Create a datatype and store in the file
     !
     CALL h5tcopy_f(H5T_NATIVE_REAL, type_id, error)
     CALL h5tcommit_f(file_id, "MyType", type_id, error)
     !
     !  Close dataspaces, groups and integer dataset
     !
     CALL h5sclose_f(space_id, error)
     CALL h5sclose_f(spacer_id, error)
     CALL h5tclose_f(type_id, error)
     CALL h5dclose_f(dset_id, error)
     CALL h5gclose_f(grp1_id, error)
     CALL h5gclose_f(grp2_id, error)
     !
     ! Create references to two groups, integer dataset and shared datatype
     ! and write it to the dataset in the file
     !
     CALL h5rcreate_f(file_id, groupname1, ref(1), error)
     CALL h5rcreate_f(file_id, "/GROUP1/GROUP2", ref(2), error)
     CALL h5rcreate_f(file_id, dsetnamei, ref(3), error)
     CALL h5rcreate_f(file_id, "MyType", ref(4), error)
     ref_size(1) = size(ref)
     CALL h5dwrite_f(dsetr_id, H5T_STD_REF_OBJ, ref, ref_size, error)
     !
     ! Close the dataset
     !
     CALL h5dclose_f(dsetr_id, error)
     !
     ! Reopen the dataset with object references and read references to the buffer
     !
     CALL h5dopen_f(file_id, dsetnamer,dsetr_id,error)
     ref_size(1) = size(ref_out)
     CALL h5dread_f(dsetr_id, H5T_STD_REF_OBJ, ref_out, ref_size, error)
     !
     ! Dereference the third reference. We know that it is a dataset. On practice
     ! one should use h5rget_object_type_f function to find out
     ! the type of an object the reference points to.
     !
     CALL h5rdereference_f(dsetr_id, ref(3), dset_id, error)
     !
     ! Write data to the dataset.
     !
     data_dims(1) = size(data)
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data, data_dims, error)
     if (error .eq. 0) write(*,*) "Data has been successfully written to the dataset "
     !
     ! Dereference the fourth reference. We know that it is a datatype. On practice
     ! one should use h5rget_object_type_f function to find out
     ! the type of an object the reference points to.
     !
     CALL h5rdereference_f(dsetr_id, ref(4), type_id, error)
     !
     ! Get datatype class and display it if it is of a FLOAT class.
     !
     CALL h5tget_class_f(type_id, class, error)
     if(class .eq. H5T_FLOAT_F) write(*,*) "Stored datatype is of a FLOAT class"
     !
     ! Close all objects.
     !
     CALL h5dclose_f(dset_id, error)
     CALL h5tclose_f(type_id, error)
     CALL h5dclose_f(dsetr_id, error)
     CALL h5fclose_f(file_id, error)
     !
     !  Close FORTRAN interface.
     !
     CALL h5close_f(error)

     END PROGRAM OBJ_REFERENCES


