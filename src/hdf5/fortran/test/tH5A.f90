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
    SUBROUTINE attribute_test(cleanup, total_error)

!   This subroutine tests following functionalities:
!   h5acreate_f,  h5awrite_f, h5aclose_f,h5aread_f, h5aopen_name_f,
!   h5aget_name_f,h5aget_space_f, h5aget_type_f,
!

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error

     CHARACTER(LEN=5), PARAMETER :: filename = "atest"    !File name
     CHARACTER(LEN=80) :: fix_filename
     CHARACTER(LEN=9), PARAMETER :: dsetname = "atestdset"        !Dataset name
     CHARACTER(LEN=11), PARAMETER :: aname = "attr_string"   !String Attribute name
     CHARACTER(LEN=14), PARAMETER :: aname2 = "attr_character"!Character Attribute name
     CHARACTER(LEN=11), PARAMETER :: aname3 = "attr_double"   !DOuble Attribute name
     CHARACTER(LEN=9), PARAMETER :: aname4 = "attr_real"      !Real Attribute name
     CHARACTER(LEN=12), PARAMETER :: aname5 = "attr_integer"  !Integer Attribute name
     CHARACTER(LEN=9), PARAMETER :: aname6 = "attr_null"     !Null Attribute name

     !
     !data space rank and dimensions
     !
     INTEGER, PARAMETER :: RANK = 2
     INTEGER, PARAMETER :: NX = 4
     INTEGER, PARAMETER :: NY = 5



     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: dataspace     ! Dataspace identifier for dataset

     INTEGER(HID_T) :: attr_id        !String Attribute identifier
     INTEGER(HID_T) :: attr2_id       !Character Attribute identifier
     INTEGER(HID_T) :: attr3_id       !Double Attribute identifier
     INTEGER(HID_T) :: attr4_id       !Real Attribute identifier
     INTEGER(HID_T) :: attr5_id       !Integer Attribute identifier
     INTEGER(HID_T) :: attr6_id       !Null Attribute identifier
     INTEGER(HID_T) :: aspace_id      !String Attribute Dataspace identifier
     INTEGER(HID_T) :: aspace2_id     !Character Attribute Dataspace identifier
     INTEGER(HID_T) :: aspace6_id     !Null Attribute Dataspace identifier
     INTEGER(HID_T) :: atype_id       !String Attribute Datatype identifier
     INTEGER(HID_T) :: atype2_id      !Character Attribute Datatype identifier
     INTEGER(HID_T) :: atype3_id      !Double Attribute Datatype identifier
     INTEGER(HID_T) :: atype4_id      !Real Attribute Datatype identifier
     INTEGER(HID_T) :: atype5_id      !Integer Attribute Datatype identifier
     INTEGER(HSIZE_T), DIMENSION(1) :: adims = (/2/) ! Attribute dimension
     INTEGER(HSIZE_T), DIMENSION(1) :: adims2 = (/1/) ! Attribute dimension
     INTEGER     ::   arank = 1                      ! Attribure rank
     INTEGER(SIZE_T) :: attrlen    ! Length of the attribute string

     INTEGER(HID_T) :: attr_space     !Returned String Attribute Space identifier
     INTEGER(HID_T) :: attr2_space    !Returned other Attribute Space identifier
     INTEGER(HID_T) :: attr_type      !Returned Attribute Datatype identifier
     INTEGER(HID_T) :: attr2_type      !Returned CHARACTER Attribute Datatype identifier
     INTEGER(HID_T) :: attr3_type      !Returned DOUBLE Attribute Datatype identifier
     INTEGER(HID_T) :: attr4_type      !Returned REAL Attribute Datatype identifier
     INTEGER(HID_T) :: attr5_type      !Returned INTEGER Attribute Datatype identifier
     INTEGER(HID_T) :: attr6_type      !Returned NULL Attribute Datatype identifier
     INTEGER        :: num_attrs      !number of attributes
     INTEGER(HSIZE_T) :: attr_storage   ! attributes storage requirements .MSB.
     CHARACTER(LEN=256) :: attr_name    !buffer to put attr_name
     INTEGER(SIZE_T)    ::  name_size = 80 !attribute name length

     CHARACTER(LEN=35), DIMENSION(2) ::  attr_data  ! String attribute data
     CHARACTER(LEN=35), DIMENSION(2) ::  aread_data ! Buffer to put read back
                                               ! string attr data
     CHARACTER ::  attr_character_data = 'A'
     DOUBLE PRECISION,  DIMENSION(1) ::  attr_double_data = 3.459
     REAL,         DIMENSION(1) ::  attr_real_data = 4.0
     INTEGER,      DIMENSION(1) ::  attr_integer_data = 5
     INTEGER(HSIZE_T), DIMENSION(7) :: data_dims


     CHARACTER :: aread_character_data ! variable to put read back Character attr data
     INTEGER, DIMENSION(1)  :: aread_integer_data ! variable to put read back integer attr data
     INTEGER, DIMENSION(1)  :: aread_null_data = 7 ! variable to put read back null attr data
     DOUBLE PRECISION, DIMENSION(1)   :: aread_double_data ! variable to put read back double attr data
     REAL, DIMENSION(1)  :: aread_real_data ! variable to put read back real attr data

     !
     !general purpose integer
     !
     INTEGER     ::   i, j
     INTEGER     ::   error ! Error flag

     !
     !The dimensions for the dataset.
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/NX,NY/)

     !
     !data buffers
     !
     INTEGER, DIMENSION(NX,NY) :: data_in


     !
     !Initialize data_in buffer
     !
     DO i = 1, NX
        DO j = 1, NY
           data_in(i,j) =  (i-1) + (j-1)
        END DO
     END DO
     !
     ! Initialize attribute's data
     !
     attr_data(1) = 'Dataset character attribute'
     attr_data(2) = 'Some other string here     '
     attrlen = LEN(attr_data(1))

     !
     ! Create the file.
     !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          IF (error .NE. 0) THEN
              WRITE(*,*) "Cannot modify file name"
              STOP
          ENDIF
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)

     !
     !Create data space for the dataset.
     !
     CALL h5screate_simple_f(RANK, dims, dataspace, error)
     CALL check("h5screate_simple_f",error,total_error)

     !
     ! create dataset in the file.
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
               dset_id, error)
     CALL check("h5dcreate_f",error,total_error)

     !
     ! Write data_in to the dataset
     !
     data_dims(1) = NX
     data_dims(2) = NY
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, data_dims, error)
     CALL check("h5dwrite_f",error,total_error)

     !
     ! Create scalar data space for the String attribute.
     !
     CALL h5screate_simple_f(arank, adims, aspace_id, error)
     CALL check("h5screate_simple_f",error,total_error)
     !
     ! Create scalar data space for all other attributes.
     !
     CALL h5screate_simple_f(arank, adims2, aspace2_id, error)
     CALL check("h5screate_simple_f",error,total_error)
     !
     ! Create null data space for null attributes.
     !
     CALL h5screate_f(H5S_NULL_F, aspace6_id, error)
     CALL check("h5screate_f",error,total_error)

     !
     ! Create datatype for the String attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
     CALL check("h5tcopy_f",error,total_error)

     CALL h5tset_size_f(atype_id, attrlen, error)
     CALL check("h5tset_size_f",error,total_error)

     !
     ! Create datatype for the Character attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype2_id, error)
     CALL check("h5tcopy_f",error,total_error)
     !
     ! Create datatype for the Double attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_DOUBLE, atype3_id, error)
     CALL check("h5tcopy_f",error,total_error)
     !
     ! Create datatype for the Real attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_REAL, atype4_id, error)
     CALL check("h5tcopy_f",error,total_error)
     !
     ! Create datatype for the Integer attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_INTEGER, atype5_id, error)
     CALL check("h5tcopy_f",error,total_error)


     !
     ! Create dataset String attribute.
     !
     CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, &
                      attr_id, error)
     CALL check("h5acreate_f",error,total_error)


     !
     ! Create dataset CHARACTER attribute.
     !
     CALL h5acreate_f(dset_id, aname2, atype2_id, aspace2_id, &
                      attr2_id, error)
     CALL check("h5acreate_f",error,total_error)


     !
     ! Create dataset DOUBLE attribute.
     !
     CALL h5acreate_f(dset_id, aname3, atype3_id, aspace2_id, &
                      attr3_id, error)
     CALL check("h5acreate_f",error,total_error)
     !
     ! Create dataset REAL attribute.
     !
     CALL h5acreate_f(dset_id, aname4, atype4_id, aspace2_id, &
                      attr4_id, error)
     CALL check("h5acreate_f",error,total_error)
     !
     ! Create dataset INTEGER attribute.
     !
     CALL h5acreate_f(dset_id, aname5, atype5_id, aspace2_id, &
                      attr5_id, error)
     CALL check("h5acreate_f",error,total_error)
     !
     ! Create dataset NULL attribute of INTEGER.
     !

     CALL h5acreate_f(dset_id, aname6, atype5_id, aspace6_id, &
                      attr6_id, error)

     CALL check("h5acreate_f",error,total_error)

     !
     ! Write the String attribute data.
     !
     data_dims(1) = 2
     CALL h5awrite_f(attr_id, atype_id, attr_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)
      !
     ! Write the Character attribute data.
     !
     CALL h5awrite_f(attr2_id, atype2_id, attr_character_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)
     !
     ! Write the DOUBLE attribute data.
     !
     data_dims(1) = 1
     CALL h5awrite_f(attr3_id, atype3_id, attr_double_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)
     !
     ! Write the Real attribute data.
     !
     data_dims(1) = 1
     CALL h5awrite_f(attr4_id, atype4_id, attr_real_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     !
     ! Write the Integer attribute data.
     !
     data_dims(1) = 1
     CALL h5awrite_f(attr5_id, atype5_id, attr_integer_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     !
     ! Write the NULL attribute data(nothing can be written).
     !
     CALL h5awrite_f(attr6_id, atype5_id, attr_integer_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     !
     ! check the amount of storage that is required for the specified attribute .MSB.
     !
     CALL h5aget_storage_size_f(attr_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL VERIFY("h5aget_storage_size_f",attr_storage,*SizeOf(attr_storage),total_error)
     CALL h5aget_storage_size_f(attr2_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,1,total_error)
     CALL h5aget_storage_size_f(attr3_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,8,total_error)
     CALL h5aget_storage_size_f(attr4_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,4,total_error)
     CALL h5aget_storage_size_f(attr5_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,4,total_error)
     CALL h5aget_storage_size_f(attr6_id, attr_storage, error)
     CALL check("h5aget_storage_size_f",error,total_error)
!     CALL verify("h5aget_storage_size_f",attr_storage,0,total_error)


     !
     ! Close the attribute.
     !
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr2_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr3_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr4_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr5_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr6_id, error)
     CALL check("h5aclose_f",error,total_error)

     CALL h5tclose_f(atype_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype2_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype3_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype4_id, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(atype5_id, error)
     CALL check("h5tclose_f",error,total_error)

     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(aspace_id, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5sclose_f(aspace2_id, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5sclose_f(aspace6_id, error)
     CALL check("h5sclose_f",error,total_error)
     !
     ! Terminate access to the dataset.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f",error,total_error)
     !
     ! Terminate access to the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)
     !
     ! Open file
     !
     CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, file_id, error)
     CALL check("h5open_f",error,total_error)
     !
     ! Reopen dataset
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)
     CALL check("h5dopen_f",error,total_error)
     !
     !open the String attrbute by name
     !
     CALL h5aopen_name_f(dset_id, aname, attr_id, error)
     CALL check("h5aopen_name_f",error,total_error)

     !
     !open the CHARACTER attrbute by name
     !
     CALL h5aopen_name_f(dset_id, aname2, attr2_id, error)
     CALL check("h5aopen_name_f",error,total_error)
      !
     !open the DOUBLE attrbute by name
     !
     CALL h5aopen_name_f(dset_id, aname3, attr3_id, error)
     CALL check("h5aopen_name_f",error,total_error)
     !
     !open the REAL attrbute by name
     !
     CALL h5aopen_name_f(dset_id, aname4, attr4_id, error)
     CALL check("h5aopen_name_f",error,total_error)

     !
     !open the INTEGER attrbute by name
     !
     CALL h5aopen_name_f(dset_id, aname5, attr5_id, error)
     CALL check("h5aopen_idx_f",error,total_error)

     !
     !open the NULL attrbute by name
     !
     CALL h5aopen_name_f(dset_id, aname6, attr6_id, error)
     CALL check("h5aopen_idx_f",error,total_error)

     !
     !get the attrbute name
     !
     CALL h5aget_name_f(attr5_id, name_size, attr_name, error)
     CALL check("h5aget_name_f",error,total_error)
     IF (attr_name(1:12) .NE. aname5) THEN
       total_error = total_error + 1
     END IF
     IF (error .NE. 12) THEN
       total_error = total_error + 1
     END IF

     !
     !get the STRING attrbute space
     !
     CALL h5aget_space_f(attr_id, attr_space, error)
     CALL check("h5aget_space_f",error,total_error)
     !
     !get other attrbute space
     !
     CALL h5aget_space_f(attr2_id, attr2_space, error)
     CALL check("h5aget_space_f",error,total_error)
     !
     !get the string attrbute datatype
     !
     CALL h5aget_type_f(attr_id, attr_type, error)
     CALL check("h5aget_type_f",error,total_error)
     !
     !get the character attrbute datatype
     !
     CALL h5aget_type_f(attr2_id, attr2_type, error)
     CALL check("h5aget_type_f",error,total_error)
     !
     !get the double attrbute datatype
     !
     CALL h5aget_type_f(attr3_id, attr3_type, error)
     CALL check("h5aget_type_f",error,total_error)
     !
     !get the real attrbute datatype
     !
     CALL h5aget_type_f(attr4_id, attr4_type, error)
     CALL check("h5aget_type_f",error,total_error)

     !
     !get the integer attrbute datatype
     !
     CALL h5aget_type_f(attr5_id, attr5_type, error)
     CALL check("h5aget_type_f",error,total_error)

     !
     !get the null attrbute datatype
     !
     CALL h5aget_type_f(attr6_id, attr6_type, error)
     CALL check("h5aget_type_f",error,total_error)

     !
     !get number of attributes
     !
     CALL h5aget_num_attrs_f(dset_id, num_attrs, error)
     CALL check("h5aget_num_attrs_f",error,total_error)
     IF (num_attrs .NE. 6) THEN
       WRITE(*,*) "got number of attributes wrong", num_attrs
       total_error = total_error +1
     END IF

     !
     !set the read back data type's size
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
     CALL check("h5tcopy_f",error,total_error)

     CALL h5tset_size_f(atype_id, attrlen, error)
     CALL check("h5tset_size_f",error,total_error)
     !
     !read the string attribute data back to memory
     !
     data_dims(1) = 2
     CALL h5aread_f(attr_id, atype_id, aread_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)

     IF ( (aread_data(1) .NE. attr_data(1)) .OR. (aread_data(2) .NE. attr_data(2)) ) THEN
         WRITE(*,*) "Read back string attrbute is wrong", aread_data(1), aread_data(2)
         total_error = total_error + 1
     END IF

     !
     !read the CHARACTER attribute data back to memory
     !
     CALL h5aread_f(attr2_id, H5T_NATIVE_CHARACTER, aread_character_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_character_data .NE. 'A' ) THEN
         WRITE(*,*) "Read back character attrbute is wrong ",aread_character_data
         total_error = total_error + 1
     END IF
     !
     !read the double attribute data back to memory
     !
     data_dims(1) = 1
     CALL h5aread_f(attr3_id, H5T_NATIVE_DOUBLE, aread_double_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_double_data(1) .NE. 3.459 ) THEN
         WRITE(*,*) "Read back double attrbute is wrong", aread_double_data(1)
         total_error = total_error + 1
     END IF
     !
     !read the real attribute data back to memory
     !
     data_dims(1) = 1
     CALL h5aread_f(attr4_id, H5T_NATIVE_REAL, aread_real_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_real_data(1) .NE. 4.0 ) THEN
         WRITE(*,*) "Read back real attrbute is wrong ", aread_real_data
         total_error = total_error + 1
     END IF
     !
     !read the Integer attribute data back to memory
     !
     data_dims(1) = 1
     CALL h5aread_f(attr5_id, H5T_NATIVE_INTEGER, aread_integer_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_integer_data(1) .NE. 5 ) THEN
         WRITE(*,*) "Read back integer attrbute is wrong ", aread_integer_data
         total_error = total_error + 1
     END IF
     !
     !read the null attribute data. nothing can be read.
     !
     data_dims(1) = 1
     CALL h5aread_f(attr6_id, H5T_NATIVE_INTEGER, aread_null_data, data_dims, error)
     CALL check("h5aread_f",error,total_error)
     IF (aread_null_data(1) .NE. 7 ) THEN
         WRITE(*,*) "Read back null attrbute is wrong ", aread_null_data
         total_error = total_error + 1
     END IF

     !
     ! Close the attribute.
     !
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr2_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr3_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr4_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr5_id, error)
     CALL check("h5aclose_f",error,total_error)
     CALL h5aclose_f(attr6_id, error)
     CALL check("h5aclose_f",error,total_error)

     !
     ! Delete the attribute from the Dataset.
     !
     CALL h5adelete_f(dset_id, aname, error)
     CALL check("h5adelete_f",error,total_error)

     !
     !get number of attributes
     !
     CALL h5aget_num_attrs_f(dset_id, num_attrs, error)
     CALL check("h5aget_num_attrs_f",error,total_error)
     IF (num_attrs .NE. 5) THEN
       WRITE(*,*) "got number of attributes wrong", num_attrs
       total_error = total_error +1
     END IF



     CALL h5sclose_f(attr_space, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5sclose_f(attr2_space, error)
     CALL check("h5sclose_f",error,total_error)

     !
     ! Terminate access to the data type.
     !
     CALL h5tclose_f(attr_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr2_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr3_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr4_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr5_type, error)
     CALL check("h5tclose_f",error,total_error)
     CALL h5tclose_f(attr6_type, error)
     CALL check("h5tclose_f",error,total_error)

     !
     ! End access to the dataset and release resources used by it.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f",error,total_error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)
     !
     ! Remove the file
     !
     IF (cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)

     RETURN
     END SUBROUTINE attribute_test
