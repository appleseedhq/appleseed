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
! This file contains FORTRAN90 interfaces for H5TB functions
!

MODULE h5tb
USE h5fortran_types
USE hdf5


INTERFACE h5tbwrite_field_name_f
 MODULE PROCEDURE h5tbwrite_field_name_f_int
 MODULE PROCEDURE h5tbwrite_field_name_f_float
 MODULE PROCEDURE h5tbwrite_field_name_f_double
 MODULE PROCEDURE h5tbwrite_field_name_f_string
END INTERFACE

INTERFACE h5tbread_field_name_f
 MODULE PROCEDURE h5tbread_field_name_f_int
 MODULE PROCEDURE h5tbread_field_name_f_float
 MODULE PROCEDURE h5tbread_field_name_f_double
 MODULE PROCEDURE h5tbread_field_name_f_string
END INTERFACE

INTERFACE h5tbwrite_field_index_f
 MODULE PROCEDURE h5tbwrite_field_index_f_int
 MODULE PROCEDURE h5tbwrite_field_index_f_float
 MODULE PROCEDURE h5tbwrite_field_index_f_double
 MODULE PROCEDURE h5tbwrite_field_index_f_string
END INTERFACE

INTERFACE h5tbread_field_index_f
 MODULE PROCEDURE h5tbread_field_index_f_int
 MODULE PROCEDURE h5tbread_field_index_f_float
 MODULE PROCEDURE h5tbread_field_index_f_double
 MODULE PROCEDURE h5tbread_field_index_f_string
END INTERFACE


INTERFACE h5tbinsert_field_f
 MODULE PROCEDURE h5tbinsert_field_f_int
 MODULE PROCEDURE h5tbinsert_field_f_float
 MODULE PROCEDURE h5tbinsert_field_f_double
 MODULE PROCEDURE h5tbinsert_field_f_string
END INTERFACE


CONTAINS


!-------------------------------------------------------------------------
! Function: h5tbmake_table_f
!
! Purpose: Make a table
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbmake_table_f(table_title,&
                            loc_id,&
                            dset_name,&
                            nfields,&
                            nrecords,&
                            type_size,&
                            field_names,&
                            field_offset,&
                            field_types,&
                            chunk_size,&
                            compress,&
                            errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbmake_table_f
!DEC$endif
!

 CHARACTER(LEN=*), INTENT(in) :: table_title                      ! name of the dataset
 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER(hsize_t), INTENT(in) :: nfields                          ! fields
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 CHARACTER(LEN=*), DIMENSION(1:nfields), INTENT(in) :: field_names  ! field names
 INTEGER(size_t),  DIMENSION(1:nfields), INTENT(in) :: field_offset ! field offset
 INTEGER(hid_t),   DIMENSION(1:nfields), INTENT(in) :: field_types  ! field types
 INTEGER(hsize_t), INTENT(in) :: chunk_size                       ! chunk size
 INTEGER,          INTENT(in) :: compress                         ! compress
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length
 INTEGER :: errcode                                               ! error code
 INTEGER, DIMENSION(1:nfields) :: char_len_field_names            ! field name lengths
 INTEGER :: max_char_size_field_names                             ! character len of field names
 INTEGER :: i                                                     ! general purpose integer


 INTERFACE
    INTEGER FUNCTION h5tbmake_table_c(namelen1,&
         table_title,&
         loc_id,&
         namelen,&
         dset_name,&
         nfields,&
         nrecords,&
         type_size,&
         field_offset,&
         field_types,&
         chunk_size,&
         compress,&
         char_len_field_names,&
         max_char_size_field_names,&
         field_names)

      USE h5global
      !DEC$IF DEFINED(HDF5F90_WINDOWS)
      !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBMAKE_TABLE_C'::h5tbmake_table_c
      !DEC$ENDIF
      !DEC$ATTRIBUTES reference :: dset_name
      !DEC$ATTRIBUTES reference :: table_title
      CHARACTER(LEN=*), INTENT(in) :: table_title                      ! name of the dataset
      INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
      CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
      INTEGER(hsize_t), INTENT(in) :: nfields                          ! fields
      INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
      INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
      CHARACTER(LEN=*), DIMENSION(nfields), INTENT(in) :: field_names  ! field names
      INTEGER(size_t),  DIMENSION(nfields), INTENT(in) :: field_offset ! field offset
      INTEGER(hid_t),   DIMENSION(nfields), INTENT(in) :: field_types  ! field types
      INTEGER(hsize_t), INTENT(in) :: chunk_size                       ! chunk size
      INTEGER,          INTENT(in) :: compress                         ! compress
      INTEGER :: namelen                                               ! name length
      INTEGER :: namelen1                                              ! name length
      INTEGER, DIMENSION(nfields) :: char_len_field_names              ! field name's lengths
      INTEGER :: max_char_size                                         ! character len of field names
    END FUNCTION h5tbmake_table_c
 END INTERFACE
 
 namelen  = LEN(dset_name)
 namelen1 = LEN(table_title)
 
 ! Find the size of each character string in the array
 DO i = 1, nfields
    char_len_field_names(i) = LEN_TRIM(field_names(i))
 END DO

 max_char_size_field_names = LEN(field_names(1)) 

 errcode = h5tbmake_table_c(namelen1,&
  table_title,&
  loc_id,&
  namelen,&
  dset_name,&
  nfields,&
  nrecords,&
  type_size,&
  field_offset,&
  field_types,&
  chunk_size,&
  compress,&
  char_len_field_names, &
  max_char_size_field_names, &
  field_names)

END SUBROUTINE h5tbmake_table_f


!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_int
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_name_f_int(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_f_int
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_name_int_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_NAME_INT_C'::h5tbwrite_field_name_int_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbwrite_field_name_int_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbwrite_field_name_int_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_name_f_int

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_float
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_name_f_float(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_f_float
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_name_fl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_NAME_FL_C'::h5tbwrite_field_name_fl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbwrite_field_name_fl_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbwrite_field_name_fl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_name_f_float



!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_double
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_name_f_double(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_f_double
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_name_dl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_NAME_DL_C'::h5tbwrite_field_name_dl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbwrite_field_name_dl_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbwrite_field_name_dl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_name_f_double

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_name_f_string
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_name_f_string(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_name_f_string
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_name_st_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_NAME_ST_C'::h5tbwrite_field_name_st_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbwrite_field_name_st_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbwrite_field_name_st_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_name_f_string


!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_int
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_name_f_int(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_f_int
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_name_int_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_NAME_INT_C'::h5tbread_field_name_int_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbread_field_name_int_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbread_field_name_int_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_name_f_int

!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_float
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_name_f_float(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_f_float
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_name_fl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_NAME_FL_C'::h5tbread_field_name_fl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbread_field_name_fl_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbread_field_name_fl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_name_f_float

!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_double
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_name_f_double(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_f_double
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_name_dl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_NAME_DL_C'::h5tbread_field_name_dl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbread_field_name_dl_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbread_field_name_dl_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_name_f_double

!-------------------------------------------------------------------------
! Function: h5tbread_field_name_f_string
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_name_f_string(loc_id,&
                                      dset_name,&
                                      field_name,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbread_field_name_f_string
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_name_st_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_NAME_ST_C'::h5tbread_field_name_st_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length
  END FUNCTION h5tbread_field_name_st_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbread_field_name_st_c(loc_id,namelen,dset_name,namelen1,field_name,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_name_f_string


!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_int
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_index_f_int(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_f_int
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_index_int_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_INDEX_INT_C'::h5tbwrite_field_index_int_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbwrite_field_index_int_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbwrite_field_index_int_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_index_f_int

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_float
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_index_f_float(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_f_float
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_index_fl_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_INDEX_FL_C'::h5tbwrite_field_index_fl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbwrite_field_index_fl_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbwrite_field_index_fl_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_index_f_float



!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_double
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_index_f_double(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_f_double
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_index_dl_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_INDEX_DL_C'::h5tbwrite_field_index_dl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbwrite_field_index_dl_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbwrite_field_index_dl_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_index_f_double

!-------------------------------------------------------------------------
! Function: h5tbwrite_field_index_f_string
!
! Purpose: Writes one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbwrite_field_index_f_string(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbwrite_field_index_f_string
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbwrite_field_index_st_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBWRITE_FIELD_INDEX_ST_C'::h5tbwrite_field_index_st_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbwrite_field_index_st_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbwrite_field_index_st_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbwrite_field_index_f_string


!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_int
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_index_f_int(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport ::h5tbread_field_index_f_int
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_index_int_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_INDEX_INT_C'::h5tbread_field_index_int_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbread_field_index_int_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbread_field_index_int_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_index_f_int

!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_float
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_index_f_float(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbread_field_index_f_float
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_index_fl_c(loc_id,namelen,dset_name,field_index,&
   start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_INDEX_FL_C'::h5tbread_field_index_fl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbread_field_index_fl_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbread_field_index_fl_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_index_f_float

!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_double
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_index_f_double(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbread_field_index_f_double
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_index_dl_c(loc_id,namelen,dset_name,field_index,&
   start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_INDEX_DL_C'::h5tbread_field_index_dl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbread_field_index_dl_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbread_field_index_dl_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_index_f_double

!-------------------------------------------------------------------------
! Function: h5tbread_field_index_f_string
!
! Purpose: Reads one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 12, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbread_field_index_f_string(loc_id,&
                                      dset_name,&
                                      field_index,&
                                      start,&
                                      nrecords,&
                                      type_size,&
                                      buf,&
                                      errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbread_field_index_f_string
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 INTEGER, INTENT(in) :: field_index                               ! index
 INTEGER(hsize_t), INTENT(in) :: start                            ! start record
 INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
 INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
 CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: errcode                                               ! error code
 INTEGER :: namelen                                               ! name length

 INTERFACE
  INTEGER FUNCTION h5tbread_field_index_st_c(loc_id,namelen,dset_name,field_index,&
   start,nrecords,type_size,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBREAD_FIELD_INDEX_ST_C'::h5tbread_field_index_st_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  INTEGER, INTENT(in) :: field_index                               ! index
  INTEGER(hsize_t), INTENT(in) :: start                            ! start record
  INTEGER(hsize_t), INTENT(in) :: nrecords                         ! records
  INTEGER(size_t),  INTENT(in) :: type_size                        ! type size
  CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: errcode                                               ! error code
  INTEGER :: namelen                                               ! name length
  END FUNCTION h5tbread_field_index_st_c
 END INTERFACE

 namelen  = LEN(dset_name)

 errcode = h5tbread_field_index_st_c(loc_id,namelen,dset_name,field_index,&
  start,nrecords,type_size,buf)

END SUBROUTINE h5tbread_field_index_f_string


!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_int
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbinsert_field_f_int(loc_id,&
                                  dset_name,&
                                  field_name,&
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode )
 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_f_int
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
 INTEGER, INTENT(in) :: field_index                               ! field_index
 INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length
 INTEGER :: errcode                                               ! error code


 INTERFACE
  INTEGER FUNCTION h5tbinsert_field_int_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBINSERT_FIELD_INT_C'::h5tbinsert_field_int_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
  INTEGER, INTENT(in) :: field_index                               ! field_index
  INTEGER, INTENT(in), DIMENSION(*) :: buf                         ! data buffer
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length length
  END FUNCTION h5tbinsert_field_int_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbinsert_field_int_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

END SUBROUTINE h5tbinsert_field_f_int



!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_float
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbinsert_field_f_float(loc_id,&
                                  dset_name,&
                                  field_name,&
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode )
 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_f_float
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
 INTEGER, INTENT(in) :: field_index                               ! field_index
 REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length
 INTEGER :: errcode                                               ! error code


 INTERFACE
  INTEGER FUNCTION h5tbinsert_field_fl_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBINSERT_FIELD_FL_C'::h5tbinsert_field_fl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
  INTEGER, INTENT(in) :: field_index                               ! field_index
  REAL, INTENT(in), DIMENSION(*) :: buf                            ! data buffer
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length length
  END FUNCTION h5tbinsert_field_fl_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbinsert_field_fl_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

END SUBROUTINE h5tbinsert_field_f_float



!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_double
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbinsert_field_f_double(loc_id,&
                                  dset_name,&
                                  field_name,&
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode )
 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_f_double
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
 INTEGER, INTENT(in) :: field_index                               ! field_index
 DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length
 INTEGER :: errcode                                               ! error code


 INTERFACE
  INTEGER FUNCTION h5tbinsert_field_dl_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBINSERT_FIELD_DL_C'::h5tbinsert_field_dl_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
  INTEGER, INTENT(in) :: field_index                               ! field_index
  DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length length
  END FUNCTION h5tbinsert_field_dl_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbinsert_field_dl_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

END SUBROUTINE h5tbinsert_field_f_double




!-------------------------------------------------------------------------
! Function: h5tbinsert_field_f_string
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbinsert_field_f_string(loc_id,&
                                  dset_name,&
                                  field_name,&
                                  field_type,&
                                  field_index,&
                                  buf,&
                                  errcode )
 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbinsert_field_f_string
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
 INTEGER, INTENT(in) :: field_index                               ! field_index
 CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length
 INTEGER :: errcode                                               ! error code


 INTERFACE
  INTEGER FUNCTION h5tbinsert_field_st_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBINSERT_FIELD_ST_C'::h5tbinsert_field_st_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
  INTEGER(hid_t), INTENT(in)   :: field_type                       ! field type
  INTEGER, INTENT(in) :: field_index                               ! field_index
  CHARACTER(LEN=*), INTENT(in), DIMENSION(*) :: buf                ! data buffer
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length length
  END FUNCTION h5tbinsert_field_st_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbinsert_field_st_c(loc_id,namelen,dset_name,namelen1,field_name,&
   field_type,field_index,buf)

END SUBROUTINE h5tbinsert_field_f_string




!-------------------------------------------------------------------------
! Function: h5tbdelete_field_f
!
! Purpose: Inserts one field
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbdelete_field_f(loc_id,&
                              dset_name,&
                              field_name,&
                              errcode )
 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbdelete_field_f
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id                           ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name                        ! name of the dataset
 CHARACTER(LEN=*), INTENT(in) :: field_name                       ! name of the field
 INTEGER :: namelen                                               ! name length
 INTEGER :: namelen1                                              ! name length
 INTEGER :: errcode                                               ! error code


 INTERFACE
  INTEGER FUNCTION h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name)

  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBDELETE_FIELD_C'::h5tbdelete_field_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: field_name
  INTEGER(HID_T),   INTENT(IN) :: loc_id                           ! file or group identifier
  CHARACTER(LEN=*), INTENT(IN) :: dset_name                        ! name of the dataset
  CHARACTER(LEN=*), INTENT(IN) :: field_name                       ! name of the field
  INTEGER :: namelen                                               ! name length
  INTEGER :: namelen1                                              ! name length length
  END FUNCTION h5tbdelete_field_c
 END INTERFACE

 namelen  = LEN(dset_name)
 namelen1 = LEN(field_name)

 errcode = h5tbdelete_field_c(loc_id,namelen,dset_name,namelen1,field_name)

END SUBROUTINE h5tbdelete_field_f



!-------------------------------------------------------------------------
! Function: h5tbget_table_info_f
!
! Purpose: Gets the number of records and fields of a table
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbget_table_info_f(loc_id,&
                                dset_name,&
                                nfields,&
                                nrecords,&
                                errcode )

 IMPLICIT NONE

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbget_table_info_f
!DEC$endif
!

 INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
 CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
 INTEGER(hsize_t), INTENT(inout):: nfields          ! nfields
 INTEGER(hsize_t), INTENT(inout):: nrecords         ! nrecords
 INTEGER :: errcode                                 ! error code
 INTEGER :: namelen                                 ! name length

 INTERFACE
  INTEGER FUNCTION h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords)
  USE h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBGET_TABLE_INFO_C'::h5tbget_table_info_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name          ! name of the dataset
  INTEGER(hsize_t), INTENT(inout):: nfields          ! nfields
  INTEGER(hsize_t), INTENT(inout):: nrecords         ! nrecords
  INTEGER :: namelen                                 ! name length
  END FUNCTION h5tbget_table_info_c
 END INTERFACE

 namelen = LEN(dset_name)
 errcode = h5tbget_table_info_c(loc_id,namelen,dset_name,nfields,nrecords)

END SUBROUTINE h5tbget_table_info_f


!-------------------------------------------------------------------------
! Function: h5tbget_field_info_f
!
! Purpose: Get information about fields
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 13, 2004
!
! Comments:
!
! Modifications: 
!  Added optional parameter for returning the maximum character lenght
!  in the field name array. March 3, 2011 
!
!-------------------------------------------------------------------------

SUBROUTINE h5tbget_field_info_f(loc_id,&
                                dset_name,&
                                nfields,&
                                field_names,&
				field_sizes,&
				field_offsets,&
				type_size,&
                                errcode, maxlen_out )

  IMPLICIT NONE
!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5tbget_field_info_f
!DEC$endif
!
  INTEGER(hid_t),   INTENT(in) :: loc_id                                ! file or group identifier
  CHARACTER(LEN=*), INTENT(in) :: dset_name                             ! name of the dataset
  INTEGER(hsize_t), INTENT(in) :: nfields                               ! nfields
  CHARACTER(LEN=*), DIMENSION(nfields), INTENT(inout) :: field_names    ! field names
  INTEGER(size_t),  DIMENSION(nfields), INTENT(inout) :: field_sizes    ! field sizes
  INTEGER(size_t),  DIMENSION(nfields), INTENT(inout) :: field_offsets  ! field offsets
  INTEGER(size_t),  INTENT(inout):: type_size                           ! type size
  INTEGER :: errcode                                                    ! error code
  INTEGER, OPTIONAL :: maxlen_out                                       ! maximum character len of the field names
  INTEGER :: namelen                                                    ! name length
  INTEGER, DIMENSION(nfields) :: namelen2                               ! name lengths
  INTEGER :: i                                                          ! general purpose integer
  INTEGER :: maxlen
  INTEGER :: c_maxlen_out

  INTERFACE
     INTEGER FUNCTION h5tbget_field_info_c(loc_id,namelen,dset_name,nfields,&
          field_sizes,field_offsets,type_size,namelen2, maxlen, field_names, c_maxlen_out)

       USE h5global
       !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TBGET_FIELD_INFO_C'::h5tbget_field_info_c
       !DEC$ENDIF
       !DEC$ATTRIBUTES reference :: dset_name
       INTEGER(hid_t),   INTENT(in) :: loc_id                                 ! file or group identifier
       CHARACTER(LEN=*), INTENT(in) :: dset_name                              ! name of the dataset
       INTEGER(hsize_t), INTENT(in):: nfields                                 ! nfields
       CHARACTER(LEN=*), DIMENSION(1:nfields), INTENT(inout) :: field_names   ! field names
       INTEGER(size_t),  DIMENSION(1:nfields), INTENT(inout) :: field_sizes   ! field sizes
       INTEGER(size_t),  DIMENSION(1:nfields), INTENT(inout) :: field_offsets ! field offsets
       INTEGER(size_t),  INTENT(inout):: type_size                            ! type size
       INTEGER :: namelen                                                     ! name length
       INTEGER :: maxlen                                                      ! maxiumum length of input field names
       INTEGER, DIMENSION(1:nfields) :: namelen2                              ! name lengths
       INTEGER :: c_maxlen_out                  ! maximum character length of a field array element 
     END FUNCTION h5tbget_field_info_c
  END INTERFACE

  namelen = LEN(dset_name)
  DO i = 1, nfields
     namelen2(i) = LEN_TRIM(field_names(i))
  END DO
  maxlen = LEN(field_names(1))
  c_maxlen_out = 0

  errcode = h5tbget_field_info_c(loc_id, namelen,dset_name, nfields, &
       field_sizes, field_offsets, type_size, namelen2, maxlen, field_names, c_maxlen_out)

  IF(PRESENT(maxlen_out)) maxlen_out = c_maxlen_out

END SUBROUTINE h5tbget_field_info_f

END MODULE H5TB






