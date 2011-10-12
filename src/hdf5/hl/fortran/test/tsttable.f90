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

PROGRAM table_test

  CALL test_table1()


END PROGRAM table_test


!-------------------------------------------------------------------------
! test_table1
!-------------------------------------------------------------------------

SUBROUTINE test_table1()

  USE H5TB ! module of H5TB
  USE HDF5 ! module of HDF5 library

  IMPLICIT NONE

  CHARACTER(len=8), PARAMETER :: filename = "f1tab.h5"   ! File name
  CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"     ! Dataset name
  INTEGER(HID_T) :: file_id                              ! File identifier
  INTEGER(HSIZE_T), PARAMETER :: nfields  = 4;           ! nfields
  INTEGER(HSIZE_T), PARAMETER :: nrecords = 5;           ! nrecords
  CHARACTER(LEN=10),DIMENSION(1:nfields) :: field_names  ! field names
  INTEGER(SIZE_T),  DIMENSION(1:nfields) :: field_offset ! field offset
  INTEGER(HID_T),   DIMENSION(1:nfields) :: field_types  ! field types
  INTEGER(HSIZE_T), PARAMETER  :: chunk_size = 5         ! chunk size
  INTEGER, PARAMETER :: compress = 0                     ! compress
  INTEGER            :: errcode                          ! Error flag
  INTEGER            :: i                                ! general purpose integer
  INTEGER(SIZE_T)    :: type_size                        ! Size of the datatype
  INTEGER(SIZE_T)    :: type_sizec                       ! Size of the character datatype
  INTEGER(SIZE_T)    :: type_sizei                       ! Size of the integer datatype
  INTEGER(SIZE_T)    :: type_sized                       ! Size of the double precision datatype
  INTEGER(SIZE_T)    :: type_sizer                       ! Size of the real datatype
  INTEGER(HID_T)     :: type_id_c                        ! Memory datatype identifier (for character field)
  INTEGER(SIZE_T)    :: offset                           ! Member's offset
  INTEGER(HSIZE_T)   :: start = 0                        ! start record
  INTEGER, DIMENSION(nrecords) :: bufi                   ! Data buffer
  INTEGER, DIMENSION(nrecords) :: bufir                  ! Data buffer
  REAL, DIMENSION(nrecords) :: bufr                      ! Data buffer
  REAL, DIMENSION(nrecords) :: bufrr                     ! Data buffer
  DOUBLE PRECISION, DIMENSION(nrecords) :: bufd          ! Data buffer
  DOUBLE PRECISION, DIMENSION(nrecords) :: bufdr         ! Data buffer
  CHARACTER(LEN=2), DIMENSION(nrecords), PARAMETER :: bufs = (/"AB","CD","EF","GH","IJ"/) ! Data buffer
  CHARACTER(LEN=2), DIMENSION(nrecords) :: bufsr         ! Data buffer
  INTEGER(HSIZE_T) :: nfieldsr                           ! nfields
  INTEGER(HSIZE_T) :: nrecordsr                          ! nrecords
  CHARACTER(LEN=9), DIMENSION(1:nfields) :: field_namesr  ! field names
  INTEGER(SIZE_T),  DIMENSION(1:nfields) :: field_offsetr ! field offset
  INTEGER(SIZE_T),  DIMENSION(1:nfields) :: field_sizesr  ! field sizes
  INTEGER(SIZE_T)  :: type_sizeout                        ! size of the datatype
  INTEGER :: maxlen                                       ! max chararter length of a field name


  !
  ! Initialize the data arrays.
  !
  DO i = 1, nrecords
     bufi(i) = i
     bufr(i) = i
     bufd(i) = i
  END DO

  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(errcode)

  !
  ! Create a new file using default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)


  !-------------------------------------------------------------------------
  ! make table
  ! initialize the table parameters
  !-------------------------------------------------------------------------

  field_names(1) = "field1"
  field_names(2) = "field2a"
  field_names(3) = "field3ab"
  field_names(4) = "field4abc"

  !
  ! calculate total size by calculating sizes of each member
  !
  CALL h5tcopy_f(H5T_NATIVE_CHARACTER, type_id_c, errcode)
  type_size = 2
  CALL h5tset_size_f(type_id_c, type_size, errcode)
  CALL h5tget_size_f(type_id_c, type_sizec, errcode)
  CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, errcode)
  CALL h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, errcode)
  CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, errcode)
  type_size = type_sizec + type_sizei + type_sized + type_sizer

  !
  ! type ID's
  !
  field_types(1) = type_id_c
  field_types(2) = H5T_NATIVE_INTEGER
  field_types(3) = H5T_NATIVE_DOUBLE
  field_types(4) = H5T_NATIVE_REAL

  !
  ! offsets
  !
  offset = 0
  field_offset(1) = offset
  offset = offset + type_sizec ! Offset of the second memeber is 2
  field_offset(2) = offset
  offset = offset + type_sizei ! Offset of the second memeber is 6
  field_offset(3) = offset
  offset = offset + type_sized ! Offset of the second memeber is 14
  field_offset(4) = offset

  !-------------------------------------------------------------------------
  ! make table
  !-------------------------------------------------------------------------

  CALL test_begin(' Make table                     ')

  CALL h5tbmake_table_f(dsetname1,&
       file_id,&
       dsetname1,&
       nfields,&
       nrecords,&
       type_size,&
       field_names,&
       field_offset,&
       field_types,&
       chunk_size,&
       compress,&
       errcode )

  CALL passed()


  !-------------------------------------------------------------------------
  ! write field
  !-------------------------------------------------------------------------

  CALL test_begin(' Read/Write field by name       ')

  CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(1),start,nrecords,type_sizec,&
       bufs,errcode)

  CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(2),start,nrecords,type_sizei,&
       bufi,errcode)

  CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
       bufd,errcode)

  CALL h5tbwrite_field_name_f(file_id,dsetname1,field_names(4),start,nrecords,type_sizer,&
       bufr,errcode)


  !-------------------------------------------------------------------------
  ! read field
  !-------------------------------------------------------------------------

  CALL h5tbread_field_name_f(file_id,dsetname1,field_names(1),start,nrecords,type_sizec,&
       bufsr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufsr(i) .NE. bufs(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufsr(i), ' and ',   bufs(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_name_f(file_id,dsetname1,field_names(2),start,nrecords,type_sizei,&
       bufir,errcode)


  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufir(i) .NE. bufi(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufir(i), ' and ',   bufi(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_name_f(file_id,dsetname1,field_names(3),start,nrecords,type_sized,&
       bufdr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufdr(i) .NE. bufd(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufdr(i), ' and ',   bufd(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_name_f(file_id,dsetname1,field_names(4),start,nrecords,type_sizer,&
       bufrr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufrr(i) .NE. bufr(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufrr(i), ' and ',   bufr(i)
        STOP
     ENDIF
  END DO


  CALL passed()

  !-------------------------------------------------------------------------
  ! write field
  !-------------------------------------------------------------------------

  CALL test_begin(' Read/Write field by index      ')

  CALL h5tbwrite_field_index_f(file_id,dsetname1,1,start,nrecords,type_sizec,&
       bufs,errcode)

  CALL h5tbwrite_field_index_f(file_id,dsetname1,2,start,nrecords,type_sizei,&
       bufi,errcode)

  CALL h5tbwrite_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
       bufd,errcode)

  CALL h5tbwrite_field_index_f(file_id,dsetname1,4,start,nrecords,type_sizer,&
       bufr,errcode)



  !-------------------------------------------------------------------------
  ! read field
  !-------------------------------------------------------------------------

  CALL h5tbread_field_index_f(file_id,dsetname1,1,start,nrecords,type_sizec,&
       bufsr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufsr(i) .NE. bufs(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufsr(i), ' and ',   bufs(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_index_f(file_id,dsetname1,2,start,nrecords,type_sizei,&
       bufir,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufir(i) .NE. bufi(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufir(i), ' and ',   bufi(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_index_f(file_id,dsetname1,3,start,nrecords,type_sized,&
       bufdr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufdr(i) .NE. bufd(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufdr(i), ' and ',   bufd(i)
        STOP
     ENDIF
  END DO

  CALL h5tbread_field_index_f(file_id,dsetname1,4,start,nrecords,type_sizer,&
       bufrr,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufrr(i) .NE. bufr(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufrr(i), ' and ',   bufr(i)
        STOP
     ENDIF
  END DO


  CALL passed()


  !-------------------------------------------------------------------------
  ! Insert field
  ! we insert a field callsed "field5" with the same type and buffer as field 4 (Real)
  !-------------------------------------------------------------------------



  CALL test_begin(' Insert field                   ')

  CALL h5tbinsert_field_f(file_id,dsetname1,"field5",field_types(4),4,bufr,errcode)
  CALL h5tbread_field_index_f(file_id,dsetname1,5,start,nrecords,type_sizer,&
       bufrr,errcode)
  !
  ! compare read and write buffers.
  !
  DO i = 1, nrecords
     IF ( bufrr(i) .NE. bufr(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufrr(i), ' and ',   bufr(i)
        STOP
     ENDIF
  END DO


  CALL passed()

  !-------------------------------------------------------------------------
  ! Delete field
  !-------------------------------------------------------------------------

  CALL test_begin(' Delete field                   ')

  CALL h5tbdelete_field_f(file_id,dsetname1,"field4abc",errcode)

  CALL passed()


  !-------------------------------------------------------------------------
  ! Gets the number of records and fields
  !-------------------------------------------------------------------------

  CALL test_begin(' Get table info                 ')

  CALL h5tbget_table_info_f(file_id,dsetname1,nfieldsr,nrecordsr,errcode )

  IF ( nfieldsr .NE. nfields .AND. nrecordsr .NE. nrecords ) THEN
     PRINT *, 'h5tbget_table_info_f return error'
     STOP
  ENDIF

  CALL passed()


  !-------------------------------------------------------------------------
  ! Get information about fields
  !-------------------------------------------------------------------------

  CALL test_begin(' Get fields info                ')

  CALL h5tbget_field_info_f(file_id, dsetname1, nfields, field_namesr, field_sizesr,&
       field_offsetr, type_sizeout, errcode, maxlen )

  IF ( errcode.NE.0 ) THEN
     WRITE(*,'(/,5X,"H5TBGET_FIELD_INFO_F: RETURN ERROR")')
     STOP
  ENDIF

  ! "field4abc" was deleted and "field5" was added.
  field_names(4) = "field5"

  IF ( maxlen .NE. 8 ) THEN
     WRITE(*,'(/,5X,"H5TBGET_FIELD_INFO_F: INCORRECT MAXIMUM CHARACTER LENGTH OF THE FIELD NAMES")')
     WRITE(*,'(5X,"RETURNED VALUE = ", I0, ", CORRECT VALUE = ", I0)') maxlen, 8 
     STOP
  ENDIF

  DO i = 1,  nfields
     IF ( field_namesr(i) .NE. field_names(i)) THEN
        WRITE(*,'(/,5X,"H5TBGET_FIELD_INFO_F: READ/WRITE FIELD NAMES DIFFER")')
        WRITE(*,'(27X,A," AND ",A)') TRIM(field_namesr(i)), TRIM(field_names(i))
        STOP
     ENDIF
  END DO

  CALL passed()


  !-------------------------------------------------------------------------
  ! end
  !-------------------------------------------------------------------------

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, errcode)

  !
  ! Close FORTRAN predefined datatypes.
  !
  CALL h5close_f(errcode)

  !
  ! end function.
  !
END SUBROUTINE test_table1


!-------------------------------------------------------------------------
! test_begin
!-------------------------------------------------------------------------

SUBROUTINE test_begin(string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  WRITE(*, fmt = '(14a)', advance = 'no') string
  WRITE(*, fmt = '(40x,a)', advance = 'no') ' '
END SUBROUTINE test_begin

!-------------------------------------------------------------------------
! passed
!-------------------------------------------------------------------------

SUBROUTINE passed()
  WRITE(*, fmt = '(6a)')  'PASSED'
END SUBROUTINE passed


