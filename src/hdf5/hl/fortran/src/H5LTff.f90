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
! This file contains FORTRAN90 interfaces for H5LT functions
!

MODULE h5lt
  USE h5fortran_types
  USE hdf5

  INTERFACE h5ltmake_dataset_f
     MODULE PROCEDURE h5ltmake_dataset_f_int1
     MODULE PROCEDURE h5ltmake_dataset_f_int2
     MODULE PROCEDURE h5ltmake_dataset_f_int3
     MODULE PROCEDURE h5ltmake_dataset_f_int4
     MODULE PROCEDURE h5ltmake_dataset_f_int5
     MODULE PROCEDURE h5ltmake_dataset_f_int6
     MODULE PROCEDURE h5ltmake_dataset_f_int7
     MODULE PROCEDURE h5ltmake_dataset_f_float1
     MODULE PROCEDURE h5ltmake_dataset_f_float2
     MODULE PROCEDURE h5ltmake_dataset_f_float3
     MODULE PROCEDURE h5ltmake_dataset_f_float4
     MODULE PROCEDURE h5ltmake_dataset_f_float5
     MODULE PROCEDURE h5ltmake_dataset_f_float6
     MODULE PROCEDURE h5ltmake_dataset_f_float7
     MODULE PROCEDURE h5ltmake_dataset_f_double1
     MODULE PROCEDURE h5ltmake_dataset_f_double2
     MODULE PROCEDURE h5ltmake_dataset_f_double3
     MODULE PROCEDURE h5ltmake_dataset_f_double4
     MODULE PROCEDURE h5ltmake_dataset_f_double5
     MODULE PROCEDURE h5ltmake_dataset_f_double6
     MODULE PROCEDURE h5ltmake_dataset_f_double7
  END INTERFACE

  INTERFACE h5ltread_dataset_f
     MODULE PROCEDURE h5ltread_dataset_f_int1
     MODULE PROCEDURE h5ltread_dataset_f_int2
     MODULE PROCEDURE h5ltread_dataset_f_int3
     MODULE PROCEDURE h5ltread_dataset_f_int4
     MODULE PROCEDURE h5ltread_dataset_f_int5
     MODULE PROCEDURE h5ltread_dataset_f_int6
     MODULE PROCEDURE h5ltread_dataset_f_int7
     MODULE PROCEDURE h5ltread_dataset_f_float1
     MODULE PROCEDURE h5ltread_dataset_f_float2
     MODULE PROCEDURE h5ltread_dataset_f_float3
     MODULE PROCEDURE h5ltread_dataset_f_float4
     MODULE PROCEDURE h5ltread_dataset_f_float5
     MODULE PROCEDURE h5ltread_dataset_f_float6
     MODULE PROCEDURE h5ltread_dataset_f_float7
     MODULE PROCEDURE h5ltread_dataset_f_double1
     MODULE PROCEDURE h5ltread_dataset_f_double2
     MODULE PROCEDURE h5ltread_dataset_f_double3
     MODULE PROCEDURE h5ltread_dataset_f_double4
     MODULE PROCEDURE h5ltread_dataset_f_double5
     MODULE PROCEDURE h5ltread_dataset_f_double6
     MODULE PROCEDURE h5ltread_dataset_f_double7
  END INTERFACE

  INTERFACE h5ltmake_dataset_int_f
     MODULE PROCEDURE h5ltmake_dataset_int_f_1
     MODULE PROCEDURE h5ltmake_dataset_int_f_2
     MODULE PROCEDURE h5ltmake_dataset_int_f_3
     MODULE PROCEDURE h5ltmake_dataset_int_f_4
     MODULE PROCEDURE h5ltmake_dataset_int_f_5
     MODULE PROCEDURE h5ltmake_dataset_int_f_6
     MODULE PROCEDURE h5ltmake_dataset_int_f_7
  END INTERFACE

  INTERFACE h5ltmake_dataset_float_f
     MODULE PROCEDURE h5ltmake_dataset_float_f_1
     MODULE PROCEDURE h5ltmake_dataset_float_f_2
     MODULE PROCEDURE h5ltmake_dataset_float_f_3
     MODULE PROCEDURE h5ltmake_dataset_float_f_4
     MODULE PROCEDURE h5ltmake_dataset_float_f_5
     MODULE PROCEDURE h5ltmake_dataset_float_f_6
     MODULE PROCEDURE h5ltmake_dataset_float_f_7
  END INTERFACE

  INTERFACE h5ltmake_dataset_double_f
     MODULE PROCEDURE h5ltmake_dataset_double_f_1
     MODULE PROCEDURE h5ltmake_dataset_double_f_2
     MODULE PROCEDURE h5ltmake_dataset_double_f_3
     MODULE PROCEDURE h5ltmake_dataset_double_f_4
     MODULE PROCEDURE h5ltmake_dataset_double_f_5
     MODULE PROCEDURE h5ltmake_dataset_double_f_6
     MODULE PROCEDURE h5ltmake_dataset_double_f_7
  END INTERFACE

  INTERFACE h5ltread_dataset_int_f
     MODULE PROCEDURE h5ltread_dataset_int_f_1
     MODULE PROCEDURE h5ltread_dataset_int_f_2
     MODULE PROCEDURE h5ltread_dataset_int_f_3
     MODULE PROCEDURE h5ltread_dataset_int_f_4
     MODULE PROCEDURE h5ltread_dataset_int_f_5
     MODULE PROCEDURE h5ltread_dataset_int_f_6
     MODULE PROCEDURE h5ltread_dataset_int_f_7
  END INTERFACE

  INTERFACE h5ltread_dataset_float_f
     MODULE PROCEDURE h5ltread_dataset_float_f_1
     MODULE PROCEDURE h5ltread_dataset_float_f_2
     MODULE PROCEDURE h5ltread_dataset_float_f_3
     MODULE PROCEDURE h5ltread_dataset_float_f_4
     MODULE PROCEDURE h5ltread_dataset_float_f_5
     MODULE PROCEDURE h5ltread_dataset_float_f_6
     MODULE PROCEDURE h5ltread_dataset_float_f_7
  END INTERFACE

  INTERFACE h5ltread_dataset_double_f
     MODULE PROCEDURE h5ltread_dataset_double_f_1
     MODULE PROCEDURE h5ltread_dataset_double_f_2
     MODULE PROCEDURE h5ltread_dataset_double_f_3
     MODULE PROCEDURE h5ltread_dataset_double_f_4
     MODULE PROCEDURE h5ltread_dataset_double_f_5
     MODULE PROCEDURE h5ltread_dataset_double_f_6
     MODULE PROCEDURE h5ltread_dataset_double_f_7
  END INTERFACE

CONTAINS
  !-------------------------------------------------------------------------
  ! Make/Read dataset functions
  !-------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_int1
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int1(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_int1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER, INTENT(in), DIMENSION(*) :: buf           ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_int1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_INT1_C'::h5ltmake_dataset_int1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), DIMENSION(*) :: buf                ! data buffer
       END FUNCTION h5ltmake_dataset_int1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_int1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_int1

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_int2
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_int2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_int2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_INT2_C'::h5ltmake_dataset_int2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltmake_dataset_int2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_int2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_int2

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_int3
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int3(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_int3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_int3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_INT3_C'::h5ltmake_dataset_int3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_int3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_int3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_int3

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_int4
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int4(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_int4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_int4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_INT4_C'::h5ltmake_dataset_int4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf      ! data buffer
       END FUNCTION h5ltmake_dataset_int4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_int4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_int4

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_int5
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int5(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_int5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_int5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_INT5_C'::h5ltmake_dataset_int5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf      ! data buffer
       END FUNCTION h5ltmake_dataset_int5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_int5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_int5

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_int6
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int6(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_int6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_int6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_INT6_C'::h5ltmake_dataset_int6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf      ! data buffer
       END FUNCTION h5ltmake_dataset_int6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_int6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_int6

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_int7
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_int7(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )
    
    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_int7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_int7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_INT7_C'::h5ltmake_dataset_int7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf      ! data buffer
       END FUNCTION h5ltmake_dataset_int7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_int7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_int7


  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_float1
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_float1(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_float1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    REAL, INTENT(in), DIMENSION(*) :: buf              ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_fl1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_FL1_C'::h5ltmake_dataset_fl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), DIMENSION(*) :: buf                   ! data buffer
       END FUNCTION h5ltmake_dataset_fl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_fl1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_float1

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_float2
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_float2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_float2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_fl2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_FL2_C'::h5ltmake_dataset_fl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltmake_dataset_fl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_fl2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_float2

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_float3
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_float3(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_float3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_fl3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_FL3_C'::h5ltmake_dataset_fl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_fl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_fl3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_float3

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_float4
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_float4(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_float4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_fl4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_FL4_C'::h5ltmake_dataset_fl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf ! data buffer
       END FUNCTION h5ltmake_dataset_fl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_fl4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_float4

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_float5
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_float5(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_float5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_fl5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_FL5_C'::h5ltmake_dataset_fl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf ! data buffer
       END FUNCTION h5ltmake_dataset_fl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_fl5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_float5

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_float6
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_float6(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_float6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_fl6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_FL6_C'::h5ltmake_dataset_fl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf ! data buffer
       END FUNCTION h5ltmake_dataset_fl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_fl6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_float6

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_float7
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_float7(loc_id, dset_name, rank, dims,&
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_float7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_fl7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_FL7_C'::h5ltmake_dataset_fl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf ! data buffer
       END FUNCTION h5ltmake_dataset_fl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_fl7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_float7

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_double1
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_double1(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_double1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf  ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_dl1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_DL1_C'::h5ltmake_dataset_dl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf       ! data buffer
       END FUNCTION h5ltmake_dataset_dl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_dl1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_double1

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_double2
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_double2(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_double2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_dl2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_DL2_C'::h5ltmake_dataset_dl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltmake_dataset_dl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_dl2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_double2

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_double3
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 1, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_double3(loc_id,&
       dset_name,&
       rank,&
       dims,&
       type_id,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_double3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_dl3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_DL3_C'::h5ltmake_dataset_dl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_dl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_dl3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_double3

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_double4
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_double4(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_double4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_dl4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_DL4_C'::h5ltmake_dataset_dl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_dl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_dl4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_double4

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_double5
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_double5(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_double5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_dl5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_DL5_C'::h5ltmake_dataset_dl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_dl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_dl5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_double5

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_double6
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_double6(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )
    
    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_double6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_dl6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_DL6_C'::h5ltmake_dataset_dl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_dl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_dl6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_double6

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_f_double7
  !
  ! Purpose: Creates and writes a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 7, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_f_double7(loc_id, dset_name, rank, dims, &
       type_id, buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_f_double7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_dl7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_DL7_C'::h5ltmake_dataset_dl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_dl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_dl7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)

  END SUBROUTINE h5ltmake_dataset_f_double7

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int1
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int1(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_int1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER, INTENT(inout), DIMENSION(*) :: buf        ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_int1_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_INT1_C'::h5ltread_dataset_int1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(HID_T),   INTENT(IN) :: loc_id                  ! file or group identifier
         INTEGER(HID_T),   INTENT(IN) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(LEN=*), INTENT(IN) :: dset_name               ! name of the dataset
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(IN), DIMENSION(*) :: buf                ! data buffer
       END FUNCTION h5ltread_dataset_int1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_int1_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_int1

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int2
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_int2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_int2_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_INT2_C'::h5ltread_dataset_int2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltread_dataset_int2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_int2_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_int2

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int3
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_int3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_int3_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_INT3_C'::h5ltread_dataset_int3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_int3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_int3_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_int3

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int4
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 12, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_int4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_int4_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_INT4_C'::h5ltread_dataset_int4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_int4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_int4_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_int4

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int5
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 12, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_int5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_int5_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_INT5_C'::h5ltread_dataset_int5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_int5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_int5_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_int5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int6
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 12, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_int6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_int6_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_INT6_C'::h5ltread_dataset_int6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_int6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_int6_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_int6

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_int7
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 12, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_int7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_int7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_int7_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_INT7_C'::h5ltread_dataset_int7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_int7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_int7_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_int7


  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_float1
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_float1(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_float1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    REAL, INTENT(inout), DIMENSION(*) :: buf           ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_fl1_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_FL1_C'::h5ltread_dataset_fl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), DIMENSION(*) :: buf                   ! data buffer
       END FUNCTION h5ltread_dataset_fl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_fl1_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_float1

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_float2
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_float2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_float2
    !DEC$endif
    !


    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_fl2_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_FL2_C'::h5ltread_dataset_fl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltread_dataset_fl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_fl2_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_float2

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_float3
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_float3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_float3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_fl3_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_FL3_C'::h5ltread_dataset_fl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_fl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_fl3_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_float3

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_float4
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_float4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_float4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_fl4_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_FL4_C'::h5ltread_dataset_fl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_fl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_fl4_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_float4

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_float5
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_float5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_float5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_fl5_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_FL5_C'::h5ltread_dataset_fl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_fl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_fl5_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_float5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_float6
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_float6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_float6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_fl6_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_FL6_C'::h5ltread_dataset_fl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_fl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_fl6_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_float6

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_float7
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_float7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_float7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_fl7_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_FL7_C'::h5ltread_dataset_fl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_fl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_fl7_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_float7


  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_double1
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_double1(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport ::h5ltread_dataset_f_double1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    DOUBLE PRECISION, INTENT(inout), DIMENSION(*) :: buf ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_dl1_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_DL1_C'::h5ltread_dataset_dl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf       ! data buffer
       END FUNCTION h5ltread_dataset_dl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_dl1_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_double1

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_double2
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_double2(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_double2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_dl2_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_DL2_C'::h5ltread_dataset_dl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltread_dataset_dl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_dl2_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_double2

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_double3
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_double3(loc_id,&
       dset_name,&
       type_id,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_double3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_dl3_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_DL3_C'::h5ltread_dataset_dl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_dl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_dl3_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_double3

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_double4
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_double4(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_double4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_dl4_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_DL4_C'::h5ltread_dataset_dl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_dl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_dl4_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_double4

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_double5
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_double5(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_double5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_dl5_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_DL5_C'::h5ltread_dataset_dl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_dl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_dl5_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_double5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_double6
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_double6(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_double6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_dl6_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_DL6_C'::h5ltread_dataset_dl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_dl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_dl6_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_double6

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_f_double7
  !
  ! Purpose: Read a dataset of a type TYPE_ID
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_f_double7(loc_id, dset_name, type_id, buf, &
       dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_f_double7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hid_t),   INTENT(in) :: type_id            ! datatype identifier
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_dl7_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_DL7_C'::h5ltread_dataset_dl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_dl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_dl7_c(loc_id,namelen,dset_name,type_id,buf,dims)

  END SUBROUTINE h5ltread_dataset_f_double7

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_1
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_int_f_1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER, INTENT(in), DIMENSION(*) :: buf           ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nint1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NINT1_C'::h5ltmake_dataset_nint1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), DIMENSION(*) :: buf                ! data buffer
       END FUNCTION h5ltmake_dataset_nint1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nint1_c(loc_id,namelen,dset_name,rank,dims,h5t_native_integer,buf)

  END SUBROUTINE h5ltmake_dataset_int_f_1

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_2
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_2 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_int_f_2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer


    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nint2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NINT2_C'::h5ltmake_dataset_nint2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer
       END FUNCTION h5ltmake_dataset_nint2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nint2_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

  END SUBROUTINE h5ltmake_dataset_int_f_2


  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_3
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_3 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_int_f_3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer


    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nint3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NINT3_C'::h5ltmake_dataset_nint3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_nint3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nint3_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

  END SUBROUTINE h5ltmake_dataset_int_f_3

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_4
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_4(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_int_f_4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer


    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nint4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NINT4_C'::h5ltmake_dataset_nint4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_nint4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nint4_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

  END SUBROUTINE h5ltmake_dataset_int_f_4

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_5
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_5(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_int_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer


    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nint5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NINT5_C'::h5ltmake_dataset_nint5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_nint5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nint5_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

  END SUBROUTINE h5ltmake_dataset_int_f_5

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_6
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_6(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_int_f_6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer


    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nint6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NINT6_C'::h5ltmake_dataset_nint6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_nint6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nint6_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

  END SUBROUTINE h5ltmake_dataset_int_f_6

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_int_f_7
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_INT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_int_f_7(loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_int_f_7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer


    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nint7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NINT7_C'::h5ltmake_dataset_nint7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf  ! data buffer
       END FUNCTION h5ltmake_dataset_nint7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nint7_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_INTEGER,buf)

  END SUBROUTINE h5ltmake_dataset_int_f_7

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_float_f_1
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_float_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_float_f_1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    REAL, INTENT(in), DIMENSION(*) :: buf              ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nfl1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NFL1_C'::h5ltmake_dataset_nfl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), DIMENSION(*) :: buf                   ! data buffer
       END FUNCTION h5ltmake_dataset_nfl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nfl1_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

  END SUBROUTINE h5ltmake_dataset_float_f_1

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_float_f_2
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_float_f_2 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_float_f_2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nfl2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NFL2_C'::h5ltmake_dataset_nfl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer
       END FUNCTION h5ltmake_dataset_nfl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nfl2_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

  END SUBROUTINE h5ltmake_dataset_float_f_2

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_float_f_3
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_float_f_3 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_float_f_3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nfl3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NFL3_C'::h5ltmake_dataset_nfl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_nfl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nfl3_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

  END SUBROUTINE h5ltmake_dataset_float_f_3

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_float_f_4
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_float_f_4 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_float_f_4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nfl4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NFL4_C'::h5ltmake_dataset_nfl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_nfl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nfl4_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

  END SUBROUTINE h5ltmake_dataset_float_f_4

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_float_f_5
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_float_f_5 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_float_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nfl5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NFL5_C'::h5ltmake_dataset_nfl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_nfl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nfl5_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

  END SUBROUTINE h5ltmake_dataset_float_f_5

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_float_f_6
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_float_f_6 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_float_f_6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nfl6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NFL6_C'::h5ltmake_dataset_nfl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_nfl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nfl6_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

  END SUBROUTINE h5ltmake_dataset_float_f_6

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_float_f_7
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_FLOAT type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_float_f_7 (loc_id, dset_name, rank, dims, &
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_float_f_7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_nfl7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NFL7_C'::h5ltmake_dataset_nfl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_nfl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_nfl7_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_REAL,buf)

  END SUBROUTINE h5ltmake_dataset_float_f_7


  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_double_f_1
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_double_f_1 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_double_f_1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1)) :: buf                          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_ndl1_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NDL1_C'::h5ltmake_dataset_ndl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1)) :: buf                               ! data buffer
       END FUNCTION h5ltmake_dataset_ndl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_ndl1_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

  END SUBROUTINE h5ltmake_dataset_double_f_1


  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_double_f_2
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_double_f_2 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_double_f_2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_ndl2_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NDL2_C'::h5ltmake_dataset_ndl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltmake_dataset_ndl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_ndl2_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

  END SUBROUTINE h5ltmake_dataset_double_f_2

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_double_f_3
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_double_f_3 (loc_id,&
       dset_name,&
       rank,&
       dims,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_double_f_3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_ndl3_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NDL3_C'::h5ltmake_dataset_ndl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_ndl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_ndl3_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

  END SUBROUTINE h5ltmake_dataset_double_f_3

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_double_f_4
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_double_f_4 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_double_f_4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_ndl4_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NDL4_C'::h5ltmake_dataset_ndl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_ndl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_ndl4_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

  END SUBROUTINE h5ltmake_dataset_double_f_4

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_double_f_5
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_double_f_5 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_double_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_ndl5_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NDL5_C'::h5ltmake_dataset_ndl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_ndl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_ndl5_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

  END SUBROUTINE h5ltmake_dataset_double_f_5

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_double_f_6
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_double_f_6 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_double_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_ndl6_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NDL6_C'::h5ltmake_dataset_ndl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_ndl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_ndl6_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

  END SUBROUTINE h5ltmake_dataset_double_f_6

  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_double_f_7
  !
  ! Purpose: Creates and writes a dataset of H5T_NATIVE_DOUBLE type
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_double_f_7 (loc_id, dset_name, rank, dims,&
       buf, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_double_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(in) :: rank               ! rank
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(in), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_ndl7_c(loc_id,namelen,dset_name,rank,dims,type_id,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_NDL7_C'::h5ltmake_dataset_ndl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(in) :: rank                    ! rank
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(in), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltmake_dataset_ndl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltmake_dataset_ndl7_c(loc_id,namelen,dset_name,rank,dims,H5T_NATIVE_DOUBLE,buf)

  END SUBROUTINE h5ltmake_dataset_double_f_7

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_int_f_1
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_int_f_1
    !DEC$endif
    !

    INTEGER(HID_T),   INTENT(IN) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1)) :: buf                          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nint1_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NINT1_C'::h5ltread_dataset_nint1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(inout), &
              DIMENSION(dims(1)) :: buf                               ! data buffer
       END FUNCTION h5ltread_dataset_nint1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nint1_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

  END SUBROUTINE h5ltread_dataset_int_f_1


  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_int_f_2
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_int_f_2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nint2_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NINT2_C'::h5ltread_dataset_nint2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(inout), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltread_dataset_nint2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nint2_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

  END SUBROUTINE h5ltread_dataset_int_f_2

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_int_f_3
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_int_f_3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nint3_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NINT3_C'::h5ltread_dataset_nint3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nint3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nint3_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

  END SUBROUTINE h5ltread_dataset_int_f_3

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_int_f_4
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_int_f_4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nint4_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NINT4_C'::h5ltread_dataset_nint4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nint4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nint4_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

  END SUBROUTINE h5ltread_dataset_int_f_4

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_int_f_5
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_int_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nint5_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NINT5_C'::h5ltread_dataset_nint5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nint5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nint5_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

  END SUBROUTINE h5ltread_dataset_int_f_5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_int_f_6
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_int_f_6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nint6_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NINT6_C'::h5ltread_dataset_nint6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nint6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nint6_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

  END SUBROUTINE h5ltread_dataset_int_f_6

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_int_f_7
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_int_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_int_f_7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nint7_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NINT7_C'::h5ltread_dataset_nint7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         INTEGER, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nint7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nint7_c(loc_id,namelen,dset_name,H5T_NATIVE_INTEGER,buf,dims)

  END SUBROUTINE h5ltread_dataset_int_f_7


  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_float_f_1
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_float_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_float_f_1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1)) :: buf                          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nfl1_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NFL1_C'::h5ltread_dataset_nfl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(inout), &
              DIMENSION(dims(1)) :: buf                               ! data buffer
       END FUNCTION h5ltread_dataset_nfl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nfl1_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

  END SUBROUTINE h5ltread_dataset_float_f_1


  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_float_f_2
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_float_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_float_f_2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nfl2_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NFL2_C'::h5ltread_dataset_nfl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(inout), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltread_dataset_nfl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nfl2_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

  END SUBROUTINE h5ltread_dataset_float_f_2

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_float_f_3
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_float_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_float_f_3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nfl3_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NFL3_C'::h5ltread_dataset_nfl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nfl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nfl3_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

  END SUBROUTINE h5ltread_dataset_float_f_3

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_float_f_4
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_float_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_float_f_4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nfl4_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NFL4_C'::h5ltread_dataset_nfl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nfl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nfl4_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

  END SUBROUTINE h5ltread_dataset_float_f_4

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_float_f_5
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_float_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_float_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nfl5_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NFL5_C'::h5ltread_dataset_nfl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nfl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nfl5_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

  END SUBROUTINE h5ltread_dataset_float_f_5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_float_f_6
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_float_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_float_f_6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nfl6_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NFL6_C'::h5ltread_dataset_nfl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nfl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nfl6_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

  END SUBROUTINE h5ltread_dataset_float_f_6

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_float_f_7
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_float_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_float_f_7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    REAL, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_nfl7_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NFL7_C'::h5ltread_dataset_nfl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         REAL, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_nfl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_nfl7_c(loc_id,namelen,dset_name,H5T_NATIVE_REAL,buf,dims)

  END SUBROUTINE h5ltread_dataset_float_f_7

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_double_f_1
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_double_f_1(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_double_f_1
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1)) :: buf                          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_ndl1_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NDL1_C'::h5ltread_dataset_ndl1_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(inout), &
              DIMENSION(dims(1)) :: buf                               ! data buffer
       END FUNCTION h5ltread_dataset_ndl1_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_ndl1_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

  END SUBROUTINE h5ltread_dataset_double_f_1


  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_double_f_2
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_double_f_2(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_double_f_2
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2)) :: buf                  ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_ndl2_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NDL2_C'::h5ltread_dataset_ndl2_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(inout), &
              DIMENSION(dims(1),dims(2)) :: buf                       ! data buffer
       END FUNCTION h5ltread_dataset_ndl2_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_ndl2_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

  END SUBROUTINE h5ltread_dataset_double_f_2

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_double_f_3
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_double_f_3(loc_id,&
       dset_name,&
       buf,&
       dims,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_double_f_3
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_ndl3_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NDL3_C'::h5ltread_dataset_ndl3_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_ndl3_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_ndl3_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

  END SUBROUTINE h5ltread_dataset_double_f_3

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_double_f_4
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_double_f_4(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_double_f_4
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_ndl4_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NDL4_C'::h5ltread_dataset_ndl4_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_ndl4_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_ndl4_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

  END SUBROUTINE h5ltread_dataset_double_f_4

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_double_f_5
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_double_f_5(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_double_f_5
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_ndl5_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NDL5_C'::h5ltread_dataset_ndl5_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_ndl5_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_ndl5_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

  END SUBROUTINE h5ltread_dataset_double_f_5

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_double_f_6
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_double_f_6(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_double_f_6
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_ndl6_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NDL6_C'::h5ltread_dataset_ndl6_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_ndl6_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_ndl6_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

  END SUBROUTINE h5ltread_dataset_double_f_6

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_double_f_7
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: M. Scot Breitenfeld
  !
  ! Date: March 8, 2011
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_double_f_7(loc_id, dset_name, buf, dims, errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_double_f_7
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims ! size of the bufffer buf
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    DOUBLE PRECISION, INTENT(inout), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf          ! data buffer

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_ndl7_c(loc_id,namelen,dset_name,type_id,buf,dims)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_NDL7_C'::h5ltread_dataset_ndl7_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER(hid_t),   INTENT(in) :: type_id                 ! datatype identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t), DIMENSION(*), INTENT(in) :: dims      ! size of the bufffer buf
         DOUBLE PRECISION, INTENT(inout), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf               ! data buffer
       END FUNCTION h5ltread_dataset_ndl7_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_ndl7_c(loc_id,namelen,dset_name,H5T_NATIVE_DOUBLE,buf,dims)

  END SUBROUTINE h5ltread_dataset_double_f_7


  !-------------------------------------------------------------------------
  ! Function: h5ltmake_dataset_string_f
  !
  ! Purpose: Creates and writes a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltmake_dataset_string_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltmake_dataset_string_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: buf                ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER :: buflen                                  ! buffer length

    INTERFACE
       INTEGER FUNCTION h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buflen,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTMAKE_DATASET_STRING_C'::h5ltmake_dataset_string_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: buflen                                       ! lenght of data buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: buf                     ! data buffer
       END FUNCTION h5ltmake_dataset_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    buflen = LEN(buf)
    errcode = h5ltmake_dataset_string_c(loc_id,namelen,dset_name,buflen,buf)

  END SUBROUTINE h5ltmake_dataset_string_f

  !-------------------------------------------------------------------------
  ! Function: h5ltread_dataset_string_f
  !
  ! Purpose: Read a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 22, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltread_dataset_string_f(loc_id,&
       dset_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltread_dataset_string_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(inout) :: buf             ! data buffer
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTREAD_DATASET_STRING_C'::h5ltread_dataset_string_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(inout) :: buf                  ! data buffer
       END FUNCTION h5ltread_dataset_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltread_dataset_string_c(loc_id,namelen,dset_name,buf)

  END SUBROUTINE h5ltread_dataset_string_f




  !-------------------------------------------------------------------------
  ! Make/Read attribute functions
  !-------------------------------------------------------------------------


  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_int_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_int_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltset_attribute_int_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(size_t),  INTENT(in) :: size               ! size of attribute array
    INTEGER :: errcode                                 ! error code
    INTEGER, INTENT(in), DIMENSION(*) :: buf           ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltset_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTSET_ATTRIBUTE_INT_C'::h5ltset_attribute_int_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER(size_t),  INTENT(in) :: size                    ! size of attribute array
         INTEGER, INTENT(in), DIMENSION(*) :: buf                ! data buffer
       END FUNCTION h5ltset_attribute_int_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)

  END SUBROUTINE h5ltset_attribute_int_f

  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_float_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_float_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltset_attribute_float_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(size_t),  INTENT(in) :: size               ! size of attribute array
    INTEGER :: errcode                                 ! error code
    REAL, INTENT(in), DIMENSION(*) :: buf              ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltset_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTSET_ATTRIBUTE_FLOAT_C'::h5ltset_attribute_float_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER(size_t),  INTENT(in) :: size                    ! size of attribute array
         REAL, INTENT(in), DIMENSION(*) :: buf                   ! data buffer
       END FUNCTION h5ltset_attribute_float_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)

  END SUBROUTINE h5ltset_attribute_float_f

  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_double_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_double_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       size,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltset_attribute_double_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(size_t),  INTENT(in) :: size               ! size of attribute array
    INTEGER :: errcode                                 ! error code
    DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf  ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltset_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTSET_ATTRIBUTE_DOUBLE_C'::h5ltset_attribute_double_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER(size_t),  INTENT(in) :: size                    ! size of attribute array
         DOUBLE PRECISION, INTENT(in), DIMENSION(*) :: buf       ! data buffer
       END FUNCTION h5ltset_attribute_double_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltset_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,size,buf)

  END SUBROUTINE h5ltset_attribute_double_f


  !-------------------------------------------------------------------------
  ! Function: h5ltset_attribute_string_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltset_attribute_string_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltset_attribute_string_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    CHARACTER(len=*), INTENT(in) :: buf                ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length
    INTEGER :: buflen                                  ! data buffer length

    INTERFACE
       INTEGER FUNCTION h5ltset_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buflen,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTSET_ATTRIBUTE_STRING_C'::h5ltset_attribute_string_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         INTEGER :: buflen                                       ! data buffer length
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         CHARACTER(len=*), INTENT(in) :: buf                     ! data buffer
       END FUNCTION h5ltset_attribute_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    buflen = LEN(buf)
    errcode = h5ltset_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buflen,buf)

  END SUBROUTINE h5ltset_attribute_string_f



  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_int_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_int_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_attribute_int_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    INTEGER, INTENT(inout), DIMENSION(*) :: buf        ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_ATTRIBUTE_INT_C'::h5ltget_attribute_int_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER, INTENT(inout), DIMENSION(*) :: buf             ! data buffer
       END FUNCTION h5ltget_attribute_int_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_int_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

  END SUBROUTINE h5ltget_attribute_int_f


  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_float_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_float_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_attribute_float_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    REAL, INTENT(inout), DIMENSION(*) :: buf           ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_ATTRIBUTE_FLOAT_C'::h5ltget_attribute_float_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         REAL, INTENT(inout), DIMENSION(*) :: buf                ! data buffer
       END FUNCTION h5ltget_attribute_float_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_float_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

  END SUBROUTINE h5ltget_attribute_float_f

  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_double_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_double_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_attribute_double_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    DOUBLE PRECISION,INTENT(inout),DIMENSION(*) :: buf ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_ATTRIBUTE_DOUBLE_C'::h5ltget_attribute_double_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         DOUBLE PRECISION, INTENT(inout), DIMENSION(*) :: buf    ! data buffer
       END FUNCTION h5ltget_attribute_double_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_double_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

  END SUBROUTINE h5ltget_attribute_double_f

  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_string_f
  !
  ! Purpose: Reads an attribute named ATTR_NAME
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_string_f(loc_id,&
       dset_name,&
       attr_name,&
       buf,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_attribute_string_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER :: errcode                                 ! error code
    CHARACTER(len=*), INTENT(inout) :: buf             ! data buffer
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_ATTRIBUTE_STRING_C'::h5ltget_attribute_string_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         CHARACTER(len=*), INTENT(inout) :: buf                  ! data buffer
       END FUNCTION h5ltget_attribute_string_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_string_c(loc_id,namelen,dset_name,attrlen,attr_name,buf)

  END SUBROUTINE h5ltget_attribute_string_f

  !-------------------------------------------------------------------------
  ! Query dataset functions
  !-------------------------------------------------------------------------

  !-------------------------------------------------------------------------
  ! Function: h5ltget_dataset_ndims_f
  !
  ! Purpose: Gets the dimensionality of a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 30, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_dataset_ndims_f(loc_id,&
       dset_name,&
       rank,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_dataset_ndims_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER,          INTENT(inout) :: rank            ! rank
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_DATASET_NDIMS_C'::h5ltget_dataset_ndims_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER,          INTENT(inout) :: rank                 ! rank
       END FUNCTION h5ltget_dataset_ndims_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltget_dataset_ndims_c(loc_id,namelen,dset_name,rank)

  END SUBROUTINE h5ltget_dataset_ndims_f


  !-------------------------------------------------------------------------
  ! Function: h5ltfind_dataset_f
  !
  ! Purpose: Inquires if a dataset named dset_name exists attached
  !           to the object loc_id.
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  INTEGER FUNCTION h5ltfind_dataset_f(loc_id,&
       dset_name)

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltfind_dataset_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltfind_dataset_c(loc_id,namelen,dset_name)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTFIND_DATASET_C'::h5ltfind_dataset_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
       END FUNCTION h5ltfind_dataset_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltfind_dataset_c(loc_id,namelen,dset_name)
    h5ltfind_dataset_f = errcode

  END FUNCTION h5ltfind_dataset_f

  !-------------------------------------------------------------------------
  ! Function: h5ltget_dataset_info_f
  !
  ! Purpose: Gets information about a dataset
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 30, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_dataset_info_f(loc_id,&
       dset_name,&
       dims,&
       type_class,&
       type_size,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_dataset_info_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims ! dimensions
    INTEGER, INTENT(inout)         :: type_class       ! type class
    INTEGER(size_t), INTENT(inout) :: type_size        ! type size
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_DATASET_INFO_C'::h5ltget_dataset_info_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims      ! dimensions
         INTEGER, INTENT(inout)         :: type_class            ! type class
         INTEGER(size_t), INTENT(inout) :: type_size             ! type size
       END FUNCTION h5ltget_dataset_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    errcode = h5ltget_dataset_info_c(loc_id,namelen,dset_name,dims,type_class,type_size)

  END SUBROUTINE h5ltget_dataset_info_f


  !-------------------------------------------------------------------------
  ! Query attribute functions
  !-------------------------------------------------------------------------


  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_ndims_f
  !
  ! Purpose: Create and write an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: October 05, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_ndims_f(loc_id,&
       dset_name,&
       attr_name,&
       rank,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_attribute_ndims_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER,          INTENT(inout) :: rank            ! rank
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_ndims_c(loc_id,namelen,dset_name,attrlen,attr_name,rank)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_ATTRIBUTE_NDIMS_C'::h5ltget_attribute_ndims_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER,          INTENT(inout) :: rank                 ! rank
       END FUNCTION h5ltget_attribute_ndims_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_ndims_c(loc_id,namelen,dset_name,attrlen,attr_name,rank)

  END SUBROUTINE h5ltget_attribute_ndims_f


  !-------------------------------------------------------------------------
  ! Function: h5ltget_attribute_info_f
  !
  ! Purpose: Gets information about an attribute
  !
  ! Return: Success: 0, Failure: -1
  !
  ! Programmer: pvn@ncsa.uiuc.edu
  !
  ! Date: September 30, 2004
  !
  ! Comments:
  !
  ! Modifications:
  !
  !-------------------------------------------------------------------------

  SUBROUTINE h5ltget_attribute_info_f(loc_id,&
       dset_name,&
       attr_name,&
       dims,&
       type_class,&
       type_size,&
       errcode )

    IMPLICIT NONE

    !
    !This definition is needed for Windows DLLs
    !DEC$if defined(BUILD_HDF5_DLL)
    !DEC$attributes dllexport :: h5ltget_attribute_info_f
    !DEC$endif
    !

    INTEGER(hid_t),   INTENT(in) :: loc_id             ! file or group identifier
    CHARACTER(len=*), INTENT(in) :: dset_name          ! name of the dataset
    CHARACTER(len=*), INTENT(in) :: attr_name          ! name of the attribute
    INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims ! dimensions
    INTEGER, INTENT(inout)         :: type_class       ! type class
    INTEGER(size_t), INTENT(inout) :: type_size        ! type size
    INTEGER :: errcode                                 ! error code
    INTEGER :: namelen                                 ! name length
    INTEGER :: attrlen                                 ! name length

    INTERFACE
       INTEGER FUNCTION h5ltget_attribute_info_c(loc_id,namelen,dset_name,attrlen,attr_name,dims,type_class,type_size)
         USE h5global
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LTGET_ATTRIBUTE_INFO_C'::h5ltget_attribute_info_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: dset_name
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(hid_t),   INTENT(in) :: loc_id                  ! file or group identifier
         INTEGER :: namelen                                      ! lenght of name buffer
         INTEGER :: attrlen                                      ! lenght of attr name buffer
         CHARACTER(len=*), INTENT(in) :: dset_name               ! name of the dataset
         CHARACTER(len=*), INTENT(in) :: attr_name               ! name of the attribute
         INTEGER(hsize_t),DIMENSION(*),INTENT(inout):: dims      ! dimensions
         INTEGER, INTENT(inout)         :: type_class            ! type class
         INTEGER(size_t), INTENT(inout) :: type_size             ! type size
       END FUNCTION h5ltget_attribute_info_c
    END INTERFACE

    namelen = LEN(dset_name)
    attrlen = LEN(attr_name)
    errcode = h5ltget_attribute_info_c(loc_id,namelen,dset_name,attrlen,attr_name,dims,type_class,type_size)

  END SUBROUTINE h5ltget_attribute_info_f
  !  end
  !
END MODULE H5LT






