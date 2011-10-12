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
! This file contains Fortran90 interfaces for H5F functions.
!
      MODULE H5F
      USE H5GLOBAL

        CONTAINS

!----------------------------------------------------------------------
! Name:		h5fcreate_f
!
! Purpose:	Creates HDF5 files.
!
! Inputs:
!		name		- name of the file to create
!		access_flags	- File access flags. Allowable values are:
!				  H5F_ACC_TRUNC_F
!				  H5F_ACC_EXCL_F
! Outputs:
!		file_id		- file identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		creation_prp	- file creation property list identifier
!		access_prp	- file access property list identifier
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5fcreate_f(name, access_flags, file_id, hdferr, &
                                 creation_prp, access_prp)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
            INTEGER, INTENT(IN) :: access_flags    ! File access flags
            INTEGER(HID_T), INTENT(OUT) :: file_id ! File identifier
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: creation_prp
                                                   ! File creation propertly
                                                   ! list identifier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                                   ! File access property list
                                                   ! identifier
            INTEGER(HID_T) :: creation_prp_default
            INTEGER(HID_T) :: access_prp_default
            INTEGER :: namelen ! Length of the name character string

!            INTEGER, EXTERNAL :: h5fcreate_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5fcreate_c(name, namelen, access_flags, &
                               creation_prp_default, access_prp_default, file_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FCREATE_C':: h5fcreate_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER, INTENT(IN) :: access_flags
              INTEGER(HID_T), INTENT(OUT) :: file_id
              INTEGER(HID_T), INTENT(IN) :: creation_prp_default
              INTEGER(HID_T), INTENT(IN) :: access_prp_default
              INTEGER :: namelen
              END FUNCTION h5fcreate_c
            END INTERFACE

            creation_prp_default = H5P_DEFAULT_F
            access_prp_default = H5P_DEFAULT_F

            if (present(creation_prp)) creation_prp_default = creation_prp
            if (present(access_prp))   access_prp_default   = access_prp
            namelen = LEN_TRIM(name)

            hdferr = h5fcreate_c(name, namelen, access_flags, &
                     creation_prp_default, access_prp_default, file_id)

          END SUBROUTINE h5fcreate_f

!----------------------------------------------------------------------
! Name:		h5fflush_f
!
! Purpose:	Flushes all buffers associated with a file to disk
!
! Inputs:
!		object_id	- identifier of object used to identify
!				  the file.
!		scope		- specifies the scope of the flushing action.
!				  Possible values are:
!				  H5F_SCOPE_GLOBAL_F
!				  H5F_SCOPE_LOCAL_F
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		creation_prp	- file creation property list identifier
!		access_prp	- file access property list identifier
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5fflush_f(object_id, scope, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: object_id !identifier for any object
                                                    !associate with a file,
                                                    !including the file itself,
                                                    !a dataset, a group, an
                                                    !attribute, or a named
                                                    !data type

            INTEGER, INTENT(IN) :: scope            !scope of the flushing
                                                    !action, possible values
                                                    !are: H5F_SCOPE_GLOBAL_F
                                                    ! which flushes the entire
                                                    !virtual file,
                                                    !and H5F_SCOPE_LOCAL_F
                                                    !which flushes only the
                                                    !specified file.

            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5fflush_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fflush_c(object_id, scope)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FFLUSH_C':: h5fflush_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: object_id
              INTEGER, INTENT(IN) :: scope
              END FUNCTION h5fflush_c
            END INTERFACE

           hdferr = h5fflush_c(object_id, scope)

          END SUBROUTINE h5fflush_f

!----------------------------------------------------------------------
! Name:		h5fmount_f
!
! Purpose:	Mounts a file.
!
! Inputs:
!		loc_id		- the identifier for of file or group in
!				  which name is defined
!		name		- the name of the group onto which the file
!				  specified by child_id is to be mounted.
!		child_id	- the identifier of the file to be mounted.
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		access_prp	- the identifier of the property list to be used
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fmount_f(loc_id, name, child_id, hdferr, access_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for file or group
                                                   ! in which dsetname is defined
            CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of the group
            INTEGER(HID_T), INTENT(IN) :: child_id ! File identifier for the
                                                   ! file to be mounted
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                                   ! File access property list
                                                   ! identifier
            INTEGER(HID_T) :: access_prp_default
            INTEGER :: namelen ! Length of the name character string

!            INTEGER, EXTERNAL :: h5fmount_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5fmount_c(loc_id, name, namelen, &
                                          child_id, access_prp_default)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FMOUNT_C':: h5fmount_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER(HID_T), INTENT(IN) :: child_id
              INTEGER(HID_T), INTENT(IN) :: access_prp_default
              INTEGER :: namelen
              END FUNCTION h5fmount_c
            END INTERFACE

            access_prp_default = H5P_DEFAULT_F
            if (present(access_prp))   access_prp_default   = access_prp
            namelen = LEN(name)
            hdferr = h5fmount_c(loc_id, name, namelen, child_id, access_prp_default)

          END SUBROUTINE h5fmount_f


!----------------------------------------------------------------------
! Name:		h5funmount_f
!
! Purpose:	Unmounts a file.
!
! Inputs:
!		loc_id		- the identifier for of file or group in
!				  which name is defined
!		name		- the name of the mount point
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5funmount_f(loc_id, name, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for file or group
                                                   ! at which the specified file
                                                   ! is to be unmounted
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the mount point
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER :: namelen ! Length of the name character string

!            INTEGER, EXTERNAL :: h5fumount_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5funmount_c(loc_id, name, namelen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FUNMOUNT_C':: h5funmount_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              END FUNCTION h5funmount_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5funmount_c(loc_id, name, namelen)

          END SUBROUTINE h5funmount_f

!----------------------------------------------------------------------
! Name:		h5fopen_f
!
! Purpose:	Opens HDF5 file.
!
! Inputs:
!		name		- name of the file to acecss
!		access_flags	- File access flags. Allowable values are:
!				  H5F_ACC_RDWR_F
!				  H5F_ACC_RDONLY_F
! Outputs:
!		file_id		- file identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		access_prp	- file access property list identifier
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5fopen_f(name, access_flags, file_id, hdferr, &
                               access_prp)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
            INTEGER, INTENT(IN) :: access_flags    ! File access flags
            INTEGER(HID_T), INTENT(OUT) :: file_id ! File identifier
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: access_prp
                                                   ! File access property list
                                                   ! identifier
            INTEGER(HID_T) :: access_prp_default
            INTEGER :: namelen ! Length of the name character string

!            INTEGER, EXTERNAL :: h5fopen_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fopen_c(name, namelen, access_flags, &
                               access_prp_default, file_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FOPEN_C':: h5fopen_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER, INTENT(IN) :: access_flags
              INTEGER(HID_T), INTENT(IN) :: access_prp_default
              INTEGER(HID_T), INTENT(OUT) :: file_id
              END FUNCTION h5fopen_c
            END INTERFACE

            access_prp_default = H5P_DEFAULT_F
            if (present(access_prp))   access_prp_default   = access_prp
            namelen = LEN(name)
            hdferr = h5fopen_c(name, namelen, access_flags, &
                               access_prp_default, file_id)

          END SUBROUTINE h5fopen_f

!----------------------------------------------------------------------
! Name:		h5freopen_f
!
! Purpose:	Reopens HDF5 file.
!
! Inputs:
!		file_id		- identifier of a file for which an
!				  additional identifier is required
! Outputs:
!		ret_file_id	- new file identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5freopen_f(file_id, ret_file_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id      ! File identifier
            INTEGER(HID_T), INTENT(OUT) :: ret_file_id ! New File identifier
            INTEGER, INTENT(OUT) :: hdferr             ! Error code

!            INTEGER, EXTERNAL :: h5freopen_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5freopen_c(file_id, ret_file_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FREOPEN_C':: h5freopen_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              INTEGER(HID_T), INTENT(OUT) :: ret_file_id
              END FUNCTION h5freopen_c
            END INTERFACE

            hdferr = h5freopen_c(file_id, ret_file_id)

          END SUBROUTINE h5freopen_f

!----------------------------------------------------------------------
! Name:		h5fget_create_plist_f
!
! Purpose:	Returns a file creation property list identifier.
!
! Inputs:
!		file_id		- identifier of a file to get
!				  get creation property list of
! Outputs:
!		prop_id 	- creation property list identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fget_create_plist_f(file_id, prop_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id    ! File identifier
            INTEGER(HID_T), INTENT(OUT) :: prop_id   ! File creation property
                                                     ! list identifier
            INTEGER, INTENT(OUT) :: hdferr           ! Error code

!            INTEGER, EXTERNAL :: h5fget_create_plist_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fget_create_plist_c(file_id, prop_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
!DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_CREATE_PLIST_C':: h5fget_create_plist_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              INTEGER(HID_T), INTENT(OUT) :: prop_id
              END FUNCTION h5fget_create_plist_c
            END INTERFACE

            hdferr = h5fget_create_plist_c(file_id, prop_id)

          END SUBROUTINE h5fget_create_plist_f

!----------------------------------------------------------------------
! Name:		h5fget_access_plist_f
!
! Purpose:	Returns a file access property list identifier.
!
! Inputs:
!		file_id		- identifier of a file to get
!				  get creation property list of
! Outputs:
!		access_id 	- access property list identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fget_access_plist_f(file_id, access_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id      ! File identifier
            INTEGER(HID_T), INTENT(OUT) :: access_id   ! File access property
                                                       ! list identifier
            INTEGER, INTENT(OUT) :: hdferr             ! Error code

!            INTEGER, EXTERNAL :: h5fget_access_plist_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fget_access_plist_c(file_id, access_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
!DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_CREATE_PLIST_C':: h5fget_access_plist_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              INTEGER(HID_T), INTENT(OUT) :: access_id
              END FUNCTION h5fget_access_plist_c
            END INTERFACE


            hdferr = h5fget_access_plist_c(file_id, access_id)

          END SUBROUTINE h5fget_access_plist_f

!----------------------------------------------------------------------
! Name:		h5fis_hdf5_f
!
! Purpose:	Determines whether a file is in the HDF5 format.
!
! Inputs:
!		name		- name of the file to check
! Outputs:
!		status		- indicates if file is and HDF5 file
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fis_hdf5_f(name, status, hdferr)
            IMPLICIT NONE
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the file
            LOGICAL, INTENT(OUT) :: status         ! Indicates if file
                                                   ! is an HDF5 file
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER :: namelen ! Length of the name character string
            INTEGER :: flag    ! "TRUE/FALSE" flag from C routine
                               ! to define status value.

!            INTEGER, EXTERNAL :: h5fis_hdf5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fis_hdf5_c(name, namelen, flag)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FIS_HDF5_C':: h5fis_hdf5_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER :: flag
              END FUNCTION h5fis_hdf5_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5fis_hdf5_c(name, namelen, flag)
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.

          END SUBROUTINE h5fis_hdf5_f

!----------------------------------------------------------------------
! Name:		h5fclose_f
!
! Purpose:	Closes HDF5 file.
!
! Inputs:
!		file_id		- file identifier
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fclose_f(file_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5fclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fclose_c(file_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FCLOSE_C':: h5fclose_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              END FUNCTION h5fclose_c
            END INTERFACE

            hdferr = h5fclose_c(file_id)

          END SUBROUTINE h5fclose_f

!----------------------------------------------------------------------
! Name:		h5fget_obj_count_f
!
! Purpose:	Gets number of the objects open within a file
!
! Inputs:
!		file_id		- file identifier
!               obj_type        - type of the object; possible values are:
!                                 H5F_OBJ_FILE_F
!                                 H5F_OBJ_DATASET_F
!                                 H5F_OBJ_GROUP_F
!                                 H5F_OBJ_DATATYPE_F
!                                 H5F_OBJ_ALL_F
! Outputs:
!               obj_count       - number of open objects
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		September 30, 2002
!
! Modifications:
!               Changed the type of obj_count to INTEGER(SIZE_T)
!                               September 25, 2008 EIP
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fget_obj_count_f(file_id, obj_type, obj_count, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
            INTEGER, INTENT(IN)  :: obj_type      ! Object type
            INTEGER(SIZE_T), INTENT(OUT) :: obj_count
                                                  ! Number of open objects
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

            INTERFACE
              INTEGER FUNCTION h5fget_obj_count_c(file_id, obj_type, obj_count)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_OBJ_COUNT_C':: h5fget_obj_count_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              INTEGER, INTENT(IN)  :: obj_type      ! Object type
              INTEGER(SIZE_T), INTENT(OUT) :: obj_count
                                                    ! Number of open objects
              END FUNCTION h5fget_obj_count_c
            END INTERFACE

            hdferr = h5fget_obj_count_c(file_id, obj_type, obj_count)

          END SUBROUTINE h5fget_obj_count_f

!----------------------------------------------------------------------
! Name:		h5fget_obj_ids_f
!
! Purpose:	Get list of open objects identifiers within a file
!
! Inputs:
!		file_id		- file identifier
!               obj_type        - type of the object; possible values are:
!                                 H5F_OBJ_FILE_F
!                                 H5F_OBJ_DATASET_F
!                                 H5F_OBJ_GROUP_F
!                                 H5F_OBJ_DATATYPE_F
!                                 H5F_OBJ_ALL_F
! Outputs:
!               obj_ids         - array of open object identifiers
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		September 30, 2002
!
! Modifications:
!               Added optional parameter num_objs for number of open objects
!               of the specified type and changed type of max_obj to
!               INTEGER(SIZE_T)
!                               September 25, 2008 EIP
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fget_obj_ids_f(file_id, obj_type, max_objs, obj_ids, hdferr, num_objs)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
            INTEGER, INTENT(IN)  :: obj_type   ! Object type
            INTEGER(SIZE_T), INTENT(IN)  :: max_objs   ! Maximum # of objects to retrieve
            INTEGER(HID_T), DIMENSION(*), INTENT(INOUT) :: obj_ids
                                               ! Array of open objects iidentifiers
            INTEGER, INTENT(OUT) :: hdferr     ! Error code
            INTEGER(SIZE_T), INTENT(OUT), OPTIONAL :: num_objs
            INTEGER(SIZE_T) :: c_num_objs
                              ! Number of open objects of the specified type

            INTERFACE
              INTEGER FUNCTION h5fget_obj_ids_c(file_id, obj_type, max_objs, obj_ids, c_num_objs)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_OBJ_IDS_C':: h5fget_obj_ids_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              INTEGER, INTENT(IN)  :: obj_type
              INTEGER(SIZE_T), INTENT(IN)  :: max_objs
              INTEGER(HID_T), DIMENSION(*), INTENT(INOUT) :: obj_ids
              INTEGER(SIZE_T), INTENT(OUT) :: c_num_objs
              END FUNCTION h5fget_obj_ids_c
            END INTERFACE

            hdferr = h5fget_obj_ids_c(file_id, obj_type, max_objs, obj_ids, c_num_objs)
            if (present(num_objs)) num_objs= c_num_objs

          END SUBROUTINE h5fget_obj_ids_f

!----------------------------------------------------------------------
! Name:		h5fget_freespace_f
!
! Purpose:	Get amount of free space within a file
!
! Inputs:
!		file_id		- file identifier
! Outputs:
!               free_space      - amount of free space in file
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Quincey Koziol
!		October 7, 2003
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5fget_freespace_f(file_id, free_space, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id ! File identifier
            INTEGER(HSSIZE_T), INTENT(OUT) :: free_space
                                             !amount of free space in file
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

            INTERFACE
              INTEGER FUNCTION h5fget_freespace_c(file_id, free_space)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
       !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_FREESPACE_C':: h5fget_freespace_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              INTEGER(HSSIZE_T), INTENT(OUT) :: free_space
              END FUNCTION h5fget_freespace_c
            END INTERFACE

            hdferr = h5fget_freespace_c(file_id, free_space)

          END SUBROUTINE h5fget_freespace_f

!----------------------------------------------------------------------
! Name:		h5fget_name_f
!
! Purpose: 	Gets the name of the file from the object identifier
!
! Inputs:
!		obj_id		- object identifier
! Inputs/Outputs:
!		buf		- buffer to read name in
! Outputs:
!		size		- actual size of the name
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!
! Programmer:	Elena Pourmal
!		July 6, 2004
!
!----------------------------------------------------------------------


          SUBROUTINE h5fget_name_f(obj_id, buf, size, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: obj_id   ! Object identifier
            CHARACTER(LEN=*), INTENT(INOUT) :: buf
                                                   ! Buffer to hold file name
            INTEGER(SIZE_T), INTENT(OUT) :: size   ! Size of the file name
            INTEGER, INTENT(OUT) :: hdferr         ! Error code: 0 on success,
                                                   ! -1 if fail
            INTEGER(SIZE_T) :: buflen
!            INTEGER, EXTERNAL :: h5fget_name_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fget_name_c(obj_id, size, buf, buflen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_NAME_C'::h5fget_name_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: obj_id
              INTEGER(SIZE_T), INTENT(OUT) :: size
              INTEGER(SIZE_T) :: buflen
              CHARACTER(LEN=*), INTENT(OUT) :: buf
              END FUNCTION h5fget_name_c
            END INTERFACE
            buflen = LEN(buf)
            hdferr = h5fget_name_c(obj_id, size, buf, buflen)
          END SUBROUTINE h5fget_name_f

!----------------------------------------------------------------------
! Name:		h5fget_filesize_f
!
! Purpose: 	Retrieves the file size of the HDF5 file.
!
! Inputs:
!		file_id		- file identifier
! Outputs:
!		size		- file size
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!
! Programmer:	Elena Pourmal
!		July 7, 2004
!
!----------------------------------------------------------------------


          SUBROUTINE h5fget_filesize_f(file_id, size, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: file_id  ! file identifier
            INTEGER(HSIZE_T), INTENT(OUT) :: size  ! Size of the file
            INTEGER, INTENT(OUT) :: hdferr         ! Error code: 0 on success,
                                                   ! -1 if fail
!            INTEGER, EXTERNAL :: h5fget_filesize_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5fget_filesize_c(file_id, size)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5FGET_FILESIZE_C'::h5fget_filesize_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: file_id
              INTEGER(HSIZE_T), INTENT(OUT) :: size
              END FUNCTION h5fget_filesize_c
            END INTERFACE
            hdferr = h5fget_filesize_c(file_id, size)
          END SUBROUTINE h5fget_filesize_f


      END MODULE H5F
