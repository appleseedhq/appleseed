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
! This file contains Fortran90 interfaces for H5R functions.
!
      MODULE H5R
        USE H5GLOBAL

! If you change the value of these parameters, do not forget to change corresponding
! values in the H5f90.h file.
!        INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
!        INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
!
!        TYPE hobj_ref_t_f
!             INTEGER ref(REF_OBJ_BUF_LEN)
!        END TYPE
!
!        TYPE hdset_reg_ref_t_f
!             INTEGER ref(REF_REG_BUF_LEN)
!        END TYPE
!
          INTERFACE h5rcreate_f

            MODULE PROCEDURE h5rcreate_object_f
            MODULE PROCEDURE h5rcreate_region_f

          END INTERFACE

          INTERFACE h5rdereference_f

            MODULE PROCEDURE h5rdereference_object_f
            MODULE PROCEDURE h5rdereference_region_f

          END INTERFACE

          INTERFACE h5rget_region_f

            MODULE PROCEDURE h5rget_region_region_f

          END INTERFACE

          INTERFACE h5rget_object_type_f

            MODULE PROCEDURE h5rget_object_type_obj_f

          END INTERFACE

          INTERFACE h5rget_name_f

            MODULE PROCEDURE h5rget_name_object_f
            MODULE PROCEDURE h5rget_name_region_f

          END INTERFACE

        CONTAINS

!----------------------------------------------------------------------
! Name:		h5rcreate_object_f
!
! Purpose: 	Creates reference to the object
!
! Inputs:
!		loc_id		- location identifier
!		name		- name of the object at the specified location
! Outputs:
!		ref		- reference to the specified object
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
! Comment:		This is a module procedure for the h5rcreate_f
!			subroutine.
!----------------------------------------------------------------------

          SUBROUTINE h5rcreate_object_f(loc_id, name, ref, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Location identifier
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the object at location specified
                                                   ! by loc_id identifier
            TYPE(hobj_ref_t_f), INTENT(OUT) :: ref   ! Object reference
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

            INTEGER :: namelen                     ! Name length
            INTEGER(HADDR_T) :: ref_f              ! Local buffer to pass reference

!            INTEGER, EXTERNAL :: h5fcreate_object_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5rcreate_object_c(ref_f, loc_id, name, namelen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RCREATE_OBJECT_C':: h5rcreate_object_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HADDR_T) :: ref_f
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              END FUNCTION h5rcreate_object_c
            END INTERFACE

            namelen = LEN(name)
            ref_f = 0
            hdferr = h5rcreate_object_c(ref_f, loc_id, name, namelen )
            ref%ref = ref_f

          END SUBROUTINE h5rcreate_object_f

!----------------------------------------------------------------------
! Name:		h5rcreate_region_f
!
! Purpose: 	Creates r eference to the dataset region
!
! Inputs:
!		loc_id		- location identifier
!		name		- name of the dataset at the specified location
!		space_id	- dataspace identifier that describes selected region
! Outputs:
!		ref		- reference to the dataset region
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
! Comment:		This is a module procedure for the h5rcreate_f
!			subroutine.
!----------------------------------------------------------------------

          SUBROUTINE h5rcreate_region_f(loc_id, name, space_id, ref, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! Location identifier
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset at location specified
                                                   ! by loc_id identifier
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataset's dataspace identifier
            TYPE(hdset_reg_ref_t_f), INTENT(OUT) :: ref ! Dataset region reference
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

            INTEGER :: namelen                     ! Name length
            INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference

!            INTEGER, EXTERNAL :: h5fcreate_region_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RCREATE_REGION_C':: h5rcreate_region_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
!              INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
              INTEGER :: ref_f(REF_REG_BUF_LEN)
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(HID_T), INTENT(IN) :: space_id
              END FUNCTION h5rcreate_region_c
            END INTERFACE

            namelen = LEN(name)
            ref_f = 0
            hdferr = h5rcreate_region_c(ref_f, loc_id, name, namelen, space_id )
            ref%ref = ref_f

          END SUBROUTINE h5rcreate_region_f

!----------------------------------------------------------------------
! Name:		h5rdereference_object_f
!
! Purpose: 	Opens the HDF5 object referenced
!
! Inputs:
!		dset_id		- identifier of the dataset containing
!				  reference
!		ref		- reference to open
! Outputs:
!		obj_id		- object_identifier
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
! Comment:		This is a module procedure for the h5rdereference_f
!			subroutine.
!----------------------------------------------------------------------


          SUBROUTINE h5rdereference_object_f(dset_id, ref, obj_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference
            INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Object identifier
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

            INTEGER(HADDR_T) :: ref_f          ! Local buffer to pass reference

!            INTEGER, EXTERNAL :: h5h5rdereference_object_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5rdereference_object_c(dset_id, ref_f, obj_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
           !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RDEREFERENCE_OBJECT_C':: h5rdereference_object_c
              !DEC$ENDIF
!              INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HADDR_T) :: ref_f
              INTEGER(HID_T), INTENT(OUT) :: obj_id
              END FUNCTION h5rdereference_object_c
            END INTERFACE

            ref_f = ref%ref
            hdferr = h5rdereference_object_c(dset_id, ref_f, obj_id )

          END SUBROUTINE h5rdereference_object_f

!----------------------------------------------------------------------
! Name:		h5rdereference_region_f
!
! Purpose: 	Opens the dataset region
!
! Inputs:
!		dset_id		- identifier of the dataset containing
!				  reference to teh regions
!		ref		- reference to open
! Outputs:
!		obj_id		- dataspace identifier
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
! Comment:		This is a module procedure for the h5rdereference_f
!			subroutine.
!----------------------------------------------------------------------


          SUBROUTINE h5rdereference_region_f(dset_id, ref, obj_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref   ! Object reference
            INTEGER(HID_T), INTENT(OUT) :: obj_id   ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

            INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference

!            INTEGER, EXTERNAL :: h5rdereference_region_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5rdereference_region_c(dset_id, ref_f, obj_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
           !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RDEREFERENCE_REGION_C':: h5rdereference_region_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
!              INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
              INTEGER :: ref_f(REF_REG_BUF_LEN)
              INTEGER(HID_T), INTENT(OUT) :: obj_id
              END FUNCTION h5rdereference_region_c
            END INTERFACE

            ref_f = ref%ref
            hdferr = h5rdereference_region_c(dset_id, ref_f, obj_id )

          END SUBROUTINE h5rdereference_region_f

!----------------------------------------------------------------------
! Name:		h5rget_region_region_f
!
! Purpose: 	Retrieves a dataspace with the specified region selected
!
! Inputs:
!		dset_id		- identifier of the dataset containing
!				  reference to the regions
!		ref		- reference to open
! Outputs:
!		space_id	- dataspace identifier
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
! Comment:		This is a module procedure for the h5rget_region_f
!			subroutine.
!----------------------------------------------------------------------



          SUBROUTINE h5rget_region_region_f(dset_id, ref, space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref   ! Dataset region reference
            INTEGER(HID_T), INTENT(OUT) :: space_id   ! Space identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: ref_f(REF_REG_BUF_LEN)          ! Local buffer to pass reference

!            INTEGER, EXTERNAL :: h5rget_region_region_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5rget_region_region_c(dset_id, ref_f, space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
           !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_REGION_REGION_C':: h5rget_region_region_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
!              INTEGER, PARAMETER :: REF_REG_BUF_LEN = 3
              INTEGER :: ref_f(REF_REG_BUF_LEN)
              INTEGER(HID_T), INTENT(OUT) :: space_id
              END FUNCTION h5rget_region_region_c
            END INTERFACE

            ref_f = ref%ref
            hdferr = h5rget_region_region_c(dset_id, ref_f, space_id )

          END SUBROUTINE h5rget_region_region_f

!----------------------------------------------------------------------
! Name:		h5rget_object_type_obj_f
!
! Purpose: 	Retrieves the type of object that an object reference points to.
!
! Inputs:
!		dset_id		- identifier of the dataset containing
!				  reference to the objects
!		ref		- reference to open
! Outputs:
!		obj_type	- object_type, possible values:
!					  H5G_UNKNOWN_F     (-1)
!					  H5G_GROUP_F        0
!					  H5G_DATASET_F      1
! 					  H5G_TYPE_F         2
!					  H5G_LINK_F         3
!
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
! Comment:		This is a module procedure for the h5rget_object_type_f
!			subroutine.
!----------------------------------------------------------------------


          SUBROUTINE h5rget_object_type_obj_f(dset_id, ref, obj_type, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            TYPE(hobj_ref_t_f), INTENT(IN) :: ref   ! Object reference
            INTEGER, INTENT(OUT) :: obj_type   ! Object type
                                               !  H5G_UNKNOWN_F     (-1)
                                               !  H5G_GROUP_F        0
                                               !  H5G_DATASET_F      1
                                               !  H5G_TYPE_F         2
                                               !  H5G_LINK_F         3

            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER(HADDR_T) :: ref_f          ! Local buffer to pass reference

!            INTEGER, EXTERNAL :: h5rget_object_type_obj_c
!  Interface is needed for MS FORTRAN
!
            INTERFACE
              INTEGER FUNCTION h5rget_object_type_obj_c(dset_id, ref_f, obj_type)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
           !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_OBJECT_TYPE_OBJ_C':: h5rget_object_type_obj_c
              !DEC$ENDIF
!              INTEGER, PARAMETER :: REF_OBJ_BUF_LEN = 2
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HADDR_T) :: ref_f
              INTEGER, INTENT(OUT) :: obj_type
              END FUNCTION h5rget_object_type_obj_c
            END INTERFACE

            ref_f = ref%ref
            hdferr = h5rget_object_type_obj_c(dset_id, ref_f, obj_type )

          END SUBROUTINE h5rget_object_type_obj_f

!----------------------------------------------------------------------
! Name:		h5rget_name_object_f
!
! Purpose: 	Retrieves a name of a referenced object.
!
! Inputs:
!                   loc_id - Identifier for the dataset containing the reference or for the group that dataset is in.
!                      ref - An object or dataset region reference.
!
! Outputs:
!                     name - A name associated with the referenced object or dataset region.
!
!		hdferr:    - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!                     size - The size of the name buffer.
!
! Programmer:	M.S. Breitenfeld
!		March 28, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------


  SUBROUTINE h5rget_name_object_f(loc_id,  ref, name, hdferr, size)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for the dataset containing the reference
                                           ! or for the group that dataset is in.
    TYPE(hobj_ref_t_f), INTENT(IN) :: ref  ! Object reference
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size   ! The size of the name buffer,
                                           ! returning 0 (zero) if no name is associated with the identifier
    CHARACTER(LEN=*), INTENT(OUT) :: name  ! A name associated with the referenced object or dataset region.
    INTEGER, INTENT(OUT) :: hdferr         ! Error code
    INTEGER(HADDR_T) :: ref_f              ! Local buffer to pass reference

    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len

    INTERFACE
       INTEGER FUNCTION h5rget_name_object_c(loc_id, ref_f, name, name_len, size_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_NAME_OBJECT_C':: h5rget_name_object_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(SIZE_T)  :: size_default
         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER(HADDR_T) :: ref_f

         INTEGER(SIZE_T) :: name_len
       END FUNCTION h5rget_name_object_c
    END INTERFACE

    name_len=LEN(name)

    ref_f = ref%ref
    hdferr = h5rget_name_object_c(loc_id, ref_f, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_object_f

!----------------------------------------------------------------------
! Name:		h5rget_name_region_f
!
! Purpose: 	Retrieves a name of a dataset region.
!
! Inputs:
!                   loc_id - Identifier for the dataset containing the reference or for the group that dataset is in.
!                      ref - An object or dataset region reference.
!
! Outputs:
!                     name - A name associated with the referenced object or dataset region.
!
!		hdferr:    - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!                     size - The size of the name buffer.
!
! Programmer:	M.S. Breitenfeld
!		March 28, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------


  SUBROUTINE h5rget_name_region_f(loc_id, ref, name, hdferr, size)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! Identifier for the dataset containing the reference
                                           ! or for the group that dataset is in.
    TYPE(hdset_reg_ref_t_f), INTENT(IN) :: ref  ! Object reference
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size   ! The size of the name buffer,
                                           ! returning 0 (zero) if no name is associated with the identifier
    CHARACTER(LEN=*), INTENT(OUT) :: name  ! A name associated with the referenced object or dataset region.
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

    INTEGER :: ref_f(REF_REG_BUF_LEN)      ! Local buffer to pass reference
    INTEGER(SIZE_T) :: size_default
    INTEGER(SIZE_T) :: name_len

    INTERFACE
       INTEGER FUNCTION h5rget_name_region_c(loc_id, ref_f, name, name_len, size_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5RGET_NAME_REGION_C':: h5rget_name_region_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(SIZE_T)  :: size_default
         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER :: ref_f(REF_REG_BUF_LEN)

         INTEGER(SIZE_T) :: name_len
       END FUNCTION h5rget_name_region_c
    END INTERFACE

    name_len=LEN(name)

    ref_f = ref%ref
    hdferr = h5rget_name_region_c(loc_id, ref_f, name, name_len, size_default)

    IF(PRESENT(size)) size = size_default

  END SUBROUTINE h5rget_name_region_f

END MODULE H5R
