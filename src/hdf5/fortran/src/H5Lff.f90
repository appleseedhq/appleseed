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
! This file contains Fortran90 interfaces for H5L functions.
!
MODULE H5L

  USE H5GLOBAL

CONTAINS

!----------------------------------------------------------------------
! Name:		h5lcopy_f
!
! Purpose: 	Copies a link from one location to another.
!
! Inputs:
!     src_loc_id - Location identifier of the source link
!       src_name - Name of the link to be copied
!    dest_loc_id - Location identifier specifying the destination of the copy
!      dest_name - Name to be assigned to the NEW copy
!         loc_id - Identifier of the file or group containing the object
!           name - Name of the link to delete
!
! Outputs:
!         hdferr - error code:
!			Success:  0
!			Failure: -1
! Optional parameters:
!        lcpl_id - Link creation property list identifier
!        lapl_id - Link access property list identifier
!
! Programmer:	M.S. Breitenfeld
!		February 27, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------
  SUBROUTINE h5lcopy_f(src_loc_id, src_name, dest_loc_id, dest_name, hdferr, &
       lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: src_loc_id  ! Location identifier of the source link
    CHARACTER(LEN=*), INTENT(IN) :: src_name   ! Name of the link to be copied
    INTEGER(HID_T), INTENT(IN) :: dest_loc_id ! Location identifier specifying the destination of the copy
    CHARACTER(LEN=*), INTENT(IN) :: dest_name ! Name to be assigned to the NEW copy

    INTEGER, INTENT(OUT) :: hdferr        ! Error code:
                                          ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list identifier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: src_namelen
    INTEGER(SIZE_T) :: dest_namelen


!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lcopy_c(src_loc_id, src_name, src_namelen, dest_loc_id, dest_name, dest_namelen, &
            lcpl_id_default, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LCOPY_C'::h5lcopy_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: src_name, dest_name
         INTEGER(HID_T), INTENT(IN) :: src_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: src_name
         INTEGER(HID_T), INTENT(IN) :: dest_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: dest_name

         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default

         INTEGER(SIZE_T) :: src_namelen
         INTEGER(SIZE_T) :: dest_namelen
       END FUNCTION h5lcopy_c
    END INTERFACE

    src_namelen  = LEN(src_name)
    dest_namelen = LEN(dest_name)

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lcopy_c(src_loc_id, src_name, src_namelen, dest_loc_id, dest_name, dest_namelen, &
         lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5lcopy_f

!----------------------------------------------------------------------
! Name:		h5ldelete_f
!
! Purpose: 	Removes a link from a group.
!
! Inputs:
!         loc_id   - Identifier of the file or group containing the object
!         name     - Name of the link to delete
!
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!         lapl_id  - Link access property list identifier
!
! Programmer:	M.S. Breitenfeld
!		January, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------
  SUBROUTINE h5ldelete_f(loc_id, name, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! Identifier of the file or group containing the object
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of the link to delete
    INTEGER, INTENT(OUT) :: hdferr        ! Error code:
                                          ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: namelen


!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5ldelete_c(loc_id, name, namelen, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LDELETE_C'::h5ldelete_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: namelen
       END FUNCTION h5ldelete_c
    END INTERFACE

    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5ldelete_c(loc_id, name, namelen, lapl_id_default)

  END SUBROUTINE h5ldelete_f

!----------------------------------------------------------------------
! Name:		H5Lcreate_soft_f
!
! Purpose: 	Creates a soft link to an object.
!
! Inputs:
!       target_path - Path to the target object, which is not required to exist.
!       link_loc_id - The file or group identifier for the new link.
!       link_name   - The name of the new link.
!
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!       lcpl_id     - Link creation property list identifier.
!       lapl_id     - Link access property list identifier.
!
! Programmer:	M.S. Breitenfeld
!		February 20, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------
  SUBROUTINE h5lcreate_soft_f(target_path, link_loc_id, link_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: target_path     ! Path to the target object, which is not required to exist.
    INTEGER(HID_T), INTENT(IN) :: link_loc_id     ! The file or group identifier for the new link.
    CHARACTER(LEN=*), INTENT(IN) :: link_name       ! The name of the new link.
    INTEGER, INTENT(OUT) :: hdferr        ! Error code:
                                          ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list identifier.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier.

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: target_path_len
    INTEGER(SIZE_T) :: link_name_len

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lcreate_soft_c(target_path, target_path_len, &
            link_loc_id, &
            link_name,link_name_len, &
            lcpl_id_default, lapl_id_default )
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LCREATE_SOFT_C'::h5lcreate_soft_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: target_path, link_name
         CHARACTER(LEN=*), INTENT(IN) :: target_path
         INTEGER(SIZE_T) :: target_path_len
         INTEGER(HID_T), INTENT(IN) ::   link_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: link_name
         INTEGER(SIZE_T) :: link_name_len
         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lcreate_soft_c
    END INTERFACE

    target_path_len = LEN(target_path)
    link_name_len = LEN(link_name)

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lcreate_soft_c(target_path, target_path_len,&
         link_loc_id, &
         link_name, link_name_len, &
         lcpl_id_default, lapl_id_default )

  END SUBROUTINE h5lcreate_soft_f

!----------------------------------------------------------------------
! Name:		H5Lcreate_hard_f
!
! Purpose: 	Creates a hard link to an object.
!
! Inputs:
!
!    obj_loc_id - The file or group identifier for the target object.
!      obj_name - Name of the target object, which must already exist.
!   link_loc_id - The file or group identifier for the new link.
!     link_name - The name of the new link.
!
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!       lcpl_id     - Link creation property list identifier.
!       lapl_id     - Link access property list identifier.
!
! Programmer:	M.S. Breitenfeld
!		February 27, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------
  SUBROUTINE h5lcreate_hard_f(obj_loc_id, obj_name, link_loc_id, link_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_loc_id  ! The file or group identifier for the target object.
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of the target object, which must already exist.
    INTEGER(HID_T), INTENT(IN) :: link_loc_id ! The file or group identifier for the new link.
    CHARACTER(LEN=*), INTENT(IN) :: link_name ! The name of the new link.

    INTEGER, INTENT(OUT) :: hdferr        ! Error code:
                                          ! 0 on success and -1 on failure

    INTEGER(HID_T), OPTIONAL, INTENT(IN) ::   lcpl_id         ! Link creation property list identifier.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) ::   lapl_id         ! Link access property list identifier.

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: link_namelen

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lcreate_hard_c(obj_loc_id, obj_name, obj_namelen, &
            link_loc_id, link_name, link_namelen, lcpl_id_default, lapl_id_default)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LCREATE_HARD_C'::h5lcreate_hard_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, link_name
         INTEGER(HID_T), INTENT(IN) :: obj_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(HID_T), INTENT(IN) :: link_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: link_name
         INTEGER(SIZE_T) :: obj_namelen
         INTEGER(SIZE_T) :: link_namelen
         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lcreate_hard_c
    END INTERFACE
    obj_namelen = LEN(obj_name)
    link_namelen = LEN(link_name)

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lcreate_hard_c(obj_loc_id, obj_name, obj_namelen, &
            link_loc_id, link_name, link_namelen, lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5lcreate_hard_f

!----------------------------------------------------------------------
! Name:		H5Lcreate_external_f
!
! Purpose: 	Creates a soft link to an object in a different file.
!
! Inputs:
!
!    file_name - Name of the file containing the target object. Neither the file nor the target object is
!                required to exist. May be the file the link is being created in.
!     obj_name - Path within the target file to the target object.
!  link_loc_id - The file or group identifier for the new link.
!    link_name - The name of the new link.
!
! Outputs:
!		hdferr:	- error code
!  		            Success:  0
!		            Failure: -1
! Optional parameters:
!       lcpl_id     - Link creation property list identifier.
!       lapl_id     - Link access property list identifier.
!
! Programmer:	M.S. Breitenfeld
!		February 27, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------
  SUBROUTINE h5lcreate_external_f(file_name, obj_name, link_loc_id, link_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: file_name  ! Name of the file containing the target object. Neither
                                              ! the file nor the target object is required to exist.
                                              ! May be the file the link is being created in.
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of the target object, which must already exist.
    INTEGER(HID_T), INTENT(IN) :: link_loc_id ! The file or group identifier for the new link.
    CHARACTER(LEN=*), INTENT(IN) :: link_name ! The name of the new link.

    INTEGER, INTENT(OUT) :: hdferr        ! Error code:
                                          ! 0 on success and -1 on failure

    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list identifier.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier.

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: file_namelen
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: link_namelen

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lcreate_external_c(file_name, file_namelen, obj_name, obj_namelen, &
            link_loc_id, link_name, link_namelen, lcpl_id_default, lapl_id_default)

         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LCREATE_EXTERNAL_C'::h5lcreate_external_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: file_name, obj_name, link_name
         CHARACTER(LEN=*), INTENT(IN) :: file_name
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(HID_T), INTENT(IN) :: link_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: link_name
         INTEGER(SIZE_T) :: file_namelen
         INTEGER(SIZE_T) :: obj_namelen
         INTEGER(SIZE_T) :: link_namelen
         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lcreate_external_c
    END INTERFACE
    file_namelen = LEN(file_name)
    obj_namelen = LEN(obj_name)
    link_namelen = LEN(link_name)

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lcreate_external_c(file_name, file_namelen, obj_name, obj_namelen, &
            link_loc_id, link_name, link_namelen, lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5lcreate_external_f

!----------------------------------------------------------------------
! Name:		h5ldelete_by_idx_f
!
! Purpose:  	Removes the nth link in a group.
! Inputs:
!		loc_id     - File or group identifier specifying location of subject group
!               group_name - Name of subject group
!               index_field   - Type of index; Possible values are:
!
!                  H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!                  H5_INDEX_NAME_F          - Index on names
!                  H5_INDEX_CRT_ORDER_F     - Index on creation order
!                  H5_INDEX_N_F	            - Number of indices defined
!
!               order - Order within field or index; Possible values are:
!
!                  H5_ITER_UNKNOWN_F   - Unknown order
!                  H5_ITER_INC_F       - Increasing order
!                  H5_ITER_DEC_F       - Decreasing order
!                  H5_ITER_NATIVE_F    - No particular order, whatever is fastest
!                  H5_ITER_N_F	       - Number of iteration orders
!
!               n           - Link for which to retrieve information
! Outputs:
!		hdferr:     - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!               lapl_id    - Link access property list
!
! Programmer:	M.S. Breitenfeld
!		February 29, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5ldelete_by_idx_f(loc_id, group_name, index_field, order, n, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id       ! Identifer for object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: group_name ! Name of object, relative to location,
                                               !  from which attribute is to be removed
    INTEGER, INTENT(IN) :: index_field         ! Type of index; Possible values are:
                                               !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                               !    H5_INDEX_NAME_F      - Index on names
                                               !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                               !    H5_INDEX_N_F	      - Number of indices defined
    INTEGER, INTENT(IN) :: order               ! Order in which to iterate over index; Possible values are:
                                               !    H5_ITER_UNKNOWN_F  - Unknown order
                                               !    H5_ITER_INC_F      - Increasing order
                                               !    H5_ITER_DEC_F      - Decreasing order
                                               !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest
                                               !    H5_ITER_N_F	    - Number of iteration orders
    INTEGER(HSIZE_T), INTENT(IN) :: n      ! Offset within index
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id     ! Link access property list

    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: group_namelen

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5ldelete_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LDELETE_BY_IDX_C'::h5ldelete_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: group_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: group_name
         INTEGER, INTENT(IN) :: index_field
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: group_namelen
       END FUNCTION h5ldelete_by_idx_c
    END INTERFACE

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    group_namelen = LEN(group_name)
    hdferr = h5ldelete_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, lapl_id_default)

  END SUBROUTINE h5ldelete_by_idx_f

!----------------------------------------------------------------------
! Name:	        H5Lexists_f
!
! Purpose:  	Check if a link with a particular name exists in a group.
!
! Inputs:
!      loc_id - Identifier of the file or group to query.
!        name - Link name to check
!
! Outputs:
!   link_exists  - link exists status (.TRUE.,.FALSE.)
!        hdferr  - error code
!	              Success:  0
!		      Failure: -1
! Optional parameters:
!	 lapl_id - Link access property list identifier.
!
! Programmer:	M. S. Breitenfeld
!		February 29, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5lexists_f(loc_id, name, link_exists, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! Identifier of the file or group to query.
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Link name to check.
    LOGICAL, INTENT(OUT) :: link_exists   ! .TRUE. if exists, .FALSE. otherwise
    INTEGER, INTENT(OUT) :: hdferr        ! Error code:
                                          ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id
                                          ! Link access property list identifier.
    INTEGER :: link_exists_c
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T)  :: namelen
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lexists_c(loc_id, name, namelen, lapl_id_default, link_exists_c)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LEXISTS_C'::h5lexists_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(SIZE_T), INTENT(IN) :: namelen
         INTEGER, INTENT(OUT) :: link_exists_c
         INTEGER(HID_T) :: lapl_id_default

       END FUNCTION h5lexists_c
    END INTERFACE

    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lexists_c(loc_id, name, namelen, lapl_id_default, link_exists_c)

    link_exists = .FALSE.
    IF(link_exists_c.GT.0) link_exists = .TRUE.

  END SUBROUTINE h5lexists_f

!----------------------------------------------------------------------
! Name:		h5lget_info_f
!
! Purpose:  	Returns information about a link.
!
! Inputs:
!		link_loc_id - File or group identifier.
!               link_name - Name of the link for which information is being sought
!
! Outputs:  NOTE: In C these are contained in the structure H5L_info_t
!
!              cset - indicates the character set used for link’s name.
!      corder - specifies the link’s creation order position.
!f_corder_valid - indicates whether the value in corder is valid.
!         link_type -  specifies the link class:
!     	                H5L_TYPE_HARD_F      - Hard link
!     	                H5L_TYPE_SOFT_F      - Soft link
!     	                H5L_TYPE_EXTERNAL_F  - External link
!     	                H5L_TYPE_ERROR_F    - Error
!           address - If the link is a hard link, address specifies the file address that the link points to
!          val_size - If the link is a symbolic link, val_size will be the length of the link value, e.g.,
!                     the length of the name of the pointed-to object with a null terminator.
!            hdferr - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!      lapl_id - Link access property list
!
! Programmer:	M. S. Breitenfeld
!		February 29, 2008
!
! Modifications:
!   Changed the link_type names to match those in C (bug 1720) from,
!   H5L_LINK_HARD_F, H5L_LINK_SOFT_F,H5L_LINK_EXTERNAL_F,H5L_LINK_ERROR_F
!   to
!   H5L_TYPE_HARD_F, H5L_TYPE_SOFT_F,H5L_TYPE_EXTERNAL_F,H5L_TYPE_ERROR_F
!   MSB January 8, 2010.
!
!----------------------------------------------------------------------

  SUBROUTINE h5lget_info_f(link_loc_id, link_name, &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       hdferr, lapl_id)
    IMPLICIT NONE

    INTEGER(HID_T), INTENT(IN) :: link_loc_id ! File or group identifier.
    CHARACTER(LEN=*), INTENT(IN) :: link_name ! Name of the link for which information is being sought

! Outputs:  NOTE: In C these are contained in the structure H5L_info_t
    INTEGER, INTENT(OUT) :: cset ! Indicates the character set used for the link’s name.
    INTEGER, INTENT(OUT) :: corder ! Specifies the link’s creation order position.
    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the value in corder is valid.
    INTEGER, INTENT(OUT) :: link_type ! Specifies the link class:
     	                              !  H5L_TYPE_HARD_F      - Hard link
     	                              !  H5L_TYPE_SOFT_F      - Soft link
     	                              !  H5L_TYPE_EXTERNAL_F  - External link
     	                              !  H5L_TYPE_ERROR _F    - Error
    INTEGER(HADDR_T), INTENT(OUT) :: address  ! If the link is a hard link, address specifies the file address that the link points to
    INTEGER(SIZE_T), INTENT(OUT) :: val_size ! If the link is a symbolic link, val_size will be the length of the link value, e.g.,
                                             ! the length of the name of the pointed-to object with a null terminator.
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list

    INTEGER(SIZE_T) :: link_namelen
    INTEGER(HID_T) :: lapl_id_default
    INTEGER :: corder_valid

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lget_info_c(link_loc_id, link_name, link_namelen, &
            cset, corder, corder_valid, link_type, address, val_size, &
            lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LGET_INFO_C'::h5lget_info_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: link_name
         INTEGER(HID_T), INTENT(IN) :: link_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: link_name
         INTEGER, INTENT(OUT) :: cset
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: link_type
         INTEGER(HADDR_T), INTENT(OUT) :: address
         INTEGER(SIZE_T), INTENT(OUT) :: val_size
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: link_namelen
         INTEGER :: corder_valid
       END FUNCTION h5lget_info_c
    END INTERFACE

    link_namelen = LEN(link_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lget_info_c(link_loc_id, link_name, link_namelen, &
         cset, corder, corder_valid, link_type, &
         address, val_size, &
         lapl_id_default)

    f_corder_valid =.FALSE.
    IF(corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5lget_info_f

!----------------------------------------------------------------------
! Name:	   h5lget_info_by_idx_f
!
! Purpose:  Retrieves metadata for a link in a group, according to the order within a field or index.
!
! Inputs:
!	loc_id - File or group identifier specifying location of subject group
!   group_name - Name of subject group
!  index_field - Index or field which determines the order
!        order - Order within field or index
!            n - Link for which to retrieve information
!
! Outputs:  NOTE: In C these are defined as a structure: H5L_info_t
!    corder_valid   - indicates whether the creation order data is valid for this attribute
!    corder         - is a positive integer containing the creation order of the attribute
!    cset           - indicates the character set used for the attribute’s name
!    address        - If the link is a hard link, address specifies the file address that the link points to
!    val_size       - If the link is a symbolic link, val_size will be the length of the link value, e.g.,
!                     the length of the name of the pointed-to object with a null terminator.
!    hdferr         - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!      lapl_id - Link access property list
!
! Programmer: M.S. Breitenfeld
!	      February 29, 2008
!
! Modifications:
!   Changed the link_type names to match those in C (bug 1720) from,
!   H5L_LINK_HARD_F, H5L_LINK_SOFT_F,H5L_LINK_EXTERNAL_F,H5L_LINK_ERROR_F
!   to
!   H5L_TYPE_HARD_F, H5L_TYPE_SOFT_F,H5L_TYPE_EXTERNAL_F,H5L_TYPE_ERROR_F
!   MSB January 8, 2010.
!
!----------------------------------------------------------------------
  SUBROUTINE h5lget_info_by_idx_f(loc_id, group_name, index_field, order, n, &
       link_type, f_corder_valid, corder, cset, address, val_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier specifying location of subject group
    CHARACTER(LEN=*), INTENT(IN) :: group_name ! Name of subject group
    INTEGER, INTENT(IN) :: index_field  ! Index or field which determines the order
                                        !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                        !    H5_INDEX_NAME_F      - Index on names
                                        !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                        !    H5_INDEX_N_F	      - Number of indices defined
    INTEGER, INTENT(IN) :: order        ! Order in which to iterate over index; Possible values are:
                                        !    H5_ITER_UNKNOWN_F  - Unknown order
                                        !    H5_ITER_INC_F      - Increasing order
                                        !    H5_ITER_DEC_F      - Decreasing order
                                        !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest
    INTEGER(HSIZE_T), INTENT(IN) :: n   ! Attribute’s position in index
    INTEGER, INTENT(OUT) :: link_type ! Specifies the link class:
     	                              !  H5L_TYPE_HARD_F      - Hard link
     	                              !  H5L_TYPE_SOFT_F      - Soft link
     	                              !  H5L_TYPE_EXTERNAL_F  - External link
     	                              !  H5L_TYPE_ERROR _F    - Error
    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
    INTEGER, INTENT(OUT) :: cset ! Indicates the character set used for the attribute’s name
    INTEGER(HADDR_T), INTENT(OUT) :: address  ! If the link is a hard link, address specifies the file address that the link points to
    INTEGER(SIZE_T), INTENT(OUT) :: val_size  ! If the link is a symbolic link, val_size will be the length of the link value, e.g.,
                                              ! the length of the name of the pointed-to object with a null terminator.
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
    INTEGER :: corder_valid
    INTEGER(SIZE_T)  :: group_namelen
    INTEGER(HID_T) :: lapl_id_default

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lget_info_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
            link_type, corder_valid, corder, cset, address, val_size, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LGET_INFO_BY_IDX_C'::h5lget_info_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: group_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: group_name
         INTEGER(SIZE_T)  :: group_namelen
         INTEGER, INTENT(IN) :: index_field
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER, INTENT(OUT) :: link_type
         INTEGER :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HADDR_T), INTENT(OUT) :: address
         INTEGER(SIZE_T), INTENT(OUT) :: val_size
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lget_info_by_idx_c
    END INTERFACE

    group_namelen = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5lget_info_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
         link_type, corder_valid, corder, cset, address, val_size, lapl_id_default)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5lget_info_by_idx_f

!----------------------------------------------------------------------
! Name:	   h5lis_registered_f
!
! Purpose:  Determines whether a class of user-defined links is registered.
!
! Inputs:
!      link_cls_id - User-defined link class identifier
!
! Outputs:
!       registered - .TRUE. - if the link class has been registered
!                    .FALSE. - if it is unregistered
!           hdferr - Error code
!		       Success:  0
!	               Failure: -1
! Optional parameters:
!                      None
!
! Programmer: M.S. Breitenfeld
!	      February 29, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5lis_registered_f(link_cls_id, registered, hdferr)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: link_cls_id  ! User-defined link class identifier
    LOGICAL, INTENT(OUT) :: registered  ! .TRUE. - if the link class has been registered and
                                        ! .FALSE. - if it is unregistered
    INTEGER, INTENT(OUT) :: hdferr      ! Error code:
                                        ! 0 on success and -1 on failure
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lis_registered_c(link_cls_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LIS_REGISTERED_C'::h5lis_registered_c
         !DEC$ENDIF
         INTEGER, INTENT(IN) :: link_cls_id  ! User-defined link class identifier
       END FUNCTION h5lis_registered_c
    END INTERFACE

    hdferr = h5lis_registered_c(link_cls_id)

    IF(hdferr.GT.0)THEN
       registered = .TRUE.
    ELSE IF(hdferr.EQ.0)THEN
       registered = .FALSE.
    ENDIF

  END SUBROUTINE h5lis_registered_f

!----------------------------------------------------------------------
! Name:	   h5lmove_f
!
! Purpose:  Renames a link within an HDF5 file.
!
! Inputs:
!    src_loc_id  - Original file or group identifier.
!    src_name    - Original link name.
!    dest_loc_id - Destination file or group identifier.
!    dest_name   - NEW link name.
!
! Outputs:
!         hdferr - Error code
!		     Success:  0
!	             Failure: -1
! Optional parameters:
!    lcpl_id  - Link creation property list identifier to be associated WITH the NEW link.
!    lapl_id  - Link access property list identifier to be associated WITH the NEW link.
!
! Programmer: M.S. Breitenfeld
!	      March 3, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5lmove_f(src_loc_id, src_name, dest_loc_id, dest_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: src_loc_id  ! Original file or group identifier.
    CHARACTER(LEN=*), INTENT(IN) :: src_name  ! Original link name.
    INTEGER(HID_T), INTENT(IN) :: dest_loc_id ! Destination file or group identifier.
    CHARACTER(LEN=*), INTENT(IN) :: dest_name ! NEW link name.
    INTEGER, INTENT(OUT) :: hdferr     ! Error code:
                                              ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list identifier
                                                    ! to be associated WITH the NEW link.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier
                                                    ! to be associated WITH the NEW link.

    INTEGER(SIZE_T) :: src_namelen
    INTEGER(SIZE_T) :: dest_namelen

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: lapl_id_default

!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lmove_c(src_loc_id, src_name, src_namelen, dest_loc_id, &
            dest_name, dest_namelen, lcpl_id_default, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LMOVE_C'::h5lmove_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: src_name, dest_name
         INTEGER(HID_T), INTENT(IN) :: src_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: src_name
         INTEGER(SIZE_T) :: src_namelen
         INTEGER(HID_T), INTENT(IN) :: dest_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: dest_name
         INTEGER(SIZE_T) :: dest_namelen

         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: lapl_id_default

       END FUNCTION h5lmove_c
    END INTERFACE

    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    src_namelen = LEN(src_name)
    dest_namelen = LEN(dest_name)

    hdferr = H5Lmove_c(src_loc_id, src_name, src_namelen, dest_loc_id, &
         dest_name, dest_namelen, lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5lmove_f

!----------------------------------------------------------------------
! Name:		h5lget_name_by_idx_f
!
! Purpose:      Retrieves name of the nth link in a group, according to the order within a specified field or index.
!
! Inputs:
!   loc_id      - File or group identifier specifying location of subject group
!   group_name  - Name of subject group
!   index_field - Index or field which determines the order
!   order       - Order within field or index
!   n           - Link for which to retrieve information
!
! Outputs:
!   name        - Buffer in which link value is returned
!   hdferr      - error code
!		      Success:  0
!		      Failure: -1
!
! Optional parameters:
!    lapl_id    - List access property list identifier.
!    size       - Maximum number of characters of link value to be returned.
!
! Programmer:	M. S. Breitenfeld
!		March 10, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5lget_name_by_idx_f(loc_id, group_name, index_field, order, n, &
        name, hdferr, size, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id       ! File or group identifier specifying location of subject group
    CHARACTER(LEN=*), INTENT(IN) :: group_name ! Name of subject group
    INTEGER, INTENT(IN) :: index_field  ! Index or field which determines the order
                                        !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                        !    H5_INDEX_NAME_F      - Index on names
                                        !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                        !    H5_INDEX_N_F	      - Number of indices defined
    INTEGER, INTENT(IN) :: order        ! Order in which to iterate over index; Possible values are:
                                        !    H5_ITER_UNKNOWN_F  - Unknown order
                                        !    H5_ITER_INC_F      - Increasing order
                                        !    H5_ITER_DEC_F      - Decreasing order
                                        !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest
    INTEGER(HSIZE_T), INTENT(IN) :: n   ! Attribute’s position in index
    CHARACTER(LEN=*), INTENT(OUT) :: name ! Buffer in which link value is returned
    INTEGER, INTENT(OUT) :: hdferr        ! Error code:
                                          ! 0 on success and -1 on failure

    INTEGER(SIZE_T)  :: group_namelen
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size   ! Indicates the size, in the number of characters, of the link
    INTEGER(SIZE_T) :: size_default


!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5lget_name_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
             size_default, name, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LGET_NAME_BY_IDX_C'::h5lget_name_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: group_name, name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: group_name
         INTEGER(SIZE_T)  :: group_namelen
         INTEGER, INTENT(IN) :: index_field
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(SIZE_T) :: size_default
         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER(HID_T) :: lapl_id_default
       END FUNCTION h5lget_name_by_idx_c
    END INTERFACE

    group_namelen = LEN(group_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    size_default = LEN(name)

    hdferr = h5lget_name_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
             size_default, name, lapl_id_default)

    IF(PRESENT(size)) size = size_default


  END SUBROUTINE h5lget_name_by_idx_f


! HAS PROBLEM WITH void pointer in C

!!$!----------------------------------------------------------------------
!!$! Name:		h5lget_val_by_idx_f
!!$!
!!$! Purpose:      Returns the link value of a link, according to the order of
!!$!               an index.  For symbolic links, this is the path to which the
!!$!               link points, including the null terminator.  For user-defined
!!$!               links, it is the link buffer.
!!$! Inputs:
!!$!   loc_id      - File or group identifier specifying location of subject group
!!$!   group_name  - Name of subject group
!!$!   index_field - Index or field which determines the order
!!$!   order       - Order within field or index
!!$!   n           - Link for which to retrieve information
!!$!   size       - Maximum number of characters of link value to be returned.
!!$!
!!$! Outputs:  NOTE: In C these are defined as a structure: H5L_info_t
!!$!    corder_valid   - indicates whether the creation order data is valid for this attribute
!!$!    corder         - is a positive integer containing the creation order of the attribute
!!$!    cset           - indicates the character set used for the attribute’s name
!!$!    data_size      - indicates the size, in the number of characters, of the attribute
!!$!    hdferr         - error code
!!$!				 Success:  0
!!$!				 Failure: -1
!!$! Optional parameters:
!!$!    lapl_id      - List access property list identifier.
!!$!
!!$! Programmer:	M. S. Breitenfeld
!!$!		March 3, 2008
!!$!
!!$! Modifications:  N/A
!!$!
!!$!----------------------------------------------------------------------
!!$  SUBROUTINE h5lget_val_by_idx_f(loc_id, group_name, index_field, order, n, &
!!$       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
!!$    IMPLICIT NONE
!!$    INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier specifying location of subject group
!!$    CHARACTER(LEN=*), INTENT(IN) :: group_name ! Name of subject group
!!$    INTEGER, INTENT(IN) :: index_field  ! Index or field which determines the order
!!$                                        !    H5_INDEX_UNKNOWN_F   - Unknown index type
!!$                                        !    H5_INDEX_NAME_F      - Index on names
!!$                                        !    H5_INDEX_CRT_ORDER_F - Index on creation order
!!$                                        !    H5_INDEX_N_F	      - Number of indices defined
!!$    INTEGER, INTENT(IN) :: order        ! Order in which to iterate over index; Possible values are:
!!$                                        !    H5_ITER_UNKNOWN_F  - Unknown order
!!$                                        !    H5_ITER_INC_F      - Increasing order
!!$                                        !    H5_ITER_DEC_F      - Decreasing order
!!$                                        !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest
!!$    INTEGER(HSIZE_T), INTENT(IN) :: n   ! Attribute’s position in index
!!$    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
!!$    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
!!$    INTEGER, INTENT(OUT) :: cset ! Indicates the character set used for the attribute’s name
!!$    INTEGER(HSIZE_T), INTENT(OUT) :: data_size   ! Indicates the size, in the number of characters, of the attribute
!!$    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
!!$                                         ! 0 on success and -1 on failure
!!$    INTEGER :: corder_valid
!!$    INTEGER(SIZE_T)  :: group_namelen
!!$    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
!!$    INTEGER(HID_T) :: lapl_id_default
!!$
!!$!  MS FORTRAN needs explicit interface for C functions called here.
!!$!
!!$    INTERFACE
!!$       INTEGER FUNCTION h5lget_val_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
!!$            corder_valid, corder, cset, data_size, lapl_id_default)
!!$         USE H5GLOBAL
!!$         !DEC$IF DEFINED(HDF5F90_WINDOWS)
!!$         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LGET_VAL_BY_IDX_C'::h5lget_val_by_idx_c
!!$         !DEC$ENDIF
!!$         INTEGER(HID_T), INTENT(IN) :: loc_id
!!$         CHARACTER(LEN=*), INTENT(IN) :: group_name
!!$         INTEGER(SIZE_T)  :: group_namelen
!!$         INTEGER, INTENT(IN) :: index_field
!!$         INTEGER, INTENT(IN) :: order
!!$         INTEGER(HSIZE_T), INTENT(IN) :: n
!!$         INTEGER :: corder_valid
!!$         INTEGER, INTENT(OUT) :: corder
!!$         INTEGER, INTENT(OUT) :: cset
!!$         INTEGER(HSIZE_T), INTENT(OUT) :: data_size
!!$         INTEGER(HID_T) :: lapl_id_default
!!$       END FUNCTION h5lget_val_by_idx_c
!!$    END INTERFACE
!!$
!!$    group_namelen = LEN(group_name)
!!$
!!$    lapl_id_default = H5P_DEFAULT_F
!!$    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
!!$
!!$    hdferr = h5lget_info_by_idx_c(loc_id, group_name, group_namelen, index_field, order, n, &
!!$         corder_valid, corder, cset, data_size, lapl_id_default)
!!$
!!$    f_corder_valid =.FALSE.
!!$    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.
!!$
!!$  END SUBROUTINE h5lget_val_by_idx_f



!----------------------------------------------------------------------
! Name:		h5lget_val_f
!
! Purpose:  	Returns the value of a symbolic link.
!
! Inputs:
!   link_loc_id  - File or group identifier.
!   link_name    - Link whose value is to be returned.
!   size         - Maximum number of characters of link value to be returned.
!
! Outputs:
!   linkval_buff - The buffer to hold the returned link value.
!         hdferr - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!    lapl_id      - List access property list identifier.
!
! Programmer:	M. S. Breitenfeld
!		March 3, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------

!!$  SUBROUTINE h5lget_val_f(link_loc_id, link_name, size, linkval_buff,   &
!!$       hdferr, lapl_id)
!!$    IMPLICIT NONE
!!$    INTEGER(HID_T), INTENT(IN) :: link_loc_id  ! File or group identifier.
!!$    CHARACTER(LEN=*), INTENT(IN) :: link_name  ! Link whose value is to be returned.
!!$    INTEGER(SIZE_T), INTENT(IN) :: size        !  Maximum number of characters of link value to be returned.
!!$
!!$    CHARACTER(LEN=size), INTENT(OUT) :: linkval_buff ! The buffer to hold the returned link value.
!!$    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
!!$    ! 0 on success and -1 on failure
!!$    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
!!$
!!$    INTEGER :: link_namelen
!!$    INTEGER(HID_T) :: lapl_id_default
!!$    INTEGER :: corder_valid
!!$
!!$    INTEGER :: link_namelen
!!$    INTEGER(HID_T) :: lapl_id_default
!!$
!!$!  MS FORTRAN needs explicit interface for C functions called here.
!!$!
!!$    INTERFACE
!!$       INTEGER FUNCTION h5lget_val_c(link_loc_id, link_name, link_namelen, size, linkval_buff, &
!!$            lapl_id_default)
!!$         USE H5GLOBAL
!!$         !DEC$IF DEFINED(HDF5F90_WINDOWS)
!!$         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LGET_VAL_C'::h5lget_val_c
!!$         !DEC$ENDIF
!!$         INTEGER(HID_T), INTENT(IN) :: link_loc_id  ! File or group identifier.
!!$         CHARACTER(LEN=*), INTENT(IN) :: link_name  ! Link whose value is to be returned.
!!$         INTEGER :: link_namelen
!!$         INTEGER(SIZE_T), INTENT(IN) :: size        !  Maximum number of characters of link value to be returned.
!!$
!!$         CHARACTER(LEN=size), INTENT(OUT) :: linkval_buff ! The buffer to hold the returned link value.
!!$
!!$         INTEGER :: link_namelen
!!$         INTEGER(HID_T) :: lapl_id_default
!!$
!!$       END FUNCTION h5lget_val_c
!!$    END INTERFACE
!!$
!!$    link_namelen = LEN(link_name)
!!$
!!$    lapl_id_default = H5P_DEFAULT_F
!!$    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
!!$
!!$    hdferr = h5lget_val_c(link_loc_id, link_name, link_namelen, size, linkval_buff, &
!!$         lapl_id_default)
!!$
!!$  END SUBROUTINE h5lget_val_f



!----------------------------------------------------------------------
! Name:	    H5Lregistered_f
!
! Purpose:  Registers user-defined link class or changes behavior of existing class.
!
! Inputs: NOTE: In C the following represents struct H5L_class_t:
!     version      - Version number of this struct
!     class_id     - Link class identifier
!     comment      - Comment for debugging
!     create_func  - Callback during link creation
!     move_func    - Callback after moving link
!     copy_func    - Callback after copying link
!     trav_func    - The main traversal function
!     del_func     - Callback for link deletion
!     query_func   - Callback for queries
!
! Outputs:
!           hdferr - Error code
!		       Success:  0
!	               Failure: -1
! Optional parameters:
!                      None
!
! Programmer: M.S. Breitenfeld
!	      February 29, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
!!$  SUBROUTINE H5Lregistered_f(version, class_id, comment, create_func, &
!!$       move_func, copy_func, trav_func, del_func, query_func, hdferr)
!!$    IMPLICIT NONE
!!$    INTEGER, INTENT(IN) :: version      ! Version number of this struct
!!$    INTEGER, INTENT(IN) :: class_id     ! Link class identifier
!!$    CHARACTER(LEN=*), INTENT(IN) :: comment      ! Comment for debugging
!!$    CHARACTER(LEN=*), INTENT(IN) :: create_func  ! Callback during link creation
!!$    CHARACTER(LEN=*), INTENT(IN) :: move_func    ! Callback after moving link
!!$    CHARACTER(LEN=*), INTENT(IN) :: copy_func    ! Callback after copying link
!!$    CHARACTER(LEN=*), INTENT(IN) :: trav_func    ! The main traversal function
!!$    CHARACTER(LEN=*), INTENT(IN) :: del_func     ! Callback for link deletion
!!$    CHARACTER(LEN=*), INTENT(IN) :: query_func   ! Callback for queries
!!$    INTEGER, INTENT(OUT) :: hdferr      ! Error code:
!!$                                        ! 0 on success and -1 on failure
!!$    INTEGER :: comment_len
!!$    INTEGER :: create_func_len
!!$    INTEGER :: move_func_len
!!$    INTEGER :: copy_func_len
!!$    INTEGER :: trav_func_len
!!$    INTEGER :: del_func_len
!!$    INTEGER :: query_func_len
!!$
!!$!
!!$!  MS FORTRAN needs explicit interface for C functions called here.
!!$!
!!$    INTERFACE
!!$       INTEGER FUNCTION H5Lregistered_c(version, class_id, comment, &
!!$            create_func, create_func_len, &
!!$            move_func, move_func_len, &
!!$            copy_func, copy_func_len, &
!!$            trav_func, trav_func_len, &
!!$            del_func, del_func_len, &
!!$            query_func,query_func_len)
!!$         USE H5GLOBAL
!!$         !DEC$IF DEFINED(HDF5F90_WINDOWS)
!!$         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5LREGISTERED_C'::H5Lregistered_c
!!$         !DEC$ENDIF
!!$         INTEGER, INTENT(IN) :: version      ! Version number of this struct
!!$         INTEGER, INTENT(IN) :: class_id     ! Link class identifier
!!$         CHARACTER(LEN=*), INTENT(IN) :: comment      ! Comment for debugging
!!$         CHARACTER(LEN=*), INTENT(IN) :: create_func  ! Callback during link creation
!!$         CHARACTER(LEN=*), INTENT(IN) :: move_func    ! Callback after moving link
!!$         CHARACTER(LEN=*), INTENT(IN) :: copy_func    ! Callback after copying link
!!$         CHARACTER(LEN=*), INTENT(IN) :: trav_func    ! The main traversal function
!!$         CHARACTER(LEN=*), INTENT(IN) :: del_func     ! Callback for link deletion
!!$         CHARACTER(LEN=*), INTENT(IN) :: query_func   ! Callback for queries
!!$         INTEGER, INTENT(OUT) :: hdferr   ! Error code:
!!$                                          ! 0 on success and -1 on failure
!!$         INTEGER :: comment_len
!!$         INTEGER :: create_func_len
!!$         INTEGER :: move_func_len
!!$         INTEGER :: copy_func_len
!!$         INTEGER :: trav_func_len
!!$         INTEGER :: del_func_len
!!$         INTEGER :: query_func_len
!!$
!!$       END FUNCTION H5Lregistered_c
!!$    END INTERFACE
!!$
!!$    comment_len     = LEN(comment)
!!$    create_func_len = LEN(create_func)
!!$    move_func_len   = LEN(move_func)
!!$    copy_func_len   = LEN(copy_func)
!!$    trav_func_len   = LEN(trav_func)
!!$    del_func_len    = LEN(del_func)
!!$    query_func_len  = LEN(query_func)
!!$
!!$    hdferr = H5Lregistered_c(version, class_id, comment, &
!!$         create_func, create_func_len, &
!!$         move_func, move_func_len, &
!!$         copy_func, copy_func_len, &
!!$         trav_func, trav_func_len, &
!!$         del_func, del_func_len, &
!!$         query_func, query_func_len)
!!$
!!$  END SUBROUTINE H5Lregistered_f

END MODULE H5L
