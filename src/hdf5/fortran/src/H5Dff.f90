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
! This file contains Fortran90 interfaces for H5D functions.
!
MODULE H5D
  USE H5GLOBAL

  INTERFACE h5dwrite_f

     MODULE PROCEDURE h5dwrite_reference_obj
     MODULE PROCEDURE h5dwrite_reference_dsetreg
     MODULE PROCEDURE h5dwrite_integer_scalar
     MODULE PROCEDURE h5dwrite_integer_1
     MODULE PROCEDURE h5dwrite_integer_2
     MODULE PROCEDURE h5dwrite_integer_3
     MODULE PROCEDURE h5dwrite_integer_4
     MODULE PROCEDURE h5dwrite_integer_5
     MODULE PROCEDURE h5dwrite_integer_6
     MODULE PROCEDURE h5dwrite_integer_7
     MODULE PROCEDURE h5dwrite_char_scalar
     MODULE PROCEDURE h5dwrite_char_1
     MODULE PROCEDURE h5dwrite_char_2
     MODULE PROCEDURE h5dwrite_char_3
     MODULE PROCEDURE h5dwrite_char_4
     MODULE PROCEDURE h5dwrite_char_5
     MODULE PROCEDURE h5dwrite_char_6
     MODULE PROCEDURE h5dwrite_char_7
     MODULE PROCEDURE h5dwrite_real_scalar
     MODULE PROCEDURE h5dwrite_real_1
     MODULE PROCEDURE h5dwrite_real_2
     MODULE PROCEDURE h5dwrite_real_3
     MODULE PROCEDURE h5dwrite_real_4
     MODULE PROCEDURE h5dwrite_real_5
     MODULE PROCEDURE h5dwrite_real_6
     MODULE PROCEDURE h5dwrite_real_7
  END INTERFACE

  INTERFACE h5dread_f

     MODULE PROCEDURE h5dread_reference_obj
     MODULE PROCEDURE h5dread_reference_dsetreg
     MODULE PROCEDURE h5dread_integer_scalar
     MODULE PROCEDURE h5dread_integer_1
     MODULE PROCEDURE h5dread_integer_2
     MODULE PROCEDURE h5dread_integer_3
     MODULE PROCEDURE h5dread_integer_4
     MODULE PROCEDURE h5dread_integer_5
     MODULE PROCEDURE h5dread_integer_6
     MODULE PROCEDURE h5dread_integer_7
     MODULE PROCEDURE h5dread_char_scalar
     MODULE PROCEDURE h5dread_char_1
     MODULE PROCEDURE h5dread_char_2
     MODULE PROCEDURE h5dread_char_3
     MODULE PROCEDURE h5dread_char_4
     MODULE PROCEDURE h5dread_char_5
     MODULE PROCEDURE h5dread_char_6
     MODULE PROCEDURE h5dread_char_7
     MODULE PROCEDURE h5dread_real_scalar
     MODULE PROCEDURE h5dread_real_1
     MODULE PROCEDURE h5dread_real_2
     MODULE PROCEDURE h5dread_real_3
     MODULE PROCEDURE h5dread_real_4
     MODULE PROCEDURE h5dread_real_5
     MODULE PROCEDURE h5dread_real_6
     MODULE PROCEDURE h5dread_real_7
  END INTERFACE

  INTERFACE h5dwrite_vl_f
     MODULE PROCEDURE h5dwrite_vl_integer
     MODULE PROCEDURE h5dwrite_vl_real
     MODULE PROCEDURE h5dwrite_vl_string
  END INTERFACE

  INTERFACE h5dread_vl_f
     MODULE PROCEDURE h5dread_vl_integer
     MODULE PROCEDURE h5dread_vl_real
     MODULE PROCEDURE h5dread_vl_string
  END INTERFACE

  INTERFACE h5dfill_f
     MODULE PROCEDURE h5dfill_integer
     MODULE PROCEDURE h5dfill_real
     MODULE PROCEDURE h5dfill_char
  END INTERFACE

  INTERFACE h5dextend_f
     MODULE PROCEDURE h5dset_extent_f
  END INTERFACE


CONTAINS

!----------------------------------------------------------------------
! Name:		h5dcreate_f
!
! Purpose: 	Creates a dataset at the specified location
!
! Inputs:
!		loc_id		- file or group identifier
!		name		- dataset name
!		type_id		- dataset datatype identifier
!		space_id	- dataset dataspace identifier
! Outputs:
!		dset_id		- dataset identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!            creation_prp - Dataset creation property list
!            lcpl_id      - Link creation property list
!            dapl_id      - Dataset access property list
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications:
!                 - Explicit Fortran interfaces were added for
!	           called C functions (it is needed for Windows
!		   port).  February 28, 2001
!
!                 - Added version's 1.8 new optional parameters
!                  February, 2008
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5dcreate_f(loc_id, name, type_id, space_id, dset_id, &
       hdferr, dcpl_id, lcpl_id, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier
    CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset
    INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
    INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dcpl_id ! Dataset creation property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dapl_id ! Dataset access property list

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: dcpl_id_default
    INTEGER(HID_T) :: dapl_id_default

    INTEGER :: namelen                     ! Name length

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5dcreate_c(loc_id, name, namelen, type_id, &
            space_id, lcpl_id_default, dcpl_id_default, dapl_id_default, dset_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DCREATE_C'::h5dcreate_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id

         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: dcpl_id_default
         INTEGER(HID_T) :: dapl_id_default

         INTEGER(HID_T), INTENT(OUT) :: dset_id
       END FUNCTION h5dcreate_c
    END INTERFACE

    lcpl_id_default = H5P_DEFAULT_F
    dcpl_id_default = H5P_DEFAULT_F
    dapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF(PRESENT(dcpl_id)) dcpl_id_default = dcpl_id
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    namelen = LEN(name)
    hdferr = h5dcreate_c(loc_id, name, namelen, type_id, space_id, &
         lcpl_id_default, dcpl_id_default, dapl_id_default, dset_id)

  END SUBROUTINE h5dcreate_f

!----------------------------------------------------------------------
! Name:		h5dopen_f
!
! Purpose: 	Opens an existing dataset.
!
! Inputs:
!		loc_id		- file or group identifier
!		name		- dataset name
! Outputs:
!		dset_id		- dataset identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!	       dapl_id	        - Dataset access property list
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications:  -Explicit Fortran interfaces were added for
!		   called C functions (it is needed for Windows
!		   port).  February 28, 2001
!
!                 -Added 1.8 (optional) parameter dapl_id
!                  February, 2008, M. Scot Breitenfeld
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5dopen_f(loc_id, name, dset_id, hdferr, dapl_id)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Name of the dataset
            INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier
            INTEGER, INTENT(OUT) :: hdferr         ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dapl_id ! Dataset access property list
            INTEGER :: namelen                     ! Name length

            INTEGER(HID_T) :: dapl_id_default

!            INTEGER, EXTERNAL :: h5dopen_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dopen_c(loc_id, name, namelen, dapl_id_default, dset_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DOPEN_C'::h5dopen_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: loc_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER :: namelen
              INTEGER(HID_T), INTENT(IN) :: dapl_id_default
              INTEGER(HID_T), INTENT(OUT) :: dset_id
              END FUNCTION h5dopen_c
            END INTERFACE

            dapl_id_default = H5P_DEFAULT_F
            IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

            namelen = LEN(name)
            hdferr = h5dopen_c(loc_id, name, namelen, dapl_id_default, dset_id)

          END SUBROUTINE h5dopen_f

!----------------------------------------------------------------------
! Name:		h5dclose_f
!
! Purpose: 	Closes a dataset.
!
! Inputs:
!		dset_id		- dataset identifier
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

          SUBROUTINE h5dclose_f(dset_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id ! Dataset identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5dclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dclose_c(dset_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DCLOSE_C'::h5dclose_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              END FUNCTION h5dclose_c
            END INTERFACE

            hdferr = h5dclose_c(dset_id)

          END SUBROUTINE h5dclose_f

          SUBROUTINE h5dwrite_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
            TYPE(hobj_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER(HADDR_T), ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: j

!            INTEGER, EXTERNAL :: h5dwrite_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REF_OBJ_C'::h5dwrite_ref_obj_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HADDR_T), DIMENSION(*) :: ref_buf
              INTEGER(HSIZE_T), DIMENSION(*) :: dims
              END FUNCTION h5dwrite_ref_obj_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            allocate(ref_buf(dims(1)), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, dims(1)
                   ref_buf(j) = buf(j)%ref
                enddo
            endif
            hdferr = h5dwrite_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims(1))
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_obj

          SUBROUTINE h5dwrite_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims ! size of the bufffer buf
            TYPE(hdset_reg_ref_t_f), DIMENSION(dims(1)), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dwrite_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REF_REG_C'::h5dwrite_ref_reg_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER, DIMENSION(*) :: ref_buf
              INTEGER(HSIZE_T), DIMENSION(*) ::  dims
              END FUNCTION h5dwrite_ref_reg_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            allocate(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0 ) then
                hdferr = -1
                return
            else
                do j = 1, dims(1)
                  do i = 1, REF_REG_BUF_LEN
                   ref_buf(REF_REG_BUF_LEN*(j-1) + i) = buf(j)%ref(i)
                 enddo
                enddo
            endif
            hdferr = h5dwrite_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
            deallocate(ref_buf)

          END SUBROUTINE h5dwrite_reference_dsetreg


          SUBROUTINE h5dwrite_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER, INTENT(IN) :: buf ! Data buffer
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_s_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_S_C'::h5dwrite_integer_s_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN) :: buf
              END FUNCTION h5dwrite_integer_s_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_integer_s_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_integer_scalar

          SUBROUTINE h5dwrite_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_1_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_1_C'::h5dwrite_integer_1_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_integer_1_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_integer_1_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_integer_1

          SUBROUTINE h5dwrite_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_2_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_2_C'::h5dwrite_integer_2_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_integer_2_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id
            hdferr = h5dwrite_integer_2_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dwrite_integer_2

          SUBROUTINE h5dwrite_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_3_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_3_C'::h5dwrite_integer_3_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_integer_3_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_integer_3_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dwrite_integer_3

          SUBROUTINE h5dwrite_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_4_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_4_C'::h5dwrite_integer_4_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_integer_4_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_integer_4_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dwrite_integer_4

          SUBROUTINE h5dwrite_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_5_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_5_C'::h5dwrite_integer_5_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_integer_5_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F


            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_integer_5_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dwrite_integer_5

          SUBROUTINE h5dwrite_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_6_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_6_C'::h5dwrite_integer_6_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_integer_6_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_integer_6_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dwrite_integer_6

          SUBROUTINE h5dwrite_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_integer_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_integer_7_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_INTEGER_7_C'::h5dwrite_integer_7_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_integer_7_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_integer_7_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dwrite_integer_7


          SUBROUTINE h5dwrite_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_s_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_S_C'::h5dwritec_s_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN) :: buf
              END FUNCTION h5dwritec_s_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_s_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_scalar

          SUBROUTINE h5dwrite_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_1_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_1_C'::h5dwritec_1_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwritec_1_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_1_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_1

          SUBROUTINE h5dwrite_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_2_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_2_C'::h5dwritec_2_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwritec_2_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_2_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_2

          SUBROUTINE h5dwrite_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_3_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_3_C'::h5dwritec_3_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwritec_3_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_3_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_3

          SUBROUTINE h5dwrite_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_4_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_4_C'::h5dwritec_4_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwritec_4_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_4_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_4

          SUBROUTINE h5dwrite_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_5_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_5_C'::h5dwritec_5_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwritec_5_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_5_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_5

          SUBROUTINE h5dwrite_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_6_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_6_C'::h5dwritec_6_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwritec_6_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_6_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_6

          SUBROUTINE h5dwrite_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwritec_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwritec_7_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITEC_7_C'::h5dwritec_7_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwritec_7_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwritec_7_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_char_7

          SUBROUTINE h5dwrite_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_s_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_S_C'::h5dwrite_real_s_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN) :: buf
              END FUNCTION h5dwrite_real_s_c
            END INTERFACE


            xfer_prp_default  = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F
            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_s_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_scalar

          SUBROUTINE h5dwrite_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_1_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_1_C'::h5dwrite_real_1_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dwrite_real_1_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_1_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_1

          SUBROUTINE h5dwrite_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_2_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_2_C'::h5dwrite_real_2_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_real_2_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_2_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_2

          SUBROUTINE h5dwrite_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_3_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_3_C'::h5dwrite_real_3_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dwrite_real_3_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_3_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_3

          SUBROUTINE h5dwrite_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_4_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_4_C'::h5dwrite_real_4_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dwrite_real_4_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_4_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_4

          SUBROUTINE h5dwrite_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_5_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_5_C'::h5dwrite_real_5_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dwrite_real_5_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_5_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_5

          SUBROUTINE h5dwrite_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_6_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_6_C'::h5dwrite_real_6_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dwrite_real_6_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_6_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_6

          SUBROUTINE h5dwrite_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dwrite_real_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dwrite_real_7_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_REAL_7_C'::h5dwrite_real_7_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dwrite_real_7_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_real_7_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dwrite_real_7

!----------------------------------------------------------------------
! Name:		h5dread_f
!
! Purpose: 	Reads raw data from the specified dataset into buf,
!		converting from file datatype and dataspace to memory
!		datatype and dataspace.
!
! Inputs:
!		dset_id		- dataset identifier
!		mem_type_id	- memory type identifier
!		dims		- 1-dim array of size 7; dims(k) has the size
!				- of k-th dimension of the buf array
! Outputs:
!		buf		- buffer to read data in
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		mem_space_id	- memory dataspace identifier
!		file_space_id 	- file dataspace identifier
!		xfer_prp	- trasfer property list identifier
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  February 28, 2001
!
!                       dims parameter was added to make code portable;
!                       n parameter was replaced with dims parameter in
!			the h5dwrite_reference_obj and h5dwrite_reference_dsetreg
!			functions.  April 2, 2001
!
! Comment:		This function is overloaded to read INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions, and one dimensional buffers
!			of the TYPE(hobj_ref_t_f) and TYPE(hdset_reg_ref_t_f)
!			types.
!----------------------------------------------------------------------
          SUBROUTINE h5dread_reference_obj(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            TYPE(hobj_ref_t_f), INTENT(INOUT) , &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER(HADDR_T), ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: j

!            INTEGER, EXTERNAL :: h5dread_ref_obj_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_obj_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REF_OBJ_C'::h5dread_ref_obj_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER(HADDR_T), DIMENSION(*) :: ref_buf
              END FUNCTION h5dread_ref_obj_c
            END INTERFACE

            allocate(ref_buf(dims(1)), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_ref_obj_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)
             do j = 1, dims(1)
                buf(j)%ref = ref_buf(j)
             enddo
             deallocate(ref_buf)
          END SUBROUTINE h5dread_reference_obj

          SUBROUTINE h5dread_reference_dsetreg(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            TYPE(hdset_reg_ref_t_f), INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER, ALLOCATABLE, DIMENSION(:) :: ref_buf
            INTEGER :: i,j

!            INTEGER, EXTERNAL :: h5dread_ref_reg_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_ref_reg_c(dset_id, mem_type_id,&
                                                  mem_space_id_default, &
                               file_space_id_default, xfer_prp_default, ref_buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REF_REG_C'::h5dread_ref_reg_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, DIMENSION(*) :: ref_buf
              END FUNCTION h5dread_ref_reg_c
            END INTERFACE

            allocate(ref_buf(REF_REG_BUF_LEN*dims(1)), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_ref_reg_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, ref_buf, dims)

            do j = 1, dims(1)
             do i = 1, REF_REG_BUF_LEN
                   buf(j)%ref(i) = ref_buf(REF_REG_BUF_LEN*(j-1) + i)
             enddo
            enddo
            deallocate(ref_buf)
          END SUBROUTINE h5dread_reference_dsetreg


          SUBROUTINE h5dread_integer_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_s_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_S_C'::h5dread_integer_s_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT) :: buf
              END FUNCTION h5dread_integer_s_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_s_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_integer_scalar

          SUBROUTINE h5dread_integer_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_1_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_1_C'::h5dread_integer_1_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_integer_1_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_1_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_integer_1

          SUBROUTINE h5dread_integer_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_2_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_2_C'::h5dread_integer_2_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_integer_2_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_2_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dread_integer_2

          SUBROUTINE h5dread_integer_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_3_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_3_C'::h5dread_integer_3_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_integer_3_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_3_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dread_integer_3

          SUBROUTINE h5dread_integer_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_4_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_4_C'::h5dread_integer_4_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(OUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dread_integer_4_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_4_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dread_integer_4

          SUBROUTINE h5dread_integer_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_5_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_5_C'::h5dread_integer_5_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_integer_5_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_5_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dread_integer_5

          SUBROUTINE h5dread_integer_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_6_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_6_C'::h5dread_integer_6_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_integer_6_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_6_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dread_integer_6

          SUBROUTINE h5dread_integer_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_integer_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_integer_7_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_INTEGER_7_C'::h5dread_integer_7_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_integer_7_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_integer_7_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims)

          END SUBROUTINE h5dread_integer_7

          SUBROUTINE h5dread_char_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_s_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_S_C'::h5dreadc_s_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(OUT) :: buf
              END FUNCTION h5dreadc_s_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_s_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_scalar

          SUBROUTINE h5dread_char_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_1_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_1_C'::h5dreadc_1_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dreadc_1_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_1_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_1

          SUBROUTINE h5dread_char_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_2_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_2_C'::h5dreadc_2_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dreadc_2_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_2_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_2

          SUBROUTINE h5dread_char_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_3_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_3_C'::h5dreadc_3_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dreadc_3_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_3_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_3

          SUBROUTINE h5dread_char_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_4_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_4_C'::h5dreadc_4_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5dreadc_4_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_4_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_4

          SUBROUTINE h5dread_char_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_5_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_5_C'::h5dreadc_5_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dreadc_5_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_5_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_5

          SUBROUTINE h5dread_char_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_6_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_6_C'::h5dreadc_6_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dreadc_6_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_6_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_6

          SUBROUTINE h5dread_char_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dreadc_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dreadc_7_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREADC_7_C'::h5dreadc_7_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dreadc_7_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dreadc_7_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_char_7

          SUBROUTINE h5dread_real_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT) :: buf ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_s_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_S_C'::h5dread_real_s_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(OUT) :: buf
              END FUNCTION h5dread_real_s_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_s_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_scalar

          SUBROUTINE h5dread_real_1(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_1_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_1_C'::h5dread_real_1_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5dread_real_1_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_1_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_1

          SUBROUTINE h5dread_real_2(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_2_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_2_C'::h5dread_real_2_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_real_2_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_2_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_2

          SUBROUTINE h5dread_real_3(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_3_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_3_C'::h5dread_real_3_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5dread_real_3_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_3_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_3

          SUBROUTINE h5dread_real_4(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_4_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_4_C'::h5dread_real_4_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3), dims(4)) :: buf
              END FUNCTION h5dread_real_4_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_4_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_4

          SUBROUTINE h5dread_real_5(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_5_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_5_C'::h5dread_real_5_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5dread_real_5_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_5_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_5

          SUBROUTINE h5dread_real_6(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier
            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_6_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_6_C'::h5dread_real_6_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5dread_real_6_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_6_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_6

          SUBROUTINE h5dread_real_7(dset_id, mem_type_id, buf, dims, hdferr, &
                                        mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T) :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

!            INTEGER, EXTERNAL :: h5dread_real_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dread_real_7_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_REAL_7_C'::h5dread_real_7_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5dread_real_7_c
            END INTERFACE

            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_real_7_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, buf, dims)

          END SUBROUTINE h5dread_real_7

!----------------------------------------------------------------------
! Name:		h5dget_space_f
!
! Purpose:	Returns an identifier for a copy of the dataspace for a
!		dataset.
!
! Inputs:
!		dataset_id	- dataset identifier
! Outputs:
!		dataspace_id	- dataspace identifier
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
          SUBROUTINE h5dget_space_f(dataset_id, dataspace_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: dataspace_id   ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code

!            INTEGER, EXTERNAL :: h5dget_space_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_space_c(dataset_id, dataspace_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_SPACE_C'::h5dget_space_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: dataspace_id
              END FUNCTION h5dget_space_c
            END INTERFACE

            hdferr = h5dget_space_c(dataset_id, dataspace_id)
          END SUBROUTINE h5dget_space_f

!----------------------------------------------------------------------
! Name:		h5dget_type_f
!
! Purpose:	Returns an identifier for a copy of the datatype for a
!		dataset.
!
! Inputs:
!		dataset_id	- dataset identifier
! Outputs:
!		datatype_id	- dataspace identifier
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

          SUBROUTINE h5dget_type_f(dataset_id, datatype_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: datatype_id    ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code
!            INTEGER, EXTERNAL :: h5dget_type_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_type_c (dataset_id, datatype_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_TYPE_C'::h5dget_type_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: datatype_id
              END FUNCTION h5dget_type_c
            END INTERFACE

            hdferr = h5dget_type_c (dataset_id, datatype_id)
          END SUBROUTINE h5dget_type_f

!----------------------------------------------------------------------
! Name:		h5dset_extent (instead of obsolete name: h5dextend_f)
!
! Purpose:	Extends a dataset with unlimited dimension.
!
! Inputs:
!		dataset_id	- dataset identifier
!		size		- array containing the new magnitude of
!				  each dimension
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
!                       Changed name from the now obsolete h5dextend_f
!                       to h5dset_extent_f. Provided interface to old name
!                       for backward compatability. -MSB- March 14, 2008
!
! Comment:
!----------------------------------------------------------------------


  SUBROUTINE h5dset_extent_f(dataset_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
    ! Array containing
    ! dimensions' sizes
    INTEGER, INTENT(OUT) :: hdferr                ! Error code

    !
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dset_extent_c(dataset_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DSET_EXTENT_C'::h5dset_extent_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dataset_id
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN)  :: size
       END FUNCTION h5dset_extent_c
    END INTERFACE

    hdferr = H5Dset_extent_c(dataset_id, size)
  END SUBROUTINE h5dset_extent_f


!----------------------------------------------------------------------
! Name:		h5dget_create_plist_f
!
! Purpose:	Returns an identifier for a copy of the dataset creation
!		property list for a dataset.
!
! Inputs:
!		dataset_id	- dataset identifier
! Outputs:
!		plist_id	- creation property list identifier
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


          SUBROUTINE h5dget_create_plist_f(dataset_id, plist_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(OUT) :: plist_id    ! Dataset creation
                                                  ! property list identifier
            INTEGER, INTENT(OUT) :: hdferr                ! Error code

!            INTEGER, EXTERNAL :: h5dget_create_plist_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_create_plist_c(dataset_id, plist_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_CREATE_PLIST_C'::h5dget_create_plist_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(OUT) :: plist_id
              END FUNCTION h5dget_create_plist_c
            END INTERFACE

            hdferr = h5dget_create_plist_c(dataset_id, plist_id)
          END SUBROUTINE h5dget_create_plist_f

!----------------------------------------------------------------------
! Name:		h5dget_storage_size_f
!
! Purpose:	Returns the amount of storage requires by a dataset
!
! Inputs:
!		dataset_id	- dataset identifier
! Outputs:
!		size		- datastorage size
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		October 15, 2002
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5dget_storage_size_f(dataset_id, size, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HSIZE_T),  INTENT(OUT)  :: size
                                                          ! Amount of storage
                                                          ! allocated for dataset
            INTEGER, INTENT(OUT) :: hdferr                ! Error code

            INTERFACE
              INTEGER FUNCTION h5dget_storage_size_c(dataset_id, size)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_STORAGE_SIZE_C'::h5dget_storage_size_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HSIZE_T), INTENT(OUT)  :: size
              END FUNCTION h5dget_storage_size_c
            END INTERFACE

            hdferr = h5dget_storage_size_c(dataset_id, size)
          END SUBROUTINE h5dget_storage_size_f

!----------------------------------------------------------------------
! Name:		h5dvlen_get_max_len_f
!
! Purpose:	Returns maximum lenght of the VL array elements
!
! Inputs:
!		dataset_id	- dataset identifier
!		type_id		- datatype identifier
!		space_id	- dataspace identifier
! Outputs:
!		size		- buffer size
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		October 15, 2002
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5dvlen_get_max_len_f(dataset_id, type_id, space_id, len,  hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dataset_id      ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: type_id         ! Datatype identifier
            INTEGER(HID_T), INTENT(IN) :: space_id        ! Dataspace identifier
            INTEGER(SIZE_T),  INTENT(OUT)  :: len         ! Maximum length of the element
            INTEGER, INTENT(OUT) :: hdferr                ! Error code

            INTERFACE
              INTEGER FUNCTION h5dvlen_get_max_len_c(dataset_id, type_id, space_id, len)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DVLEN_GET_MAX_LEN_C'::h5dvlen_get_max_len_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dataset_id
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(SIZE_T), INTENT(OUT)  :: len
              END FUNCTION h5dvlen_get_max_len_c
            END INTERFACE

            hdferr = h5dvlen_get_max_len_c(dataset_id, type_id,  space_id, len)
          END SUBROUTINE h5dvlen_get_max_len_f

          SUBROUTINE h5dwrite_vl_integer(dset_id, mem_type_id, buf, dims, len, &
                                         hdferr, &
                                         mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! MAX len x num_elem
            INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len ! Array to store
                                                              ! the lenght of each
                                                              ! element
            INTEGER, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

            INTERFACE
              INTEGER FUNCTION h5dwrite_vl_integer_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims, len)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_VL_INTEGER_C'::h5dwrite_vl_integer_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
              INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_vl_integer_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_vl_integer_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims, len)

          END SUBROUTINE h5dwrite_vl_integer

          SUBROUTINE h5dread_vl_integer(dset_id, mem_type_id, buf, dims, len, &
                                         hdferr, &
                                         mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! MAX len x num_elem
            INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len ! Array to store
                                                              ! the lenght of each
                                                              ! element
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
                                                ! -1 if failed, 0 otherwise
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER(HID_T) :: tmp
            INTEGER :: error

            INTERFACE
              INTEGER FUNCTION h5dread_vl_integer_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims, len)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_VL_INTEGER_C'::h5dread_vl_integer_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_vl_integer_c
            END INTERFACE

            CALL h5dget_space_f(dset_id, tmp, error)
            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = tmp
            file_space_id_default = tmp

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_vl_integer_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims, len)

          END SUBROUTINE h5dread_vl_integer

          SUBROUTINE h5dwrite_vl_real(dset_id, mem_type_id, buf, dims, len, &
                                         hdferr, &
                                         mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! MAX len x num_elem
            INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len ! Array to store
                                                              ! the lenght of each
                                                              ! element
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default

            INTERFACE
              INTEGER FUNCTION h5dwrite_vl_real_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims, len)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_VL_REAL_C'::h5dwrite_vl_real_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: len
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dwrite_vl_real_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dwrite_vl_real_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims, len)

          END SUBROUTINE h5dwrite_vl_real

          SUBROUTINE h5dread_vl_real(dset_id, mem_type_id, buf, dims, len, &
                                         hdferr, &
                                         mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! MAX len x num_elem
            INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len ! Array to store
                                                              ! the lenght of each
                                                              ! element
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf   ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
                                                ! -1 if failed, 0 otherwise
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
            INTEGER(HID_T) :: tmp
            INTEGER :: error

            INTERFACE
              INTEGER FUNCTION h5dread_vl_real_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
                                          xfer_prp_default, buf, dims, len)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_VL_REAL_C'::h5dread_vl_real_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
              INTEGER(SIZE_T), INTENT(INOUT), DIMENSION(*) :: len
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5dread_vl_real_c
            END INTERFACE

            CALL h5dget_space_f(dset_id, tmp, error)
            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = tmp
            file_space_id_default = tmp

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id

            hdferr = h5dread_vl_real_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims, len)

          END SUBROUTINE h5dread_vl_real

          SUBROUTINE h5dwrite_vl_string(dset_id, mem_type_id, buf, dims, str_len, &
                                         hdferr, &
                                         mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! number of strings
            INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len ! Array to store
                                                              ! the lenght of each
                                                              ! element
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(2)) :: buf           ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                                ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
!            CHARACTER, DIMENSION(dims(1)*dims(2)) :: tmp_buf

            INTERFACE
              INTEGER FUNCTION h5dwrite_vl_string_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
!                                          xfer_prp_default, tmp_buf, dims, str_len)
                                          xfer_prp_default, buf, dims, str_len)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_VL_STRING_C'::h5dwrite_vl_string_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
              INTEGER(SIZE_T), INTENT(IN), DIMENSION(*) :: str_len
!              CHARACTER, INTENT(IN), &
!              DIMENSION(dims(1)*dims(2)) :: tmp_buf
              CHARACTER(LEN=*), DIMENSION(dims(2)) :: buf
              END FUNCTION h5dwrite_vl_string_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id
!            do i = 1, dims(2)
!               do j = 1, dims(1)
!               tmp_buf((i-1)*dims(1) +j) = buf(i)(j:j)
!               enddo
!            enddo
!              write(*,*) (tmp_buf(j:j), j=1,dims(1)*dims(2))
!              write(*,*) str_len(1), str_len(2), str_len(3), str_len(4)

            hdferr = h5dwrite_vl_string_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims, str_len)

          END SUBROUTINE h5dwrite_vl_string

          SUBROUTINE h5dread_vl_string(dset_id, mem_type_id, buf, dims, str_len, &
                                         hdferr, &
                                         mem_space_id, file_space_id, xfer_prp)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
            INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims ! number of strings
            INTEGER(SIZE_T), INTENT(OUT), DIMENSION(*) :: str_len ! Array to store
                                                              ! the lenght of each
                                                              ! element
            CHARACTER(LEN=*), INTENT(OUT), &
            DIMENSION(dims(2)) :: buf           ! Data buffer
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
                                               ! Memory dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
                                                ! File dataspace identfier
            INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
                                                ! Transfer property list identifier

            INTEGER(HID_T) :: xfer_prp_default
            INTEGER(HID_T)  :: mem_space_id_default
            INTEGER(HID_T) :: file_space_id_default
!            CHARACTER, DIMENSION(dims(1)*dims(2)) :: tmp_buf
!            integer i, j

            INTERFACE
              INTEGER FUNCTION h5dread_vl_string_c(dset_id, mem_type_id, &
                                          mem_space_id_default, &
                                          file_space_id_default, &
!                                          xfer_prp_default, tmp_buf, dims, str_len)
                                          xfer_prp_default, buf, dims, str_len)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_VL_STRING_C'::h5dread_vl_string_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: dset_id
              INTEGER(HID_T), INTENT(IN) :: mem_type_id
              INTEGER(HID_T)  :: mem_space_id_default
              INTEGER(HID_T) :: file_space_id_default
              INTEGER(HID_T) :: xfer_prp_default
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(2) :: dims
              INTEGER(SIZE_T), INTENT(OUT), DIMENSION(*) :: str_len
!              CHARACTER, INTENT(IN), &
!              DIMENSION(dims(1)*dims(2)) :: tmp_buf
              CHARACTER(LEN=*), DIMENSION(dims(2)) :: buf
              END FUNCTION h5dread_vl_string_c
            END INTERFACE


            xfer_prp_default = H5P_DEFAULT_F
            mem_space_id_default = H5S_ALL_F
            file_space_id_default = H5S_ALL_F

            if (present(xfer_prp)) xfer_prp_default = xfer_prp
            if (present(mem_space_id))  mem_space_id_default = mem_space_id
            if (present(file_space_id)) file_space_id_default = file_space_id
!            do i = 1, dims(2)
!               do j = 1, dims(1)
!               tmp_buf((i-1)*dims(1) +j) = buf(i)(j:j)
!               enddo
!            enddo
!              write(*,*) (tmp_buf(j:j), j=1,dims(1)*dims(2))
!              write(*,*) str_len(1), str_len(2), str_len(3), str_len(4)

            hdferr = h5dread_vl_string_c(dset_id, mem_type_id, mem_space_id_default, &
                                file_space_id_default, xfer_prp_default, &
                                buf, dims, str_len)
          RETURN
          END SUBROUTINE h5dread_vl_string

!----------------------------------------------------------------------
! Name:		h5dfill_integer
!
! Purpose:      Fills dataspace elements with a fill value in a memory buffer.
!               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!               of the fillvalues and buffers are supported. Buffer and fillvalue
!               are assumed to have the same datatype.
!               Only one-dimesional buffers are supported.
!
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	Elena Pourmal
!		March 12, 2003
!
!----------------------------------------------------------------------

          SUBROUTINE h5dfill_integer(fill_value, space_id, buf,  hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: fill_value  ! Fill value
            INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
            INTEGER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
            INTEGER, INTENT(OUT) :: hdferr      ! Error code

            INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
            INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

!            INTEGER, EXTERNAL :: h5dfill_integer_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dfill_integer_c(fill_value, fill_type_id, space_id, &
                                         buf, mem_type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILL_INTEGER_C'::h5dfill_integer_c
              !DEC$ENDIF
              INTEGER, INTENT(IN) :: fill_value  ! Fill value
              INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
              INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
              INTEGER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
              INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier
              END FUNCTION h5dfill_integer_c
            END INTERFACE
            fill_type_id = H5T_NATIVE_INTEGER
            mem_type_id  = H5T_NATIVE_INTEGER

            hdferr = h5dfill_integer_c(fill_value, fill_type_id, space_id, &
                               buf, mem_type_id)

          END SUBROUTINE h5dfill_integer

!----------------------------------------------------------------------
! Name:		h5dfill_real
!
! Purpose:      Fills dataspace elements with a fill value in a memory buffer.
!               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!               of the fillvalues and buffers are supported. Buffer and fillvalue
!               are assumed to have the same datatype.
!               Only one-dimesional buffers are supported.
!
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	Elena Pourmal
!		March 12, 2003
!
!----------------------------------------------------------------------

          SUBROUTINE h5dfill_real(fill_valuer, space_id, buf,  hdferr)
            IMPLICIT NONE
            REAL, INTENT(IN) :: fill_valuer  ! Fill value
            INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
            REAL, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
            INTEGER, INTENT(OUT) :: hdferr      ! Error code

            INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
            INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

!            INTEGER, EXTERNAL :: h5dfill_real_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dfill_real_c(fill_valuer, fill_type_id, space_id, &
                                         buf, mem_type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILL_REAL_C'::h5dfill_real_c
              !DEC$ENDIF
              REAL, INTENT(IN) :: fill_valuer  ! Fill value
              INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
              INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
              REAL, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
              INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier
              END FUNCTION h5dfill_real_c
            END INTERFACE
            fill_type_id = H5T_NATIVE_REAL
            mem_type_id  = H5T_NATIVE_REAL

            hdferr = h5dfill_real_c(fill_valuer, fill_type_id, space_id, &
                               buf, mem_type_id)
          END SUBROUTINE h5dfill_real

!----------------------------------------------------------------------
! Name:		h5dfill_char
!
! Purpose:      Fills dataspace elements with a fill value in a memory buffer.
!               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
!               of the fillvalues and buffers are supported. Buffer and fillvalue
!               are assumed to have the same datatype.
!               Only one-dimesional buffers are supported.
!
! Inputs:
!		fill_value	- fill value
!		space_id	- memory space selection identifier
!		buf		- data buffer iin memory ro apply selection to
!				- of k-th dimension of the buf array
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	Elena Pourmal
!		March 12, 2003
!
!----------------------------------------------------------------------

          SUBROUTINE h5dfill_char(fill_value, space_id, buf,  hdferr)
            IMPLICIT NONE
            CHARACTER, INTENT(IN) :: fill_value  ! Fill value
            INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
            CHARACTER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
            INTEGER, INTENT(OUT) :: hdferr      ! Error code

            INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
            INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

!            INTEGER, EXTERNAL :: h5dfillc_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dfillc_c(fill_value, fill_type_id, space_id, &
                                         buf, mem_type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILLC_C'::h5dfillc_c
              !DEC$ENDIF
              CHARACTER, INTENT(IN) :: fill_value  ! Fill value
              INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
              INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
              CHARACTER, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
              INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier
              END FUNCTION h5dfillc_c
            END INTERFACE
            fill_type_id = H5T_NATIVE_CHARACTER
            mem_type_id  = H5T_NATIVE_CHARACTER

            hdferr = h5dfillc_c(fill_value, fill_type_id, space_id, &
                               buf, mem_type_id)

          END SUBROUTINE h5dfill_char

!----------------------------------------------------------------------
! Name:		h5dget_space_status_f
!
! Purpose:      Returns the status of data space allocation.
!
! Inputs:
!		dset_id		- dataset identifier
! Outputs:
!               flag            - status; may have one of the following values:
!				  H5D_SPACE_STS_ERROR_F
!				  H5D_SPACE_STS_NOT_ALLOCATED_F
!				  H5D_SPACE_STS_PART_ALLOCATED_F
!				  H5D_SPACE_STS_ALLOCATED_F
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	Elena Pourmal
!		March 12, 2003
!
!----------------------------------------------------------------------

          SUBROUTINE h5dget_space_status_f(dset_id, flag, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dset_id  ! Dataspace identifier
            INTEGER, INTENT(IN)        :: flag     ! Memory buffer to fill in
            INTEGER, INTENT(OUT)       :: hdferr   ! Error code

!            INTEGER, EXTERNAL :: h5dget_space_status_c
! MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5dget_space_status_c(dset_id, flag)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_SPACE_STATUS_C'::h5dget_space_status_c
              !DEC$ENDIF
              INTEGER(HID_T) :: dset_id
              INTEGER        :: flag
              END FUNCTION h5dget_space_status_c
            END INTERFACE

            hdferr = h5dget_space_status_c(dset_id, flag)
          END SUBROUTINE h5dget_space_status_f

!----------------------------------------------------------------------
! Name:		h5dcreate_anon_f
!
! Purpose: 	Creates a dataset in a file without linking it into the file structure
!
! Inputs:
!		loc_id		- Identifier of the file or group within which to create the dataset.
!		type_id		- Identifier of the datatype to use when creating the dataset.
!		space_id	- Identifier of the dataspace to use when creating the dataset.
! Outputs:
!		dset_id		- dataset identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!               dcpl_id         - Dataset creation property list identifier.
!               dapl_id  	- Dataset access property list identifier.
!
! Programmer:   M. Scot Breitenfeld
!		February 11, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5dcreate_anon_f(loc_id, type_id, space_id, dset_id, hdferr, dcpl_id, dapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id   ! File or group identifier.
    INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier.
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier.
    INTEGER(HID_T), INTENT(OUT) :: dset_id ! Dataset identifier.
    INTEGER, INTENT(OUT) :: hdferr         ! Error code.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dcpl_id  ! Dataset creation property list identifier.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: dapl_id  ! Dataset access property list identifier.

    INTEGER(HID_T) :: dcpl_id_default
    INTEGER(HID_T) :: dapl_id_default

!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5dcreate_anon_c(loc_id, type_id, space_id, dcpl_id_default, dapl_id_default, dset_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DCREATE_ANON_C'::h5dcreate_anon_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: dcpl_id_default
         INTEGER(HID_T) :: dapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: dset_id
       END FUNCTION h5dcreate_anon_c
    END INTERFACE

    dcpl_id_default = H5P_DEFAULT_F
    dapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(dcpl_id)) dcpl_id_default = dcpl_id
    IF(PRESENT(dapl_id)) dapl_id_default = dapl_id

    hdferr = h5dcreate_anon_c(loc_id, type_id, space_id, dcpl_id_default, dapl_id_default, dset_id)

  END SUBROUTINE h5dcreate_anon_f

!----------------------------------------------------------------------
! Name:		h5dget_access_plist_f
!
! Purpose: 	Returns a copy of the dataset creation property list.
!
! Inputs:
!		dset_id         - dataset identifier.
! Outputs:
!		plist_id	- the dataset access property list identifier.
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:   M. Scot Breitenfeld
!		April 13, 2009
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5dget_access_plist_f(dset_id, plist_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id
    INTEGER(HID_T), INTENT(OUT) :: plist_id
    INTEGER, INTENT(OUT) :: hdferr         ! Error code.

    INTERFACE
       INTEGER FUNCTION h5dget_access_plist_c(dset_id, plist_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DGET_ACCESS_PLIST_C'::h5dget_access_plist_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(OUT) :: plist_id
       END FUNCTION h5dget_access_plist_c
    END INTERFACE

    hdferr = h5dget_access_plist_c(dset_id, plist_id)

  END SUBROUTINE h5dget_access_plist_f

END MODULE H5D


