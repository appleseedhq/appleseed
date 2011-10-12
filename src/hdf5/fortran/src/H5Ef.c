/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This files contains C stubs for H5E Fortran APIs */

#include "H5f90.h"
#include "H5Eprivate.h"

/*----------------------------------------------------------------------------
 * Name:        h5eclear_c
 * Purpose:     Call H5Eclear to clear the error stack for the current thread
 * Inputs:
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5eclear_c(void)
{
    int_f ret_value = 0;

    /*
     * Call H5Eclear function.
     */
    if(H5Eclear2(H5E_DEFAULT) < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5eprint_c1
 * Purpose:     Call H5Eprint to print the error stack in a default manner.
 * Inputs:      name - file name
 *              namelen - length of name
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications: Bug fix: Added call to close the file with the error messages
 *                EP 11/26/01
 *---------------------------------------------------------------------------*/
int_f
nh5eprint_c1(_fcd name, int_f* namelen)
{
    FILE *file = NULL;
    char *c_name = NULL;
    int_f ret_value = 0;

    if(NULL == (c_name = (char*)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (file = HDfopen(c_name, "a")))
        HGOTO_DONE(FAIL)

    /*
     * Call H5Eprint2 function.
     */
    if(H5Eprint2(H5E_DEFAULT, file) < 0)
        HGOTO_DONE(FAIL)

done:
    if(file)
        HDfclose(file);
    if(c_name)
        HDfree(c_name);

    return ret_value;
}


/*----------------------------------------------------------------------------
 * Name:        h5eprint_c2
 * Purpose:     Call H5Eprint to print the error stack to stderr
 *              in a default manner.
 * Inputs:
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5eprint_c2(void)
{
    int_f ret_value = 0;

    /*
     * Call H5Eprint2 function.
     */
    if(H5Eprint2(H5E_DEFAULT, NULL) < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5eget_major_c
 * Purpose:     Call H5Eget_major to get a character string
 *              describing an error specified by a major error number.
 * Inputs:      error_no - Major error number
 * Outputs:     name - character string describing the error
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5eget_major_c(int_f* error_no, _fcd name, size_t_f* namelen)
{
    char *c_name = NULL;
    size_t c_namelen = (size_t)*namelen;
    int_f ret_value = 0;

    if(c_namelen > 0) 
        c_name = (char *)HDmalloc(c_namelen + 1);

    if(!c_name)
        HGOTO_DONE(FAIL)

    /*
     * Call H5Eget_major function.
     */
    H5Eget_msg((hid_t)*error_no, NULL, c_name, c_namelen);
    HD5packFstring((char*)c_name, _fcdtocp(name), c_namelen);
    if(!HDstrcmp(c_name, "Invalid major error number"))
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5eget_minor_c
 * Purpose:     Call H5Eget_minor to get a character string
 *              describing an error specified by a minor error number.
 * Inputs:      error_no - Major error number
 * Outputs:     name - character string describing the error
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5eget_minor_c(int_f* error_no, _fcd name, size_t_f* namelen)
{
    char *c_name = NULL;
    size_t c_namelen = (size_t)*namelen;
    int_f ret_value = 0;

    if(c_namelen > 0) 
        c_name = (char *)HDmalloc(c_namelen + 1);

    if(!c_name)
        HGOTO_DONE(FAIL)

    /*
     * Call H5Eget_minor function.
     */
    H5Eget_msg((hid_t)*error_no, NULL, c_name, c_namelen);
    HD5packFstring((char *)c_name, _fcdtocp(name), c_namelen);
    if(!HDstrcmp(c_name, "Invalid minor error number"))
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/*----------------------------------------------------------------------------
 * Name:        h5eset_auto_c
 * Purpose:     Call H5Eset_auto to turn automatic error printing on or off.
 * Inputs:      printflag - flag to turn automatic error printing on or off.
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, November 17, 2000
 * Modifications:  major bug fix. Function never disabled printing.
 *---------------------------------------------------------------------------*/
int_f
nh5eset_auto_c(int_f* printflag)
{
    herr_t status = -1;
    int_f ret_value = 0;

    if(*printflag == 1)
        status = H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)H5Eprint2, stderr);
    else if(*printflag == 0)
        status = H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    if(status < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}

