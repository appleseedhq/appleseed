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

/*
 *  Header file for error values, etc.
 */
#ifndef H5TOOLS_ERROR_H_
#define H5TOOLS_ERROR_H_

#include "H5Epublic.h"

/* tools-HDF5 Error variables */
H5TOOLS_DLLVAR hid_t H5tools_ERR_CLS_g;
H5TOOLS_DLLVAR hid_t H5E_tools_g;
H5TOOLS_DLLVAR hid_t H5E_tools_min_id_g;

/* Use FUNC to safely handle variations of C99 __func__ keyword handling */
#ifdef H5_HAVE_C99_FUNC
#define FUNC __func__
#elif defined(H5_HAVE_FUNCTION)
#define FUNC __FUNCTION__
#else
#error "We need __func__ or __FUNCTION__ to test function names!"
#endif

/*
 * H5TOOLS_INIT_ERROR macro, used to initialize error reporting.
 */
#define H5TOOLS_INIT_ERROR() {                                                                      \
        H5tools_ERR_CLS_g = H5Eregister_class("H5tools", "HDF5:tools", lib_str);                    \
        H5E_tools_g= H5Ecreate_msg(H5tools_ERR_CLS_g, H5E_MAJOR, "Failure in tools library");       \
        H5E_tools_min_id_g = H5Ecreate_msg(H5tools_ERR_CLS_g, H5E_MINOR, "error in function");      \
}

/*
 * H5TOOLS_CLOSE_ERROR macro, used to initialize error reporting.
 */
#define H5TOOLS_CLOSE_ERROR() {                                 \
        H5Eclose_msg(H5E_tools_min_id_g);                       \
        H5Eclose_msg(H5E_tools_g);                              \
        H5Eunregister_class(H5tools_ERR_CLS_g);                 \
}

/*
 * HERR_INIT macro, used to facilitate error reporting. Declaration and assignments of error variables.
 * Use at the beginning of a function using error handling macros.
 */
#define HERR_INIT(ret_typ, ret_init)                    \
    hbool_t past_catch = FALSE;                         \
    ret_typ ret_value = ret_init;


/*
 * HERROR macro, used to facilitate error reporting .  The arguments are the major
 * error number, the minor error number, and a description of the error.
 */
#define HERROR(maj_id, min_id, str) H5Epush2(H5E_DEFAULT, __FILE__, FUNC, __LINE__, H5tools_ERR_CLS_g, maj_id, min_id, str)

/* Macro for "catching" flow of control when an error occurs.  Note that the
 *      H5_LEAVE macro won't jump back here once it's past this point.
 */
#define CATCH catch_except:; past_catch = TRUE;

/*
 * H5_LEAVE macro, used to facilitate control flow between a
 * BEGIN_FUNC() and an END_FUNC() within a function body.  The argument is
 * the return value.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch_except' label, if we're not already past it.
 */
#define H5_LEAVE(v) {                           \
    ret_value = v;                              \
    if(!past_catch)                             \
        goto catch_except;                      \
}

/*
 * H5E_THROW macro, used to facilitate error reporting within a function body.
 * The arguments are the minor error number, and an error string.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch_except' label, if we're not already past it.
 */
#define H5E_THROW(fail_value, min_id, str) {        \
    HERROR(H5E_tools_g, min_id, str);                 \
    H5_LEAVE(fail_value)                            \
}

/*
 * HGOTO_ERROR macro, used to facilitate error reporting within a function body.  The arguments are
 * the major error number, the minor error number, the return value, and an
 * error string.  The return value is assigned to a variable `ret_value' and
 * control branches to the `done' label.
 */
#define HGOTO_ERROR(fail_value, min_id, str) {        \
   HERROR(H5E_tools_g, min_id, str);                 \
   HGOTO_DONE(fail_value)                          \
}

/*
 * HGOTO_DONE macro, used to facilitate normal return within a function body.
 * The argument is the return value which is assigned to the `ret_value'
 * variable.  Control branches to the `done' label.
 */
#define HGOTO_DONE(ret_val) {ret_value = ret_val; goto done;}

#endif /* H5TOOLS_ERROR_H_ */

