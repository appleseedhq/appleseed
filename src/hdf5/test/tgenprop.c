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

/***********************************************************
*
* Test program:	 tgenprop
*
* Test the Generic Property functionality
*
*************************************************************/

#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

/* Define this macro to indicate that the testing APIs should be available */
#define H5P_TESTING

#include "testhdf5.h"
#include "hdf5.h"
#include "H5Dprivate.h"         /* For Dataset creation property list names */
#include "H5Ppkg.h"		/* Generic Properties			*/

#define FILENAME   "tgenprop.h5"

/* Property definitions */
#define CLASS1_NAME     "Class 1"
#define CLASS1_PATH     "root/Class 1"

#define CLASS2_NAME     "Class 2"
#define CLASS2_PATH     "root/Class 1/Class 2"

/* Property definitions */
#define PROP1_NAME     "Property 1"
int         prop1_def=10;   /* Property 1 default value */
#define PROP1_SIZE      sizeof(prop1_def)
#define PROP1_DEF_VALUE (&prop1_def)

#define PROP2_NAME     "Property 2"
float         prop2_def=(float)3.14;   /* Property 2 default value */
#define PROP2_SIZE      sizeof(prop2_def)
#define PROP2_DEF_VALUE (&prop2_def)

#define PROP3_NAME     "Property 3"
char          prop3_def[10]="Ten chars";   /* Property 3 default value */
#define PROP3_SIZE      sizeof(prop3_def)
#define PROP3_DEF_VALUE (&prop3_def)

#define PROP4_NAME     "Property 4"
double          prop4_def=1.41;   /* Property 4 default value */
#define PROP4_SIZE      sizeof(prop4_def)
#define PROP4_DEF_VALUE (&prop4_def)

/****************************************************************
**
**  test_genprop_basic_class(): Test basic generic property list code.
**      Tests creating new generic classes.
**
****************************************************************/
static void
test_genprop_basic_class(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		cid2;		/* Generic Property class ID */
    hid_t		cid3;		/* Generic Property class ID */
    char       *name;       /* Name of class */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Class Creation Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS1_NAME)!=0)
        TestErrPrintf("Class names don't match!, name=%s, CLASS1_NAME=%s\n",name,CLASS1_NAME);
    free(name);

    /* Check class parent */
    cid2 = H5Pget_class_parent(cid1);
    CHECK_I(cid2, "H5Pget_class_parent");

    /* Verify class parent correct */
    ret = H5Pequal(cid2,H5P_ROOT);
    VERIFY(ret, 1, "H5Pequal");

    /* Make certain false postives aren't being returned */
    ret = H5Pequal(cid2,H5P_FILE_CREATE);
    VERIFY(ret, 0, "H5Pequal");

    /* Close parent class */
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

    /* Create another new generic class, derived from file creation class */
    cid1 = H5Pcreate_class(H5P_FILE_CREATE,CLASS2_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS2_NAME)!=0)
        TestErrPrintf("Class names don't match!, name=%s, CLASS2_NAME=%s\n",name,CLASS2_NAME);
    free(name);

    /* Check class parent */
    cid2 = H5Pget_class_parent(cid1);
    CHECK_I(cid2, "H5Pget_class_parent");

    /* Verify class parent correct */
    ret = H5Pequal(cid2,H5P_FILE_CREATE);
    VERIFY(ret, 1, "H5Pequal");

    /* Check class parent's parent */
    cid3 = H5Pget_class_parent(cid2);
    CHECK_I(cid3, "H5Pget_class_parent");

    /* Verify class parent's parent correct */
    ret = H5Pequal(cid3,H5P_GROUP_CREATE);
    VERIFY(ret, 1, "H5Pequal");

    /* Close parent class's parent */
    ret = H5Pclose_class(cid3);
    CHECK_I(ret, "H5Pclose_class");

    /* Close parent class */
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_basic_class() */

/****************************************************************
**
**  test_genprop_basic_class_prop(): Test basic generic property list code.
**      Tests adding properties to generic classes.
**
****************************************************************/
static void
test_genprop_basic_class_prop(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    size_t		size;		/* Size of property */
    size_t		nprops;		/* Number of properties in class */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Class Properties Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT, CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 0, "H5Pget_nprops");

    /* Check the existance of the first property (should fail) */
    ret = H5Pexist(cid1, PROP1_NAME);
    VERIFY(ret, 0, "H5Pexist");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Try to insert the first property again (should fail) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    VERIFY(ret, FAIL, "H5Pregister2");

    /* Check the existance of the first property */
    ret = H5Pexist(cid1, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the first property */
    ret = H5Pget_size(cid1, PROP1_NAME, &size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP1_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Try to insert the second property again (should fail) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    VERIFY(ret, FAIL, "H5Pregister2");

    /* Check the existance of the second property */
    ret = H5Pexist(cid1, PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the second property */
    ret = H5Pget_size(cid1, PROP2_NAME, &size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP2_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check the existance of the third property */
    ret = H5Pexist(cid1, PROP3_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the third property */
    ret = H5Pget_size(cid1, PROP3_NAME, &size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP3_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Unregister first property */
    ret = H5Punregister(cid1, PROP1_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Try to check the size of the first property (should fail) */
    ret = H5Pget_size(cid1, PROP1_NAME, &size);
    VERIFY(ret, FAIL, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Unregister second property */
    ret = H5Punregister(cid1, PROP2_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Unregister third property */
    ret = H5Punregister(cid1, PROP3_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 0, "H5Pget_nprops");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_basic_class_prop() */

/****************************************************************
**
**  test_genprop_iter1(): Property iterator for test_genprop_class_iter
**
****************************************************************/
static int
test_genprop_iter1(hid_t id, const char *name, void *iter_data)
{
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } *iter_struct = iter_data;

    /* Shut compiler up */
    id = id;

    return(HDstrcmp(name,iter_struct->names[iter_struct->iter_count++]));
}

/****************************************************************
**
**  test_genprop_class_iter(): Test basic generic property list code.
**      Tests iterating over properties in a generic class.
**
****************************************************************/
static void
test_genprop_class_iter(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    size_t		nprops;		/* Number of properties in class */
    int         idx;        /* Index to start iteration at */
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } iter_struct;
    const char *pnames[4]={ /* Names of properties for iterator */
        PROP1_NAME,
        PROP2_NAME,
        PROP3_NAME,
        PROP4_NAME};
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Class Property Iteration Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT, CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP4_NAME, PROP4_SIZE, PROP4_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Iterate over all properties in class */
    iter_struct.iter_count=0;
    iter_struct.names=pnames;
    ret = H5Piterate(cid1,NULL,test_genprop_iter1,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");

    /* Iterate over last three properties in class */
    idx=iter_struct.iter_count=1;
    ret = H5Piterate(cid1,&idx,test_genprop_iter1,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");
    VERIFY(idx, (int)nprops, "H5Piterate");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_class_iter() */

/****************************************************************
**
**  test_genprop_cls_*_cb1(): Property List callbacks for test_genprop_class_callback
**
****************************************************************/
static herr_t
test_genprop_cls_crt_cb1(hid_t list_id, void *create_data)
{
    struct {                /* Struct for iterations */
        int count;
        hid_t id;
    } *count_struct=create_data;

    count_struct->count++;
    count_struct->id=list_id;

    return(SUCCEED);
}

static herr_t
test_genprop_cls_cpy_cb1(hid_t new_list_id, hid_t UNUSED old_list_id, void *copy_data)
{
    struct {                /* Struct for iterations */
        int count;
        hid_t id;
    } *count_struct=copy_data;

    count_struct->count++;
    count_struct->id=new_list_id;

    return(SUCCEED);
}

static herr_t
test_genprop_cls_cls_cb1(hid_t list_id, void *create_data)
{
    struct {                /* Struct for iterations */
        int count;
        hid_t id;
    } *count_struct=create_data;

    count_struct->count++;
    count_struct->id=list_id;

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_class_callback(): Test basic generic property list code.
**      Tests callbacks for property lists in a generic class.
**
****************************************************************/
static void
test_genprop_class_callback(void)
{
    hid_t	cid1;		/* Generic Property class ID */
    hid_t	cid2;		/* Generic Property class ID */
    hid_t	lid1;		/* Generic Property list ID */
    hid_t	lid2;		/* Generic Property list ID */
    hid_t	lid3;		/* Generic Property list ID */
    size_t	nprops;		/* Number of properties in class */
    struct {                    /* Struct for callbacks */
        int count;
        hid_t id;
    } crt_cb_struct, cpy_cb_struct, cls_cb_struct;
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Class Callback Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT, CLASS1_NAME, test_genprop_cls_crt_cb1, &crt_cb_struct, test_genprop_cls_cpy_cb1, &cpy_cb_struct, test_genprop_cls_cls_cb1, &cls_cb_struct);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Initialize class callback structs */
    crt_cb_struct.count=0;
    crt_cb_struct.id=(-1);
    cpy_cb_struct.count=0;
    cpy_cb_struct.id=(-1);
    cls_cb_struct.count=0;
    cls_cb_struct.id=(-1);

    /* Create a property list from the class */
    lid1 = H5Pcreate(cid1);
    CHECK_I(lid1, "H5Pcreate");

    /* Verify that the creation callback occurred */
    VERIFY(crt_cb_struct.count, 1, "H5Pcreate");
    VERIFY(crt_cb_struct.id, lid1, "H5Pcreate");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Create another property list from the class */
    lid2 = H5Pcreate(cid1);
    CHECK_I(lid2, "H5Pcreate");

    /* Verify that the creation callback occurred */
    VERIFY(crt_cb_struct.count, 2, "H5Pcreate");
    VERIFY(crt_cb_struct.id, lid2, "H5Pcreate");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid2,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Create another property list by copying an existing list */
    lid3 = H5Pcopy(lid1);
    CHECK_I(lid3, "H5Pcopy");

    /* Verify that the copy callback occurred */
    VERIFY(cpy_cb_struct.count, 1, "H5Pcopy");
    VERIFY(cpy_cb_struct.id, lid3, "H5Pcopy");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid3, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Close first list */
    ret = H5Pclose(lid1);
    CHECK_I(ret, "H5Pclose");

    /* Verify that the close callback occurred */
    VERIFY(cls_cb_struct.count, 1, "H5Pclose");
    VERIFY(cls_cb_struct.id, lid1, "H5Pclose");

    /* Close second list */
    ret = H5Pclose(lid2);
    CHECK_I(ret, "H5Pclose");

    /* Verify that the close callback occurred */
    VERIFY(cls_cb_struct.count, 2, "H5Pclose");
    VERIFY(cls_cb_struct.id, lid2, "H5Pclose");

    /* Close third list */
    ret = H5Pclose(lid3);
    CHECK_I(ret, "H5Pclose");

    /* Verify that the close callback occurred */
    VERIFY(cls_cb_struct.count, 3, "H5Pclose");
    VERIFY(cls_cb_struct.id, lid3, "H5Pclose");

    /* Create another new generic class, derived from first class */
    cid2 = H5Pcreate_class(cid1, CLASS2_NAME, test_genprop_cls_crt_cb1, &crt_cb_struct, test_genprop_cls_cpy_cb1, &cpy_cb_struct, test_genprop_cls_cls_cb1, &cls_cb_struct);
    CHECK_I(cid2, "H5Pcreate_class");

    /* Insert fourth property into class (with no callbacks) */
    ret = H5Pregister2(cid2, PROP4_NAME, PROP4_SIZE, PROP4_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check the number of properties in class */
    /* (only reports the number of properties in 2nd class) */
    ret = H5Pget_nprops(cid2, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Create a property list from the 2nd class */
    lid1 = H5Pcreate(cid2);
    CHECK_I(lid1, "H5Pcreate");

    /* Verify that both of the creation callbacks occurred */
    VERIFY(crt_cb_struct.count, 4, "H5Pcreate");
    VERIFY(crt_cb_struct.id, lid1, "H5Pcreate");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Create another property list by copying existing list */
    lid2 = H5Pcopy(lid1);
    CHECK_I(lid2, "H5Pcopy");

    /* Verify that both of the copy callbacks occurred */
    VERIFY(cpy_cb_struct.count, 3, "H5Pcopy");
    VERIFY(cpy_cb_struct.id, lid2, "H5Pcopy");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid2, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Close first list */
    ret = H5Pclose(lid1);
    CHECK_I(ret, "H5Pclose");

    /* Verify that both of the close callbacks occurred */
    VERIFY(cls_cb_struct.count, 5, "H5Pclose");
    VERIFY(cls_cb_struct.id, lid1, "H5Pclose");

    /* Close second list */
    ret = H5Pclose(lid2);
    CHECK_I(ret, "H5Pclose");

    /* Verify that both of the close callbacks occurred */
    VERIFY(cls_cb_struct.count, 7, "H5Pclose");
    VERIFY(cls_cb_struct.id, lid2, "H5Pclose");

    /* Close classes */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_class_callback() */

/****************************************************************
**
**  test_genprop_basic_list(): Test basic generic property list code.
**      Tests creating new generic property lists.
**
****************************************************************/
static void
test_genprop_basic_list(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		cid2;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property list ID */
    size_t		nprops;		/* Number of properties */
    size_t		size;		/* Size of property */
    int                 prop1_value;    /* Value for property #1 */
    float               prop2_value;    /* Value for property #2 */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Creation Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Add several properties (w/default values) */

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Create a property list from the class */
    lid1 = H5Pcreate(cid1);
    CHECK_I(lid1, "H5Pcreate");

    /* Get the list's class */
    cid2 = H5Pget_class(lid1);
    CHECK_I(cid2, "H5Pget_class");

    /* Check that the list's class is correct */
    ret = H5Pequal(cid1,cid2);
    VERIFY(ret, 1, "H5Pequal");

    /* Check correct "is a" class/list relationship */
    ret = H5Pisa_class(lid1,cid1);
    VERIFY(ret, 1, "H5Pisa_class");

    /* Check "is a" class/list relationship another way */
    ret = H5Pisa_class(lid1,cid2);
    VERIFY(ret, 1, "H5Pisa_class");

    /* Close class */
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Check existence of properties */
    ret = H5Pexist(lid1, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1, PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the sizes of the properties */
    ret = H5Pget_size(lid1, PROP1_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP1_SIZE, "H5Pget_size");
    ret = H5Pget_size(lid1, PROP2_NAME,&size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP2_SIZE, "H5Pget_size");

    /* Check values of properties (set with default values) */
    ret = H5Pget(lid1, PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");
    ret = H5Pget(lid1, PROP2_NAME,&prop2_value);
    CHECK_I(ret, "H5Pget");
    /* Verify the floating-poing value in this way to avoid compiler warning. */
    if(!FLT_ABS_EQUAL(prop2_value,*PROP2_DEF_VALUE))
	printf("*** UNEXPECTED VALUE from %s should be %f, but is %f at line %4d in %s\n",
	    "H5Pget", *PROP2_DEF_VALUE, prop2_value, (int)__LINE__, __FILE__);


    /* Close list */
    ret = H5Pclose(lid1);
    CHECK_I(ret, "H5Pclose");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* end test_genprop_basic_list() */

/****************************************************************
**
**  test_genprop_basic_list_prop(): Test basic generic property list code.
**      Tests creating new generic property lists and adding and
**      removing properties from them.
**
****************************************************************/
static void
test_genprop_basic_list_prop(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property list ID */
    size_t		nprops;		/* Number of properties */
    int                 prop1_value;    /* Value for property #1 */
    float               prop2_value;    /* Value for property #2 */
    char                prop3_value[10];/* Property #3 value */
    double              prop4_value;    /* Property #4 value */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Property Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Add several properties (several w/default values) */

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Create a property list from the class */
    lid1 = H5Pcreate(cid1);
    CHECK_I(lid1, "H5Pcreate");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Add temporary properties */

    /* Insert first temporary property into class (with no callbacks) */
    ret = H5Pinsert2(lid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pinsert2");

    /* Insert second temporary property into class (with no callbacks) */
    ret = H5Pinsert2(lid1, PROP4_NAME, PROP4_SIZE, PROP4_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pinsert2");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Check existence of all properties */
    ret = H5Pexist(lid1, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1, PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1, PROP3_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1, PROP4_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of permanent properties (set with default values) */
    ret = H5Pget(lid1, PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");
    ret = H5Pget(lid1, PROP2_NAME,&prop2_value);
    CHECK_I(ret, "H5Pget");
    /* Verify the floating-poing value in this way to avoid compiler warning. */
    if(!FLT_ABS_EQUAL(prop2_value,*PROP2_DEF_VALUE))
	printf("*** UNEXPECTED VALUE from %s should be %f, but is %f at line %4d in %s\n",
	    "H5Pget", *PROP2_DEF_VALUE, prop2_value, (int)__LINE__, __FILE__);


    /* Check values of temporary properties (set with regular values) */
    ret = H5Pget(lid1, PROP3_NAME,&prop3_value);
    CHECK_I(ret, "H5Pget");
    if(HDmemcmp(&prop3_value, PROP3_DEF_VALUE, PROP3_SIZE)!=0)
        TestErrPrintf("Property #3 doesn't match!, line=%d\n",__LINE__);
    ret = H5Pget(lid1, PROP4_NAME,&prop4_value);
    CHECK_I(ret, "H5Pget");
    /* Verify the floating-poing value in this way to avoid compiler warning. */
    if(!FLT_ABS_EQUAL(prop4_value,*PROP4_DEF_VALUE))
	printf("*** UNEXPECTED VALUE from %s should be %f, but is %f at line %4d in %s\n",
	    "H5Pget", *PROP4_DEF_VALUE, prop4_value, (int)__LINE__, __FILE__);

    /* Delete permanent property */
    ret = H5Premove(lid1, PROP2_NAME);
    CHECK_I(ret, "H5Premove");

    /* Check number of properties */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Delete temporary property */
    ret = H5Premove(lid1, PROP3_NAME);
    CHECK_I(ret, "H5Premove");

    /* Check number of properties */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Check existence of remaining properties */
    ret = H5Pexist(lid1, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");
    ret = H5Pexist(lid1, PROP4_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of permanent properties (set with default values) */
    ret = H5Pget(lid1, PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Check values of temporary properties (set with regular values) */
    ret = H5Pget(lid1, PROP4_NAME,&prop4_value);
    CHECK_I(ret, "H5Pget");
    /* Verify the floating-poing value in this way to avoid compiler warning. */
    if(!FLT_ABS_EQUAL(prop4_value,*PROP4_DEF_VALUE))
	printf("*** UNEXPECTED VALUE from %s should be %f, but is %f at line %4d in %s\n",
	    "H5Pget", *PROP4_DEF_VALUE, prop4_value, (int)__LINE__, __FILE__);

    /* Close list */
    ret = H5Pclose(lid1);
    CHECK_I(ret, "H5Pclose");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* end test_genprop_basic_list_prop() */

/****************************************************************
**
**  test_genprop_iter2(): Property iterator for test_genprop_list_iter
**
****************************************************************/
static int
test_genprop_iter2(hid_t id, const char *name, void *iter_data)
{
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } *iter_struct=iter_data;

    /* Shut compiler up */
    id=id;

    return(HDstrcmp(name,iter_struct->names[iter_struct->iter_count++]));
}

/****************************************************************
**
**  test_genprop_list_iter(): Test basic generic property list code.
**      Tests iterating over generic property list properties.
**
****************************************************************/
static void
test_genprop_list_iter(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property list ID */
    size_t		nprops;		/* Number of properties */
    int         idx;        /* Index to start iteration at */
    struct {                /* Struct for iterations */
        int iter_count;
        const char **names;
    } iter_struct;
    const char *pnames[4]={ /* Names of properties for iterator */
        PROP3_NAME,
        PROP4_NAME,
        PROP1_NAME,
        PROP2_NAME
        };
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Generic Property List Iteration Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Add several properties (several w/default values) */

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Create a property list from the class */
    lid1 = H5Pcreate(cid1);
    CHECK_I(lid1, "H5Pcreate");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Add temporary properties */

    /* Insert first temporary property into class (with no callbacks) */
    ret = H5Pinsert2(lid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pinsert2");

    /* Insert second temporary property into class (with no callbacks) */
    ret = H5Pinsert2(lid1, PROP4_NAME, PROP4_SIZE, PROP4_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pinsert2");

    /* Check the number of properties in list */
    ret = H5Pget_nprops(lid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Iterate over all properties in list */
    iter_struct.iter_count=0;
    iter_struct.names=pnames;
    ret = H5Piterate(lid1,NULL,test_genprop_iter2,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");

    /* Iterate over last three properties in list */
    idx=iter_struct.iter_count=1;
    ret = H5Piterate(lid1,&idx,test_genprop_iter2,&iter_struct);
    VERIFY(ret, 0, "H5Piterate");
    VERIFY(idx, (int)nprops, "H5Piterate");

    /* Close list */
    ret = H5Pclose(lid1);
    CHECK_I(ret, "H5Pclose");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* end test_genprop_list_iter() */

typedef struct {
    /* Creation information */
    int crt_count;
    char *crt_name;
    void *crt_value;

    /* Set information */
    int set_count;
    hid_t set_plist_id;
    char *set_name;
    void *set_value;

    /* Get information */
    int get_count;
    hid_t get_plist_id;
    char *get_name;
    void *get_value;

    /* Delete information */
    int del_count;
    hid_t del_plist_id;
    char *del_name;
    void *del_value;

    /* Copy information */
    int cop_count;
    char *cop_name;
    void *cop_value;

    /* Compare information */
    int cmp_count;

    /* Close information */
    int cls_count;
    char *cls_name;
    void *cls_value;
} prop_cb_info;

/* Global variables for Callback information */
prop_cb_info prop1_cb_info;     /* Callback statistics for property #1 */
prop_cb_info prop2_cb_info;     /* Callback statistics for property #2 */
prop_cb_info prop3_cb_info;     /* Callback statistics for property #3 */

/****************************************************************
**
**  test_genprop_cls_cpy_cb2(): Property Class callback for test_genprop_list_callback
**
****************************************************************/
static herr_t
test_genprop_cls_cpy_cb2(hid_t new_list_id, hid_t UNUSED old_list_id, void *create_data)
{
    struct {                /* Struct for iterations */
        int count;
        hid_t id;
    } *count_struct=create_data;

    count_struct->count++;
    count_struct->id=new_list_id;

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_crt_cb1(): Property creation callback for test_genprop_list_callback
**
****************************************************************/
static herr_t
test_genprop_prop_crt_cb1(const char *name, size_t size, void *def_value)
{
    /* Set the information from the creation call */
    prop1_cb_info.crt_count++;
    prop1_cb_info.crt_name=HDstrdup(name);
    prop1_cb_info.crt_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.crt_value,def_value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_set_cb1(): Property set callback for test_genprop_list_callback
**
****************************************************************/
static herr_t
test_genprop_prop_set_cb1(hid_t plist_id, const char *name, size_t size, void *value)
{
    /* Set the information from the set call */
    prop1_cb_info.set_count++;
    prop1_cb_info.set_plist_id=plist_id;
    if(prop1_cb_info.set_name==NULL)
        prop1_cb_info.set_name=HDstrdup(name);
    if(prop1_cb_info.set_value==NULL)
        prop1_cb_info.set_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.set_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_get_cb1(): Property get callback for test_genprop_list_callback
**
****************************************************************/
static herr_t
test_genprop_prop_get_cb1(hid_t plist_id, const char *name, size_t size, void *value)
{
    /* Set the information from the get call */
    prop1_cb_info.get_count++;
    prop1_cb_info.get_plist_id=plist_id;
    if(prop1_cb_info.get_name==NULL)
        prop1_cb_info.get_name=HDstrdup(name);
    if(prop1_cb_info.get_value==NULL)
        prop1_cb_info.get_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.get_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_cop_cb1(): Property copy callback for test_genprop_list_callback
**
****************************************************************/
static herr_t
test_genprop_prop_cop_cb1(const char *name, size_t size, void *value)
{
    /* Set the information from the get call */
    prop1_cb_info.cop_count++;
    if(prop1_cb_info.cop_name==NULL)
        prop1_cb_info.cop_name=HDstrdup(name);
    if(prop1_cb_info.cop_value==NULL)
        prop1_cb_info.cop_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.cop_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_cmp_cb1(): Property comparison callback for test_genprop_list_callback
**
****************************************************************/
static int
test_genprop_prop_cmp_cb1(const void *value1, const void *value2, size_t size)
{
    /* Set the information from the comparison call */
    prop1_cb_info.cmp_count++;

    return(HDmemcmp(value1, value2, size));
}

/****************************************************************
**
**  test_genprop_prop_cmp_cb3(): Property comparison callback for test_genprop_list_callback
**
****************************************************************/
static int
test_genprop_prop_cmp_cb3(const void *value1, const void *value2, size_t size)
{
    /* Set the information from the comparison call */
    prop3_cb_info.cmp_count++;

    return(HDmemcmp(value1, value2, size));
}

/****************************************************************
**
**  test_genprop_prop_cls_cb1(): Property close callback for test_genprop_list_callback
**
****************************************************************/
static herr_t
test_genprop_prop_cls_cb1(const char *name, size_t size, void *value)
{
    /* Set the information from the close call */
    prop1_cb_info.cls_count++;
    if(prop1_cb_info.cls_name==NULL)
        prop1_cb_info.cls_name=HDstrdup(name);
    if(prop1_cb_info.cls_value==NULL)
        prop1_cb_info.cls_value=HDmalloc(size);
    HDmemcpy(prop1_cb_info.cls_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_prop_del_cb2(): Property delete callback for test_genprop_list_callback
**
****************************************************************/
static herr_t
test_genprop_prop_del_cb2(hid_t plist_id, const char *name, size_t size, void *value)
{
    /* Set the information from the delete call */
    prop2_cb_info.del_count++;
    prop2_cb_info.del_plist_id=plist_id;
    prop2_cb_info.del_name=HDstrdup(name);
    prop2_cb_info.del_value=HDmalloc(size);
    HDmemcpy(prop2_cb_info.del_value,value,size);

    return(SUCCEED);
}

/****************************************************************
**
**  test_genprop_list_callback(): Test basic generic property list code.
**      Tests callbacks for properties in a generic property list.
**
****************************************************************/
static void
test_genprop_list_callback(void)
{
    hid_t	cid1;		/* Generic Property class ID */
    hid_t	lid1;		/* Generic Property list ID */
    hid_t	lid2;		/* 2nd Generic Property list ID */
    size_t	nprops;		/* Number of properties in class */
    int         prop1_value;    /* Value for property #1 */
    int         prop1_new_value=20;   /* Property #1 new value */
    float       prop2_value;    /* Value for property #2 */
    char        prop3_value[10];/* Property #3 value */
    char        prop3_new_value[10]="10 chairs";    /* Property #3 new value */
    double      prop4_value;    /* Property #4 value */
    struct {                    /* Struct for callbacks */
        int count;
        hid_t id;
    } cop_cb_struct;
    herr_t	ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Property Callback Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL,test_genprop_cls_cpy_cb2,&cop_cb_struct,NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE,test_genprop_prop_crt_cb1,test_genprop_prop_set_cb1,test_genprop_prop_get_cb1,NULL,test_genprop_prop_cop_cb1,test_genprop_prop_cmp_cb1,test_genprop_prop_cls_cb1);
    CHECK_I(ret, "H5Pregister2");

    /* Insert second property into class (with only delete callback) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL,test_genprop_prop_del_cb2,NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert third property into class (with only compare callback) */
    ret = H5Pregister2(cid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, test_genprop_prop_cmp_cb3, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert fourth property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP4_NAME, PROP4_SIZE, PROP4_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1,&nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 4, "H5Pget_nprops");

    /* Initialize class callback structs */
    cop_cb_struct.count=0;
    cop_cb_struct.id=(-1);

    /* Initialize callback information for properties tracked */
    HDmemset(&prop1_cb_info,0,sizeof(prop_cb_info));
    HDmemset(&prop2_cb_info,0,sizeof(prop_cb_info));
    HDmemset(&prop3_cb_info,0,sizeof(prop_cb_info));

    /* Create a property list from the class */
    lid1 = H5Pcreate(cid1);
    CHECK_I(lid1, "H5Pcreate");

    /* The compare callback should have been called once on property 1 (to check
     * if the create callback modified the value) */
    VERIFY(prop1_cb_info.cmp_count, 1, "H5Pequal");
    /* The compare callback should not have been called on property 3, as there
     * is no create callback */
    VERIFY(prop3_cb_info.cmp_count, 0, "H5Pequal");

    /* Verify creation callback information for properties tracked */
    VERIFY(prop1_cb_info.crt_count, 1, "H5Pcreate");
    if(HDstrcmp(prop1_cb_info.crt_name, PROP1_NAME)!=0)
        TestErrPrintf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    if(HDmemcmp(prop1_cb_info.crt_value, PROP1_DEF_VALUE, PROP1_SIZE)!=0)
        TestErrPrintf("Property #1 value doesn't match!, line=%d\n",__LINE__);

    /* Check values of permanent properties (set with default values) */
    ret = H5Pget(lid1, PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");
    /* The compare callback should have been called once (to check if the get
     * callback modified the value) */
    VERIFY(prop1_cb_info.cmp_count, 2, "H5Pequal");
    ret = H5Pget(lid1, PROP2_NAME,&prop2_value);
    CHECK_I(ret, "H5Pget");
    /* Verify the floating-poing value in this way to avoid compiler warning. */
    if(!FLT_ABS_EQUAL(prop2_value,*PROP2_DEF_VALUE))
	printf("*** UNEXPECTED VALUE from %s should be %f, but is %f at line %4d in %s\n",
	    "H5Pget", *PROP2_DEF_VALUE, prop2_value, (int)__LINE__, __FILE__);

    /* Check values of temporary properties (set with regular values) */
    ret = H5Pget(lid1, PROP3_NAME,&prop3_value);
    CHECK_I(ret, "H5Pget");
    if(HDmemcmp(&prop3_value, PROP3_DEF_VALUE, PROP3_SIZE)!=0)
        TestErrPrintf("Property #3 doesn't match!, line=%d\n",__LINE__);
    /* The compare callback should not have been called, as there is no get
     * callback for this property */
    VERIFY(prop3_cb_info.cmp_count, 0, "H5Pequal");
    ret = H5Pget(lid1, PROP4_NAME,&prop4_value);
    CHECK_I(ret, "H5Pget");
    /* Verify the floating-poing value in this way to avoid compiler warning. */
    if(!FLT_ABS_EQUAL(prop4_value,*PROP4_DEF_VALUE))
	printf("*** UNEXPECTED VALUE from %s should be %f, but is %f at line %4d in %s\n",
	    "H5Pget", *PROP4_DEF_VALUE, prop4_value, (int)__LINE__, __FILE__);

    /* Verify get callback information for properties tracked */
    VERIFY(prop1_cb_info.get_count, 1, "H5Pget");
    VERIFY(prop1_cb_info.get_plist_id, lid1, "H5Pget");
    if(HDstrcmp(prop1_cb_info.get_name, PROP1_NAME)!=0)
        TestErrPrintf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    if(HDmemcmp(prop1_cb_info.get_value, PROP1_DEF_VALUE, PROP1_SIZE)!=0)
        TestErrPrintf("Property #1 value doesn't match!, line=%d\n",__LINE__);

    /* Set value of property #1 to different value */
    ret = H5Pset(lid1, PROP1_NAME,&prop1_new_value);
    CHECK_I(ret, "H5Pset");

    /* Verify set callback information for properties tracked */
    VERIFY(prop1_cb_info.set_count, 1, "H5Pset");
    VERIFY(prop1_cb_info.set_plist_id, lid1, "H5Pset");
    if(HDstrcmp(prop1_cb_info.set_name, PROP1_NAME)!=0)
        TestErrPrintf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    if(HDmemcmp(prop1_cb_info.set_value,&prop1_new_value, PROP1_SIZE)!=0)
        TestErrPrintf("Property #1 value doesn't match!, line=%d\n",__LINE__);

    /* The compare callback should have been called once (to check if the new
     * value needed to be copied onto the property list) */
    VERIFY(prop1_cb_info.cmp_count, 3, "H5Pequal");

    /* Set value of property #3 to different value */
    ret = H5Pset(lid1, PROP3_NAME,prop3_new_value);
    CHECK_I(ret, "H5Pset");

    /* The compare callback should have been called once (to check if the new
     * value needed to be copied onto the property list) */
    VERIFY(prop3_cb_info.cmp_count, 1, "H5Pequal");

    /* Check new value of tracked properties */
    ret = H5Pget(lid1, PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, prop1_new_value, "H5Pget");

    /* Verify get callback information again for properties tracked */
    VERIFY(prop1_cb_info.get_count, 2, "H5Pget");
    VERIFY(prop1_cb_info.get_plist_id, lid1, "H5Pget");
    if(HDstrcmp(prop1_cb_info.get_name, PROP1_NAME)!=0)
        TestErrPrintf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    if(HDmemcmp(prop1_cb_info.get_value,&prop1_new_value, PROP1_SIZE)!=0)
        TestErrPrintf("Property #1 value doesn't match!, line=%d\n",__LINE__);

    /* Delete property #2 */
    ret = H5Premove(lid1, PROP2_NAME);
    CHECK_I(ret, "H5Premove");

    /* Verify delete callback information for properties tracked */
    VERIFY(prop2_cb_info.del_count, 1, "H5Premove");
    VERIFY(prop2_cb_info.del_plist_id, lid1, "H5Premove");
    if(HDstrcmp(prop2_cb_info.del_name, PROP2_NAME)!=0)
        TestErrPrintf("Property #2 name doesn't match!, line=%d\n",__LINE__);
    if(HDmemcmp(prop2_cb_info.del_value, PROP2_DEF_VALUE, PROP2_SIZE)!=0)
        TestErrPrintf("Property #2 value doesn't match!, line=%d\n",__LINE__);

    /* Copy first list */
    lid2 = H5Pcopy(lid1);
    CHECK_I(lid2, "H5Pcopy");

    /* Verify copy callback information for properties tracked */
    VERIFY(prop1_cb_info.cop_count, 1, "H5Pcopy");
    if(HDstrcmp(prop1_cb_info.cop_name, PROP1_NAME)!=0)
        TestErrPrintf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    if(HDmemcmp(prop1_cb_info.cop_value,&prop1_new_value, PROP1_SIZE)!=0)
        TestErrPrintf("Property #1 value doesn't match!, line=%d\n",__LINE__);

    /* Verify that the class creation callback occurred */
    VERIFY(cop_cb_struct.count, 1, "H5Pcopy");
    VERIFY(cop_cb_struct.id, lid2, "H5Pcopy");

    /* Compare the two lists */
    ret = H5Pequal(lid1,lid2);
    VERIFY(ret, 1, "H5Pequal");

    /* Verify compare callback information for properties tracked */
    VERIFY(prop1_cb_info.cmp_count, 4, "H5Pequal");
    VERIFY(prop3_cb_info.cmp_count, 2, "H5Pequal");

    /* Close first list */
    ret = H5Pclose(lid1);
    CHECK_I(ret, "H5Pclose");

    /* Verify close callback information for properties tracked */
    VERIFY(prop1_cb_info.cls_count, 1, "H5Pclose");
    if(HDstrcmp(prop1_cb_info.cls_name, PROP1_NAME)!=0)
        TestErrPrintf("Property #1 name doesn't match!, line=%d\n",__LINE__);
    if(HDmemcmp(prop1_cb_info.cls_value,&prop1_new_value, PROP1_SIZE)!=0)
        TestErrPrintf("Property #1 value doesn't match!, line=%d\n",__LINE__);

    /* Close second list */
    ret = H5Pclose(lid2);
    CHECK_I(ret, "H5Pclose");

    /* Verify close callback information for properties tracked */
    VERIFY(prop1_cb_info.cls_count, 2, "H5Pclose");

    /* Free memory allocated for tracking properties */
    HDfree(prop1_cb_info.crt_name);
    HDfree(prop1_cb_info.crt_value);
    HDfree(prop1_cb_info.get_name);
    HDfree(prop1_cb_info.get_value);
    HDfree(prop1_cb_info.set_name);
    HDfree(prop1_cb_info.set_value);
    HDfree(prop1_cb_info.cop_name);
    HDfree(prop1_cb_info.cop_value);
    HDfree(prop1_cb_info.cls_name);
    HDfree(prop1_cb_info.cls_value);
    HDfree(prop2_cb_info.del_name);
    HDfree(prop2_cb_info.del_value);

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_list_callback() */

/****************************************************************
**
**  test_genprop_list_addprop(): Test adding properties to a
**      standard HDF5 property list and verify that the library
**      ignores the extra properties.
**
****************************************************************/
static void
test_genprop_list_addprop(void)
{
    hid_t fid;          /* File ID */
    hid_t did;          /* Dataset ID */
    hid_t sid;          /* Dataspace ID */
    hid_t pid;          /* Property List ID */
    int   prop1_value;  /* Value for property #1 */
    herr_t ret;		/* Generic return value	*/

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create scalar dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset creation property list */
    pid = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(pid, FAIL, "H5Pcreate");

    /* Insert temporary property into class (with no callbacks) */
    ret = H5Pinsert2(pid, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pinsert2");

    /* Check existence of added property */
    ret = H5Pexist(pid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of property (set with default value) */
    ret = H5Pget(pid, PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Create a dataset */
    did = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check existence of added property (after using property list) */
    ret = H5Pexist(pid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of property (set with default value) (after using property list) */
    ret = H5Pget(pid, PROP1_NAME, &prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Close property list */
    ret = H5Pclose(pid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_genprop_list_addprop() */

/****************************************************************
**
**  test_genprop_class_addprop(): Test adding properties to a
**      standard HDF5 property class and verify that the library
**      ignores the extra properties and continues to recognize the
**      derived class as a valid version of the derived-from class.
**
****************************************************************/
static void
test_genprop_class_addprop(void)
{
    hid_t fid;          /* File ID */
    hid_t did;          /* Dataset ID */
    hid_t sid;          /* Dataspace ID */
    hid_t cid;          /* Property Class ID */
    hid_t pid;          /* Property List ID */
    int   prop1_value;  /* Value for property #1 */
    herr_t ret;		/* Generic return value	*/

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create scalar dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a new class, derived from the dataset creation property list class */
    cid = H5Pcreate_class(H5P_DATASET_CREATE, CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid, "H5Pcreate_class");

    /* Check existence of an original property */
    ret = H5Pexist(cid, H5O_CRT_PIPELINE_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check existence of an original property */
    ret = H5Pexist(cid, H5O_CRT_PIPELINE_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check existence of added property */
    ret = H5Pexist(cid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Create a derived dataset creation property list */
    pid = H5Pcreate(cid);
    CHECK(pid, FAIL, "H5Pcreate");

    /* Check existence of an original property */
    ret = H5Pexist(pid, H5O_CRT_PIPELINE_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check existence of added property */
    ret = H5Pexist(pid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of property (set with default value) */
    ret = H5Pget(pid, PROP1_NAME, &prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Check existence of an original property (in class) */
    ret = H5Pexist(cid, H5O_CRT_PIPELINE_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check existence of first added property (in class) */
    ret = H5Pexist(cid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check existence of second added property (in class) */
    ret = H5Pexist(cid, PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check existence of an original property (in property list) */
    ret = H5Pexist(pid, H5O_CRT_PIPELINE_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check existence of first added property (in property list) */
    ret = H5Pexist(pid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check existence of second added property (in property list) (should not exist) */
    ret = H5Pexist(pid, PROP2_NAME);
    VERIFY(ret, 0, "H5Pexist");

    /* Create a dataset */
    did = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check existence of added property (after using property list) */
    ret = H5Pexist(pid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of property (set with default value) (after using property list) */
    ret = H5Pget(pid, PROP1_NAME, &prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Close property class */
    ret = H5Pclose_class(cid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close property list */
    ret = H5Pclose(pid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_genprop_class_addprop() */

/****************************************************************
**
**  test_genprop_equal(): Test basic generic property list code.
**      More tests for H5Pequal()
**
****************************************************************/
static void
test_genprop_equal(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property list ID */
    hid_t		lid2;		/* Generic Property list ID */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Generic Property List Equal Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Create a property list from the class */
    lid1 = H5Pcreate(cid1);
    CHECK_I(lid1, "H5Pcreate");

    /* Copy the property list */
    lid2 = H5Pcopy(lid1);
    CHECK_I(lid2, "H5Pcopy");

    /* Check that the lists are equal */
    ret = H5Pequal(lid1,lid2);
    VERIFY(ret, 1, "H5Pequal");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* ent test_genprop_equal() */

/****************************************************************
**
**  test_genprop_path(): Test basic generic property list code.
**      Tests for class paths
**
****************************************************************/
static void
test_genprop_path(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		cid2;		/* Generic Property class ID */
    hid_t		cid3;		/* Generic Property class ID */
    char               *path;           /* Class path */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Generic Property List Class Path Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Get full path for first class */
    path=H5P_get_class_path_test(cid1);
    CHECK_PTR(path, "H5P_get_class_path_test");
    if(HDstrcmp(path,CLASS1_PATH)!=0)
        TestErrPrintf("Class names don't match!, path=%s, CLASS1_PATH=%s\n",path,CLASS1_PATH);
    HDfree(path);

    /* Create another new generic class, derived from first class */
    cid2 = H5Pcreate_class(cid1,CLASS2_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid2, "H5Pcreate_class");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister2(cid2, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Get full path for second class */
    path=H5P_get_class_path_test(cid2);
    CHECK_PTR(path, "H5P_get_class_path_test");
    if(HDstrcmp(path,CLASS2_PATH)!=0)
        TestErrPrintf("Class names don't match!, path=%s, CLASS2_PATH=%s\n",path,CLASS2_PATH);

    /* Open a copy of the class with the path name */
    cid3 = H5P_open_class_path_test(path);
    CHECK_I(cid3, "H5Popen_class_path");

    /* Check that the classes are equal */
    ret = H5Pequal(cid2,cid3);
    VERIFY(ret, 1, "H5Pequal");

    /* Release the path string */
    free(path);

    /* Close class */
    ret = H5Pclose_class(cid3);
    CHECK_I(ret, "H5Pclose_class");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

    /* Close class */
    ret = H5Pclose_class(cid2);
    CHECK_I(ret, "H5Pclose_class");

} /* ent test_genprop_path() */

/****************************************************************
**
**  test_genprop_refcount(): Test basic generic property list code.
**      Tests for correct reference counting
**
****************************************************************/
static void
test_genprop_refcount(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    hid_t		lid1;		/* Generic Property class ID */
    char               *name;           /* Name of class */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Generic Property List Reference Count Functionality\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT,CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister2(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister2");

    /* Create a new generic list, derived from the root of the class hierarchy */
    lid1 = H5Pcreate(cid1);
    CHECK_I(lid1, "H5Pcreate");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS1_NAME)!=0)
        TestErrPrintf("Class names don't match!, name=%s, CLASS1_NAME=%s\n",name,CLASS1_NAME);
    HDfree(name);

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

    /* Get the list's class */
    cid1 = H5Pget_class(lid1);
    CHECK_I(cid1, "H5Pget_class");

    /* Check correct "is a" class/list relationship */
    ret = H5Pisa_class(lid1,cid1);
    VERIFY(ret, 1, "H5Pisa_class");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS1_NAME)!=0)
        TestErrPrintf("Class names don't match!, name=%s, CLASS1_NAME=%s\n",name,CLASS1_NAME);
    HDfree(name);

    /* Close list */
    ret = H5Pclose(lid1);
    CHECK_I(ret, "H5Pclose");

    /* Check class name */
    name = H5Pget_class_name(cid1);
    CHECK_PTR(name, "H5Pget_class_name");
    if(HDstrcmp(name,CLASS1_NAME)!=0)
        TestErrPrintf("Class names don't match!, name=%s, CLASS1_NAME=%s\n",name,CLASS1_NAME);
    HDfree(name);

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");

} /* ent test_genprop_refcount() */

#ifndef H5_NO_DEPRECATED_SYMBOLS
/****************************************************************
**
**  test_genprop_deprec_class(): Test basic generic property list code.
**      Tests deprecated property class API routines.
**
****************************************************************/
static void
test_genprop_deprec_class(void)
{
    hid_t		cid1;		/* Generic Property class ID */
    size_t		size;		/* Size of property */
    size_t		nprops;		/* Number of properties in class */
    herr_t		ret;		/* Generic return value	*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deprecated Generic Property List Functions\n"));

    /* Create a new generic class, derived from the root of the class hierarchy */
    cid1 = H5Pcreate_class(H5P_ROOT, CLASS1_NAME, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(cid1, "H5Pcreate_class");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 0, "H5Pget_nprops");

    /* Check the existance of the first property (should fail) */
    ret = H5Pexist(cid1, PROP1_NAME);
    VERIFY(ret, 0, "H5Pexist");

    /* Insert first property into class (with no callbacks) */
    ret = H5Pregister1(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister1");

    /* Try to insert the first property again (should fail) */
    ret = H5Pregister1(cid1, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    VERIFY(ret, FAIL, "H5Pregister1");

    /* Check the existance of the first property */
    ret = H5Pexist(cid1, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the first property */
    ret = H5Pget_size(cid1, PROP1_NAME, &size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP1_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Insert second property into class (with no callbacks) */
    ret = H5Pregister1(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister1");

    /* Try to insert the second property again (should fail) */
    ret = H5Pregister1(cid1, PROP2_NAME, PROP2_SIZE, PROP2_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    VERIFY(ret, FAIL, "H5Pregister1");

    /* Check the existance of the second property */
    ret = H5Pexist(cid1, PROP2_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the second property */
    ret = H5Pget_size(cid1, PROP2_NAME, &size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP2_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Insert third property into class (with no callbacks) */
    ret = H5Pregister1(cid1, PROP3_NAME, PROP3_SIZE, PROP3_DEF_VALUE, NULL, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pregister1");

    /* Check the existance of the third property */
    ret = H5Pexist(cid1, PROP3_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check the size of the third property */
    ret = H5Pget_size(cid1, PROP3_NAME, &size);
    CHECK_I(ret, "H5Pget_size");
    VERIFY(size, PROP3_SIZE, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 3, "H5Pget_nprops");

    /* Unregister first property */
    ret = H5Punregister(cid1, PROP1_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Try to check the size of the first property (should fail) */
    ret = H5Pget_size(cid1, PROP1_NAME, &size);
    VERIFY(ret, FAIL, "H5Pget_size");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 2, "H5Pget_nprops");

    /* Unregister second property */
    ret = H5Punregister(cid1, PROP2_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 1, "H5Pget_nprops");

    /* Unregister third property */
    ret = H5Punregister(cid1, PROP3_NAME);
    CHECK_I(ret, "H5Punregister");

    /* Check the number of properties in class */
    ret = H5Pget_nprops(cid1, &nprops);
    CHECK_I(ret, "H5Pget_nprops");
    VERIFY(nprops, 0, "H5Pget_nprops");

    /* Close class */
    ret = H5Pclose_class(cid1);
    CHECK_I(ret, "H5Pclose_class");
} /* end test_genprop_deprec_class() */

/****************************************************************
**
**  test_genprop_deprec2(): Test basic generic property list code.
**      Tests deprecated property list API routines.
**
****************************************************************/
static void
test_genprop_deprec_list(void)
{
    hid_t fid;          /* File ID */
    hid_t did;          /* Dataset ID */
    hid_t sid;          /* Dataspace ID */
    hid_t pid;          /* Property List ID */
    int   prop1_value;  /* Value for property #1 */
    herr_t ret;		/* Generic return value	*/

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create scalar dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset creation property list */
    pid = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(pid, FAIL, "H5Pcreate");

    /* Insert temporary property into class (with no callbacks) */
    ret = H5Pinsert1(pid, PROP1_NAME, PROP1_SIZE, PROP1_DEF_VALUE, NULL, NULL, NULL, NULL, NULL);
    CHECK_I(ret, "H5Pinsert1");

    /* Check existence of added property */
    ret = H5Pexist(pid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of property (set with default value) */
    ret = H5Pget(pid, PROP1_NAME,&prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Create a dataset */
    did = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, pid, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Check existence of added property (after using property list) */
    ret = H5Pexist(pid, PROP1_NAME);
    VERIFY(ret, 1, "H5Pexist");

    /* Check values of property (set with default value) (after using property list) */
    ret = H5Pget(pid, PROP1_NAME, &prop1_value);
    CHECK_I(ret, "H5Pget");
    VERIFY(prop1_value, *PROP1_DEF_VALUE, "H5Pget");

    /* Close property list */
    ret = H5Pclose(pid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_genprop_deprec_list() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/****************************************************************
**
**  test_genprop(): Main generic property testing routine.
**
****************************************************************/
void
test_genprop(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Generic Properties\n"));

    /* These tests use the same file... */
    test_genprop_basic_class(); /* Test basic code for creating a generic class */
    test_genprop_basic_class_prop(); /* Test basic code for adding properties to a generic class */
    test_genprop_class_iter();  /* Test code for iterating over properties in a generic class */
    test_genprop_class_callback();  /* Test code for property class callbacks */

    test_genprop_basic_list();  /* Test basic code for creating a generic property list */
    test_genprop_basic_list_prop(); /* Test basic code for adding properties to a generic property list */
    test_genprop_list_iter();  /* Test basic code for iterating over properties in a generic property list */
    test_genprop_list_callback();  /* Test code for property list callbacks */

    test_genprop_list_addprop();    /* Test adding properties to HDF5 property list */
    test_genprop_class_addprop();   /* Test adding properties to HDF5 property class */

    test_genprop_equal();       /* Tests for more H5Pequal verification */
    test_genprop_path();        /* Tests for class path verification */
    test_genprop_refcount();    /* Tests for class reference counting */

#ifndef H5_NO_DEPRECATED_SYMBOLS
    test_genprop_deprec_class();        /* Tests for deprecated routines */
    test_genprop_deprec_list();         /* Tests for deprecated routines */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

}   /* test_genprop() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_genprop
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              June 8, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_genprop(void)
{
    remove(FILENAME);
}

