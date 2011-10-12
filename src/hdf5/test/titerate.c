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
* Test program:	 titerate
*
* Test the Group & Attribute functionality
*
*************************************************************/

#include "testhdf5.h"

#include "hdf5.h"

#define DATAFILE   "titerate.h5"

/* Number of datasets for group iteration test */
#define NDATASETS 50

/* Number of attributes for attribute iteration test */
#define NATTR 50

/* Number of groups for second group iteration test */
#define ITER_NGROUPS 150

/* General maximum length of names used */
#define NAMELEN     80

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	4

typedef enum {
    RET_ZERO,
    RET_TWO,
    RET_CHANGE,
    RET_CHANGE2
} iter_enum;

/* Custom group iteration callback data */
typedef struct {
    char name[NAMELEN];     /* The name of the object */
    H5O_type_t type;        /* The type of the object */
    iter_enum command;      /* The type of return value */
} iter_info;

/* Local functions */
int iter_strcmp(const void *s1, const void *s2);
int iter_strcmp2(const void *s1, const void *s2);
static herr_t liter_cb(hid_t group, const char *name, const H5L_info_t *info,
    void *op_data);
static herr_t liter_cb2(hid_t group, const char *name, const H5L_info_t *info,
    void *op_data);
herr_t aiter_cb(hid_t group, const char *name, const H5A_info_t *ainfo,
    void *op_data);

/****************************************************************
**
**  iter_strcmp(): String comparison routine for qsort
**
****************************************************************/
int iter_strcmp(const void *s1, const void *s2)
{
    return(HDstrcmp(*(const char * const *)s1,*(const char * const *)s2));
}

/****************************************************************
**
**  liter_cb(): Custom link iteration callback routine.
**
****************************************************************/
static herr_t
liter_cb(hid_t UNUSED group, const char *name, const H5L_info_t UNUSED *link_info,
    void *op_data)
{
    iter_info *info = (iter_info *)op_data;
    static int count = 0;
    static int count2 = 0;

    HDstrcpy(info->name, name);

    switch(info->command) {
        case RET_ZERO:
            return(0);

        case RET_TWO:
            return(2);

        case RET_CHANGE:
            count++;
            return(count > 10 ? 1 : 0);

        case RET_CHANGE2:
            count2++;
            return(count2 > 10 ? 1 : 0);

        default:
            printf("invalid iteration command");
            return(-1);
    } /* end switch */
} /* end liter_cb() */

/****************************************************************
**
**  test_iter_group(): Test group iteration functionality
**
****************************************************************/
static void
test_iter_group(hid_t fapl, hbool_t new_format)
{
    hid_t file;             /* File ID */
    hid_t dataset;          /* Dataset ID */
    hid_t datatype;         /* Common datatype ID */
    hid_t filespace;        /* Common dataspace ID */
    hid_t root_group,grp;   /* Root group ID */
    int i;                  /* counting variable */
    hsize_t idx;            /* Index in the group */
    char name[NAMELEN];     /* temporary name buffer */
    char *lnames[NDATASETS + 2];/* Names of the links created */
    char dataset_name[NAMELEN];  /* dataset name */
    iter_info info;         /* Custom iteration information */
    H5G_info_t ginfo;       /* Buffer for querying object's info */
    herr_t ret;		    /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Group Iteration Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    /* Test iterating over empty group */
    info.command = RET_ZERO;
    idx = 0;
    ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info);
    VERIFY(ret, SUCCEED, "H5Literate");

    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, FAIL, "H5Tcopy");

    filespace=H5Screate(H5S_SCALAR);
    CHECK(filespace, FAIL, "H5Screate");

    for(i=0; i< NDATASETS; i++) {
        sprintf(name,"Dataset %d",i);
        dataset = H5Dcreate2(file, name, datatype, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate2");

        /* Keep a copy of the dataset names around for later */
        lnames[i] = HDstrdup(name);
        CHECK(lnames[i], NULL, "strdup");

        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
    } /* end for */

    /* Create a group and named datatype under root group for testing */
    grp = H5Gcreate2(file, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Gcreate2");

    lnames[NDATASETS] = HDstrdup("grp");
    CHECK(lnames[NDATASETS], NULL, "strdup");

    ret = H5Tcommit2(file, "dtype", datatype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    lnames[NDATASETS + 1] = HDstrdup("dtype");
    CHECK(lnames[NDATASETS], NULL, "strdup");

    /* Close everything up */
    ret = H5Tclose(datatype);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Sclose(filespace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Sort the dataset names */
    HDqsort(lnames, (size_t)(NDATASETS + 2), sizeof(char *), iter_strcmp);


    /* Iterate through the datasets in the root group in various ways */
    file = H5Fopen(DATAFILE, H5F_ACC_RDONLY, fapl);
    CHECK(file, FAIL, "H5Fopen");

    /* These two functions, H5Oget_info_by_idx and H5Lget_name_by_idx, actually
     * iterate through B-tree for group members in internal library design.
     */
    root_group = H5Gopen2(file, "/", H5P_DEFAULT);
    CHECK(root_group, FAIL, "H5Gopen2");

    ret = H5Gget_info(root_group, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, (NDATASETS + 2), "H5Gget_info");

    for(i = 0; i< (int)ginfo.nlinks; i++) {
        H5O_info_t oinfo;               /* Object info */

        ret = (herr_t)H5Lget_name_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, dataset_name, (size_t)NAMELEN, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Lget_name_by_idx");

        ret = H5Oget_info_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, &oinfo, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Oget_info_by_idx");
    } /* end for */

    H5E_BEGIN_TRY {
        ret = (herr_t)H5Lget_name_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)(NDATASETS+3), dataset_name, (size_t)NAMELEN, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Lget_name_by_idx");

    ret = H5Gclose(root_group);
    CHECK(ret, FAIL, "H5Gclose");

    /* These two functions, H5Oget_info_by_idx and H5Lget_name_by_idx, actually
     * iterate through B-tree for group members in internal library design.
     *  (Same as test above, but with the file ID instead of opening the root group)
     */
    ret = H5Gget_info(file, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, NDATASETS + 2, "H5Gget_info");

    for(i = 0; i< (int)ginfo.nlinks; i++) {
        H5O_info_t oinfo;               /* Object info */

        ret = (herr_t)H5Lget_name_by_idx(file, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, dataset_name, (size_t)NAMELEN, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Lget_name_by_idx");

        ret = H5Oget_info_by_idx(file, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, &oinfo, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Oget_info_by_idx");
    } /* end for */

    H5E_BEGIN_TRY {
        ret = (herr_t)H5Lget_name_by_idx(file, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)(NDATASETS + 3), dataset_name, (size_t)NAMELEN, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Lget_name_by_idx");

    /* Test invalid indices for starting iteration */
    info.command = RET_ZERO;
    idx = (hsize_t)-1;
    H5E_BEGIN_TRY {
        ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Literate");

    /* Test skipping exactly as many entries as in the group */
    idx = NDATASETS + 2;
    H5E_BEGIN_TRY {
        ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Literate");

    /* Test skipping more entries than are in the group */
    idx = NDATASETS + 3;
    H5E_BEGIN_TRY {
        ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Literate");

    /* Test all objects in group, when callback always returns 0 */
    info.command = RET_ZERO;
    idx = 0;
    if((ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info)) > 0)
        TestErrPrintf("Group iteration function didn't return zero correctly!\n");

    /* Test all objects in group, when callback always returns 1 */
    /* This also tests the "restarting" ability, because the index changes */
    info.command = RET_TWO;
    idx = i = 0;
    while((ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info)) > 0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret, 2, "H5Literate");

        /* Increment the number of times "2" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx, (hsize_t)i, "H5Literate");
        if(idx > (NDATASETS + 2))
            TestErrPrintf("Group iteration function walked too far!\n");

        /* Verify that the correct name is retrieved */
        if(HDstrcmp(info.name, lnames[(size_t)(idx - 1)]) != 0)
            TestErrPrintf("Group iteration function didn't return name correctly for link - lnames[%u] = '%s'!\n", (unsigned)(idx - 1), lnames[(size_t)(idx - 1)]);
    } /* end while */
    VERIFY(ret, -1, "H5Literate");

    if(i != (NDATASETS + 2))
        TestErrPrintf("%u: Group iteration function didn't perform multiple iterations correctly!\n", __LINE__);

    /* Test all objects in group, when callback changes return value */
    /* This also tests the "restarting" ability, because the index changes */
    info.command = new_format ? RET_CHANGE2 : RET_CHANGE;
    idx = i = 0;
    while((ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info)) >= 0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret, 1, "H5Literate");

        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx, (hsize_t)(i + 10), "H5Literate");
        if(idx > (NDATASETS + 2))
            TestErrPrintf("Group iteration function walked too far!\n");

        /* Verify that the correct name is retrieved */
        if(HDstrcmp(info.name, lnames[(size_t)(idx - 1)]) != 0)
            TestErrPrintf("Group iteration function didn't return name correctly for link - lnames[%u] = '%s'!\n", (unsigned)(idx - 1), lnames[(size_t)(idx - 1)]);
    } /* end while */
    VERIFY(ret, -1, "H5Literate");

    if(i != 42 || idx != 52)
        TestErrPrintf("%u: Group iteration function didn't perform multiple iterations correctly!\n", __LINE__);

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free the dataset names */
    for(i = 0; i< (NDATASETS + 2); i++)
        HDfree(lnames[i]);
} /* test_iter_group() */

/****************************************************************
**
**  aiter_cb(): Custom group iteration callback routine.
**
****************************************************************/
herr_t
aiter_cb(hid_t UNUSED group, const char *name, const H5A_info_t UNUSED *ainfo,
    void *op_data)
{
    iter_info *info = (iter_info *)op_data;
    static int count = 0;
    static int count2 = 0;

    HDstrcpy(info->name, name);

    switch(info->command) {
        case RET_ZERO:
            return(0);

        case RET_TWO:
            return(2);

        case RET_CHANGE:
            count++;
            return(count > 10 ? 1 : 0);

        case RET_CHANGE2:
            count2++;
            return(count2 > 10 ? 1 : 0);

        default:
            printf("invalid iteration command");
            return(-1);
    } /* end switch */
} /* end aiter_cb() */

/****************************************************************
**
**  test_iter_attr(): Test attribute iteration functionality
**
****************************************************************/
static void test_iter_attr(hid_t fapl, hbool_t new_format)
{
    hid_t file;             /* File ID */
    hid_t dataset;          /* Common Dataset ID */
    hid_t filespace;        /* Common dataspace ID */
    hid_t attribute;        /* Attribute ID */
    int i;                  /* counting variable */
    hsize_t idx;            /* Index in the attribute list */
    char name[NAMELEN];     /* temporary name buffer */
    char *anames[NATTR];    /* Names of the attributes created */
    iter_info info;         /* Custom iteration information */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attribute Iteration Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    filespace = H5Screate(H5S_SCALAR);
    CHECK(filespace, FAIL, "H5Screate");

    dataset = H5Dcreate2(file, "Dataset", H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    for(i = 0; i < NATTR; i++) {
        sprintf(name, "Attribute %02d", i);
        attribute = H5Acreate2(dataset, name, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attribute, FAIL, "H5Acreate2");

        /* Keep a copy of the attribute names around for later */
        anames[i] = HDstrdup(name);
        CHECK(anames[i], NULL, "strdup");

        ret = H5Aclose(attribute);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Close everything up */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(filespace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");


    /* Iterate through the attributes on the dataset in various ways */
    file = H5Fopen(DATAFILE, H5F_ACC_RDONLY, fapl);
    CHECK(file, FAIL, "H5Fopen");

    dataset = H5Dopen2(file, "Dataset", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Test invalid indices for starting iteration */
    info.command = RET_ZERO;

    /* Test skipping exactly as many attributes as there are */
    idx = NATTR;
    H5E_BEGIN_TRY {
        ret = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, &idx, aiter_cb, &info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Aiterate2");

    /* Test skipping more attributes than there are */
    idx = NATTR + 1;
    H5E_BEGIN_TRY {
        ret = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, &idx, aiter_cb, &info);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Aiterate2");

    /* Test all attributes on dataset, when callback always returns 0 */
    info.command = RET_ZERO;
    idx = 0;
    if((ret = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, &idx, aiter_cb, &info)) > 0)
        TestErrPrintf("Attribute iteration function didn't return zero correctly!\n");

    /* Test all attributes on dataset, when callback always returns 1 */
    /* This also tests the "restarting" ability, because the index changes */
    info.command = RET_TWO;
    idx = i = 0;
    while((ret = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, &idx, aiter_cb, &info)) > 0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret, 2, "H5Aiterate2");

        /* Increment the number of times "2" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx, (unsigned)i, "H5Aiterate2");

        /* Don't check name when new format is used */
        if(!new_format) {
            /* Verify that the correct name is retrieved */
            if(HDstrcmp(info.name, anames[(size_t)idx - 1]) != 0)
                TestErrPrintf("%u: Attribute iteration function didn't set names correctly, info.name = '%s', anames[%u] = '%s'!\n", __LINE__, info.name, (unsigned)(idx - 1), anames[(size_t)idx - 1]);
        } /* end if */
    } /* end while */
    VERIFY(ret, -1, "H5Aiterate2");
    if(i != 50 || idx != 50)
        TestErrPrintf("%u: Attribute iteration function didn't perform multiple iterations correctly!\n", __LINE__);


    /* Test all attributes on dataset, when callback changes return value */
    /* This also tests the "restarting" ability, because the index changes */
    info.command = new_format ? RET_CHANGE2 : RET_CHANGE;
    idx = i = 0;
    while((ret = H5Aiterate2(dataset, H5_INDEX_NAME, H5_ITER_INC, &idx, aiter_cb, &info)) > 0) {
        /* Verify return value from iterator gets propagated correctly */
        VERIFY(ret, 1, "H5Aiterate2");

        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        VERIFY(idx, (unsigned)i + 10, "H5Aiterate2");

        /* Don't check name when new format is used */
        if(!new_format) {
            /* Verify that the correct name is retrieved */
            if(HDstrcmp(info.name, anames[(size_t)idx - 1]) != 0)
                TestErrPrintf("%u: Attribute iteration function didn't set names correctly, info.name = '%s', anames[%u] = '%s'!\n", __LINE__, info.name, (unsigned)(idx - 1), anames[(size_t)idx - 1]);
        } /* end if */
    } /* end while */
    VERIFY(ret, -1, "H5Aiterate2");
    if(i != 40 || idx != 50)
        TestErrPrintf("%u: Attribute iteration function didn't perform multiple iterations correctly!\n", __LINE__);

    ret=H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    ret=H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Free the attribute names */
    for(i=0; i< NATTR; i++)
        HDfree(anames[i]);

} /* test_iter_attr() */

/****************************************************************
**
**  iter_strcmp2(): String comparison routine for qsort
**
****************************************************************/
int iter_strcmp2(const void *s1, const void *s2)
{
    return(HDstrcmp((const char *)s1, (const char *)s2));
} /* end iter_strcmp2() */

/****************************************************************
**
**  liter_cb2(): Custom link iteration callback routine.
**
****************************************************************/
static herr_t
liter_cb2(hid_t loc_id, const char *name, const H5L_info_t UNUSED *link_info,
    void *opdata)
{
    const iter_info *test_info = (const iter_info *)opdata;
    H5O_info_t oinfo;
    herr_t ret;		/* Generic return value		*/

    if(HDstrcmp(name, test_info->name)) {
        TestErrPrintf("name = '%s', test_info = '%s'\n", name, test_info->name);
        return(H5_ITER_ERROR);
    } /* end if */

    /*
     * Get type of the object and check it.
     */
    ret = H5Oget_info_by_name(loc_id, name, &oinfo, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_name");

    if(test_info->type != oinfo.type) {
        TestErrPrintf("test_info->type = %d, oinfo.type = %d\n", test_info->type, (int)oinfo.type);
        return(H5_ITER_ERROR);
    } /* end if */

    return(H5_ITER_STOP);
} /* liter_cb2() */

/****************************************************************
**
**  test_iter_group_large(): Test group iteration functionality
**          for groups with large #'s of objects
**
****************************************************************/
static void
test_iter_group_large(hid_t fapl)
{
    hid_t		file;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;      /* Group ID             */
    hid_t		sid;       /* Dataspace ID			*/
    hid_t		tid;       /* Datatype ID			*/
    hsize_t		dims[] = {SPACE1_DIM1};
    herr_t		ret;		/* Generic return value		*/
    char gname[20];         /* Temporary group name */
    iter_info names[ITER_NGROUPS+2]; /* Names of objects in the root group */
    iter_info *curr_name;        /* Pointer to the current name in the root group */
    int                 i;

    /* Compound datatype */
    typedef struct s1_t {
        unsigned int a;
        unsigned int b;
        float c;
    } s1_t;

    HDmemset(names, 0, sizeof names);

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Large Group Iteration Functionality\n"));

    /* Create file */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create a bunch of groups */
    for(i = 0; i < ITER_NGROUPS; i++) {
        sprintf(gname, "Group_%d", i);

        /* Add the name to the list of objects in the root group */
        HDstrcpy(names[i].name, gname);
        names[i].type = H5O_TYPE_GROUP;

        /* Create a group */
        group = H5Gcreate2(file, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(group, FAIL, "H5Gcreate2");

        /* Close a group */
        ret = H5Gclose(group);
        CHECK(ret, FAIL, "H5Gclose");
    } /* end for */

    /* Create a dataset  */
    dataset = H5Dcreate2(file, "Dataset1", H5T_STD_U32LE, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Add the name to the list of objects in the root group */
    HDstrcpy(names[ITER_NGROUPS].name, "Dataset1");
    names[ITER_NGROUPS].type = H5O_TYPE_DATASET;

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create a datatype */
    tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid, FAIL, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(file, "Datatype1", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Add the name to the list of objects in the root group */
    HDstrcpy(names[ITER_NGROUPS + 1].name, "Datatype1");
    names[ITER_NGROUPS + 1].type = H5O_TYPE_NAMED_DATATYPE;

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Need to sort the names in the root group, cause that's what the library does */
    HDqsort(names, (size_t)(ITER_NGROUPS + 2), sizeof(iter_info), iter_strcmp2);

    /* Iterate through the file to see members of the root group */
    curr_name = &names[0];
    ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, NULL, liter_cb2, curr_name);
    CHECK(ret, FAIL, "H5Literate");
    for(i = 1; i < 100; i++) {
        hsize_t idx = i;

        curr_name = &names[i];
        ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb2, curr_name);
        CHECK(ret, FAIL, "H5Literate");
    } /* end for */

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_iterate_group_large() */

/****************************************************************
**
**  test_grp_memb_funcs(): Test group member information
**                         functionality
**
****************************************************************/
static void test_grp_memb_funcs(hid_t fapl)
{
    hid_t file;             /* File ID */
    hid_t dataset;          /* Dataset ID */
    hid_t datatype;         /* Common datatype ID */
    hid_t filespace;        /* Common dataspace ID */
    hid_t root_group,grp;   /* Root group ID */
    int i;                  /* counting variable */
    char name[NAMELEN];     /* temporary name buffer */
    char *dnames[NDATASETS+2];/* Names of the datasets created */
    char *obj_names[NDATASETS+2];/* Names of the objects in group */
    char dataset_name[NAMELEN];  /* dataset name */
    ssize_t name_len;       /* Length of object's name */
    H5G_info_t ginfo;       /* Buffer for querying object's info */
    herr_t ret;		    /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Group Member Information Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    datatype = H5Tcopy(H5T_NATIVE_INT);
    CHECK(datatype, FAIL, "H5Tcopy");

    filespace = H5Screate(H5S_SCALAR);
    CHECK(filespace, FAIL, "H5Screate");

    for(i = 0; i < NDATASETS; i++) {
        sprintf(name, "Dataset %d", i);
        dataset = H5Dcreate2(file, name, datatype, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(dataset, FAIL, "H5Dcreate2");

        /* Keep a copy of the dataset names around for later */
        dnames[i] = HDstrdup(name);
        CHECK(dnames[i], NULL, "strdup");

        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
    } /* end for */

    /* Create a group and named datatype under root group for testing */
    grp = H5Gcreate2(file, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Gcreate2");

    dnames[NDATASETS] = HDstrdup("grp");
    CHECK(dnames[NDATASETS], NULL, "strdup");

    ret = H5Tcommit2(file, "dtype", datatype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    dnames[NDATASETS + 1] = HDstrdup("dtype");
    CHECK(dnames[NDATASETS], NULL, "strdup");

    /* Close everything up */
    ret = H5Tclose(datatype);
    CHECK(ret, FAIL, "H5Tclose");

    ret = H5Gclose(grp);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Sclose(filespace);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Sort the dataset names */
    HDqsort(dnames, (size_t)(NDATASETS + 2), sizeof(char *), iter_strcmp);

    /* Iterate through the datasets in the root group in various ways */
    file = H5Fopen(DATAFILE, H5F_ACC_RDONLY, fapl);
    CHECK(file, FAIL, "H5Fopen");

    /* These two functions, H5Oget_info_by_idx and H5Lget_name_by_idx, actually
     * iterate through B-tree for group members in internal library design.
     */
    root_group = H5Gopen2(file, "/", H5P_DEFAULT);
    CHECK(root_group, FAIL, "H5Gopen2");

    ret = H5Gget_info(root_group, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, (NDATASETS + 2), "H5Gget_info");

    for(i = 0; i < (int)ginfo.nlinks; i++) {
        H5O_info_t oinfo;               /* Object info */

        /* Test with NULL for name, to query length */
        name_len = H5Lget_name_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, NULL, (size_t)NAMELEN, H5P_DEFAULT);
        CHECK(name_len, FAIL, "H5Lget_name_by_idx");

        ret = (herr_t)H5Lget_name_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, dataset_name, (size_t)(name_len + 1), H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Lget_name_by_idx");

        /* Double-check that the length is the same */
        VERIFY(ret, name_len, "H5Lget_name_by_idx");

        /* Keep a copy of the dataset names around for later */
        obj_names[i] = HDstrdup(dataset_name);
        CHECK(obj_names[i], NULL, "strdup");

        ret = H5Oget_info_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, &oinfo, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Oget_info_by_idx");

        if(!HDstrcmp(dataset_name, "grp"))
            VERIFY(oinfo.type, H5O_TYPE_GROUP, "H5Lget_name_by_idx");
        if(!HDstrcmp(dataset_name, "dtype"))
            VERIFY(oinfo.type, H5O_TYPE_NAMED_DATATYPE, "H5Lget_name_by_idx");
        if(!HDstrncmp(dataset_name, "Dataset", (size_t)7))
            VERIFY(oinfo.type, H5O_TYPE_DATASET, "H5Lget_name_by_idx");
    } /* end for */

    H5E_BEGIN_TRY {
        ret = (herr_t)H5Lget_name_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)(NDATASETS+3), dataset_name, (size_t)NAMELEN, H5P_DEFAULT);
    } H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Lget_name_by_idx");

    /* Sort the dataset names */
    HDqsort(obj_names, (size_t)(NDATASETS + 2), sizeof(char *), iter_strcmp);

    /* Compare object names */
    for(i = 0; i< (int)ginfo.nlinks; i++) {
        ret = HDstrcmp(dnames[i], obj_names[i]);
        VERIFY(ret, 0, "HDstrcmp");
    } /* end for */

    ret = H5Gclose(root_group);
    CHECK(ret, FAIL, "H5Gclose");


    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free the dataset names */
    for(i = 0; i< (NDATASETS + 2); i++) {
        HDfree(dnames[i]);
        HDfree(obj_names[i]);
    } /* end for */
} /* test_grp_memb_funcs() */

/****************************************************************
**
**  test_links(): Test soft and hard link iteration
**
****************************************************************/
static void test_links(hid_t fapl)
{
    hid_t file;             /* File ID */
    char obj_name[NAMELEN]; /* Names of the object in group */
    ssize_t name_len;       /* Length of object's name */
    hid_t    gid, gid1;
    H5G_info_t ginfo;       /* Buffer for querying object's info */
    hsize_t i;
    herr_t ret;		    /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Soft and Hard Link Iteration Functionality\n"));

    /* Create the test file with the datasets */
    file = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    /* create groups */
    gid = H5Gcreate2(file, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, FAIL, "H5Gcreate2");

    gid1 = H5Gcreate2(file, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid1, FAIL, "H5Gcreate2");

    /* create soft and hard links to the group "/g1". */
    ret = H5Lcreate_soft("something", gid, "softlink", H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lcreate_soft");

    ret = H5Lcreate_hard(gid, "/g1", H5L_SAME_LOC, "hardlink", H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Lcreate_hard");

    ret = H5Gget_info(gid, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, 3, "H5Gget_info");

    /* Test these two functions, H5Oget_info_by_idx and H5Lget_name_by_idx */
    for(i = 0; i < ginfo.nlinks; i++) {
        H5O_info_t oinfo;               /* Object info */
        H5L_info_t linfo;               /* Link info */

        /* Get link name */
        name_len = H5Lget_name_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, i, obj_name, (size_t)NAMELEN, H5P_DEFAULT);
        CHECK(name_len, FAIL, "H5Lget_name_by_idx");

        /* Get link type */
        ret = H5Lget_info_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, &linfo, H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Lget_info_by_idx");

        /* Get object type */
        if(linfo.type == H5L_TYPE_HARD) {
            ret = H5Oget_info_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, &oinfo, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Oget_info_by_idx");
        } /* end if */

        if(!HDstrcmp(obj_name, "g1.1"))
            VERIFY(oinfo.type, H5O_TYPE_GROUP, "H5Lget_name_by_idx");
        else if(!HDstrcmp(obj_name, "hardlink"))
            VERIFY(oinfo.type, H5O_TYPE_GROUP, "H5Lget_name_by_idx");
        else if(!HDstrcmp(obj_name, "softlink"))
            VERIFY(linfo.type, H5L_TYPE_SOFT, "H5Lget_name_by_idx");
        else
            CHECK(0, 0, "unknown object name");
    } /* end for */

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Gclose(gid1);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_links() */

/****************************************************************
**
**  test_iterate(): Main iteration testing routine.
**
****************************************************************/
void
test_iterate(void)
{
    hid_t fapl, fapl2;          /* File access property lists */
    hbool_t new_format;         /* Whether to use the new format or not */
    herr_t ret;		        /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Iteration Operations\n"));

    /* Get the default FAPL */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Copy the file access property list */
    fapl2 = H5Pcopy(fapl);
    CHECK(fapl2, FAIL, "H5Pcopy");

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    ret = H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* These next tests use the same file */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        test_iter_group(new_format ? fapl2 : fapl, new_format); /* Test group iteration */
        test_iter_group_large(new_format ? fapl2 : fapl);   /* Test group iteration for large # of objects */
        test_iter_attr(new_format ? fapl2 : fapl, new_format);  /* Test attribute iteration */
        test_grp_memb_funcs(new_format ? fapl2 : fapl);     /* Test group member information functions */
        test_links(new_format ? fapl2 : fapl);              /* Test soft and hard link iteration */
    } /* end for */

    /* Close FAPLs */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl2);
    CHECK(ret, FAIL, "H5Pclose");
}   /* test_iterate() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_iterate
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              April 5, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_iterate(void)
{
    remove(DATAFILE);
}

