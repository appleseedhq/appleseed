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
 * Tests for free-space manager
 */
#include "h5test.h"

#define H5FS_PACKAGE
#define H5FS_TESTING
#include "H5FSpkg.h"		/* Free space manager */


/* Other private headers that this test requires */
#define H5F_PACKAGE
#include "H5Fpkg.h"
#include "H5Iprivate.h"
#include "H5Vprivate.h"

#define FILENAME_LEN	1024

#define TEST_FSPACE_SECT_TYPE			0
#define TEST_FSPACE_SECT_TYPE_NEW		1
#define TEST_FSPACE_SECT_TYPE_NONE		2

#define	TEST_FSPACE_SHRINK			80
#define TEST_FSPACE_EXPAND      		120
#define TEST_MAX_SECT_SIZE 			(64 * 1024)
#define TEST_MAX_INDEX 				32

#define TEST_SECT_ADDR60         60
#define TEST_SECT_ADDR70         70
#define TEST_SECT_ADDR80         80
#define TEST_SECT_ADDR100        100
#define TEST_SECT_ADDR150        150
#define TEST_SECT_ADDR200        200
#define TEST_SECT_ADDR300        300

#define TEST_SECT_SIZE5         5
#define TEST_SECT_SIZE10        10
#define TEST_SECT_SIZE15        15
#define TEST_SECT_SIZE20        20
#define TEST_SECT_SIZE30        30
#define TEST_SECT_SIZE40        40
#define TEST_SECT_SIZE50        50
#define TEST_SECT_SIZE80        80

#define FSPACE_THRHD_DEF   1               /* Default: no alignment threshold */
#define FSPACE_ALIGN_DEF   1               /* Default: no alignment */

const char *FILENAME[] = {
    "frspace",
    NULL
};

typedef struct frspace_state_t {
    hsize_t tot_space;          /* Total amount of space tracked              */
    hsize_t tot_sect_count;     /* Total # of sections tracked                */
    hsize_t serial_sect_count;  /* # of serializable sections tracked         */
    hsize_t ghost_sect_count;   /* # of un-serializable sections tracked      */
} frspace_state_t;

haddr_t		g_eoa=0;
static haddr_t 	TEST_get_eoa(void);
static void 	TEST_set_eoa(haddr_t);

/*
 * TEST client
 */
typedef struct TEST_free_section_t {
    H5FS_section_info_t sect_info;    /* Free space section information (must be first in struct) */
} TEST_free_section_t;


static herr_t TEST_sect_init_cls(H5FS_section_class_t *, void *);
static herr_t TEST_sect_free(H5FS_section_info_t *_sect);
static herr_t TEST_sect_can_merge(const H5FS_section_info_t *, const H5FS_section_info_t *, void UNUSED *);
static herr_t TEST_sect_merging(H5FS_section_info_t *, H5FS_section_info_t *, void UNUSED *);
static herr_t TEST_sect_can_shrink(const H5FS_section_info_t *, void *);
static herr_t TEST_sect_shrinking(H5FS_section_info_t **, void *);

static unsigned test_fs_create(hid_t fapl);
static unsigned test_fs_sect_add(hid_t fapl);
static unsigned test_fs_sect_merge(hid_t fapl);
static unsigned test_fs_sect_shrink(hid_t fapl);
static unsigned test_fs_sect_find(hid_t fapl);
static unsigned test_fs_sect_change_class(hid_t fapl);
static unsigned test_fs_sect_extend(hid_t fapl);
static unsigned test_fs_sect_iterate(hid_t fapl);


H5FS_section_class_t TEST_FSPACE_SECT_CLS[1] = {{
    TEST_FSPACE_SECT_TYPE,     	/* Section type                 */
    0,                          /* Extra serialized size        */
    H5FS_CLS_MERGE_SYM | H5FS_CLS_ADJUST_OK,	/* Class flags  */
    NULL,                       /* Class private info           */

    /* Class methods */
    TEST_sect_init_cls,         /* Initialize section class     */
    NULL,                       /* Terminate section class      */

    /* Object methods */
    NULL,                       /* Add section                  */
    NULL,                       /* Serialize section            */
    NULL,       		/* Deserialize section          */
    TEST_sect_can_merge,   	/* Can sections merge?          */
    TEST_sect_merging,          /* Merge sections               */
    TEST_sect_can_shrink, 	/* Can section shrink container?*/
    TEST_sect_shrinking,        /* Shrink container w/section   */
    TEST_sect_free,             /* Free section                 */
    NULL,             		/* Check validity of section    */
    NULL,             		/* Split section node for alignment */
    NULL,                       /* Dump debugging for section   */
}};

H5FS_section_class_t TEST_FSPACE_SECT_CLS_NEW[1] = {{
    TEST_FSPACE_SECT_TYPE_NEW,  /* Section type                 */
    0,                          /* Extra serialized size        */
    H5FS_CLS_MERGE_SYM | H5FS_CLS_ADJUST_OK,	/* Class flags  */
    NULL,                       /* Class private info           */

    /* Class methods */
    TEST_sect_init_cls,         /* Initialize section class     */
    NULL,                       /* Terminate section class      */

    /* Object methods */
    NULL,                       /* Add section                  */
    NULL,                       /* Serialize section            */
    NULL,       		/* Deserialize section          */
    TEST_sect_can_merge,   	/* Can sections merge?          */
    TEST_sect_merging,          /* Merge sections               */
    NULL,        		/* Can section shrink container?*/
    NULL,            		/* Shrink container w/section   */
    TEST_sect_free,             /* Free section                 */
    NULL,             		/* Check validity of section    */
    NULL,             		/* Split section node for alignment */
    NULL,                       /* Dump debugging for section   */
}};

H5FS_section_class_t TEST_FSPACE_SECT_CLS_NOINIT[1] = {{
    TEST_FSPACE_SECT_TYPE_NONE, /* Section type                 */
    0,                          /* Extra serialized size        */
    H5FS_CLS_MERGE_SYM | H5FS_CLS_ADJUST_OK,	/* Class flags  */
    NULL,                       /* Class private info           */

    /* Class methods */
    NULL,         		/* Initialize section class     */
    NULL,                       /* Terminate section class      */

    /* Object methods */
    NULL,                       /* Add section                  */
    NULL,                       /* Serialize section            */
    NULL,       		/* Deserialize section          */
    TEST_sect_can_merge,   	/* Can sections merge?          */
    TEST_sect_merging,          /* Merge sections               */
    NULL,        		/* Can section shrink container?*/
    NULL,            		/* Shrink container w/section   */
    TEST_sect_free,             /* Free section                 */
    NULL,             		/* Check validity of section    */
    NULL,             		/* Split section node for alignment */
    NULL,                       /* Dump debugging for section   */
}};

const H5FS_section_class_t *test_classes[] = {
    TEST_FSPACE_SECT_CLS,
    TEST_FSPACE_SECT_CLS_NEW,
    TEST_FSPACE_SECT_CLS_NOINIT
};


static void init_cparam(H5FS_create_t *);
static void init_sect_node(TEST_free_section_t *, haddr_t, hsize_t, unsigned, H5FS_section_state_t);
static int check_stats(const H5F_t *, const H5FS_t *, frspace_state_t *);

#define	NUM_SECTIONS	1000

/* User data for free space section iterator callback */
typedef struct {
    hsize_t     tot_size;
    hsize_t	tot_sect_count;
} TEST_iter_ud_t;

static herr_t TEST_sects_cb(const H5FS_section_info_t *_sect, void *_udata);


/*
 * Tests
 */

/*
 * free-space section routines for client TEST
 */
static herr_t
TEST_sect_init_cls(H5FS_section_class_t *cls, void *_udata)
{
    herr_t 	ret_value = SUCCEED;         /* Return value */
    unsigned 	*init_flags;

    /* Check arguments. */
    HDassert(cls);
    HDassert(_udata);

    init_flags = (unsigned *)_udata;
    cls->flags |= *init_flags;

    return(ret_value);
} /* TEST_sect_init_cls() */

/*
 * Check if the two sections can be merged:
 * 	true if second section adjoins the first section
 */
static herr_t
TEST_sect_can_merge(const H5FS_section_info_t *_sect1,
    const H5FS_section_info_t *_sect2, void UNUSED *_udata)
{
    const TEST_free_section_t *sect1 = (const TEST_free_section_t *)_sect1;
    const TEST_free_section_t *sect2 = (const TEST_free_section_t *)_sect2;
    htri_t ret_value;                   /* Return value */

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(sect1->sect_info.type == sect2->sect_info.type);   /* Checks "MERGE_SYM" flag */
    HDassert(H5F_addr_lt(sect1->sect_info.addr, sect2->sect_info.addr));

    /* Check if second section adjoins first section */
    ret_value = H5F_addr_eq(sect1->sect_info.addr + sect1->sect_info.size, sect2->sect_info.addr);

    return(ret_value);
} /* TEST_sect_can_merge() */

/*
 * Merge the two sections (second section is merged into the first section)
 */
static herr_t
TEST_sect_merging(H5FS_section_info_t *_sect1, H5FS_section_info_t *_sect2,
    void UNUSED *_udata)
{
    TEST_free_section_t *sect1 = (TEST_free_section_t *)_sect1;
    TEST_free_section_t *sect2 = (TEST_free_section_t *)_sect2;
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Check arguments. */
    HDassert(sect1);
    HDassert((sect1->sect_info.type == TEST_FSPACE_SECT_TYPE) ||
	     (sect1->sect_info.type == TEST_FSPACE_SECT_TYPE_NEW) ||
	     (sect1->sect_info.type == TEST_FSPACE_SECT_TYPE_NONE));
    HDassert(sect2);
    HDassert((sect2->sect_info.type == TEST_FSPACE_SECT_TYPE) ||
	     (sect2->sect_info.type == TEST_FSPACE_SECT_TYPE_NEW) ||
	     (sect2->sect_info.type == TEST_FSPACE_SECT_TYPE_NONE));
    HDassert(H5F_addr_eq(sect1->sect_info.addr + sect1->sect_info.size, sect2->sect_info.addr));

    /* Add second section's size to first section */
    sect1->sect_info.size += sect2->sect_info.size;

    /* Get rid of second section */
    if(TEST_sect_free((H5FS_section_info_t *)sect2) < 0)
	TEST_ERROR
error:
    return(ret_value);
} /* TEST_sect_merging() */

/*
 * Free the section
 */
static herr_t
TEST_sect_free(H5FS_section_info_t *sect)
{
    /* Release the section */
    HDfree(sect);

    return(0);
} /* TEST_sect_free() */

/*
 * Determine if the section can be shrunk and set _udata accordingly
 *	if _udata passed in is NULL, return FALSE
 *	Otherwise:
 *		if section's address+size is the end of file, return TRUE
 *		otherwise return FALSE
 */
static herr_t
TEST_sect_can_shrink(const H5FS_section_info_t *_sect, void *_udata)
{
    unsigned 	*can_shrink = (unsigned *)_udata;
    const TEST_free_section_t *sect = (const TEST_free_section_t *)_sect;
    haddr_t	end, eoa;

    if (can_shrink == NULL)
	return(FALSE);

    end = sect->sect_info.addr + sect->sect_info.size;
    eoa = TEST_get_eoa();

    if (end == eoa)
	*can_shrink = TRUE;
    else
	*can_shrink = FALSE;

    return((htri_t)*can_shrink);
} /* TEST_sect_can_shrink() */

/*
 * Shrink the section
 */
static herr_t
TEST_sect_shrinking(H5FS_section_info_t **_sect, void *_udata)
{
    TEST_free_section_t **sect = (TEST_free_section_t **)_sect;
    unsigned 	*can_shrink = (unsigned *)_udata;

    /* address of the section is faked, so, doesn't need to do anything */
    /* just free the section node */
    if (*can_shrink) {
	if (TEST_sect_free((H5FS_section_info_t *)*sect) < 0)
	    TEST_ERROR
	*sect = NULL;
	return(TRUE);
    }

error:
    return(FALSE);
}


/*
 * iteration callback
 */
static herr_t
TEST_sects_cb(const H5FS_section_info_t *_sect, void *_udata)
{
    const TEST_free_section_t *sect = (const TEST_free_section_t *)_sect;
    TEST_iter_ud_t *udata = (TEST_iter_ud_t *)_udata;
    herr_t      ret_value = SUCCEED;    /* Return value */

    HDassert(sect);
    HDassert(udata);

    udata->tot_size += sect->sect_info.size;
    udata->tot_sect_count += 1;

    return(ret_value);
}

/* supporting routine for shrinking */
static haddr_t
TEST_get_eoa(void)
{
    return(g_eoa);
}

/* supporting routine for shrinking */
static void
TEST_set_eoa(haddr_t val)
{
   g_eoa = val;
}

/*
 * Initialize creation parameter structure for TEST client
 */
static void
init_cparam(H5FS_create_t *cparam)
{
    HDmemset(cparam, 0, sizeof(H5FS_create_t));

    /* Set the free space creation parameters */
    cparam->shrink_percent = TEST_FSPACE_SHRINK;
    cparam->expand_percent = TEST_FSPACE_EXPAND;
    cparam->max_sect_size = TEST_MAX_SECT_SIZE;
    cparam->max_sect_addr = TEST_MAX_INDEX;

} /* init_cparam() */

/*
 * Initialize free space section node
 */
static void
init_sect_node(TEST_free_section_t *sect_node, haddr_t addr, hsize_t size, unsigned type, H5FS_section_state_t state)
{
    sect_node->sect_info.addr = addr;
    sect_node->sect_info.size = size;
    sect_node->sect_info.type = type;
    sect_node->sect_info.state = state;
} /* init_sect_node() */

/*
 * Verify statistics for the free-space manager
 */
static int
check_stats(const H5F_t *f, const H5FS_t *frsp, frspace_state_t *state)
{
    H5FS_stat_t frspace_stats;             /* Statistics about the heap */

    /* Get statistics for heap and verify they are correct */
    if(H5FS_stat_info(f, frsp, &frspace_stats) < 0)
        FAIL_STACK_ERROR

    if(frspace_stats.tot_space != state->tot_space) {
        HDfprintf(stdout, "frspace_stats.tot_space = %Hu, state->tot_space = %Zu\n",
	    frspace_stats.tot_space, state->tot_space);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.tot_sect_count != state->tot_sect_count) {
        HDfprintf(stdout, "frspace_stats.tot_sect_count = %Hu, state->tot_sect_count = %Hu\n",
	    frspace_stats.tot_sect_count, state->tot_sect_count);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.serial_sect_count != state->serial_sect_count) {
        HDfprintf(stdout, "frspace_stats.serial_sect_count = %Hu, state->serial_sect_count = %Hu\n",
	    frspace_stats.serial_sect_count, state->serial_sect_count);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.ghost_sect_count != state->ghost_sect_count) {
        HDfprintf(stdout, "frspace_stats.ghost_sect_count = %Hu, state->ghost_sect_count = %Hu\n",
	    frspace_stats.ghost_sect_count, state->ghost_sect_count);
        TEST_ERROR
    } /* end if */

    /* All tests passed */
    return(0);

error:
    return(1);
} /* check_stats() */

/*
 * TESTS for free-space manager
 */

/*
 *  To verify the creation, close, reopen and deletion of the free-space manager
 */
static unsigned
test_fs_create(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr; 		/* address of free space */
    h5_stat_size_t      file_size, empty_size;  /* File size */
    frspace_state_t 	state;          	/* State of free space*/
    H5FS_create_t 	cparam, test_cparam; 	/* creation parameters */
    size_t		nclasses;
    unsigned		init_flags=0;

    TESTING("the creation/close/reopen/deletion of the free-space manager");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* initialize creation parameters for free-space manager */
    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR
    if (frsp->nclasses != nclasses)
	TEST_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    HDmemset(&test_cparam, 0, sizeof(H5FS_create_t));
    if(H5FS_get_cparam_test(frsp, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if (H5FS_cmp_cparam_test(&cparam, &test_cparam))
        FAIL_STACK_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
        FAIL_STACK_ERROR
    frsp = NULL;

    /* reopen the free-space manager */
    if(NULL == (frsp = H5FS_open(f, H5P_DATASET_XFER_DEFAULT, fs_addr,
			nclasses, test_classes, NULL, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR
    if (frsp->nclasses != nclasses)
	TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
        FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
        FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if(file_size != empty_size)
        TEST_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_fs_create() */


/*
 * Test 1:
 *	Create free-space manager
 *	Add section A via H5FS_sect_add() with H5FS_ADD_RETURNED_SPACE
 *	Close the free-space manager
 *	Result: section A is serialized to the file
 *
 * Test 2:
 *	Create free-space manager with H5FS_CLS_GHOST_OBJ section class setting
 *	Add section A via H5FS_sect_add() with H5FS_ADD_RETURNED_SPACE
 *	Close the free-space manager
 *	Result: section A is not serialized to the file
 *
 * Test 3:
 *	Add section A via H5FS_sect_add() to allow shrinking with H5FS_ADD_RETURNED_SPACE
 *	Set EOF to be the ending address of section A
 *	Result: H5FS_sect_add() will shrink section A
 *
 * Test 4:
 *	Add section A via H5FS_sect_add() to allow shrinking with H5FS_ADD_DESERIALIZING
 *	Set EOF to be the ending address of section A
 *	Result: H5FS_sect_add() will not shrink section A
 *
 */
static unsigned
test_fs_sect_add(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr=HADDR_UNDEF; 	/* address of free space */
    size_t		nclasses;
    H5FS_create_t 	cparam; 		/* creation parameters */
    frspace_state_t 	state;          	/* State of free space*/

    TEST_free_section_t 	*sect_node = NULL;
    unsigned			init_flags=0;
    h5_stat_size_t 		file_size=0, tmp_file_size=0, fr_meta_size=0;
    unsigned			can_shrink=FALSE;

    TESTING("adding a section via H5FS_sect_add() to free-space: test 1");

    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
            &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
        FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    if(NULL == (sect_node = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
        FAIL_STACK_ERROR

    init_sect_node(sect_node, (haddr_t)TEST_SECT_ADDR80, (hsize_t)TEST_SECT_SIZE20, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node,
            H5FS_ADD_RETURNED_SPACE, NULL) < 0)
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    fr_meta_size = H5FS_HEADER_SIZE(f) + H5FS_SINFO_PREFIX_SIZE(f);

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
        FAIL_STACK_ERROR
    frsp = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((tmp_file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    if (tmp_file_size <= (file_size+fr_meta_size))
        TEST_ERROR

    PASSED()

    TESTING("adding a section via H5FS_sect_add() to free-space with H5FS_CLS_GHOST_OBJ: test 2");

    /* Get the size of a file w/empty heap*/
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    init_flags = H5FS_CLS_GHOST_OBJ;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
            &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
        FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /* Create free list section node */
    if(NULL == (sect_node = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
        FAIL_STACK_ERROR

    init_sect_node(sect_node, (haddr_t)TEST_SECT_ADDR80, (hsize_t)TEST_SECT_SIZE20, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node,
            0, NULL) < 0)
        FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node->sect_info.size;
    state.tot_sect_count += 1;
    state.ghost_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    fr_meta_size = H5FS_HEADER_SIZE(f);

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((tmp_file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    if (tmp_file_size != (file_size+fr_meta_size))
        TEST_ERROR

    PASSED()

    TESTING("adding a section via H5FS_sect_add() to free-space: test 3");

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    TEST_set_eoa((haddr_t)TEST_SECT_ADDR150);  /* set end of file address for shrinking */

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    init_flags = 0;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    if(NULL == (sect_node = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    /*
     * Add section A
     */
    init_sect_node(sect_node, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node,
	    H5FS_ADD_RETURNED_SPACE, &can_shrink) < 0)
	FAIL_STACK_ERROR

    /* nothing in free-space */
    HDmemset(&state, 0, sizeof(frspace_state_t));

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    TESTING("adding a section via H5FS_sect_add() to free-space: test 4");

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    TEST_set_eoa((haddr_t)TEST_SECT_ADDR150);  /* set end of file address for shrinking */

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    init_flags = 0;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    if(NULL == (sect_node = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    /*
     * Add section A
     */
    init_sect_node(sect_node, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node,
	    H5FS_ADD_DESERIALIZING, &can_shrink) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node) < 0)
	FAIL_STACK_ERROR

    /* Free the section node(s) */
    if(TEST_sect_free((H5FS_section_info_t *)sect_node) < 0)
	TEST_ERROR
    sect_node = NULL;

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(sect_node)
            TEST_sect_free((H5FS_section_info_t *)sect_node);
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_fs_sect_add() */


/*
 * To verify the finding of a section with the requested-size from free-space
 *
 * Test 1: Free-space is empty and is not able to fulfill the requested-size
 *	Set up: free-space is started up but is empty
 *
 * Test 2: Add a section and find the section whose size is equal to the requested-size
 * 	Set up: Add section A whose size is less than requested-size
 *		Add section B whose size is the same as requested-size with addr=b
 *		Add section C whose size is the same as requested-size with addr=c > b
 *		Add section D whose size is greater than requested-size
 *
 * Test 3: Add a section and find the section whose size is > requested-size
 * 	Set up: Add section A whose size is less than requested-size
 *		Add section B whose size is greater than requested-size
 *
 * Test 4: Add a section but the section is not able to fulfill the requested-size
 * 	Set up:	Add section A whose size is less than requested-size
 *
 */
static unsigned
test_fs_sect_find(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr=HADDR_UNDEF; 	/* address of free space */
    size_t		nclasses;
    H5FS_create_t 	cparam; 		/* creation parameters */
    frspace_state_t 	state;          	/* State of free space*/

    TEST_free_section_t 	*sect_node1 = NULL, *sect_node2, *sect_node3 = NULL, *sect_node4 = NULL;
    TEST_free_section_t 	*node;
    htri_t 			node_found = FALSE;
    unsigned			init_flags=0;

    TESTING("H5FS_sect_find(): free-space is empty");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)TEST_SECT_SIZE30, (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    PASSED()

    TESTING("H5FS_sect_find() a section equal to requested-size from free-space");

    /* reopen the free-space manager */
    if(NULL == (frsp = H5FS_open(f, H5P_DATASET_XFER_DEFAULT, fs_addr, nclasses,
			    test_classes, NULL, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR
    if (frsp->nclasses != nclasses)
	TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR60, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR


    /*
     * Add section C
     */
    if(NULL == (sect_node3 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node3, (haddr_t)(TEST_SECT_ADDR200), (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node3,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node3->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node2->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section D
     */
    if(NULL == (sect_node4 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node4, (haddr_t)TEST_SECT_ADDR300, (hsize_t)TEST_SECT_SIZE80, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node4,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node4->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)TEST_SECT_SIZE50, (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR

    if ((node->sect_info.addr != TEST_SECT_ADDR100) || (node->sect_info.size != TEST_SECT_SIZE50))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* remove sections A, C and D */
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1) < 0)
	FAIL_STACK_ERROR
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node3) < 0)
	FAIL_STACK_ERROR
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node4) < 0)
	FAIL_STACK_ERROR

    /* Free the section node(s) */
    if(TEST_sect_free((H5FS_section_info_t *)sect_node1) < 0)
	TEST_ERROR
    sect_node1 = NULL;
    if(TEST_sect_free((H5FS_section_info_t *)sect_node3) < 0)
	TEST_ERROR
    sect_node3 = NULL;
    if(TEST_sect_free((H5FS_section_info_t *)sect_node4) < 0)
	TEST_ERROR
    sect_node4 = NULL;

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    PASSED()

    TESTING("H5FS_sect_find() a section greater than requested-size from free-space");

    /* reopen the free-space manager */
    if(NULL == (frsp = H5FS_open(f, H5P_DATASET_XFER_DEFAULT, fs_addr, nclasses,
			    test_classes, NULL, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR
    if (frsp->nclasses != nclasses)
	TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR60, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR200, (hsize_t)TEST_SECT_SIZE80, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node2->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)TEST_SECT_SIZE50, (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR
    if ((node->sect_info.addr != TEST_SECT_ADDR200) || (node->sect_info.size < TEST_SECT_SIZE50))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR
    node = NULL;

    /* remove sections A */
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1) < 0)
	FAIL_STACK_ERROR

    /* Free the section node(s) */
    if(TEST_sect_free((H5FS_section_info_t *)sect_node1) < 0)
	TEST_ERROR
    sect_node1 = NULL;

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    PASSED()

    TESTING("H5FS_sect_find(): cannot find a section with requested-size from free-space");

    /* reopen the free-space manager */
    if(NULL == (frsp = H5FS_open(f, H5P_DATASET_XFER_DEFAULT, fs_addr, nclasses,
			    test_classes, NULL, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR
    if (frsp->nclasses != nclasses)
	TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR60, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)TEST_SECT_SIZE50, (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    /* remove sections A */
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1) < 0)
	FAIL_STACK_ERROR

    /* Free the section node(s) */
    if(TEST_sect_free((H5FS_section_info_t *)sect_node1) < 0)
	TEST_ERROR
    sect_node1 = NULL;

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(sect_node1)
            TEST_sect_free((H5FS_section_info_t *)sect_node1);
        if(sect_node3)
            TEST_sect_free((H5FS_section_info_t *)sect_node3);
        if(sect_node4)
            TEST_sect_free((H5FS_section_info_t *)sect_node4);
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_fs_sect_find() */


/*
 * To verify that sections are merged when adding sections to free-space
 *
 * Test 1:
 *   Set up:
 *	H5FS_CLS_SEPAR_OBJ (cls->flags) is not set
 *	H5FS_ADD_RETURNED_SPACE is passed to H5FS_sect_add()
 *
 *   Add sections C, B, A & D that can be merged together
 *
 * Test 2:
 *   Set up:
 *	H5FS_CLS_SEPAR_OBJ (cls->flags) is set
 *	H5FS_ADD_RETURNED_SPACE is passed to H5FS_sect_add()
 *
 *   Add sections A & B that can be merged together but cannot do so because H5FS_CLS_SEPAR_OBJ flag is set
 *
 * Test 3:
 *   Set up:
 *	H5FS_CLS_SEPAR_OBJ (cls->flags) is not set
 *	H5FS_ADD_RETURNED_SPACE is passed to H5FS_sect_add()
 *
 *    Add 4 sections that adjoin each other as follows:
 *	section A is of section class type A
 *	section B is of section class type B
 *	section C is of section class type B
 *	section D is of section class type A
 *    Sections B & C are merged together but not section A nor D because:
 *	sections B & C are merged because of the same section class type
 *	section A cannot be merged with the merged section of B & C because of different section class type
 *	section D cannot be merged with the merged section of B & C because of different section class type
 */
static unsigned
test_fs_sect_merge(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr=HADDR_UNDEF; 	/* address of free space */
    size_t		nclasses;
    H5FS_create_t 	cparam; 		/* creation parameters */
    frspace_state_t 	state;          	/* State of free space*/

    TEST_free_section_t 	*sect_node1=NULL, *sect_node2=NULL, *sect_node3=NULL, *sect_node4=NULL;
    unsigned			init_flags=0;
    htri_t 			node_found = FALSE;
    TEST_free_section_t 	*node;

    TESTING("the merge of sections when H5FS_sect_add() to free-space: test 1");

    /*
     * TEST 1
     */
    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section C
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_SECT_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /* section B & C are merged */
    state.tot_space += TEST_SECT_SIZE30;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node3 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node3, (haddr_t)TEST_SECT_ADDR60, (hsize_t)TEST_SECT_SIZE10, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node3,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /* section A is merged with the merged section of B & C */
    state.tot_space += TEST_SECT_SIZE10;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section D
     */
    if(NULL == (sect_node4 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node4, (haddr_t)TEST_SECT_ADDR150, (hsize_t)TEST_SECT_SIZE80, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node4,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /* section D is merged with the merged section of A & B & C */
    state.tot_space += TEST_SECT_SIZE80;

    if(check_stats(f, frsp, &state))
        TEST_ERROR


    /* should be able to find the merged section of A, B, C & D */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE10+TEST_SECT_SIZE30+TEST_SECT_SIZE50+TEST_SECT_SIZE80), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR
    if ((node->sect_info.addr != TEST_SECT_ADDR60) ||
	(node->sect_info.size != (TEST_SECT_SIZE10+TEST_SECT_SIZE30+TEST_SECT_SIZE50+TEST_SECT_SIZE80)))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    /*
     * TEST 2
     */
    TESTING("the merge of sections when H5FS_sect_add() to free-space: test 2");

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    init_flags = H5FS_CLS_SEPAR_OBJ;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_SECT_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /* section A & B are not merged because H5FS_CLS_SEPAR_OBJ is set */
    state.tot_space += TEST_SECT_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* should not be able to find the merged section of A & B */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE30+TEST_SECT_SIZE50), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    /* remove section A from free-space */
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1) < 0)
	FAIL_STACK_ERROR
    /* remove section B from free-space */
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2) < 0)
	FAIL_STACK_ERROR

    /* Free the section node(s) */
    if(TEST_sect_free((H5FS_section_info_t *)sect_node1) < 0)
	TEST_ERROR
    if(TEST_sect_free((H5FS_section_info_t *)sect_node2) < 0)
	TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    /*
     * TEST 3
     */
    TESTING("the merge of sections when H5FS_sect_add() to free-space: test 3");

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    init_flags = 0; /* reset */
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR60, (hsize_t)TEST_SECT_SIZE10, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_SECT_SIZE10;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE_NEW, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /* sections A & B are not merged because H5FS_CLS_MERGE_SYM is set & section class type is different */
    state.tot_space += TEST_SECT_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section C
     */
    if(NULL == (sect_node3 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node3, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE_NEW, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node3,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /* sections B & C are merged because H5FS_CLS_MERGE_SYM is set & section class type is the same */
    state.tot_space += TEST_SECT_SIZE50;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section D
     */
    if(NULL == (sect_node4 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node4, (haddr_t)TEST_SECT_ADDR150, (hsize_t)TEST_SECT_SIZE80, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node4,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /*
     * section D is not merged with the merged section of B & C because
     * H5FS_CLS_MERGE_SYM is set and section class type is different
     */
    state.tot_space += TEST_SECT_SIZE80;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* should not be able to find a merged section of A, B, C & D */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE10+TEST_SECT_SIZE30+TEST_SECT_SIZE50+TEST_SECT_SIZE80), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    /* should be able to find the merged section of B & C */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE30+TEST_SECT_SIZE50), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR

    if ((node->sect_info.addr != TEST_SECT_ADDR70) ||
	(node->sect_info.size != (TEST_SECT_SIZE30+TEST_SECT_SIZE50)))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* should be able to find section A */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE10), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR

    if ((node->sect_info.addr != TEST_SECT_ADDR60) || (node->sect_info.size != TEST_SECT_SIZE10))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* should be able to find section D */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE80), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR

    if ((node->sect_info.addr != TEST_SECT_ADDR150) || (node->sect_info.size != TEST_SECT_SIZE80))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(sect_node1)
            TEST_sect_free((H5FS_section_info_t *)sect_node1);
        if(sect_node2)
            TEST_sect_free((H5FS_section_info_t *)sect_node2);
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_fs_sect_merge() */

/*
 * To verify that sections are shrunk when adding sections to free-space
 *
 *	Test 1:
 *	  Set EOF to be the ending address of section A
 *	  H5FS_CLS_SEPAR_OBJ (cls->flags) is not set when creating free-space manager
 *	  Add section A to allow shrinking but is not shrunk because its section class type
 *		TEST_FSPACE_SECT_TYPE_NEW does not define "can_shrink"
 *	  Result:section A is not shrunk and section A is still in free-space
 *
 *	  Re-add section A to allow shrinking and with section class type TEST_FSPACE_SECT_TYPE
 *		that defines "can_shrink"
 *	  Result:section A is shrunk and there is nothing in free-space
 *
 *	Test 2:
 *	  Set EOF to be greater than the ending address of section A
 *	  Set H5FS_CLS_SEPAR_OBJ (cls->flags) when creating free-space manager
 *
 *	  Add section A to allow shrinking but is not shrunk because it is not at EOF,
 *		and section A is not on the merge list due to H5FS_CLS_SEPAR_OBJ
 *	  Add section B to allow shrinking and whose ending address is the same as eof.
 *		 Section B is not merged with section A because of H5FS_CLS_SEPAR_OBJ but it is shrunk
 *	  Result: section A is still in free-space
 *
 *	Test 3:
 *	  Set EOF to be greater than the ending address of section A
 *	  H5FS_CLS_SEPAR_OBJ (cls->flags) is not set when creating free-space manager
 *
 *	  Add section A to allow shrinking but is not shrunk because it is not at EOF,
 *		and section A is on the merge list
 *	  Add section B to allow shrinking and whose ending address is the same as eof.
 *	  	Section B is merged with section A and then shrunk.
 *	  Result: free-space should be empty
 */
static unsigned
test_fs_sect_shrink(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr=HADDR_UNDEF; 	/* address of free space */
    size_t		nclasses;
    H5FS_create_t 	cparam; 		/* creation parameters */
    frspace_state_t 	state;          	/* State of free space*/

    TEST_free_section_t 	*sect_node1=NULL, *sect_node2=NULL;
    unsigned			init_flags=0;
    unsigned			can_shrink=FALSE;
    htri_t 			node_found = FALSE;
    TEST_free_section_t 	*node;

    TESTING("shrinking of sections when H5FS_sect_add() to free-space: test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    TEST_set_eoa((haddr_t)TEST_SECT_ADDR150);  /* set end of file address for shrinking */

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A that allow shrinking but its section class type does not define "can_shrink"
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE_NEW, H5FS_SECT_LIVE);

    can_shrink = FALSE;
    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, &can_shrink) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* section A should still be there in free-space */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE50), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR

    if ((node->sect_info.addr != TEST_SECT_ADDR100) || (node->sect_info.size != TEST_SECT_SIZE50))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /*
     * Re-add section A that allow shrinking and its section class type defines "can_shrink"
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    can_shrink = FALSE;
    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, &can_shrink) < 0)
	FAIL_STACK_ERROR

    /* should have nothing in free-space */
    HDmemset(&state, 0, sizeof(frspace_state_t));

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* section A should not be there in free-space */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE50), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()


    TESTING("shrinking of sections when H5FS_sect_add() to free-space: test 2");

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    TEST_set_eoa((haddr_t)TEST_SECT_ADDR150);  /* set end of file address for shrinking */

    /* does not allow merging */
    init_flags = H5FS_CLS_SEPAR_OBJ;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR80, (hsize_t)TEST_SECT_SIZE20, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, &can_shrink) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, &can_shrink) < 0)
	FAIL_STACK_ERROR

    /* free-space should be the same since section B is shrunk */
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* section B should not be there in free-space */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE50), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    if(check_stats(f, frsp, &state))
        TEST_ERROR


    /* section A should still be there in free-space */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE20), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (!node_found) TEST_ERROR

    if ((node->sect_info.addr != TEST_SECT_ADDR80) || (node->sect_info.size != TEST_SECT_SIZE20))
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    TESTING("shrinking of sections when H5FS_sect_add() to free-space: test 3");

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    TEST_set_eoa((haddr_t)TEST_SECT_ADDR150);  /* set end of file address for shrinking */

    init_flags = 0; /* reset */
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, &can_shrink) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, &can_shrink) < 0)
	FAIL_STACK_ERROR

    /* section A & B are merged and then strunk, so there is nothing in free-space */
    HDmemset(&state, 0, sizeof(frspace_state_t));
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* section B should not be there in free-space */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE50), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    /* section A should not be there in free-space */
    if((node_found = H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)(TEST_SECT_SIZE30), (H5FS_section_info_t **)&node)) < 0)
	FAIL_STACK_ERROR

    if (node_found) TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_sect_shrink() */

/*
 * To verify a section's class is changed via H5FS_sect_change_class()
 *
 * Test 1:
 *	Add section A with TEST_FSPACE_SECT_TYPE class type with H5FS_CLS_GHOST_OBJ setting
 *	Add section B with TEST_FSPACE_SECT_TYPE_NONE class type without H5FS_CLS_GHOST_OBJ setting
 * 	Change section A's class to the second section's class
 *	Result: serial_sect_count is incremented by 1; ghost_sect_count is decremented by 1
 *
 * Test 2:
 *	Add section A with TEST_FSPACE_SECT_TYPE class type with H5FS_CLS_SEPAR_OBJ setting
 *	Add section B with TEST_FSPACE_SECT_TYPE_NONE class type without H5FS_CLS_SEPAR_OBJ setting
 *	Add section C with TEST_FSPACE_SECT_TYPE_NONE class type without H5FS_CLS_SEPAR_OBJ setting
 *	Sections B & C are on the merge list
 * 	Change section class of B and C to A's section class
 *	Result: the merge list should be null and the class of sections B & C should be changed
 */
static unsigned
test_fs_sect_change_class(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr=HADDR_UNDEF; 	/* address of free space */
    size_t		nclasses;
    H5FS_create_t 	cparam; 		/* creation parameters */
    frspace_state_t 	state;          	/* State of free space*/

    TEST_free_section_t	*sect_node1=NULL, *sect_node2=NULL, *sect_node3=NULL;
    unsigned		init_flags=0;
    TEST_free_section_t 	*node;

    TESTING("the change of section class via H5FS_sect_change_class() in free-space: Test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    init_flags = H5FS_CLS_GHOST_OBJ;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR60, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_SECT_SIZE30;
    state.tot_sect_count += 1;
    state.ghost_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE_NONE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += TEST_SECT_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    if (H5FS_sect_change_class(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
		    TEST_FSPACE_SECT_TYPE_NONE) < 0)
	TEST_ERROR

    state.serial_sect_count += 1;
    state.ghost_sect_count -=1;
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)TEST_SECT_SIZE30, (H5FS_section_info_t **)&node) < 0)
	FAIL_STACK_ERROR

    if (node->sect_info.type != TEST_FSPACE_SECT_TYPE_NONE)
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2) < 0)
	FAIL_STACK_ERROR

    /* Free the section node(s) */
    if(TEST_sect_free((H5FS_section_info_t *)sect_node2) < 0)
	TEST_ERROR
    sect_node2 = NULL;

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    /*
     * TEST 2
     */
    TESTING("the merge of sections when H5FS_sect_add() to free-space: test 2");

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    init_flags = H5FS_CLS_SEPAR_OBJ;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE30, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE50, TEST_FSPACE_SECT_TYPE_NONE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /*
     * Add section C
     */
    if(NULL == (sect_node3 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node3, (haddr_t)TEST_SECT_ADDR200, (hsize_t)TEST_SECT_SIZE80, TEST_FSPACE_SECT_TYPE_NONE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node3,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    /* change the class of B to A's class */
    if (H5FS_sect_change_class(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
		    TEST_FSPACE_SECT_TYPE) < 0)
	TEST_ERROR

    /* change the class of C to A's class */
    if (H5FS_sect_change_class(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node3,
		    TEST_FSPACE_SECT_TYPE) < 0)
	TEST_ERROR

    /* the merge_list should be empty */
    if (frsp->sinfo->merge_list)
	if (H5SL_count(frsp->sinfo->merge_list))
	    TEST_ERROR

    /* verify that section B has changed class */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)TEST_SECT_SIZE50, (H5FS_section_info_t **)&node) < 0)
	FAIL_STACK_ERROR

    if (node->sect_info.type != TEST_FSPACE_SECT_TYPE)
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* verify that section C has changed class */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, frsp,
            (hsize_t)TEST_SECT_SIZE80, (H5FS_section_info_t **)&node) < 0)
	FAIL_STACK_ERROR

    if (node->sect_info.type != TEST_FSPACE_SECT_TYPE)
	TEST_ERROR

    if(TEST_sect_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* remove section A from free-space */
    if(H5FS_sect_remove(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1) < 0)
	FAIL_STACK_ERROR

    /* Free the section node(s) */
    if(TEST_sect_free((H5FS_section_info_t *)sect_node1) < 0)
	TEST_ERROR
    sect_node1 = NULL;

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(sect_node1)
            TEST_sect_free((H5FS_section_info_t *)sect_node1);
        if(sect_node2)
            TEST_sect_free((H5FS_section_info_t *)sect_node2);
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_sect_change_class() */


/*
 * To verify the extension of a block using space from a section in free-space
 *
 * Test 1: Try to extend the block by requested-size, which is equal to section B's size
 *	  	Add section A (addr=70, size=5)
 *		Add section B (addr=100, size=40)
 *		Try to extend the block (addr=80, size=20) by 40, which is the same as section B's size
 *		Result: succeed in extending the block
 *
 * Test 2: Try to extend the block by requested-size, which is greater than section B's size
 *	  	Add section A (addr=70, size=5)
 *		Add section B (addr=100, size=40)
 *		Try to extend the block (addr=80, size=20) by 50, which is greater than section B's size
 *		Result: fail in extending the block
 *
 * Test 3: Try to extend the block by requested-size, which is less than section B's size
 *	  	Add section A (addr=70, size=5)
 *		Add section B (addr=100, size=40)
 *		Try to extend the block (addr=80, size=20) by 30, which is less than section B's size
 *		Result: succeed in extending the block and a section of size=10 is left in free-space
 *
 * Test 4: Try to extend the block which does not adjoin section B
 *	  	Add section A (addr=70, size=5)
 *		Add section B (addr=100, size=40)
 *		Try to extend the block (addr=80, size=15) by 40
 *		Result: fail in extending the block because:
 *			the block does not adjoin section B (80+15 != addr of section B (80))
 *			even though the requested-size is equal to section B's size
 *
 */
static unsigned
test_fs_sect_extend(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr=HADDR_UNDEF; 	/* address of free space */
    size_t		nclasses;
    H5FS_create_t 	cparam; 		/* creation parameters */
    frspace_state_t 	state;          	/* State of free space*/
    TEST_free_section_t *sect_node1=NULL, *sect_node2=NULL;
    unsigned		init_flags=0;
    htri_t              status;                 /* Status of 'try' calls */

    TESTING("a block's extension by requested-size which is = adjoining free section's size: Test 1");

    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /*
     * TEST 1
     */
    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE5, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE40, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node2->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Extend a block by requested-size */
    if((status = H5FS_sect_try_extend(f, H5P_DATASET_XFER_DEFAULT, frsp, (haddr_t)TEST_SECT_SIZE80, (hsize_t)TEST_SECT_SIZE20, (hsize_t)TEST_SECT_SIZE40)) < 0)
	FAIL_STACK_ERROR
    if(FALSE == status)
        TEST_ERROR

    /* Succeed in extending the block: free space info is decremented accordingly */
    state.tot_space -= (hsize_t)TEST_SECT_SIZE40;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    PASSED()

    /*
     * TEST 2
     */
    TESTING("a block's extension by requested-size which is > adjoining free section's size: Test 2");

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE5, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE40, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node2->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Extend the block by requested-size */
    if((status = H5FS_sect_try_extend(f, H5P_DATASET_XFER_DEFAULT, frsp, (haddr_t)TEST_SECT_ADDR80, (hsize_t)TEST_SECT_SIZE20, (hsize_t)TEST_SECT_SIZE50)) < 0)
	FAIL_STACK_ERROR
    if(TRUE == status)
        TEST_ERROR

    /* Not able to extend the block: free space info remains the same */
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    PASSED()

    /*
     * Test 3
     */
    TESTING("a block's extension by requested-size which is < adjoining free section's size: Test 3");

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE5, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE40, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node2->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Extend the block by requested-size */
    if((status = H5FS_sect_try_extend(f, H5P_DATASET_XFER_DEFAULT, frsp, (haddr_t)TEST_SECT_ADDR80, (hsize_t)TEST_SECT_SIZE20, (hsize_t)TEST_SECT_SIZE30)) < 0)
	TEST_ERROR
    if(FALSE == status)
        TEST_ERROR

    /* Succeed in extending the block: total free space is decreased but other info remains the same */
    state.tot_space -= 30;
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    PASSED()

    /*
     * TEST 4
     */
    TESTING("a block's extension by requested-size which does not adjoin any free section: Test 4");

    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    /*
     * Add section A
     */
    if(NULL == (sect_node1 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node1, (haddr_t)TEST_SECT_ADDR70, (hsize_t)TEST_SECT_SIZE5, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node1,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += sect_node1->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /*
     * Add section B
     */
    if(NULL == (sect_node2 = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	FAIL_STACK_ERROR

    init_sect_node(sect_node2, (haddr_t)TEST_SECT_ADDR100, (hsize_t)TEST_SECT_SIZE40, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

    if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node2,
	    H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	FAIL_STACK_ERROR

    state.tot_space += sect_node2->sect_info.size;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Extend the block by requested-size */
    if((status = H5FS_sect_try_extend(f, H5P_DATASET_XFER_DEFAULT, frsp, (haddr_t)TEST_SECT_ADDR80, (hsize_t)TEST_SECT_SIZE15, (hsize_t)TEST_SECT_SIZE40)) < 0)
	TEST_ERROR
    if(TRUE == status)
        TEST_ERROR

    /* Not able to extend the block: free space manager info remains the same */
    if(check_stats(f, frsp, &state))
        TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    PASSED()

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    return(0);

error:
    H5E_BEGIN_TRY {
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_sect_extend() */


/*
 * To verify the iteration of free-space sections
 *
 * Create free-space manager with H5FS_CLS_SEPAR_OBJ
 * Create a whole bunch of sections
 * Iterate through all sections and collect size and count for all sections
 * Check info with H5FS_sect_stat()
 */
static unsigned
test_fs_sect_iterate(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FS_t      	*frsp = NULL;          	/* pointer to free space structure */
    haddr_t     	fs_addr=HADDR_UNDEF; 	/* address of free space */
    size_t		nclasses;
    H5FS_create_t 	cparam; 		/* creation parameters */

    TEST_free_section_t 	*sect_node=NULL;
    unsigned			init_flags=0, sect_size;
    TEST_iter_ud_t 		udata;
    int				i;
    hsize_t			tot_space, nsects;

    TESTING("iteration of sections in the free-space manager");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    init_cparam(&cparam);
    nclasses = NELMTS(test_classes);
    udata.tot_size = 0;
    udata.tot_sect_count = 0;

    init_flags = H5FS_CLS_SEPAR_OBJ;
    if(NULL == (frsp = H5FS_create(f, H5P_DATASET_XFER_DEFAULT, &fs_addr,
	    &cparam, nclasses, test_classes, &init_flags, (hsize_t)FSPACE_THRHD_DEF, (hsize_t)FSPACE_ALIGN_DEF)))
	FAIL_STACK_ERROR

    if(!H5F_addr_defined(fs_addr))
        TEST_ERROR

    for (i = 1; i <= NUM_SECTIONS; i++) {
	if(NULL == (sect_node = (TEST_free_section_t *)HDmalloc(sizeof(TEST_free_section_t))))
	    FAIL_STACK_ERROR

	sect_size = (unsigned)((i-1) % 9) + 1;
	init_sect_node(sect_node, (haddr_t)i*10, (hsize_t)sect_size, TEST_FSPACE_SECT_TYPE, H5FS_SECT_LIVE);

	if(H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, frsp, (H5FS_section_info_t *)sect_node,
		H5FS_ADD_RETURNED_SPACE, NULL) < 0)
	    FAIL_STACK_ERROR
    }

    if(H5FS_sect_iterate(f, H5P_DATASET_XFER_DEFAULT, frsp, TEST_sects_cb, &udata) < 0)
	TEST_ERROR

    H5FS_sect_stats(frsp, &tot_space, &nsects);

    if (udata.tot_size != tot_space)
	TEST_ERROR
    if (udata.tot_sect_count != nsects)
	TEST_ERROR

    /* Close the free space manager */
    if(H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp) < 0)
	FAIL_STACK_ERROR
    frsp = NULL;

    /* Delete free space manager */
    if(H5FS_delete(f, H5P_DATASET_XFER_DEFAULT, fs_addr) < 0)
	FAIL_STACK_ERROR
    fs_addr = HADDR_UNDEF;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(frsp)
	    H5FS_close(f, H5P_DATASET_XFER_DEFAULT, frsp);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_fs_sect_iterate() */


int
main(void)
{
    hid_t       	fapl = -1;	/* File access property list for data files */
    unsigned    	nerrors = 0;    /* Cumulative error count */
    const char *env_h5_drvr = NULL;     /* File Driver value from environment */

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    fapl = h5_fileaccess();

    /* make sure alignment is not set for tests to succeed */
    if(H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)1) < 0)
        TEST_ERROR

    nerrors += test_fs_create(fapl);
    nerrors += test_fs_sect_add(fapl);
    nerrors += test_fs_sect_merge(fapl);
    nerrors += test_fs_sect_shrink(fapl);
    nerrors += test_fs_sect_find(fapl);
    nerrors += test_fs_sect_change_class(fapl);
    nerrors += test_fs_sect_extend(fapl);
    nerrors += test_fs_sect_iterate(fapl);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    puts("All free-space tests passed.");

    h5_cleanup(FILENAME, fapl);
    return (0);

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
    } H5E_END_TRY;
    return (1);
} /* main() */

