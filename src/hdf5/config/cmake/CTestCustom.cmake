SET (CTEST_CUSTOM_MAXIMUM_NUMBER_OF_WARNINGS 1500)
 
SET (CTEST_CUSTOM_WARNING_EXCEPTION
    ${CTEST_CUSTOM_WARNING_EXCEPTION}
    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning C4090"
    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning:[ \t]*passing argument"
    "H5detect.c[0-9 \t:]*warning:[ \t]*passing argument"
    "H5Tconv.c[0-9 \t:]*warning:[ \t]*comparison is always false due to limited range of data type"
    "testhdf5.h.[0-9]+.[ \t]*:[ \t]*warning C4005"
    "H5Ztrans.c.[0-9]+.[ \t]*:[ \t]*warning C4244"
    "SZIP.src.*:[ \t]*warning"
    "POSIX name for this item is deprecated"
    "disabling jobserver mode"
)
 
SET (CTEST_CUSTOM_MEMCHECK_IGNORE
    ${CTEST_CUSTOM_MEMCHECK_IGNORE}
    flush1           #designed to fail
    flush2           #designed to need flush1
    error_test       #uses runTest.cmake
    err_compat       #uses runTest.cmake
    links_env        #uses runTest.cmake
    h5test-clear-objects
    h5perform-clear-objects
    hl_test-clear-objects
    hl_fortran_test-clear-objects
    ######### tools/h5copy #########
    H5COPY-clearall-objects
    H5COPY-clear-refs
    H5COPY-clear-ext-links
    H5COPY-clear-misc
    H5COPY-clear-samefile
    ######### tools/h5diff #########
    H5DIFF-clearall-objects
    ######### tools/h5dump #########
    H5DUMP-clearall-objects
    H5DUMP-clear-out1
    H5DUMP-clear-out3
    H5DUMP-clear-objects
    H5DUMP_PACKED_BITS-clearall-objects
    H5DUMP-XML-clearall-objects
    ######### tools/h5import #########
    H5IMPORT-clear-objects
    H5IMPORT-ASCII_F32-clear-objects
    H5IMPORT-ASCII_UI16-clear-objects
    H5IMPORT-ASCII_UI32-clear-objects
    H5IMPORT-ASCII_I8-clear-objects
    H5IMPORT-ASCII_I16-clear-objects
    H5IMPORT-ASCII_I32-clear-objects
    H5IMPORT-ASCII_F64_R1-clear-objects
    H5IMPORT-BINARY_I8_EOF-clear-objects
    H5IMPORT-STR-clear-objects
    H5IMPORT-BINARY_UI32-clear-objects
    H5IMPORT-BINARY_UI16-clear-objects
    H5IMPORT-BINARY_I32-clear-objects
    H5IMPORT-BINARY_I8-clear-objects
    H5IMPORT-BINARY_I16-clear-objects
    H5IMPORT-BINARY_F64-clear-objects
    H5IMPORT-ASCII_F64-clear-objects
    ######### tools/h5jam #########
    H5JAM-SETUP-N_twithub_u10_c-clear-objects
    H5JAM-SETUP-N_twithub_u10_c
    H5JAM-N_twithub_u10_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u10_c
    H5JAM-CHECKFILE-N_twithub_u10_c-clear-objects
    H5JAM-SETUP-N_twithub_u511_c-clear-objects
    H5JAM-SETUP-N_twithub_u511_c
    H5JAM-N_twithub_u511_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u511_c
    H5JAM-CHECKFILE-N_twithub_u511_c-clear-objects
    H5JAM-SETUP-N_twithub_u512_c-clear-objects
    H5JAM-SETUP-N_twithub_u512_c
    H5JAM-N_twithub_u512_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u512_c
    H5JAM-CHECKFILE-N_twithub_u512_c-clear-objects
    H5JAM-SETUP-N_twithub_u513_c-clear-objects
    H5JAM-SETUP-N_twithub_u513_c
    H5JAM-N_twithub_u513_c-clear-objects
    H5JAM-NONE_COPY-N_twithub_u513_c
    H5JAM-CHECKFILE-N_twithub_u513_c-clear-objects
    H5JAM-SETUP-N_twithub513_u10_c-clear-objects
    H5JAM-SETUP-N_twithub513_u10_c
    H5JAM-N_twithub513_u10_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u10_c
    H5JAM-CHECKFILE-N_twithub513_u10_c-clear-objects
    H5JAM-SETUP-N_twithub513_u511_c-clear-objects
    H5JAM-SETUP-N_twithub513_u511_c
    H5JAM-N_twithub513_u511_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u511_c
    H5JAM-CHECKFILE-N_twithub513_u511_c-clear-objects
    H5JAM-SETUP-N_twithub513_u512_c-clear-objects
    H5JAM-SETUP-N_twithub513_u512_c
    H5JAM-N_twithub513_u512_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u512_c
    H5JAM-CHECKFILE-N_twithub513_u512_c-clear-objects
    H5JAM-SETUP-N_twithub513_u513_c-clear-objects
    H5JAM-SETUP-N_twithub513_u513_c
    H5JAM-N_twithub513_u513_c-clear-objects
    H5JAM-NONE_COPY-N_twithub513_u513_c
    H5JAM-CHECKFILE-N_twithub513_u513_c-clear-objects
    H5JAM-CHECKFILE-twithub_u10_c-clear-objects
    H5JAM-twithub_u511_c-clear-objects
    H5JAM-CHECKFILE-twithub_u511_c-clear-objects
    H5JAM-twithub_u512_c-clear-objects
    H5JAM-CHECKFILE-twithub_u512_c-clear-objects
    H5JAM-twithub_u513_c-clear-objects
    H5JAM-CHECKFILE-twithub_u513_c-clear-objects
    H5JAM-twithub513_u10_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u10_c-clear-objects
    H5JAM-twithub513_u511_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u511_c-clear-objects
    H5JAM-twithub513_u512_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u512_c-clear-objects
    H5JAM-twithub513_u513_c-clear-objects
    H5JAM-CHECKFILE-twithub513_u513_c-clear-objects
    H5JAM-SETUP-twithub_tall-clear-objects
    H5JAM-SETUP-twithub_tall
    H5JAM-UNJAM-twithub_tall-clear-objects
    H5JAM-UNJAM_D-twithub_tall-clear-objects
    H5JAM-CHECKFILE-twithub_tall-clear-objects
    H5JAM-SETUP-twithub513_tall-clear-objects
    H5JAM-SETUP-twithub513_tall
    H5JAM-UNJAM-twithub513_tall-clear-objects
    H5JAM-UNJAM_D-twithub513_tall-clear-objects
    H5JAM-CHECKFILE-twithub513_tall-clear-objects
    H5JAM-SETUP-N_twithub_tall-clear-objects
    H5JAM-SETUP-N_twithub_tall
    H5JAM-UNJAM-N_twithub_tall-clear-objects
    H5JAM-UNJAM_D-N_twithub_tall-clear-objects
    H5JAM-CHECKFILE-N_twithub_tall-clear-objects
    H5JAM-SETUP-N_twithub513_tall-clear-objects
    H5JAM-SETUP-N_twithub513_tall
    H5JAM-UNJAM-N_twithub513_tall-clear-objects
    H5JAM-UNJAM_D-N_twithub513_tall-clear-objects
    H5JAM-CHECKFILE-N_twithub513_tall-clear-objects
    H5JAM-SETUP-D_twithub_tall-clear-objects
    H5JAM-SETUP-D_twithub_tall
    H5JAM-UNJAM-D_twithub_tall-clear-objects
    H5JAM-UNJAM_D-D_twithub_tall-clear-objects
    H5JAM-CHECKFILE-D_twithub_tall-clear-objects
    H5JAM-SETUP-D_twithub513_tall-clear-objects
    H5JAM-SETUP-D_twithub513_tall
    H5JAM-UNJAM-D_twithub513_tall-clear-objects
    H5JAM-UNJAM_D-D_twithub513_tall-clear-objects
    H5JAM-CHECKFILE-D_twithub513_tall-clear-objects
    H5JAM-CHECKFILE-ta_u513-clear-objects
    H5JAM-twithub_u10-clear-objects
    H5JAM-CHECKFILE-twithub_u10-clear-objects
    H5JAM-twithub_u511-clear-objects
    H5JAM-CHECKFILE-twithub_u511-clear-objects
    H5JAM-twithub_u512-clear-objects
    H5JAM-CHECKFILE-twithub_u512-clear-objects
    H5JAM-twithub_u513-clear-objects
    H5JAM-CHECKFILE-twithub_u513-clear-objects
    H5JAM-twithub513_u10-clear-objects
    H5JAM-CHECKFILE-twithub513_u10-clear-objects
    H5JAM-twithub513_u511-clear-objects
    H5JAM-CHECKFILE-twithub513_u511-clear-objects
    H5JAM-twithub513_u512-clear-objects
    H5JAM-CHECKFILE-twithub513_u512-clear-objects
    H5JAM-twithub513_u513-clear-objects
    H5JAM-CHECKFILE-twithub513_u513-clear-objects
    H5JAM-twithub_u10_c-clear-objects
    H5JAM-tall_u10-clear-objects
    H5JAM-CHECKFILE-tall_u10-clear-objects
    H5JAM-tall_u511-clear-objects
    H5JAM-CHECKFILE-tall_u511-clear-objects
    H5JAM-tall_u512-clear-objects
    H5JAM-CHECKFILE-tall_u512-clear-objects
    H5JAM-tall_u513-clear-objects
    H5JAM-CHECKFILE-tall_u513-clear-objects
    H5JAM-SETUP-ta_u10-clear-objects
    H5JAM-SETUP-ta_u10
    H5JAM-ta_u10-clear-objects
    H5JAM-NONE_COPY-ta_u10
    H5JAM-CHECKFILE-ta_u10-clear-objects
    H5JAM-SETUP-ta_u511-clear-objects
    H5JAM-SETUP-ta_u511
    H5JAM-ta_u511-clear-objects
    H5JAM-NONE_COPY-ta_u511
    H5JAM-CHECKFILE-ta_u511-clear-objects
    H5JAM-SETUP-ta_u512-clear-objects
    H5JAM-SETUP-ta_u512
    H5JAM-ta_u512-clear-objects
    H5JAM-NONE_COPY-ta_u512
    H5JAM-CHECKFILE-ta_u512-clear-objects
    H5JAM-SETUP-ta_u513-clear-objects
    H5JAM-SETUP-ta_u513
    H5JAM-ta_u513-clear-objects
    H5JAM-NONE_COPY-ta_u513
    ######### tools/h5ls #########
    H5LS-clearall-objects
    ######### tools/h5repack #########
    H5REPACK-clearall-objects
    H5REPACK-gzip_verbose_filters                       #uses runTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_chunk_20x10            #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-chunk_20x10              #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-conti                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-compa                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_18x13                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_compa           #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_fixed_compa     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_long_switches     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_short_switches    #uses grepTest.cmake
    ######### tools/h5stat #########
    H5STAT-clearall-objects
    ######### tools/misc #########
    h5repart_20K-clear-objects
    h5repart_5K-clear-objects
    h5repart_sec2-clear-objects
    H5MKGRP_CMP-clear-h5mkgrp_help
    H5MKGRP_CMP-clear-h5mkgrp_version
    H5MKGRP-clear-h5mkgrp_single
    H5MKGRP-h5mkgrp_single                              #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single                         #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single-v
    H5MKGRP-h5mkgrp_single-v                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single-v                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single-p
    H5MKGRP-h5mkgrp_single-p                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single-p                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single_latest-l
    H5MKGRP-h5mkgrp_single_latest-l                     #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single_latest-l                #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several
    H5MKGRP-h5mkgrp_several                             #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several                        #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several-v
    H5MKGRP-h5mkgrp_several-v                           #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several-v                      #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several-p
    H5MKGRP-h5mkgrp_several-p                           #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several-p                      #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several_latest-l
    H5MKGRP-h5mkgrp_several_latest-l                    #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several_latest-l               #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested-p
    H5MKGRP-h5mkgrp_nested-p                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested-p                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_latest-lp
    H5MKGRP-h5mkgrp_nested_latest-lp                    #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_latest-lp               #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_mult-p
    H5MKGRP-h5mkgrp_nested_mult-p                       #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_mult-p                  #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_mult_latest-lp
    H5MKGRP-h5mkgrp_nested_mult_latest-lp               #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_mult_latest-lp          #uses runTest.cmake
    ######### examples #########
    EXAMPLES-clear-objects
    cpp_ex-clear-objects
)
