#############################
 output for 'h5ls --follow-symlinks -r tsoftlinks.h5'
#############################
/                        Group
/dset1                   Dataset {4, 2}
/dset2                   Dataset {4, 2}
/dtype                   Type
/group1                  Group
/group1/soft_dangle      Soft Link {not_yet} {**NOT FOUND**}
/group1/soft_dset1       Soft Link {/dset1} {Dataset {4, 2}}
/group1/soft_dset2       Soft Link {/dset2} {Dataset {4, 2}}
/group1/soft_dtype       Soft Link {/dtype} {Type}
/group1/soft_empty_grp   Soft Link {/group_empty} {Group}
/group_empty             Group
/soft_dangle             Soft Link {not_yet} {Already Visited}
/soft_dset1              Soft Link {/dset1} {Already Visited}
/soft_dtype              Soft Link {/dtype} {Already Visited}
/soft_empty_grp          Soft Link {/group_empty} {Already Visited}
/soft_group1             Soft Link {/group1} {Group}
/soft_group1/soft_dangle Soft Link {not_yet} {Already Visited}
/soft_group1/soft_dset1  Soft Link {/dset1} {Already Visited}
/soft_group1/soft_dset2  Soft Link {/dset2} {Already Visited}
/soft_group1/soft_dtype  Soft Link {/dtype} {Already Visited}
/soft_group1/soft_empty_grp Soft Link {/group_empty} {Already Visited}
