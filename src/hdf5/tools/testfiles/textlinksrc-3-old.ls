#############################
 output for 'h5ls -w80 -Er textlinksrc.h5/ext_link1'
#############################
ext_link1                External Link {textlinktar.h5//group} {Group}
/dset                    Dataset {6}
/elink_t1                External Link {textlinksrc.h5//} {Group}
/elink_t1/ext_link1      External Link {textlinktar.h5//group} {Already Visited}
/elink_t1/ext_link2      External Link {textlinktar.h5//dset} {Dataset {6}}
/elink_t1/ext_link3      External Link {textlinktar.h5//type} {Type}
/elink_t1/ext_link4      External Link {textlinktar.h5//group/elink_t2} {**NOT FOUND**}
/elink_t1/ext_link5      External Link {textlinktar.h5//empty_group} {Group}
/elink_t2                External Link {textlinksrc.h5//ext_link4} {**NOT FOUND**}
/subgroup                Group
/subgroup/link_to_group  Group, same as /ext_link1
