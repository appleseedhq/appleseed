#############################
 output for 'h5ls -w80 -Er textlinksrc.h5'
#############################
/                        Group
/ext_link1               External Link {textlinktar.h5//group} {Group}
/ext_link1/dset          Dataset {6}
/ext_link1/elink_t1      External Link {textlinksrc.h5//} {Group}
/ext_link1/elink_t1/ext_link1 External Link {textlinktar.h5//group} {Already Visited}
/ext_link1/elink_t1/ext_link2 External Link {textlinktar.h5//dset} {Dataset {6}}
/ext_link1/elink_t1/ext_link3 External Link {textlinktar.h5//type} {Type}
/ext_link1/elink_t1/ext_link4 External Link {textlinktar.h5//group/elink_t2} {**NOT FOUND**}
/ext_link1/elink_t1/ext_link5 External Link {textlinktar.h5//empty_group} {Group}
/ext_link1/elink_t2      External Link {textlinksrc.h5//ext_link4} {**NOT FOUND**}
/ext_link1/subgroup      Group
/ext_link1/subgroup/link_to_group Group, same as /ext_link1
/ext_link2               External Link {textlinktar.h5//dset} {Already Visited}
/ext_link3               External Link {textlinktar.h5//type} {Already Visited}
/ext_link4               External Link {textlinktar.h5//group/elink_t2} {Already Visited}
/ext_link5               External Link {textlinktar.h5//empty_group} {Already Visited}
