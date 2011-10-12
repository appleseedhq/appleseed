#############################
Expected output for 'h5ls ./testfiles/h5copy_ref.out.h5'
#############################
Opened "./testfiles/h5copy_ref.out.h5" with sec2 driver.
/                        Group
    Location:  1:96
    Links:     1
/COPY                    Group
    Location:  1:1464
    Links:     1
/COPY/Dset1              Dataset {3/3}
    Location:  1:1504
    Links:     2
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/COPY/Dset2              Dataset {3/3, 16/16}
    Location:  1:1960
    Links:     3
    Storage:   <details removed for portability>
    Type:      8-bit integer
/COPY/Dset_OBJREF        Dataset {2/2}
    Location:  1:5184
    Links:     1
    Storage:   <details removed for portability>
    Type:      object reference
/COPY/Dset_REGREF        Dataset {2/2}
    Location:  1:9400
    Links:     1
    Storage:   <details removed for portability>
    Type:      dataset region reference
/COPY/Group              Group
    Location:  1:2096
    Links:     3
/~obj_pointed_by_2096    Group, same as /COPY/Group
