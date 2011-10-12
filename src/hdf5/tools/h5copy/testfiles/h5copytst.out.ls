#############################
Expected output for 'h5ls ./testfiles/h5copytst.out.h5'
#############################
Opened "./testfiles/h5copytst.out.h5" with sec2 driver.
/                        Group
    Location:  1:96
    Links:     1
/A                       Group
    Location:  1:90032
    Links:     1
/A/B1                    Group
    Location:  1:90736
    Links:     1
/A/B1/simple             Dataset {6/6}
    Location:  1:89904
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/A/B2                    Group
    Location:  1:94272
    Links:     1
/A/B2/simple2            Dataset {6/6}
    Location:  1:94144
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/C                       Group
    Location:  1:97480
    Links:     1
/C/D                     Group
    Location:  1:98184
    Links:     1
/C/D/simple              Dataset {6/6}
    Location:  1:97352
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E                       Group
    Location:  1:112096
    Links:     1
/E/F                     Group
    Location:  1:112800
    Links:     1
/E/F/grp_dsets           Group
    Location:  1:100296
    Links:     1
/E/F/grp_dsets/chunk     Dataset {6/6}
    Location:  1:104480
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compact   Dataset {6/6}
    Location:  1:104936
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/compound  Dataset {2/2}
    Location:  1:105072
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/E/F/grp_dsets/compressed Dataset {6/6}
    Location:  1:107384
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/E/F/grp_dsets/named_vl  Dataset {2/2}
    Location:  1:111696
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:107552 variable length of
                   32-bit little-endian integer
/E/F/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:111824
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/E/F/grp_dsets/simple    Dataset {6/6}
    Location:  1:111968
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/E/F/grp_dsets/vl        Type
    Location:  1:107552
    Links:     2
    Type:      shared-1:107552 variable length of
                   32-bit little-endian integer
/G                       Group
    Location:  1:127744
    Links:     1
/G/H                     Group
    Location:  1:128448
    Links:     1
/G/H/grp_nested          Group
    Location:  1:114824
    Links:     1
/G/H/grp_nested/grp_dsets Group
    Location:  1:115616
    Links:     1
/G/H/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:119800
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:120256
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:120392
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/G/H/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:122704
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:127016
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:122872 variable length of
                   32-bit little-endian integer
/G/H/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:127144
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/G/H/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:127288
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/G/H/grp_nested/grp_dsets/vl Type
    Location:  1:122872
    Links:     2
    Type:      shared-1:122872 variable length of
                   32-bit little-endian integer
/chunk                   Dataset {6/6}
    Location:  1:6312
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compact                 Dataset {6/6}
    Location:  1:6440
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/compound                Dataset {2/2}
    Location:  1:8624
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/compressed              Dataset {6/6}
    Location:  1:12984
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets               Group
    Location:  1:33856
    Links:     1
/grp_dsets/chunk         Dataset {6/6}
    Location:  1:38040
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compact       Dataset {6/6}
    Location:  1:38496
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/compound      Dataset {2/2}
    Location:  1:38632
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_dsets/compressed    Dataset {6/6}
    Location:  1:40944
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_dsets/named_vl      Dataset {2/2}
    Location:  1:45256
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:41112 variable length of
                   32-bit little-endian integer
/grp_dsets/nested_vl     Dataset {2/2}
    Location:  1:45384
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_dsets/simple        Dataset {6/6}
    Location:  1:45528
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/simple_group  Dataset {6/6}
    Location:  1:61640
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_dsets/vl            Type
    Location:  1:41112
    Links:     2
    Type:      shared-1:41112 variable length of
                   32-bit little-endian integer
/grp_empty               Group
    Location:  1:33064
    Links:     1
/grp_nested              Group
    Location:  1:46320
    Links:     1
/grp_nested/grp_dsets    Group
    Location:  1:47112
    Links:     1
/grp_nested/grp_dsets/chunk Dataset {6/6}
    Location:  1:51296
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compact Dataset {6/6}
    Location:  1:51752
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/compound Dataset {2/2}
    Location:  1:51888
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_nested/grp_dsets/compressed Dataset {6/6}
    Location:  1:54200
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/named_vl Dataset {2/2}
    Location:  1:58512
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:54368 variable length of
                   32-bit little-endian integer
/grp_nested/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:58640
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_nested/grp_dsets/simple Dataset {6/6}
    Location:  1:58784
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_nested/grp_dsets/vl Type
    Location:  1:54368
    Links:     2
    Type:      shared-1:54368 variable length of
                   32-bit little-endian integer
/grp_rename              Group
    Location:  1:62848
    Links:     1
/grp_rename/chunk        Dataset {6/6}
    Location:  1:67032
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compact      Dataset {6/6}
    Location:  1:67488
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/compound     Dataset {2/2}
    Location:  1:67624
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/compressed   Dataset {6/6}
    Location:  1:69936
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets    Group
    Location:  1:75728
    Links:     1
/grp_rename/grp_dsets/chunk Dataset {6/6}
    Location:  1:79912
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compact Dataset {6/6}
    Location:  1:80368
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/compound Dataset {2/2}
    Location:  1:80504
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/grp_rename/grp_dsets/compressed Dataset {6/6}
    Location:  1:82816
    Links:     1
    Chunks:    {2} 8 bytes
    Storage:   <details removed for portability>
    Filter-0:  deflate-1 OPT {1}
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/named_vl Dataset {2/2}
    Location:  1:87128
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:82984 variable length of
                   32-bit little-endian integer
/grp_rename/grp_dsets/nested_vl Dataset {2/2}
    Location:  1:87256
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/grp_dsets/simple Dataset {6/6}
    Location:  1:87400
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/grp_dsets/vl Type
    Location:  1:82984
    Links:     2
    Type:      shared-1:82984 variable length of
                   32-bit little-endian integer
/grp_rename/named_vl     Dataset {2/2}
    Location:  1:74248
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:70104 variable length of
                   32-bit little-endian integer
/grp_rename/nested_vl    Dataset {2/2}
    Location:  1:74376
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/grp_rename/simple       Dataset {6/6}
    Location:  1:74520
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/grp_rename/vl           Type
    Location:  1:70104
    Links:     2
    Type:      shared-1:70104 variable length of
                   32-bit little-endian integer
/named_vl                Dataset {2/2}
    Location:  1:13200
    Links:     1
    Storage:   <details removed for portability>
    Type:      shared-1:13152 variable length of
                   32-bit little-endian integer
/nested_vl               Dataset {2/2}
    Location:  1:27488
    Links:     1
    Storage:   <details removed for portability>
    Type:      variable length of
                   variable length of
                       32-bit little-endian integer
/rename                  Dataset {2/2}
    Location:  1:31856
    Links:     1
    Storage:   <details removed for portability>
    Type:      struct {
                   "str1"             +0    20-byte null-terminated ASCII string
                   "str2"             +20   20-byte null-terminated ASCII string
               } 40 bytes
/simple                  Dataset {6/6}
    Location:  1:800
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
/simple_top              Dataset {6/6}
    Location:  1:29680
    Links:     1
    Storage:   <details removed for portability>
    Type:      32-bit little-endian integer
