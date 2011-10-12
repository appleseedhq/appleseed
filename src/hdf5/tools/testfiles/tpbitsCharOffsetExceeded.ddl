#############################
Expected output for 'h5dump -d /DS08BITS -M 8,1 packedbits.h5'
#############################
HDF5 "packedbits.h5" {
DATASET "/DS08BITS" {
   DATATYPE  H5T_STD_I8LE
   DATASPACE  SIMPLE { ( 8, 8 ) / ( 8, 8 ) }
   PACKED_BITS OFFSET=8 LENGTH=1
   DATA {
   (0,0): 0, 0, 0, 0, 0, 0, 0, 0,
   (1,0): 0, 0, 0, 0, 0, 0, 0, 0,
   (2,0): 0, 0, 0, 0, 0, 0, 0, 0,
   (3,0): 0, 0, 0, 0, 0, 0, 0, 0,
   (4,0): 0, 0, 0, 0, 0, 0, 0, 0,
   (5,0): 0, 0, 0, 0, 0, 0, 0, 0,
   (6,0): 0, 0, 0, 0, 0, 0, 0, 0,
   (7,0): 0, 0, 0, 0, 0, 0, 0, 0
   }
}
}
h5dump error: Packed Bit offset+length value(9) too large. Max is 8
