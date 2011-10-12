#############################
Expected output for 'h5dump -d /Dataset1 -M 0,1,1,1 tarray1.h5'
#############################
HDF5 "tarray1.h5" {
DATASET "/Dataset1" {
   DATATYPE  H5T_ARRAY { [4] H5T_STD_I32LE }
   DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
   PACKED_BITS OFFSET=0 LENGTH=1
   DATA {
   (0): [ 0, 1, 0, 1 ], [ 0, 1, 0, 1 ], [ 0, 1, 0, 1 ], [ 0, 1, 0, 1 ]
   }
   PACKED_BITS OFFSET=1 LENGTH=1
   DATA {
   (0): [ 0, 0, 1, 1 ], [ 1, 1, 0, 0 ], [ 0, 0, 1, 1 ], [ 1, 1, 0, 0 ]
   }
}
}
