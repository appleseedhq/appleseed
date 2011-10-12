#############################
Expected output for 'h5dump tvldtypes2.h5'
#############################
HDF5 "tvldtypes2.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_VLEN { H5T_VLEN { H5T_STD_U32LE}}
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): ((0)), ((100), (110, 111)), ((200), (210, 211), (220, 221, 222)),
      (3): ((300), (310, 311), (320, 321, 322), (330, 331, 332, 333))
      }
   }
}
}
