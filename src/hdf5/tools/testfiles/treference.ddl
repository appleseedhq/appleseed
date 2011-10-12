#############################
Expected output for 'h5dump tattr2.h5'
#############################
HDF5 "tattr2.h5" {
GROUP "/" {
   ATTRIBUTE "array" {
      DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): [ 1, 2, 3 ], [ 4, 5, 6 ]
      }
   }
   ATTRIBUTE "array2D" {
      DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
      (1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
      (2,0): [ 13, 14, 15 ], [ 16, 17, 18 ]
      }
   }
   ATTRIBUTE "array3D" {
      DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
      (0,1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
      (0,2,0): [ 13, 14, 15 ], [ 16, 17, 18 ],
      (1,0,0): [ 19, 20, 21 ], [ 22, 23, 24 ],
      (1,1,0): [ 25, 26, 27 ], [ 28, 29, 30 ],
      (1,2,0): [ 31, 32, 33 ], [ 34, 35, 36 ],
      (2,0,0): [ 37, 38, 39 ], [ 40, 41, 42 ],
      (2,1,0): [ 43, 44, 45 ], [ 46, 47, 48 ],
      (2,2,0): [ 49, 50, 51 ], [ 52, 53, 54 ],
      (3,0,0): [ 55, 56, 57 ], [ 58, 59, 60 ],
      (3,1,0): [ 61, 62, 63 ], [ 64, 65, 66 ],
      (3,2,0): [ 67, 68, 69 ], [ 70, 71, 72 ]
      }
   }
   ATTRIBUTE "bitfield" {
      DATATYPE  H5T_STD_B8LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): 0x01, 0x02
      }
   }
   ATTRIBUTE "bitfield2D" {
      DATATYPE  H5T_STD_B8LE
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): 0x01, 0x02,
      (1,0): 0x03, 0x04,
      (2,0): 0x05, 0x06
      }
   }
   ATTRIBUTE "bitfield3D" {
      DATATYPE  H5T_STD_B8LE
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): 0x01, 0x02,
      (0,1,0): 0x03, 0x04,
      (0,2,0): 0x05, 0x06,
      (1,0,0): 0x07, 0x08,
      (1,1,0): 0x09, 0x0a,
      (1,2,0): 0x0b, 0x0c,
      (2,0,0): 0x0d, 0x0e,
      (2,1,0): 0x0f, 0x10,
      (2,2,0): 0x11, 0x12,
      (3,0,0): 0x13, 0x14,
      (3,1,0): 0x15, 0x16,
      (3,2,0): 0x17, 0x18
      }
   }
   ATTRIBUTE "compound" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I8LE "a";
         H5T_IEEE_F64LE "b";
      }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): {
            1,
            2
         },
      (1): {
            3,
            4
         }
      }
   }
   ATTRIBUTE "compound2D" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I8LE "a";
         H5T_IEEE_F64LE "b";
      }
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): {
            1,
            2
         },
      (0,1): {
            3,
            4
         },
      (1,0): {
            5,
            6
         },
      (1,1): {
            7,
            8
         },
      (2,0): {
            9,
            10
         },
      (2,1): {
            11,
            12
         }
      }
   }
   ATTRIBUTE "compound3D" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I8LE "a";
         H5T_IEEE_F64LE "b";
      }
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): {
            1,
            2
         },
      (0,0,1): {
            3,
            4
         },
      (0,1,0): {
            5,
            6
         },
      (0,1,1): {
            7,
            8
         },
      (0,2,0): {
            9,
            10
         },
      (0,2,1): {
            11,
            12
         },
      (1,0,0): {
            13,
            14
         },
      (1,0,1): {
            15,
            16
         },
      (1,1,0): {
            17,
            18
         },
      (1,1,1): {
            19,
            20
         },
      (1,2,0): {
            21,
            22
         },
      (1,2,1): {
            23,
            24
         },
      (2,0,0): {
            25,
            26
         },
      (2,0,1): {
            27,
            28
         },
      (2,1,0): {
            29,
            30
         },
      (2,1,1): {
            31,
            32
         },
      (2,2,0): {
            33,
            34
         },
      (2,2,1): {
            35,
            36
         },
      (3,0,0): {
            37,
            38
         },
      (3,0,1): {
            39,
            40
         },
      (3,1,0): {
            41,
            42
         },
      (3,1,1): {
            43,
            44
         },
      (3,2,0): {
            45,
            46
         },
      (3,2,1): {
            47,
            48
         }
      }
   }
   ATTRIBUTE "enum" {
      DATATYPE  H5T_ENUM {
            H5T_STD_I32LE;
            "RED"              0;
            "GREEN"            1;
         }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): RED, RED
      }
   }
   ATTRIBUTE "enum2D" {
      DATATYPE  H5T_ENUM {
            H5T_STD_I32LE;
            "RED"              0;
            "GREEN"            1;
         }
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): RED, RED,
      (1,0): RED, RED,
      (2,0): RED, RED
      }
   }
   ATTRIBUTE "enum3D" {
      DATATYPE  H5T_ENUM {
            H5T_STD_I32LE;
            "RED"              0;
            "GREEN"            1;
         }
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): RED, RED,
      (0,1,0): RED, RED,
      (0,2,0): RED, RED,
      (1,0,0): RED, RED,
      (1,1,0): RED, RED,
      (1,2,0): RED, RED,
      (2,0,0): RED, RED,
      (2,1,0): RED, RED,
      (2,2,0): RED, RED,
      (3,0,0): RED, RED,
      (3,1,0): RED, RED,
      (3,2,0): RED, RED
      }
   }
   ATTRIBUTE "float" {
      DATATYPE  H5T_IEEE_F32LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): 1, 2
      }
   }
   ATTRIBUTE "float2D" {
      DATATYPE  H5T_IEEE_F32LE
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): 1, 2,
      (1,0): 3, 4,
      (2,0): 5, 6
      }
   }
   ATTRIBUTE "float3D" {
      DATATYPE  H5T_IEEE_F32LE
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): 1, 2,
      (0,1,0): 3, 4,
      (0,2,0): 5, 6,
      (1,0,0): 7, 8,
      (1,1,0): 9, 10,
      (1,2,0): 11, 12,
      (2,0,0): 13, 14,
      (2,1,0): 15, 16,
      (2,2,0): 17, 18,
      (3,0,0): 19, 20,
      (3,1,0): 21, 22,
      (3,2,0): 23, 24
      }
   }
   ATTRIBUTE "integer" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): 1, 2
      }
   }
   ATTRIBUTE "integer2D" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): 1, 2,
      (1,0): 3, 4,
      (2,0): 5, 6
      }
   }
   ATTRIBUTE "integer3D" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): 1, 2,
      (0,1,0): 3, 4,
      (0,2,0): 5, 6,
      (1,0,0): 7, 8,
      (1,1,0): 9, 10,
      (1,2,0): 11, 12,
      (2,0,0): 13, 14,
      (2,1,0): 15, 16,
      (2,2,0): 17, 18,
      (3,0,0): 19, 20,
      (3,1,0): 21, 22,
      (3,2,0): 23, 24
      }
   }
   ATTRIBUTE "opaque" {
      DATATYPE  
         H5T_OPAQUE;
         OPAQUE_TAG "1-byte opaque type";
      
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): 0x01, 0x02
      }
   }
   ATTRIBUTE "opaque2D" {
      DATATYPE  
         H5T_OPAQUE;
         OPAQUE_TAG "1-byte opaque type";
      
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): 0x01, 0x02,
      (1,0): 0x03, 0x04,
      (2,0): 0x05, 0x06
      }
   }
   ATTRIBUTE "opaque3D" {
      DATATYPE  
         H5T_OPAQUE;
         OPAQUE_TAG "1-byte opaque type";
      
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): 0x01, 0x02,
      (0,1,0): 0x03, 0x04,
      (0,2,0): 0x05, 0x06,
      (1,0,0): 0x07, 0x08,
      (1,1,0): 0x09, 0x0a,
      (1,2,0): 0x0b, 0x0c,
      (2,0,0): 0x0d, 0x0e,
      (2,1,0): 0x0f, 0x10,
      (2,2,0): 0x11, 0x12,
      (3,0,0): 0x13, 0x14,
      (3,1,0): 0x15, 0x16,
      (3,2,0): 0x17, 0x18
      }
   }
   ATTRIBUTE "string" {
      DATATYPE  H5T_STRING {
            STRSIZE 2;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): "ab", "de"
      }
   }
   ATTRIBUTE "string2D" {
      DATATYPE  H5T_STRING {
            STRSIZE 2;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): "ab", "cd",
      (1,0): "ef", "gh",
      (2,0): "ij", "kl"
      }
   }
   ATTRIBUTE "string3D" {
      DATATYPE  H5T_STRING {
            STRSIZE 2;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): "ab", "cd",
      (0,1,0): "ef", "gh",
      (0,2,0): "ij", "kl",
      (1,0,0): "mn", "pq",
      (1,1,0): "rs", "tu",
      (1,2,0): "vw", "xz",
      (2,0,0): "AB", "CD",
      (2,1,0): "EF", "GH",
      (2,2,0): "IJ", "KL",
      (3,0,0): "MN", "PQ",
      (3,1,0): "RS", "TU",
      (3,2,0): "VW", "XZ"
      }
   }
   ATTRIBUTE "vlen" {
      DATATYPE  H5T_VLEN { H5T_STD_I32LE}
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): (1), (2, 3)
      }
   }
   ATTRIBUTE "vlen2D" {
      DATATYPE  H5T_VLEN { H5T_STD_I32LE}
      DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
      DATA {
      (0,0): (0), (1),
      (1,0): (2, 3), (4, 5),
      (2,0): (6, 7, 8), (9, 10, 11)
      }
   }
   ATTRIBUTE "vlen3D" {
      DATATYPE  H5T_VLEN { H5T_STD_I32LE}
      DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
      DATA {
      (0,0,0): (0), (1),
      (0,1,0): (2), (3),
      (0,2,0): (4), (5),
      (1,0,0): (6, 7), (8, 9),
      (1,1,0): (10, 11), (12, 13),
      (1,2,0): (14, 15), (16, 17),
      (2,0,0): (18, 19, 20), (21, 22, 23),
      (2,1,0): (24, 25, 26), (27, 28, 29),
      (2,2,0): (30, 31, 32), (33, 34, 35),
      (3,0,0): (36, 37, 38, 39), (40, 41, 42, 43),
      (3,1,0): (44, 45, 46, 47), (48, 49, 50, 51),
      (3,2,0): (52, 53, 54, 55), (56, 57, 58, 59)
      }
   }
   DATASET "dset" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): 0, 0
      }
      ATTRIBUTE "array" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): [ 1, 2, 3 ], [ 4, 5, 6 ]
         }
      }
      ATTRIBUTE "array2D" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
         (1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
         (2,0): [ 13, 14, 15 ], [ 16, 17, 18 ]
         }
      }
      ATTRIBUTE "array3D" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
         (0,1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
         (0,2,0): [ 13, 14, 15 ], [ 16, 17, 18 ],
         (1,0,0): [ 19, 20, 21 ], [ 22, 23, 24 ],
         (1,1,0): [ 25, 26, 27 ], [ 28, 29, 30 ],
         (1,2,0): [ 31, 32, 33 ], [ 34, 35, 36 ],
         (2,0,0): [ 37, 38, 39 ], [ 40, 41, 42 ],
         (2,1,0): [ 43, 44, 45 ], [ 46, 47, 48 ],
         (2,2,0): [ 49, 50, 51 ], [ 52, 53, 54 ],
         (3,0,0): [ 55, 56, 57 ], [ 58, 59, 60 ],
         (3,1,0): [ 61, 62, 63 ], [ 64, 65, 66 ],
         (3,2,0): [ 67, 68, 69 ], [ 70, 71, 72 ]
         }
      }
      ATTRIBUTE "bitfield" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 0x01, 0x02
         }
      }
      ATTRIBUTE "bitfield2D" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 0x01, 0x02,
         (1,0): 0x03, 0x04,
         (2,0): 0x05, 0x06
         }
      }
      ATTRIBUTE "bitfield3D" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 0x01, 0x02,
         (0,1,0): 0x03, 0x04,
         (0,2,0): 0x05, 0x06,
         (1,0,0): 0x07, 0x08,
         (1,1,0): 0x09, 0x0a,
         (1,2,0): 0x0b, 0x0c,
         (2,0,0): 0x0d, 0x0e,
         (2,1,0): 0x0f, 0x10,
         (2,2,0): 0x11, 0x12,
         (3,0,0): 0x13, 0x14,
         (3,1,0): 0x15, 0x16,
         (3,2,0): 0x17, 0x18
         }
      }
      ATTRIBUTE "compound" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): {
               1,
               2
            },
         (1): {
               3,
               4
            }
         }
      }
      ATTRIBUTE "compound2D" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): {
               1,
               2
            },
         (0,1): {
               3,
               4
            },
         (1,0): {
               5,
               6
            },
         (1,1): {
               7,
               8
            },
         (2,0): {
               9,
               10
            },
         (2,1): {
               11,
               12
            }
         }
      }
      ATTRIBUTE "compound3D" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): {
               1,
               2
            },
         (0,0,1): {
               3,
               4
            },
         (0,1,0): {
               5,
               6
            },
         (0,1,1): {
               7,
               8
            },
         (0,2,0): {
               9,
               10
            },
         (0,2,1): {
               11,
               12
            },
         (1,0,0): {
               13,
               14
            },
         (1,0,1): {
               15,
               16
            },
         (1,1,0): {
               17,
               18
            },
         (1,1,1): {
               19,
               20
            },
         (1,2,0): {
               21,
               22
            },
         (1,2,1): {
               23,
               24
            },
         (2,0,0): {
               25,
               26
            },
         (2,0,1): {
               27,
               28
            },
         (2,1,0): {
               29,
               30
            },
         (2,1,1): {
               31,
               32
            },
         (2,2,0): {
               33,
               34
            },
         (2,2,1): {
               35,
               36
            },
         (3,0,0): {
               37,
               38
            },
         (3,0,1): {
               39,
               40
            },
         (3,1,0): {
               41,
               42
            },
         (3,1,1): {
               43,
               44
            },
         (3,2,0): {
               45,
               46
            },
         (3,2,1): {
               47,
               48
            }
         }
      }
      ATTRIBUTE "enum" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): RED, RED
         }
      }
      ATTRIBUTE "enum2D" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): RED, RED,
         (1,0): RED, RED,
         (2,0): RED, RED
         }
      }
      ATTRIBUTE "enum3D" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): RED, RED,
         (0,1,0): RED, RED,
         (0,2,0): RED, RED,
         (1,0,0): RED, RED,
         (1,1,0): RED, RED,
         (1,2,0): RED, RED,
         (2,0,0): RED, RED,
         (2,1,0): RED, RED,
         (2,2,0): RED, RED,
         (3,0,0): RED, RED,
         (3,1,0): RED, RED,
         (3,2,0): RED, RED
         }
      }
      ATTRIBUTE "float" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 1, 2
         }
      }
      ATTRIBUTE "float2D" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 1, 2,
         (1,0): 3, 4,
         (2,0): 5, 6
         }
      }
      ATTRIBUTE "float3D" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 1, 2,
         (0,1,0): 3, 4,
         (0,2,0): 5, 6,
         (1,0,0): 7, 8,
         (1,1,0): 9, 10,
         (1,2,0): 11, 12,
         (2,0,0): 13, 14,
         (2,1,0): 15, 16,
         (2,2,0): 17, 18,
         (3,0,0): 19, 20,
         (3,1,0): 21, 22,
         (3,2,0): 23, 24
         }
      }
      ATTRIBUTE "integer" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 1, 2
         }
      }
      ATTRIBUTE "integer2D" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 1, 2,
         (1,0): 3, 4,
         (2,0): 5, 6
         }
      }
      ATTRIBUTE "integer3D" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 1, 2,
         (0,1,0): 3, 4,
         (0,2,0): 5, 6,
         (1,0,0): 7, 8,
         (1,1,0): 9, 10,
         (1,2,0): 11, 12,
         (2,0,0): 13, 14,
         (2,1,0): 15, 16,
         (2,2,0): 17, 18,
         (3,0,0): 19, 20,
         (3,1,0): 21, 22,
         (3,2,0): 23, 24
         }
      }
      ATTRIBUTE "opaque" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 0x01, 0x02
         }
      }
      ATTRIBUTE "opaque2D" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 0x01, 0x02,
         (1,0): 0x03, 0x04,
         (2,0): 0x05, 0x06
         }
      }
      ATTRIBUTE "opaque3D" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 0x01, 0x02,
         (0,1,0): 0x03, 0x04,
         (0,2,0): 0x05, 0x06,
         (1,0,0): 0x07, 0x08,
         (1,1,0): 0x09, 0x0a,
         (1,2,0): 0x0b, 0x0c,
         (2,0,0): 0x0d, 0x0e,
         (2,1,0): 0x0f, 0x10,
         (2,2,0): 0x11, 0x12,
         (3,0,0): 0x13, 0x14,
         (3,1,0): 0x15, 0x16,
         (3,2,0): 0x17, 0x18
         }
      }
      ATTRIBUTE "reference" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): DATASET 976 /dset , DATASET 976 /dset 
         }
      }
      ATTRIBUTE "reference2D" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,0): DATASET 976 /dset , DATASET 976 /dset 
         }
      }
      ATTRIBUTE "reference3D" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (0,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (0,2,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,2,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,2,0): DATASET 976 /dset , DATASET 976 /dset ,
         (3,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (3,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (3,2,0): DATASET 976 /dset , DATASET 976 /dset 
         }
      }
      ATTRIBUTE "string" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): "ab", "de"
         }
      }
      ATTRIBUTE "string2D" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): "ab", "cd",
         (1,0): "ef", "gh",
         (2,0): "ij", "kl"
         }
      }
      ATTRIBUTE "string3D" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): "ab", "cd",
         (0,1,0): "ef", "gh",
         (0,2,0): "ij", "kl",
         (1,0,0): "mn", "pq",
         (1,1,0): "rs", "tu",
         (1,2,0): "vw", "xz",
         (2,0,0): "AB", "CD",
         (2,1,0): "EF", "GH",
         (2,2,0): "IJ", "KL",
         (3,0,0): "MN", "PQ",
         (3,1,0): "RS", "TU",
         (3,2,0): "VW", "XZ"
         }
      }
      ATTRIBUTE "vlen" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): (1), (2, 3)
         }
      }
      ATTRIBUTE "vlen2D" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): (0), (1),
         (1,0): (2, 3), (4, 5),
         (2,0): (6, 7, 8), (9, 10, 11)
         }
      }
      ATTRIBUTE "vlen3D" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): (0), (1),
         (0,1,0): (2), (3),
         (0,2,0): (4), (5),
         (1,0,0): (6, 7), (8, 9),
         (1,1,0): (10, 11), (12, 13),
         (1,2,0): (14, 15), (16, 17),
         (2,0,0): (18, 19, 20), (21, 22, 23),
         (2,1,0): (24, 25, 26), (27, 28, 29),
         (2,2,0): (30, 31, 32), (33, 34, 35),
         (3,0,0): (36, 37, 38, 39), (40, 41, 42, 43),
         (3,1,0): (44, 45, 46, 47), (48, 49, 50, 51),
         (3,2,0): (52, 53, 54, 55), (56, 57, 58, 59)
         }
      }
   }
   GROUP "g1" {
      ATTRIBUTE "array" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): [ 1, 2, 3 ], [ 4, 5, 6 ]
         }
      }
      ATTRIBUTE "array2D" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
         (1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
         (2,0): [ 13, 14, 15 ], [ 16, 17, 18 ]
         }
      }
      ATTRIBUTE "array3D" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
         (0,1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
         (0,2,0): [ 13, 14, 15 ], [ 16, 17, 18 ],
         (1,0,0): [ 19, 20, 21 ], [ 22, 23, 24 ],
         (1,1,0): [ 25, 26, 27 ], [ 28, 29, 30 ],
         (1,2,0): [ 31, 32, 33 ], [ 34, 35, 36 ],
         (2,0,0): [ 37, 38, 39 ], [ 40, 41, 42 ],
         (2,1,0): [ 43, 44, 45 ], [ 46, 47, 48 ],
         (2,2,0): [ 49, 50, 51 ], [ 52, 53, 54 ],
         (3,0,0): [ 55, 56, 57 ], [ 58, 59, 60 ],
         (3,1,0): [ 61, 62, 63 ], [ 64, 65, 66 ],
         (3,2,0): [ 67, 68, 69 ], [ 70, 71, 72 ]
         }
      }
      ATTRIBUTE "bitfield" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 0x01, 0x02
         }
      }
      ATTRIBUTE "bitfield2D" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 0x01, 0x02,
         (1,0): 0x03, 0x04,
         (2,0): 0x05, 0x06
         }
      }
      ATTRIBUTE "bitfield3D" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 0x01, 0x02,
         (0,1,0): 0x03, 0x04,
         (0,2,0): 0x05, 0x06,
         (1,0,0): 0x07, 0x08,
         (1,1,0): 0x09, 0x0a,
         (1,2,0): 0x0b, 0x0c,
         (2,0,0): 0x0d, 0x0e,
         (2,1,0): 0x0f, 0x10,
         (2,2,0): 0x11, 0x12,
         (3,0,0): 0x13, 0x14,
         (3,1,0): 0x15, 0x16,
         (3,2,0): 0x17, 0x18
         }
      }
      ATTRIBUTE "compound" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): {
               1,
               2
            },
         (1): {
               3,
               4
            }
         }
      }
      ATTRIBUTE "compound2D" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): {
               1,
               2
            },
         (0,1): {
               3,
               4
            },
         (1,0): {
               5,
               6
            },
         (1,1): {
               7,
               8
            },
         (2,0): {
               9,
               10
            },
         (2,1): {
               11,
               12
            }
         }
      }
      ATTRIBUTE "compound3D" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): {
               1,
               2
            },
         (0,0,1): {
               3,
               4
            },
         (0,1,0): {
               5,
               6
            },
         (0,1,1): {
               7,
               8
            },
         (0,2,0): {
               9,
               10
            },
         (0,2,1): {
               11,
               12
            },
         (1,0,0): {
               13,
               14
            },
         (1,0,1): {
               15,
               16
            },
         (1,1,0): {
               17,
               18
            },
         (1,1,1): {
               19,
               20
            },
         (1,2,0): {
               21,
               22
            },
         (1,2,1): {
               23,
               24
            },
         (2,0,0): {
               25,
               26
            },
         (2,0,1): {
               27,
               28
            },
         (2,1,0): {
               29,
               30
            },
         (2,1,1): {
               31,
               32
            },
         (2,2,0): {
               33,
               34
            },
         (2,2,1): {
               35,
               36
            },
         (3,0,0): {
               37,
               38
            },
         (3,0,1): {
               39,
               40
            },
         (3,1,0): {
               41,
               42
            },
         (3,1,1): {
               43,
               44
            },
         (3,2,0): {
               45,
               46
            },
         (3,2,1): {
               47,
               48
            }
         }
      }
      ATTRIBUTE "enum" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): RED, RED
         }
      }
      ATTRIBUTE "enum2D" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): RED, RED,
         (1,0): RED, RED,
         (2,0): RED, RED
         }
      }
      ATTRIBUTE "enum3D" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): RED, RED,
         (0,1,0): RED, RED,
         (0,2,0): RED, RED,
         (1,0,0): RED, RED,
         (1,1,0): RED, RED,
         (1,2,0): RED, RED,
         (2,0,0): RED, RED,
         (2,1,0): RED, RED,
         (2,2,0): RED, RED,
         (3,0,0): RED, RED,
         (3,1,0): RED, RED,
         (3,2,0): RED, RED
         }
      }
      ATTRIBUTE "float" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 1, 2
         }
      }
      ATTRIBUTE "float2D" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 1, 2,
         (1,0): 3, 4,
         (2,0): 5, 6
         }
      }
      ATTRIBUTE "float3D" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 1, 2,
         (0,1,0): 3, 4,
         (0,2,0): 5, 6,
         (1,0,0): 7, 8,
         (1,1,0): 9, 10,
         (1,2,0): 11, 12,
         (2,0,0): 13, 14,
         (2,1,0): 15, 16,
         (2,2,0): 17, 18,
         (3,0,0): 19, 20,
         (3,1,0): 21, 22,
         (3,2,0): 23, 24
         }
      }
      ATTRIBUTE "integer" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 1, 2
         }
      }
      ATTRIBUTE "integer2D" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 1, 2,
         (1,0): 3, 4,
         (2,0): 5, 6
         }
      }
      ATTRIBUTE "integer3D" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 1, 2,
         (0,1,0): 3, 4,
         (0,2,0): 5, 6,
         (1,0,0): 7, 8,
         (1,1,0): 9, 10,
         (1,2,0): 11, 12,
         (2,0,0): 13, 14,
         (2,1,0): 15, 16,
         (2,2,0): 17, 18,
         (3,0,0): 19, 20,
         (3,1,0): 21, 22,
         (3,2,0): 23, 24
         }
      }
      ATTRIBUTE "opaque" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 0x01, 0x02
         }
      }
      ATTRIBUTE "opaque2D" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 0x01, 0x02,
         (1,0): 0x03, 0x04,
         (2,0): 0x05, 0x06
         }
      }
      ATTRIBUTE "opaque3D" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 0x01, 0x02,
         (0,1,0): 0x03, 0x04,
         (0,2,0): 0x05, 0x06,
         (1,0,0): 0x07, 0x08,
         (1,1,0): 0x09, 0x0a,
         (1,2,0): 0x0b, 0x0c,
         (2,0,0): 0x0d, 0x0e,
         (2,1,0): 0x0f, 0x10,
         (2,2,0): 0x11, 0x12,
         (3,0,0): 0x13, 0x14,
         (3,1,0): 0x15, 0x16,
         (3,2,0): 0x17, 0x18
         }
      }
      ATTRIBUTE "string" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): "ab", "de"
         }
      }
      ATTRIBUTE "string2D" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): "ab", "cd",
         (1,0): "ef", "gh",
         (2,0): "ij", "kl"
         }
      }
      ATTRIBUTE "string3D" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): "ab", "cd",
         (0,1,0): "ef", "gh",
         (0,2,0): "ij", "kl",
         (1,0,0): "mn", "pq",
         (1,1,0): "rs", "tu",
         (1,2,0): "vw", "xz",
         (2,0,0): "AB", "CD",
         (2,1,0): "EF", "GH",
         (2,2,0): "IJ", "KL",
         (3,0,0): "MN", "PQ",
         (3,1,0): "RS", "TU",
         (3,2,0): "VW", "XZ"
         }
      }
      ATTRIBUTE "vlen" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): (1), (2, 3)
         }
      }
      ATTRIBUTE "vlen2D" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): (0), (1),
         (1,0): (2, 3), (4, 5),
         (2,0): (6, 7, 8), (9, 10, 11)
         }
      }
      ATTRIBUTE "vlen3D" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): (0), (1),
         (0,1,0): (2), (3),
         (0,2,0): (4), (5),
         (1,0,0): (6, 7), (8, 9),
         (1,1,0): (10, 11), (12, 13),
         (1,2,0): (14, 15), (16, 17),
         (2,0,0): (18, 19, 20), (21, 22, 23),
         (2,1,0): (24, 25, 26), (27, 28, 29),
         (2,2,0): (30, 31, 32), (33, 34, 35),
         (3,0,0): (36, 37, 38, 39), (40, 41, 42, 43),
         (3,1,0): (44, 45, 46, 47), (48, 49, 50, 51),
         (3,2,0): (52, 53, 54, 55), (56, 57, 58, 59)
         }
      }
   }
   GROUP "g2" {
      DATASET "array" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): [ 1, 2, 3 ], [ 4, 5, 6 ]
         }
      }
      DATASET "array2D" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
         (1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
         (2,0): [ 13, 14, 15 ], [ 16, 17, 18 ]
         }
      }
      DATASET "array3D" {
         DATATYPE  H5T_ARRAY { [3] H5T_STD_I32LE }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): [ 1, 2, 3 ], [ 4, 5, 6 ],
         (0,1,0): [ 7, 8, 9 ], [ 10, 11, 12 ],
         (0,2,0): [ 13, 14, 15 ], [ 16, 17, 18 ],
         (1,0,0): [ 19, 20, 21 ], [ 22, 23, 24 ],
         (1,1,0): [ 25, 26, 27 ], [ 28, 29, 30 ],
         (1,2,0): [ 31, 32, 33 ], [ 34, 35, 36 ],
         (2,0,0): [ 37, 38, 39 ], [ 40, 41, 42 ],
         (2,1,0): [ 43, 44, 45 ], [ 46, 47, 48 ],
         (2,2,0): [ 49, 50, 51 ], [ 52, 53, 54 ],
         (3,0,0): [ 55, 56, 57 ], [ 58, 59, 60 ],
         (3,1,0): [ 61, 62, 63 ], [ 64, 65, 66 ],
         (3,2,0): [ 67, 68, 69 ], [ 70, 71, 72 ]
         }
      }
      DATASET "bitfield" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 0x01, 0x02
         }
      }
      DATASET "bitfield2D" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 0x01, 0x02,
         (1,0): 0x03, 0x04,
         (2,0): 0x05, 0x06
         }
      }
      DATASET "bitfield3D" {
         DATATYPE  H5T_STD_B8LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 0x01, 0x02,
         (0,1,0): 0x03, 0x04,
         (0,2,0): 0x05, 0x06,
         (1,0,0): 0x07, 0x08,
         (1,1,0): 0x09, 0x0a,
         (1,2,0): 0x0b, 0x0c,
         (2,0,0): 0x0d, 0x0e,
         (2,1,0): 0x0f, 0x10,
         (2,2,0): 0x11, 0x12,
         (3,0,0): 0x13, 0x14,
         (3,1,0): 0x15, 0x16,
         (3,2,0): 0x17, 0x18
         }
      }
      DATASET "compound" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): {
               1,
               2
            },
         (1): {
               3,
               4
            }
         }
      }
      DATASET "compound2D" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): {
               1,
               2
            },
         (0,1): {
               3,
               4
            },
         (1,0): {
               5,
               6
            },
         (1,1): {
               7,
               8
            },
         (2,0): {
               9,
               10
            },
         (2,1): {
               11,
               12
            }
         }
      }
      DATASET "compound3D" {
         DATATYPE  H5T_COMPOUND {
            H5T_STD_I8LE "a";
            H5T_IEEE_F64LE "b";
         }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): {
               1,
               2
            },
         (0,0,1): {
               3,
               4
            },
         (0,1,0): {
               5,
               6
            },
         (0,1,1): {
               7,
               8
            },
         (0,2,0): {
               9,
               10
            },
         (0,2,1): {
               11,
               12
            },
         (1,0,0): {
               13,
               14
            },
         (1,0,1): {
               15,
               16
            },
         (1,1,0): {
               17,
               18
            },
         (1,1,1): {
               19,
               20
            },
         (1,2,0): {
               21,
               22
            },
         (1,2,1): {
               23,
               24
            },
         (2,0,0): {
               25,
               26
            },
         (2,0,1): {
               27,
               28
            },
         (2,1,0): {
               29,
               30
            },
         (2,1,1): {
               31,
               32
            },
         (2,2,0): {
               33,
               34
            },
         (2,2,1): {
               35,
               36
            },
         (3,0,0): {
               37,
               38
            },
         (3,0,1): {
               39,
               40
            },
         (3,1,0): {
               41,
               42
            },
         (3,1,1): {
               43,
               44
            },
         (3,2,0): {
               45,
               46
            },
         (3,2,1): {
               47,
               48
            }
         }
      }
      DATASET "enum" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): RED, RED
         }
      }
      DATASET "enum2D" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): RED, RED,
         (1,0): RED, RED,
         (2,0): RED, RED
         }
      }
      DATASET "enum3D" {
         DATATYPE  H5T_ENUM {
               H5T_STD_I32LE;
               "RED"              0;
               "GREEN"            1;
            }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): RED, RED,
         (0,1,0): RED, RED,
         (0,2,0): RED, RED,
         (1,0,0): RED, RED,
         (1,1,0): RED, RED,
         (1,2,0): RED, RED,
         (2,0,0): RED, RED,
         (2,1,0): RED, RED,
         (2,2,0): RED, RED,
         (3,0,0): RED, RED,
         (3,1,0): RED, RED,
         (3,2,0): RED, RED
         }
      }
      DATASET "float" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 1, 2
         }
      }
      DATASET "float2D" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 1, 2,
         (1,0): 3, 4,
         (2,0): 5, 6
         }
      }
      DATASET "float3D" {
         DATATYPE  H5T_IEEE_F32LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 1, 2,
         (0,1,0): 3, 4,
         (0,2,0): 5, 6,
         (1,0,0): 7, 8,
         (1,1,0): 9, 10,
         (1,2,0): 11, 12,
         (2,0,0): 13, 14,
         (2,1,0): 15, 16,
         (2,2,0): 17, 18,
         (3,0,0): 19, 20,
         (3,1,0): 21, 22,
         (3,2,0): 23, 24
         }
      }
      DATASET "integer" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 1, 2
         }
      }
      DATASET "integer2D" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 1, 2,
         (1,0): 3, 4,
         (2,0): 5, 6
         }
      }
      DATASET "integer3D" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 1, 2,
         (0,1,0): 3, 4,
         (0,2,0): 5, 6,
         (1,0,0): 7, 8,
         (1,1,0): 9, 10,
         (1,2,0): 11, 12,
         (2,0,0): 13, 14,
         (2,1,0): 15, 16,
         (2,2,0): 17, 18,
         (3,0,0): 19, 20,
         (3,1,0): 21, 22,
         (3,2,0): 23, 24
         }
      }
      DATASET "opaque" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): 0x01, 0x02
         }
      }
      DATASET "opaque2D" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): 0x01, 0x02,
         (1,0): 0x03, 0x04,
         (2,0): 0x05, 0x06
         }
      }
      DATASET "opaque3D" {
         DATATYPE  
            H5T_OPAQUE;
            OPAQUE_TAG "1-byte opaque type";
         
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): 0x01, 0x02,
         (0,1,0): 0x03, 0x04,
         (0,2,0): 0x05, 0x06,
         (1,0,0): 0x07, 0x08,
         (1,1,0): 0x09, 0x0a,
         (1,2,0): 0x0b, 0x0c,
         (2,0,0): 0x0d, 0x0e,
         (2,1,0): 0x0f, 0x10,
         (2,2,0): 0x11, 0x12,
         (3,0,0): 0x13, 0x14,
         (3,1,0): 0x15, 0x16,
         (3,2,0): 0x17, 0x18
         }
      }
      DATASET "reference" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): DATASET 976 /dset , DATASET 976 /dset 
         }
      }
      DATASET "reference2D" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,0): DATASET 976 /dset , DATASET 976 /dset 
         }
      }
      DATASET "reference3D" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (0,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (0,2,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (1,2,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (2,2,0): DATASET 976 /dset , DATASET 976 /dset ,
         (3,0,0): DATASET 976 /dset , DATASET 976 /dset ,
         (3,1,0): DATASET 976 /dset , DATASET 976 /dset ,
         (3,2,0): DATASET 976 /dset , DATASET 976 /dset 
         }
      }
      DATASET "string" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): "ab", "de"
         }
      }
      DATASET "string2D" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): "ab", "cd",
         (1,0): "ef", "gh",
         (2,0): "ij", "kl"
         }
      }
      DATASET "string3D" {
         DATATYPE  H5T_STRING {
               STRSIZE 2;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            }
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): "ab", "cd",
         (0,1,0): "ef", "gh",
         (0,2,0): "ij", "kl",
         (1,0,0): "mn", "pq",
         (1,1,0): "rs", "tu",
         (1,2,0): "vw", "xz",
         (2,0,0): "AB", "CD",
         (2,1,0): "EF", "GH",
         (2,2,0): "IJ", "KL",
         (3,0,0): "MN", "PQ",
         (3,1,0): "RS", "TU",
         (3,2,0): "VW", "XZ"
         }
      }
      DATASET "vlen" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
         (0): (1), (2, 3)
         }
      }
      DATASET "vlen2D" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 3, 2 ) / ( 3, 2 ) }
         DATA {
         (0,0): (0), (1),
         (1,0): (2, 3), (4, 5),
         (2,0): (6, 7, 8), (9, 10, 11)
         }
      }
      DATASET "vlen3D" {
         DATATYPE  H5T_VLEN { H5T_STD_I32LE}
         DATASPACE  SIMPLE { ( 4, 3, 2 ) / ( 4, 3, 2 ) }
         DATA {
         (0,0,0): (0), (1),
         (0,1,0): (2), (3),
         (0,2,0): (4), (5),
         (1,0,0): (6, 7), (8, 9),
         (1,1,0): (10, 11), (12, 13),
         (1,2,0): (14, 15), (16, 17),
         (2,0,0): (18, 19, 20), (21, 22, 23),
         (2,1,0): (24, 25, 26), (27, 28, 29),
         (2,2,0): (30, 31, 32), (33, 34, 35),
         (3,0,0): (36, 37, 38, 39), (40, 41, 42, 43),
         (3,1,0): (44, 45, 46, 47), (48, 49, 50, 51),
         (3,2,0): (52, 53, 54, 55), (56, 57, 58, 59)
         }
      }
   }
}
}
