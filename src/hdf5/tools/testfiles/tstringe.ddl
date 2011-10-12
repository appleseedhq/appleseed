#############################
Expected output for 'h5dump -e tstr3.h5'
#############################
HDF5 "tstr3.h5" {
GROUP "/" {
   DATASET "str1" {
      DATATYPE  H5T_STRING {
            STRSIZE 73;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
      (0): "quote \"  backspace\b form feed\f new line\n tab\t new line\n carriage return\r"
      }
   }
   DATASET "str2" {
      DATATYPE  H5T_STRING {
            STRSIZE H5T_VARIABLE;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): "Four score and seven\n years ago our forefathers brought forth on this continent a new nation,",
      (1): "conceived in liberty\n and dedicated to the proposition that all men are created equal.",
      (2): "Now we are engaged\n in a great civil war,",
      (3): "testing whether that\n nation or any nation so conceived and so dedicated can long endure."
      }
   }
   DATASET "str3" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32LE "a";
         H5T_STRING {
            STRSIZE 255;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "str";
      }
      DATASPACE  SIMPLE { ( 1 ) / ( 1 ) }
      DATA {
      (0): {
            24,
            "Four score and seven\n years ago our forefathers brought forth on this continent a new nation"
         }
      }
   }
   DATASET "str4" {
      DATATYPE  H5T_STD_I8LE
      DATASPACE  SIMPLE { ( 93 ) / ( 93 ) }
      DATA {
      (0): 70, 111, 117, 114, 32, 115, 99, 111, 114, 101, 32, 97, 110, 100,
      (14): 32, 115, 101, 118, 101, 110, 10, 32, 121, 101, 97, 114, 115, 32,
      (28): 97, 103, 111, 32, 111, 117, 114, 32, 102, 111, 114, 101, 102, 97,
      (42): 116, 104, 101, 114, 115, 32, 98, 114, 111, 117, 103, 104, 116,
      (55): 32, 102, 111, 114, 116, 104, 32, 111, 110, 32, 116, 104, 105,
      (68): 115, 32, 99, 111, 110, 116, 105, 110, 101, 110, 116, 32, 97, 32,
      (82): 110, 101, 119, 32, 110, 97, 116, 105, 111, 110, 0
      }
   }
}
}
