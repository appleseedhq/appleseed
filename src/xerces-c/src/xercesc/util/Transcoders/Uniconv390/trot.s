*
* Licensed to the Apache Software Foundation (ASF) under one or more
* contributor license agreements.  See the NOTICE file distributed with
* this work for additional information regarding copyright ownership.
* The ASF licenses this file to You under the Apache License, Version 2.0
* (the "License"); you may not use this file except in compliance with
* the License.  You may obtain a copy of the License at
* 
*      http://www.apache.org/licenses/LICENSE-2.0
* 
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or      
* implied. See the License for the specific language governing
* permissions and limitations under the License.
*

*
* $Id: trot.s 568078 2007-08-21 11:43:25Z amassari $
*

TROT     CSECT
TROT     RMODE ANY
         USING   *,15
         STM     14,12,12(13)  Save register content
         USING   PARMS,1       R1 = parameters
         L       6,INPUT       R6 = input data
         L       4,LENGTH@     
         L       9,0(4)        R9 = length of input data
         L       4,STOP
         L       0,0(4)        R0 = Stop Character
         LA      4,0
         LA      5,1
         L       8,OUTPUT      R8 = output data
         L       1,TBL         R1 = Translate table @
RETR     TROT    8,6   Translate input pointed to output buffer pointed
         BZ      END           Branch to END if done
         BM      END           Branch to END if Stop Char encountered
         B       RETR          Continue translate
END      L       14,12(,13)    Restore the registers
         LM      2,12,28(13)
         BALR    1,14          Return
PARMS    DSECT
INPUT    DS A
OUTPUT   DS A
LENGTH@  DS A
TBL      DS A
STOP     DS A
   END
