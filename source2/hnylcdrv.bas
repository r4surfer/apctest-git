        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y     Y  L       CCCC  DDDD   RRRR  V    V *~
            *  H   H  NN  N   Y   Y   L      C    C D   D  R   R V    V *~
            *  HHHHH  N N N    Y Y    L      C      D   D  RRRR  V    V *~
            *  H   H  N  NN     Y     L      C    C D   D  R  R   V  V  *~
            *  H   H  N   N     Y     LLLLL   CCCC  DDDD   R   R   VV   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLCDRV - Driver program for HNYLCSUB for management of  *~
            *            location contents.                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1989, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/17/89 ! ORIGINAL                                 ! WPH *~
            * 03/09/90 ! Changed LOCATION from 200 - 400.         ! MLJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        dim part$25,                    /* Part Number passed          */~
            store$3,                    /* Store passed                */~
            lot$6                       /* lot  Number passed          */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            fs%(32)                      /* = 1 if file open, -1 if it */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************~
            DIM CMS2V$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
            cms2v$ = cms2v$

            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE (INVENTORY DATE) *~
            * # 2 ! STORNAME ! STORE MASTER FILE                        *~
            * # 3 ! USERINFO ! USER INFORMATION FILE                    *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 6 ! HNYLOCNS ! Inventory quantity by location file      *~
            * # 7 ! HNYQUAN  ! INVENTORY QUANTITY INFORMATION FILE      *~
            * # 8 ! LOCATION ! Location master File                     *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  #2, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select  #3, "USERINFO",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 3

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select  #6, "HNYLOCNS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  700,                                 ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42

            select #7,  "HNYQUAN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 650,                                  ~
                         keypos= 17, keylen = 44,                        ~
                         alternate key 1, keypos =  1, keylen = 44

            select #8,  "LOCATION",                                      ~
                        varc, indexed,  recsize =  400,                  ~
                        keypos = 1, keylen = 11,                         ~
                         alternate key 1, keypos =  4, keylen = 11


            call "SHOSTAT"  ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  fs%( 1), f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (#2,  fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (#3,  fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (#4,  fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (#6,  fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (#7,  fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (#8,  fs%( 8), f2%( 8),   0%, rslt$( 8))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

             part$    = " "
             store$   = " "
             lot$     = " "
             action%  =  1%    /* 1 = prompt the user for data */
             qty      =  0

            call "HNYLCSUB" (part$,   /* to see locations of or manage */~
                            store$,   /* " " if to use the user default*/~
                            lot$,     /* to see locations of or manage */~
                            qty,      /* to transfer, add, or withdraw */~
                            action%,  /* See HNYLCSUB header           */~
                            #1,    /* SYSFILE2   Misc. Info            */~
                            #2,    /* STORNAME   Warehouse information */~
                            #3,    /* USERINFO   User Information      */~
                            #4,    /* HNYMASTR   Part Master File      */~
                            #6,    /* HNYLOCNS   Location quantity file*/~
                            #7,    /* HNYQUAN    Part/Store/Lot Qty    */~
                            #8)    /* LOCATION   Location master file  */~


        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            call "SHOSTAT" ("One Moment Please")
            end
