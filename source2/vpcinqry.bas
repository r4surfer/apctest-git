        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP    CCC   IIIII  N   N   QQQ   RRRR   Y   Y   *~
            *  V   V  P   P  C   C    I    NN  N  Q   Q  R   R  Y   Y   *~
            *  V   V  PPPP   C        I    N N N  Q   Q  RRRR    Y Y    *~
            *   V V   P      C   C    I    N  NN  Q  QQ  R   R    Y     *~
            *    V    P       CCC   IIIII  N   N   QQQQ  R   R    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPCINQRY - Inquiry program for Purchasing Contracts.      *~
            *            Basically all this does is open files and then *~
            *            call the VPCINQSB routine.                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/08/94 ! Original                                 ! LDJ *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        dim                                                              ~
            contract_id$16,              /* Contract ID                */~
            contract_line$4              /* Contract Line ID           */

        dim f2%(14),                     /* = 0 if the file is open    */~
            f1%(14),                     /* = 1 if READ was successful */~
            fs%(14),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(14)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! VPCMASTR ! Vendor Purchases Contract Master File    *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! GENCODES ! System General Codes file.               *~
            * #05 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #06 ! TXTFILE  ! System Text File                         *~
            * #07 ! VBKMASTR ! Purchase Order Master file               *~
            * #08 ! VBKLINES ! Purchase Order Lines file                *~
            * #09 ! PAYMASTR ! A/P Invoice Master file                  *~
            * #10 ! PAYLINES ! A/P Invoice Lines file                   *~
            * #11 ! RCVLINES ! Receiver Line Items File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "VPCMASTR",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =  10,  keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   60, keylen =  26, dup

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #04, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #05, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #06, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #07, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25

            select #08, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28

            select #09, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  25

            select #10, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28

            select #11, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (#03, fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (#04, fs%( 4%), f2%( 4%), 0%, rslt$( 4%))
            call "OPENCHCK" (#05, fs%( 5%), f2%( 5%), 0%, rslt$( 5%))
            call "OPENCHCK" (#06, fs%( 6%), f2%( 6%), 0%, rslt$( 6%))
            call "OPENCHCK" (#07, fs%( 7%), f2%( 7%), 0%, rslt$( 7%))
            call "OPENCHCK" (#08, fs%( 8%), f2%( 8%), 0%, rslt$( 8%))
            call "OPENCHCK" (#09, fs%( 9%), f2%( 9%), 0%, rslt$( 9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))

        REM *************************************************************~
            *               C A L L   M A I N   P R O G R A M           *~
            *-----------------------------------------------------------*~
            * CALL VPCINQSB to do all the work.                         *~
            *************************************************************

        call "VPCINQSB" (                                                ~
            contract_id$,                /* Contract to Query          */~
            contract_line$,              /* Contract Line for Query    */~
            #01,   /* VPCMASTR Vendor Purchases Contract Master File   */~
            #03,   /* HNYMASTR Inventory Master File                   */~
            #04,   /* GENCODES System General Codes file.              */~
            #05,   /* VENDOR   VENDOR MASTER RECORD                    */~
            #06,   /* TXTFILE  System Text File                        */~
            #07,   /* VBKMASTR Purchase Order Master file              */~
            #08,   /* VBKLINES Purchase Order Lines file               */~
            #09,   /* PAYMASTR A/P Invoice Master file                 */~
            #10,   /* PAYLINES A/P Invoice Lines file                  */~
            #11,   /* RCVLINES Receiver Line Items File                */~
            f1%(1%))                     /* Contract Found Status      */
                                         /*  1 = Found, 2 = Not        */
        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution - returns to caller.                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN
            call "SHOSTAT" ("One Moment Please")
            end
