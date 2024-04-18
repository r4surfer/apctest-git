        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP    CCC   DDDD   EEEEE  FFFFF  L      TTTTT   *~
            *  V   V  P   P  C   C  D   D  E      F      L        T     *~
            *  V   V  PPPP   C      D   D  EEEE   FFFF   L        T     *~
            *   V V   P      C   C  D   D  E      F      L        T     *~
            *    V    P       CCC   DDDD   EEEEE  F      LLLLL    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPCDEFLT - Looks for and returns a Default Contract ID    *~
            *            for the passed in Vendor, Item Type, Item, and *~
            *            date.  If more than 1 valid value only the     *~
            *            first one will be returned.                    *~
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
            * 11/23/94 ! Original                                 ! LDJ *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VPCDEFLT" (vendor$,         /* Vendor Code - Optional     */~
                                         /*   IN: if supplied restricts*/~
                                         /*       view to that Vendor. */~
                                         /*  OUT: Vendor for default   */~
                                         /*       Contract.            */~
                        contract$,       /*  OUT: Contract ID          */~
                        contract_line$,  /*  OUT: Contract Line        */~
                        contract_type$,  /* Type of Contract to limit  */~
                                         /*  search to:                */~
                                         /*   A = Work Order Activities*/~
                                         /*   P = Material (Parts) Only*/~
                        item$,           /* REQUIRED.  Search for      */~
                                         /*  contracts for this Part or*/~
                                         /*  Work Center Activity Code */~
                        order_date$,     /* If non-blank this date will*/~
                                         /*  be used to restrict the   */~
                                         /*  valid contracts to those  */~
                                         /*  whose start & end date    */~
                                         /*  encompass this date.      */~
                                         /* Otherwise today's date is  */~
                                         /*  used.                     */~
                        #1,              /* VPCMASTR file channel      */~
                        f1%)             /* 1 = Valid Contract returned*/~
                                         /* 0 = No Contract found or   */~
                                         /*     selected.              */

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            contract$16,                 /* Contract ID                */~
            contract_line$4,             /* Contract Line              */~
            contract_type$1,             /* Contract Type              */~
            date$8,                      /* Date for testing           */~
            end_date$6,                  /* Contract End Date          */~
            item$25,                     /* Part Number or ActivityCode*/~
            order_date$8,                /* Order Date                 */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            start_date$6,                /* Contract Start Date        */~
            vendor$9                     /* Vendor Code                */

        dim f1%(02)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! VPCMASTR ! Vendor Purchases Contract Master File    *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A T E   S E A R C H              *~
            *-----------------------------------------------------------*~
            * Initiate search for Default Contract.                     *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

*          Normalize Date
            date$ = order_date$
            if date$ = " " or date$ = blankdate$ then date$ = date
            call "DATEOK" (date$, akey%, plowkey$)
            call "DATUNFMT" (date$)

*         Vendor Code blank
            if vendor$ <> " " then L10180
               plowkey$ = contract_type$ & item$
               akey% = 2%
               break% = 26%
               goto L10230

L10180
*         Vendor Code present
               plowkey$ = vendor$
               akey% = 1%
               break% = 9%

L10230
*         PLOW Looking for Contract Meeting Criteria
L10240:     call "PLOWALTS" (#1, plowkey$, akey%, break%, f1%(1%))
            if f1%(1%) = 0% then exit_program
            if str(key(#1,2),1%,1%) <> contract_type$ then L10240
            if str(key(#1,2),2%,25%) <> item$ then L10240
            get #1 using L10290, start_date$, end_date$
L10290:     FMT POS(86), CH(6), CH(6)
            if date$ < start_date$ or date$ > end_date$ then L10240

*         Found 1!
            vendor$ = str(key(#1,1),,9%)
            contract$ = str(key(#1,0),,16%)
            contract_line$ = str(key(#1,0),17%)

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            f1% = f1%(1%)

            end
