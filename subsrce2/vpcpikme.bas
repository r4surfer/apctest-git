        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  PPPP    CCC   PPPP   IIIII  K   K  M   M  EEEEE   *~
            *  V   V  P   P  C   C  P   P    I    K  K   MM MM  E       *~
            *  V   V  PPPP   C      PPPP     I    KKK    M M M  EEEE    *~
            *   V V   P      C   C  P        I    K  K   M   M  E       *~
            *    V    P       CCC   P      IIIII  K   K  M   M  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPCPIKME - Purchases Contract Selection/Inquiry subroutine*~
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
            * 06/27/94 ! Original                                 ! LDJ *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VPCPIKME" (vendor$,         /* Vendor Code - Optional     */~
                                         /*   IN: if supplied restricts*/~
                                         /*       view to that Vendor. */~
                                         /*  OUT: Vendor for selected  */~
                                         /*       Contract.            */~
                        contract$,       /* Contract ID                */~
                        contract_line$,  /* Optional Contract Line -   */~
                        contract_type$,  /* Type of Contract to limit  */~
                                         /*  view/validation to.       */~
                                         /*   A = Work Order Activities*/~
                                         /*   H = Contract Headers     */~
                                         /*   M = Miscell Contracts    */~
                                         /*   P = Material (Parts) Only*/~
                                         /*     = (blank) = ALL        */~
                                         /*   In addition to the passed*/~
                                         /*   in Type types H and M    */~
                                         /*   will also be eligible.   */~
                        item$,           /* Optional.  If non-blank    */~
                                         /*  then only view/validate   */~
                                         /*  contracts for this Part or*/~
                                         /*  Work Center Activity Code */~
                                         /*  (does not apply to H and  */~
                                         /*   M contract types).       */~
                        order_date$,     /* If non-blank this date will*/~
                                         /*  be used to restrict the   */~
                                         /*  valid contracts to those  */~
                                         /*  whose start & end date    */~
                                         /*  encompass this date.      */~
                        #1,              /* VPCMASTR file channel      */~
                        #2,              /* VENDOR file channel        */~
                        f1%)             /* 1 = Valid Contract returned*/~
                                         /* 0 = No Contract found or   */~
                                         /*     selected.              */

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            contract$16,                 /* Contract ID                */~
            contract_line$4,             /* Contract Line              */~
            contract_type$1,             /* Contract Type              */~
            date$8,                      /* Date for testing           */~
            descr_map(34),               /* PLOWCODE Arg               */~
            errormsg$79,                 /* Error message              */~
            hdr$(3)132,                  /* PLOWCODE Arg               */~
            incl_excl(7),incl_excl$(7)25,/* PLOWCODE Arg               */~
            inpmessage$79,               /* Informational Message      */~
            item$25,                     /* Part Number or ActivityCode*/~
            order_date$8,                /* Order Date                 */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
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
            * #02 ! VENDOR   ! Vendor Master File                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            date$ = order_date$
            if date$ <> " " and date$ <> blankdate$ ~
                  then call "DATEOK" (date$, i%, errormsg$)
            if date$ <> " " and date$ <> blankdate$ then call "DATUNFMT" (date$)

            hdr$(3%) = hex(ac) & "Select Purchase Contract"
            str(hdr$(3%),62%) = "VPCPIKME: " & str(cms2v$,,8)
            i% = 1%
            mat incl_excl = zer

*         Vendor Code blank
            if vendor$ <> " " then L09220
               plowkey$ = str(contract$) & contract_line$
               altkeydef = 0.132
               break% = 9000%
               goto L09270

L09220
*         Vendor Code present
               plowkey$ = str(vendor$) & str(contract$) & contract_line$
               altkeydef = 1.132
               break% = 9009%

L09270
*         Part or Activity Code present?
            if item$ = " " then L09320
               incl_excl(i%) = 61.25 : incl_excl$(i%) = item$
               i% = i% + 1%
               incl_excl(i%) = 61.25 : incl_excl$(i%) = " "
               i% = i% + 1%

L09320
*         Contract Type Present?
            if contract_type$ = " " then L09390
               incl_excl(i%) = 60.01 : incl_excl$(i%) = contract_type$
               i% = i% + 1%
               incl_excl(i%) = 60.01 : incl_excl$(i%) = "H"
               i% = i% + 1%
               incl_excl(i%) = 60.01 : incl_excl$(i%) = "M"
               i% = i% + 1%

L09390
*         Order Date Present?
            if date$ = " " or date$ = blankdate$ then L09460
               incl_excl(i%) = 86.06 : incl_excl$(i%) = "<" & date$
               i% = i% + 1%
               incl_excl(i%) = 92.06 : incl_excl$(i%) = "<999999>" & date$
               i% = i% + 1%

L09460
*         Layout Screen / Report Line Map
            hdr$(1%) = "  Contract ID           Type Item                ~
        ~   Start/End  Max$/Qty   $/UOM  Min$/Qty"

            descr_map(01%) = 001.09 : descr_map(02%) =2001   /* Vendor  */
            descr_map(03%) = 010.16 : descr_map(04%) = 001   /* Contract*/
            descr_map(05%) = 026.04 : descr_map(06%) = 018   /* Line    */
            descr_map(07%) = 060.01 : descr_map(08%) = 024   /* Type    */
            descr_map(09%) = 061.21 : descr_map(10%) = 028   /* Item    */
            descr_map(11%) = 086.061: descr_map(12%) = 052   /*StartDate*/
            descr_map(13%) = 092.061: descr_map(14%) =1052   /*End Date */
            descr_map(15%) = 030.30 : descr_map(16%) =1001   /* Descript*/
            descr_map(17%) =-010.30 : descr_map(18%) =2011   /*Vend Name*/
            descr_map(19%) = 142.50 : descr_map(20%) =2042   /*Comment 1*/
            descr_map(21%) = 192.50 : descr_map(22%) =2093   /*Comment 2*/
            descr_map(23%) = 134.08 : descr_map(24%) = 61.094/* Max $   */
            descr_map(25%) = 118.08 : descr_map(26%) =71.0772/* Price   */
            descr_map(27%) = 110.08 : descr_map(28%)=1061.094/* Max Qty */
            descr_map(29%) =  98.04 : descr_map(30%) =1074   /* UOM     */
            descr_map(31%) = 126.08 : descr_map(32%) = 79.094/* Min $   */
            descr_map(33%) = 102.08 : descr_map(34%)=1079.094/* Min Qty */

            inpmessage$ = hex(06) & "Select a Contract"
            if vendor$ > " " then L09730
               if item$ > " " then inpmessage$ = inpmessage$ &           ~
                   " for Item: " & item$
               goto L09780
L09730:     inpmessage$ = inpmessage$ & " from Vendor: " & vendor$
            descr_map(01%), descr_map(2%) = 0
            if item$ > " " then inpmessage$ = inpmessage$ &              ~
                " for this Item: " & item$

L09780:     call "PLOWCODE" (#1, plowkey$, inpmessage$, break%,altkeydef,~
                            f1%(1%), hdr$(), 0,      -1, incl_excl(),    ~
                            incl_excl$(),"D", " ", #2, descr_map())
            if f1%(1%) = 1% then contract$ = str(key(#1,0),,16%)
            if f1%(1%) = 1% then contract_line$ = str(key(#1,0),17%)

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

            f1% = f1%(1%)

            end
