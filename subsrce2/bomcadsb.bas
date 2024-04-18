        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M   CCC    AAA   DDDD    SSS   BBBB    *~
            *  B   B  O   O  MM MM  C   C  A   A  D   D  S      B   B   *~
            *  BBBB   O   O  M M M  C      AAAAA  D   D   SSS   BBBB    *~
            *  B   B  O   O  M   M  C   C  A   A  D   D      S  B   B   *~
            *  BBBB    OOO   M   M   CCC   A   A  DDDD    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMCADSB - This program extracts single level CMS BOM info*~
            *            to an ASCII Attribute File for later importing *~
            *            into a CAD drawing.                            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/25/93 ! Original                                 ! RJH *~
            * 09/26/97 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BOMCADSB" (#01,           /* HNYMASTR                     */~
                        #02,           /* BOMMASTR                     */~
                        #03,           /* BOMCADAT  BOM Attribute File */~
                        #04,           /* BOMCADXR  Xref File          */~
                        assypart$,     /* Assembly Part                */~
                        bomid$,        /* BOM Id                       */~
                        draw_name$,    /* Drawing name                 */~
                        draw_rev$,     /* Drawing Revision Number      */~
                        draw_file$,    /* Drawing File Name            */~
                        draw_loc$,     /* Drawing Location             */~
                        err%)          /* Code = 0% - All OK           */~
                                       /* Code = 1% - No hit HNYMASTR  */~
                                       /* Code = 2% - No hit BOMMASTR  */~
                                   /* Code = 3% - Cant Write BOMCADAT  */~
                                   /* Code = 4% - Cant Write BOMCADXR  */
        dim                                                              ~
            autoflag$1,                  /* Auto Replace Flag          */~
            assycat$4,                   /* Assembly Part Category     */~
            assypart$25,                 /* Assembly Part Number       */~
            assydescr$32,                /* Assembly Part Description  */~
            assyroute$3,                 /* Assembly Part Route        */~
            assytextid$10,               /* Assembly Part BOM Text ID  */~
            assytype$3,                  /* Assembly Part Type         */~
            batchqty$14,                 /* Batch Quantitiy            */~
            bomid$3,                     /* BOM Identifier             */~
            bommkr$2,                    /* BOM Marker                 */~
            bomdescr$30,                 /* BOM Description            */~
            compcat$4,                   /* Component Part Category    */~
            comppart$25,                 /* Component Part Number      */~
            compdescr$32,                /* Component Part Description */~
            comptype$3,                  /* Component Part Type        */~
            compuom$4,                   /* Component Part Unit of Meas*/~
            count$4,                     /* Count of Components in BOM */~
            date$8,                      /* Date for screen display    */~
            dateunfmt$6,                 /* Unformated system date     */~
            datetime$14,                 /* Date- Time Stamp           */~
            draw_file$8,                 /*        Drawing File        */~
            draw_loc$30,                 /*            Location        */~
            draw_name$25,                /* Target Drawing Name        */~
            draw_rev$2,                  /* Imputed  Revision #        */~
            draw_rev_f$2,                /* File     Revision #        */~
            dsize$1,                     /* Drawing Size               */~
            fixed_qty$14,                /* Fixed Quantity             */~
            over_qty$14,                 /* Overage Quantity           */~
            op_flag$1,                   /* Option Part Flag           */~
            peg$3,                       /* Component Specified BOM    */~
            phantom$1,                   /* Phantom Part Flag          */~
            plowkey$70,                  /* KEY TO PLOW FILE WITH      */~
            readkey$70,                  /* KEY TO PLOW FILE WITH      */~
            req_qty$14,                  /* Quantity Required          */~
            seqnum$3,                    /* BOM Sequence Number (line) */~
            step$4,                      /* BOM Pick-before Route Step */~
            times_qty$14,                /* Times Used                 */~
            udate$8,                     /* Temp Date Variable         */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            call "DATEFMT" ( date, 0%, udate$ )
            udate$ = str(udate$,3%,6%)
            dateunfmt$ = udate$
            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *       G E T   S E T   U P                                 *~
            *-----------------------------------------------------------*~
            * Get descriptions, and Assembly Part info.                 *~
            * Test to see if they slipped us any bad data and if so,    *~
            * set RET% and blow out.                                    *~
            *************************************************************

            call "READ100" (#01, assypart$, f1%(01%))
            if f1%(01%) <> 0% then L10120
                err% = 1%
                goto exit_program
L10120:     get #01, using L10130, assydescr$, assycat$, assytype$
L10130:         FMT POS(26), CH(32), POS(90), CH(4), POS(180), CH(3)

            readkey$ = str(assypart$) & str(bomid$) & "  0"
            call "READ100" (#02, readkey$, f1%(02%))
            if f1%(02%) <> 0% then L10190
                err% = 2%
                goto exit_program
L10190:     get #02 using L10200, bomdescr$
L10200:         FMT POS(57), CH(30)

            goto extract_bom

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

*       STARTOVER
*          U3% = 2%
*          CALL "STARTOVR" (U3%)
*          IF U3% = 1% THEN RETURN
*          RETURN CLEAR ALL
*          GOTO EXIT_PROGRAM

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        extract_bom
            call "SHOSTAT" ("Extracting BOM Data")

            plowkey$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
            call "READ100" (#2, plowkey$, f1%(2%))
                if f1%(2%) <> 0% then L30130
                err% = 2%  :  goto exit_program
L30130:     get #2, using L35030, bomdescr$, assyroute$, tempbi%,         ~
                                 dsize$, draw_rev_f$, phantom$, batchqty

            /* Format Variables */
            call "CONVERT" (batchqty, 4.4, batchqty$)
            convert tempbi% to assytextid$, pic(-#########)

           /* Loop thru BOMMASTR to get Components */
            count% = 0%
L30260:     call "PLOWNEXT" (#2, plowkey$, 28%, f1%(2%))
                 if f1%(2%) = 0% then rewrite_bom_header
            get    #2, using L35200, comppart$, seqnum$, req_qty,         ~
                             times_qty, fixed_qty, over_qty, bommkr$,    ~
                             op_flag$, peg$, step$, autoflag$
            count% = count% + 1%

            /* Get HNYMASTR Data for the Component */
            compdescr$, compcat$, comptype$ = " "
            call "READ100" (#01, comppart$, f1%(01%))
                 if f1%(01%) = 0% then L30360        /* Shouldn't happen */
            get #01, using L30345, compdescr$, compuom$,compcat$, comptype$
L30345:         FMT POS(26), CH(32), POS(74), CH(4), POS(90), CH(4),     ~
                    POS(180), CH(3)

L30360:     /* Convert Component Variables */
            call "CONVERT" (req_qty  , 2.4, req_qty$  )
            call "CONVERT" (times_qty, 2.4, times_qty$)
            call "CONVERT" (fixed_qty, 2.4, fixed_qty$)
            call "CONVERT" (over_qty , 2.4, over_qty$ )

            gosub write_bom_component
            goto L30260    /* Loop for next Component */


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        write_bom_header
            readkey$ = "C" &  str(assypart$) & str(bomid$) &             ~
                                str(draw_name$) & str(draw_rev$) & "  0"
            call "READ101" (#03, readkey$, f1%(03%))

            put #03, using L35500, readkey$, " "   , bomdescr$,           ~
                                  assyroute$, assytextid$, dsize$,       ~
                                  " ", phantom$, batchqty$, " ",         ~
                                  assydescr$, assycat$, assytype$,       ~
                                  draw_file$, " "

            if f1%(03%) = 0% then write #03 else rewrite #03
            return

        write_bom_component
            if count% = 1% then gosub write_bom_header    /* 1st Time */
            readkey$ = "C" & str(assypart$) & str(bomid$) &              ~
                              str(draw_name$) & str(draw_rev$) & seqnum$
            call "READ101" (#03, readkey$, f1%(03%))

            put   #03, using L35680, readkey$, comppart$, req_qty$,       ~
                             times_qty$, fixed_qty$, over_qty$, bommkr$, ~
                             op_flag$, peg$, step$, autoflag$, compuom$, ~
                             " ", compdescr$, compcat$, comptype$, " "


            if f1%(03%) = 0% then write #03 else rewrite #03

            return

        write_bom_xref

L31315
*          CALL "GETDTTM" ADDR(DATETIME$)
            datetime$ = udate$ & time

            put  #04 using L35900, "C", assypart$, bomid$, draw_name$,    ~
                                  draw_rev$, datetime$, draw_file$,      ~
                                  draw_loc$, "0", dateunfmt$, " ", " "

            write #04  , eod goto L31315

            err% = 0%
            goto exit_program

        rewrite_bom_header
            convert count% to count$, pic(0000)
            readkey$ = "C" &  str(assypart$) & str(bomid$) &             ~
                                str(draw_name$) & str(draw_rev$) & "  0"
            call "READ101" (#03, readkey$, f1%(03%))

            put #03, using L31480, count$

L31480:  FMT POS(60), CH(4)

            rewrite #03
            goto write_bom_xref


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: BOMMASTR Header Level             */~
               POS(57),                  /* Key Area                   */~
                CH(30),                  /* DESCRIPTION                */~
                CH(3),                   /* WHICH ROUTE                */~
                BI(4),                   /* TEXTID Header              */~
                CH(1),                   /* DRAWING Paper Size         */~
                CH(2),                   /* DRAWING REVISION LEVEL     */~
                XX(9),                   /* CREATE FOLLOWED BY MOD DATE*/~
                                         /* LAST USERS ID              */~
                CH(1),                   /* PHANTOM ASSY FLAG          */~
                PD(14,4)                 /* BATCH QUANTITY             */

L35200: FMT                 /* FILE: BOMMASTR  Component Level         */~
            CH(25),         /* component part number                   */~
            POS(54),        /* assembly part number                    */~
                            /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* Bill of Materials seq. no.              */~
            PD(14,4),       /* Quantity Required of Comp               */~
            PD(14,4),       /* Times Used (size)                       */~
            PD(14,4),       /* Added This Quantity Independent Of Paren*/~
            PD(14,4),       /* Allowed Overage Quantity                */~
            CH(2),          /* BOM Marker                              */~
            CH(1),          /* Generic option field                    */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            XX(4),          /* Internal ID to text in TXTFILE          */~
            CH(4),          /* RTE Step To Identify A Route Line       */~
            XX(3),          /* Type of Part in HNYMASTR file           */~
            CH( 1),         /* Auto Replace Flag                       */~
            CH(41)          /* Unused Space                            */

L35500: FMT                 /* FILE: BOMCADAT Header Level             */~
                CH(59),                  /* Source Flag ('D' or 'C')   */~
                                         /* Assembly Part Number       */~
                                         /* Specified BOM Id           */~
                                         /* Drawing Number             */~
                                         /* Drawing Revision Level     */~
                                         /* BOM Sequence Number (0)    */~
                CH( 4),                  /* Number of Components in BOM*/~
                CH(30),                  /* BOM Description            */~
                CH( 3),                  /* Route for this BOM         */~
                CH(10),                  /* TEXTID Header              */~
                CH( 1),                  /* Drawing Size               */~
                CH( 9),                  /* FILLER                     */~
                CH( 1),                  /* PHANTOM ASSY FLAG          */~
                CH(14),                  /* BATCH QUANTITY             */~
                CH(30),                  /* FILLER                     */~
                CH(32),                  /* Part Description           */~
                CH( 4),                  /* Part Category              */~
                CH( 3),                  /* Part Type                  */~
                CH( 8),                  /* Drawing File Name          */~
                CH(42)                   /* FILLER                     */

L35680: FMT                 /* FILE: BOMCADAT  Component Level         */~
            CH(59),         /* Source Flag ('D' or 'C')                */~
                            /* Assembly Part Number                    */~
                            /* Specified BOM Id                        */~
                            /* Drawing Number                          */~
                            /* Drawing Revision Level                  */~
                            /* BOM Sequence Number (NNN)               */~
            CH(25),         /* Component part number                   */~
            CH(14),         /* Quantity Required of Comp               */~
            CH(14),         /* Times Used (size)                       */~
            CH(14),         /* Added This Quantity Independent Of Paren*/~
            CH(14),         /* Allowed Overage Quantity                */~
            CH( 2),         /* BOM Marker                              */~
            CH( 1),         /* Generic option field                    */~
            CH( 3),         /* Peg-to BOM identifier                   */~
            CH( 4),         /* RTE Step To Identify A Route Line       */~
            CH( 1),         /* Auto Replace Flag                       */~
            CH( 4),         /* Unit of Measeure                        */~
            CH( 6),         /* FILLER                                  */~
            CH(32),         /* Part Description                        */~
            CH( 4),         /* Part Category                           */~
            CH( 3),         /* Part Type                               */~
            CH(50)          /* FILLER                                  */

L35900: FMT                 /* FILE:  BOMCADXR  (Cross Ref)            */~
            CH( 1),         /* Source Flag ('D'rawing or 'C'MS)        */~
            CH(25),         /* Assembly Part Number                    */~
            CH( 3),         /* Specified BOM Id                        */~
            CH(25),         /* Drawing Number                          */~
            CH( 2),         /* Drawing Revision Level                  */~
            CH(14),         /* Date-Time Stamp                         */~
            CH( 8),         /* Drawing File Name                       */~
            CH(30),         /* Drawing File Location                   */~
            CH( 1),         /* Results 0-No BOM,1-BOM w/prob,2-OK BOM  */~
            CH( 6),         /* Extract Date                            */~
            CH( 6),         /* Import Date                             */~
            CH(29)          /* FILLER                                  */



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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
