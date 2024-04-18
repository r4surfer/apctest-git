        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M   CCC   PPPP   RRRR   PPPP   TTTTT   *~
            *  B   B  O   O  MM MM  C   C  P   P  R   R  P   P    T     *~
            *  BBBB   O   O  M M M  C      PPPP   RRRR   PPPP     T     *~
            *  B   B  O   O  M   M  C   C  P      R   R  P        T     *~
            *  BBBB    OOO   M   M   CCC   P      R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMCPRPT - BOMCPRPT- Comparison of BOMs Report            *~
            *                                                           *~
            *            Compares the components of two BOMs.           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/13/87 ! Original                                 ! TLJ *~
            * 10/24/90 ! Corrected FAC assignment on input mode.  ! MJB *~
            * 11/20/90 ! Corrected input message truncation       ! MJB *~
            * 03/31/93 ! PRR 12835. Corrected Report ID.          ! JDH *~
            * 07/31/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            abrtrsn$40,                  /* Abort Messages             */~
            assypart$25,                 /* Assembly part number       */~
            assydescr1$32,               /* Assembly part description  */~
            assydescr2$32,               /* Assembly part description  */~
            assypartdescr$32,            /* Assembly part description  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomid$3,                     /* BOM ID                     */~
            bomid1$3,                    /* Enter First BOM            */~
            bomid2$3,                    /* Enter Second BOM           */~
            bomseq1$3,                   /* BOM Sequence               */~
            bomseq2$3,                   /* BOM Sequence for BOM2      */~
            compkey$99,                  /* Comp. Key for Plowing #9   */~
            company$60,                  /* Company Name               */~
            comppn$25,                   /* Component Part #           */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            eff$(490)3,                  /* Effectivity array          */~
            eff_string$21,               /* Display for Eff. Date      */~
            effect1$21,                  /* Display for Eff. Date, BOM1*/~
            effect2$21,                  /* Display for Eff. Date, BOM2*/~
            errormsg$79,                 /* Error message              */~
            found%(1),                   /* Needed for SEARCH call.    */~
            found$8,                     /* Date of BOM in Eff. cal.   */~
            hdr$(3)79,                   /* PLOWCODE Screen Titles     */~
            in_heading1$25,              /* Input Heading 1.           */~
            in_heading2$6,               /* Input Heading 2.           */~
            i$(24)80,                    /* Screen Image               */~
            incl(1),                     /* PLOWCODE Argument          */~
            incl$(1)1,                   /* PLOWCODE Argument          */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            marker1$2,                   /* Marker                     */~
            marker2$2,                   /* Marker for BOM2            */~
            option1$,                    /* Option                     */~
            optdescr1$32,                /* Description of BOMID 1     */~
            optdescr2$32,                /* Description of BOMID 2     */~
            part$29,                     /* Global part for sub. calls */~
            part1$25,                    /* Enter First Part           */~
            part2$25,                    /* Enter Second Part          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pldate$8,                    /* A Date                     */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rtestep1$4,                  /* Route Step                 */~
            rtestep2$4,                  /* Route Step for BOM2        */~
            runtime$8,                   /* Time of day                */~
            specid1$3,                   /* Specific BOM ID            */~
            specid2$3,                   /* Specific BOM ID for BOM 2  */~
            textid1$4,                   /* Text ID                    */~
            textid2$4,                   /* Text ID                    */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! BOMMASTR ! BOM relationship file                    *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! ENGMASTR ! Engineering Master Filer                 *~
            * # 4 ! HNYMASTR ! Inventory File                           *~
            * # 8 ! WORKFILE ! Temporary System Workfile                *~
            * # 9 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select # 2,  "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20

            select # 3, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select # 4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 9, dup

            select # 8, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =  1,   keylen = 3                        ~

            select # 9, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =  1,   keylen =  3,                      ~
                        alternate key 1, keypos = 4, keylen = 25,   dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))

            if fs%( 1) = 1% and fs%( 2) = 1% then goto L09000
                if fs%(1) <> 1%                                          ~
                       then abrtrsn$ = "BOM Master file not found."      ~
                       else abrtrsn$ = "Inventory Master file not found."
               call "ASKUSER" ( 0%, "E R R O R  I N  B O M C P R P T",   ~
                                        abrtrsn$,                        ~
                                        " ",                             ~
                                        "---PRESS RETURN TO EXIT---"  )
               goto exit_program

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  call "DATEFMT" (date$)

*        Init. Today's and the Base date of the planning calender
            call "READ100" (#2, "MONTHS OPEN", f1%(2))

            if f1%(2) = 0%  /* If error reading the planning calender */ ~
            then pldate$ = blankdate$ /* use PLDATE$ as a no date flag  */ ~
            else get #2 using L09140, pldate$
L09140:                 FMT XX(32), CH(6)

            call "PIPINDEX" (#2, " ", today%, err%)

*        Initialize the Input Screen Messages and Headings.
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            in_heading1$ = "ASSEMBLY"
            in_heading2$ = "BOM ID"

            str(line2$,62) = "BOMCPRPT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%,0%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 1%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_rpt
                  if keyhit% <>  0% then       editpg1
*        IF on the first input line or before
            if cursor%(1) > 6 then goto L11170
                if cursor%(2) < 43% then fieldnr% = 1%                   ~
                                    else fieldnr% = 2%
                goto L11200
L11170
*        Else if on the second input line or prior
                if cursor%(2) < 43% then fieldnr% = 3%                   ~
                                    else fieldnr% = 4%
L11200:     if fieldnr% < 1% or fieldnr% >  4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11240:     gosub'101(fieldnr%, 1%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11240
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11240
                  if fieldnr% <> 1% and fieldnr% <> 3% then goto L11290
                     fieldnr% = fieldnr% + 1% : goto L11240
L11290:           lastfieldnr% = fieldnr%
            goto L11200

        REM *************************************************************~
            *             G E N E R A T E   R E P O R T                 *~
            *-----------------------------------------------------------*~
            * Calls TRANS_DAT to create workfiles for the two BOM's to  *~
            * be compared.                                              *~
            *************************************************************

        generate_rpt

            call "SHOSTAT"  ( "GENERATING THE BOM COMPARISON REPORT." )

*        Open the workfiles to be used when generating the report.
            call "WORKOPEN" ( #8, "IO", 300%, f2%(8%) )
            call "WORKOPEN" ( #9, "IO", 300%, f2%(9%) )

*        Obtain header information for both BOMs.
            call "DESCRIBE" ( #4%, part1$, assydescr1$, 0%, f1%(4) )
            call "DESCRIBE" ( #4%, part2$, assydescr2$, 0%, f1%(4) )

            readkey$ = str(part1$,,25) &  str(bomid1$,,3) & "  0"
            call "DESCRIBE" ( #1%, readkey$, optdescr1$, 0%, f1%(1) )

            call "READ100" ( #1%, readkey$, f1%(1%) )
            if f1%(1) = 1% then L12250
                ask% = 0%
                call "ASKUSER" ( ask%, "ERROR READING FROM BOMMASTR ",   ~
                               " ", "Press RETURN to Exit Program and ", ~
                               "Resolve the Problem " )
                goto exit_program

L12250:     get #1%, using L12260, rteid1$
L12260:         FMT POS(87), CH(3)

            readkey$ = str(part2$,,25) &  str(bomid2$,,3) & "  0"
            call "DESCRIBE" ( #1%, readkey$, optdescr2$, 0%, f1%(1) )

            call "READ100" ( #1%, readkey$, f1%(1%) )
            if f1%(1) = 1% then L12320
                ask% = 0%
                call "ASKUSER" ( ask%, "ERROR READING FROM BOMMASTR ",   ~
                               " ", "Press RETURN to Exit Program and " &~
                               "Resolve the Problem." )
                goto exit_program

L12320:     get #1%, using L12330, rteid2$
L12330:              FMT POS(87), CH(3)

*         Load two Workfiles with the BOMs to be compared

            to_file% = 8%
            readkey$ = str(part1$,,25) & str(bomid1$,,3) & "  0"
            gosub trans_dat

            to_file% = 9%
            readkey$ = str(part2$,,25) & str(bomid2$,,3) & "  0"
            gosub trans_dat

            part$ = str(part1$,,25) & "1001"
            bom$ = bomid1$
            gosub get_effect
            effect1$ = eff_string$

            part$ = str(part2$,,25) & "1001"
            bom$ = bomid2$
            gosub get_effect
            effect2$ = eff_string$

*         Compare the two Workfiles and Generate the report.
            runtime$ = " "
            call "TIME" (runtime$)
            call "COMPNAME" ( 12%, company$, u3% )
            page% = 99%
            num_lines% = 99%
            select printer(134)
            call "SETPRNT" ( "BOM004", " ", 0%, 0% )
            offset% = 0%

*        Compare all component parts in BOM1 with parts in BOM2
           readkey$ = hex (00)
           plow_bom1                    /* Loop While not EOF for BOM1 */
              if num_lines% > 54% then gosub print_headings
              call "PLOWNEXT" ( #8%, readkey$, 0%, f1%(8%) )
              if f1%(8%) = 0% then L12800

              get #8% using L35210, bomseq1$, comppn$, bomqty1, bomused1, ~
                                 fixedqty1, overage1, marker1$, option1$,~
                                 specid1$, textid1$, rtestep1$

              qtyreq1 = (( bomqty1 * bomused1 ) + overage1 ) + fixedqty1
              compkey$ = str (comppn$,,25) & hex (00)
              call "REDALT1" ( #9%, compkey$, 1%, f1%(9) )
              if f1%(9) = 0 then gosub rec_not_found                     ~
              else gosub rec_found
              num_lines% = num_lines% + 1%
           goto plow_bom1

L12800:    print
           readkey$ = hex (00)
           plow_bom2                    /* Loop While not EOF for BOM2 */
              if num_lines% > 54% then gosub print_headings
              call "PLOWNEXT" ( #9%, readkey$, 0%, f1%(9%) )
              if f1%(9%) = 0% then goto L12990

              get #9% using L35210, bomseq2$, comppn$, bomqty2, bomused2, ~
                                 fixedqty2, overage2, marker2$, option2$,~
                                 specid2$, textid2$, rtestep2$

              qtyreq2 = (( bomqty2 * bomused2 ) + overage2 ) + fixedqty2
              if option2$ = " " then option2$ = "N"
              print using L14310, "   ", comppn$, "NO", "YES", bomseq2$,  ~
                                 " ", "       ", qtyreq2," ", "  ",      ~
                                 marker2$, " ", option2$, "   ",         ~
                                 specid2$, "    ", rtestep2$
              num_lines% = num_lines% + 1
           goto plow_bom2

L12990:    gosub end_of_report
           goto inputmode                      /* Report is COMPLETE */

        rec_not_found
           if option1$ = " " then option1$ = "N"
           print using L14310, readkey$, comppn$, "YES", "NO", "   ", " ",~
                              qtyreq1, "       ", " ", marker1$, "  ",   ~
                              option1$, " ", specid1$, "   ", rtestep1$
        return

        rec_found
           get #9% using L35210, bomseq2$,  comppn$, bomqty2, bomused2,   ~
                                fixedqty2, overage2, marker2$, option2$, ~
                                specid2$,  textid2$, rtestep2$

           convert bomseq2$ to seq2%
           convert bomseq1$ to seq1%
           if ( seq2% + offset% ) = seq1% then goto L13190
              sstar$ = "*"
              offset% = seq1% - seq2%
              goto L13210
L13190:    sstar$ = " "

L13210:    qtyreq2 = (( bomqty2 * bomused2 ) + overage2 ) + fixedqty2
           if qtyreq2 = qtyreq1 and ( bomqty2 <> bomqty1 or              ~
                                      bomused2 <> bomused1 or            ~
                                      fixedqty2 <> fixedqty1 )           ~
           then qstar$ = "+"                                             ~
           else qstar$ = " "
           if option1$ = " " then option1$ = "N"
           if option2$ = " " then option2$ = "N"
           print using L14310, readkey$, comppn$, "YES",  "YES",          ~
                            bomseq2$, sstar$, qtyreq1, qtyreq2, qstar$,  ~
                            marker1$, marker2$, option1$, option2$,      ~
                            specid1$, specid2$, rtestep1$, rtestep2$
           delete #9%
        return

        print_headings
           if page% = 99% then page% = 0
           if page% <> 0 then gosub print_footers
           page% = page% + 1
           print page
           print using L14040, date$, runtime$, company$
           print using L14070, page%
           print skip(2)
           print using L14100
           print using L14130
           print using L14160, 1%, part1$, bomid1$, assydescr1$,          ~
                                  optdescr1$, rteid1$, effect1$

           print using L14160, 2%, part2$, bomid2$, assydescr2$,          ~
                                  optdescr2$, rteid2$, effect2$
           print skip (2)
           print using L14190
           print using L14220
           print using L14250
           print using L14280

           num_lines% = 15%
        return

        print_footers
           print skip(2)
           print "* - Denotes that a component has been inserted, deleted~
        ~ or moved."
           print "+ - Denotes that the values used to calculate the equiv~
        ~alent quantities are different."

        return

        end_of_report
            print skip ( 53% - num_lines% )
            gosub print_footers
            print
            runtime$ = " "
            call "TIME" (runtime$)
            print using L14340, runtime$
            close printer
            call "SETPRNT" ( "BOM004", " ", 0%, 1% )
            call "FILEBGON" (#8%)
            call "FILEBGON" (#9%)
        return

        /****************************************************************/
        /*              IMAGE STATEMENTS FOR THE REPORT                 */
        /****************************************************************/

L14040: %RUN DATE: ######## ########        #############################~
        ~###############################                   BOMCPRPT-BOM004

L14070: %                                             B O M  C O M P A R ~
        ~I S O N  R E P O R T                                     PAGE: ##

L14100: %       ASSEMBLY NUMBER           BOM ID ASSEMBLY DESCRIPTION    ~
        ~       BOM DESCRIPTION                PEG TO EFFECTIVE

L14130: %       ------------------------- ------ ------------------------~
        ~------ ------------------------------ ------ --------------------

L14160: %BOM#:  ######################### ###    ########################~
        ~###### ############################## ###    ####################

L14190: %SEQ.                                                 SEQ.       ~
        ~                                         SPECIFIC     PICK BEFORE

L14220: %NUMBER  COMPONENT                  INCLD.   INCLD.   NUMBER     ~
        ~QTY REQUIRED      MARKER     OPTION      BOM ID.      ROUTE STEP

L14250: %BOM1    PART NUMBER                in BOM1  in BOM2  BOM2       ~
        ~BOM1     BOM2     BOM1 BOM2  BOM1 BOM2   BOM1  BOM2   BOM1   BOM2

L14280: %------  -------------------------  -------  -------  ------  ---~
        ~---------------   ---------  ---------   ----------   -----------

L14310: % ###    #########################    ###      ###     ### #  ###~
        ~####  ####### #    ##   ##    #    #     ###   ###    ####   ####

L14340: %    * * *  End of Report  (@ ########)  * * *

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Enter First PART       */~
                              L20170,         /* Enter First BOM ID     */~
                              L20200,         /* Enter Second PART      */~
                              L20240          /* Enter Second BOM ID    */
            return

L20140: REM Def/Enable Enter First PART            PART1$
            return

L20170: REM Def/Enable Enter First BOM ID          BOMID1$
            return

L20200: REM Def/Enable Enter Second PART            PART2$
            part2$ = part1$
            return

L20240: REM Def/Enable Enter Second BOM ID          BOMID2$
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the 1st ASSEMBLY part number. (Leave blank to see existin~
        ~g ASSEMBLIES.)",                                                 ~
         "Enter the 1st BOM ID.  (Leave blank to see existing BOMS)",    ~
         "Enter the 2nd ASSEMBLY part number. (Enter BLANKS to see existi~
        ~ng ASSEMBLIES.)",                                                ~
         "Enter the 2nd BOM ID.  (Leave blank to see existing BOMS)"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      part1$, part2$, bomid1$, bomid2$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *         TRANSFER  DATA FROM A FILE TO A WORK FILE         *~
            *-----------------------------------------------------------*~
            * General subroutine to load wokfiles with corresponding    *~
            * components.  Called twice once for each BOM.              *~
            *************************************************************
        trans_dat
            transfer_loop                       /* Loop while not EOF */
                call "PLOWNEXT" (#1%, readkey$, 28%, f1%(1%))
                if f1%(1%) = 0% then goto L30191     /* EOF? then DONE */

                get #1% using L35060, comppn$, assypart$, bomid$,         ~
                     bomseq$, bomqty, bomused, fixedqty, overage,        ~
                     marker$, option$, specid$, textid$, rtestep$

                write #to_file%, using L35210, bomseq$,                   ~
                     comppn$, bomqty, bomused, fixedqty, overage,        ~
                     marker$, option$, specid$, textid$, rtestep$

            goto transfer_loop
L30191: return

        get_effect
           call "READ100" (#3, part$, f1%(3%))

*        if unable to read ENGMASTR or to obtain planning date.
           if f1%(3) = 1% and pldate$ <> blankdate$ then goto L30271
              eff_string$ = " "       /*   clear effectivity date   */
              goto L30500              /*   and return.              */

L30271
*        else obtain the effectivity date.
           found%(1) = 0%
           get #3 using L30520, eff$()
           eff$() = str( eff$(), (today%-1%)*3% + 1%, )
           search eff$() = str(bom$,,3) to found%() step 3

*        If BOM is not effective in the planning calender
           if found%(1) <> 0% then goto L30370
              eff_string$ = "NOT SET"
              goto L30500
*        If BOM is effective in the plannning calender today
L30370:    if found%(1) <> 1% then goto L30460
              search eff$() <> str(bom$,,3) to found%() step 3
              if found%(1) = 0% then found%(1) = 490%                    ~
              else found%(1) = ( found%(1) + 2 )/3 + today% - 2%
              found$ = " "
              call "DATE" addr( "G+", str(pldate$,1,6), found%(1)-1%,    ~
                                                    str(found$,1,6),err%)
              call "DATEFMT" ( found$ )
              eff_string$ = "THRU       " & found$
              goto L30500
*        If BOM is effective, at a later date.
L30460:    found%(1) = ( found%(1) + 2 )/3 + today% - 2%
           found$ = " "
           call "DATE" addr( "G+", str(pldate$,1,6), found%(1),          ~
                                                    str(found$,1,6),err%)
           call "DATEFMT" ( found$ )
           eff_string$ = "BEGINNING  " & found$
L30500: return

L30520: FMT XX(29), 490*CH(3)

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: BOMMASTR                          */~
            CH(25),         /* component part number                   */~
            CH(25),         /* assembly part number                    */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* bill of materials seq. no.              */~
            PD(14,4),       /* quantity required of comp. for asse.    */~
            PD(14,4),       /* times used (size)                       */~
            PD(14,4),       /* Added This Quantity Independent Of Paren*/~
            PD(14,4),       /* Allowed Overage Quantity                */~
            CH(2),          /* BOM marker for STandard, PHantom, Option*/~
            CH(1),          /* Generic option field                    */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(4)           /* RTE Step To Identify A Route Line       */

L35210: FMT                                                              ~
            CH(3),          /* bill of materials seq. no.              */~
            CH(25),         /* component part number                   */~
            PD(14,4),       /* quantity required of comp. for asse.    */~
            PD(14,4),       /* times used (size)                       */~
            PD(14,4),       /* Added This Quantity Independent Of Paren*/~
            PD(14,4),       /* Allowed Overage Quantity                */~
            CH(2),          /* BOM marker for STandard, PHantom, Option*/~
            CH(1),          /* Generic option field                    */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(4)           /* RTE Step To Identify A Route Line       */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              if edit% = 0% then gosub set_pf_input                      ~
                            else gosub set_pf_edit

              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              on fieldnr% gosub L40270,         /* Enter First PART    */ ~
                                L40270,         /* Enter First BOM ID  */ ~
                                L40270,         /* Enter Second PART   */ ~
                                L40270          /* Enter Second BOM ID */
              goto L40300

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40270:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40300:     accept                                                       ~
               at (01,02),                                               ~
                  "Comparison of BOMs Report",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,19), fac(hex(ac)), in_heading1$           ,        ~
               at (05,49), fac(hex(ac)), in_heading2$           ,        ~
                                                                         ~
               at (06,02), " First BOM:"                        ,        ~
               at (06,19), fac(lfac$( 1)), part1$               , ch(25),~
               at (06,49), fac(lfac$( 2)), bomid1$              , ch(3), ~
                                                                         ~
               at (07,02), "Second BOM:"                        ,        ~
               at (07,19), fac(lfac$( 3)), part2$               , ch(25),~
               at (07,49), fac(lfac$( 4)), bomid2$              , ch(3), ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40590
                  call "MANUAL" ("BOMCPRPT") : goto L40300

L40590:        if keyhit% <> 15 then L40620
                  call "PRNTSCRN" : goto L40300

L40620:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf_input
              pf$(1) = "(1)Start Over                           " &      ~
                       "                       (13)Instructions"
              pf$(2) = "                 (4)Previous Field      " &      ~
                       "                       (15)Print Screen"
              pf$(3) = "                                        " &      ~
                       "                       (16)EXIT PROGRAM"
              pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
              if fieldnr% = 1% and edit% = 0% then L40760
                  str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40760:       if fieldnr% > 1% then L40780
                  str(pf$(2),18,26) = " "  :  str(pfkeys$, 4, 1) = hex(ff)
L40780:       return

         set_pf_edit
              pf$(1) = "(1)Start Over                           " &      ~
                       "                      (13)Instructions "
              pf$(2) = "                                        " &      ~
                       "                      (15)Print Screen "
              pf$(3) = "                                        " &      ~
                       "                      (16)Create Report "
              pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
              if fieldnr% = 0% then L40880
                  str(pf$(3),63)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40880:       return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Enter First PART       */~
                              L50320,         /* Enter First BOM ID     */~
                              L50450,         /* Enter Second Part      */~
                              L50620          /* Enter Second BOM ID    */
            return

L50140: REM Test for Enter First PART             PART1$
            readkey$ = part1$
            if readkey$ = "?" then readkey$ = " "
            hdr$(2)="  Part Assemblies             Part Descriptions"
            hdr$(3) = hex(ac) & "Position cursor (TAB) to a line "       ~
                      &  "and press (RETURN) to choose that PART."
            assypartdescr$ = hex(06) & "   Select a PART:"
            incl(1) = 0
            call "PLOWCODE" (#1, readkey$, assypartdescr$, -8025%,       ~
                             -.32, f1%(1), hdr$(), 3, 75,                ~
                               incl(), incl$(), "Y", "Y", #4%)
            if f1%(1) = 1% then L50270
                errormsg$ = "Unknown Assembly Part: " & part1$
                return
L50270:     part1$ = readkey$
            assypartdescr$ = "(NOT ON FILE)"
            call "DESCRIBE" (#4%, part1$, assypartdescr$, 1%, f1%(4))
            return

L50320: REM TEST DATA FOR SPECIFIC BOM ID 1
            readkey$ = str(part1$,,25) & str(bomid1$,,3)
            if str(readkey$,26%,1%) = "?" then str(readkey$,26%,3%) = " "
            hdr$()= " Listed Below Are The Existing BOMs "               ~
                                              & "For Part: " & part1$
            errormsg$ = hex(06) & "Select a Bill Of Materials."
            call "PLOWCODE" (#1, readkey$, errormsg$, 2025%, .30, f1%(1),~
                                                               hdr$(), 3)
            errormsg$ = " "
                if f1%(1) <> 0% then L50420
                errormsg$ = "ERROR - BOM not on file" : return
L50420:     bomid1$ = str(readkey$,26%,3%)
            return

L50450: REM Test for Enter Second PART            PART2$
                readkey$ = part2$
                if readkey$ = "?" then readkey$ = " "
                hdr$(2)="  Part Assemblies             Part Descriptions"
                hdr$(3) = hex(ac) & "Position cursor (TAB) to a line "   ~
                  &  "and press (RETURN) to choose that PART."
                assypartdescr$ = hex(06) & "   Select a PART:"
                incl(1) = 0
                call "PLOWCODE" (#1, readkey$, assypartdescr$, -8025%,   ~
                       -.32,f1%(1),hdr$(),3,75,incl(),incl$(),"Y","Y",#4%)
                     if f1%(1) = 0 then L50590
                part2$ = readkey$
                assypartdescr$ = "(NOT ON FILE)"
                call "DESCRIBE" (#4%, part2$, assypartdescr$, 1%, f1%(4))
                return
L50590:         errormsg$ = "ERROR - Unknown Assembly Part: " & part2$
            return

L50620: REM TEST DATA FOR SPECIFIC BOM ID 2
            readkey$ = str(part2$,,25) & str(bomid2$,,3)
            if str(readkey$,26%,1%) = "?" then str(readkey$,26%,3%) = " "
            hdr$()="  Listed Below Are The Existing BOMs For Part: " &   ~
                                                                part2$
            errormsg$ = hex(06) & "Select a Bill Of Materials."
            call "PLOWCODE" (#1, readkey$, errormsg$, 2025%, .30, f1%(1),~
                                                               hdr$(), 3)
            errormsg$ = " "
                if f1%(1) <> 0% then L50720
                errormsg$ = "ERROR - BOM not on file" : return
L50720:         bomid2$ = str(readkey$,26%,3%)
                if str (part1$,,25) & str (bomid1$,,3) <>                ~
                   str (part2$,,25) & str (bomid2$,,3) then return
                errormsg$ = "ERROR : Comparing BOM to itself." : return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
