        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  TTTTT  Y   Y  PPPP   EEEEE   SSS    *~
            *    T     X X     T      T     Y Y   P   P  E      S       *~
            *    T      X      T      T      Y    PPPP   EEEE    SSS    *~
            *    T     X X     T      T      Y    P      E          S   *~
            *    T    X   X    T      T      Y    P      EEEEE   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTTYPES - For the hardcoded list of system text sources. *~
            *            This program allows user to specify how many   *~
            *            types of text there are per source, and what   *~
            *            they are to be called.                         *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/10/85 ! ORIGINAL                                 ! HES *~
            * 08/01/86 ! Mod to recognize multiple types at create! MJB *~
            * 10/20/86 ! Modified for allowing user defined print ! ERN *~
            *          !   print flags; added some sources        !     *~
            * 02/23/87 ! Added Source 020 and 021                 ! MJB *~
            * 04/02/87 ! Added Source 023                         ! JRH *~
            * 08/03/87 ! Corrected short 'T' rec write            ! MJB *~
            * 09/13/88 ! Added source 024                         ! JIM *~
            * 09/13/88 ! Added source AAB for WRA Mailing List.   ! JIM *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.  If O.S. !     *~
            *          !   version cannot be converted, assume non!     *~
            *          !   WANG platform and ask, otherwise 28.   !     *~
            *          !   Cannot be changed here once file exists!     *~
            * 11/11/92 ! Added source 025- Core Tracking X-ref.   ! JIM *~
            * 11/11/92 ! Added source 026-029- Core Bank DRs/CRs. ! JIM *~
            * 11/11/92 ! Added source 030&031- Core Receipts.     ! JIM *~
            * 09/24/93 ! Added source 032&033- Multi-Line Quotes. ! JIM *~
            * 01/28/94 ! Added source 034-036- Eng Chg Requests.  ! LDJ *~
            * 06/15/94 ! Added source 037 for HNYACTXF file.      ! ERN *~
            * 06/16/94 ! Added source 038 for VPCMASTR file.      ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$(21)200,              /* Some Filler                */~
            filler64$64,                 /* Some Filler                */~
            hex0b$1,                     /* LITERAL HEX(0B)            */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            fac$(20,3)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            pgmid$79,                    /* Display for line 2         */~
            txtlines$2, gtprm$2, aid$1,  /* GETPARM STUFF              */~
            osver$6,                     /* O. S. Version              */~
            readkey$20                   /* GEN PURPOSE READKEY        */

        dim                                                              ~
            source$  (99)3,              /* SOURCE CODES               */~
            srcdescr$(99)25,             /* SOURCE DESCRIPTION         */~
            typsz%(99,10),               /* Type Text Length           */~
            typedescr$(99,10)25,         /* Type Descriptions          */~
            sourceheader$79              /* SOURCES SCREEN HEADER      */~

        dim                                                              ~
            typsrc$3,                                                    ~
            typsrcdescr$25,                                              ~
            typeseq$1,                                                   ~
            typeseq$(10)1,                                               ~
            typdescr$(10)25,                                             ~
            twidth$(10)2,                                                ~
            typescrheader$40                                             ~

        dim                                                              ~
            prtdescr$30,                                                 ~
            prtsrcdescr$30,                                              ~
            prtsrctitle$128,                                             ~
            prttypdescr$128

        dim f2%(16),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(16),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(16)20                  /* TEXT FROM FILE OPENING     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE F2%() SHOULD NOT BE MODIFIED.  IT */
                     /* IS AN INTRINSIC PART OF THE THE FILE OPENING   */
                     /* ROUTINES.                                      */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! TXTFILE  ! System Free Text File                    *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  11

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%, f2%( 1), 0%, rslt$( 1))
               if fs% > 0% then L02410

            txtlines$ = "28" : gtprm$= "I " : aid$ = hex(40)
            call "EXTRACT" addr("S#", osver$)
            convert str(osver$,,6) to textsize%, data goto L02270
              gtprm$= "ID"
L02270:     call "GETPARM" addr (gtprm$, "R", "TEXTSIZE", aid$, "0001",  ~
                                     "TXTTYP",                           ~
         "Enter Number of Text Lines per Record in Text File (1 - 28)  ",~
                                     61%, "K", "TXTLINES", txtlines$, 2%,~
                                     5%, 32%, "I")
            gtprm$ = "R "
            convert txtlines$ to textsize%, data goto L02270
            if textsize% < 1% or textsize% > 28% then L02270
               textsize% = textsize% * 70%
               textsize% = textsize% + 64%
            txtlines$ = bin(textsize%,2)
            call "PUTUFBRS" addr(#1, txtlines$)

            call "OPENCHCK" (#1, fs%, f2%( 1), 200%, rslt$( 1))
               if fs% < 0% then end /* Something Seriously Wrong */
L02410:     call "GETUFBRS" addr(#1, txtlines$)
            textsize% = val(txtlines$,2)
            textsize% = textsize% - 64%

        REM *************************************************************~
            * Each pair of DATA statements must be formatted as follows *~
            *                                                           *~
            * 1 - DATA    CH(03)    Source ID                           *~
            *             CH(25)    Source Description                  *~
            *                                                           *~
            * 2 - DATA    CH(25)    Type Description                    *~
            *             m%        Text Length fo Type                 *~
            *                                                           *~
            *   The last two fields may be repeated up to nine times.   *~
            *   If less than ten types are defined, you must include a  *~
            *     single " " as the last data element.                  *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

*        Set up Descriptors and Misc. Variables
            date$ = date
            call "DATEFMT" (date$)
            str(pgmid$,61,18) = "TXTTYPES: " & str(cms2v$,,8)
            hex0b$ = hex(0b)

            prtsrctitle$ = "SYSTEM TEXT SOURCES"
               call "FMTTITLE" (prtsrctitle$, " ",  2%)
               str(prtsrctitle$,3,14) = "DATE: " & date$
               str(prtsrctitle$,119,5) = "PAGE:"

            prttypdescr$ = "SYSTEM TEXT TYPES FOR SOURCE"
               call "FMTTITLE" (prttypdescr$, " ",  2%)
               str(prttypdescr$,3,14) = "DATE: " & date$
               str(prttypdescr$,119,5) = "PAGE:"

            sourceheader$ = "            Sources"

            typescrheader$ =                                             ~
                      "Type Description              Line Width"


*        Define Arrays Describing Sources
*        NOTE: Numeric Sources are reserved for use in CMS Generic;
*              Alpha Sources may be used for user Customizations
            restore line = L08440

            for i% = 1% to dim(source$(),1)
                read source$(i%), srcdescr$(i%)
                if source$(i%) = " " then L09580     /* End of Data      */
                     srcmax% = srcmax% + 1%
                     for j% = 1% to 10%         /* Get supplied Types  */
                          read typedescr$(i%,j%)
                          if typedescr$(i%,j%) = " " then L08420
                               read typsz%(i%,j%)
                     next j%
L08420:     next i%

L08440:     data "001", "Parts Master File        ",                     ~
                        "Part Text                ", 70%, " "
            data "002", "Vendor Master File       ",                     ~
                        "Vendor Master Text       ", 70%, " "
            data "003", "Purchase Orders- Headers ",                     ~
                        "P.O. Header Text         ", 70%, " "
            data "004", "Purchase Orders, Lines   ",                     ~
                        "P.O. Line Item Text      ", 70%, " "
            data "005", "Purchase Orders, Receipts",                     ~
                        "Receiver Text            ", 70%, " "
            data "006", "Quality Control          ",                     ~
                        "Quality Control Text     ", 70%, " "
            data "007", "Routing Header           ",                     ~
                        "General Header Text      ", 65%,                ~
                        "Procedural Header Text   ", 65%, " "
            data "008", "Routing Step             ",                     ~
                        "Routing Step Text        ", 65%, " "
            data "009", "Bill of Materials Header ",                     ~
                        "General Header Text      ", 65%,                ~
                        "Procedural Header Text   ", 65%, " "
            data "010", "BOM Component            ",                     ~
                        "BOM Component Text       ", 65%, " "
            data "011", "A/P Invoice              ",                     ~
                        "A/P Invoice Free Text    ", 70%, " "
            data "012", "Customer Master          ",                     ~
                        "Customer Master Text     ", 70%, " "
            data "013", "Sales Orders- Headers    ",                     ~
                        "Sales Order Header Text  ", 70%, " "
            data "014", "Sales Orders- Line Items ",                     ~
                        "Sales Order Line Text    ", 70%, " "
            data "015", "A/R Invoices- Headers    ",                     ~
                        "A/R Invoice Header Text  ", 70%, " "
            data "016", "A/R Invoices- Line Items ",                     ~
                        "A/R Invoice Line Text    ", 70%, " "
            data "017", "Estimates                ",                     ~
                        "Estimate Free Text       ", 70%, " "
            data "018", "A/R Late Notices         ",                     ~
                        "Late Notices-Don't Delete", 70%, " "
            data "019", "Inventory Cost/Qty Summ. ",                     ~
                        "Inventory Summ. Free Text", 70%, " "
            data "020", "Serial Number Text       ",                     ~
                        "Part - Serial Number Text", 70%, " "
            data "021", "Job Text                 ",                     ~
                        "Job Free Text            ", 70%, " "
            data "022", "Cost Set Text            ",                     ~
                        "Cost Set Free Text       ", 70%, " "
            data "023", "Part Standard Cost       ",                     ~
                        "Part Std Cst Free Text   ", 70%, " "
            data "024", "Vendor Part Catalog      ",                     ~
                        "Vendor & Part Free Text  ", 70%, " "
            data "025", "Core Tracking Cross-Ref  ",                     ~
                        "Core Tracking X-Ref Text ", 70%, " "
            data "026", "Core Bank Debit Master   ",                     ~
                        "Core Bank DB Master Text ", 70%, " "
            data "027", "Core Bank Line Items     ",                     ~
                        "Core Bank Line Item Text ", 70%, " "
            data "028", "Core Bank Credit Master  ",                     ~
                        "Core Bank CR Master Text ", 70%, " "
            data "030", "Core Rec Hold Master File",                     ~
                        "Core Rec Hold Master Text", 70%, " "
            data "031", "Core Rec Hold Line Items ",                     ~
                        "Core Rec Hold Ln Itm Text", 70%, " "
            data "032", "Multi-Line Quote Masters ",                     ~
                        "M/L Quote Master Text    ", 70%, " "
            data "033", "Multi-Line Quote Ln Items",                     ~
                        "M/L Quote Line Item Text ", 70%, " "
            data "034", "ECR Master Description   ",                     ~
                        "ECR Description Text     ", 40%, " "
            data "035", "ECR Master Disposition   ",                     ~
                        "ECR Final Disposition Txt", 40%, " "
            data "036", "ECR Detail Approval Text ",                     ~
                        "Approval / Denial Text   ", 40%, " "
            data "037", "Part/Activity X-Ref      ",                     ~
                        "Part/Activity X-ref Text ", 70%, " "
            data "038", "Purchasing Contracts     ",                     ~
                        "Purchasing Contracts Text", 70%, " "
*        Above is a convenient line break for future insertions.
            data "AAB", "Mailing List Master      ",                     ~
                        "Mailing List Text        ", 70%, " "
            data "ZZZ", "Miscellaneous            ",                     ~
                        "Miscellaneous Text       ", 70%, " "
            data "   ", " "    /* MUST be last Data Statement!!!!      */

*          **************************************************************

L09580:     init(" ") errormsg$ : ml% = 0%
            call "CEXIT" addr ("S"," ","N")  /*Disable help key for sec*/
            readkey$ = "S"
            call "DELETE" (#1, readkey$, 1%)

*        Write Out Source List and Types in case they have Changed...
            for s% = 1% to dim(source$(),1)
               if source$(s%) = " " then L09920

               put filler64$ using L09690, "S", source$(s%), " ",          ~
                                          srcdescr$(s%), " "
L09690:            FMT CH(1), CH(3), CH(7), CH(25), CH(28)
               write #1, str(filler64$,,64), str(filler$(),,textsize%)

               readkey$ = "T" & str(source$(s%)) & hex(00)
               call "PLOWNEXT" (#1, readkey$, 4%, f1%(1))
               if f1%(1) <> 0% then L09860

               for t% = 1% to 10%
                   if typedescr$(s%,t%) = " " then goto L09860
                   if t% = 1% then typeseq$ = "1"
                   if t% = 2% then typeseq$ = "A"
                   if t% > 2% then typeseq$ = add hex(01)
                   put filler64$ using L09830, "T", source$(s%), typeseq$, ~
                             " ", typedescr$(s%,t%), typsz%(s%,t%), "  "
L09830:            FMT CH(1), CH(3), CH(1), CH(6), CH(25), BI(1), CH(27)
                   write #1, str(filler64$,,64), str(filler$(),,textsize%)
               next t%
L09860:     next s%

            FMT CH(1), CH(3), CH(7), CH(25), CH(1988)
            FMT CH(1), CH(3), CH(1), CH(6), CH(25), BI(1), CH(1987)


L09920:     call "CEXIT" addr ("C") /* Re-enable help key */

        REM *************************************************************~
            *                                                           *~
            *   SHOW TEXT SOURCES                                       *~
            *                                                           *~
            *************************************************************

        main_display
                gosub show_sources
                      if keyhit%  = 16 then L65000
                      if keyhit%  = 14 then gosub print_list
                      if keyhit%  =  2 then ml% = 0%
                      if keyhit%  =  4 then ml% = max(0%, ml% - 12%)
                      if keyhit%  =  5 then                              ~
                            ml% = max(0%, min(srcmax% - 15%, ml% + 12%))
                      if keyhit%  =  6 then ml% = max(0%, ml% - 1%)
                      if keyhit%  =  7 then                              ~
                            ml% = max(0%, min(srcmax% - 15%, ml% + 1%))
                      if keyhit%  = 12 then L10210
                      if keyhit%  = 11 then L10210
                      if keyhit% <>  0 then main_display

L10210:         fieldnr% = cursor%(1%) - 4%
                if fieldnr% > 15% then main_display
                if fieldnr% > srcmax% - ml% then main_display
                   if keyhit% = 11% then fieldnr% = fieldnr% + 1%
                if fieldnr% < 1% then main_display
                   c% = fieldnr% + ml%

        REM Edit Types...
                typsrc$ = source$(c%)
                source% = c%
                typsrcdescr$ = srcdescr$(c%)
                gosub load_types

        REM *************************************************************~
            *                                                           *~
            *   MAINTAIN TYPES RECORDS                                  *~
            *                                                           *~
            *************************************************************

        types_edit
                gosub'102(0%,0%)
                      errormsg$ = " "
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then datasave
                      if keyhit%  = 12 then L11160
                      if keyhit%  = 11 then type_insert
                      if keyhit% <>  0 then types_edit

L11160:         fieldnr% = cursor%(1%) - 5%
                if fieldnr% > 15% then types_edit
                if fieldnr% > typmax% then types_edit
                if fieldnr% < 1% then types_edit
                   c% = fieldnr%
                if keyhit% = 12% then type_delete

        REM TYPE EDIT
L11240:            gosub'102 (1%, fieldnr%)
                   gosub'152
                     if errormsg$<> " " then L11240
                   goto types_edit

        type_insert
                if typmax% < dim(typeseq$(),1) then L11330
                     errormsg$ = "Sorry, 10 Types Per Source Is Limit"
                     goto types_edit
L11330:         c%, fieldnr% = typmax% + 1%

L11350:         gosub'102 (2%, fieldnr%)
                     if keyhit%  <> 1% then L11410
                        errormsg$ = " "
                        typmax% = typmax% + 1%
                        gosub types_pull_up
                        goto types_edit
L11410:              if keyhit% <> 0% then L11350
                gosub'152
                     if errormsg$ <> " " then L11350
                     typmax% = typmax% + 1%
                     goto types_edit

        type_delete
                if typmax% > 1 then L11510
                     errormsg$ = "At Least One Type Must Stay On File"
                     goto types_edit
L11510:         gosub'102 (3%, fieldnr%)
                     if keyhit% = 1% then types_edit
                     if keyhit% <> 0% then L11510

                gosub types_pull_up
                goto types_edit

        types_pull_up
                if c% = typmax% then L11660
                for i% = c% to typmax% - 1%
                     typeseq$ (i%) = typeseq$ (i% + 1%)
                     typdescr$(i%) = typdescr$(i% + 1%)
                     twidth$  (i%) = twidth$  (i% + 1%)
                next i%
L11660:         init (" ") typeseq$(typmax%), typdescr$(typmax%),        ~
                           twidth$(typmax%)
                typmax% = typmax% - 1%
                return

        datasave
                gosub save_types
                goto main_display

        REM *************************************************************~
            *                                                           *~
            * PRINT FULL LIST OF SOURCES, TYPES                         *~
            *                                                           *~
            *************************************************************

        print_list
            call "SHOSTAT" ("Printing Listing...")

        REM FIRST PRINT DEFINED SOURCES
            line% = 999%
            page% = 0%

            for source% = 1 to srcmax%
                if line% > 60% then gosub print_source_heading
                print using L20460, srcdescr$(source%)
                line% = line% + 1%
            next source%
            goto print_types

        print_source_heading
            select printer(134)
            page% = page% + 1%
            convert page% to str(prtsrctitle$,124,3), pic(###)
            print page

            print using L20360
            print using L20400
            print using L20380, prtsrctitle$
            print using L20400
            print using L20360
            print skip (1)
            print using L20420
            print using L20440
            line% = 9%
            return

L20360: %****************************************************************~
        ~******************************************************************
L20380: %*###############################################################~
        ~#################################################################*
L20400: %*                                                               ~
        ~                                                                 *
L20420: %                                           S O U R C E

L20440: %                                  ==============================

L20460: %                                  ##############################

L20480: %            SOURCE DESCRIPTION                          TYPE DES~
        ~CRIPTION                        LINE WIDTH
L20500: %            ==============================              ========~
        ~======================          ==========
L20520: %            ##############################              ########~
        ~######################              ##


        print_type_heading
            select printer(134)
            page% = page% + 1%
            convert page% to str(prttypdescr$,124,3), pic(###)
            print page

            print using L20360
            print using L20400
            print using L20380, prttypdescr$
            print using L20400
            print using L20360
            print skip (1)
            print using L20480
            print using L20500
            line% = 9%
            return

        print_types
            if page% = 0% then return
            page% = 0%
            line% = 999%

            for source% = 1 to srcmax%
                if source% = 1 then L20820
                line% = line% + 2% : print : print
L20820:         prtsrcdescr$ = srcdescr$(source%)

                readkey$ = "T" & source$(source%)
L20850:         call "PLOWNEXT" (#1, readkey$, 4%, f1%(1))
                    if f1%(1) = 0% then L20990
                get #1, using L21010, prtdescr$, linewidth%


                if line% > 60% then gosub print_type_heading
                print using L20520, prtsrcdescr$, prtdescr$, linewidth%

                prtsrcdescr$ = " "
                line% = line% + 1%
                goto L20850
L20990:     next source%

L21010:     FMT XX(11), CH(25), BI(1)

            close printer
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto main_display

        REM *************************************************************~
            * FILE I/O FOR TYPE RECORDS                                 *~
            *************************************************************

        save_types
            readkey$ = "T" & str(typsrc$) & hex(00000000)
            call "DELETE" (#1, readkey$, 4%)

            if typmax% = 0% then return  /* Better Not Happen */
            for i% = 1% to typmax%
                twidth% = 70%
                convert twidth$(i%) to twidth%, data goto L31130
L31130:         put filler64$, using L31190, readkey$, typeseq$(i%), " ", ~
                          typdescr$(i%), twidth%, " "
                write #1, str(filler64$,,64), str(filler$(),,textsize%)
            next i%
            return

L31190:     FMT CH(4), CH(1), CH(6), CH(25), BI(1), CH(27)


        load_types
            init (" ") typeseq$(), typdescr$(), twidth$()

            typmax% = 0%
            readkey$ = "T" & str(typsrc$)
L31260:     call "PLOWNEXT" (#1, readkey$, 4%, f1%(1))
            if f1%(1) = 0% then return
                typmax% = typmax% + 1%
                get #1 using L31310, typeseq$(typmax%),                   ~
                                    typdescr$(typmax%), twidth%
L31310:              FMT XX(4), CH(1), XX(6), CH(25), BI(1)
                convert twidth% to twidth$(typmax%), pic(##)
                if typmax% < dim(typeseq$(),1) then L31260
                return

        REM *************************************************************~
            *                                                           *~
            * SOURCES SCREEN                                            *~
            *                                                           *~
            *************************************************************

            show_sources
            init(hex(9c)) fac$()
            edtmessage$="Tab To Source And Press (RETURN) To Edit Types."
            str(pgmid$,1,50) = "Text Sources Are Shown Below"
            for i% = 1 to 15
                if source$(ml%+ i%) <> " " then fac$(i%,1%) = hex(8e)
            next i%

L40130:     accept                                                       ~
               at (01,02), "MANAGE SYSTEM TEXT TYPES",                   ~
               at (01,66), "DATE:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), pgmid$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,24), fac(hex(a4)), sourceheader$          , ch(32),~
                                                                         ~
               at (05,28), fac(hex(81)), str(i$(),,1)           , ch(01),~
               at (05,28), fac(fac$( 1%, 1%)), hex0b$           , ch(01),~
               at (06,28), fac(fac$( 2%, 1%)), hex0b$           , ch(01),~
               at (07,28), fac(fac$( 3%, 1%)), hex0b$           , ch(01),~
               at (08,28), fac(fac$( 4%, 1%)), hex0b$           , ch(01),~
               at (09,28), fac(fac$( 5%, 1%)), hex0b$           , ch(01),~
               at (10,28), fac(fac$( 6%, 1%)), hex0b$           , ch(01),~
               at (11,28), fac(fac$( 7%, 1%)), hex0b$           , ch(01),~
               at (12,28), fac(fac$( 8%, 1%)), hex0b$           , ch(01),~
               at (13,28), fac(fac$( 9%, 1%)), hex0b$           , ch(01),~
               at (14,28), fac(fac$(10%, 1%)), hex0b$           , ch(01),~
               at (15,28), fac(fac$(11%, 1%)), hex0b$           , ch(01),~
               at (16,28), fac(fac$(12%, 1%)), hex0b$           , ch(01),~
               at (17,28), fac(fac$(13%, 1%)), hex0b$           , ch(01),~
               at (18,28), fac(fac$(14%, 1%)), hex0b$           , ch(01),~
               at (19,28), fac(fac$(15%, 1%)), hex0b$           , ch(01),~
                                                                         ~
               at (05,30), fac(hex(8c)),      srcdescr$(ml%+ 1%), ch(25),~
               at (06,30), fac(hex(8c)),      srcdescr$(ml%+ 2%), ch(25),~
               at (07,30), fac(hex(8c)),      srcdescr$(ml%+ 3%), ch(25),~
               at (08,30), fac(hex(8c)),      srcdescr$(ml%+ 4%), ch(25),~
               at (09,30), fac(hex(8c)),      srcdescr$(ml%+ 5%), ch(25),~
               at (10,30), fac(hex(8c)),      srcdescr$(ml%+ 6%), ch(25),~
               at (11,30), fac(hex(8c)),      srcdescr$(ml%+ 7%), ch(25),~
               at (12,30), fac(hex(8c)),      srcdescr$(ml%+ 8%), ch(25),~
               at (13,30), fac(hex(8c)),      srcdescr$(ml%+ 9%), ch(25),~
               at (14,30), fac(hex(8c)),      srcdescr$(ml%+10%), ch(25),~
               at (15,30), fac(hex(8c)),      srcdescr$(ml%+11%), ch(25),~
               at (16,30), fac(hex(8c)),      srcdescr$(ml%+12%), ch(25),~
               at (17,30), fac(hex(8c)),      srcdescr$(ml%+13%), ch(25),~
               at (18,30), fac(hex(8c)),      srcdescr$(ml%+14%), ch(25),~
               at (19,30), fac(hex(8c)),      srcdescr$(ml%+15%), ch(25),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(2)First   (4)Previous   (6)Down One",                ~
               at (22,65), "(13)Instructions",                           ~
               at (24,02),                                               ~
                  "           (5)Next       (7)Up One           (14)Print~
        ~ Report",                                                        ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(000102040506070d0e0f10)),                        ~
               key (keyhit%)

               if keyhit% <> 13 then L40680
                  call "MANUAL" ("TXTTYPES")
                  goto L40130

L40680:        if keyhit% <> 15 then L40720
                  call "PRNTSCRN"
                  goto L40130

L40720:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *                                                           *~
            * TYPES MAINTENANCE SCREEN                                  *~
            *                                                           *~
            *************************************************************

            deffn'102(fn%, field%)
            str(pgmid$,1,8) = "Source: "
            str(pgmid$,9,25) = typsrcdescr$
                  init(hex(8c)) fac$()
                  on fn%      goto  L41190,         /* EDIT             */~
                                    L41230,         /* INSERT           */~
                                    L41290          /* DELETE           */~

        REM DISPLAY MODE
            edtmessage$ = "Position Cursor And Press (RETURN) to Edit"
            for i% = 1 to 10
                if typdescr$(i%) <> " " then fac$(i%,1%) = hex(8e)
            next i%
            goto L41340

L41190: REM EDIT MODE
            edtmessage$="Edit Data As Required, Press (RETURN) When Done"
            goto L41250

L41230: REM INSERT MODE
            edtmessage$="Enter Data Then Press (RETURN), PF(1) To Cancel"
L41250:     fac$(field%, 2%) = hex(82)
            fac$(field%, 1%) = hex(80)
            fac$(field%, 3%) = hex(81)
            goto L41340

L41290: REM DELETE MODE
            edtmessage$ ="Press (RETURN) To Delete Type, PF(1) To Cancel"
            fac$(field%,1%), fac$(field%,2%), fac$(field%,3%) = hex(94)
            goto L41340

L41340:     accept                                                       ~
               at (01,02), "MAINTAIN TEXT TYPES",                        ~
               at (01,66), "DATE:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), pgmid$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,20), fac(hex(a4)), typescrheader$         , ch(40),~
               at (06,20), fac(hex(80)),      typdescr$( 1%)    , ch(25),~
                                                                         ~
               at (06,20), fac(fac$( 1%, 1%)),typdescr$( 1%)    , ch(25),~
               at (07,20), fac(fac$( 2%, 1%)),typdescr$( 2%)    , ch(25),~
               at (08,20), fac(fac$( 3%, 1%)),typdescr$( 3%)    , ch(25),~
               at (09,20), fac(fac$( 4%, 1%)),typdescr$( 4%)    , ch(25),~
               at (10,20), fac(fac$( 5%, 1%)),typdescr$( 5%)    , ch(25),~
               at (11,20), fac(fac$( 6%, 1%)),typdescr$( 6%)    , ch(25),~
               at (12,20), fac(fac$( 7%, 1%)),typdescr$( 7%)    , ch(25),~
               at (13,20), fac(fac$( 8%, 1%)),typdescr$( 8%)    , ch(25),~
               at (14,20), fac(fac$( 9%, 1%)),typdescr$( 9%)    , ch(25),~
               at (15,20), fac(fac$(10%, 1%)),typdescr$(10%)    , ch(25),~
                                                                         ~
               at (06,54), fac(fac$( 1%, 2%)),twidth$  ( 1%)    , ch( 2),~
               at (07,54), fac(fac$( 2%, 2%)),twidth$  ( 2%)    , ch( 2),~
               at (08,54), fac(fac$( 3%, 2%)),twidth$  ( 3%)    , ch( 2),~
               at (09,54), fac(fac$( 4%, 2%)),twidth$  ( 4%)    , ch( 2),~
               at (10,54), fac(fac$( 5%, 2%)),twidth$  ( 5%)    , ch( 2),~
               at (11,54), fac(fac$( 6%, 2%)),twidth$  ( 6%)    , ch( 2),~
               at (12,54), fac(fac$( 7%, 2%)),twidth$  ( 7%)    , ch( 2),~
               at (13,54), fac(fac$( 8%, 2%)),twidth$  ( 8%)    , ch( 2),~
               at (14,54), fac(fac$( 9%, 2%)),twidth$  ( 9%)    , ch( 2),~
               at (15,54), fac(fac$(10%, 2%)),twidth$  (10%)    , ch( 2),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,02),                                               ~
                  "                         (11)Add Type",               ~
               at (22,65), "(13)Instructions",                           ~
               at (24,02),                                               ~
                  "                         (12)Remove Type",            ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)SAVE DATA   ",                           ~
                                                                         ~
               keys(hex(00010b0c0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L42110
                  call "MANUAL" ("TXTTYPES")
                  goto L41340

L42110:        if keyhit% <> 15 then L42150
                  call "PRNTSCRN"
                  goto L41340

L42150:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Tests input for Type (IDed by C%).                        *~
            *************************************************************

        deffn'152
            errormsg$ = " "

*        Test Type Sequence (KEY)
            if typeseq$(c%) <> " " then L50190
                typeseq$(c%) = "A"

L50130:     search str(typeseq$(),1) = typeseq$(c%) to cursor%()
                if cursor%(1%) <> c% then L50160
                if cursor%(2%)  = 0% then L50190
L50160:            typeseq$(c%) = add hex(01)
                   goto L50130

L50190
*        Test Description
            if typdescr$(c%) <> " " then L50240
                errormsg$ = "Description Cannot Be Blank"
                return

L50240
*        Test Text Width
            convert twidth$(c%) to twidth%, data goto L50260
L50260:     twidth% = min(70%, max(twidth%, 10%))
            convert twidth% to twidth$(c%), pic(##)

            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            call "ALLFREE"
            end
