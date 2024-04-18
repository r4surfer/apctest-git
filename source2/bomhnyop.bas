        rem**************************************************************~
            *                                                           *~
            *  bbbb    ooo   m   m  h   h  n   n  y   y   ooo   pppp    *~
            *  b   b  o   o  mm mm  h   h  nn  n   yyy   o   o  p   p   *~
            *  bbbb   o   o  m m m  hhhhh  n n n    y    o   o  pppp    *~
            *  b   b  o   o  m   m  h   h  n  nn    y    o   o  p       *~
            *  bbbb    ooo   m   m  h   h  n   n    y     ooo   p       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * bomhnyop - inputs the list of component replacements      *~
            *            allowed, either by part or by assy/part.       *~
            *-----------------------------------------------------------*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+----------------what----------------------+-who-*~
            * 09/23/86 ! original                                 ! hes *~
            * 05/13/87 ! hnymastr record length mod for std cost  ! jim *~
            * 09/17/87 ! added print flag for s.o.s               ! hes *~
            * 05/16/88 ! added bom approvals (actually put in on  ! tlj *~
            *          !  12-27-89 by mjb from tj's program)      !     *~
            * 06/01/88 ! automatic replacement stuff              ! lkm *~
            * 06/14/90 ! right justified seq. # for auto replcmnts! jdh *~
            * 12/11/91 ! cms/dec 'MASK' project/ added 'ALLFREE'  ! sid *~
            * 03/02/92 ! repositioned bom sequence number when    ! sid *~
            *          !   checking for the header record thru    !     *~
            *          !   bommastr alternate key.                !     *~
            *          ! added the 5th argument for askuser on    !     *~
            *          !   line 12280 for dec/cms.                !     *~
            * 08/09/93 ! added flags for default selection and    ! wph *~
            *          ! whether to prompt for qty.               !     *~
            * 02/02/94 ! added support for 'any part' and 'any    ! WPH *~
        ~    *          ! non-stock part', removed auto-replace.   !     *~
            * 11/29/94 ! disallowed saving of record if no lines. ! wph *~
            *************************************************************

        dim                                                              ~
            apprflag$1,                  /* Approval Flag              */~
            approved$1,                  /* Specific BOM Apprv. flag   */~
            assypart$25,                 /* ASSEMBLY PART NUMBER       */~
            assypart2$25,                /* ASSEMBLY PART NUMBER       */~
            in_bom$3,                                                    ~
            at_seq$3,                                                    ~
            bomid$3,                     /* ALT BOM THIS BOM           */~
            bomid2$3,                    /* ALT BOM THIS BOM           */~
            bom$(114)3,                  /* PART DESCRIPTIONS          */~
            optnpart$25,                 /* COMPONENT PART NUMBER      */~
            compdescr$34,                /* COMPONENT PART DESCRIPTION */~
            cursor%(2),                  /* CURSOR LOCATIONS FOR EDIT  */~
            date$8,                      /* TODAY'S CLOCK DATE         */~
            descr$(114)32,               /* PART DESCRIPTIONS          */~
            dflag$(114)1,                /* Flag selection as the deflt*/~
            dfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            edtmessage$79,               /* Edit Message Text          */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            hdr$(3)79,                   /* PLOWCODE Argument          */~
            header$79,                   /* Screen Title               */~
            header1$79,                  /* Screen Subtitle            */~
            hfac$1,                      /* Screen Subtitle FAC        */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            incl(2),                     /* PLOWCODE Argument          */~
            incl$(2)25,                  /* PLOWCODE Argument          */~
            lastpart$25,                 /* LAST PART NUMBER INPUT     */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfc1$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfc2$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfc3$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lfc4$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line1$50,                                                    ~
            message$79,                  /* INPUT MESSAGE TEXT         */~
            part$(114)25,                /* COMPONENT PART NUMBER LIST */~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            pflag$(114)1,                /* PRINT ON S.O. FLAG         */~
            plowkey$99,                                                  ~
            pseq$3,                                                      ~
            qflag$(114)1,                /* Is qty editable? flag      */~
            readkey$99,                  /* KEY TO PLOW FILE WITH      */~
            readkey2$99,                 /* KEY TO PLOW FILE WITH      */~
            seq$(999)5,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            tfac$(37)1,                  /* SUMMARY SCREEN FAC'S       */~
            type$(114)3,                 /* INV ITEM TYPE              */~
            typedescr$(114)17            /* COMPONENT PART NUMBER LIST */


        dim f2%(24),                     /* FILE STATUS FLAGS FOR      */~
            f1%(24)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! HNYOPTNS ! OPTIONS LIST                             *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                         varc, indexed, recsize = 500,                   ~
                         keypos = 1, keylen = 20

            select  #2,  "HNYOPTNS",                                     ~
                         varc, indexed, recsize = 100,                   ~
                         keypos = 1, keylen = 54

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select # 5, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 2), 100%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4), 0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            lines_allowed% = dim(part$(),1)
            edtmessage$ = "To Modify a Line Item, Position Cursor and" & ~
                          " press RETURN."
            for i% = 1% to 999%
                convert i% to seq$(i%), pic(####)
                str(seq$(i%),5) = ")"
            next i%

*        Get approval flag
            apprflag$ = "N"
            call "READ100" (#1, "SWITCHS.BOM", f1%(1))
            if f1%(1) = 0% then L10000
            get #1 using L09190 , apprflag$
L09190:         FMT XX(20), CH(1)
            if apprflag$ <> "Y" then apprflag$ = "N"
            if apprflag$ = "N" then L10000
L09220:     ask% = 2%
            call "ASKUSER" (ask%, "***** WARNING *****", "BOM Approvals"&~
                           " is ACTIVE,", "Option Processing is NOT "   &~
                           "Compatible with Approved BOM's", "Press "   &~
                           "PF-1 to Continue -or- PF-16 to EXIT PROGRAM")
                if ask% = 16% then L65000
                if ask% <> 1% then L09220


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            compdescr$, assypart$, optnpart$, bomid$, assypart2$,        ~
            bomid2$, pseq$, in_bom$, at_seq$ = " "

            call "ALLFREE"

        inputmode_special
            errormsg$, part$(), descr$(), pflag$(), typedescr$() = " "
            type$(), bom$(), dflag$(), qflag$()  = " "
            editmode%, maxlines%     = 0%

L10150:  for fieldnr% = 1% to 2%
             gosub'161(fieldnr%)
                   if enabled% = 0% then L10280
L10180:      gosub'201(fieldnr%)
               if keyhit%  =  1% then gosub startover
               if keyhit% <>  4% then L10260
L10210:           fieldnr% = fieldnr% - 1%
                  if fieldnr% < 1% then L10150
                     gosub'161(fieldnr%)
                     if enabled% <> 0% then L10180
                     goto L10210
L10260:        if keyhit%  = 16% and fieldnr% = 1 then L65000
               if keyhit% <>  0% then       L10180
L10280:      gosub'151(fieldnr%)
                   if errormsg$ <> " " then L10180
             next fieldnr%


        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

L10303:     if maxlines% = lines_allowed% then L10311
                c% = maxlines% + 1%
                gosub inputlines
                if keyhit% <> 16% then L10315
L10311:              gosub columnone
                     goto line_summary
L10315:         maxlines% = maxlines% + 1%
                goto L10303

        inputlines
            gosub columnone
            if maxlines% = 0% and editmode% = 0 then                     ~
                errormsg$ = hex(84) & "Enter The Replacements List..."
            for fieldnr% = 1% to 5%
                gosub'163(fieldnr%)
                      if enabled% = 0% then L10440
L10400:         gosub'203(fieldnr%)
                      if keyhit% = 16% and maxlines% = 0% then inputlines
                      if keyhit%  = 16% then return
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  0% then       L10400
L10440:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10400
                next fieldnr%
                return


        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        line_summary              /* Summary Screen */
            editmode% = 1%
            line1$ = "Manage Option Part Replacements List"
            message$ = edtmessage$
L11100:     line% = max(0%, min(line%, maxlines% - 14%))
            gosub'115(0%)
            errormsg$ = " "
            if keyhit% =   1% then gosub startover
            if keyhit% <>  2% then L11170
                line% = 0%  :  goto L11100
L11170:     if keyhit% <>  3% then L11190
                line% = maxlines%  :  goto L11100
L11190:     if keyhit% <>  4% then L11230
                line% = line% - 13%
                goto L11100
L11230:     if keyhit% <>  5% then L11270
                line% = line% + 13%
                goto L11100
L11270:     if keyhit% <>  6% then L11290
                line% = line% - 1%  :  goto L11100
L11290:     if keyhit% <>  7% then L11310
                line% = line% + 1%  :  goto L11100
L11310:     if keyhit% <> 16% then L11340
                goto datasave

L11340:     fieldnr% = cursor%(1) - 5%

            if keyhit% <> 28% then L11460
                fieldnr% = 0%
                goto deletemode

L11460:     if fieldnr% >= 1% then L11480
                errormsg$ = hex(84) & "Please Position Cursor First..."
                goto L11100

L11480:     c% = min(line% + fieldnr%, maxlines%)
            if keyhit% = 11% then insertmode
            if keyhit% = 12% then deletemode
            if keyhit% <> 0% then line_summary
            fieldnr% = 1%
            if cursor%(2) > 33% then fieldnr% = 2%
            if cursor%(2) > 37% then fieldnr% = 3%
            if cursor%(2) > 75% then fieldnr% = 4%
            if cursor%(2) > 77% then fieldnr% = 5%

            gosub'163(fieldnr%)
                if enabled% = 0% then line_summary
L12620:     gosub'113(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12620
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L12620
            goto line_summary


*       ** Insert Line Item Logic
        insertmode
            if maxlines% = lines_allowed% then line_summary

            REM Copy all Elements Up One...
            maxlines%=maxlines%+1%
            c% = c% + 1%
            if c%=maxlines% then L14180
                roll% = -1%
                for temp% = maxlines% to c% step -1
                     gosub roll_lines
                next temp%

L14180:     gosub   inputlines
            if keyhit% = 16% then delete_line
            goto insertmode

*       ** Delete Line Item and/or Delete all Logic
        deletemode
            errormsg$ = " "
            if maxlines% = 0 then line_summary
            message$ = "To DELETE Flashing Data, press RETURN, To Return ~
        ~without Delete, press PF1."
            gosub'125(fieldnr%)
                  if keyhit% =1% then line_summary
                  if keyhit% <> 0% then deletemode
            if fieldnr% <> 0% then delete_line
            for c% = 1% to maxlines%
                gosub columnone
            next c%
            maxlines% = 0%
            goto line_summary

        delete_line
            gosub delete_it
            goto line_summary

        delete_it
            if c%=maxlines% then L14940
                roll% = 1%
                for temp% = c% to maxlines%-1%
                     gosub roll_lines
                next temp%
                c%=maxlines%
L14940:     gosub columnone
            maxlines%=maxlines%-1%
        return

        roll_lines
                part$     (temp%) = part$     (temp%+roll%)
                descr$    (temp%) = descr$    (temp%+roll%)
                typedescr$(temp%) = typedescr$(temp%+roll%)
                pflag$    (temp%) = pflag$    (temp%+roll%)
                dflag$    (temp%) = dflag$    (temp%+roll%)
                qflag$    (temp%) = qflag$    (temp%+roll%)
        return

        columnone
            part$(c%), descr$(c%), typedescr$(c%), pflag$(c%),           ~
            errormsg$, type$(c%), bom$(c%), dflag$(c%), qflag$(c%)  = " "
            return

        REM *************************************************************~
            *               M I S C   R O U T I N E S                   *~
            *-----------------------------------------------------------*~
            * Some Miscellaneous routine reside here...                 *~
            *************************************************************

        describe_part
            typedescr$(c%) = "    "

            if part$(c%) <> "** DON'T USE ANYTHING **" then L15580
               descr$(c%) = "'No Replacement' Is Allowed"
               return

L15580:     if part$(c%) <> "** USE ANY STOCK PART **" then L15590
               descr$(c%) = "Any part can be replacement"
               return

L15590:     if part$(c%) <> "** USE ANY NON-STOCK **" then L15600
               descr$(c%) = "Anything can be replacement"
               return

L15600:     descr$(c%) = hex(94) & "Not On File"
            call "GETCODE"(#4, part$(c%), descr$(c%), 0%, 99, f1%(4))
                if f1%(4) = 0% then return
            get #4, using L15640, type$(c%)
L15640:         FMT POS(180), CH(3)
            convert type$(c%) to type%, data goto L15670
            call "HNYTYPE" (type%, typedescr$(c%), 0%)
L15670:     if typedescr$(c%) = "Purchased Tool" then                    ~
               typedescr$(c%) = "PTool"
            if typedescr$(c%) = "Manufactured Tool" then                 ~
               typedescr$(c%) = "MTool"
            if typedescr$(c%) = "** Invalid **" then                     ~
               typedescr$(c%) = "Inval"
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if maxlines% > 0 or assypart$ <> " " then L19180
                readkey$ = hex(21)
                str(readkey$,29) = str(optnpart$) & hex(00)
L19100:         call "PLOWNEXT" (#2, readkey$, 0%, f1%(2))
                     if f1%(2) = 0 then L19180
                if str(readkey$,29,25) <> optnpart$ then L19100
                call "ASKUSER" (2%, "Sorry", "The Options List For This P~
        ~art Can't Be Deleted", "Until All Assembly Specific Lists For Thi~
        ~s Part Are Deleted.", "Press ENTER to START OVER")
                goto inputmode_special

L19180:     readkey$ = str(assypart$)&str(bomid$)&str(optnpart$)&hex(00)
            call "DELETE" (#2, readkey$, 53%)
            if maxlines% = 0% then L19220 /* NO RECORDS TO WRITE        */
            gosub L31000                  /* WRITE HNYOPTNS RECORDS     */
L19220:     lastpart$ = assypart$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  message$ = " "
                  enabled% = 0
                  on fieldnr% gosub L20120,         /* Component Part   */~
                                    L20160          /* Assembly & BOM   */

                  return
L20120: REM Default/Enable For Component Part Number
            message$ = "Enter Part To Manage Options List For."
            enabled% = 1
            return

L20160: REM Default/Enable For Bom Structure ID
*       ** Has Primary List Been Defined?   ...or approval flag set
            if apprflag$ <> "Y" then L20180
                enabled% = 1%
                message$ = "Enter Specific ASSEMBLY and BOM ID."
                return
L20180:     readkey$ = " "
            str(readkey$,29) = str(optnpart$) & hex(00)
            call "PLOWNEXT" (#2, readkey$, 53%, f1%(2))
                 if f1%(2) = 0 then return
*       ** Is Part Used In Any Assemblies?...
            readkey$ = optnpart$
L20240:     call "PLOWALTS" (#5, readkey$, 1%, 25%, f1%(5))
                if f1%(5) = 0 then return
            if str(readkey$,54,3) = "  0" then L20240   /* Header */
            enabled% = 1
            message$ = "Blank, Unless You Wish To Override This Parts Reg~
        ~ular Options In a Certain BOM."
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   T A B L E      *~
            *                                                           *~
            * DEFAULTS/ENABLES FOR TABULAR INPUT. EVERY FIELD ENABLED,  *~
            * FIELD DESCRIPTIONS SET HERE...                            *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0%
                  message$ = " "
                  on fieldnr% gosub L21130,         /* Part Number      */~
                                    L21160,         /* BOM ID           */~
                                    L21180,         /* Print Flag       */~
                                    L21230,         /* Default Flag     */~
                                    L21260          /* Quantity Flag    */
                     return

L21130: REM Default/Enable for Component Part Number   PART$()
                enabled% = 1%
                message$ = "Enter Replacement Part or leave blank, or " &~
                           "'ANY PART', ANY NSP', or 'NOTHING'."
                return

L21160: REM Default/Enable For Bom ID                        BOM$()
            enabled% = 1%
            if part$(c%) = "** DON'T USE ANYTHING **" then enabled% = 0%
            if part$(c%) = "** USE ANY STOCK PART **" then enabled% = 0%
            if part$(c%) = "** USE ANY NON-STOCK **" then  enabled% = 0%
            if type$(c%) > "000" and type$(c%) < "500" then enabled% = 0%
            message$="Enter the Specific BOM ID for the Replacement Part"
            return

L21180: REM Default/Enable For Print Flag
            enabled% = 1%
            if part$(c%) <> "** DON'T USE ANYTHING **" then L21220
                enabled% = 0%
                pflag$(c%) = "N"
L21220:     message$ = "'Y' to print part & desc on SO if selected, 'D'"&~
                       " Descr only, 'blank' never print"
            if pflag$(c%) = " " then pflag$(c%) = "N"
            return

L21230: REM Default/Enable For Default Flag
            enabled% = 1%
            if part$(c%)= "** USE ANY STOCK PART **" then L21240
            if part$(c%)= "** USE ANY NON-STOCK **" then L21240
            message$ = "Enter 'Y' to cause this part to be the default"& ~
                       " selection."
            if dflag$(c%) = " " then dflag$(c%) = "N"
            return

L21240:     enabled%=0%
            dflag$(c%) = "N"
            return

L21260: REM Default/Enable For Quantity Flag
            enabled% = 1%
            if part$(c%)= "** DON'T USE ANYTHING **" then enabled%=0%
            message$ = "Enter 'Y' to prompt for quantity or 'N' to not"& ~
                       " prompt for quantity."
            if qflag$(c%) = " " then qflag$(c%) = "N"
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            startover% = 2%
            call "STARTOVR" (startover%)
            if startover% = 1% then return
            return clear all
            init(hex(00)) plowkey$
            goto inputmode

L30000: REM *************************************************************~
            *        L O A D   R E P L A C E M E N T   P A R T S        *~
            *-----------------------------------------------------------*~
            * Load Replacement Parts for Option Part Entered.           *~
            *************************************************************

            maxlines%, c% = 0%
            readkey$ = str(assypart$) & str(bomid$)
            str(readkey$,29) = str(optnpart$) & hex(00)
L30120:     call "PLOWNEXT" (#2, readkey$, 53%, f1%(2))
                 if f1%(2) = 0% then return
            if c% = 0 then print at(4,1,80);hex(84);                     ~
                                           "Loading Replacements List..."
            c%, maxlines% = c% + 1
            get #2, using L30480, part$(c%),pflag$(c%),type$(c%),bom$(c%),~
                                 dflag$(c%), qflag$(c%)
            if pflag$(c%)<>"Y" and pflag$(c%)<>"D" then pflag$(c%)="N"
            if dflag$(c%)<>"Y" and dflag$(c%)<>"N" then dflag$(c%)="N"
            if qflag$(c%)<>"Y" and qflag$(c%)<>"N" then qflag$(c%)="N"

            gosub describe_part
            goto L30120

*       ** Format Statement for file HNYOPTNS
L30480:     FMT XX(54),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* Replacement Part Number    */~
                CH(1),                   /* Print On S.O. Flag         */~
                CH(3),                   /* Part Type                  */~
                CH(3),                   /* BOM ID for Manuf Parts     */~
                CH(1),                   /* Default Flag               */~
                CH(1)                    /* Quantity Flag              */


L31000: REM *************************************************************~
            *     W R I T E   R E P L A C E M E N T S   T O   F I L E   *~
            *                                                           *~
            * Saves the replacement list to the HNYOPTNS file for this  *~
            * assembly                                                  *~
            *************************************************************

            call "SHOSTAT"("Saving Options List")

            for temp% = 1% to maxlines%
                write #2, using L31570, assypart$, bomid$, optnpart$,     ~
                          temp%, part$(temp%), pflag$(temp%),            ~
                          type$(temp%), bom$(temp%), dflag$(temp%),      ~
                          qflag$(temp%), " "
            next temp%
            return

L31570:     FMT CH(25),                  /* Assembly Part Number       */~
                CH(03),                  /* Which Bom Structure?       */~
                CH(25),                  /* Component Part Number      */~
                BI(1),                   /* Sequence Number            */~
                CH(25),                  /* Replacement Part Number    */~
                CH(1),                   /* Print On S.O. Flag         */~
                CH(3),                   /* Part Type                  */~
                CH(3),                   /* BOM ID for Manuf Parts     */~
                CH(1),                   /* Default Selection Flag     */~
                CH(1),                   /* Prompt for Quantity Flag   */~
                CH(12)                   /* Filler                     */


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                header$ = " "
                pfktext$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                  (8)Find Lists By Assembl~
        ~y                    (16)Exit Program"
                pfkeys$ = hex(000104080d0f10)

                if fieldnr% > 1% then L40600
                  str(pfktext$(1),,62) = " "    /* Shut Off Start Over */
                  str(pfktext$(3),,62) = " "    /* Shut Off More       */
                  str(pfkeys$,2,3) = hex(ffffff)
                  if lastpart$ <> " " then header$ =                     ~
                                        "Last Part Managed: " & lastpart$
                  goto L40675
L40600:        str(pfktext$(3),64) = " "        /* Shut Off Exit       */
               str(pfkeys$,7,1) = hex(ff)

L40675:        str(pfktext$(3),63,1) = hex(84)
               str(header$,62) = "BOMHNYOP: " & cms2v$
                  on fieldnr% gosub L40900,         /* COMPONENT PART # */~
                                    L40900          /* ASSEMBLY PART #  */
                     goto L41000

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40900:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L41000:     accept                                                       ~
               at (01,02), "Manage Option Part Replacements List",       ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Optional Part Number",                       ~
               at (06,23), fac(lfac$( 1)), optnpart$            , ch(25),~
               at (06,49), fac(hex(8c)),   compdescr$           , ch(32),~
                                                                         ~
               at (07,02), "Specific Assembly",                          ~
               at (07,23), fac(lfac$( 2)), assypart$            , ch(25),~
               at (08,02), "   BOM Identifier",                          ~
               at (08,23), fac(lfac$( 2)), bomid$               , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 8% then L41975
                  hdr$() = " "
                  hdr$(2) = "Assembly Part              BOM Id."
                  incl$() = optnpart$
                  incl(1) = 29.25
                  incl(2) = -1.28
                  readkey$ = hex(21)
                  str(readkey$,29) = optnpart$
                  call "PLOWCODE" (#2, readkey$, " ", -8028%, -0.32,     ~
                      f1%(5), hdr$(),  0, 0, incl(), incl$()," "," ", #4)
                     if f1%(5) = 0 then L41925
                  assypart$ = str(readkey$,,25)
                  bomid$ = str(readkey$,26)
L41925:           goto L41000

L41975:        if keyhit% <> 13% then L42030
                  call "MANUAL" ("BOMHNYOP")
                  goto L41000

L42030:        if keyhit% <> 15% then L42070
                  call "PRNTSCRN"
                  goto L41000

L42070:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       L I N E   S U M M A R Y   S C R E E N               *~
            * --------------------------------------------------------- *~
            * Line Item Summary Screen.                                 *~
            *************************************************************

            deffn'203(fieldnr%)          /* Input Mode Lines */
                init(hex(8c)) lfac$(), tfac$(), dfac$() : hfac$ = hex(ac)
                init(hex(8c)) lfc1$(), lfc2$(), lfc3$(), lfc4$()
                pfktext$(1) = "(1)Start Over                           "&~
                              "                       (13)Instructions"
                pfktext$(2) = "                                        "&~
                              "                       (15)Print Screen"
                pfktext$(3) = "                                        "&~
                              "                       (16)Edit Mode"
                pfkeys$ = hex(00010d0f10)
                if c% > line%+13 then line% = c%-13
                goto L43740


            deffn'113(fieldnr%)          /* Edit Mode Lines */
                init(hex(8c)) lfac$(), tfac$(), dfac$() : hfac$ = hex(ac)
                init(hex(8c)) lfc1$(), lfc2$(), lfc3$(), lfc4$()
                pfktext$(1) = "(1)Start Over                           "&~
                              "                       (13)Instructions"
                pfktext$(2) = "                                        "&~
                              "                       (15)Print Screen"
                pfktext$(3) = " "
                pfkeys$ = hex(00010d0f)

L43740:         REM Set Modifiable FAC...
                on fieldnr% gosub L43790,         /* Part Number        */~
                                  L43815,         /* BOM ID             */~
                                  L43830,         /* Print Flag         */~
                                  L43835,         /* Default Flag       */~
                                  L43840          /* Quantity Flag      */
                     goto L44330

L43790:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(c%-line%) = hex(81)
                      return

L43815:               lfc1$(c%-line%) = hex(81)
                      return

L43830:               lfc2$(c%-line%) = hex(81)
                      return

L43835:               lfc3$(c%-line%) = hex(81)
                      return

L43840:               lfc4$(c%-line%) = hex(81)
                      return

            deffn'115(fieldnr%) /* Line summary display screen */
                init (hex(8e)) tfac$() : init (hex(84)) lfac$()
                init (hex(8c)) dfac$() : hfac$=hex(ae)
                init (hex(8e)) lfc1$(), lfc2$(), lfc3$(), lfc4$()
                pfktext$(1) = "(1)Start Over                             ~
        ~      (11)Insert     (13)Instructions"
                pfktext$(2) = "(2)First Line  (4)Prev  (6)Down One       ~
        ~      (12)Delete     (15)Print Screen"
                pfktext$(3) = "(3)Last Lines  (5)Next  (7)Up One         ~
        ~      (28)Delete All (16)Save Data"
                pfkeys$ = hex(0001020406030507ffff0b0c0d0f10ff1c)

                REM Turn Off Appropriate Fields
                if maxlines% > 0 then L44120
                  str(pfktext$(2),49,10) = " " /* Shut Off Delete */
                  str(pfktext$(3),49,14) = " " /* Shut Off Delete All */
                  str(pfkeys$,12,1), str(pfkeys$,17,1) = hex(ff)
L44120:         if line% > 0 then L44150
                  str(pfktext$(2),,36)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,3,3) = hex(ffffff)
L44150:         if line%+14 < maxlines% then L44330
                  str(pfktext$(3),,36)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,6,3) = hex(ffffff)
                  goto L44330

            deffn'125(d%)  /* Delete Lines From Memory */
                init (hex(8c)) lfac$(), tfac$(), dfac$() : hfac$=hex(ae)
                init (hex(8c)) lfc1$(), lfc2$(), lfc3$(), lfc4$()
                pfktext$(1) = "(1)Cancel Delete Request                  ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "(ENTER) Delete Flashing Line(s)"
                pfkeys$ = hex(00010d0f10)
                if d% = 0% then init(hex(94)) lfac$(), tfac$(),          ~
                                              dfac$(), lfc1$()           ~
                           else lfc1$(d%), lfac$(d%), tfac$(d%),         ~
                                lfc2$(d%), dfac$(d%),                    ~
                                lfc3$(d%), lfc4$(d%) = hex(94)

L44330:         header1$ = "Seq.  Part Number                BOM Descript~
        ~ion                    Type  P D Q"
                   header$ = "PART: " & optnpart$
                   if assypart$ = " " then L44400
                   header$=header$&", Assy: "&assypart$&",BOM Id: "&bomid$
                   goto L44410
L44400:         header$ = header$ & " " & compdescr$
L44410:         if len(header$) > 60 then L44530
                str(header$,62) = "BOMHNYOP: " & cms2v$
L44530:         str(tfac$(), min(20, (maxlines%-line%)+1)) = all(hex(9c))
                str(pfktext$(3),63,1) = hex(84)

L45025:     accept                                                       ~
               at (01,02), fac(hex(8c)), line1$                 , ch(50),~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), str(header1$,1,05)     , ch(05),~
               at (05,08), fac(hex(ac)), str(header1$,7,25)     , ch(25),~
               at (05,34), fac(hex(ac)), str(header1$,34,03)    , ch(03),~
               at (05,38), fac(hex(ac)), str(header1$,38,30)    , ch(30),~
               at (05,69), fac(hex(ac)), str(header1$,69,05)    , ch(05),~
               at (05,75), fac(hex(ac)), str(header1$,75,01)    , ch(01),~
               at (05,77), fac(hex(ac)), str(header1$,77,01)    , ch(01),~
               at (05,79), fac(hex(ac)), str(header1$,79,01)    , ch(01),~
                                                                         ~
               at (06,02), fac(tfac$( 1)), seq$   (line%+ 1%)   , ch(05),~
               at (07,02), fac(tfac$( 2)), seq$   (line%+ 2%)   , ch(05),~
               at (08,02), fac(tfac$( 3)), seq$   (line%+ 3%)   , ch(05),~
               at (09,02), fac(tfac$( 4)), seq$   (line%+ 4%)   , ch(05),~
               at (10,02), fac(tfac$( 5)), seq$   (line%+ 5%)   , ch(05),~
               at (11,02), fac(tfac$( 6)), seq$   (line%+ 6%)   , ch(05),~
               at (12,02), fac(tfac$( 7)), seq$   (line%+ 7%)   , ch(05),~
               at (13,02), fac(tfac$( 8)), seq$   (line%+ 8%)   , ch(05),~
               at (14,02), fac(tfac$( 9)), seq$   (line%+ 9%)   , ch(05),~
               at (15,02), fac(tfac$(10)), seq$   (line%+10%)   , ch(05),~
               at (16,02), fac(tfac$(11)), seq$   (line%+11%)   , ch(05),~
               at (17,02), fac(tfac$(12)), seq$   (line%+12%)   , ch(05),~
               at (18,02), fac(tfac$(13)), seq$   (line%+13%)   , ch(05),~
               at (19,02), fac(tfac$(14)), seq$   (line%+14%)   , ch(05),~
                                                                         ~
               at (06,08), fac(lfac$( 1)), part$  (line%+ 1%)   , ch(25),~
               at (07,08), fac(lfac$( 2)), part$  (line%+ 2%)   , ch(25),~
               at (08,08), fac(lfac$( 3)), part$  (line%+ 3%)   , ch(25),~
               at (09,08), fac(lfac$( 4)), part$  (line%+ 4%)   , ch(25),~
               at (10,08), fac(lfac$( 5)), part$  (line%+ 5%)   , ch(25),~
               at (11,08), fac(lfac$( 6)), part$  (line%+ 6%)   , ch(25),~
               at (12,08), fac(lfac$( 7)), part$  (line%+ 7%)   , ch(25),~
               at (13,08), fac(lfac$( 8)), part$  (line%+ 8%)   , ch(25),~
               at (14,08), fac(lfac$( 9)), part$  (line%+ 9%)   , ch(25),~
               at (15,08), fac(lfac$(10)), part$  (line%+10%)   , ch(25),~
               at (16,08), fac(lfac$(11)), part$  (line%+11%)   , ch(25),~
               at (17,08), fac(lfac$(12)), part$  (line%+12%)   , ch(25),~
               at (18,08), fac(lfac$(13)), part$  (line%+13%)   , ch(25),~
               at (19,08), fac(lfac$(14)), part$  (line%+14%)   , ch(25),~
                                                                         ~
               at (06,34), fac(lfc1$( 1)), bom$   (line%+ 1%)   , ch(03),~
               at (07,34), fac(lfc1$( 2)), bom$   (line%+ 2%)   , ch(03),~
               at (08,34), fac(lfc1$( 3)), bom$   (line%+ 3%)   , ch(03),~
               at (09,34), fac(lfc1$( 4)), bom$   (line%+ 4%)   , ch(03),~
               at (10,34), fac(lfc1$( 5)), bom$   (line%+ 5%)   , ch(03),~
               at (11,34), fac(lfc1$( 6)), bom$   (line%+ 6%)   , ch(03),~
               at (12,34), fac(lfc1$( 7)), bom$   (line%+ 7%)   , ch(03),~
               at (13,34), fac(lfc1$( 8)), bom$   (line%+ 8%)   , ch(03),~
               at (14,34), fac(lfc1$( 9)), bom$   (line%+ 9%)   , ch(03),~
               at (15,34), fac(lfc1$(10)), bom$   (line%+10%)   , ch(03),~
               at (16,34), fac(lfc1$(11)), bom$   (line%+11%)   , ch(03),~
               at (17,34), fac(lfc1$(12)), bom$   (line%+12%)   , ch(03),~
               at (18,34), fac(lfc1$(13)), bom$   (line%+13%)   , ch(03),~
               at (19,34), fac(lfc1$(14)), bom$   (line%+14%)   , ch(03),~
                                                                         ~
               at (06,38), fac(dfac$( 1)), descr$ (line%+ 1%)   , ch(30),~
               at (07,38), fac(dfac$( 2)), descr$ (line%+ 2%)   , ch(30),~
               at (08,38), fac(dfac$( 3)), descr$ (line%+ 3%)   , ch(30),~
               at (09,38), fac(dfac$( 4)), descr$ (line%+ 4%)   , ch(30),~
               at (10,38), fac(dfac$( 5)), descr$ (line%+ 5%)   , ch(30),~
               at (11,38), fac(dfac$( 6)), descr$ (line%+ 6%)   , ch(30),~
               at (12,38), fac(dfac$( 7)), descr$ (line%+ 7%)   , ch(30),~
               at (13,38), fac(dfac$( 8)), descr$ (line%+ 8%)   , ch(30),~
               at (14,38), fac(dfac$( 9)), descr$ (line%+ 9%)   , ch(30),~
               at (15,38), fac(dfac$(10)), descr$ (line%+10%)   , ch(30),~
               at (16,38), fac(dfac$(11)), descr$ (line%+11%)   , ch(30),~
               at (17,38), fac(dfac$(12)), descr$ (line%+12%)   , ch(30),~
               at (18,38), fac(dfac$(13)), descr$ (line%+13%)   , ch(30),~
               at (19,38), fac(dfac$(14)), descr$ (line%+14%)   , ch(30),~
                                                                         ~
               at (06,69), fac(dfac$( 1)), typedescr$(line%+ 1%), ch(05),~
               at (07,69), fac(dfac$( 2)), typedescr$(line%+ 2%), ch(05),~
               at (08,69), fac(dfac$( 3)), typedescr$(line%+ 3%), ch(05),~
               at (09,69), fac(dfac$( 4)), typedescr$(line%+ 4%), ch(05),~
               at (10,69), fac(dfac$( 5)), typedescr$(line%+ 5%), ch(05),~
               at (11,69), fac(dfac$( 6)), typedescr$(line%+ 6%), ch(05),~
               at (12,69), fac(dfac$( 7)), typedescr$(line%+ 7%), ch(05),~
               at (13,69), fac(dfac$( 8)), typedescr$(line%+ 8%), ch(05),~
               at (14,69), fac(dfac$( 9)), typedescr$(line%+ 9%), ch(05),~
               at (15,69), fac(dfac$(10)), typedescr$(line%+10%), ch(05),~
               at (16,69), fac(dfac$(11)), typedescr$(line%+11%), ch(05),~
               at (17,69), fac(dfac$(12)), typedescr$(line%+12%), ch(05),~
               at (18,69), fac(dfac$(13)), typedescr$(line%+13%), ch(05),~
               at (19,69), fac(dfac$(14)), typedescr$(line%+14%), ch(05),~
                                                                         ~
               at (06,75), fac(lfc2$( 1)), pflag$(line%+ 1%)    , ch(01),~
               at (07,75), fac(lfc2$( 2)), pflag$(line%+ 2%)    , ch(01),~
               at (08,75), fac(lfc2$( 3)), pflag$(line%+ 3%)    , ch(01),~
               at (09,75), fac(lfc2$( 4)), pflag$(line%+ 4%)    , ch(01),~
               at (10,75), fac(lfc2$( 5)), pflag$(line%+ 5%)    , ch(01),~
               at (11,75), fac(lfc2$( 6)), pflag$(line%+ 6%)    , ch(01),~
               at (12,75), fac(lfc2$( 7)), pflag$(line%+ 7%)    , ch(01),~
               at (13,75), fac(lfc2$( 8)), pflag$(line%+ 8%)    , ch(01),~
               at (14,75), fac(lfc2$( 9)), pflag$(line%+ 9%)    , ch(01),~
               at (15,75), fac(lfc2$(10)), pflag$(line%+10%)    , ch(01),~
               at (16,75), fac(lfc2$(11)), pflag$(line%+11%)    , ch(01),~
               at (17,75), fac(lfc2$(12)), pflag$(line%+12%)    , ch(01),~
               at (18,75), fac(lfc2$(13)), pflag$(line%+13%)    , ch(01),~
               at (19,75), fac(lfc2$(14)), pflag$(line%+14%)    , ch(01),~
                                                                         ~
               at (06,77), fac(lfc3$( 1)), dflag$(line%+ 1%)    , ch(01),~
               at (07,77), fac(lfc3$( 2)), dflag$(line%+ 2%)    , ch(01),~
               at (08,77), fac(lfc3$( 3)), dflag$(line%+ 3%)    , ch(01),~
               at (09,77), fac(lfc3$( 4)), dflag$(line%+ 4%)    , ch(01),~
               at (10,77), fac(lfc3$( 5)), dflag$(line%+ 5%)    , ch(01),~
               at (11,77), fac(lfc3$( 6)), dflag$(line%+ 6%)    , ch(01),~
               at (12,77), fac(lfc3$( 7)), dflag$(line%+ 7%)    , ch(01),~
               at (13,77), fac(lfc3$( 8)), dflag$(line%+ 8%)    , ch(01),~
               at (14,77), fac(lfc3$( 9)), dflag$(line%+ 9%)    , ch(01),~
               at (15,77), fac(lfc3$(10)), dflag$(line%+10%)    , ch(01),~
               at (16,77), fac(lfc3$(11)), dflag$(line%+11%)    , ch(01),~
               at (17,77), fac(lfc3$(12)), dflag$(line%+12%)    , ch(01),~
               at (18,77), fac(lfc3$(13)), dflag$(line%+13%)    , ch(01),~
               at (19,77), fac(lfc3$(14)), dflag$(line%+14%)    , ch(01),~
                                                                         ~
               at (06,79), fac(lfc4$( 1)), qflag$(line%+ 1%)    , ch(01),~
               at (07,79), fac(lfc4$( 2)), qflag$(line%+ 2%)    , ch(01),~
               at (08,79), fac(lfc4$( 3)), qflag$(line%+ 3%)    , ch(01),~
               at (09,79), fac(lfc4$( 4)), qflag$(line%+ 4%)    , ch(01),~
               at (10,79), fac(lfc4$( 5)), qflag$(line%+ 5%)    , ch(01),~
               at (11,79), fac(lfc4$( 6)), qflag$(line%+ 6%)    , ch(01),~
               at (12,79), fac(lfc4$( 7)), qflag$(line%+ 7%)    , ch(01),~
               at (13,79), fac(lfc4$( 8)), qflag$(line%+ 8%)    , ch(01),~
               at (14,79), fac(lfc4$( 9)), qflag$(line%+ 9%)    , ch(01),~
               at (15,79), fac(lfc4$(10)), qflag$(line%+10%)    , ch(01),~
               at (16,79), fac(lfc4$(11)), qflag$(line%+11%)    , ch(01),~
               at (17,79), fac(lfc4$(12)), qflag$(line%+12%)    , ch(01),~
               at (18,79), fac(lfc4$(13)), qflag$(line%+13%)    , ch(01),~
               at (19,79), fac(lfc4$(14)), qflag$(line%+14%)    , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key(keyhit%)

               if keyhit% <> 8% then L47625
                  hdr$(3) = hex(ac) & "Shown below is the single level 'w~
        ~here used' for this optional part."
                  header1$ = hex(06) & "Part " & optnpart$ &             ~
                                                " is used in these BOM's"
                  mat incl = zer
                  readkey$ = optnpart$

                  incl(1)  = -54.03 /* Exclude Header BOM Seq Number */
                  incl$(1) = hex(202030)

                  call "PLOWCODE" (#5, readkey$, header1$, 8025%, 1.32,  ~
                      f1%(5), hdr$(), 28, 0, incl(), incl$()," ","Y", #4)
                  goto L44330

L47625:        if keyhit% <> 13% then L47725
                  call "MANUAL" ("BOMHNYOP")
                  goto L45025

L47725:        if keyhit% <> 15% then L47825
                  call "PRNTSCRN"
                  goto L45025

L47825:        close ws
               call "SCREEN" addr ("C", 3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110,         /* COMPONENT PART # */~
                                    L50170          /* ASSEMBLY PART #  */
                     return
L50110:     REM TEST DATA FOR COMPONENT PART NUMBER
                call "GETCODE"(#4, optnpart$, compdescr$, 0%, 0, f1%(4))
                if f1%(4) = 1% then return
                   errormsg$ = "Invalid Part Number: " & optnpart$
                   return

L50170: REM Test Data For Assembly Part Number
            if assypart$ = " " and bomid$ = " " and                      ~
                                   apprflag$ <> 'Y' then L50610
            readkey$ = str(optnpart$) & str(assypart$) & str(bomid$)
            hdr$(2)="  Part Assemblies             Part Descriptions"
            hdr$(1) = " Shown Below are Existing Assemblies Using the" & ~
                      " Option Part."
            hdr$(3) = hex(ac) & "Select the Assembly Part And/Or BOM" &  ~
                      " To Enter List For.  Use PF-16 to Exit."
            mat incl = zer

*       ** Get Specific Part & BOMID ***
            call "PLOWCODE" (#5, readkey$, header1$, 8025%, 1.32,        ~
                  f1%(5), hdr$(), 28, 0, incl(), incl$()," ","Y", #4)
            if f1%(5) <> 0% then L50460

*       ** Does assembly exist ***
            readkey2$=str(assypart$,,25) & str(bomid$,,3) & "  0"
            call "READ100" (#5,readkey2$,f1%(5))
            if f1%(5) <> 0% then L50420
L50370:         if assypart$ = " " then errormsg$ = "Approval Flag is" & ~
                               " Set.  Must Enter an existing Assembly." ~
                   else errormsg$ = "Assembly Not On File"
                return

L50420:     errormsg$ = "The Option Part specified is not an Option" &   ~
                        " Part within the Assembly."
            return

L50460:     assypart$  = str(readkey$,26%,25%)
                bomid$ = str(readkey$,51%,3%)

*        If approval flag is set check if BOM is approved.
            if apprflag$ <> "Y" then L50610
                readkey2$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
                call "READ100" (#5, readkey2$, f1%(5))
                if f1%(5) = 0% then L50370
                get #5 using L50550, approved$
L50550:              FMT POS(115), CH(6)
                if approved$ = " " then L50610
                     errormsg$ = "BOM may not be modified, "  &          ~
                                 "BOM is APPROVED."
                     return

L50610: REM Now Try And Load Options List
            call "PUTPAREN" (compdescr$)
            gosub L30000
            if maxlines% = 0% then return
            return clear all
            goto line_summary

        REM *************************************************************~
            *      T E S T   R E P L A C E M E N T   P A R T S          *~
            *-----------------------------------------------------------*~
            * Test Data for Normal Replacement Partsd.                  *~
            *************************************************************

        deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51130,         /* PART NUMBER      */~
                                    L51520,         /* BOM ID           */~
                                    L51740,         /* PRINT FLAG       */~
                                    L51770,         /* Default Flag     */~
                                    L51800          /* Quantity Flag    */
                     return
L51130: REM Test Data For Replacement Part Number
            if part$(c%) = "NOTHING" then part$(c%) =                    ~
                                    "** DON'T USE ANYTHING **"
            if part$(c%) = "ANY PART" then part$(c%) =                   ~
                                    "** USE ANY STOCK PART **"
            if part$(c%) = "ANY NSP" then part$(c%) =                    ~
                                    "** USE ANY NON-STOCK **"

            if part$(c%) = "** DON'T USE ANYTHING **" then L51260
            if part$(c%) = "** USE ANY STOCK PART **" then L51260
            if part$(c%) = "** USE ANY NON-STOCK **"  then L51260

            errormsg$ = hex(0684) & "Select a replacement from the list:"
            call "GETCODE" (#4, part$(c%), errormsg$, 1%, 0, f1%(4))
            errormsg$ = " "
                if f1%(4) <> 0% then L51270
                errormsg$ = "Invalid Part Number: " & part$(c%)
                return

L51260:     bom$(c%) = "   "
L51270:     mat cursor% = zer
            search part$() = str(part$(c%)) to cursor%() step 25
                if cursor%(2) = 0% then L51320
                     errormsg$ = "Replacement Is Already On This List"
                     return

L51320
*       ** Is Replacement Part Redundant In Any Assemblies?...
            if assypart$ = " " then L51370
                if part$(c%) <> assypart$ then L51490
                     errormsg$ = "Replacement Can't Be Same As Assembly"
                     return
L51370:     readkey$ = optnpart$
L51380:     call "PLOWALTS" (#5, readkey$, 1%, 25%, f1%(5))
                if f1%(5) = 0 then L51490
            if str(readkey$,54,3) = "  0" then L51380    /* HEADER ? */
            get #5, using L51420, op$
            if op$ <> "Y" then L51380
L51420:         FMT POS(91), CH(1)
            if str(readkey$,26,25) <> part$(c%) then L51380
                errormsg$ = "This Replacement Is Redundant In" &         ~
                            " Assembly: " & part$(c%)
                errormsg$ = errormsg$ &", BOM: " & str(readkey$,51,3)
                return

L51490:     gosub describe_part
            return

L51520
*        Test Data for BOM ID                      BOM$()
                if apprflag$ <> "Y" and bom$(c%) = "   " then return
                if part$(c%) = "** DON'T USE ANYTHING **"  then return
                if type% < 500% or (type% > 789% and type% < 800%)       ~
                           then return
                readkey$ = str(part$(c%),,25) & str(bom$(c%),,3)
                hdr$()= " Listed Below Are The Existing BOMs "           ~
                                              & "For Part: " & part$(c%)
                errormsg$ = hex(06) & "Select a Bill Of Materials."
                call "PLOWCODE" (#5, readkey$, errormsg$,                ~
                                           2025%, .30, f1%(5), hdr$(), 3)
                errormsg$ = " "
                if f1%(5) = 1% then L51710
                  if apprflag$ <> "Y" then errormsg$ =                   ~
                    "BOM does not exist.  Leave blank or enter an "     &~
                                                          "existing BOM."~
                  else errormsg$ = "BOM does not exist.  Enter an "     &~
                                                          "existing BOM."
                  goto L51720
L51710:           bom$(c%) = str(readkey$,26,3)
L51720:         return

L51740: REM Test Data For Print Flag
            if pflag$(c%) = "Y" then return
            if pflag$(c%) = "D" then return
            pflag$(c%) = "N"
            return

L51770: REM Test Data For Default Flag
            if dflag$(c%) = "Y" then L51776
            if dflag$(c%) = "N" then return
            errormsg$ = "Enter 'Y' or 'N'."
            return

L51776:     for i% = 1% to maxlines%
              if i% = c% then L51779
              if dflag$(i%) = "Y" then  L51783
L51779:     next i%
            return

L51783:     ask% = 2%
            call "ASKUSER"(ask%, "*** WHAT TO DO? ***",                  ~
         "Another Part is Already Flagged as the Default Replacement.",  ~
         "Press RETURN to Retain That Part as the Default, or    "    ,  ~
         "Press PF 16 to Override it and Make This Part the Default.")
            if ask% <> 16% then L51791
               dflag$(i%) = "N"
               return
L51791:     if ask% <> 0% then L51783
               dflag$(c%) = "N"
               return

L51800: REM Test Data For Quantity Flag
            if qflag$(c%) = "Y" then return
            if qflag$(c%) = "N" then return
            errormsg$ = "Enter 'Y' or 'N'."
            return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
