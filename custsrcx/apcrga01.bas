        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA01                             *~
            *  Creation Date     - 09/11/95                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - This Program provides RGA Header     *~
            *                      Entry/Update and RGA Inquiry         *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/11/95 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *          !                                          !     *~
            *************************************************************
        dim                                                              ~
            rga_cuscode$9,               /* Customer No.               */~
            rga_hd_status$2,             /* Status of RGA Header       */~
            rga_number$4,                /* RGA No.                    */~
            rga_auth_id$3,               /* Authorizing Userid         */~
            rga_dte$8,                   /* RGA Enter Date             */~
            rga_filed_dte$8,             /* RGA Filed Date             */~
            rga_hd_desc_txt$4,           /* RGA Header Text Code       */~
            rga_hd_txt$1,                /* RGA Header Text Flag       */~
            rga_userid$3,                /* Userid of Entry/Mod        */~
            rga_mod_dte$8,               /* RGA Entry/Mod Date         */~
            rga_rg$1,                    /* RGA/RG Flag                */~
            rga_filler$29,               /* APCRGAHD Filler Area       */~
            prev_number$4,               /* Previous RGA Added         */~
            readkey$25,                  /* Gencodes File Read Key     */~
            g_rec$30,                    /* Gencodes Data Area         */~
            dtlkey$6,                    /* APCRGADT File Read Key     */~
            cus_desc$30,                 /* Customer Description       */~
            status_desc$30,              /* Header Status Description  */~
            desc$32,                     /* Gencodes Description       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$60,                 /* Error message              */~
            text$(113,1)70,              /* Text Buffer Area           */~
            txt$4,                       /* Text ID                    */~
            header$79,                   /* Text Edit Description Hdr. */~
            i$(24%)80,                   /* Detail Line(10) Array Area */~
            inpmessage$79,               /* Informational Message      */~
            cursor%(2%),                 /*                            */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            progid$18,                   /* Screen Line #2 Program ID  */~
            line10$79,                   /* Screen Line #10            */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim table_number$(99%)4,         /* Array RGA No.              */~
            table_item$(99%)2,           /* Array Item No.             */~
            table_part$(99%)25,          /* Array Part No.             */~
            table_so$(99%)8,             /* Array S.O. No.             */~
            table_line$(99%)2,           /* Array S.O. Line            */~
            table_po$(99%)16,            /* Array P.O. No.             */~
            table_credit(99%),           /* Array Credit Amount        */~
            table_credit$(99%)9,         /* Array Credit Amount        */~
            table_desc$(99%)4,           /* Array Detail Text ID       */~
            table_txt$(99%)1,            /* Array Detail Text Flag     */~
            table_status$(99%)2,         /* Array Detail Status        */~
/*PAR000*/  table_subp$(99%)20           /* Array Sub Part No.         */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(12%)20                 /* Text from file opening     */

	dim blankdate$6,		 /* Blank (empty) date         */~
	    workdate10$10,         	 /* Century date mm-dd-yyyy    */~
	    workdate8$8			 /* Regular date mm-dd-yy      */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 RGA Inquiry/Update Program     "
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
            * #01 ! APCRGAHD ! APC RGA Header Master File               *~
            * #02 ! APCRGADT ! APC RGA Detail Master File               *~
            * #03 ! GENCODES ! System Code Table File                   *~
            * #04 ! CUSTOMER ! Customer Master File                     *~
            * #05 ! TXTRGA   ! RGA Text File                            *~
            * #12 ! USERLCMS ! Caelus User ID Master File               *~
            * #63 ! BCKSUBPT ! Sub Part File                 (PAR000)   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =   4,                     ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #2,  "APCRGADT",                                      ~
/*PAR000*/              varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =   6,                     ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #4,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #5,  "TXTRGA",                                        ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =   11

            select #12,  "USERLCMS",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

/*PAR000*/
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup     


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" ( #1, fs%(1%), f2%( 1%), 100%, rslt$(1%))
            call "OPENCHCK" ( #2, fs%(2%), f2%( 2%), 100%, rslt$(2%))
            call "OPENCHCK" ( #3, fs%(3%), f2%( 3%),   0%, rslt$(3%))
            call "OPENCHCK" ( #4, fs%(4%), f2%( 4%),   0%, rslt$(4%))
            call "OPENCHCK" ( #5, fs%(5%), f2%( 5%),   0%, rslt$(5%))
            call "OPENOLIB" (#12, "SHARE", f2%(12%), rslt$(12%), axd$)
/*PAR000*/  call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            date$ = date
            call "DATEFMT" (date$)

	    call "DATUFMTC" (blankdate$)

            prev_number$ = " "
            progid$      = "APCRGA01: " & str(cms2v$,,8)
            line10$      = "Item  Part No.                 SO No.  Line"&~
                           "  PO No.             Credit   Status"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 6%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then L10250

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit% =  1% then gosub startover
                     if keyhit% <> 6% then L10220 /* Previous Field */
L10150:                   fieldnr% = max(1%, fieldnr% - 1%)

                          gosub'051(fieldnr%)
                               if enabled% = 1% then L10120
                               if fieldnr% = 1% then L10090
                               goto L10150

L10220:              if keyhit% = 16% and  fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then L10120

L10250:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10120

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit% =  1% then gosub startover
                if keyhit% = 10% then gosub add_detail
                if keyhit% = 12% then gosub data_delete
                if keyhit% = 14% then gosub dataput
                if keyhit% = 16% then exit_program
                if keyhit% <> 0% then editpg1
REM below for line items
            if cursor%(1%) < 11% or cursor%(1%) > 19% then L11230
            if dtl% = 0%         then editpg1
              if cursor%(1%) < 13% then scr% = 1%
              if cursor%(1%) >= 13% and             ~
                            cursor%(1%) < 15% then scr% = 2%
              if cursor%(1%) >= 15% and             ~
                            cursor%(1%) < 17% then scr% = 3%
              if cursor%(1%) >= 17% then scr% = 4%
            if scr% > rga_max%   then editpg1
                gosub edit_detail

                goto editpg1
REM Not line item
L11230:     fieldnr%    = cursor%(1%) - 2%
            if fieldnr% < 2% or fieldnr% > 6% then editpg1
            if fieldnr% = lastfieldnr%        then editpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled%  =  0%            then editpg1

L11300:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%   =  1%            then gosub startover
                if keyhit%   <> 0%            then L11300

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " "           then L11300
                lastfieldnr% = fieldnr%
            goto L11230

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************
        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
            return

*        Define the Input Message for the Screen/Field Indicated
L28110:     if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
            "Enter a Valid RGA No.?                                    ",~
            "Enter a Valid Customer No.?                               ",~
            "Enter 'RG' Flag (Y/N)?                                    ",~
            "Enter the ID of the Person Authorizing RGA?               ",~
            "Enter a Valid Filed Date?                                 ",~
            "Enter or Edit Header Text. PF(8) Key                      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, rga_number$, rga_cuscode$, ~
                rga_auth_id$, rga_dte$, rga_filed_dte$, rga_userid$,     ~
                rga_mod_dte$, rga_hd_txt$, rga_rg$, rga_filler$,         ~
                cus_desc$, status_desc$
            gosub initialize_table

            rec% = 0%
            rga_hd_status$ = "01"
            init(hex(ff)) rga_hd_desc_txt$, text$()
            call "TXTFUTIL" (#5, f2%(5), "INTL", rga_hd_desc_txt$)

        return

        initialize_table
            init(" ") table_item$(), table_part$(), table_so$(),         ~
                table_line$(), table_po$(), table_desc$(), table_txt$(), ~
                table_status$(), table_number$(), table_credit$(),       ~
/*PAR000*/      table_subp$()
            rga_max% = 0%
            mat table_credit = zer
            gosub lookup_rga_number

        return

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            rec% = 0%
            convert str(rga_number$,2%,3%) to rga_number%, data goto L30350

            convert rga_number% to str(rga_number$,2%,3%), pic(000)

            read #1,key = rga_number$, eod goto L30350

            get #1, using L35030,                                         ~
                     rga_cuscode$,       /* Customer No.               */~
                     rga_hd_status$,     /* Status of RGA Header       */~
                     rga_number$,        /* RGA Number                 */~
                     rga_auth_id$,       /* Authorizing Userid         */~
                     rga_dte$,           /* RGA Enter Date             */~
                     rga_filed_dte$,     /* RGA Filed Date             */~
                     rga_hd_desc_txt$,   /* RGA Header Text Code       */~
                     rga_hd_txt$,        /* RGA Header Text Flag       */~
                     rga_userid$,        /* Userid of Entry/Mod        */~
                     rga_mod_dte$,       /* RGA Entry/Mod Date         */~
                     rga_rg$,            /* RGA/RG Flag                */~
                     rga_filler$         /* APCRGAHD Filler Area       */

            call "DATEFMT" (rga_dte$)

            if rga_filed_dte$ = " " or rga_filed_dte$ = blankdate$ then L30320
            call "DATEFMT" (rga_filed_dte$)

L30320:     gosub load_detail

            rec% = 1%
L30350: return

        load_detail
            i% = 0% : dtl% = 0% : rga_max% = 0%
            str(dtlkey$,1%,4%) = rga_number$
            str(dtlkey$,5%,2%) = "  "

        load_next
            read #2,key > dtlkey$, using L30440, dtlkey$, eod goto L30570
L30440:         FMT POS(12), CH(06)

            if str(dtlkey$,1%,4%) <> rga_number$ then L30570
            i% = i% + 1%
            get #2, using L35160,                                         ~
                table_status$(i%), table_number$(i%), table_item$(i%),   ~
                table_part$  (i%), table_so$    (i%), table_line$(i%),   ~
                table_po$    (i%), table_credit (i%), table_desc$(i%),   ~
/*PAR000*/      table_txt$   (i%), table_subp$  (i%)

            convert table_credit(i%) to table_credit$(i%), pic(#,##0.00-)

            goto load_next
L30570:     rga_max%    = i%
            i%          = 0%
            if rga_max% > 0% then dtl% = 1%
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        data_delete
            if userid$ = "JBF"       then L31080
            if rga_hd_status$ > "01" then L31100
L31080:     if dtl% <> 0% then gosub delete_details
            if dtl% =  0% then dataput
L31100:     call "SHOSTAT" ( "ERROR - Unable to Delete APCRGAHD" ) : stop

        return

        dataput
            call "SHOSTAT" ( "Updating APCRGAHD Data" )

            read #1,hold,key = rga_number$, eod goto L31210

            delete #1

L31210:     if keyhit% <> 12% then L31250
                gosub delete_text

                goto L31520
L31250:     if rga_dte$ = " " or rga_dte$ = blankdate$ then rga_dte$ = date$
            call "DATUNFMT" (rga_dte$)

            if rga_filed_dte$ = " " or rga_filed_dte$ = blankdate$ then L31310
            call "DATUNFMT" (rga_filed_dte$)

L31310:     rga_mod_dte$ = date$
            rga_userid$  = userid$
            if rec% = 0%      then gosub assign_rga_number

            put #1, using L35030,                                         ~
                     rga_cuscode$,       /* Customer #                 */~
                     rga_hd_status$,     /* Status of RGA Header       */~
                     rga_number$,        /* RGA Number                 */~
                     rga_auth_id$,       /* Authorizing Userid         */~
                     rga_dte$,           /* RGA Enter Date             */~
                     rga_filed_dte$,     /* RGA Filed Date             */~
                     rga_hd_desc_txt$,   /* RGA Header Text Code       */~
                     rga_hd_txt$,        /* RGA Header Text Flag       */~
                     rga_userid$,        /* Userid of Entry/Mod        */~
                     rga_mod_dte$,       /* RGA Entry/Mod Date         */~
                     rga_rg$,            /* RGA/RG Flag                */~
                     rga_filler$         /* APCRGAHD Filler Area       */

            write #1, eod goto L31540

            if keyhit% = 10% then L31560  /* Return to Add Detail       */
L31520: return clear all
        goto inputmode
L31540:     call "SHOSTAT" ( "ERROR - Unable to Update APCRGAHD" ) : stop

L31560: return

        assign_rga_number
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "APC  RGA1"
            str(readkey$,10%,15%) = "00"
            read #3,hold,key = readkey$, using L31640, g_rec$,            ~
                eod goto L31780
L31640:         FMT POS(25), CH(30)

            convert str(g_rec$,21%,4%) to rga_number%, data goto L31790

            if str(g_rec$,21%,4%) = "9999" then goto L31790
            
            convert rga_number% to rga_number$,        pic(0000)

            rga_number% = rga_number% + 1%
            convert rga_number% to str(g_rec$,21%,4%), pic(0000)

            put #3, using L31640, g_rec$

            rewrite #3

        return
L31780:     call "SHOSTAT" ( "ERROR - Unable to Allocate RGA # ?" ) : stop

        return clear all
        goto inputmode

L31790:   hex% = 0%
          hex% = 65%
          if str(g_rec$,22%,3%) <> "999" then goto L31830
          
          if str(g_rec$,21%,1%) = "9" then goto L31810
             get str(g_rec$,21%,4%), using L31820, hex%
L31820:             FMT BI(1)
             hex% = hex% + 1%

          if hex% = 91% then goto L31840
          
L31810:   put str(g_rec$,21%,1%) using L31820, hex%

L31830:   convert str(g_rec$,22%,3%) to rga_number%, data goto L31780

          rga_number% = rga_number% + 1%

          convert rga_number% to str(g_rec$,22%,3%), pic(000)

          rga_number$ = str(g_rec$,21%,4%)

L31840:   if hex% = 91% then str(g_rec$,21%,4%) = "0000"

          put #3, using L31640, g_rec$

          rewrite #3

        return

            
        add_detail
            if rga_hd_status$ > "03" then L32170
            if rga_number$ = " "     then gosub dataput
            if rga_max% <> 0%        then L31890
                rga_item$  = "00"
                goto L31900
L31890:     rga_item$      = table_item$(rga_max%)
L31900:     convert rga_item$ to rga_item%, data goto L31920

L31920:     rga_item%      = rga_item% + 1%
            if rga_item% > 99%       then L32150
            convert rga_item% to rga_item$, pic(00)

            proc% = 1%                             /* Add Details      */
            goto L32020

        edit_detail
            proc%     = 0%                               /* Edit Only  */
            rga_item$ = table_item$(scr%)
L32020:     call "APCRGA1B"   (rga_cuscode$,       /* RGA Customer No. */~
                               rga_number$,        /* RGA No.          */~
                               rga_item$,          /* RGA Item         */~
                               proc%,              /* Process Flag     */~
                               #2,                 /* APCRGADT Channel */~
                               #3,                 /* GENCODES Channel */~
                               #4,                 /* CUSTOMER Channel */~
                               #5,                 /* TXTRGA   Channel */~
/*PAR000*/                     #63  )              /* BCKSUBPT CHANNEL */

            gosub initialize_table
            gosub load_screen                      /* Reload Screen    */

            goto L32190
L32150:     call "SHOSTAT" ( "ERROR - Cannot Exceed 99 Details" )   : stop
            goto L32190
L32170:     call "SHOSTAT" ( "ERROR - Invalid STATUS for Updates" ) : stop

L32190: return

        delete_details
            for s% = 1% to rga_max%
                if userid$ = "JBF"          then L32260
                if table_status$(s%) > "10" then L32400

L32260:     next s%

            for s% = 1% to rga_max%
                str(dtlkey$,1%,4%) = rga_number$
                str(dtlkey$,5%,2%) = table_item$(s%)
                read #2,hold,key = dtlkey$, eod goto L32370

                delete #2

                if table_txt$(s%) = "Y" then gosub delete_dt_text

L32370:     next s%

            dtl% = 0%
L32400: return

        edit_text
            gosub'099(rga_hd_desc_txt$)
            if txt% = 0% then L32470
                call "TXTFUTIL" (#5, f2%(5), "LOAD", rga_hd_desc_txt$)

L32470:     header$ = "Edit Header TEXT for RGA # " & rga_number$
            call "TXTINSUB" (#5, f2%(5), "RGH", header$,                 ~
                rga_hd_desc_txt$, text$() )

            gosub text_check

            if edit% <> 2% then fieldnr% = fieldnr% + 1%
            if txt%  =  0% then L32570
                call "TXTFUTIL" (#5, f2%(5), "SAV2", rga_hd_desc_txt$)

L32570: return

        delete_text
            call "SHOSTAT" ("Deleting Text for RGA ("&rga_number$&")")
            call "TXTFUTIL" (#5, f2%(5), "DELE", rga_hd_desc_txt$)

        return

        delete_dt_text
            call "SHOSTAT" ("Delete Detail Text - RGA ("&rga_number$&")")
            call "TXTFUTIL" (#5, f2%(5), "DELE", table_desc$(s%))

        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
L35030:     FMT CH(09),                  /* RGA Customer               */~
                CH(02),                  /* RGA Header Status          */~
                CH(04),                  /* RGA Number                 */~
                CH(03),                  /* Authorizing Userid         */~
                CH(08),                  /* RGA Entry Date             */~
                CH(08),                  /* RGA Filed Date             */~
                CH(04),                  /* RGA Header Text Code       */~
                CH(01),                  /* RGA Header Text Flag       */~
                CH(03),                  /* Userid of Mod/Entry        */~
                CH(08),                  /* RGA Mod/Entry Date         */~
                CH(01),                  /* RGA/RG Flag                */~
                CH(29)                   /* APCRGAHD Filler Area       */

L35160:     FMT POS(10),  CH(02),        /* RGA Detail Item Status     */~
                CH(04),                  /* RGA No.                    */~
                CH(02),                  /* RGA Item                   */~
                XX(11),   CH(25),        /* RGA Part No.               */~
                CH(08),                  /* RGA Sales Order No.        */~
                CH(02),                  /* RGA Sales Order Line Item  */~
                XX(08),   CH(16),        /* RGA Purchase Order No.     */~
                XX(21),   PD(14,4),      /* RGA Credit Amount          */~
                XX(112),  CH(04),        /* RGA Detail Text ID         */~
                CH(01),                  /* RGA Detail Text Flag       */~
/*PAR000*/      XX(10), CH(20)           /* RGA Detail Sub Part        */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
L40060:     gosub'050(1%, fieldnr%)
            gosub set_pf1

            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40200,     /* RGA No.                  */  ~
                              L40200,     /* Customer No.             */  ~
                              L40200,     /* RGA/RG Flag              */  ~
                              L40200,     /* Authorizing Userid       */  ~
                              L40200,     /* RGA Filed Date           */  ~
                              L40200      /* Header Text Flag         */
            goto accept_screen

                lfac$(fieldnr%) = hex(80)  :  return   /* Up / Low   */
L40200:         lfac$(fieldnr%) = hex(81)  :  return   /* Upper Only */
                lfac$(fieldnr%) = hex(82)  :  return   /* Numeric    */

        accept_screen
            if rec% = 1% then lfac$(4%) = hex(84)
            accept                                                       ~
                at (01,02),                                              ~
                   "RGAII Header Entry/Edit"                    ,        ~
                                                                         ~
                at (01,66), "Today:"                            ,        ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                                                                         ~
                at (02,02), fac(hex(94)),   errormsg$           , ch(60),~
                at (02,63), fac(hex(8c)),   progid$             , ch(18),~
                                                                         ~
                at (03,02), "RGA No.   :"                       ,        ~
                at (03,14), fac(lfac$(1%)), rga_number$         , ch(04),~
                at (03,31), "RGA Status:"                       ,        ~
                at (03,43), fac(hex(84)),   rga_hd_status$      , ch(02),~
                at (03,47), fac(hex(84)),   status_desc$        , ch(30),~
                                                                         ~
                at (04,02), "Customer  :"                       ,        ~
                at (04,14), fac(lfac$(2%)), rga_cuscode$        , ch(09),~
                at (04,31), fac(hex(84)),   cus_desc$           , ch(32),~
                                                                         ~
                at (05,02), "'RG' Flag :"                       ,        ~
                at (05,14), fac(lfac$(3%)), rga_rg$             , ch(01),~
                                                                         ~
                at (06,02), "Authorized:"                       ,        ~
                at (06,14), fac(lfac$(4%)), rga_auth_id$        , ch(03),~
                                                                         ~
                at (07,02), "Filed Date:"                       ,        ~
                at (07,14), fac(lfac$(5%)), rga_filed_dte$      , ch(08),~
                at (07,31), "Entry Date:"                       ,        ~
                at (07,43), fac(hex(84)),   rga_dte$            , ch(08),~
                at (07,58), "Prev. RGA No.:"                    ,        ~
                at (07,73), fac(hex(84)),   prev_number$        , ch(04),~
                                                                         ~
                at (08,02), "RGA Text  :"                       ,        ~
                at (08,14), fac(lfac$(6%)), rga_hd_txt$         , ch(01),~
                                                                         ~
                at (10,02), fac(hex(ac)),   line10$             , ch(79),~
                                                                         ~
                at (11,03), fac(hex(84)),  table_item$ ( 1%+i%),  ch(02),~
                at (11,07), fac(hex(84)),  table_part$ ( 1%+i%),  ch(25),~
/*PAR000*/      at (12,07), fac(hex(84)),  table_subp$ ( 1%+i%),  ch(20),~
                at (11,33), fac(hex(84)),  table_so$   ( 1%+i%),  ch(08),~
                at (11,42), fac(hex(84)),  table_line$ ( 1%+i%),  ch(02),~
                at (11,47), fac(hex(84)),  table_po$   ( 1%+i%),  ch(16),~
                at (11,64), fac(hex(84)), table_credit$( 1%+i%),  ch(09),~
                at (11,77), fac(hex(84)), table_status$( 1%+i%),  ch(02),~
                                                                         ~
                                                                         ~
                at (13,03), fac(hex(84)),  table_item$ ( 2%+i%),  ch(02),~
                at (13,07), fac(hex(84)),  table_part$ ( 2%+i%),  ch(25),~
/*PAR000*/      at (14,07), fac(hex(84)),  table_subp$ ( 2%+i%),  ch(20),~
                at (13,33), fac(hex(84)),  table_so$   ( 2%+i%),  ch(08),~
                at (13,42), fac(hex(84)),  table_line$ ( 2%+i%),  ch(02),~
                at (13,47), fac(hex(84)),  table_po$   ( 2%+i%),  ch(16),~
                at (13,64), fac(hex(84)), table_credit$( 2%+i%),  ch(09),~
                at (13,77), fac(hex(84)), table_status$( 2%+i%),  ch(02),~
                                                                         ~
                                                                         ~
                at (15,03), fac(hex(84)),  table_item$ ( 3%+i%),  ch(02),~
                at (15,07), fac(hex(84)),  table_part$ ( 3%+i%),  ch(25),~
/*PAR000*/      at (16,07), fac(hex(84)),  table_subp$ ( 3%+i%),  ch(20),~
                at (15,33), fac(hex(84)),  table_so$   ( 3%+i%),  ch(08),~
                at (15,42), fac(hex(84)),  table_line$ ( 3%+i%),  ch(02),~
                at (15,47), fac(hex(84)),  table_po$   ( 3%+i%),  ch(16),~
                at (15,64), fac(hex(84)), table_credit$( 3%+i%),  ch(09),~
                at (15,77), fac(hex(84)), table_status$( 3%+i%),  ch(02),~
                                                                         ~
                                                                         ~
                at (17,03), fac(hex(84)),  table_item$ ( 4%+i%),  ch(02),~
                at (17,07), fac(hex(84)),  table_part$ ( 4%+i%),  ch(25),~
/*PAR000*/      at (18,07), fac(hex(84)),  table_subp$ ( 4%+i%),  ch(20),~
                at (17,33), fac(hex(84)),  table_so$   ( 4%+i%),  ch(08),~
                at (17,42), fac(hex(84)),  table_line$ ( 4%+i%),  ch(02),~
                at (17,47), fac(hex(84)),  table_po$   ( 4%+i%),  ch(16),~
                at (17,64), fac(hex(84)), table_credit$( 4%+i%),  ch(09),~
                at (17,77), fac(hex(84)), table_status$( 4%+i%),  ch(02),~
                                                                         ~
                                                                         ~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if rec% = 1%     then lfac$(4%) = hex(84)
                if keyhit% <> 2% then L41460
                     i% = 0%
                     goto L40060
L41460:         if keyhit% <> 3% then L41500
L41470:              x% = int(rga_max% / 4.0)
                     i% = (x% * 4%)
                     goto L40060
L41500:         if keyhit% <> 4% then L41550
                     if i% < 5% then i% = 0%
                     if i% =  0% then L40060
                        i% = i% - 4%
                     goto L40060
L41550:         if keyhit% <> 5% then L41620
                     i% = i% + 4%
                     if i% <  rga_max% then L40060
                        x% = (rga_max% / 4%) - 1%
                     if x% = 0%  then L41470
                        i% = x% * 4%
                     goto L40060
L41620:         if keyhit% = 8% and fieldnr% = 6% then gosub edit_text
                if keyhit% <> 15% then L41670
                     call "PRNTSCRN"

                     goto accept_screen
L41670:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            return

        set_pf1
        if edit% = 2% then L41880     /*  Input Mode             */
            pf$(1) = "(1)Start Over                            " &       ~
                     "                                      "
            pf$(2) = "                                         " &       ~
                     "                      (15)Print Screen"
            pf$(3) = "                                         " &       ~
                     "                      (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

            if fieldnr% = 1% then L41840
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41840:     gosub check_screen

            return

L41880: if fieldnr% > 0% then L42040  /*  Edit Mode - Select Field */
            pf$(1) = "(1)Start Over     (4)Prev Page           " &       ~
                     "(8)Add/Edit Text      (14)Update Data "
            pf$(2) = "(2)First Page     (5)Next Page           " &       ~
                     "(10)Add Details       (15)Print Screen"
            pf$(3) = "(3)Last Page                             " &       ~
                     "(12)Delete            (16)Exit Program"
            pfkeys$ = hex(0102030405ffff08ff0aff0cff0e0f1000)

            if rec% <> 0% then goto L41990
                str(pf$(3%),42%,20%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41990:     gosub check_screen

            return

                                     /*  Edit Mode - Enabled    */
L42040:     pf$(1) = "(1)Start Over     (4)Prev Page           " &       ~
                     "(8)Add/Edit Text                      "
            pf$(2) = "(2)First Page     (5)Next Page           " &       ~
                     "(10)Add Details       (15)Print Screen"
            pf$(3) = "(3)Last Page                             " &       ~
                     "                                      "
            pfkeys$ = hex(0102030405ffff08ff0affffffff0fff00)
            gosub check_screen

            return

        check_screen
            if rga_max% > 9%         then L42240
                gosub no_first
                gosub no_next
                gosub no_last
                gosub no_prev

            return

L42240:     if i% >= 4%              then L42280
                gosub no_first
                gosub no_prev

L42280:     if (i% + 4%) <= rga_max% then L42310
                gosub no_last

L42310:     if i% <= (rga_max% - 4%) then L42340
                gosub no_next

L42340:     return

        no_first
            str(pf$(2%),1%,14%) = " "  : str(pfkeys$,2%,1%) = hex(ff)
        return

        no_next
            str(pf$(2%),18%,20%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return

        no_last
            str(pf$(3%),1%,14%) = " "  : str(pfkeys$,3%,1%) = hex(ff)
        return

        no_prev
            str(pf$(1%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50160,     /* RGA No.                    */~
                              L50340,     /* Customer No.               */~
                              L50440,     /* RGA/RG Flag                */~
                              L50520,     /* Authorized by              */~
                              L50610,     /* Filed Date                 */~
                              L50720      /* Header Text Flag           */
            return

        REM RGA No.                             RGA_NUMBER$
L50160:     if rga_number$ = " "     then L50250
            if edit% <> 1%           then L50270

        load_screen
            gosub dataload
                if rec% <> 1%        then L50280
                fieldnr% = 6%
                gosub lookup_customer

L50250:     gosub lookup_status

L50270: return
L50280:     errormsg$ = "(Error) - Invalid RGA Number (Required)?"
            init(" ") rga_number$
        return

        REM Customer No.                        RGA_CUSCODE$
        lookup_customer
L50340:     read #4,key = rga_cuscode$, using L50360, cus_desc$,          ~
                eod goto L50390
L50360:         FMT POS(10), CH(30)

        return
L50390:     errormsg$ = "(Error) - Invalid Customer No. (Required)?"
            init(" ") rga_cuscode$, cus_desc$
        return

        REM RGA/RG Flag                         RGA_RG$
L50440:     if rga_rg$ = " "         then L50480
            if rga_rg$ = "Y"                                             ~
            or rga_rg$ = "N"         then L50490
            errormsg$ = "(Error) - Invalid RGA/RG Flag (Y/N)?"
L50480:     init("N") rga_rg$
L50490: return

        REM Authorizing Userid                  RGA_AUTH_ID$
L50520:     if rga_auth_id$ = " "    then rga_auth_id$ = userid$
            read #12,key = rga_auth_id$, eod goto L50560

            goto L50580
L50560:     errormsg$ = "(Error) - Invalid Auth. Userid  (Required)?"
            init(" ") rga_auth_id$
L50580: return

        REM RGA Filed Date                      RGA_FILED_DTE$
L50610:     if rga_filed_dte$ = " "  or rga_filed_dte$ = blankdate$ then L50660
            if rga_hd_status$   <> "05" then L50670
            call "DATEOK" (rga_filed_dte$, date%, errormsg$)

            if date% = 0% then L50670
L50660: return
L50670:     errormsg$ = "(Error) - Invalid Filed Date ?"
            init(" ") rga_filed_dte$
        return

        REM RGA Header Text Flag                RGA_HD_TXT$
L50720: text_check
            gosub'099(rga_hd_desc_txt$)

            if txt%        =  1%     then rga_hd_txt$ = "Y"
            if rga_hd_txt$ = "Y"     then L50790
            gosub edit_text

L50790: return

        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************
        lookup_rga_number
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "APC  RGA1"
            str(readkey$,10%,15%) = "00"
            read #3,key = readkey$, using L60080, g_rec$, eod goto L60160
L60080:         FMT POS(25), CH(30)

            if str(g_rec$,21%,4) = "0000" then goto L60160
            convert str(g_rec$,21%,4%) to prev_number%, data goto L60160

            prev_number% = prev_number% - 1
            convert prev_number% to prev_number$, pic(0000)

        return
L60160:   hex% = 0%
          prev_number$ = str(g_rec$,21%,4%)
          if str(g_rec$,22%,3%) <> "000" then goto L60170
          
          if str(g_rec$,21%,1%) = "A" then goto L60180
          if str(g_rec$,21%,4%) = "0000" then goto L60170
             get str(g_rec$,21%,4%), using L60190, hex%
L60190:             FMT BI(1)
             hex% = hex% - 1%

          put str(prev_number$,1%,1%) using L60190, hex%
 
L60170:   convert str(g_rec$,22%,3%) to prev_number%, data goto L31780

          prev_number% = prev_number% - 1%
          if prev_number% = -1% then prev_number% = 999

          convert prev_number% to str(prev_number$,2%,3%), pic(000)


L60180:   if str(g_rec$,21%,4%) = "0000" then str(prev_number$,1%,4%) = "Z999"
          if str(g_rec$,21%,1%) = "A"    then str(prev_number$,1%,4%) = "9999"
        return

        lookup_status
            readkey$ = " "
            str(readkey$,1%,9%)   = "APC  RGA1"
            str(readkey$,10%,15%) =  rga_hd_status$

            read #3,key = readkey$, using L60250, desc$, eod goto L60290
L60250:         FMT POS(25), CH(30)

            status_desc$ = str(desc$,1%,30%)
        return
L60290:     errormsg$ = "(Error) - Invalid STATUS Lookup?"
            init(" ") status_desc$
        return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L60380
            txt% = 1%
L60380: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
