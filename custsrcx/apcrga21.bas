        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA21  (Like APCRGA01)            *~
            *  Creation Date     - 01/21/97                             *~
            *  Last Modified Date- 06/17/08                             *~
            *  Description       - This Program provides RGA Header     *~
            *                      Entry/Update and RGA Inquiry         *~
            *                                                           *~
            *  Special Comments  - Uses NEW Planning Files              *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            *          !                                          !     *~
            * 04/01/98 ! Y2K                                      ! DJD *~
            * 12/15/00 ! Mod to allow alpa in rga number (EWD001) ! CMG *~
            * 10/01/01 ! Mod to add faxing of RGA's.     (EWD002) ! CMG *~
            * 04/17/02 ! Mod to change company name.     (EWD003) ! TLM *~
            * 03/04/04 ! (EWD004) Mods to for the new version of  ! CMG *~
            *          !          Rightfax                        !     *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            * 03/16/06 ! (AWD005) - modification for North East   ! CMG *~
            * 06/17/08 ! (AWD006) - modification for RGA NUmbers  ! CMG *~
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
/*PAR000*/  table_subp$(99%)20,          /* Array Sub Part No.         */~
            table_so$(99%)8,             /* Array S.O. No.             */~
            table_line$(99%)2,           /* Array S.O. Line            */~
            table_po$(99%)16,            /* Array P.O. No.             */~
            table_credit(99%),           /* Array Credit Amount        */~
            table_credit$(99%)9,         /* Array Credit Amount        */~
            table_desc$(99%)4,           /* Array Detail Text ID       */~
            table_txt$(99%)1,            /* Array Detail Text Flag     */~
            table_status$(99%)2          /* Array Detail Status        */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        dim blankdate$6,                 /* blank date (6 chars)       */~
	    date$10                      /* System Date mm-dd-yy       */

        dim                              /*                   EWD002   */~
            exp_dte$8,                   /* Exp Enter Date             */~
            exp_date$8,                  /* Exp Enter Date             */~
            attmsg$30,                   /* Attention Message          */~
            text_id$4,                   /* Detail Text ID             */~
            fax_text$(7%)50,             /* Common Text ID for Lookup  */~
            fax_note$(5%)50,             /* Fax Notes                  */~
            apc$(2%)60,                  /* Descriptions (1) thru (2)  */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Description           */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_prv$30,                    /* Private Label Name         */~
            s_1$2,                       /* Private Label Code         */~ 
            tot_credit$14,               /* Total Credit Amount        */~
            ship_to$(6%)30,              /* Customer Ship To Info      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            a$80,                        /* Print Line on Invoice      */~
            attn$60,                     /* Attention Line             */~
            fax_key$24,                  /* Save File Key              */~
            inv_no$11,                   /* CUSTOMER Phone No.         */~
            fax_phone$11,                /* CUSTOMER Phone No.         */~
            file$8,                      /* Inv Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* FAX SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            text_key$11,                 /* Primary Key for Text       */~
            textid$4,                    /* Text Id.                   */~
            s_key$9,                     /* Save 1 thru 9 of Key       */~
            sku_code$3,                  /* CUSTOMER SKU CODE          */~
            sku_no$25,                   /* CUSTOMER SKU NUMBER(EWD)   */~
            sku_key$28,                  /* SKU LOOKUP KEY             */~
            company$60                   /* Company Name  (EWD003)     */

        dim schema$8                     /* (AWD005) Schema            */

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
            * #13 ! CHRISTIE ! Work File to Build Fax                   *~
            * #15 ! APCSKUNO ! SKU NUMBER MASTER FILE  (EWD)            *~
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

/*  (EWD002)  */
            select #13, "CHRISTIE", varc, consec,  recsize =  80

            select #14, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =  1,   keylen =  32

            select #15, "APCSKUNO",                                      ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

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
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#63, fs%(63%), f2%(63%), 0%, rslt$(63%))

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
            progid$      = "APCRGA21: " & str(cms2v$,,8)
            line10$      = "Item  Part No.                 SO No.  Line"&~
                           "  PO No.             Credit   Status"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

* (AWD005) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)

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
              if cursor%(1%) < 13% then scr% = i% + 1%
              if cursor%(1%) >= 13% and             ~
                            cursor%(1%) < 15% then scr% = i% + 2%
              if cursor%(1%) >= 15% and             ~
                            cursor%(1%) < 17% then scr% = i% + 3%
              if cursor%(1%) >= 17% then scr% = i% + 4%

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
        initialize_variables                               /*  (EWD002)  */
            init(" ") errormsg$, inpmessage$, rga_number$, rga_cuscode$, ~
                rga_auth_id$, rga_dte$, rga_filed_dte$, rga_userid$,     ~
                rga_mod_dte$, rga_hd_txt$, rga_rg$, rga_filler$,         ~
                cus_desc$, status_desc$, exp_dte$, exp_date$, attmsg$,   ~
                attn$, text_id$, fax_text$(), apc$(), apc_scr$, apc_prt$,~
                apc_sze$, s_23m$, s_23$, s_prv$, s_1$, tot_credit$,      ~
                ship_to$(), hdr$, msg$(), a$, fax_key$, inv_no$,         ~
                fax_phone$, file$, library$, script$, volume$, sku_code$

            gosub initialize_table

            rec% = 0%
            rga_hd_status$ = "01"
            init(hex(ff)) rga_hd_desc_txt$, text$(), table_desc$(),      ~
                          table_txt$()
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
            rec% = 0%                                   /* (EWD001) */
            convert str(rga_number$,3%,2%) to rga_number%, data goto L30350

            convert rga_number% to str(rga_number$,3%,2%), pic(00)

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

            if str(rga_filed_dte$,1,6) = blankdate$ then rga_filed_dte$= " "

            call "DATEFMT" (rga_filed_dte$)

REM L30320:     gosub load_detail

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
L31250:     if rga_dte$ = " " or str(rga_dte$,1,6) = blankdate$ then rga_dte$ = date$
            call "DATUNFMT" (rga_dte$)

            if rga_filed_dte$ = " " or str(rga_filed_dte$,1,6) = blankdate$ then L31310
            call "DATUNFMT" (rga_filed_dte$)

L31310:     rga_mod_dte$ = date$
            rga_userid$  = userid$
            if rec% = 0%      then gosub assign_rga_number
	    call "DATUNFMT" (rga_mod_dte$)

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

REM         gosub ask_fax                 /*  (EWD002)  */

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
                                                      /* (EWD002) */
            convert str(g_rec$,21%,4%) to rga_number%, data goto L31790
                                                      /* (EWD002) */
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

L31790:   hex% = 0%                                   /* (EWD002) Begin */
          hex% = 65%
/* (AWD006) */
             pos% = 21%
L31826
             if str(g_rec$,pos%+1%,1%) <= "9" then goto L31825
                  pos% = pos% + 1%
                  goto L31826
L31825
REM          if str(g_rec$,22%,3%) <> "999" then goto L31830
          if str(g_rec$,23%,2%) <> "99" then goto L31830
          
          if str(g_rec$,21%,1%) = "9" then goto L31810

             get str(g_rec$,pos%,4%), using L31820, hex%
L31820:             FMT BI(1)
             hex% = hex% + 1%

REM          if hex% = 91% then goto L31840
          if hex% <> 91% then goto L31810
                pos% = pos% + 1%
                hex% = 65%              /* reset back to A             */
                                        /* using DEC to CHAR chart    */
L31810:   put str(g_rec$,pos%,1%) using L31820, hex%

L31830:   convert str(g_rec$,pos%+1%,25%-pos%) to rga_number%, data goto L31780

          rga_number% = rga_number% + 1%
          if rga_number% > 99 then rga_number% = 0%

          convert rga_number% to str(g_rec$,pos%+1%,25%-pos%), pic(00)

          rga_number$ = str(g_rec$,21%,4%)

REM L31840   if hex% = 91% then str(g_rec$,21%,4%) = "0000"

          put #3, using L31640, g_rec$

          rewrite #3

        return                                     /* (EWD002) End   */

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
L32020:     call "APCRG21B"   (rga_cuscode$,       /* RGA Customer No. */~
                               rga_number$,        /* RGA No.          */~
                               rga_item$,          /* RGA Item         */~
                               proc%,              /* Process Flag     */~
                               #2,                 /* APCRGADT Channel */~
                               #3,                 /* GENCODES Channel */~
                               #4,                 /* CUSTOMER Channel */~
                               #5,                 /* TXTRGA   Channel */~
/* PAR000 */                   #63     )           /* BCKSUBPR Channel */

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
            if rga_max% > 4%         then L42240
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


        show_inv_no                                 /*  (EWD002) - Begin */
            gosub set_pfinv

            accept                                                       ~
                at (01,70), fac(hex(8c)),   date$               , ch(10),~
                at (01,02), "Fax Number Entry"                  ,        ~
                at (02,02), fac(hex(94)),   errormsg$           , ch(60),~
                at (03,02), "RGA No.   :"                       ,        ~
                at (03,14), fac(hex(84)), rga_number$           , ch(04),~
                at (03,31), "RGA Status:"                       ,        ~
                at (03,43), fac(hex(84)),   rga_hd_status$      , ch(02),~
                at (03,47), fac(hex(84)),   status_desc$        , ch(30),~
                                                                         ~
                at (04,02), "Customer  :"                       ,        ~
                at (04,14), fac(hex(84)), rga_cuscode$          , ch(09),~
                at (04,31), fac(hex(84)),   cus_desc$           , ch(32),~
                                                                         ~
                at (05,02), "'RG' Flag :"                       ,        ~
                at (05,14), fac(hex(84)), rga_rg$               , ch(01),~
                                                                         ~
                at (06,02), "Authorized:"                       ,        ~
                at (06,14), fac(hex(84)), rga_auth_id$          , ch(03),~
                                                                         ~
                at (07,02), "Filed Date:"                       ,        ~
                at (07,14), fac(hex(84)), rga_filed_dte$        , ch(08),~
                at (07,31), "Entry Date:"                       ,        ~
                at (07,43), fac(hex(84)),   rga_dte$            , ch(08),~
                at (07,58), "Prev. RGA No.:"                    ,        ~
                at (07,73), fac(hex(84)),   prev_number$        , ch(04),~
                                                                         ~
                at (08,02), "RGA Text  :"                       ,        ~
                at (08,14), fac(hex(84)), rga_hd_txt$           , ch(01),~
                                                                         ~
                                                                         ~
                at (10,20), "Fax Number: "                      ,        ~
                at (10,40), fac(hex(82)),   inv_no$             , ch(11),~
                at (11,20), "Attention:  "                      ,        ~
                at (11,40), fac(hex(80)),   attmsg$             , ch(30),~
                                                                         ~
                at (11,20), "Attention:  "                      ,        ~
                at (11,40), fac(hex(80)),   attmsg$             , ch(30),~
                                                                         ~
                at (12,20), "Notes    :  "                      ,        ~
                at (13,20), fac(hex(80)),   fax_note$(1%)       , ch(50),~
                                                                         ~
                at (14,20), fac(hex(80)),   fax_note$(2%)        , ch(50),~
                                                                         ~
                at (15,20), fac(hex(80)),   fax_note$(3%)       , ch(50),~
                                                                         ~
                at (16,20), fac(hex(80)),   fax_note$(4%)       , ch(50),~
                                                                         ~
                at (17,20), fac(hex(80)),   fax_note$(5%)       , ch(50),~
                                                                         ~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               inv_no% = 0%
               convert inv_no$ to inv_no%, data goto invalid_inv_no

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        invalid_inv_no
            errormsg$ = " Invalid Fax Number? "
        goto show_inv_no


        set_pfinv
            inpmessage$ = "Enter a Valid Fax Number and Attention Message?"
            pf$(1%)= "                                        " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(ffffffffffffffffffffffffffffffff00)
        return                                   /*  (EWD002)    -  END  */


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
L50160:     if rga_number$ = " "      then L50250
            if edit% <> 1%            then L50270

        load_screen
            gosub dataload
                if rec% <> 1%         then L50280
                fieldnr% = 6%
                gosub lookup_customer

L50250:     gosub lookup_status

L50270: return
L50280:     errormsg$ = "(Error) - Invalid RGA Number (Required)?"
            init(" ") rga_number$
        return

        REM Customer No.                        RGA_CUSCODE$
        lookup_customer
L50340:     read #4,key = rga_cuscode$, using L50360, cus_desc$, ~
                ship_to$(), inv_no$, sku_code$, eod goto L50390
                                                       /*  (EWD002)  */
L50360:         FMT POS(10), CH(30), POS(253), 6*CH(30),         ~
                    POS(820), CH(11), POS(1000), CH(3)

        return
L50390:     errormsg$ = "(Error) - Invalid Customer No. (Required)?"
            init(" ") rga_cuscode$, cus_desc$
        return

        REM RGA/RG Flag                         RGA_RG$
L50440:     if rga_rg$ = " "          then L50480
            if rga_rg$ = "Y"                                             ~
            or rga_rg$ = "N"          then L50490
            errormsg$ = "(Error) - Invalid RGA/RG Flag (Y/N)?"
L50480:     init("N") rga_rg$
L50490: return

        REM Authorizing Userid                  RGA_AUTH_ID$
L50520:     if rga_auth_id$ = " "     then rga_auth_id$ = userid$
            read #12,key = rga_auth_id$, eod goto L50560

            goto L50580
L50560:     errormsg$ = "(Error) - Invalid Auth. Userid  (Required)?"
            init(" ") rga_auth_id$
L50580: return

        REM RGA Filed Date                      RGA_FILED_DTE$
L50610:     if str(rga_filed_dte$,1,6) = blankdate$ then rga_filed_dte$ = " "
            if rga_filed_dte$ = " " then goto L50660

                if rga_hd_status$ <> "05" then L50670
                    call "DATEOK" (rga_filed_dte$, date%, errormsg$)
                    if date%           =  0%  then L50670
L50660:     return

L50670:     errormsg$ = "(Error) - Invalid Filed Date ?"
            init(" ") rga_filed_dte$
            return

        REM RGA Header Text Flag                RGA_HD_TXT$
L50720: text_check
            gosub'099(rga_hd_desc_txt$)

            if txt%        =  1%      then rga_hd_txt$ = "Y"
            if rga_hd_txt$ = "Y"      then L50790
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
          if str(g_rec$,23%,3%) <> "000" then goto L60170
          
          if str(g_rec$,22%,1%) = "A" then goto L60180
          if str(g_rec$,21%,4%) = "0000" then goto L60170
             get str(g_rec$,22%,4%), using L60190, hex%
L60190:             FMT BI(1)
             hex% = hex% - 1%

          put str(prev_number$,1%,1%) using L60190, hex%
 
L60170:   convert str(g_rec$,23%,2%) to prev_number%, data goto L31780

          prev_number% = prev_number% - 1%
          if prev_number% = -1% then prev_number% = 99

          convert prev_number% to str(prev_number$,3%,2%), pic(00)


L60180:   
REM          if str(g_rec$,21%,4%) = "0000" then str(prev_number$,1%,4%) = "Z999"
          if str(g_rec$,21%,1%) = "A000"    then str(prev_number$,1%,4%) = "9999"
          if str(g_rec$,21%,1%) = "ZA00"    then str(prev_number$,1%,4%) = "Z999"
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

        ask_fax                                     /*  (EWD002) - Begin */
           comp% = 2%
           hdr$     = "*******   (FAX)  (FAX)  (FAX)    *******"
           msg$(1%) = " - - - Do You Want to Fax this RGA ??  - - "
           msg$(2%) = " Press Enter to Fax or Any other Key to Abort! "
REM        msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%))
           if comp% <> 0% then return

            a$ = all(hex(20))
 	    max_lines%      = 34%
            date$           =  date
            library$        = "APCDATA "
            volume$         = "CARLOS"
            call "DATEFMT"  (date$)
            counter%, error%, page_nbr% = 0%
            gosub set_file_name
                if error% <> 0% then exit_program
            gosub open_file
REM            gosub setup_fax
            gosub print_heading
            gosub print_detail
            gosub setup_fax

            counter%       = 0% : fx1% = 0% : fx2% = 0%
            if error%      = 1% then return
            close #13
            if error% <> 0% then delete_fax

            call "LINK" addr(script$, fx1%, fx2%)
                if fx1%    > 0% then error% = 4%

        delete_fax
            call "FILEBGON" (#13)          /* Scratch Fax File        */
L60470:     read    #3,hold,key = fax_key$, eod goto L60480
            put     #3, using L60410, "-"
            rewrite #3

        return
L60480:     counter%       = counter% + 1%
            if counter%    < 4% then L60470
            error%         = 6%
     
        return

        set_file_name
            init(" ") fax_key$, desc$, file$, script$
            str(fax_key$,1%,9%)   = "PLAN FAXR"
            str(fax_key$,10%,15%) = "FAXRGA"

        check_next_file
            read #3,hold,key > fax_key$, using L60400, fax_key$, desc$,  ~
                eod goto check_next_done
L60400:         FMT CH(24), CH(30)
            if str(fax_key$,1%,15%) <> "PLAN FAXRFAXRGA" then            ~
                    goto check_next_done
            if str(desc$,1%,1%) <> "-"                   then            ~
                    goto check_next_file
            put #3, using L60410, "*"
            rewrite #3
L60410:         FMT POS(25), CH(1)

            file$       = str(fax_key$,10%,8%)
            script$     = str(desc$,3%,8%)
        return

        check_next_done
            counter%    = counter% + 1%
            if counter% < 4% then goto set_file_name
            error%      = 1%                       /* EXIT TRY LATER */
        return

        open_file
            open nodisplay #13, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return


        setup_fax
	    /* EWD RIGHTFAX */
            gosub show_inv_no
            init(" ") company$                                   /* (EWD003) */
            call "COMPNAME" (12%, company$, ret%)                /* (EWD003) */
            init(" ") attn$
            if str(attn$,1%,60%) = " " then attn$ = company$     /* (EWD003)  */
            a$ = all(hex(20))
            a$ = "<FROMNAME:" & attn$  & ">"        
            gosub print_line

            init(" ") attn$
            if str(attn$,1%,60%) = " " then attn$ = ship_to$(1%) 
            a$ = all(hex(20))
            a$ = "<TONAME:" & attn$  & ">"        
            gosub print_line

            init(" ") attn$
            if str(attn$,1%,60%) = " " then attn$ = attmsg$
            a$ = all(hex(20))
            a$ = "<TOCOMPANY:" & " Attn:  " & attn$  & ">"        
            gosub print_line

            a$ = all(hex(20))
*            a$ = "<TOFAXNUM: 764-1567>"    /*  EWD Fax No. for Testing */
            fax_phone$ =  inv_no$
            call "SPCSMASH" (fax_phone$)                   
            if len(fax_phone$) = 10 then fax_phone$ = "1" & fax_phone$
            a$ = "<TOFAXNUM: " & fax_phone$ & ">"
            gosub print_line                               

            a$ = all(hex(20))
            a$ = "<NOTE:   RETURN   GOODS   AUTHORIZATION - Please Keep following Information>"
            gosub print_line

            for nl% = 1% to 5%
              call "SPCESMSH" (fax_note$(nl%), 2%)
              if len(fax_note$(nl%)) < 5%  then L60415
                 a$ = all(hex(20))
                 a$ = "<NOTE: " & fax_note$(nl%) & ">"
                 gosub print_line
L60415:     next nl%  	

            a$ = all(hex(20))
* (AWD005)   
            if schema% = 1% then a$ = "<FORMTYPE: RGA>"
            if schema% = 2% then a$ = "<FORMTYPE: NERGA>"
            gosub print_line

        return

        print_heading
            page_nbr%       = page_nbr% + 1%

	    /* EWD RIGHTFAX */
	    if page_nbr%    = 1%                    then L60440

            a$              = all(hex(20))
            str(a$, 8%,58%) = ">>> C O N T I N U E D <<<"
            str(a$,71%,10%) = "CONTINUED"
            gosub print_line

            a$              = all(hex(20))  
            for pp%         = 1% to (34% - nbr_lines%)
                gosub print_line        /*^EWD RightFax*/

            next pp%
    


	    /* EWD RIGHTFAX for bl%         = 1% to 2%		 */
REM            for bl%         = 1% to 4%
REM                gosub print_line
REM            next bl%
L60440:
            a$ = all(hex(20))
            for bl% = 1% to 4%
                gosub print_line
            next bl%
            str(a$,50%,6%) = rga_number$
            str(a$,63%,8%) = rga_dte$
            convert page_nbr% to page_nbr$, pic(####)

            str(a$,74%, 4%) = page_nbr$
            gosub print_line
            a$              = all(hex(20))
            for bl%         = 1% to 2%
                gosub print_line

            next bl%

            exp_dte$ = rga_dte$
            call "DATUNFMT" (exp_dte$)

            call "DATE" addr("G+", exp_dte$, 30%, exp_date$, err%)
            if err% = 0% then exp_dte$ = exp_date$

            call "DATEFMT"(exp_dte$)

            str(a$,44%,16%) =  table_po$(1%)  
            str(a$,61%, 9%) =  rga_cuscode$   
            str(a$,70%,10%) =  exp_dte$       
            gosub print_line

            a$              = all(hex(20))
            for bl%         = 1% to 3%
                gosub print_line

            next bl%

            for st%             = 1% to 6%
                call "SPCESMSH" (ship_to$(st%), 2%)
                a$              = all(hex(20))
                str(a$,15%,31%) = ship_to$(st%)
                gosub print_line

            next st%

            a$              = all(hex(20))
            for bl%         = 1% to 3%
                gosub print_line

            next bl%

            str(a$, 5%,20%) = table_so$(1%)
            str(a$,30%,20%) = " Next Atrium Truck "          /* (EWD003) */
            gosub print_line

            a$              = all(hex(20))
            for bl%         = 1% to 3%
                gosub print_line

            next bl%

            nbr_lines%      =  0%
REM            if page_nbr%    >  1%                   then return
        return

        print_detail
            tot_credit, table_credit = 0.00
            init(" ") tot_credit$
            for p% = 1% to rga_max%
                if (nbr_lines% + 3%) > max_lines% then gosub print_heading
                gosub build_print
                call "STRING" addr ("RJ", table_credit$(p%),      ~
                                       len(str(table_credit$(p%))))
                a$              = all(hex(20))
                str(a$, 3%, 3%) = table_item$(p%)
                str(a$, 7%, 3%) = table_line$(p%)
                str(a$,12%,52%) = apc$(1%)
                str(a$,71%,10%) = table_credit$(p%)
                gosub print_line

                a$              = all(hex(20))
                str(a$,12%,52%) = apc$(2%)
                gosub print_line

                if sku_code$ <> " " and str(rga_cuscode$,1%,2%) = "LO"    ~
                   then gosub lookup_sku

                gosub print_detail_text
                convert table_credit$(p%) to table_credit, data goto L60490

                tot_credit = tot_credit + table_credit
                a$              = all(hex(20))
                gosub print_line
                nbr_lines% = nbr_lines% + 3%
L60490:     next p%

            if (nbr_lines% + 3%) > max_lines% then gosub print_heading
            a$     = all(hex(20))
            str(a$,4%,60%)  = "____________________________________________________________"
            str(a$,68%,12%) = "____________"    
            gosub print_line
            nbr_lines% = nbr_lines% + 1%

            gosub print_header_text
            a$     = all(hex(20))
            str(a$,4%,60%)  = "|__________________________________________________________|"
            nbr_lines% = nbr_lines% + 1%            
            gosub print_line
            gosub print_ending
        return

        lookup_sku
              sku_no$ = "99999"
              sku_key$ = all(hex(00))
              str(sku_key$,1%,3%)  = sku_code$
              str(sku_key$,4%,25%) = table_part$(p%)
              read #15,key 1% = sku_key$, using L60550, sku_no$,          ~
                                                       eod goto no_sku
L60550:          FMT POS(4), CH(25)

        no_sku
            a$              = all(hex(20))
            str(a$,12%,52%) =  "SKU- " & str(sku_no$,1%,10%) 
            gosub print_line
            nbr_lines% = nbr_lines% + 1%
        return

        print_detail_text
                if table_desc$(p%) = hex(ffffffff)      then return
                if table_desc$(p%) = " "                then return
                txt%            = (page_nbr% * 100%) + nbr_lines%
                comp%           = 0%
                text_id$ = all(hex(00))
                str(text_id$,2%,3%) = str(table_desc$(p%),1%,3%)
                textid$ = all(hex(00))
                textid$ = text_id$
                gosub get_text
                call "SPCESMSH" (fax_text$(1%), 2%)
                call "SPCESMSH" (fax_text$(2%), 2%)
                if len(fax_text$(1%))   < 5          then L60450
                     a$     = all(hex(20))
                     str(a$,12%,70%) = "TXT: " & str(fax_text$(1%),1%,70%)
                     gosub print_line
                     nbr_lines% = nbr_lines% + 1%
                     if nbr_lines%  > max_lines% then gosub print_heading
L60450:         if len(fax_text$(2%))   < 5          then L60460
                     a$             = all(hex(20))
                     str(a$,12%,70%) = "TXT: " & str(fax_text$(2%),1%,70%)
                     gosub print_line
                     nbr_lines% = nbr_lines% + 1%
                     if nbr_lines% > max_lines%  then gosub print_heading

L60460:     REM if txt% = (page_nbr% * 100%) + nbr_lines% then return
REM            a$ = all(hex(20))
REM            gosub print_line
        return

        print_header_text
                if rga_hd_desc_txt$ = hex(ffffffff)      then return
                if rga_hd_desc_txt$ = " "                then return
                comp%          = 0%
                textid$ = all(hex(00))
                textid$ = rga_hd_desc_txt$
                gosub get_text
                
               for p% = 1% to 7%
                  call "SPCESMSH" (fax_text$(p%), 2%)
                  if len(fax_text$(p%)) < 5% and p% <> 1% then L60500
                     a$     = all(hex(20))
                     str(a$,4%,60%) = "| TXT: " & str(fax_text$(p%),1%,50%) & "  |"
                     if p% = 1% then gosub add_credit
                     gosub print_line
                     nbr_lines% = nbr_lines% + 1%
                     if nbr_lines%  > max_lines% then gosub print_heading
L60500:         next p%

        return

        add_credit
            convert tot_credit to tot_credit$, pic(##,###,##0.00-)
            call "STRING" addr ("RJ", tot_credit$, len(str(tot_credit$)))
            str(a$,66%,14%) = tot_credit$
        return
        
        get_text
           init(" ") fax_text$()
           text_key$ = all(hex(00))
           str(text_key$,1%,1%) = "M"
           str(text_key$,2%,3%) = "   "
           str(text_key$,5%,4%) = textid$
           str(text_key$,9%,1%) = "1"
           s_key$               = str(text_key$,1%,9%)
           read #5,key > text_key$, eod goto L60520
             get #5, using L60510, text_key$, fax_text$()
L60510:           FMT CH(11), POS(65), 7*CH(50)
           if s_key$ = str(text_key$,1%,9%) then return
           init(" ") fax_text$()
L60520: return

        print_ending
          if (nbr_lines% + 11%) > max_lines% then gosub print_heading
          a$              = all(hex(20))
          gosub print_line
          a$              = all(hex(20))
          str(a$, 10%,40%)= "Credit will be issued upon return of    "
          gosub print_line
          str(a$, 10%,40%)= "product to " & company$                    /* (EWD003) */
          gosub print_line
          gosub print_line
          str(a$, 10%,40%)= "Trailer Number:_________________________"
          gosub print_line
          gosub print_line                
          str(a$, 10%,40%)= "No. Windows Returned:___________________"
          gosub print_line
          gosub print_line
          str(a$, 10%,40%)= "Customer Signature:_____________________"
          gosub print_line
          gosub print_line
          str(a$, 10%,40%)= "Driver Signature:_______________________"
          gosub print_line
        return


        build_print                     /* PRINT DESCRIPTIONS       */
            err%            = 0%
            if len(table_part$(p%)) < 19%        then return
            init(" ") apc_scr$, apc_prt$, apc_sze$

            call "APCDESCR"  (table_part$(p%), apc_scr$, apc_prt$, apc_sze$,~
                              #14, err%)
            apc$(1%)        = apc_prt$
            apc$(2%)        = "WIDTH: "   & str(apc_sze$, 1%,7%) &       ~
                              " HEIGHT: " & str(apc_sze$,11%,6%)
            s_23% = 0%                       
            s_23m$ = str(table_part$(p%),1%,3%)
            
            
            init(" ") s_prv$, s_23$                   /* s_1$ Passed In  */
            prv% = 1%                                 /* Use BCKLINES    */
            call "APCPRZSB" (prv%, s_1$, rga_cuscode$, s_23m$, " ",      ~
                                   " ", s_prv$, s_23$, s_23%,            ~
                                   #4, #3, " ", " ", x_er% )
            if x_er% <> 0% then return
               str(apc$(1%),1%,8%) = s_23$
        return                                  


        print_line                                   /*  (EWD004)  */
            write #13, using L60430, a$, hex(0D), eod goto L60420
L60430:    FMT CH(79), CH(01)
            a$ = all(hex(20))
        return
L60420:     error% = 5%
            a$ = all(hex(20))
        return clear all                     /*  (EWD002)   -  End   */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*
        exit_program                                      /*  (EWD002)  */
            if error% <> 0% then goto L65000
            if edit% = 2% and rga_max% <> 0% then gosub ask_fax

L65000:     call "SHOSTAT" ("One Moment Please")

        end
