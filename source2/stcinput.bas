        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  S        T    C   C    I    NN  N  P   P  U   U    T     *~
            *   SSS     T    C        I    N N N  PPPP   U   U    T     *~
            *      S    T    C   C    I    N  NN  P      U   U    T     *~
            *   SSS     T     CCC   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCINPUT - Driver program for Creation and Management of  *~
            *            Standard Cost Sets.                            *~
            *----------------------------------------------------------Q*~
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
            * 03/24/87 ! Original                                 ! ERN *~
            * 04/20/88 ! Added Copy Function and fixed mapping    ! TLJ *~
            * 05/09/88 ! Now calls STCENGSB for better visibility ! HES *~
            * 06/03/88 ! Remove Phantom Net Change Roll Up Option ! HES *~
            * 08/30/89 ! Eliminate COPY 7.18.84 generated OPEN    ! KAB *~
            *          ! GETPARM when source file is empty.       !     *~
            * 11/14/90 ! Added MGTFCTR1 to call to STCROLUP.      ! JDH *~
            * 06/14/91 ! Added MGTFCTR1 to call to STCHNYSB.      ! KAB *~
            * 04/07/92 ! PRR 11819. Disable DELETE of Current Set.! JDH *~
            *          ! PRR 10593. Descr not overridden w/copy.  !     *~
            * 05/31/94 ! PRR 13167 - OUTVOL can't be blank to Copy! RJH *~
            * 05/31/94 ! PRR 12903,12961 - Add PFKEYS to Print or ! RJH *~
            *          !  Delete zero costed items fm a non Frozen!     *~
            *          !  Cost Set.                               !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            bfac$(12)1, dfac$(12)1,      /* Bucket FACS                */~
            bom$3,                       /* BOM ID                     */~
            buckets$2, buckets_msg$20,   /* Number of Cost Buckets     */~
            bucket_descrs$(12)20,        /* Cost Bucket Descriptions   */~
            bucket_ids$(12)10,           /* Cost Bucket IDs            */~
            bucket_nrs$(12)2,            /* Bucket Numbers             */~
            company$60,                  /* Company or Division Name   */~
            copy_from$(3)8,              /* Copy From Cost Sets        */~
            copy_id$(3)4,                /* Internal IDs for Copy Srce */~
            copy_map%(3,12),             /* Copy Mapping Definitions   */~
            copy_map$(3,12)2,            /*                            */~
            costs(12), costs$96,         /* A place to keep costs      */~
            currset$8, currsetid$4,      /* Current Cost Set           */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dp$2,                        /* Default Bucket             */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            from_buckets%(3),            /* Number of Buckets in Srce  */~
            from_global$1,               /* Copy Set Global Flag       */~
            frozen$8, frzn_by$3,         /* Frozen on and By           */~
            global$1, global_flag$1,     /* Global Required Flag       */~
            hdr2$(3)20,                  /* Screen 2 Column Headings   */~
            hdr3$(3)36,                  /* Screen 3 Column Headings   */~
            i$(24)80,                    /* Screen Image               */~
            idx2%(2), idx6%(6),          /* Bucket Indices             */~
            in_file$8,                   /* For Copies                 */~
            inpmessage$79,               /* Informational Message      */~
            keys$22,                     /* Available PF Keys          */~
            labor_id$10, labor_descr$20, /* Default Labor bucket       */~
            last_global$6,               /* Last Global Roll-up on     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Hdr  */~
            mapping%(12),                /* Fold-in Mapping            */~
            mfac$(3,12)1,                /* Copy Mapping FACs          */~
            net_chng$6,                  /* Last Net Change Roll-up on */~
            new_set$8,                   /* Cost Set, Copied too       */~
            old_costs(12),               /* A place for old costs      */~
            old_from$8,                  /* Previous Copy Set Value    */~
            opts_prompt$16, opt$(11)50,  /* Edit Mode Options          */~
            out_file$8, out_vol$6,       /* For Copies                 */~
            ovrhd_id$10, ovrhd_descr$20, /* Default lbr ovrhd bucket   */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Number Description    */~
            pf$(3)79,                    /* PF Key Descriptors         */~
            plowkey$99,                  /* Miscellaneous Plow Key     */~
            prnt_rmv$7,                  /* Print Remove var           */~
            prnt_tot$12,                 /* Print Totals var           */~
            purch_id$10, purch_descr$20, /* Default purchases bucket   */~
            readkey$99,                  /* Miscellaneous Read Key     */~
            rollup$1,                    /* Type of rollup flag        */~
            rptid$8,                     /* Report Id                  */~
            rte$3,                       /* Route (not root!) ID       */~
            sav_descr$30,                /* Cost Set Description COPY  */~
            set$8, saveset$8,            /* Cost Set ID                */~
            set_descr$30,                /* Cost Set Description       */~
            setid$4, setid_msg$20,       /* Internal Set ID            */~
            text$(560,1)70,              /* Text Array                 */~
            textid$4,                    /* Cost Set Text ID           */~
            textmsg$79,                  /* Message to text routine    */~
            time$8,                      /* System Time                */~
            userid$3,                    /* Current User Id            */~
            zeros$96,                    /* A bunch of HEX(00)s        */~
            zzov$6                       /* Extracted OUTVOL           */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! GENCODES ! System General Codes file.               *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! BOMMASTR ! BOM relationship file                    *~
            * #6  ! WCMASTR  ! Workcenter Master File                   *~
            * #7  ! ENGMASTR ! Engineering Master Filer                 *~
            * #8  ! STCBOMXF ! Standard Cost / BOM-RTE Cross Reference  *~
            * #9  ! TXTFILE  ! System Text File                         *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #11 ! STCDETAL ! Standard Cost Details                    *~
            * #12 ! STCWCACT ! Standard Cost Work Center / Activities   *~
            * #13 ! STCLABOR ! Standard Costing Labor Standards         *~
            * #14 ! STCMAPNG ! Standard Cost Set Mappings               *~
            * #15 ! STCCHNGS ! Tickler file of changed components       *~
            * #16 ! RTEMASTR ! Route Master File                        *~
            * #17 ! MGTFCTR1 ! Management Factors - Billing & Transfer  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2 , "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3 , "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4 , "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1, keylen = 25,                        ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos =  90, keylen = 4, dup

            select #5 , "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen = 56

            select #6 , "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #7 , "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29

            select #8 , "STCBOMXF",                                      ~
                        varc,     indexed,  recsize = 72,                ~
                        keypos =   29, keylen =  33,                     ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select #9 , "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos  = 1, keylen = 11

            select #10, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

            select #11, "STCDETAL",                                      ~
                        varc,     indexed,  recsize =  340,              ~
                        keypos = 1,    keylen = 28

            select #12, "STCWCACT",                                      ~
                        varc,     indexed,  recsize =  381,              ~
                        keypos =  1,   keylen = 8

            select #13, "STCLABOR",                                      ~
                        varc,     indexed,  recsize =  323,              ~
                        keypos =  1,   keylen = 4

            select #14, "STCMAPNG",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =  1,   keylen =  8

            select #15, "STCCHNGS",                                      ~
                        varc,     indexed,  recsize =   26,              ~
                        keypos =    1, keylen =  26

            select #16, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =   5, keylen = 31,                       ~
                        alt key  1, keypos = 1, keylen = 35

            select #17, "MGTFCTR1",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 19,                      ~
                        alt key 1, keypos = 9,    keylen = 11, dup

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#2 , fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (#3 , fs%( 3), f2%( 3), 100%, rslt$( 3))
            call "OPENCHCK" (#4 , fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (#5 , fs%( 5), f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (#6 , fs%( 6), f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (#7 , fs%( 7), f2%( 7),   0%, rslt$( 7))
            call "OPENCHCK" (#8 , fs%( 8), f2%( 8),2000%, rslt$( 8))
            call "OPENCHCK" (#16, fs%(16), f2%(16),   0%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17),   0%, rslt$(17))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            call "COMPNAME" (12%, company$, ret%)
               ret% = ret%

            date$  = date  :  call "DATEFMT" (date$)
            zeros$ = all(hex(00))
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            str(line2$,62) = "STCINPUT: " & str(cms2v$,,8)

            hdr2$(1) = "##"
            hdr2$(2) = "Bucket ID"
            hdr2$(3) = "Bucket Description"

            hdr3$(1) = "Cost Components"
            hdr3$(2) = "From Set"
            hdr3$(3) = "  1  2  3  4  5  6  7  8  9 10 11 12"

            call "EXTRACT" addr("OV", out_vol$)

            call "STCSETID" (1%, #2, currset$, currsetid$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Get which set to maintain and define if that set is new.  *~
            *************************************************************

        inputmode
            gosub init_for_input
            gosub init_for_input2

*        First Screen.  Get Cost Set ID and Description.
            for fieldnr% = 1% to  2%
                gosub'051(fieldnr%)
                     if enabled% = 0% then L10180
                gosub set_pf1_input
L10140:         gosub'101(fieldnr%)
                     if keyhit% =  1% then gosub startover
                     if keyhit% =  3% then gosub copy_set
                     if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then L10140
L10180:         if keyhit% <> 3% then gosub'151(fieldnr%,0%) /* Valid? */
                      if errormsg$ <> " " then L10140
            next fieldnr%

*        Second Screen.  Get Cost Set Bucket Descriptions.
            for fieldnr% = 1% to 2%
L10240:         gosub'052(fieldnr%)
                      if enabled% = 0 then L10370
                gosub set_pf2_input
L10270:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10350
L10300:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10270
                         if fieldnr% = 1% then L10240
                         goto L10300
L10350:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10270
L10370:         gosub'152(fieldnr%, 1%)
                      if errormsg$ <> " " then L10270
            next fieldnr%

L10410:     inpmessage$ = edtmessage$
            gosub set_pf2_display
            gosub'102(0%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then       L10590
                if keyhit% <>  0% then       L10410
            fieldnr% = max(1, cursor%(1) - 4%)
            if fieldnr% > 1% then fieldnr% = 2%

            gosub'052(fieldnr%)
            gosub set_pf2_edit
L10520:     gosub'102(fieldnr%)
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then       L10520
            gosub'152(fieldnr%, 2%)
                if errormsg$ <> " " then L10520
            goto L10410

L10590
*        Third Screen.  Get Initial Sources.
            readkey$ = "STC.HDR."
            call "PLOWNEXT" (#2, readkey$, 8%, f1%(2))
            if f1%(2) = 1% then L10710
L10630:         keyhit% = 2%
                call "ASKUSER" (keyhit%, "CREATE NEW SET???",            ~
                     "Press RETURN to Continue and Create this Cost Set",~
                     "-OR-", "Press PF-1 to Restart Input...")
                if keyhit% =  0% then create_new_set
                if keyhit% =  1% then inputmode
                goto L10630

L10710:     for l% = 1% to 3%
                gosub get_fields
            next l%

L10750:     inpmessage$ = edtmessage$
            gosub set_pf3_display
            gosub'103(0%)
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then       create_new_set
                if keyhit% <>  0% then       L10750
            l% = max(1%, cursor%(1) - 7%)
            if l% > 3% then l% = 3%
            gosub get_fields
            goto L10750

          get_fields
            for fieldnr% = 1% to 2%
                gosub'053(fieldnr%)
                     if enabled% = 0% then L10960
                gosub set_pf3_edit
L10910:         gosub'103(fieldnr%)
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  0% then       L10910
                gosub'153(fieldnr%)
                     if errormsg$ <> " " then L10910
L10960:     next fieldnr%
            return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Selector Screen for program options.                      *~
            *************************************************************

        editmode:
*        First Set Options Descriptions and PF Keys
            keys$ = hex(ff02030405060708090a0b0c0d0eff0f10191a1b1c00)
            opts_prompt$ = " Manage"
            if frozen$ <> " " then opts_prompt$ = " Display"
            readkey$ = "STC.HDR." & set$
            call "READ100" (#2, readkey$, f1%(2))
            get #2 using L11140, global$
L11140:         FMT POS(420), CH(1)
            global_flag$ = global$
                if global_flag$ = " " then global_flag$ = "N"
                if frozen$     <> " " then global_flag$ = "F"
            opt$( 2)= "( 2)" & opts_prompt$ & " Part Standard Costs"
            opt$( 3)= "( 3)" & opts_prompt$ & " Work Centers-Activities"
            opt$( 4)= "( 4)" & opts_prompt$ & " Labor Class Rates"
            opt$( 5)= "( 5) Manage Cost Bucket IDs and Descriptions"
            opt$( 6)= "( 6)" & opts_prompt$ & " Fold-in Mappings"
            opt$( 7)= "( 7) Flag Cost Set for Global Roll-up"
            opt$( 8)= "( 8) Perform Global Roll-up"
        REM OPT$( 9)= "( 9) Perform Net Change Roll-up"
            opt$(10)= "(10) Freeze Cost Set"
            opt$(11)= "(11) Build Purchase Part Costs/Set BOMs and RTEs"
            opts_prompt$ = "Program Options:"

            if frozen$ = " " then L11380
                opt$( 7)= " "
                opt$( 8)= "     The Cost Set was Frozen on " &  frozen$ &~
                          " by " & frzn_by$ & "."
                opt$( 9)= "     You may review the Cost Standards but you"
                opt$(10)= "     may not change them."
                opt$(11)= " "  :  str(keys$,11,1) = hex(ff)
                str(keys$,7,4) = hex(ffffffff)
                str(keys$,19%,2%) = hex(ffff)
                goto L11500

L11380:     if global$ = " " then L11440
                opt$( 7)= " "  :  str(keys$, 7,1) = hex(ff)
                opt$( 9)= " "  :  str(keys$, 9,1) = hex(ff)
                opt$(10)= " "  :  str(keys$,10,1) = hex(ff)
                goto L11500

L11440:     plowkey$ = hex(00)
            call "PLOWNEXT" (#15, plowkey$, 0%, f1%(15))
            if f1%(15) = 1% then L11480
                opt$( 9)= " "  :  str(keys$, 9,1) = hex(ff) : goto L11500
L11480:         opt$(10)= " "  :  str(keys$,10,1) = hex(ff)

L11500:     inpmessage$ = "Select Option Desired.  Press RETURN to edit"&~
                          " Cost Set Description."
            str(keys$, 9,1) = hex(ff)  /* Currently Dissabled */
            if set$ <> currset$ then L11520
                str(keys$,12%,1%) = hex(ff) : str(keys$,21%,1%) = hex(ff)
L11520:     gosub set_pf1_display
         options_screen:
L11540:     gosub'101(0%)
                errormsg$  =  " "
                if keyhit% =  0% then edit_descr
                if keyhit% =  2% then stchnysb
                if keyhit% =  3% then stcwcasb
                if keyhit% =  4% then stclbrsb
                if keyhit% =  5% then edit_cost_buckets
                if keyhit% =  6% then stcmapsb
                if keyhit% =  7% then flag_for_global
                if keyhit% =  8% then global_rollup
                if keyhit% =  9% then net_change_rollup
                if keyhit% = 10% then freeze_set
                if keyhit% = 11% then stcengsb
                if keyhit% = 12% then delete_files
                if keyhit% = 26% then print_zero_costs
                if keyhit% = 27% then remove_zero_costs
                if keyhit% = 28% then delete_all
                if keyhit% = 25% then manage_text
                if keyhit% = 16% then inputmode
                goto L11540


        edit_descr:
            gosub'051(2%)
            gosub set_pf1_edit
L11760:     gosub'101(2%)
                if keyhit%   <>  0% then L11760
            gosub'151(2%,0%)
                if errormsg$ <> " " then L11760
            gosub hold_header
            put #2 using L11820, set_descr$
L11820:         FMT POS(21), CH(30)
            rewrite #2
            goto editmode

        stchnysb:    call "STCHNYSB" (set$, set_descr$, global_flag$,    ~
                                      buckets%, bucket_ids$(),           ~
                                      bucket_descrs$(), #4, #5, #10,     ~
                                      #11, #15, #8, #14, #2, #9, #16,    ~
                                      #12, #13, #6, #17)
                     goto editmode

        stcwcasb:    call "STCWCASB" (set$, set_descr$, global_flag$,    ~
                                      buckets%, bucket_ids$(),           ~
                                      bucket_descrs$(), #6, #12, #3, #2, ~
                                      #13)
                     goto editmode

        stclbrsb:    call "STCLBRSB" (set$, set_descr$, global_flag$,    ~
                                      bucket_ids$(), bucket_descrs$(),   ~
                                      #2, #3, #13)
                     goto editmode

        stcengsb:    call "STCENGSB" (set$, #2, #10, #4, #8, #11, #5,    ~
                                      #16, #7)
                     goto editmode

        edit_cost_buckets:
            inpmessage$ = "Make changes and press PF-16 to return to" &  ~
                          " Options Screen."
            gosub set_pf2_edit
L12070:     gosub'102(2%)
                if keyhit% = 0% or keyhit% = 16% then L12090 else L12070
L12090:     gosub'152(fieldnr%, 2%)
                  if errormsg$ <> " " or keyhit% <> 16% then L12070
            gosub hold_header
            put #2 using L12130, bucket_ids$(), bucket_descrs$(),         ~
                                purch_dflt%, labor_dflt%, ovrhd_dflt%
L12130:         FMT POS(60), 12*CH(10), POS(180), 12*CH(20),             ~
                                                        POS(442), 3*BI(1)
            rewrite #2
            readkey$ = "STC.BD." & str(userid$) & "."
            call "DELETE" (#2, readkey$, 11%)
            for b% = 1% to buckets%
                readkey$ = "STC.BD." & str(userid$) & "." &              ~
                                                     str(bucket_nrs$(b%))
                write #2 using L12220, readkey$, bucket_ids$(b%), " ",    ~
                                             bucket_descrs$(b%), " "
L12220:              FMT CH(20), CH(10), CH(4), CH(250), CH(216)
            next b%
            goto editmode

        stcmapsb:    call "STCMAPSB" (set$, set_descr$, global_flag$,    ~
                                      bucket_ids$(), bucket_descrs$(),   ~
                                      buckets%, #14)
                     goto editmode

        flag_for_global:
L12320:     u3% = 2%
            call "ASKUSER" (u3%, "SET GLOBAL ROLLUP = REQUIRED",         ~
                     "Press RETURN to indicate that a Global Roll-up is",~
                     "required for this Cost Set.",                      ~
                     "-OR- Press PF-1 to return to Options Selector.")
            if u3%  = 1% then options_screen
            if u3% <> 0% then L12320
                gosub hold_header
                global$ = "Y"
                put #2 using L12420, global$
L12420:              FMT POS(420), CH(1)
                rewrite #2
                goto editmode

        global_rollup:
            rollup$ = "G":goto L12510

        net_change_rollup:
            rollup$ = "N"
L12510:     call "STCFOPEN" (set$, "URRRRU", #2, errormsg$,              ~
                                            #10, #11, #12, #13, #14, #15)
            if errormsg$ = " " then L12590
                call "ASKUSER" (2%, "STC FILES IN USE",                  ~
                          "The Cost Set is currently being used --",     ~
                          "Unable to perform Roll-up at this time.",     ~
                          "Press RETURN to Continue...")
                goto L12610
L12590:     call "STCROLUP" (set$, rollup$, #2, #5, #10, #12, #13,       ~
                                            #16, #6, #15, #4, #17)
L12610:     gosub reopen_cost_set
            goto editmode

        freeze_set:
            gosub hold_header
            if global$ = " " then L12710
                errormsg$ = "Set is flagged for a Global Rollup.  May" & ~
                            " not be frozen."
                call "STRTRLSE" addr(#2)
                goto editmode
L12710:     call "STCFOPEN" (set$, "RRRRRR", #2, errormsg$,              ~
                                            #10, #11, #12, #13, #14, #15)
            if errormsg$ = " " then L12800
                call "STRTRLSE" addr(#2)
                call "ASKUSER" (2%, "COST SET IN USE",                   ~
                          "The Cost Set is currently being used -",      ~
                          "Unable to Freeze the Set at this time.",      ~
                          "Press RETURN to Continue...")
                goto L13020
L12800:     plowkey$ = hex(00)
            call "PLOWNEXT" (#15, plowkey$, 0%, f1%(15))
            if f1%(15) = 0% then L12890
                call "STRTRLSE" addr(#2)
                call "ASKUSER" (2%, "ROLL-UP TRANSACTIONS PENDING",      ~
                          "Changes are pending that require a Roll-up",  ~
                          "to be done before the Set may be frozen.",    ~
                          "Press RETURN to Continue...")
                goto L13020
L12890:     u3% = 2%
            call "ASKUSER" (u3%, "* * * FREEZE COST SET * * *",          ~
                            "Press RETURN to FREEZE this Cost Set",      ~
                            "- OR -",                                    ~
                            "Press PF-1 to Return to Options Selector.")
            if u3% <> 1% then L12950
                gosub reopen_cost_set
                goto  options_screen
L12950:     if u3% <> 0% then L12890
                gosub hold_header
                frozen$  = date$
                frzn_by$ = userid$
                put #2 using L13000, date, userid$
L13000:              FMT POS(433), CH(6), CH(3)
                rewrite #2
L13020:         gosub reopen_cost_set
                readkey$ = "STC.HDR." & set$
                call "READ100" (#2, readkey$, f1%(2))
                goto  editmode

        delete_files:
L13080:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE ARCHIVED COST SET",             ~
                            "Note: References to Cost Set will Remain",  ~
                            "Press Return to DELETE Cost Set",           ~
                            "-OR- PF-1 to Abort Delete")
            if u3%  = 1% then editmode
            if u3% <> 0% then L13080
                gosub file_delete : if delete% <> 99% then editmode
                call "STRTRLSE" addr(#2)
                goto inputmode

        delete_all:
L13200:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE COST SET",                      ~
                            "Note: References to Set will be Removed",   ~
                            "Press Return to DELETE Cost Set",           ~
                            "-OR- PF-1 to Abort Delete")
            if u3%  = 1% then editmode
            if u3% <> 0% then L13200
                gosub file_delete : if delete% <> 99% then editmode
                plowkey$ = set$
                call "DELETE" (#8, plowkey$, 8%)
                gosub hold_header  :  delete #2
                call "TXTFUTIL" (#9, f2%(9), "DELE", textid$)
                goto inputmode

        file_delete:
            delete% = 0%
            gosub hold_header
            call "STCFOPEN" (set$, "RRRRRR", #2, errormsg$,              ~
                                            #10, #11, #12, #13, #14, #15)
            if errormsg$ = " " then L13470
                call "STRTRLSE" (#2)
                call "ASKUSER" (2%, "COST SET IN USE",                   ~
                          "The Cost Set is currently being used -",      ~
                          "Unable to Delete the Set at this time.",      ~
                          "Press RETURN to Continue...")
                gosub reopen_cost_set
                return
L13470:     for f% = 10% to 15% : call "FILEBGON" (#f%) : next f%
            delete% = 99%
            return

        manage_text:
            textmsg$ = "Cost Set " & set$ & ", '" & set_descr$ & "'"
            call "TXTINSUB" (#9, f2%(9), "022", textmsg$, textid$,       ~
                                                                 text$())
            gosub hold_header
            put #2 using L13570, textid$
L13570:         FMT POS(55), CH(4)
            rewrite #2
            call "TXTFUTIL" (#9, f2%(9), "TOS2", textid$)
            call "TXTFUTIL" (#9, f2%(9), "LOAD", textid$)
            goto editmode

        hold_header:
            readkey$ = "STC.HDR." & set$
            call "READ101" (#2, readkey$, f1%(2))
            get #2 using L13670, global$
L13670:         FMT POS(420), CH(1)
            return

        reopen_cost_set:
            call "STCFOPEN" (set$, "SSSSSS", #2, errormsg$,              ~
                                            #10, #11, #12, #13, #14, #15)
            if errormsg$ = " " then return
                call "ASKUSER" (2%, "FILE ERROR",                        ~
                                "Unable to re-open Cost Set (may be",    ~
                                "held by another user).",                ~
                                "Press RETURN to continue...")
                return clear all
                goto inputmode

        copy_set:
           sav_descr$ = set_descr$
           plowkey$ = all(" ")
           plowkey$ = "STC.HDR."
           call "PLOWCODE" (#2, plowkey$, set_descr$, 8%, 0.30, f1%(2))
           if f1%(2) <> 0% then L13890
              errormsg$ = "Must choose an EXISTING COST SET to copy"    &~
                          " from.  Copy CANCELLED."
              return
L13890:    new_set$ = set$
           set$ = str(plowkey$,9,8)      /* COST SET to Copy */
           u3% = 2%
           call "ASKUSER" (u3%, "COPY COST SET???",                      ~
                "Press RETURN to Continue and Copy the Cost SET " & set$ ~
                & ".", "-OR-", "Press PF-1 to Continue with INPUT...")
           if u3% <> 1% then L13965
              set_descr$ = sav_descr$
              set$ = new_set$ : return
L13965:    copy% = 1%
           gosub'151(1%,1%)
           copy% = 0%
           if errormsg$ <> " " then copy_error
           copy_from$(1), copy_from$(2), copy_from$(3) = set$
           for l% = 1% to 3%
              gosub'153(1%)
              if errormsg$ <> " " then copy_error
              gosub'153(2%)
              if errormsg$ <> " " then copy_error
              display% = 1%
           next l%
           display% = 0%
           set$ = new_set$
           goto create_new_set
        copy_error:
              errormsg$ = "Unable To COPY Cost Set " & set$ & "."
              set$ = new_set$
              set_descr$ = sav_descr$
              gosub init_for_input2
              return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Cost Set ID        */    ~
                              L20180          /* Cost Set Descr     */
            return

L20130
*        Def/Enable COST SET ID                 SET$
            inpmessage$ = "Enter Cost Set ID.  Leave Set ID blank to" &  ~
                          " list Cost Sets on file."
            return

L20180
*        Def/Enable COST SET DESCRIPTION        SET_DESCR$
            inpmessage$ = "Enter Cost Set Description."
            return

        set_pf1_input:
           pf$(1) = "(1)Start Over          (3)Copy Cost Set           "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           keys$  = hex(01ff03ffffffffffffffffff0dff0f10ffffff00)
           if fieldnr% <> 1% then L20310
                str(pf$(1),24,17) = " " : str(keys$,3,1) = hex(ff)
                return

L20310:         str(pf$(3),64) = " " : str(keys$,16,1) = hex(ff)
                return

        set_pf1_display:
           pf$(1) = "                                                 " &~
                    "              (13)Instructions"
           pf$(2) = "                 (26)Print 0 Costs   (12)Del Set " &~
                    "(Leave Ref)   (15)Print Screen"
           pf$(3) = "(25)Manage Text  (27)Remove 0 Costs  (28)Del Set " &~
                    "(-and- Ref)   (16)End Cost Set"
           if set$ <> currset$ then L20410
                str(pf$(2%),38%,24%) = " " : str(pf$(3%),38%,24%) = " "
L20410:    if frozen$ = " " then return
                str(pf$(2%),18%,18%) = " " : str(pf$(3%),18%,18%) = " "
           return



        set_pf1_edit:
           pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           keys$  = hex(ffffffffffffffffffffffff0dff0fffffffff00)
           return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L21130,             /* Number of Buckets  */~
                              L21180              /* Buckets IDs        */
            return

L21130
*        Def/Enable NUMBER OF COST BUCKETS      BUCKETS$
            inpmessage$ = "Enter Number of Buckets for this Cost Set" &  ~
                          " (1 - 12)."
            return

L21180
*        Def/Enable COST BUCKET IDs & DESCRS    BUCKET_IDS$()/_DESCRS$()
            inpmessage$ = "Enter Bucket IDs and Descriptions"
            return

        set_pf2_input:
           pf$(1) = "(1)Start Over       (4)Previous Field             "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "                             "
           pf$(3) = "                                    (15)Print Scre"&~
                    "en                           "
           keys$  = hex(01ffff04ffffffffffffffff0dff0fffffffff00)
           if fieldnr% > 1% then L21320
                str(pf$(1),20,18) = " " : str(keys$,4,1) = hex(ff)
L21320:    return

        set_pf2_display:
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "                             "
           pf$(3) = "                                    (15)Print Scre"&~
                    "en           (16)Continue... "
           keys$  = hex(01ffffffffffffffffffffff0dff0f10ffffff00)
           if onfile% = 0% then L21440
                str(pf$(1),,14) = " " : str(keys$,,1) = hex(ff)
L21440:    return

        set_pf2_edit:
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "                             "
           pf$(3) = "                                    (15)Print Scre"&~
                    "en           (16)Return      "
           keys$  = hex(01ffffffffffffffffffffff0dff0f10ffffff00)
           if onfile% = 1% then L21570
                str(pf$(3), 64) = " " : str(keys$,16,1) = hex(ff)
                goto L21580
L21570:         str(pf$(1),,14) = " " : str(keys$, 1,1) = hex(ff)
L21580:    return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L22130,             /* From set           */~
                              L22180              /* Copy Mapping       */
            return

L22130
*        Def/Enable FROM SET                    FROM_SET$
            inpmessage$ = "Enter Cost Set to Copy From -or- leave blank."
            old_from$   = copy_from$(l%)
            return

L22180
*        Def/Enable MAPPING                     COPY_MAP$()
            inpmessage$ = "Enter New Cost Buckets to copy/sum rates into."
            if copy_from$(l%) = " " then enabled% = 0%
            return


        set_pf3_display:
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Create Set  "
           keys$  = hex(01ffffffffffffffffffffff0dff0f10ffffff00)
           return

        set_pf3_edit:
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           keys$  = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        init_for_input:
            if set$ <> " " then                                          ~
                     call "STCFOPEN" (set$, "C", #2, errormsg$,          ~
                                            #10, #11, #12, #13, #14, #15)
            init(" ") errormsg$, inpmessage$, set$, set_descr$,          ~
                      setid$, setid_msg$, buckets$, buckets_msg$,        ~
                      bucket_ids$(), bucket_descrs$(), bucket_nrs$(),    ~
                      copy_from$(), copy_map$(), global$, net_chng$,     ~
                      last_global$, frozen$, frzn_by$, opts_prompt$,     ~
                      opt$(), purch_id$, purch_descr$, labor_id$,        ~
                      labor_descr$, ovrhd_id$, ovrhd_descr$
            purch_dflt% = 1%
            labor_dflt% = 2%
            ovrhd_dflt% = 3%
            textid$ = hex(ffffffff)
            call "TXTFUTIL" (#9, f2%(9), "INTL", textid$)
            readkey$ = "STC.BD." & str(userid$) & "." & hex(00)
            call "DELETE" (#2, readkey$, 11%)
            display% = 0%
            return

        init_for_input2  /* Don't want to do these from LOAD_DATA */
            copy% = 0%
            sav_descr$ = " "
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

        startover:
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Load Cost Set Header Data.                                *~
            *************************************************************
        load_data
            saveset$ = set$
            gosub init_for_input
            readkey$ = "STC.HDR." & saveset$
            call "READ100" (#2, readkey$, onfile%)
            get #2 using L30150, set$, set_descr$, setid$, textid$,       ~
                                buckets%, bucket_ids$(),bucket_descrs$(),~
                                global$, net_chng$, last_global$,        ~
                                frozen$, frzn_by$, purch_dflt%,          ~
                                labor_dflt%, ovrhd_dflt%
L30150:         FMT POS(9), CH(8), POS(21), CH(30), 2*CH(4), BI(1),      ~
                    12*CH(10), 12*CH(20), CH( 1), 3*CH(6), CH(3), 3*BI(1)
            if copy% = 1% and sav_descr$ <> " " then set_descr$ =        ~
                                                               sav_descr$
            sav_descr$ = " "
            call "STCFOPEN" (set$, "SSSSSS", #2, errormsg$,              ~
                                            #10, #11, #12, #13, #14, #15)
            if errormsg$ <> " " then return
            call "TXTFUTIL" (#9, f2%(9), "LOAD", textid$)
            setid_msg$ = "Internal ID = " & setid$
            convert buckets% to str(buckets_msg$,,2), pic(#0)
            buckets$ = buckets_msg$
            init(hex(8c)) bfac$()
            for b% = 1% to buckets%
                convert b% to str(bucket_nrs$(b%)), pic(#0)
                bfac$(b%) = hex(80)
                readkey$ = "STC.BD." & str(userid$) & "." &              ~
                                                     str(bucket_nrs$(b%))
                write #2 using L30320, readkey$, bucket_ids$(b%), " ",    ~
                                             bucket_descrs$(b%), " "
L30320:              FMT CH(20), CH(10), CH(4), CH(250), CH(216)
            next b%
            call "STRING" addr("LJ", str(buckets_msg$,,2), 2%)
            buckets_msg$ = buckets_msg$ & " Buckets Defined"
            purch_dflt%  = min(max(1%, purch_dflt%), buckets%)
            purch_id$    = bucket_ids$   (purch_dflt%)
            purch_descr$ = bucket_descrs$(purch_dflt%)
            labor_dflt%  = min(max(1%, labor_dflt%), buckets%)
            labor_id$    = bucket_ids$   (labor_dflt%)
            labor_descr$ = bucket_descrs$(labor_dflt%)
            ovrhd_dflt%  = min(max(1%, ovrhd_dflt%), buckets%)
            ovrhd_id$    = bucket_ids$   (ovrhd_dflt%)
            ovrhd_descr$ = bucket_descrs$(ovrhd_dflt%)
            call "DATEFMT" (frozen$)
            return

        REM *************************************************************~
            *           C R E A T E   N E W   C O S T   S E T           *~
            *-----------------------------------------------------------*~
            * Create new Cost Set per parameters supplied.              *~
            *************************************************************
        create_new_set:
*       * Befoe We Start Let's Check for  OUTVOL
            if copy_from$() = " " then L31069
                gosub check_outvol
                if ok% = 0% then exit_program  /* OUTVOL Not Set */
L31069:     print page
*       * First, get the next internal Set ID
            call "SHOSTAT" ("Generating Internal Set ID")
L31090:     readkey$ = "STC.CONTROL"
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then L31150
                put #2 using L31130, readkey$, "0001", " ", " "
L31130:              FMT CH(20), CH(4), CH(255), CH(221)
                write #2, eod goto L31090 : goto L31090
L31150:     get #2 using L31160, setid$
L31160:         FMT POS(21), CH(4)
            convert setid$ to nextid%
            nextid% = nextid% + 1%
            put #2 using L31200, nextid%
L31200:         FMT POS(21), PIC(0000)
            rewrite #2

*       * Now Create Cost Set Files
            call "SHOSTAT" ("Creating Cost Set Data Base")
            f% = 10% : in_file$  = "STC" & copy_id$(1) & "H" /* HNY    */
                       out_file$ = "STC" & setid$      & "H"
                       if copy_from$(1) = " " then gosub file_create     ~
                                              else gosub file_copy
            f% = 11% : in_file$  = "STC" & copy_id$(1) & "D" /* DETAL  */
                       out_file$ = "STC" & setid$      & "D"
                       if copy_from$(1) = " " then gosub file_create     ~
                                              else gosub file_copy
            f% = 12% : in_file$  = "STC" & copy_id$(2) & "W" /* WCACT  */
                       out_file$ = "STC" & setid$      & "W"
                       if copy_from$(2) = " " then gosub file_create     ~
                                              else gosub file_copy
            f% = 13% : in_file$  = "STC" & copy_id$(3) & "L" /* LABOR  */
                       out_file$ = "STC" & setid$      & "L"
                       if copy_from$(3) = " " then gosub file_create     ~
                                              else gosub file_copy
            f% = 14% : in_file$  = "STC" & copy_id$(1) & "M" /* MAPNG  */
                       out_file$ = "STC" & setid$      & "M"
                       if copy_from$(1) = " " then gosub file_create     ~
                                              else gosub file_copy
            f% = 15% : out_file$ = "STC" & setid$      & "C" /* CHNGS  */
                                                   gosub file_create

*       * Write out the Cost Set Header Record and open the new files.
            readkey$ = "STC.HDR." & set$
            write #2 using L31550, readkey$, set_descr$, setid$,          ~
                                  hex(ffffffff), buckets%, bucket_ids$(),~
                                  bucket_descrs$(), "Y", " ",            ~
                                  purch_dflt%, labor_dflt%, ovrhd_dflt%, ~
                                  " "
L31550:         FMT CH(20), CH(30), 2*CH(4), BI(1), 12*CH(10), 12*CH(20),~
                    CH( 1), CH(21), 3*BI(1), CH(56)

            call "STCFOPEN" (set$, "UUUUUU", #2, errormsg$,              ~
                                           #10, #11, #12, #13, #14, #15)

*       * A little miscellany setup
            mat copy_map% = zer
            for l% = 1% to 3% : for b% = 1% to 12%
                if copy_map$(l%,b%) <> " " then                          ~
                             convert copy_map$(l%,b%) to copy_map%(l%,b%)
            next b% : next l%


*       * Finish Off Part Related Files
            call "SHOSTAT" ("Aligning Part's Data")
*         First pass STCHNY.  Remap buckets, clear text, write X-Ref.
L31720:     call "READNXT1" (#10, f1%(10))
            if f1%(10) = 0% then L31920
                get #10 using L31750, part$, bom$, rte$, old_costs(), dp$
L31750:              FMT CH(25), POS(38), 2*CH(3), POS(252), 12*PD(14,4),~
                         POS(362), CH(2)
                dp% = 0%
                convert dp$ to dp%, data goto L31760
L31760:         mat costs = zer : tot_cost = 0
                for b% = 1% to from_buckets%(1%)
                     if b% <> dp% then L31780
                        convert copy_map%(1%,b%) to dp$, pic(##)
L31780:              costs(copy_map%(1%,b%)) =                           ~
                                  costs(copy_map%(1%,b%)) + old_costs(b%)
                     tot_cost = tot_cost + old_costs(b%)
                next b%
                call "PACKZERO" (costs(), costs$)
                put #10 using L31850, hex(ffffffff), tot_cost, zeros$,    ~
                                     zeros$, costs$, " ", " ", dp$
L31850:              FMT POS(34), CH(4), POS(52), PD(14,4), 2*CH(96),    ~
                         POS(252), CH(96), 2*CH(3), POS(362), CH(2)
                rewrite #10
                if bom$ = " " and rte$ = " " then L31720
                write #8 using L31890,part$, bom$, set$, part$, rte$, set$
L31890:              FMT CH(25), CH(3), CH(8), CH(25), CH(3), CH(8)
                goto L31720

L31920
*         Next, take care of bucket references in the Details File
L31930:     call "READNXT1" (#11, f1%(11))
            if f1%(11) = 0% then L32010
                get #11 using L31960, b%
L31960:              FMT POS(85), BI(1)
                put #11 using L31960, copy_map%(1%,b%)
                rewrite #11
                goto L31930

L32010
*         Lastly (for the Parts), remap the maps.
L32020:     call "READNXT1" (#14, f1%(14))
            if f1%(14) = 0% then L32170
                get #14 using L32050, mapping%()
L32050:              FMT POS(39), 12*BI(1)
                for b% = 1% to buckets%
                     mapping%(b%) = copy_map%(1%,mapping%(b%))
                next b%
                if buckets% = 12% then L32130
                     for b% = buckets% + 1%  to 12%
                          mapping%(b%) = 0%
                     next b%
L32130:         put #14 using L32050, mapping%()
                rewrite #14
                goto L32020

L32170
*       * Re-align Work Centers/Activities File's Buckets
            call "SHOSTAT" ("Aligning WC-Activity Data")
L32190:     call "READNXT1" (#12, f1%(12))
            if f1%(12) = 0% then L32310
                get #12 using L32220, idx6%()
L32220:              FMT POS(57), 6*BI(1)
                for b% = 1% to 6%
                     if idx6%(b%) <> 0% then                             ~
                                       idx6%(b%) = copy_map%(2,idx6%(b%))
                next b%
                put #12 using L32220, idx6%()
                rewrite #12
                goto L32190

L32310
*       * And now take care of Bucket references in the Labor file
            call "SHOSTAT" ("Aligning Labor Class Data")
L32330:     call "READNXT1" (#13, f1%(13))
            if f1%(13) = 0% then L32490
                get #13 using L32360, idx6%(), idx2%()
L32360:              FMT POS(53), 6*BI(1), POS(67), 2*BI(1)
                for b% = 1% to 6%
                     if idx6%(b%) <> 0% then                             ~
                                       idx6%(b%) = copy_map%(3,idx6%(b%))
                next b%
                for b% = 1% to 2%
                     if idx2%(b%) <> 0% then                             ~
                                       idx2%(b%) = copy_map%(3,idx2%(b%))
                next b%
                put #13 using L32360, idx6%(), idx2%()
                rewrite #13
                goto L32330

L32490
*       ** DONE with Creation.  Close files and continue with edit.
            call "STCFOPEN" (set$, "C", #2, errormsg$,                   ~
                                           #10, #11, #12, #13, #14, #15)
            readkey$ = "STC.HDR." & set$
            call "READ100" (#2, readkey$, f1%(2))
            gosub load_data
            gosub clear_all
        clear_all: return clear all
            goto  editmode


        file_copy  /* Copy IN_FILE$ to create OUT_FILE$                */
            call "PUTPRNAM" addr(#f%, in_file$)
            f2%(f%) = 1%
            call "OPENFILE" (#f%, "VALID", f2%(f%), rslt$(f%), " ")
            if f2%(f%) <> 0% then file_create
            if str(rslt$(f%),17,4) = hex(00000000) then file_create
                call "PUTPARM" addr("E", "INPUT   ", 4%,                 ~
                                    "FILE    ", in_file$, 8%,            ~
                                    "LIBRARY ", str(rslt$(f%),1,8), 8%,  ~
                                    "VOLUME  ", str(rslt$(f%),9,6), 6%,  ~
                                    "MODE    ", "SHARED", 6%, u3%)
                call "PUTPARM" addr("E", "LOCK    ", 1%,                 ~
                                    "LOCK    ", "NO ", 3%, u3%)
                call "PUTPARM" addr("E", "OPTIONS ", 0%, u3%)
                call "PUTPARM" addr("E", "OUTPUT  ", 3%,                 ~
                                    "FILE    ", out_file$, 8%,           ~
                                    "LIBRARY ", str(rslt$(f%),1,8), 8%,  ~
                                    "VOLUME  ", out_vol$, 6%, u3%)
                call "PUTPARM" addr("E", "EOJ     ", 0%, u3%)
                call "LINK" addr("COPY    ", "S", "        ", "      ",  ~
                                  u3%, fs%)

        file_create
            call "PUTPRNAM" addr(#f%, out_file$)
            call "OPENCHCK" (#f%, fs%, f2%(f%), 100%, " ")
            close #f%
            return

        check_outvol
            ok% = 1%
            call "EXTRACT" addr("OV", zzov$)
            if zzov$ <> " " then return       /* OUTVOL IS Set & OK */
            ask% = 1%
            call "ASKUSER" (ask%, "OUTVOL IS BLANK",                     ~
                            "Your OUTVOL Must Be Set to Copy a Cost Set",~
                            "This Program Can Not Continue ",            ~
                            "Press Any Key to Acknowledge & Exit"    )
            ok% = 0%
            return

        print_zero_costs
            call "SHOSTAT"("Printing Zero Cost Items")
            rptid$ = "STC011"
            gosub setup_for_printing
            prnt_rmv$ = " "   :  prnt_tot$ = "ARE PRESENT"
            readkey$ = hex(00)
            call "READ104"(#10, readkey$, f1%(10%))
            goto L33060
          print_loop
            call "READNEXT" (#10, f1%(10%))
L33060:     if f1%(10%) = 0% then goto print_totals
            totallines% = totallines% + 1%
            get #10 using L33090, part$, tot_cost
L33090:    FMT CH(25), POS(52), PD(14,4)
            if tot_cost <> 0 then print_loop
                call "DESCRIBE"(#4, part$, partdescr$, 0%, f1%(4%))
                totalprints% = totalprints% + 1%
                lcntr% = lcntr% + 1%
                if lcntr% > 57% then gosub print_zero_costs_header

            print using L33170, part$, partdescr$
L33170: %#########################   ################################

            goto print_loop

        remove_zero_costs
            call "SHOSTAT"("Removing Zero Cost Items")
            rptid$ = "STC012"
            gosub setup_for_printing
            prnt_rmv$ = "REMOVE"   :  prnt_tot$ = "WERE REMOVED"
            readkey$ = hex(00)
            call "READ105"(#10, readkey$, f1%(10%))
            goto L33270
          print_remove_loop
            call "READNXT1" (#10, f1%(10%))
L33270:     if f1%(10%) = 0% then goto print_totals
            totallines% = totallines% + 1%
            get #10 using L33090, part$, tot_cost
            if tot_cost <> 0 then print_remove_loop
                call "DESCRIBE"(#4, part$, partdescr$, 0%, f1%(4%))
                totalprints% = totalprints% + 1%
                lcntr% = lcntr% + 1%
                if lcntr% > 57% then gosub print_zero_costs_header

            print using L33170, part$, partdescr$
            delete #10

            goto print_remove_loop

        print_zero_costs_header
            pcntr% = pcntr% + 1%
            print page
            print using  L33540 , date$, time$, company$, "STCINPUT",rptid$
            print using  L33570 , prnt_rmv$, set$, pcntr%

            print
            print "PART                        DESCRIPTION"
            print "------------------------    --------------------------~
        ~--------"
            lcntr% = 5%
            return

L33540: %RUN  ######## @ ########                  ######################~
        ~####################################              ########:######

L33570: %                                        ####### PARTS WITH ZERO ~
        ~COST FOR COST SET: ########                           PAGE: ####

        print_totals

            if lcntr% > 57% then gosub print_zero_costs_header

            print
            print
            print using L33650,  totalprints%, totallines%, prnt_tot$

L33650: %             A TOTAL OF ##########  ZERO COST PARTS OUT OF #####~
        ~##### ############

            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L33705, time$        /* End of report line */

            close printer
            call "SETPRNT" ( " ", " ", 0%, 1%)

L33705:         %                          * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

            goto editmode

        setup_for_printing
            select printer(134)
            time$ = " " :  call "TIME" (time$)
            pcntr%, totalprints%, totallines% = 0%  :   lcntr% = 99%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,,61) = "Current Cost Set = " & currset$
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40150,         /* Cost Set ID       */   ~
                                L40140          /* Cost Set Descr    */
              goto L40180

L40140:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40180:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Standard Cost Sets",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Cost Set ID",                                ~
               at (06,24), fac(lfac$( 1)), set$                 , ch(08),~
               at (06,59), fac(hex(8c))  , setid_msg$           , ch(20),~
                                                                         ~
               at (07,02), "Cost Set Description",                       ~
               at (07,24), fac(lfac$( 2)), set_descr$           , ch(30),~
               at (07,59), fac(hex(8c))  , buckets_msg$         , ch(20),~
                                                                         ~
               at (09,02), fac(hex(8c)),   opts_prompt$,                 ~
               at (09,24), fac(hex(8c)),   opt$( 2),                     ~
               at (10,24), fac(hex(8c)),   opt$( 3),                     ~
               at (11,24), fac(hex(8c)),   opt$( 4),                     ~
               at (12,24), fac(hex(8c)),   opt$( 5),                     ~
               at (13,24), fac(hex(8c)),   opt$( 6),                     ~
               at (15,24), fac(hex(8c)),   opt$( 7),                     ~
               at (16,24), fac(hex(8c)),   opt$( 8),                     ~
               at (17,24), fac(hex(8c)),   opt$( 9),                     ~
               at (18,24), fac(hex(8c)),   opt$(10),                     ~
               at (19,24), fac(hex(8c)),   opt$(11),                     ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(keys$), key(keyhit%)

            if keyhit% <> 13 then L40550
                call "MANUAL" ("STCINPUT")
                goto L40180

L40550:     if keyhit% <> 15 then L40590
                call "PRNTSCRN"
                goto L40180

L40590:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Get Cost Bucket Definitions.                              *~
            *************************************************************

        deffn'102(fieldnr%)
            str(line2$,,61) = "Standard Cost Set: " & set$ & "  (" &     ~
                              set_descr$ & ")"
            if errormsg$ <> " " then L41120

            init(hex(86)) lfac$(), bfac$() : init(hex(84)) str(bfac$(),2)
                                             init(hex(84)) dfac$()
            on fieldnr% gosub L41080,              /* Number of Buckets */~
                              L41090               /* Bucket IDs/Descrs */
            goto L41120
L41080:         lfac$(1) = hex(82) : init(hex(8c))bfac$()
                                     init(hex(8c))dfac$()  :  return
L41090:         lfac$(1) = hex(8c) : init(hex(81))bfac$()
                lfac$(2) = hex(82) : init(hex(8c))str(bfac$(),buckets%+1%)
                lfac$(3) = hex(82) : init(hex(80))dfac$()
                lfac$(4) = hex(82) : init(hex(8c))str(dfac$(),buckets%+1%)
                                     return

L41120:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Standard Cost Sets: Define Cost Buckets",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Number of Buckets",                          ~
               at (04,20), fac(lfac$( 1)), buckets$             , ch(02),~
                                                                         ~
               at (05,20), fac(hex(ac)), hdr2$(1)               , ch(02),~
               at (05,24), fac(hex(ac)), hdr2$(2)               , ch(10),~
               at (05,36), fac(hex(ac)), hdr2$(3)               , ch(20),~
                                                                         ~
               at (06,02), "Bucket Definition",                          ~
               at (06,20), fac(hex(8c)), bucket_nrs$( 1)        , ch(02),~
               at (07,20), fac(hex(8c)), bucket_nrs$( 2)        , ch(02),~
               at (08,20), fac(hex(8c)), bucket_nrs$( 3)        , ch(02),~
               at (09,20), fac(hex(8c)), bucket_nrs$( 4)        , ch(02),~
               at (10,20), fac(hex(8c)), bucket_nrs$( 5)        , ch(02),~
               at (11,20), fac(hex(8c)), bucket_nrs$( 6)        , ch(02),~
               at (12,20), fac(hex(8c)), bucket_nrs$( 7)        , ch(02),~
               at (13,20), fac(hex(8c)), bucket_nrs$( 8)        , ch(02),~
               at (14,20), fac(hex(8c)), bucket_nrs$( 9)        , ch(02),~
               at (15,20), fac(hex(8c)), bucket_nrs$(10)        , ch(02),~
               at (16,20), fac(hex(8c)), bucket_nrs$(11)        , ch(02),~
               at (17,20), fac(hex(8c)), bucket_nrs$(12)        , ch(02),~
                                                                         ~
               at (06,24), fac(bfac$( 1)), bucket_ids$( 1)      , ch(10),~
               at (07,24), fac(bfac$( 2)), bucket_ids$( 2)      , ch(10),~
               at (08,24), fac(bfac$( 3)), bucket_ids$( 3)      , ch(10),~
               at (09,24), fac(bfac$( 4)), bucket_ids$( 4)      , ch(10),~
               at (10,24), fac(bfac$( 5)), bucket_ids$( 5)      , ch(10),~
               at (11,24), fac(bfac$( 6)), bucket_ids$( 6)      , ch(10),~
               at (12,24), fac(bfac$( 7)), bucket_ids$( 7)      , ch(10),~
               at (13,24), fac(bfac$( 8)), bucket_ids$( 8)      , ch(10),~
               at (14,24), fac(bfac$( 9)), bucket_ids$( 9)      , ch(10),~
               at (15,24), fac(bfac$(10)), bucket_ids$(10)      , ch(10),~
               at (16,24), fac(bfac$(11)), bucket_ids$(11)      , ch(10),~
               at (17,24), fac(bfac$(12)), bucket_ids$(12)      , ch(10),~
                                                                         ~
               at (06,36), fac(dfac$( 1)), bucket_descrs$( 1)   , ch(20),~
               at (07,36), fac(dfac$( 2)), bucket_descrs$( 2)   , ch(20),~
               at (08,36), fac(dfac$( 3)), bucket_descrs$( 3)   , ch(20),~
               at (09,36), fac(dfac$( 4)), bucket_descrs$( 4)   , ch(20),~
               at (10,36), fac(dfac$( 5)), bucket_descrs$( 5)   , ch(20),~
               at (11,36), fac(dfac$( 6)), bucket_descrs$( 6)   , ch(20),~
               at (12,36), fac(dfac$( 7)), bucket_descrs$( 7)   , ch(20),~
               at (13,36), fac(dfac$( 8)), bucket_descrs$( 8)   , ch(20),~
               at (14,36), fac(dfac$( 9)), bucket_descrs$( 9)   , ch(20),~
               at (15,36), fac(dfac$(10)), bucket_descrs$(10)   , ch(20),~
               at (16,36), fac(dfac$(11)), bucket_descrs$(11)   , ch(20),~
               at (17,36), fac(dfac$(12)), bucket_descrs$(12)   , ch(20),~
                                                                         ~
               at (18,02), "Purchases Default",                          ~
               at (18,20), fac(lfac$( 2)), purch_dflt%          ,pic(#0),~
               at (18,24), fac(hex(8c))  , purch_id$            , ch(10),~
               at (18,36), fac(hex(8c))  , purch_descr$         , ch(20),~
                                                                         ~
               at (19,02), "Direct Labor Dflt",                          ~
               at (19,20), fac(lfac$( 3)), labor_dflt%          ,pic(#0),~
               at (19,24), fac(hex(8c))  , labor_id$            , ch(10),~
               at (19,36), fac(hex(8c))  , labor_descr$         , ch(20),~
                                                                         ~
               at (20,02), "Labor Overhd Dflt",                          ~
               at (20,20), fac(lfac$( 4)), ovrhd_dflt%          ,pic(#0),~
               at (20,24), fac(hex(8c))  , ovrhd_id$            , ch(10),~
               at (20,36), fac(hex(8c))  , ovrhd_descr$         , ch(20),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(keys$), key(keyhit%)

               if keyhit% <> 13 then L41465
                  call "MANUAL" ("STCINPUT")
                  goto L41120

L41465:        if keyhit% <> 15 then L41485
                  call "PRNTSCRN"
                  goto L41120

L41485:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%)
            str(line2$,,61) = "Standard Cost Set: " & set$ & "  (" &     ~
                              set_descr$ & ")"
            if errormsg$ <> " " or fieldnr% = 2% then L42220
            if fieldnr% <> 0% then L42140
                init(hex(86)) lfac$() : init(hex(84)) mfac$()
                goto L42220

L42140:     init(hex(8c)) lfac$(), mfac$()
            on fieldnr% goto    L42190,         /* From Set             */~
                                L42220          /* Copy Mapping         */
            goto L42220
L42190:           lfac$(l%) = hex(81)

L42220:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Standard Cost Sets: Define Data Sources",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,40), "COPY MAPPINGS: Enter New Set Buckets",       ~
               at (07,04), fac(hex(ac)), hdr3$(1)               , ch(22),~
               at (07,29), fac(hex(ac)), hdr3$(2)               , ch(08),~
               at (07,40), fac(hex(ac)), hdr3$(3)               , ch(36),~
                                                                         ~
               at (08,04), "Parts",                                      ~
               at (08,29), fac(lfac$(1))   , copy_from$(1)      , ch(08),~
               at (08,41), fac(mfac$(1, 1)), copy_map$ (1, 1)   , ch(02),~
               at (08,44), fac(mfac$(1, 2)), copy_map$ (1, 2)   , ch(02),~
               at (08,47), fac(mfac$(1, 3)), copy_map$ (1, 3)   , ch(02),~
               at (08,50), fac(mfac$(1, 4)), copy_map$ (1, 4)   , ch(02),~
               at (08,53), fac(mfac$(1, 5)), copy_map$ (1, 5)   , ch(02),~
               at (08,56), fac(mfac$(1, 6)), copy_map$ (1, 6)   , ch(02),~
               at (08,59), fac(mfac$(1, 7)), copy_map$ (1, 7)   , ch(02),~
               at (08,62), fac(mfac$(1, 8)), copy_map$ (1, 8)   , ch(02),~
               at (08,65), fac(mfac$(1, 9)), copy_map$ (1, 9)   , ch(02),~
               at (08,68), fac(mfac$(1,10)), copy_map$ (1,10)   , ch(02),~
               at (08,71), fac(mfac$(1,11)), copy_map$ (1,11)   , ch(02),~
               at (08,74), fac(mfac$(1,12)), copy_map$ (1,12)   , ch(02),~
                                                                         ~
               at (09,04), "Work Center-Activities",                     ~
               at (09,29), fac(lfac$(2))   , copy_from$(2)      , ch(08),~
               at (09,41), fac(mfac$(2, 1)), copy_map$ (2, 1)   , ch(02),~
               at (09,44), fac(mfac$(2, 2)), copy_map$ (2, 2)   , ch(02),~
               at (09,47), fac(mfac$(2, 3)), copy_map$ (2, 3)   , ch(02),~
               at (09,50), fac(mfac$(2, 4)), copy_map$ (2, 4)   , ch(02),~
               at (09,53), fac(mfac$(2, 5)), copy_map$ (2, 5)   , ch(02),~
               at (09,56), fac(mfac$(2, 6)), copy_map$ (2, 6)   , ch(02),~
               at (09,59), fac(mfac$(2, 7)), copy_map$ (2, 7)   , ch(02),~
               at (09,62), fac(mfac$(2, 8)), copy_map$ (2, 8)   , ch(02),~
               at (09,65), fac(mfac$(2, 9)), copy_map$ (2, 9)   , ch(02),~
               at (09,68), fac(mfac$(2,10)), copy_map$ (2,10)   , ch(02),~
               at (09,71), fac(mfac$(2,11)), copy_map$ (2,11)   , ch(02),~
               at (09,74), fac(mfac$(2,12)), copy_map$ (2,12)   , ch(02),~
                                                                         ~
               at (10,04), "Labor Class Rates",                          ~
               at (10,29), fac(lfac$(3))   , copy_from$(3)      , ch(08),~
               at (10,41), fac(mfac$(3, 1)), copy_map$ (3, 1)   , ch(02),~
               at (10,44), fac(mfac$(3, 2)), copy_map$ (3, 2)   , ch(02),~
               at (10,47), fac(mfac$(3, 3)), copy_map$ (3, 3)   , ch(02),~
               at (10,50), fac(mfac$(3, 4)), copy_map$ (3, 4)   , ch(02),~
               at (10,53), fac(mfac$(3, 5)), copy_map$ (3, 5)   , ch(02),~
               at (10,56), fac(mfac$(3, 6)), copy_map$ (3, 6)   , ch(02),~
               at (10,59), fac(mfac$(3, 7)), copy_map$ (3, 7)   , ch(02),~
               at (10,62), fac(mfac$(3, 8)), copy_map$ (3, 8)   , ch(02),~
               at (10,65), fac(mfac$(3, 9)), copy_map$ (3, 9)   , ch(02),~
               at (10,68), fac(mfac$(3,10)), copy_map$ (3,10)   , ch(02),~
               at (10,71), fac(mfac$(3,11)), copy_map$ (3,11)   , ch(02),~
               at (10,74), fac(mfac$(3,12)), copy_map$ (3,12)   , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                    keys(keys$), key(keyhit%)

               if keyhit% <> 13 then L42930
                  call "MANUAL" ("STCINPUT")
                  goto L42220

L42930:        if keyhit% <> 15 then L42970
                  call "PRNTSCRN"
                  goto L42220

L42970:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%,copying%)
            errormsg$ = " "
            on fieldnr% gosub L50120,         /* Cost Set ID       */     ~
                              L50490          /* Cost Set Descr    */
            return

L50120
*        Test for COST SET ID                  SET$
            plowkey$   = "STC.HDR." & set$
            set_descr$ = hex(06) & "Select Cost Set"
            if set$ = " " then call "PLOWCODE" (#2, plowkey$, set_descr$,~
                                                8%, 0.30, onfile%)       ~
                          else call "READ100"  (#2, plowkey$, onfile%)
            if onfile% = 1% then L50220
                if set$ = " " then errormsg$ = hex(00)
                set_descr$ = " "
                return
L50220:     if set$ = " " then set$ = str(plowkey$,9)
            get #2 using L50240, in_file$, textid$
L50240:         FMT POS(51), 2*CH(4)
            in_file$ = "STC" & in_file$ & "H"
            call "PUTPRNAM" addr(#10%, in_file$)
            f2%(10) = 1%
            call "OPENFILE" (#10%, "VALID", f2%(10), rslt$(10), " ")
            if f2%(10%) = 0% then L50440
                u3% = 2%
                call "ASKUSER" (u3%, "COST SET ARCHIVED",                ~
                       "This Cost Set is not on disk and is assumed to", ~
                       "have been archived.  Press RETURN to select",    ~
                       "another Set -or- PF-27 to remove set references.")
                if u3% = 27% then L50380
                     errormsg$ = "Cost Set Archived or Unavailable"
                     return
L50380:         plowkey$ = set$
                call "DELETE" (#8, plowkey$, 8%)
                gosub hold_header  :  delete #2
                call "TXTFUTIL" (#9, f2%(9), "DELE", textid$)
                return clear all
                goto inputmode
L50440:     gosub load_data
            if errormsg$ <> " " or copying% = 1% then return
                return clear all
                goto editmode

L50490
*        Test for COST SET DESCRIPTION         SET_DESCR$
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%, edit%)
            errormsg$ = " "
            on fieldnr% gosub L51120,         /* Number of Buckets      */~
                              L51280          /* Bucket IDs             */
            return

L51120
*        Test for NUMBER OF COST BUCKETS       BUCKETS$
            convert buckets$ to buckets%, data goto L51150
            if buckets% >= 1% and buckets% <=12% then L51160
L51150:         errormsg$ = "Number of Buckets must be 1 - 12"  :  return
L51160:     convert buckets% to buckets$, pic(#0)
            for b% = 1% to 12%
                if b% > buckets% then L51240
                     convert b% to str(bucket_nrs$(b%)), pic(#0)
                     if bucket_ids$(b%) <> " " then L51250
                          bucket_ids$(b%) = "BUCKET ##"
                          convert b% to str(bucket_ids$(b%),8,2), pic(#0)
                          goto L51250
L51240:         bucket_ids$(b%), bucket_descrs$(b%), bucket_nrs$(b%)= " "
L51250:     next b%
            if edit% = 1% then return

L51280
*        Test for BUCKETS IDS                  BUCKET_IDS$()
            init(hex(8c)) lfac$(1)
            init(hex(89)) bfac$() : init(hex(88)) dfac$()
            init(hex(8c)) str(bfac$(),buckets%+1%)
            init(hex(8c)) str(dfac$(),buckets%+1%)
            for b% = 1% to buckets%
                if bucket_ids$(b%) <> " " then L51380
                     errormsg$ = "Bucket ID can not be blank"
                     bfac$(b%) = hex(81)
                     return
L51380:         if b% = 1% then L51450
                     search str(bucket_ids$(),,10%*(b%-1%)) =            ~
                            str(bucket_ids$(b%)) to cursor%() step 10
                     if cursor%(1) = 0% then L51450
                          errormsg$ = "Each ID must be unique."
                          bfac$(b%) = hex(81)
                          return
L51450:     next b%

            if purch_dflt% >= 1% and purch_dflt% <= buckets% then L51520
                errormsg$ = "Purchases Default must reference a valid" & ~
                            " bucket number."
                lfac$(2)  = hex(82)
                return
L51520:     purch_id$    = bucket_ids$   (purch_dflt%)
            purch_descr$ = bucket_descrs$(purch_dflt%)

            if labor_dflt% >= 1% and labor_dflt% <= buckets% then L51600
                errormsg$ = "Labor Default must reference a valid"  &    ~
                            " bucket number."
                lfac$(3)  = hex(82)
                return
L51600:     labor_id$    = bucket_ids$   (labor_dflt%)
            labor_descr$ = bucket_descrs$(labor_dflt%)

            if ovrhd_dflt% >= 1% and ovrhd_dflt% <= buckets% then L51680
                errormsg$ = "Overhead Default must reference a valid"  & ~
                            " bucket number."
                lfac$(4)  = hex(82)
                return
L51680:     ovrhd_id$    = bucket_ids$   (ovrhd_dflt%)
            ovrhd_descr$ = bucket_descrs$(ovrhd_dflt%)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52120,         /* From Set               */~
                              L52700          /* Copy Mapping           */
            return

L52120
*       ** Test for FROM SET                        COPY_FROM$()
*        First Test Set Entered
            if copy_from$(l%) = " " then L52540
                plowkey$ = "STC.HDR." & copy_from$(l%)
                readkey$ = hex(06) & "Select Source Cost Set"
                call "PLOWCODE" (#2, plowkey$, readkey$, 8%,.30,f1%(2))
                if f1%(2) = 1% then L52210
                     errormsg$ = "From Cost Set not on file."
                     return
L52210:         get #2 using L52220, in_file$
L52220:              FMT POS(51), CH(4)
                in_file$ = "STC" & in_file$ & "H"
                call "PUTPRNAM" addr(#10%, in_file$)
                f2%(10) = 1%
                call "OPENFILE" (#10%, "VALID", f2%(10), rslt$(10), " ")
                if f2%(10%) = 0% then L52300
                     errormsg$ = "From Cost Set has been archived"
                     return
L52300:         copy_from$(l%) = str(plowkey$,9)
                get #2 using L52330, copy_id$(l%),  from_buckets%(l%),    ~
                                    from_global$
L52330:              FMT POS(51), CH(4), POS(59), BI(1), POS(420), CH(1)
                call "STCFOPEN" (copy_from$(l%), "SSSSSS", #2, errormsg$,~
                                 #10, #11, #12, #13, #14, #15)
                if errormsg$    <> " " then return
                if from_global$ <> " " then L52400
                     plowkey$ = hex(00)
                     call "PLOWNEXT" (#15, plowkey$, 0%, f1%(15))
L52400:         for f% = 10% to 15%
                     call "GETUFBS1" addr(#f%, f1%)
                     if f1% = 1% then close #f%
                next f%
                if f1%(15) = 0% and from_global$ = " " then L52540
                     if display% <> 0% then L52540
L52450:              u3% = 2%
                     call "ASKUSER" (u3%, "CHANGES PENDING",             ~
                               "There are changes pending for this set", ~
                               "Press RETURN to continue with this set", ~
                               "-OR- PF-1 to enter another set to use.")
                     if u3% = 0% then L52540
                          if u3% <> 1% then L52450
                          errormsg$ = hex(00)
                          return
L52540
*        Now set up the bucket mappings and their FACs
            lfac$(l%) = hex(8c)
            if copy_from$(l%) <> " " then L52590
                str(copy_map$(),l%*24%-23%,24) = " "
                return
L52590:     if copy_from$(l%) = old_from$ then L52650
                str(copy_map$(),l%*24%-23%,24) = " "
                if buckets% <> from_buckets%(l%) then L52650
                     for b% = 1% to buckets%
                          convert b% to copy_map$(l%,b%), pic(#0)
                     next b%
L52650:     for b% = 1% to from_buckets%(l%)
                mfac$(l%,b%) = hex(82)
            next b%
            return

L52700
*       ** Test for COPY MAPPING                    COPY_MAP$()
            init(hex(8a)) str(mfac$(),l%*12%-11%,from_buckets%(l%))
            for b% = 1% to from_buckets%(l%)
                convert copy_map$(l%,b%) to nb%, data goto L52780
                if nb% < 1% or nb% > buckets% then L52780
                convert nb% to copy_map$(l%,b%), pic(#0)
            next b%
            return
L52780:         errormsg$ = "Entries must be between 1 and ##"
                convert buckets% to str(errormsg$,31,2), pic(#0)
                mfac$(l%,b%) = hex(82)
                return


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
