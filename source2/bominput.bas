        rem**************************************************************~
            *                                                           *~
            *  bbbb    ooo   m   m  iiiii  n   n  pppp   u   u  ttttt   *~
            *  b   b  o   o  mm mm    i    nn  n  p   p  u   u    t     *~
            *  bbbb   o   o  m m m    i    n n n  pppp   u   u    t     *~
            *  b   b  o   o  m   m    i    n  nn  p      u   u    t     *~
            *  bbbb    ooo   m   m  iiiii  n   n  p       uuu     t     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * bominput - inputs the list of components and how many of  *~
            *            each for a parent part number.                 *~
            *----------------------------------------------------------q*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+----------------what----------------------+-who-*~
            * 03/11/81 ! original                                 ! bcw *~
            * 11/21/85 ! added text processing                    ! mjb *~
            * 07/31/86 ! specify route steps rather the seq nbmrs,!     *~
            *          ! enter options by bom, ergo overhaul.     ! hes *~
            * 12/30/86 ! added uom description to detail screen   ! mjb *~
            * 01/29/87 ! added part descriptions to summary scrn  ! mjb *~
            * 03/23/87 ! fixed part descriptions on summary scrn  ! hes *~
            * 03/30/87 ! stc project- change frozen edit/stddetal ! ern *~
            * 10/15/87 ! now creates engmastr file                ! hes *~
            * 01/19/88 ! added approval tests. (tlj 05/31/88)     ! tlj *~
            * 04/26/88 ! added soft enables                       ! tlj *~
            * 09/23/88 ! better summary rounding/toggle           ! kab *~
            * 09/23/88 ! fixed soft enable on times used          ! kab *~
            * 02/01/89 ! fixed convert statement on yield$        ! mjb *~
            * 02/15/89 ! added plowcode for bom selection         ! mjb *~
            * 03/04/89 ! added "COPIED FROM" & id to description, ! mlj *~
            *          ! creation date now date copied, 'modified'!     *~
            *          ! and 'by' blank (applies to copy only)    !     *~
            * 04/20/89 ! fixed line item alignment (prr 10480)    ! rjm *~
            *          ! bug only happened if on last page,       !     *~
            *          !(if > 15 components & last line displayed)!     *~
            *          ! after delete line only last 13 lines     !     *~
            *          ! displayed.  this caused next insert or   !     *~
            *          ! delete to occur at the wrong seq number. !     *~
            * 03/06/90 ! fixed pf28 - delete all. (prr 11256)     ! sid *~
            *          ! re-initialized reference index counter   !     *~
            *          ! to zero.                                 !     *~
            * 04/09/90 ! added warning re auto replacements on    ! mjb *~
            *          !  file for bom being edited.              !     *~
            * 10/15/90 ! moved large array variables from let     ! mjb *~
            *          !  to init in initialize section for       !     *~
            *          !  compatibility with basic 4.3.1          !     *~
            * 04/05/91 !(prr 10093) added plowcode to test of the ! rjb *~
            *          !     route id                             !     *~
            *          !(prr 10091) added check to obsolete flag  !     *~
            *          !     on hnymastr in describe_part         !     *~
            *          !(prr 11122) added limiting bom markers for!     *~
            *          !     by-products to 'ST', 'RE', or 'AS' in!     *~
            *          !     qty-per and bom marker testing       !     *~
            *          !(prr 11421) added to editpg3 a branching  !     *~
            *          !     to inputlines/inputlocations when the!     *~
            *          !     part (field #1) selected for editing !     *~
            *          !(prr 05720) removed clearing of all screen!     *~
            *          !     variables when using pf4 to return to!     *~
            *          !     field #1 in inputlines               !     *~
            *          !(prr 11422) replaced using cursor%() with !     *~
            *          !     srch%() in all search statements.    !     *~
            *          !(prr 11729, 10592) added a askuser when   !     *~
            *          !     copying a bom to allow option of also!     *~
            *          !     copying the original bom's descr.    !     *~
            *          !(new standards) added call to allfree     !     *~
            * 06/21/91 !qc-fixes put 'ALLFREE' in the right place ! rjb *~
            * 07/09/91 !qc-fixes corrected route-id test section  ! rjb *~
            *          !     to retrieve routing and eliminated a !     *~
            *          !     redundent plowcode screen.           !     *~
            * 07/15/91 !qc-fixes simplfied the route-id test      ! rjb *~
            *          !     section to be a single screen.       !     *~
            * 11/07/91 !cms/dec 'MASK' project                    ! sid *~
            * 03/02/92 ! repositioned bom sequence number when    ! sid *~
            *          !   checking for the header record thru    !     *~
            *          !   bommastr alternate key.                !     *~
            * 01/29/93 ! (prr 10641) now uses a flag in sysfile   ! rjh *~
            *          !   to enable/disable the input location   !     *~
            *          !   screen. sys flg is set via bomflags.   !     *~
            *          ! (prr 12293) a changed bom item retains   !     *~
            *          !  the previous bom item's locations.      !     *~
            *          ! (prr 12632) if copying a bom an askuser  !     *~
            *          !  to copy/not copy the bom text is called.!     *~
            *          !  an existing bom description will not be !     *~
            *          !   over written by 'COPIED FROM- XXX'.    !     *~
            *          ! (prr 12730) location screen accessible   !     *~
            *          !  from the part detail screen via pf(8).  !     *~
            * 07/15/93 ! added 'Purchased Job Item?' to screen 1. ! mlj *~
            * 09/03/93 ! test bom for existence in cad drawing.   ! rjh *~
            * 09/14/93 ! prr 12945  added call to bomefdsp.       ! jdh *~
            *          ! prr 12948  added uom tgl on summary scrn.!     *~
            *          ! prr 13011  added 'NC' marker for component     *~
            *          !  parts that will not have costs rolled up!     *~
            *          !  for std costing but are planned & listed!     *~
            *          !  on pick lists, etc. ('st' with no cost.)!     *~
            *          ! added 'NP' marker for component parts    !     *~
            *          !  that will not have costs rolled up for  !     *~
            *          !  std costing and planning will force an  !     *~
            *          !  iw for (no procurements) and will       !     *~
            *          !  create a pipout for.                    !     *~
            * 09/23/93 ! purchased jobs - part type '000' cannot  ! mlj *~
            *          !  be flagged as purchased job item.       !     *~
            * 10/15/93 ! added 'BP' marker for by-products.  neg  ! jdh *~
            *          !  qty only.  costs will be subtracted     !     *~
            *          !  at std cost roll-up (i hope).           !     *~
            * 10/18/93 ! options can't be by-products.            ! jdh *~
            * ??/??/94 ! will hanson did something here???        ! wph *~
            *          !  (but he didn't have the guts to tell us !     *~
            *          !   what it was!)                          !     *~
            * 03/30/94 ! added pfkey prompt and call to ecrinqsb. ! ldj *~
            * 04/12/94 ! on delete of bom also delete from bomcad ! rjh *~
            *          !  cross-reference file.                   !     *~
            * 06/15/94 ! added pfkey prompt and call to bomfcalc. ! ldj *~
            * 11/27/95 ! prrs 13512,13519. added selection screen ! jdh *~
            *          !  for component bom id.                   !     *~
            *          ! prr 13120.  added effective bom message. !     *~
            * 09/06/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            apprby$15,                   /* Approved By                */~
            appron$8,                    /* Approved On                */~
            autoflag$1,                  /* Auto Replace Flag          */~
            assypart$25,                 /* PART TO DO BOM FOR         */~
            assypartdescr$34,            /* ASSEMBLY PART NUMBER       */~
            assytype$3,                  /* INV ITEM TYPE              */~
            batch$10,                    /* BATCH QUANTITY             */~
            bom$(12)3,                   /* ALT BOMS AVALABLE          */~
            bom_factors(1),              /* Arg for BOMFCALC           */~
            bomdescr$30, bomdescr_org$30,/* DESCRIPTION OF THIS BOM    */~
            bomdescr$(12)30,             /* DESCRIPTION OF BOMS        */~
            bomid$3,                     /* ALT BOM THIS BOM           */~
            bommkr$(300)2,               /* BOM MARKER                 */~
            bompaging$,                  /* SWITCHABLE PAGING INTRUCTIO*/~
            box$21,                      /* FOR DETAIL INPUT SCREEN    */~
            cbom$(300)3,                 /* Component's Specific BOMID */~
            cbomdescr$32,                /* Component's Specific BOMID */~
            cadmsg$79,                   /* SWITCHABLE DISPLAY MESSAGE */~
            cad_exdate$8,                /* Date BOM Export toward DWG */~
            cad_indate$8,                /* Date BOM Imported into DWG */~
            cad_rsltflg$1,               /* Status flag for BOM to CAD */~
            cursor%(2),                  /* CURSOR LOCATIONS FOR EDIT  */~
            date$8,                      /* TODAY'S CLOCK DATE         */~
            date$(2)8,                   /* CREATE & LAST MOD DATES    */~
            desc$(300)32,                /* DESCRIPTION OF COMPONENTS  */~
            descr$34,                    /* DESCRIPTION OF COMPONENTS  */~
            dfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            dsize$1,                     /* DRAWING SIZE               */~
            ecrpfk$14,                   /* ECR Inquiry PFKey Prompt   */~
            eff_bom$3,                   /* Current Effective BOM      */~
            effective$(490)3,            /* BOM EFFECTIVE DATES        */~
            enteron$8,                   /* System date of approval    */~
            enterby$3,                   /* User that modified approval*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            ext$30,                      /* Extended Multiplier        */~
            fc$(16)1,                    /* FIELD ATTRIBUTE CHARACTERS */~
            fcc$(36)1,                   /* More FACs                  */~
            fixed$(300)10,               /* Fixed Quantity Per Run     */~
            form_calc_flag$1,            /* Formula Calculation Enabled*/~
            hdr$(3)79,                   /* PLOWCODE Argument          */~
            header$79,                   /* Screen Title               */~
            header1$79,                  /* Screen Subtitle            */~
            header2$79,                  /* Screen Subtitle - 2        */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            incl(1),                     /* PLOWCODE Argument          */~
            incl$(1)3,                   /* PLOWCODE Argument          */~
            lastpart$25,                 /* LAST PART NUMBER INPUT     */~
            lastbom$3,                   /* LAST BOM FOR BOM SEARCH    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            loc$(6001)6,                 /* REFERENCE ON PARENT        */~
            locqty$(6001)8,              /* # AT LOCATION              */~
            locind%(6000),               /* LOCATION INDEX             */~
            locedt%(999),                /* LOCATION INDEX FOR EDIT    */~
            lockey$100,                  /* LOCATION FILE KEY          */~
            locscreen_flag$1,            /* To Do Location Screen Flag */~
            luser$3,                     /* LAST MODIFY BY USER        */~
            message$79,                  /* INPUT MESSAGE TEXT         */~
            mfac$1,                      /* FIELD ATTRIBUTE CHARACTER  */~
            mkrdescr$30,                 /* BOM MARKER DESCRIPTION     */~
            mkrdescr$(14)25,             /* BOM MARKER DESCRIPTIONS    */~
            msg$79,                      /* SWITCHABLE DISPLAY MESSAGE */~
            netqty$(14,2)10,             /* Net Mutliplier For Display */~
            odescr$(14)32,               /* Work Variable              */~
            oldpart$25,                  /* For Line Item Edit         */~
            oldseq$(300)3,               /* For Plow on HNYOPTN2       */~
            op$(300)1,                   /* BOM OPTION MARKER          */~
            over$(300)10,                /* Added Overage Quantity     */~
            paflag$1,                    /* Phantom Assembly Flag      */~
            part$(300)28,                /* COMPONENT PART NUMBER LIST */~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            pjflag$1,                    /* Purchased Job Item Flag    */~
            ptype$3,                     /* Part Type - HNYMASTR       */~
            quantity$(300)10,            /* QUANTITY INFORMATION       */~
            qtymsg$10,                   /* QUANTITY PROMPT            */~
            readkey$70,                  /* KEY TO PLOW FILE WITH      */~
            rlevel$2,                    /* DRAWING REVISION LEVEL     */~
            rteid$3,                     /* ROUTE THIS BOM USES        */~
            rtestp$(100)4,               /* ROUTE THIS BOM USES        */~
            savebomid$3,                 /* FOR COPY FUNCTION          */~
            savepart$25,                 /* FOR COPY FUNCTION          */~
            scr%(2,09), set%(255),       /* Soft Enable Tables         */~
            srch%(1),                    /* SEARCH RESULTS ARRAY       */~
            seq$(999)4,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            size$(300)10,                /* SIZE (TIMES USED) FIELD    */~
            specobso$4,                  /* Special/Obsolete Part Flag */~
            src$3,                       /* Field for Text Source Code */~
            step$(300)4,                 /* PICK BEFORE RTE STEP NMBR  */~
            step_uom$(14)4,              /* Step or UOM for display    */~
            stepdescr$32,                /* PICK BEFORE RTE STEP DESCR */~
            stkuom$25,                   /* Stock UOM Literal          */~
            switchkey$20,                /* Key for SYSFILE2           */~
            tdate$8,                     /* Temporary Date Variable    */~
            temp$12,                     /* GUESS                      */~
            texta$(196,1)65,             /* Text Matrix for TXTINSUB   */~
            textid$(301)4,               /* Text ID (301 = header)     */~
            textmsg$79,                  /* Message to TXTINSUB        */~
            tfac$(37)1,                  /* SUMMARY SCREEN FAC'S       */~
            type$(300)3,                 /* Part Type when approved    */~
            type$3,                      /* INV ITEM TYPE              */~
            typedescr$30,                /* Part Type Description      */~
            uom$4,                       /* Stock UOM                  */~
            uom$(300)4,                  /* Stock UOM Array            */~
            userid$3,                    /* Operator Identifier        */~
            yield$40,                    /* Yielded Multiplier         */~
            yield(100)                   /* Route Yield Factors        */

        dim f2%(24),                     /* FILE STATUS FLAGS FOR      */~
            f1%(24)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! STCBOMXF ! Standard Cost Set / BOM-RTE X-Ref        *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 6 ! BOMREFER ! COMPONENT REFERENCE FILE                 *~
            * # 7 ! HNYOPTNS ! OPTIONS LIST                             *~
            * # 8 ! RTEMASTR ! STANDARD ROUTING FILE W/ALTERNATE ROUTES *~
            * # 9 ! JBCROSS2 ! JOB RTE/BOM USED CROSS REF.              *~
            * #10 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #11 ! CALMASTR ! Planning Production Calendar File        *~
            * #12 ! TXTFILE  ! SYSTEM TEXT FILE                         *~
            * #13 ! RTEMAST2 ! STANDARD ROUTING TO STEP NUMBER X REF    *~
            * #14 ! BOMSPEC  ! Options selected file                    *~
            * #24 ! BOMCADXR ! BOM to CAD Xref File                     *~
            *************************************************************
            select  #1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  #3, "STCBOMXF",                                      ~
                        varc, indexed, recsize = 72,                     ~
                        keypos = 29, keylen = 33,                        ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

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

            select # 6, "BOMREFER",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 26, keylen = 34,                       ~
                         alt key  1, keypos = 1, keylen = 59

            select  #7,  "HNYOPTNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos = 1, keylen = 54

            select # 8, "RTEMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select # 9,"JBCROSS2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize =  94,                                    ~
                       keypos =29, keylen = 19,                          ~
                       alternate key 1, keypos = 1 , keylen = 47,        ~
                                 key 2, keypos = 48, keylen = 47

            select #10, "ENGMASTR" ,                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            select #11, "CALMASTR",                                      ~
                        varc, indexed, recsize = 1962,                   ~
                        keypos =    1, keylen =   2

            select #12, "TXTFILE",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =  1, keylen =  11

            select #13, "RTEMAST2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 50,                                   ~
                         keypos =   1, keylen = 32

            select #14, "BOMSPEC",                                       ~
                         varc, indexed, recsize = 150,                   ~
                         keypos =  26, keylen = 54,                      ~
                         alt key 1, keypos =  57, keylen = 23

            select #24, "BOMCADXR",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =   1,  keylen = 70

        call "SHOSTAT" ("Preparing For Bills Of Materials Management.")
            call "OPENCHCK" (# 1, 0%, f2%( 1%),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3%),   0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4%),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5%), 200%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6%), 200%, " ")
            call "OPENCHCK" (# 7, 0%, f2%( 7%),   0%, " ")
            call "OPENCHCK" (# 8, 0%, f2%( 8%),   0%, " ")
            call "OPENCHCK" (# 9, 0%, f2%( 9%),   0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10%), 200%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11%),   0%, " ")
            call "OPENCHCK" (#13, 0%, f2%(13%),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14%),   0%, " ")
            call "OPENCHCK" (#24, 0%, f2%(24%),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            lines_allowed% = dim(part$(),1)

*        See if User is an administator
            call "CMSMACHK" ("BOM", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%         ~
                                                else admin% = 0%
            gosub init_enables
            lines_allowed% = dim(part$(),1)

            for i% = 1% to 999%
                convert i% to seq$(i%), pic(###)
                str(seq$(i%),4) = ")"
            next i%

            REM Insure Marker Table Is Intact...
            REM WARNING -- Markers with 'P' as first char are treated as ~
                           Phantoms by the system...
            msg$ = "TABLE01:"
            write #1, using L09440, msg$, "PH", "Phantom Part",            ~
                                         "PHANTOM   ", " ", eod goto L09290
L09290:     write #1, using L09440, msg$, "PA", "Phantom Assembly",        ~
                                         "PHAN. ASSY", " ", eod goto L09310
L09310:     write #1, using L09440, msg$, "ST", "Standard",                ~
                                         "STANDARD  ", " ", eod goto L09330
L09330:     write #1, using L09440, msg$, "SP", "Special",                 ~
                                         "SPECIAL   ", " ", eod goto L09350
L09350:     write #1, using L09440, msg$, "UU", "Use Up",                  ~
                                         "USE UP    ", " ", eod goto L09370
L09370:     write #1, using L09440, msg$, "RE", "Reference Only",          ~
                                         "REFER ONLY", " ", eod goto L09390
L09390:     write #1, using L09440, msg$, "AS", "As Required",             ~
                                         "AS REQUIRD", " ", eod goto L09410
L09410:     write #1, using L09440, msg$, "TL", "Tool",                    ~
                                         "TOOL      ", " ", eod goto L09422
L09422:     write #1, using L09440, msg$, "NC", "No Cost for Std Costing", ~
                                         "NO COST   ", " ", eod goto L09426
L09426:     write #1, using L09440, msg$, "NP", "No Procurement or Cost",  ~
                                         "NO PROC/NC", " ", eod goto L09430
L09430:     write #1, using L09440, msg$, "BP", "By-Product (Neg. Cost)",  ~
                                         "BY-PRODUCT", " ", eod goto L09500
L09440:     FMT CH(8), CH(12), CH(30), CH(225), CH(225)


L09500
*        Load Program Switchs Record...
            locscreen_flag$ = "Y"
            switchkey$ = "SWITCHS.BOM"
            call "READ100" (#01, switchkey$, f1%(1%))
               if f1%(1%) = 1% then get #01 using L09550, locscreen_flag$, ~
                                                 form_calc_flag$
L09550:         FMT POS(23), CH(1), CH(1)
            if form_calc_flag$ = " " then form_calc_flag$ = "N"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            call "ALLFREE"
            errormsg$, message$, assypart$, assypartdescr$,              ~
            bomdescr$, msg$, rteid$, bomid$, textmsg$, rlevel$,          ~
            luser$, dsize$, paflag$, uom$, stkuom$, batch$,              ~
            autoflag$, pjflag$, ptype$, eff_bom$ = " "

            init(" ") bomdescr$(), part$(), quantity$(), size$(), bom$(),~
            bommkr$(), step$(), cbom$(), texta$(), loc$(), locqty$(),    ~
            fixed$(), over$(), date$(), op$(), desc$(), type$(),         ~
            step_uom$(), uom$(), ecrpfk$

            step_uom% = 0%
            mat locind% = zer
            editmode%, copy_mode%, line% = 0% : t% = 1%
            init(hex(00)) textid$()
            call "TXTFUTIL" (#12, f2%(12), "INTL", " ")


L10180:  for fieldnr% = 1% to 8%
             gosub'161(fieldnr%,1%)
                   if enabled% = 0 then L10540
L10210:      gosub'201(fieldnr%)
               if keyhit%  =  1 then gosub startover
               if keyhit%  =  3 and fieldnr% = 3 then gosub clone_bom
               if keyhit% <>  4 then L10300
L10250:           fieldnr% = fieldnr% - 1%
                  if fieldnr% < 1% then L10180
                     gosub'161(fieldnr%,1%)
                     if enabled% <> 0 then L10210
                     goto L10250
L10300:        if keyhit% = 11% then gosub view_ecr_info
               if keyhit% <> 14 then L10510
                  readkey$ = " "
                  hdr$(1), hdr$(3) = "  "
                  hdr$(2) = "  Assembly Number        BOMID"  &          ~
                            "   BOM Description"
                  lockey$ = hex(06) & "Select the Bill of Materials to" &~
                           " Edit.  Use PF-16 to Cancel Selection"

                  call "PLOWCODE" (#5, readkey$, lockey$, -1028%, -.30,  ~
                                   f1%(5), hdr$())
                  if f1%(5) = 0% then L10180
                       assypart$  = readkey$
                       bomid$ = str(readkey$,26%,3%)
                       fieldnr% = 2%
                       gosub set_ecr_prompt
                       goto L10540

L10510:        if keyhit%  = 16 and fieldnr% = 1 then L65000
               if keyhit%  = 5 and bompaging$ <> " " then gosub more_boms
               if keyhit% <>  0 then       L10210
L10540:      gosub'151(fieldnr%)
                   if errormsg$ <> " " then L10210
             next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

L10640:     if maxlines% = lines_allowed% then L10680
                c% = maxlines% + 1%
                gosub inputlines
                if keyhit% <> 16% then L10700
L10680:              gosub columnone
                     goto line_summary
L10700:         maxlines% = maxlines% + 1%
                insert%, insertrtn% = 0%
                gosub inputlocations
                goto L10640

        inputlines
            linemode% = 0
            if editmode% = 2% then L10800
L10770:     gosub columnone
            if maxlines% = 0 and editmode% = 0 then                      ~
                errormsg$ = hex(84) & "Enter The Component List..."
L10800:     for fieldnr% = 1 to 9
                gosub'162(fieldnr%,1%)
                      if enabled% = 0 then L11000
L10830:         gosub'203(fieldnr%)
                      if keyhit%  = 16% then return
                      if keyhit%  = 14% then gosub display_options
                      if keyhit%  = 11% then gosub formula_calculation
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then       L10770
                      if keyhit% <>  6% then L10910
                         gosub prevline
                         goto L11000
L10910:               if keyhit% <> 4% then L10990
L10920:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% > 1% then L10960
                             goto L10800
L10960:                  gosub'162(fieldnr%,1%)
                         if enabled% <> 0% then L10830
                         goto L10920
L10990:               if keyhit% <>  0% then       L10830
L11000:         gosub'152(fieldnr%)
                  if str(errormsg$,,1%) = hex(84) then L11030 /* Warning */
                  if errormsg$ <> " " then L10830
L11030:         next fieldnr%
                return

        prevline
            if c% = 1% then return
                on fieldnr% gosub L11190,           /* Part Number      */~
                                  L11200,           /* Option Marker    */~
                                  L11210,           /* Quantity         */~
                                  L11220,           /* Size             */~
                                  L11230,           /* Added Overage    */~
                                  L11240,           /* Fixed Quantity   */~
                                  L11250,           /* BOM Marker       */~
                                  L11260,           /* Pick before Step */~
                                  L11270            /* Specific BOM Id  */
            return

L11190:         part$    (c%) = part$    (c%-1) : return
L11200:         op$      (c%) = op$      (c%-1) : return
L11210:         quantity$(c%) = quantity$(c%-1) : return
L11220:         size$    (c%) = size$    (c%-1) : return
L11230:         over$    (c%) = over$    (c%-1) : return
L11240:         fixed$   (c%) = fixed$   (c%-1) : return
L11250:         bommkr$  (c%) = bommkr$  (c%-1) : return
L11260:         step$    (c%) = step$    (c%-1) : return
L11270:         cbom$    (c%) = cbom$    (c%-1) : return

        inputlocations
            if locscreen_flag$ = "N" then return  /* SYS Flag Stops Us */
            edtmax%, ss%, cc%, l% = 0
            mat locedt% = con : mat locedt% = (6001%) * locedt%
L11320:     if locmax% >= 6000% or edtmax% >= 999% then return
            edtmax% = edtmax% + 1%
            locedt%(edtmax%) = locmax% + 1%
            ss% = ss% + 1 : cc% = locmax%  + 1
L11360:     if ss% < 37 then L11390
               ss% = 36% : l%  = l% + 1%

L11390:     for fieldnr% = 1 to 1
                gosub'163(fieldnr%)
                      if enabled% = 0 then L11460
L11420:         gosub'204(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L11560
                      if keyhit% <>  0 then       L11420
L11460:         gosub'153(fieldnr%)
                      if errormsg$ = "Location Can't Be Blank" then L11560
                      if errormsg$ <> " " then L11420
                next fieldnr%

            locind%(cc%) = c%
            if insert% = 0% then locmax% = locmax% + 1%
            if insert% <> 0% then return
            if abs(temp) < .00001 then return  else goto L11320

L11560:     errormsg$ = " "
            loc$(cc%), locqty$(cc%) = " "
            if insert% <> 0% then return
            locedt%(edtmax%)=6001%:edtmax%=edtmax%-1%
            return

L12000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        editmode:
            lastfieldnr% = 0%
            editmode% = 1 : fastload% = 0
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (RETURN)."

L12120:     gosub'211(0%)
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then L13000
                  if no_mod% <> 1 and keyhit%  = 12 then gosub delete_bom
                  if keyhit% = 11% then gosub view_ecr_info
                  if keyhit%  = 16 then datasave
                  if keyhit%  = 24 then gosub override
                  if keyhit%  = 25 then gosub'181(301%)
                  if keyhit% <>  0 and keyhit% <> 29% then L12120

L12220:     if cursor%(1) + cursor%(2) = 2 then L13000
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 3 then fieldnr% = 3%
            if fieldnr% > 8% then fieldnr% = 8%
            if keyhit% <> 29% then L12281
               gosub'049(1%, fieldnr%)
               goto editmode
L12281:     gosub'161(fieldnr%, 2%)
               if enabled% = 0% then editmode
            if fieldnr% = lastfieldnr% then    editmode
            if no_mod% = 1 then L12120

L12320:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12320
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L12320
                  lastfieldnr% = fieldnr%
            goto L12220

        delete_bom
            keyno% = 0%
            call "ASKUSER" (keyno%, "DELETE BILL OF MATERIAL SELECTION", ~
                   "Press RETURN to DELETE BOM " & bomid$ &              ~
            " for Assembly Part " & assypart$, "or you can BACK OUT and",~
                   "Press PF-1 To Return to EDITMODE")
                if keyno% <> 0% then return
            brk1% = 28
            brk2% = 28
            if kh% <> 0 then return
            print at(4,1,80); hex(84); "Delete in progress"
            for i = 1 to maxlines%
                call "TXTFUTIL" (#12, f2%(12), "DELE", textid$(i))
            next i
            call "TXTFUTIL" (#12, f2%(12), "DELE", textid$(301))
            readkey$ = all(hex(00))
            str(readkey$,,28) = str(assypart$,,25) & str(bomid$)
            call "DELETE" (#5, readkey$, 28%)
            call "DELETE" (#6, readkey$, 28%)
            call "DELETE" (#7, readkey$, 28%)
            REM Zapp effectivity data if he deleted last BOM for part...
            readkey$ = assypart$
            call "PLOWNEXT" (#5, readkey$, 25%, f1%(5))
                if f1%(5) <> 0 then L12660
            str(readkey$,26) = "1001"
            call "DELETE" (#10, readkey$, 29%)

L12660:     REM  Delete from Cad Cross reference file if there
            readkey$ = all(hex(00))
            str(readkey$,,29%) = "D" & str(assypart$,,25%) & str(bomid$)
L12680:     call "PLOWNXT1" (#24, readkey$, 29%, f1%(24%))
                if f1%(24%) =  0% then L12760
            get #24 using L12695, temp$
L12695:   FMT POS(109), CH(1)
            if temp$ =  "0" then L12680    /* Leave Unprocessed Record */
            delete #24
            goto L12680                    /* Try for another */

L12760:     return clear
            goto inputmode

L13000: REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        line_summary              /* Summary Screen */
            editmode% = 1 : fastload% = 0
            line% = max(0%,min(line%,maxlines%-14%))
            message$ =   "To Modify a Line Item, Position Cursor and" &  ~
                         " press RETURN."
L13110:     errormsg$=" "
L13120:     gosub'115(0%)
              errormsg$ = " "
              if keyhit% =   1 then gosub startover
              if keyhit% =   2 then line% = 0%
              if keyhit% =   3 then line% = maxlines%
              if keyhit% =   4 then line% = line% - 13
              if keyhit% =   5 then line% = line% + 13
              if keyhit% =   6 then line% = line% - 1
              if keyhit% =   7 then line% = line% + 1
              line% = max(0%,min(line%,maxlines%-14%))
              if keyhit%  = 14 then t% = 1% + mod(t%,2%)
              if keyhit%  =  9 then       editmode
              if keyhit%  = 16 then       datasave
              if keyhit%  = 25 then gosub'181(301%)
              if keyhit% <> 28 then L13280
                 fieldnr% = 0
                 goto deletemode
L13280:     fieldnr% = cursor%(1) - 5
            if keyhit% <> 11 or cursor%(1) > 1 then L13310
                c% = maxlines% : goto L13390
L13310:     errormsg$ = hex(84) & "Please Position Cursor First..."
            if keyhit% < 8 then errormsg$ = " "
            if keyhit% = 14 then errormsg$ = " "
            if fieldnr% < 0 or fieldnr% > 14 then L13120
            if fieldnr% = 0 and keyhit% = 0 then fieldnr% = 1
            if fieldnr% = 0 and keyhit% <> 11 then L13120
                c% = min(line% + fieldnr%, maxlines%)
                if c% = 0 and keyhit% <> 11 then L13120
                fieldnr% = c% - line%
L13390:     if keyhit% <> 8% then L13400
                gosub  editlocations
                goto L13110
L13400:     errormsg$ = "Sorry, BOM Can't Be Changed."
            if no_mod% = 1 and keyhit% <> 0 then L13120
            errormsg$ = " "
            if keyhit% = 12 then deletemode
            if keyhit% = 11 then insertmode
            if keyhit% <> 0 then line_summary

        editpg3       /* Line Item Detail - First Screen      */
            lastfieldnr% = 0%
            errormsg$ = " "
            gosub describe_line
            message$ = "To Modify Displayed Values, Position Cursor " &  ~
                       "To Desired Value And Press RETURN."
            linemode% = 1

L13550:     gosub'113(0%)
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  <  2% or keyhit%  >  8% then L13650
                     if keyhit%  =  2 then c% = 1
                     if keyhit%  =  3 then c% = maxlines%
                     if keyhit%  =  6 then c% = max(1, c%-1)
                     if keyhit%  =  7 then c% = min(maxlines%, c%+1)
                     if keyhit%  =  8 then gosub editlocations
                     goto editpg3
L13650:           if keyhit%  =  9 then editmode
                  if keyhit%  = 14 then gosub display_options
                  if keyhit%  = 16 then line_summary
                  if keyhit%  = 25 then gosub'181(c%)
                  if keyhit%  = 29 then L13750
                  if keyhit% <>  0 then L13550
                  if no_mod% <>  1  then L13750
                     errormsg$ = "BOM Can't Be Modified"
                     goto L13550

L13750:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1% or fieldnr% > 9% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            if fieldnr% <> 1% then L13790
                editmode% = 2%
                oldpart$ = part$(c%)
                gosub inputlines
                gosub editlocations
                linemode%, editmode% = 1%
                goto editpg3
L13790:     if keyhit% <> 29% then L13820
                 gosub'049(2%, fieldnr%)
                 goto editpg3
L13820:     gosub'162(fieldnr%, 2%)
            if enabled% = 0% then editpg3

L13870:     gosub'113(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =11% then gosub formula_calculation
                  if keyhit% <>  0 then L13870
            gosub'152(fieldnr%)
                  if str(errormsg$,,1) = hex(84) then L13930 /* Warning */
                  if errormsg$ <> " " then L13870
L13930:           lastfieldnr% = fieldnr%
            goto L13750

        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            *                                                           *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        insertmode
            if maxlines% = lines_allowed% then line_summary

            REM Copy all Elements Up One...
            maxlines%=maxlines%+1%
            c% = c% + 1
            if c%=maxlines% then L14190
                roll% = -1
                for temp% = maxlines% to c% step -1
                     gosub roll_lines
                next temp%
                if locmax% = 0% then L14190
                    for i% = 1% to locmax%
                     if locind%(i%) < c% then L14170
                     locind%(i%) = locind%(i%) + 1%
L14170:             next i%

L14190:     REM Get Line Item Data...
            gosub inputlines
            if keyhit% = 16% then delete_line
            insert%, insertrtn% = 0%
            gosub inputlocations
        goto insertmode

        deletemode
            if maxlines% = 0 then line_summary
            message$ = "To DELETE Flashing Data, press RETURN, To Return ~
        ~without Delete, press PF1."
            gosub'125(fieldnr%)
                  if keyhit%=1 then line_summary
                  if keyhit%<>0 then deletemode
            if fieldnr% <> 0 then delete_line
            brk1% = 28
            brk2% = 28
            if kh% <> 0 then line_summary
            for c% = 1 to maxlines%  /* Kill'em All */
                call "TXTFUTIL" (#12, f2%(12), "XOUT", textid$(c%))
                gosub columnone
            next c%
            init (" ") loc$(), locqty$()
            locmax% = 0%
            maxlines% = 0
            goto line_summary

        delete_line
            brk1% = 31
            brk2% = 56
            if kh% <> 0 then line_summary
            call "TXTFUTIL" (#12, f2%(12), "XOUT", textid$(c%))
            gosub delete_it
            goto line_summary

        delete_it
            if c%=maxlines% then L14580
                roll% = 1
                for temp% = c% to maxlines%-1%
                     gosub roll_lines
                next temp%
L14580:         if locmax%=0% then L14640
                   for i%=1 to locmax%
                       if locind%(i%) < c% then L14630
                       if locind%(i%) = c% then locind%(i%) = 0%
                       locind%(i%) = locind%(i%) - 1%
L14630:            next i%
L14640:     c%=maxlines%
            gosub columnone
            maxlines%=maxlines%-1%
        return

        roll_lines
                part$     (temp%) = part$     (temp%+roll%)
                desc$     (temp%) = desc$     (temp%+roll%)
                oldseq$   (temp%) = oldseq$   (temp%+roll%)
                op$       (temp%) = op$       (temp%+roll%)
                quantity$ (temp%) = quantity$ (temp%+roll%)
                size$     (temp%) = size$     (temp%+roll%)
                over$     (temp%) = over$     (temp%+roll%)
                fixed$    (temp%) = fixed$    (temp%+roll%)
                bommkr$   (temp%) = bommkr$   (temp%+roll%)
                cbom$     (temp%) = cbom$     (temp%+roll%)
                step$     (temp%) = step$     (temp%+roll%)
                textid$   (temp%) = textid$   (temp%+roll%)
                uom$      (temp%) = uom$      (temp%+roll%)
        return

        columnone
            part$(c%), errormsg$, quantity$(c%), bommkr$(c%), cbom$(c%), ~
            descr$, size$(c%), step$(c%), textid$(c%), ext$, desc$(c%),  ~
            fixed$(c%), over$(c%), mkrdescr$, cbomdescr$, stepdescr$,    ~
            op$(c%), yield$, stkuom$, uom$, typedescr$, type$(c%),       ~
            oldseq$(c%), uom$(c%) = " "
            return

        REM *************************************************************~
            *               M I S C   R O U T I N E S                   *~
            *-----------------------------------------------------------*~
            * Some Miscellaneous routines reside here...                *~
            *************************************************************

        describe_line
            gosub describe_part
             gosub describe_marker
              gosub describe_ext
               gosub describe_rte_step
                gosub describe_cbom
        return

        describe_part
            type$ = "NS" : typedescr$, stkuom$ = " "
            call "DESCRIBE" (#4, part$(c%), descr$, 1%, f1%(4))
            if f1%(4) = 1% then L15100
                descr$ = "(Non-Stocked Part)" : return
L15100:     get #4, using L15110, uom$, specobso$, type$
L15110:     FMT POS(74), CH(4), POS(166), CH(4), POS(180), CH(3)
            if specobso$ <> "OBSO" then L15130
                descr$ = "(OBSOLETE Part)"
L15130:     typedescr$ = hex(94) & "(Unknown Part Type)"
            convert type$ to type%, data goto L15160
            call "HNYTYPE" (type%, typedescr$, 1%)
L15160:     stkuom$ = "(Stocking UOM = " & uom$ & ")"
        return

        describe_marker
            call"DESCRIBE"(#1,"TABLE01:"&bommkr$(c%),mkrdescr$,1%,f1%(1))
                if f1%(1) = 0 then mkrdescr$ = hex(94)&"(Unknown Marker)"
        return

        describe_ext
            ext$ = "(Net Multiplier =" : yield$ = " "
            temp, temp1, temp2 = 0
            convert quantity$(c%) to temp, data goto L15280
L15280:     convert size$(c%) to temp1, data goto L15290
L15290:     convert over$(c%) to temp2, data goto L15300
L15300:     netqty, totqty = (temp * temp1)/oldbatch + temp2
            if rtestp$() = " " then L15450 : i% = 1
            if step$(c%) = " " then L15380
               search rtestp$() = step$(c%) to srch%() step 4
                   if srch%(1) = 0 then errormsg$= "Invalid Route Step Id~
        ~. In BOM... Must Be Changed:  " & step$(c%)
                   if srch%(1) = 0 then L15450
               i% = (srch%(1)+3)/4
L15380:     if yield(i%) = 100 then L15450
               if yield(i%) = 0 then yield(i%) = .000000000001
               netqty = totqty * 100/yield(i%)
               yield$ = "(Yield =###%, Adj. Multiplier ="
               call "CONVERT" (yield(i%), 0.01, str(yield$,9,3))
               temp3 = netqty
               call "CONVERT" (temp3, -0.4, str(yield$,33,7))
               yield$ = yield$ & ")"
L15450:        temp3 = netqty
               call "CONVERT" (temp3, -0.4, str(ext$,19,11))
               ext$ = ext$ & ")"
        return

        describe_rte_step
            stepdescr$ = " "
            if rteid$ <> " " then stepdescr$ = "(First)"
            if step$(c%) = " " then return
            stepdescr$ = hex(94) & "(Not On File)"
            readkey$ = str(assypart$,,25) & str(rteid$,,3) & step$(c%)
            call "READ100" (#13, readkey$, f1%(13))
                if f1%(13) = 0 then return
            get #13, using L15580, str(readkey$,29)
L15580:     FMT XX(32), CH(3)
            call "READ100" (#8, readkey$, f1%(8))
                if f1%(8) = 0 then return
            get #8, using L15620, stepdescr$
L15620:     FMT POS(97), CH(30)
            call "PUTPAREN" (stepdescr$)
        return

        describe_cbom
            cbomdescr$ = " " : if cbom$(c%) = " " then return
            readkey$ = str(part$(c%),,25) & str(cbom$(c%)) & "  0"
            call "DESCRIBE" (#5, readkey$, cbomdescr$, 1%, f1%(5))
                if f1%(5) = 0 then cbomdescr$ = hex(94) & "(Not On File)"
        return

        deffn'181(i%)    /* Text Management Routine */
            textmsg$ = "BOM for Assy # " &  assypart$ & ", Component Part~
        ~ # " & part$(c%)
            if i% = 301 then textmsg$ = "BOM Text for Assy # "& assypart$
            if i% = 301 then src$ = "009" else src$ = "010"
            if no_mod% = 1% then                                         ~
            call "TXTDSPLY" (#12, f2%(12), src$, textmsg$, textid$(i%),  ~
                            texta$())      else                          ~
            call "TXTINSUB" (#12, f2%(12), src$, textmsg$, textid$(i%),  ~
                            texta$())
        return

        formula_calculation
            bom_factors(1%) = 1
            convert batch$ to bom_factors(1%), data goto L15880
L15880:     call "BOMFCALC" (assypart$, str(part$(c%),,25%),             ~
                             bom_factors(), " ", #1, #4,                 ~
                             results, results1, results2, return_code%)
            if return_code% <> 0% then return
            if bom_factors(1%) = 1 then  L15950
                call "CONVERT" (results, -0.8, quantity$(c%))
                if results2 <> 1 then errormsg$ = "WARNING: Loss of " &  ~
                "precision will occur, cannot use SIZE (Times Used) Field"
                return
L15950:     call "CONVERT" (results1, -0.4, quantity$(c%))
            call "CONVERT" (results2, -0.4, size$(c%))
            return

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                  LOCATION REFERENCES                      *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        editlocations
            gosub describe_marker
            mat locedt% = con : mat locedt%=(6001%)*locedt%
            l%, cc%, ss%, edtmax% = 0%
            if locmax% = 0% then L16290
               for i% = 1% to locmax%
                   if locind%(i%) <> c% then L16270
                      edtmax% = edtmax% + 1%
                      locedt%(edtmax%) = i%
L16270:        next i%

L16290:     errormsg$=" "
            l% = max(0%, min(l%, edtmax%-36%))
L16310:     message$ =   "To Modify a Line Item, Position Cursor and" &  ~
                         " press RETURN."
L16330:     gosub'214(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then editmode
                  if keyhit%  =  2 then l% = 0
                  if keyhit%  =  3 then l% = edtmax%
                  if keyhit%  =  4 then l% = l% - 33
                  if keyhit%  =  5 then l% = l% + 33
                  if keyhit%  =  6 then l% = l% - 1
                  if keyhit%  =  7 then l% = l% + 1
                  l% = max(0%,min(l%,edtmax%-36%))
                  if keyhit%  =  8 then c% = max(1,c%-1)
                  if keyhit%  = 10 then c% = min(maxlines%, c%+1)
                  if keyhit%  =  8 or keyhit% = 10 then editlocations
                  if keyhit%  = 16 then return
                  if keyhit%  = 28 then gosub deletemode1
                  if no_mod% = 1 then L16330
                  ss% = cursor%(1) - 7%
                  if ss% > 0 then L16560
                     if keyhit% <> 11 then L16330
                     if ss% = 0 then L16610  /* Insert Before Line 1 */
                     if ss% <> -6 then L16330
                     ss% = 12345%           /* Added At End */
                     goto L16610
L16560:           if ss% > 12 then L16330
                  if cursor%(2) > 29 then ss% = ss% + 12
                  if cursor%(2) > 57 then ss% = ss% + 12
                  if ss% > edtmax% - l% then L16330
                  if keyhit%  <> 11 then L16630
L16610:                       gosub insertmode1
                              goto L16290
L16630:           if keyhit%  <> 12 then L16660
                              gosub deletemode1
                              goto L16290
L16660:           if keyhit%  <>  0 then L16290

            REM NOW FIGURE OUT WHICH FIELD HE HIT.
                cc% = min(ss% + l%, edtmax%)
                ss% = cc% - l%
                cc% = locedt%(cc%)
                if cc% > locmax% then L16330

                gosub'163(1%)
                      if enabled% = 0 then L16310        /* Disabled    */
L16760:         gosub'214(1%)            /* NOW GET FIELD TO MODIFY    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then L16760
                gosub'153(1%)
                      if errormsg$ <> " " then L16760
                goto L16310

        REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *                                                           *~
            * HANDLES INSERTION OF A LINE ITEM INTO THE B.O.M.          *~
            *************************************************************

        insertmode1
            insertrtn% = 1%  /* Sets PF-16 text on screen */
L20080:     if locmax% = 6000% then return
            if edtmax% = 999% then return

            if ss% < 0 then return
               if ss% > 36 then l% = max(edtmax%-36%, 0%)
               cc% = 1% + min(edtmax%,ss% + l%)
               ss% = cc% - l%

               if cc% <= edtmax% then L20210
                  insert% = 0%
                  ss% = ss% - 1%
                  goto L11320

L20210:     for i%=locmax% to  locedt%(cc%) step -1
                loc$(i%+1%) = loc$(i%)
                locqty$(i%+1%) = locqty$(i%)
                locind%(i%+1%) = locind%(i%)
            next i%

                init (" ") loc$(locedt%(cc%)), locqty$(locedt%(cc%))
                locind%(locedt%(cc%)) = 0%
                locmax% = locmax% + 1%

            for i%=edtmax% to cc% step -1
                locedt%(i%+1%)=locedt%(i%)+1%
            next i%

            edtmax%=edtmax%+1%
            insert% = 1%
            cc% = locedt%(cc%)
            gosub L11360
            if keyhit% <> 16% then L20080

            for i%=ss% + l% to edtmax%
                locedt%(i%)=locedt%(i%+1%)
            next i%
            edtmax% = edtmax% - 1%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *                                                           *~
            * DELETES A LINE ITEM FROM THE BOM                          *~
            *************************************************************

        deletemode1
            if edtmax% = 0 then return
            if keyhit%  = 28 then ss% = 0
            if keyhit%  = 28 then L21160
            if ss% < 1 then return
               cc% = min(edtmax%,ss% + l%)
               ss% = cc% - l%
               cc% = locedt%(cc%)
               if cc% > locmax% then return

L21160:     gosub'234(ss%)
                  if keyhit%  =  1 then       return
                  if keyhit% <>  0 then       L21160
                  keyhit% = 99

            if ss% <> 0 then L21300
               ss%, l%, edtmax% = 0
               mat locedt% = con
               mat locedt% = (6001%) * locedt%
               for i% = 1 to locmax%
                   if locind%(i%) = c% then locind%(i%)=0%
               next i%
               return

L21300:     locind%(cc%)=0%
            if edtmax%=ss%+l% then L21350
               for i%=ss%+l% to edtmax%-1%
                     locedt%(i%)=locedt%(i%+1%)
               next i%
L21350:     locedt%(edtmax%)=6001%
            edtmax%=edtmax%-1%
            return

        REM *************************************************************~
            *               I N P U T   N E W   K E Y                   *~
            *                                                           *~
            * ALLOWS USER TO 'COPY' THIS PART/BOM TO ANOTHER PART/BOM   *~
            *************************************************************

        clone_bom
            readkey$, savepart$ = assypart$
            savebomid$ = bomid$
            bomdescr_org$ = bomdescr$
            incl(1) = 0
            hdr$(2) = "  Part Assemblies             Part Descriptions"
            hdr$(1) = "  Existing BOMs For Part.  Use PF-1 To Select Anot~
        ~her Part."
            hdr$(3) = hex(ac) & "Select the Assembly Part And/Or BOM To C~
        ~opy From.  Use PF-16 to Cancel Copy."
            lockey$ = hex(06) & "Select the Part Assembly"
            REM *** Get Part & BOMID To Copy ***
            errormsg$ = hex(84) & "Copy Request Canceled"
            call "PLOWCODE" (#5, readkey$, lockey$, 8025%, -.30, f1%(5), ~
                         hdr$(), 3.32, 57, incl(), incl$(), " ", " ", #4)
                if f1%(5) = 0% then return
            assypart$  = readkey$
            bomid$ = str(readkey$,26%,3%)

            return clear : errormsg$ = " "
            print at(4,1,80); hex(84); "Loading Data..."
            copy_mode% = 1
            gosub L30000  /* Load Data */
            copy_mode% = 0
            if assypart$ <> savepart$ then rteid$ = " "
            assypart$ = savepart$
            bomid$    = savebomid$
            init(" ")  appron$, apprby$, enteron$, enterby$

            REM Copy Text Into 'Memory', creating new Ids...
            call "TXTFUTIL" (#12, f2%(12), "INTL", " ")  /*Just In Case*/
*        ** ASK to Copy Header Text to New BOM **
L22344:         keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "***  COPY BOM TEXT  ***", "Pre~
        ~ss PF1 to INCLUDE Copying BOM Text", "- OR -",        "Press RETU~
        ~RN NOT to Copy BOM Text")
                if keyhit1%  = 0% then L22400   /* Jump over Copy */
                if keyhit1% <> 1% then L22344   /* Try Again */

            call "TXTFUTIL" (#12, f2%(12), "COPY", textid$(301))
            for i% = 1% to maxlines%
                call "TXTFUTIL" (#12, f2%(12), "COPY", textid$(i%))
            next i%
L22400:     goto editmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if no_mod% = 1 then L23210
            for temp% = 1 to maxlines%
                if step$(temp%) = " " then L23140
                search rtestp$() = step$(temp%) to srch%() step 4
                if srch%(1) = 0 then errormsg$= "Invalid Route Step Id. I~
        ~n BOM... Must Be Changed:  " & step$(temp%)
                if srch%(1) = 0 then temp% = maxlines%
L23140:     next temp%
            if rteid$ = " " then L23180
            if rtestp$() = " " then errormsg$ = "Route Not On File, Plea"~
                         & "se Change To Save Modifications"
L23180:     if errormsg$ <> " " then editmode

            gosub L31000                  /* WRITE BOM RECORD           */
L23210:     lastpart$ = assypart$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%,mode%)
                  call "ENABLSUB" ("SET", "BOMINPUT", scr%(), set%(), 1%,~
                                   fieldnr%, mode%, enabled%)
                  if mode% = 2% and fieldnr% < 3% then return

                  message$ = " "
                  on fieldnr% gosub L24200,         /* Assemply Part #  */~
                                    L24230,         /* Which BOM?       */~
                                    L24270,         /* Description      */~
                                    L24310,         /* Batch Size       */~
                                    L24370,         /* Peg To Route Id. */~
                                    L24410,         /* Drawing Size     */~
                                    L24440,         /* Revision Level   */~
                                    L24470          /* Purch'd Job Item */
                     return
L24200:     REM DEFAULT/ENABLE FOR ASSEMBLY PART NUMBER
                message$ = "Enter Part To Manage Component List For."
                return
L24230:     REM DEFAULT/ENABLE FOR BOM STRUCTURE ID
                message$ = "Enter The BOM ID To Be Managed"
                gosub read_boms
                return
L24270:     REM DEFAULT/ENABLE FOR BOM DESCRIPTION
                message$ = "Enter any desired text.  Use PF(3) To Copy An~
        ~other BOM."
                if mode% = 2% then message$ = str(message$,,24%)
                return
L24310:     REM DEFAULT/ENABLE FOR BATCH SIZE
                message$ = "Enter Batch Quantity."
                if batch$ <> " " then return
                   batch$ = "1"
                   oldbatch = 1
                   return
L24370:     REM DEFAULT/ENABLE FOR ROTUE ID
                message$="Enter The RTE ID to be used when planning this ~
        ~assembly. Blank is valid for now"
                return
L24410:     REM DEFAULT/ENABLE FOR DRAWING SIZE
                message$ = "Enter Drawing Size Or Location, If Desired"
                return
L24440:     REM DEFAULT/ENABLE FOR REVISION LEVEL
                message$ = "Enter Drawing Revision Level, If Desired"
                return
L24470:     REM DEFAULT/ENABLE FOR PURCHASED JOB FLAG
                if pjflag$ = " " then pjflag$ = "N"
                message$ = "Enter 'Y' If This Is A Purchased Job Item"
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   T A B L E      *~
            *                                                           *~
            * DEFAULTS/ENABLES FOR TABULAR INPUT. EVERY FIELD ENABLED,  *~
            * FIELD DESCRIPTIONS SET HERE...                            *~
            *************************************************************

            deffn'162(fieldnr%,mode%)
                  call "ENABLSUB" ("SET", "BOMINPUT", scr%(), set%(), 2%,~
                                   fieldnr%, mode%, enabled%)
                  if fieldnr% = 4% and oldbatch <> 1 then enabled% = 0%
*                IF MODE% = 2% AND FIELDNR% <> 2% THEN RETURN
                  if linemode% = 1 then fastload% = 0
                  message$ = " "
                  on fieldnr% gosub L25230,         /* Part Number      */~
                                    L25270,         /* Option Marker    */~
                                    L25330,         /* Quantity         */~
                                    L25360,         /* Size (Times Used)*/~
                                    L25410,         /* Added Overage    */~
                                    L25450,         /* Fixed Quantity   */~
                                    L25500,         /* BOM Marker       */~
                                    L25550,         /* Specific BOM     */~
                                    L25580          /* Route Step       */
                     return
L25230:     REM DEFAULT/ENABLE FOR COMPONENT PART NUMBER.
                message$ = "Enter Component Part Number. Blank or Partial~
        ~ To Search Part Master File."
                return
L25270:     REM DEFAULT/ENABLE FOR OPTION MARKER.
                message$ = "Is This an Option Part? (Y/N)"
                if assytype$ = "000" then return
                   enabled% = 0%
                   op$(c%) = "N"
                   return
L25330:     REM DEFAULT/ENABLE FOR QUANTITY
                message$ = "Enter Quantity Required Of This Part"
                return
L25360:     REM DEFAULT/ENABLE FOR SIZE (TIMES USED)
                if fastload% = 1 then  return
                if oldbatch <> 1 then return
                message$ = "Enter Size (Times Used) For This Part"
                return
L25410:     REM DEFAULT/ENABLE FOR ADDED OVERAGE
                if fastload% = 1 then  return
                message$ = "Enter Overage Quantity."
                return
L25450:     REM DEFAULT/ENABLE FOR FIXED QTY PER RUN
                if fastload% = 1 then  return
                message$ = "Enter Fixed Quantity To Add (Regardless of Qu~
        ~antity To Build)."
                return
L25500:     REM DEFAULT/ENABLE FOR BOM MARKER
                message$ = "STd, PHantom, REf, AS req, Use Up, SPec, TooL~
        ~, Phantom Assy, No Cost, No Proc/NC"
                if bommkr$(c%)=" " then bommkr$(c%) = "ST"
                return
L25550:     REM DEFAULT/ENABLE FOR SPECIFIC BILL TO USE
                message$ = "Input Component Specific BOM"
                return
L25580:     REM DEFAULT/ENABLE FOR ROUTE STEP TO PICK ON
                if step$(c%) <> " " then L25610
                if rteid$ = " " then return
L25610:         message$ = "Enter Route Step to pick BEFORE.  Blank Is Eq~
        ~ual To The First Step In Routing."
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   T A B L E      *~
            *                                                           *~
            * DEFAULTS/ENABLES FOR TABULAR INPUT. EVERY FIELD ENABLED,  *~
            * FIELD DESCRIPTION FLOWN BELOW IN MESSAGE$.                *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  message$ = " "
                  on fieldnr% gosub L26130          /* LOCATION & QTY   */
                  return

L26130:           message$="Enter Location & Quantity for this Reference"
                  enabled%=1
                  return

        REM *************************************************************~
            *         I N I T I A L I Z E   E N A B L E S               *~
            * --------------------------------------------------------- *~
            * Initialize Soft Enable Settings.                          *~
            *************************************************************
        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1, 1) =  1% : set%( 1) = 13%      /* Assemply Part #  */
            scr%(1, 2) =  2% : set%( 2) = 13%      /* Which BOM?       */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Description      */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* Batch Size       */
            scr%(1, 5) =  5% : set%( 5) =  2%      /* Peg To Route Id. */
            scr%(1, 6) =  6% : set%( 6) =  2%      /* Drawing Size     */
            scr%(1, 7) =  7% : set%( 7) =  2%      /* Revision Level   */
            scr%(1, 8) = 17% : set%(17) =  2%      /* Purchased Job?   */

            scr%(2, 1) =  8% : set%( 8) =  2%      /* Part Number      */
            scr%(2, 2) =  9% : set%( 9) =  2%      /* Option Marker    */
            scr%(2, 3) = 10% : set%(10) =  2%      /* Quantity         */
            scr%(2, 4) = 11% : set%(11) =  2%      /* Size (Times Used)*/
            scr%(2, 5) = 12% : set%(12) =  2%      /* Added Overage    */
            scr%(2, 6) = 13% : set%(13) =  2%      /* Fixed Quantity   */
            scr%(2, 7) = 14% : set%(14) =  2%      /* BOM Marker       */
            scr%(2, 8) = 15% : set%(15) =  2%      /* Specific BOM     */
            scr%(2, 9) = 16% : set%(16) =  2%      /* Route Step       */
        REM Next slot available is '18'
            call "ENABLSUB" ("INIT", "BOMINPUT", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "BOMINPUT", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
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
            goto inputmode

L30000: REM *************************************************************~
            *        L O A D   O L D   B O M   F R O M   F I L E        *~
            *                                                           *~
            * THIS ROUTINE LOADS THE OLD BILL OF MATERIALS FROM THE     *~
            * FILE, RETURNS IF THERE IS NOT ONE.  NOTE THAT FOR THIS    *~
            * FILE, THE PLOWNEXT AND DELETE ROUTINES MUST BE CUSTOM -   *~
            * CODED, SINCE THE PRIMARY KEY IS IN WRONG PLACE.           *~
            *************************************************************

            maxlines%, c%, locmax%, bom_on_file% = 0%
            readkey$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
            call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 0 then L30230
            get #5, using L30150, bomdescr$, rteid$, textid$(301), dsize$,~
                                 rlevel$, temp%, temp1%, luser$,         ~
                                 oldbatch, appron$, apprby$, enteron$,   ~
                                 enterby$, pjflag$
            if pjflag$ = " " then pjflag$ = "N"
            if copy_mode% = 0 then L30115
L30086:         keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "*** BOM DESCRIPTION ***", "Pre~
        ~ss PF1 to INCLUDE Copying BOM Description", "- OR -", "Press RETU~
        ~RN NOT to Copy BOM Description")
                if keyhit1% = 1% then L30135
                if keyhit1% = 0% then L30094
                goto L30086
L30094:         if bomdescr_org$ <> " " then bomdescr$ = bomdescr_org$   ~
                               else bomdescr$ = "COPIED FROM: " & bomid$
                date$(1) = date$
                date$(2) = " "
                luser$ = " "
                goto L30135
L30115:     if temp% <> 0 then convert temp% to date$(1), pic(######)
            if temp1% <> 0 then convert temp1% to date$(2), pic(######)
            call "DATEFMT" (date$(1))
            call "DATEFMT" (date$(2))
L30135:     if oldbatch <= 0 or oldbatch > 9999999 then oldbatch = 1
            call "CONVERT" (oldbatch, -0.4, batch$)

L30150:     FMT XX(56),                  /* Key Area                   */~
                CH(30),                  /* DESCRIPTION                */~
                CH(3),                   /* WHICH ROUTE                */~
                CH(4),                   /* TEXTID Header              */~
                CH(1),                   /* DRAWING LOCATION           */~
                CH(2),                   /* DRAWING REVISION LEVEL     */~
                2*BI(3),                 /* CREATE FOLLOWED BY MOD DATE*/~
                CH(3),                   /* LAST USERS ID              */~
                XX(1),                   /* PHANTOM ASSY FLAG          */~
                PD(14,4),                /* BATCH QUANTITY             */~
                CH(6),                   /* Approved ON                */~
                CH(15),                  /* Approved BY                */~
                CH(6),                   /* Approval Entered ON        */~
                CH(3),                   /* Approval Entered ON        */~
                CH(1),                   /* Purchased Job Item Flag    */~
                CH(5)                    /* Filler Rest Record         */

L30230:     call "PLOWNEXT" (#5, readkey$, 28%, f1%(5))
                 if f1%(5) = 0 then L30360
            if copy_mode% <> 1 then L30260
                if str(key(#5,1),,25) <> savepart$ then L30270
                errormsg$ = "Redundant Component Skipped" & savepart$
                goto L30230
L30260:     if c% = 0 then print at(4,1,80);hex(84);                     ~
                                  "BOM Is On File, Loading Structure..."
L30270:     c%, maxlines% = c% + 1
            get    #5, using L30440, part$(c%), oldseq$(c%), quantity,    ~
                             xused, fixed,                               ~
                             over, bommkr$(c%), op$(c%), cbom$(c%),      ~
                             textid$(c%), step$(c%), type$(c%), autoflag$

            call "DESCRIBE" (#4, part$(c%), desc$(c%), 0%, f1%(4))
            uom$(c%) = "None"
            if f1%(4%) <> 1% then L30305
                get #4, using L30304, uom$(c%)
L30304:             FMT POS(74), CH(4)
L30305:     if oldbatch = 1 then L30320
               quantity = round(quantity * xused * oldbatch, 4)
               xused = 1
L30320:     call "CONVERT" (quantity, -0.4, quantity$(c%))
            call "CONVERT" (xused, -0.4, size$(c%))
            call "CONVERT" (fixed, -0.2, fixed$(c%))
            call "CONVERT" (over, -0.4, over$(c%))
            if copy_mode% = 1 then L30350  /* Text is done elsewhere */
            call "TXTFUTIL" (#12, f2%(12), "LOAD", textid$(c%))
L30350:     goto L30230

L30360:     if maxlines% = 0% then check_route
            if copy_mode% = 1 then L30380  /* Text is done elsewhere */
            bom_on_file% = 1%
            call "TXTFUTIL" (#12, f2%(12), "LOAD", textid$(301))
L30380:     init (hex(00)) lockey$
            str(lockey$,,28) = str(assypart$,,25) & str(bomid$,,3)

L30395:     call "PLOWNEXT" (#6, lockey$, 28%, f1%(6))
            if f1%(6)=0 then check_route
            locmax% = locmax% + 1%
            get #6, using L30415, temp$, loc$(locmax%), temp
L30415:         FMT XX(53), CH(3), XX(3), CH(6), PD(14,4)
            call "CONVERT" (temp, -0.2, locqty$(locmax%))
            convert temp$ to locind%(locmax%), data goto L30430
L30430:     goto L30395

L30440:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                XX(3),                   /* WHICH ALTERNATE BOM?       */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                PD(14,4),                /* FIXED QUANTITY             */~
                PD(14,4),                /* ADDED OVERAGE              */~
                CH(2),                   /* BOM MARKER                 */~
                CH(01),                  /* OPTIONAL COMPONENT FLAG    */~
                CH(3),                   /* COMPONENTS SPEC BOM        */~
                CH(04),                  /* TEXTID Line Item           */~
                CH(04),                  /* 'PICK BEFORE' RTE STEP ID. */~
                CH(03),                  /* Part Type when approved.   */~
                CH(01),                  /* Auto Replace Flag          */~
                XX(44)                   /* Filler Rest Record         */

        check_route
            rtestp$()=" " : mat yield = con : mat yield = (100)*yield
            if rteid$=" " then return
            readkey$ = str(assypart$,,25) & str(rteid$,,3) : u3% = 0
L30545:     call "PLOWNEXT" (#13, readkey$, 28%, f1%(13))
                     if f1%(13) = 0 then return
            u3% = u3% + 1
            get #13, using L30590, lockey$, rtestp$(u3%), str(lockey$,29)
            call "READ100" (#8, lockey$, f1%(8))
                if f1%(8) <> 0 then get #8, using L30575, yield(u3%)
L30575:     FMT POS(80), PD(14,7)
            goto L30545

L30590:     FMT                          /* RTEMAST2                   */~
                CH(28),                  /*       PART+ROUTE           */~
                CH(4),                   /* USER DEFINED STEP NUMBER   */~
                CH(3)                    /* Sequence Number            */

L31000: REM *************************************************************~
            *             W R I T E   B O M   T O   F I L E             *~
            *                                                           *~
            * DELETES ALL THE OLD BILL OF MATERIALS ENTRIES FOR THIS    *~
            * PART AND WRITES THEM TO THE FILE.                         *~
            *************************************************************

            if maxlines% = 0 then L31210
            mat locedt% = zer
            for i%=1% to maxlines%
               convert i% to str(part$(i%),26,3), pic(000)
            next i%
L31120:     keyno% = 2%
            call "ASKUSER" (keyno%, "BILL OF MATERIALS STORAGE ORDER",   ~
                    "Press PF-16 to save Bill of Materials as entered.", ~
                    "----- OR -----",                                    ~
                    "Press PF-32 to save Bill of Materials in Part Nu" & ~
                    "mber Sequence.")

            if keyno% <> 16% and keyno% <> 32% then L31120

L31210:     call "SHOSTAT"("Saving BOM " & bomid$ & ", Part " & assypart$)
            readkey$ = str(assypart$,,25) &  str(bomid$,,3) & " "
            init (hex(00)) lockey$
            str(lockey$,,28) = str(readkey$,,28)
            call "DELETE" (#5, readkey$, 28%)
            call "DELETE" (#6, lockey$, 28%)
            if maxlines% = 0 then L32430

            if keyno% <> 32% then L31330
            call "SORT" addr(str(part$(),1), maxlines%, 28%,             ~
                                            str(part$(),1), 1%, 25%, "A")

L31330:     if bom_on_file% = 0% then date$(1) = date$
            call "DATUNFMT" (date$(1))
            temp%, temp1% = 0
            search str(bommkr$()) = "PA" to srch%() step 2
            if srch%(1%) <> 0% then paflag$ = "Y" else paflag$ = " "
            tdate$ = date$(1%)
            call "DATEFMT" (tdate$, temp% )
               temp% = mod( temp%, 1000000% )
            call "DATEFMT" (date,   temp1%)
               temp1% = mod(temp1%,1000000% )
L31400:     oldbatch = 1:convert batch$ to oldbatch, data goto L31420

L31420:     write #5 using L31480,                                        ~
                              " ", assypart$, bomid$, "  0", bomdescr$,  ~
                              rteid$, textid$(301), dsize$, rlevel$,     ~
                              temp%, temp1%, userid$, paflag$, oldbatch, ~
                              appron$, apprby$, enteron$, enterby$,      ~
                              pjflag$, " "

L31480:     FMT CH(25),                  /* Unused Area                */~
                CH(25),                  /* ASSEBLY PART NUMBER        */~
                CH(03),                  /* BOM ID. NUMBER             */~
                CH(03),                  /* SEQUENCE NUMBER            */~
                CH(30),                  /* DESCRIPTION                */~
                CH(3),                   /* WHICH ROUTE                */~
                CH(4),                   /* TEXTID Header              */~
                CH(1),                   /* DRAWING LOCATION           */~
                CH(2),                   /* DRAWING REVISION LEVEL     */~
                2*BI(3),                 /* CREATE FOLLOWED BY MOD DATE*/~
                CH(3),                   /* LAST USERS ID              */~
                CH(1),                   /* Phantom Assembly Phlag     */~
                PD(14,4),                /* BATCH SIZE                 */~
                CH(6),                   /* Approved ON                */~
                CH(15),                  /* Approved BY                */~
                CH(6),                   /* Approval Entered ON        */~
                CH(3),                   /* Approval Entered ON        */~
                CH(1),                   /* Purchased Job Flag         */~
                CH(5)                    /* Filler Rest Record         */

            for temp% = 1 to maxlines%
                convert str(part$(temp%),26,3) to k%
                quantity, xused, fixed, over = 0
                convert quantity$(k%) to quantity, data goto L31710
L31710:         convert size$(k%) to xused, data goto L31720
L31720:         if oldbatch = 1 then L31810
                   quantity = round(quantity * xused, 4):xused = 10
                   for i% = 0% to 4%
                       xused = 0.1 * xused
                       temp = round((quantity/xused)/oldbatch,4)
                             if round(temp*oldbatch*xused, 4) = quantity ~
                                          then L31800
                   next i%
L31800:            quantity = temp
L31810:         convert fixed$(k%) to fixed, data goto L31820
L31820:         convert over$(k%) to over, data goto L31830
L31830:         convert temp% to seqnr$, pic(###)
                write #5 using L32180,                                    ~
                        str(part$(temp%),,25), assypart$, bomid$, seqnr$,~
                        quantity, xused, fixed, over, bommkr$(k%),       ~
                        op$(k%), cbom$(k%), textid$(k%), step$(k%),      ~
                        type$(k%), autoflag$, " "

                if keyno% <> 32% then L32000
                if locmax%=0 then L32000
                for i%=1 to locmax%
                if locind%(i%)<>k% then L31990
                temp=0:convert locqty$(i%) to temp, data goto L31950
L31950:         locedt%(k%)=locedt%(k%)+1%
                write #6, using L32340, str(part$(temp%),,25),            ~
                                       assypart$, bomid$, seqnr$,        ~
                                       locedt%(k%), loc$(i%), temp, " "
L31990:         next i%
L32000:         next temp%

                call "TXTFUTIL" (#12, f2%(12), "SAV2", " ")

                if keyno% = 32% then L32430
                if locmax%=0 then L32430
                for i%=1 to locmax%
                if locind%(i%)<1% or locind%(i%)>maxlines% then L32150
                temp=0:convert locqty$(i%) to temp, data goto L32100
                locedt%(locind%(i%))=locedt%(locind%(i%))+1%
L32100:         convert locind%(i%) to seqnr$, pic(###)
                write #6, using L32340,str(part$(locind%(i%)),,25),       ~
                                      assypart$,                         ~
                                      bomid$,seqnr$,locedt%(locind%(i%)),~
                                      loc$(i%), temp, " "
L32150:         next i%
                goto L32430

L32180:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* WHICH BOM STRUCTURE?       */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                PD(14,4),                /* FIXED QUANTITY             */~
                PD(14,4),                /* ADDED OVERAGE              */~
                CH(2),                   /* BOM MARKER                 */~
                CH(01),                  /* OPTIONAL COMPONENT FLAG    */~
                CH(3),                   /* COMPONENTS SPEC BOM        */~
                CH(04),                  /* TEXTID Line Item           */~
                CH(04),                  /* 'PICK BEFORE' RTE STEP ID. */~
                CH(03),                  /* Part Type when approved    */~
                CH(01),                  /* Auto Replace Flag          */~
                CH(44)                   /* Filler Rest Record         */

L32340:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* WHICH BOM STRUCTURE?       */~
                CH(3),                   /* ASSY BOM SEQ               */~
                PIC(###),                /* CH(3), SEQUENCE NUMBER     */~
                CH(6),                   /* LOCATION                   */~
                PD(14,4),                /* QUANTITY AT THIS LOCATION  */~
                CH(27)                   /* FILLER                     */

L32430:     REM Delete Any Orphaned BOM Specific Options lists...
            readkey$ = str(assypart$,,25) & str(bomid$)
L32450:     call "PLOWNEXT" (#7, readkey$, 28%, f1%(7))
                if f1%(7) = 0 then L32570
            REM Does Component Still Exist On Bill?...
            lockey$ = str(readkey$,29,25)&str(readkey$,,25)&str(bomid$)
L32490:     call "PLOWALTS" (#5, lockey$, 1%, 53%, f1%(5))
                if str(lockey$,54,3) = "  0" then L32490
                if f1%(5) <> 0 then L32540  /* Yes, So Next Comp */
            str(readkey$,54,1) = hex(00)
            call "DELETE" (#7, readkey$, 53%)
            goto L32450
L32540:     str(readkey$,54,1) = hex(ff)
            goto L32450

L32570:     REM Now Provide Option To Make Bill Effective, If Appropriate
            /* Get approval flag */
            call "READ100" (#1, "SWITCHS.BOM", f1%(1))
            if f1%(1) = 0% then apprflag$ = "N"                          ~
            else get #1 using L32620, apprflag$
L32620:          FMT XX(20), CH(1)

            if apprflag$ = "Y" then return
            if maxlines% = 0% then return
            readkey$ = str(assypart$,,25) & "1001"
            call "READ100" (#10, readkey$, f1%(10))
               if f1%(10) <> 0 then return
            pfktext$(1) = hex(8c) & "You May Set This BOM Effective By Pr~
        ~essing PF17." & hex(84)
            pfktext$(2) = hex(8c) & "Press Any Other Key To Ignore This O~
        ~ption."
            if rteid$ <> " " then L32750
            pfktext$(2) = pfktext$(2) & hex(84)&" Assembly Has No Route."
L32750:     pfktext$(2) = pfktext$(2) & hex(84)
            pfktext$(3) = "Once a BOM Is Made Effective, Changes To It Ar~
        ~e Restricted."

            keyno% = 2%
            call "ASKUSER" (keyno%, "BILL OF MATERIALS EFFECTIVITY",     ~
                    pfktext$(1), pfktext$(2), pfktext$(3))

            if keyno% <> 17% then return
            for i% = 1% to 490%
                effective$(i%) = bomid$
            next i%
            write #10, using L32890, assypart$, "1001", effective$(), " ",~
                                    " ", " ", eod goto L32900
L32890:     FMT CH(25), CH(4), 490*CH(3), 2*CH(250), CH(16)
L32900:     return

        REM *************************************************************~
            *                   C H E C K   U S A G E                   *~
            *                                                           *~
            *               IS IT OK TO CHANGE THIS BILL?               *~
            *************************************************************

        check_usage:
            errormsg$ = " "

*        Check if approval flag is set to yes
            call "READ100" (#1, "SWITCHS.BOM", f1%(1))
            if f1%(1) = 0% then apprflag$ = "N"                          ~
            else get #1 using L33050, apprflag$
L33050:          FMT XX(20), CH(1)
            if apprflag$ <> "Y" then L33190

*        If approval flag is set check if BOM is approved.
            readkey$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
            call "READ100" (#5, readkey$, used%)
            if used% = 0% then return
                 get #5 using L33130, approved$
L33130:             FMT    POS(115), CH(6)
            if approved$ = " " then L33190
               errormsg$ = "BOM is APPROVED."
               return

*        Now See if the Bill is used in a Job (via JBCROSS2).
L33190:     readkey$ = str(assypart$,,25) & str(bomid$,,3)
            call "PLOWALTS" (#9, readkey$, 2%, 28%, used%)
            if used% = 0% then L33240
                errormsg$ = "It Was Used For Planning"  : return

L33240
*        Next See if Bill has been set effective (via ENGMASTR)
            readkey$ = str(assypart$,,25) & "1001"
            call "READ100" (#10, str(readkey$,,29), used%)
            if used% = 0% then L33350
                errormsg$ = "It Has An Effectivity Date"
                get #10, using L33300, effective$()
L33300:              FMT XX(29), 490*CH(3)
                search effective$() = str(bomid$,,3) to srch%() step 3
                if srch%(1) > 0% then return
                     errormsg$ = " " : used% = 0%

L33350
*        And last see if Bill referenced by Standard Costing (STCBOMXF)
            readkey$ = str(assypart$,,25) & str(bomid$,,3) & hex(00)
            call "PLOWALTS" (#3, readkey$, 1%, 28%, used%)
            if used% = 0% then return
                errormsg$ = "Used by Standard Cost Set " &               ~
                                                       str(readkey$,29,8)
                return

        REM *************************************************************~
            *          P L O W   B O M'S   T H I S    P A R T           *~
            *                                                           *~
            *             DIG UP EXISTING BOMS FOR DISPLAY              *~
            *************************************************************

        more_boms
          if bom$(12) = " " then read_boms
          readkey$ = str(assypart$) & str(bom$(12))
          goto L35140

        read_boms
          readkey$ = assypart$

L35140:   lastbom$, bom$(), bomdescr$() = " "
          for u3% = 1 to 12
            call "PLOWNEXT" (#5, readkey$, 25%, f1%(5))
                if f1%(5) = 0 then L35230
            get #5, using L35190, bom$(u3%), bomdescr$(u3%)
L35190:     FMT XX(50), CH(3), XX(3), CH(30)
            str(readkey$,29) = all(hex(ff))
          next u3%

L35230:   bompaging$, msg$ = " "
          if bom$() <> " " then msg$ = "Current BOMS on file for this par~
        ~t are : (Highlighted ones CAN be Modified)"
          if bom$(12) <> " " then bompaging$ = "(5)See More BOMs"
        return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                init(hex(84)) mfac$
                header$ = " "
                pfktext$(1) = "(1)Start Over     (4)Previous Field    (8)~
        ~View BOM Effectivity (13)Instructions"
                pfktext$(2) = "(3)Copy BOM       xxxxxxxxxxxxxxxx     (14~
        ~)Select BOM          (15)Print Screen"
                pfktext$(3) = "                  (9)Examine BOM/RTE/STC R~
        ~elationships         (16)Exit Program"
                str(pfktext$(2),19,19) = bompaging$
                str(pfktext$(2),63,1) = hex(8c)
                str(pfktext$(3%),,13%) = ecrpfk$
                pfkeys$ = hex(0001040503090d0f100e080bff)
                if ecrpfk$ = " " then str(pfkeys$,12%,1%) = hex(ff)

                if fieldnr% > 1% then L40280
                  str(pfktext$(1),19%,17%)= " " /* Shut Off Prev Field */
                  str(pfktext$(1),40,23) = " "  /* Shut Off Inquiry    */
                  str(pfktext$(2),,39) = " "    /* Shut Off More       */
                  str(pfktext$(3),19,37) = " "  /* Shut Off Inquiry    */
                  str(pfkeys$,3%,1%),str(pfkeys$,5%,2%) = hex(ffff)
                  str(pfkeys$,11%,1%) = hex(ff)
                  init(hex(8c)) fc$()
                  if lastpart$ <> " " then header$ =                     ~
                                        "Last Part Managed: " & lastpart$
                  goto L40780
L40280:        str(pfktext$(3),64) = " "        /* Shut Off Exit       */
               str(pfktext$(2),40,20) = " "     /* Shut Off Search     */
               str(pfkeys$,9,2) = hex(ffff)
               if fieldnr% = 3% then L40330
                  str(pfktext$(2),,15) = " "    /* Shut Off Copy Option*/
                  str(pfkeys$,5,1) = hex(ff)
L40330:        if fieldnr% > 2% then L40780
               REM Display existing BOMs...
               if errormsg$ <> " " then L40780
               init(hex(8c)) fc$()   /* Make Editable (ENG) BOMs Bright */
               temp$ = bomid$
               for u3% = 1 to 12
                 if bom$(u3%) = " " then L40440
                 bomid$ = bom$(u3%)
                 gosub check_usage
                 if used% = 0 then fc$(u3%) = hex(84)
                 if used% = 0 and temp$ = " " then temp$ = bom$(u3%)
L40440:        next u3%
               errormsg$ = " "
               bomid$ = temp$
               if bomid$ = " " and bom$() = " " then bomid$ = "001"
               goto L40780

            deffn'211(fieldnr%)
                REM Editmode logic...
                bom$(), bomdescr$(), header$ = " "
                init(hex(8c)) lfac$()
                init(hex(86)) str(lfac$(),3)
                mfac$ = hex(94)
                pfktext$(1) = "(1)Start Over                          (8)~
        ~View BOM Effectivity (13)Instructions"
                pfktext$(2) = "                      (10)See Route    (12~
        ~)Delete BOM          (15)Print Screen"
                pfktext$(3) = "(2)Manage Components                   (25~
        ~)BOM Text            (16)Save Data"
                str(pfktext$(2),,1),str(pfktext$(2),63,1) = hex(8c)
                str(pfktext$(3%),22%,14%) = ecrpfk$
                pfkeys$ = hex(0001020a0c0d0f1019ff1d080bff)
                if ecrpfk$ = " " then str(pfkeys$,13%,1%) = hex(ff)
                if fieldnr% = 0% then L40700
                     str(pfktext$(2),,62), pfktext$(3) = " "
                     str(pfktext$(1%),40%,23%) = " "
                     str(pfkeys$,3,3), str(pfkeys$,8,2) = hex(ffff)
                     str(pfkeys$,12%,2%) = hex(ffff)
                     init(hex(8c)) lfac$()
                     goto L40780
L40700:         if no_mod% <> 1% then L40780
                     str(pfktext$(2),40,24) = " "
                     str(pfkeys$,5,1) = hex(ff)
                     str(pfktext$(3),64,15) = "(16)Return"
                     if admin% <> 1% then L40780
                        str(pfktext$(2),40,24) = "(24)Override NO MOD"
                        str(pfkeys$,10,1) = hex(18)

L40780:           str(pfktext$(3),63,1) = hex(84)
                  str(header$) = "Current Effective BOM is: " & eff_bom$
                  str(header$,62) = "BOMINPUT: " & cms2v$
                  on fieldnr% gosub L40920,         /* ASSEMBLY PART #  */~
                                    L40920,         /* WHICH BOM?       */~
                                    L40890,         /* description      */~
                                    L40920,         /* BATCH SIZE       */~
                                    L40920,         /* RTE ID           */~
                                    L40920,         /* Drawing Size     */~
                                    L40920,         /* Revision Level   */~
                                    L40920          /* Purch'd Job Item */
                     goto L40960

L40890:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40920:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L40960:     accept                                                       ~
               at (01,02), "Bill Of Materials Structure Management",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Assembly Part Number",                       ~
               at (06,23), fac(lfac$( 1)), assypart$            , ch(25),~
               at (06,49), fac(hex(8c)),   assypartdescr$       , ch(32),~
                                                                         ~
               at (07,02), "BOM Identifier",                             ~
               at (07,23), fac(lfac$( 2)), bomid$               , ch(03),~
                                                                         ~
               at (08,02), "BOM Description",                            ~
               at (08,23), fac(lfac$( 3)), bomdescr$            , ch(30),~
                                                                         ~
               at (09,02), "Batch Size",                                 ~
               at (09,23), fac(lfac$( 4)), batch$               , ch(10),~
                                                                         ~
               at (10,02), "Peg To Route",                               ~
               at (10,23), fac(lfac$( 5)), rteid$               , ch(03),~
                                                                         ~
               at (11,02), "Drawing Size",                               ~
               at (11,23), fac(lfac$( 6)), dsize$               , ch(01),~
                                                                         ~
               at (12,02), "Revision Level",                             ~
               at (12,23), fac(lfac$( 7)), rlevel$              , ch(02),~
                                                                         ~
               at (13,02), "Purchased Job Item?",                        ~
               at (13,23), fac(lfac$(8%)), pjflag$              , ch(01),~
                                                                         ~
               at (12,54), "Created: ",                                  ~
               at (12,64), fac(hex(8c)),   date$(1)             , ch(08),~
               at (13,54), "Modified:           By:",                    ~
               at (13,64), fac(hex(8c)),   date$(2)             , ch(08),~
               at (13,78), fac(hex(8c)),   luser$               , ch(03),~
                                                                         ~
               at (14,02), fac(mfac$),     msg$                 , ch(79),~
                                                                         ~
         at (15,04), fac(fc$(01)), bom$(01), fac(hex(8c)), bomdescr$(01),~
         at (16,04), fac(fc$(02)), bom$(02), fac(hex(8c)), bomdescr$(02),~
         at (17,04), fac(fc$(03)), bom$(03), fac(hex(8c)), bomdescr$(03),~
         at (18,04), fac(fc$(04)), bom$(04), fac(hex(8c)), bomdescr$(04),~
         at (19,04), fac(fc$(05)), bom$(05), fac(hex(8c)), bomdescr$(05),~
         at (20,04), fac(fc$(06)), bom$(06), fac(hex(8c)), bomdescr$(06),~
                                                                         ~
         at (15,42), fac(fc$(07)), bom$(07), fac(hex(8c)), bomdescr$(07),~
         at (16,42), fac(fc$(08)), bom$(08), fac(hex(8c)), bomdescr$(08),~
         at (17,42), fac(fc$(09)), bom$(09), fac(hex(8c)), bomdescr$(09),~
         at (18,42), fac(fc$(10)), bom$(10), fac(hex(8c)), bomdescr$(10),~
         at (19,42), fac(fc$(11)), bom$(11), fac(hex(8c)), bomdescr$(11),~
         at (20,42), fac(fc$(12)), bom$(12), fac(hex(8c)), bomdescr$(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(84)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 8% then L41550
                  call "BOMEFDSP" (assypart$, #10, #5, #8, #4, #11, #3)
                  goto L40960

L41550:        if keyhit% <> 10% then L41590
                  call "RTEDSPLY" (assypart$, rteid$, #8, #4)
                  goto L40960

L41590:        if keyhit% <> 13% then L41630
                  call "MANUAL" ("BOMINPUT")
                  goto L40960

L41630:        if keyhit% <> 15% then L41670
                  call "PRNTSCRN"
                  goto L40960

L41670:        if keyhit% <> 9% then L41720
                  call "BOMSRTES" (assypart$, #4, #5, #8, #3)
                  goto L40960

L41720:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   3      *~
            * --------------------------------------------------------- *~
            * Input/Edit of First Line Item Screen.                     *~
            *************************************************************

            deffn'203(fieldnr%)          /* Input Mode Lines */
                init(hex(8c)) lfac$()
                pfktext$(1) = "(1)Start Over                           "&~
                              " (10)See Route         (13)Instructions"
                pfktext$(2) = "(2)Restart Line     (4)Prev. Field      "&~
                              " (14)See Options       (15)Print Screen"
                pfktext$(3) = "                    (6)Same as Prev Line"&~
                              "                       (16)Edit Mode"
                pfkeys$ = hex(00010204060d0f100a0e)

                if c% <> 1% then L42150
                    str(pfktext$(3%),21%,20%) = " "  /* Flip Off PF(6) */
                    str(pfkeys$,5%,1%) = hex(ff)
L42150:         if rteid$ = " " then str(pfktext$(1),40,15) = " "
                if rteid$ = " " then str(pfkeys$,9,1) = hex(ff)
                if op$(c%) <> "Y" then str(pfktext$(2),40,24) = " "
                if op$(c%) <> "Y" then str(pfkeys$,10,1) = hex(ff)
                if fieldnr% > 3% then L42240
                if fieldnr% > 1% then L42590
                  str(pfktext$(2),,37) = " "  /* Shut Off Prev Field */
                  str(pfkeys$,3,2) = hex(ffff)
                  goto L42590
L42240:         str(pfktext$(3),63) = " "   /* Shut Off Edit Mode  */
                str(pfkeys$,8,1) = hex(ff)
                goto L42590

            deffn'113(fieldnr%)          /* Edit Mode Lines */
                init(hex(86)) lfac$()
                if fieldnr% <> 0% then init(hex(8c)) lfac$()
                pfktext$(1) = "(1)Start Over   (4)Implode    (5)Explode"&~
                              " (10)See Route         (13)Instructions"
                pfktext$(2) = "(2)First Comp.  (6)Prev Comp. (8)Ref Loc"&~
                              " (14)See Options       (15)Print Screen"
                pfktext$(3) = "(3)Last Comp.   (7)Next Comp. (9)Header "&~
                              " (25)Component Text    (16)Line Summary"
                pfkeys$ = hex(000102060307090d0f100a190e0405081d)
                if rteid$ = " " then str(pfktext$(1),42,15) = " "
                if rteid$ = " " then str(pfkeys$,11,1) = hex(ff)
                if op$(c%) <> "Y" then str(pfktext$(2),42,15) = " "
                if op$(c%) <> "Y" then str(pfkeys$,13,1) = hex(ff)

            REM Turn Off Appropriate Fields. Are we editing a field?
                if fieldnr% = 0% then L42510  /* no */
                     str(pfktext$(1),17,25) = " "
                     str(pfktext$(2),,63), pfktext$(3) = " "
                     str(pfkeys$,3,5) = all(hex(ff))
                     str(pfkeys$,10) = hex(0a)
                     init(hex(8c)) lfac$()
                     goto L42590
L42510:     REM Display Mode...
                if c% > 1 then L42550
                  str(pfktext$(2),,29)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,3,2) = hex(ffff)
L42550:         if c% < maxlines% then L42590
                  str(pfktext$(3),,29)   = " "  /* Shut Off Next Stuff */
                  str(pfkeys$,5,2) = hex(ffff)

L42590:     REM Set Up header Portion Of Screen...
                str(pfktext$(3),63,1) = hex(84)
                if form_calc_flag$ = "N" or fieldnr% <> 3% then L42610
                   str(pfktext$(3%),42%,17%) = "(11)Calculate Qty"
                   str(pfkeys$,17%,1%) = hex(0b)
L42610:         header$ = "Parent: " & assypart$ & "  BOM Id: " & bomid$
                str(header$,62) = "BOMINPUT: " & cms2v$
                box$ = "  Sequence No:"
                convert c% to str(box$,16), pic(####)
                call "SPCSMASH" (str(box$,16))
                str(box$,,1), str(box$,21) = hex(06)
                if oldbatch = 1 then qtymsg$ = "per Unit"                ~
                                else qtymsg$ = "per Batch"

                on fieldnr% gosub L42810,         /* Part Number        */~
                                  L42810,         /* Option?            */~
                                  L42810,         /* Quantity           */~
                                  L42810,         /* Size               */~
                                  L42810,         /* Added Overage      */~
                                  L42810,         /* Fixed Qty Per Run  */~
                                  L42810,         /* Marker             */~
                                  L42810,         /* Specific BOM Id.   */~
                                  L42810          /* Pick Before Step   */
                     goto L42850

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42810:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L42850:     accept                                                       ~
               at (01,02), "Manage Bill Of Materials",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (03,60), fac(hex(ac)), box$                   , ch(21),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,15), fac(lfac$( 1)), part$(c%)            , ch(25),~
               at (06,41), fac(hex(8c)),   descr$               , ch(34),~
                                                                         ~
               at (07,02), "Optional Component?",                        ~
               at (07,25), fac(lfac$( 2)), op$(c%)              , ch(01),~
               at (07,41), fac(hex(8c)),   typedescr$           , ch(30),~
                                                                         ~
               at (08,02), "Quantity",                                   ~
               at (08,11), fac(hex(8c)), qtymsg$                , ch(10),~
               at (08,25), fac(lfac$( 3)), quantity$(c%)        , ch(10),~
               at (08,41), fac(hex(8c)),   stkuom$              , ch(25),~
                                                                         ~
               at (09,02), "Size (Times Used)",                          ~
               at (09,25), fac(lfac$( 4)), size$(c%)            , ch(10),~
                                                                         ~
               at (10,02), "Added Overage per Unit",                     ~
               at (10,25), fac(lfac$( 5)), over$(c%)            , ch(10),~
               at (10,41), fac(hex(8c)),   ext$                 , ch(30),~
                                                                         ~
               at (11,02), "Fixed Quantity Per Run",                     ~
               at (11,25), fac(lfac$( 6)), fixed$(c%)           , ch(10),~
                                                                         ~
               at (12,02), "BOM Marker",                                 ~
               at (12,25), fac(lfac$( 7)), bommkr$(c%)          , ch(02),~
               at (12,41), fac(hex(8c)),   mkrdescr$            , ch(30),~
                                                                         ~
               at (13,02), "Specific BOM Id.",                           ~
               at (13,25), fac(lfac$( 8)), cbom$(c%)            , ch(03),~
               at (13,41), fac(hex(8c)),   cbomdescr$           , ch(32),~
                                                                         ~
               at (14,02), "Pick Before RTE Step",                       ~
               at (14,25), fac(lfac$( 9)), step$(c%)            , ch(04),~
               at (14,41), fac(hex(8c)),   stepdescr$           , ch(32),~
                                                                         ~
               at (15,41), fac(hex(8c)),   yield$               , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$               , ch(79),~
               at (22,02), fac(hex(8c)), pfktext$(1)            , ch(79),~
               at (23,02), fac(hex(8c)), pfktext$(2)            , ch(79),~
               at (24,02), fac(hex(8c)), pfktext$(3)            , ch(79),~
                    keys(pfkeys$),                                       ~
                    key(keyhit%)

               if keyhit% <> 4 then L43460
                  if linemode% = 0 then L43460
                  incl(1)  = -54.03 /* Exclude Header BOM Seq Number */
                  incl$(1) = hex(202030)

                  hdr$(3) = hex(ac) & "Shown below is the single level 'w~
        ~here used' for this component part."
                  header1$ = hex(06) & "Part " & part$(c%) &             ~
                                                " is used in these BOM's"
                  readkey$ = part$(c%)
                  call "PLOWCODE" (#5, readkey$, header1$, 8025%, 1.32,  ~
                      f1%(5), hdr$(), 28, 0, incl(), incl$()," ","Y", #4)
                  incl(1)  = 0 : incl$(1) = hex(202020)
                  goto L42850

L43460:        if keyhit% <> 5 then L43510
                  readkey$ = part$(c%)
                  call "PLOWNEXT" (#5, readkey$, 25%, f1%(5))
                  if f1%(5) <> 0 then L43470
                  errormsg$ = "Part Has NO Bill of Materials"
                  goto L42850
L43470:           readkey$ = part$(c%) : temp$ = cbom$(c%)
                  call "BOMXPLOD" (readkey$, temp$, #4, #5)
                  goto L42850

L43510:        if keyhit% <> 10 then L43550
                  call "RTEDSPLY" (assypart$, rteid$, #8, #4)
                  goto L42850

L43550:        if keyhit% <> 13 then L43590
                  call "MANUAL" ("BOMINPUT")
                  goto L42850

L43590:        if keyhit% <> 15 then L43630
                  call "PRNTSCRN"
                  goto L42850

L43630
*             IF LINEMODE% = 0 THEN RETURN
                  close ws
                  call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                  return


        REM *************************************************************~
            *      O P T I O N S   D I S P L A Y   S C R E E N          *~
            * --------------------------------------------------------- *~
            * Shows user the list of possible replacements for the guy. *~
            *************************************************************

        display_options
                header1$ = "Seq.  Part Number                Description"
                header$ = "Component: " & part$(c%) &"  BOM Id: "& bomid$
                str(header$,62) = "BOMINPUT: " & cms2v$
                init (hex(84)) dfac$()
                pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "(2)First Parts                            ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                 (5)Next Parts            ~
        ~                     (16)Return"
                pfkeys$ = hex(00010205070d0f10)
                str(pfktext$(3),63,1) = hex(84)

L44100:         REM Gather Data For Display...
                abase% = 0
                lockey$ = str(assypart$) & str(bomid$)
                str(lockey$,29) = str(part$(c%),,25) & hex(00)
                errormsg$ = hex(84) &                                    ~
                            "Note: These are BOM Specific Replacements"
                call "PLOWNEXT" (#7, lockey$, 53%, f1%(7))
                    if f1%(7) = 0 then str(lockey$,,28), errormsg$ = " "
                str(lockey$,54) = hex(00)
                goto L44175

        display_options_loop
            abase% = abase% + 14
L44175:     alt% = 0
            mkrdescr$(), odescr$() = " "
L44185:     call "PLOWNEXT" (#7, lockey$, 53%, f1%(7))
                if f1%(7)=0 then L44240
            alt% = alt% + 1
            get #7, using L44205, mkrdescr$(alt%)
L44205:     FMT XX(54), CH(25)
            call "DESCRIBE" (#4,mkrdescr$(alt%),odescr$(alt%),1%,f1%(4))
            if mkrdescr$(alt%) <> assypart$ then L44230
                errormsg$ = "This Component Has Redundant Options: " &   ~
                                                          mkrdescr$(alt%)
L44230:     if alt% < 14 then L44185

L44240:         REM Display Data Found...
                if abase% + alt% = 0 then mkrdescr$(1%) =                ~
                                   hex(94) & "No Options Defined"
                if abase% > 0 and alt% = 0 then mkrdescr$(1%) =          ~
                                   hex(94) & "End Of Options List"
                init (hex(8c)) tfac$()
                str(tfac$(), min(20,alt%+1)) = all(hex(9c))

L44280:     accept                                                       ~
               at (01,02), "BOM Component Optional Replacements",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ae)), header1$               , ch(04),~
               at (05,08), fac(hex(ac)), str(header1$,7)        , ch(68),~
                                                                         ~
               at (06,02), fac(tfac$( 1)), seq$   (abase%+ 1%)  , ch(05),~
               at (07,02), fac(tfac$( 2)), seq$   (abase%+ 2%)  , ch(05),~
               at (08,02), fac(tfac$( 3)), seq$   (abase%+ 3%)  , ch(05),~
               at (09,02), fac(tfac$( 4)), seq$   (abase%+ 4%)  , ch(05),~
               at (10,02), fac(tfac$( 5)), seq$   (abase%+ 5%)  , ch(05),~
               at (11,02), fac(tfac$( 6)), seq$   (abase%+ 6%)  , ch(05),~
               at (12,02), fac(tfac$( 7)), seq$   (abase%+ 7%)  , ch(05),~
               at (13,02), fac(tfac$( 8)), seq$   (abase%+ 8%)  , ch(05),~
               at (14,02), fac(tfac$( 9)), seq$   (abase%+ 9%)  , ch(05),~
               at (15,02), fac(tfac$(10)), seq$   (abase%+10%)  , ch(05),~
               at (16,02), fac(tfac$(11)), seq$   (abase%+11%)  , ch(05),~
               at (17,02), fac(tfac$(12)), seq$   (abase%+12%)  , ch(05),~
               at (18,02), fac(tfac$(13)), seq$   (abase%+13%)  , ch(05),~
               at (19,02), fac(tfac$(14)), seq$   (abase%+14%)  , ch(05),~
                                                                         ~
               at (06,08), fac(dfac$( 1)), mkrdescr$( 1%)       , ch(25),~
               at (07,08), fac(dfac$( 2)), mkrdescr$( 2%)       , ch(25),~
               at (08,08), fac(dfac$( 3)), mkrdescr$( 3%)       , ch(25),~
               at (09,08), fac(dfac$( 4)), mkrdescr$( 4%)       , ch(25),~
               at (10,08), fac(dfac$( 5)), mkrdescr$( 5%)       , ch(25),~
               at (11,08), fac(dfac$( 6)), mkrdescr$( 6%)       , ch(25),~
               at (12,08), fac(dfac$( 7)), mkrdescr$( 7%)       , ch(25),~
               at (13,08), fac(dfac$( 8)), mkrdescr$( 8%)       , ch(25),~
               at (14,08), fac(dfac$( 9)), mkrdescr$( 9%)       , ch(25),~
               at (15,08), fac(dfac$(10)), mkrdescr$(10%)       , ch(25),~
               at (16,08), fac(dfac$(11)), mkrdescr$(11%)       , ch(25),~
               at (17,08), fac(dfac$(12)), mkrdescr$(12%)       , ch(25),~
               at (18,08), fac(dfac$(13)), mkrdescr$(13%)       , ch(25),~
               at (19,08), fac(dfac$(14)), mkrdescr$(14%)       , ch(25),~
                                                                         ~
               at (06,35), fac(dfac$( 1)), odescr$( 1%)         , ch(32),~
               at (07,35), fac(dfac$( 2)), odescr$( 2%)         , ch(32),~
               at (08,35), fac(dfac$( 3)), odescr$( 3%)         , ch(32),~
               at (09,35), fac(dfac$( 4)), odescr$( 4%)         , ch(32),~
               at (10,35), fac(dfac$( 5)), odescr$( 5%)         , ch(32),~
               at (11,35), fac(dfac$( 6)), odescr$( 6%)         , ch(32),~
               at (12,35), fac(dfac$( 7)), odescr$( 7%)         , ch(32),~
               at (13,35), fac(dfac$( 8)), odescr$( 8%)         , ch(32),~
               at (14,35), fac(dfac$( 9)), odescr$( 9%)         , ch(32),~
               at (15,35), fac(dfac$(10)), odescr$(10%)         , ch(32),~
               at (16,35), fac(dfac$(11)), odescr$(11%)         , ch(32),~
               at (17,35), fac(dfac$(12)), odescr$(12%)         , ch(32),~
               at (18,35), fac(dfac$(13)), odescr$(13%)         , ch(32),~
               at (19,35), fac(dfac$(14)), odescr$(14%)         , ch(32),~
                                                                         ~
               at (21,02), fac(hex(bc)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key(keypressed%)

               if keypressed% = 2 then L44100

               if keypressed% <> 5 then L44625
                  if alt% < 14 then L44280
                  goto display_options_loop

L44625:        if keypressed% <> 13 then L44645
                  call "MANUAL" ("BOMINPUT")
                  goto L44280

L44645:        if keypressed% <> 15 then L44665
                  call "PRNTSCRN"
                  goto L44280

L44665:        if keypressed% <> 16 then L44280
                  errormsg$ = " "
                  return

        REM *************************************************************~
            *       L I N E   S U M M A R Y   S C R E E N               *~
            * --------------------------------------------------------- *~
            * Line Item Summary Screen. (#5 if you're counting).        *~
            *************************************************************

            deffn'115(fieldnr%)
                init (hex(8e)) tfac$() : init (hex(84)) lfac$()
                init (hex(8c)) dfac$()
                if no_mod% = 1 and errormsg$ = " " then errormsg$ =      ~
                       hex(84) & "BOM can be reviewed but not modified."
*                                      1         2         3         4
*                             123456789012345678901234567890123456789012
                pfktext$(1) = "(1)S/Ovr (8)Ref Loc (9)Hdr (23)Toggle (14)~
        ~Chng Qty (11)Insert  (13)Instructions"
*       456789012345678901234567890123456789
*             5         6         7
                pfktext$(2) = "(2)First Line  (4)Prev    (6)Down One (10)~
        ~Route    (12)Delete  (15)Print Screen"
                pfktext$(3) = "(3)Last Lines  (5)Next    (7)Up One   (25)~
        ~Text     (28)Del All (16)Save Data"
                pfkeys$ = hex(000102040603050708090b0c0d0f10191c0a0e17)
*                             1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
*        Flip Off Appropriate Fields
                if no_mod% <> 1% then L45200 /*Can't Insert or Delete*/
                   str(pfktext$(2),52,10)=" ":str(pfkeys$,12,1) = hex(ff)
                   str(pfktext$(3),52,11)=" ":str(pfkeys$,17,1) = hex(ff)
                   str(pfktext$(1),52,10)=" ":str(pfkeys$,11,1) = hex(ff)
                   str(pfktext$(3),64,15)="(16)Return"
                   str(pfktext$(1),39,12)=" ":str(pfkeys$,19,1) = hex(ff)

L45200:         if rteid$ = " " then str(pfktext$(2),39,9) = " "
                if rteid$ = " " then str(pfkeys$,18,1) = hex(ff)
                if maxlines% > 0 then L45260
                  str(pfktext$(2),52,10) = " " /* Shut Off Delete */
                  str(pfktext$(3),52,11) = " " /* Shut Off Delete All */
                  str(pfkeys$,12,1), str(pfkeys$,17,1) = hex(ff)
L45260:         if line% > 0 then L45290
                  str(pfktext$(2),,37)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,3,3) = hex(ffffff)
L45290:         if line%+14 < maxlines% then L45320
                  str(pfktext$(3),,37)   = " "  /* Shut Off Next Stuff */
                  str(pfkeys$,6,3) = hex(ffffff)
L45320:         mkrdescr$(), netqty$() = " "
                if maxlines% < 1 then L45410
                for c% = line% + 1 to min(line%+14, maxlines%)
                    gosub describe_marker
                    gosub describe_ext
                    i% = c% - line%
                    mkrdescr$(i%) = mkrdescr$
                    call "CONVERT" (netqty*oldbatch, 0.4, netqty$(i%,2))
                    call "CONVERT" (totqty*oldbatch, 0.4, netqty$(i%,1))
                next c%
L45410:         goto L45531

            deffn'125(d%)  /* Delete Lines From Memory */
                init (hex(8c)) lfac$(), tfac$(), dfac$()
                pfktext$(1) = "(1)Cancel Delete Request                  ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "(ENTER) Delete Flashing Line(s)"
                pfkeys$ = hex(00010d0f10)
                if d% = 0% then init(hex(94)) lfac$(), tfac$(), dfac$()  ~
                           else lfac$(d%), tfac$(d%), dfac$(d%) = hex(94)

L45531:         header2$   = "                                           ~
        ~                      BOM Bm    Pick"
                if oldbatch <> 1 then                                    ~
                   str(header2$,,30) = "(Batch Quantity:" & batch$ & ")"
                if t% = 2% then str(header2$,55,14) = "Yield Adjusted"
                header1$   = "Seq. Part Number               Description ~
        ~                 Quantity Mk Op Bfor"
                header$ = "Parent: " & assypart$ & "  BOM Id: " & bomid$
                str(header$,62) = "BOMINPUT: " & cms2v$
                str(tfac$(), min(20, (maxlines%-line%)+1)) = all(hex(9c))
                str(pfktext$(3),63,1) = hex(84)
                gosub step_uom_display

L45610:     accept                                                       ~
               at (01,02), "BOM Component Summary Screen",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), header2$               , ch(79),~
               at (05,02), fac(hex(ac)), header1$               , ch(79),~
                                                                         ~
               at (06,02), fac(tfac$( 1)), seq$   (line%+ 1%)   , ch(04),~
               at (07,02), fac(tfac$( 2)), seq$   (line%+ 2%)   , ch(04),~
               at (08,02), fac(tfac$( 3)), seq$   (line%+ 3%)   , ch(04),~
               at (09,02), fac(tfac$( 4)), seq$   (line%+ 4%)   , ch(04),~
               at (10,02), fac(tfac$( 5)), seq$   (line%+ 5%)   , ch(04),~
               at (11,02), fac(tfac$( 6)), seq$   (line%+ 6%)   , ch(04),~
               at (12,02), fac(tfac$( 7)), seq$   (line%+ 7%)   , ch(04),~
               at (13,02), fac(tfac$( 8)), seq$   (line%+ 8%)   , ch(04),~
               at (14,02), fac(tfac$( 9)), seq$   (line%+ 9%)   , ch(04),~
               at (15,02), fac(tfac$(10)), seq$   (line%+10%)   , ch(04),~
               at (16,02), fac(tfac$(11)), seq$   (line%+11%)   , ch(04),~
               at (17,02), fac(tfac$(12)), seq$   (line%+12%)   , ch(04),~
               at (18,02), fac(tfac$(13)), seq$   (line%+13%)   , ch(04),~
               at (19,02), fac(tfac$(14)), seq$   (line%+14%)   , ch(04),~
                                                                         ~
               at (06,07), fac(lfac$( 1)), part$  (line%+ 1%)   , ch(25),~
               at (07,07), fac(lfac$( 2)), part$  (line%+ 2%)   , ch(25),~
               at (08,07), fac(lfac$( 3)), part$  (line%+ 3%)   , ch(25),~
               at (09,07), fac(lfac$( 4)), part$  (line%+ 4%)   , ch(25),~
               at (10,07), fac(lfac$( 5)), part$  (line%+ 5%)   , ch(25),~
               at (11,07), fac(lfac$( 6)), part$  (line%+ 6%)   , ch(25),~
               at (12,07), fac(lfac$( 7)), part$  (line%+ 7%)   , ch(25),~
               at (13,07), fac(lfac$( 8)), part$  (line%+ 8%)   , ch(25),~
               at (14,07), fac(lfac$( 9)), part$  (line%+ 9%)   , ch(25),~
               at (15,07), fac(lfac$(10)), part$  (line%+10%)   , ch(25),~
               at (16,07), fac(lfac$(11)), part$  (line%+11%)   , ch(25),~
               at (17,07), fac(lfac$(12)), part$  (line%+12%)   , ch(25),~
               at (18,07), fac(lfac$(13)), part$  (line%+13%)   , ch(25),~
               at (19,07), fac(lfac$(14)), part$  (line%+14%)   , ch(25),~
                                                                         ~
               at (06,33), fac(lfac$( 1)), desc$  (line%+ 1%)   , ch(26),~
               at (07,33), fac(lfac$( 2)), desc$  (line%+ 2%)   , ch(26),~
               at (08,33), fac(lfac$( 3)), desc$  (line%+ 3%)   , ch(26),~
               at (09,33), fac(lfac$( 4)), desc$  (line%+ 4%)   , ch(26),~
               at (10,33), fac(lfac$( 5)), desc$  (line%+ 5%)   , ch(26),~
               at (11,33), fac(lfac$( 6)), desc$  (line%+ 6%)   , ch(26),~
               at (12,33), fac(lfac$( 7)), desc$  (line%+ 7%)   , ch(26),~
               at (13,33), fac(lfac$( 8)), desc$  (line%+ 8%)   , ch(26),~
               at (14,33), fac(lfac$( 9)), desc$  (line%+ 9%)   , ch(26),~
               at (15,33), fac(lfac$(10)), desc$  (line%+10%)   , ch(26),~
               at (16,33), fac(lfac$(11)), desc$  (line%+11%)   , ch(26),~
               at (17,33), fac(lfac$(12)), desc$  (line%+12%)   , ch(26),~
               at (18,33), fac(lfac$(13)), desc$  (line%+13%)   , ch(26),~
               at (19,33), fac(lfac$(14)), desc$  (line%+14%)   , ch(26),~
                                                                         ~
               at (06,60), fac(lfac$( 1)), netqty$( 1%,t%)      , ch(10),~
               at (07,60), fac(lfac$( 2)), netqty$( 2%,t%)      , ch(10),~
               at (08,60), fac(lfac$( 3)), netqty$( 3%,t%)      , ch(10),~
               at (09,60), fac(lfac$( 4)), netqty$( 4%,t%)      , ch(10),~
               at (10,60), fac(lfac$( 5)), netqty$( 5%,t%)      , ch(10),~
               at (11,60), fac(lfac$( 6)), netqty$( 6%,t%)      , ch(10),~
               at (12,60), fac(lfac$( 7)), netqty$( 7%,t%)      , ch(10),~
               at (13,60), fac(lfac$( 8)), netqty$( 8%,t%)      , ch(10),~
               at (14,60), fac(lfac$( 9)), netqty$( 9%,t%)      , ch(10),~
               at (15,60), fac(lfac$(10)), netqty$(10%,t%)      , ch(10),~
               at (16,60), fac(lfac$(11)), netqty$(11%,t%)      , ch(10),~
               at (17,60), fac(lfac$(12)), netqty$(12%,t%)      , ch(10),~
               at (18,60), fac(lfac$(13)), netqty$(13%,t%)      , ch(10),~
               at (19,60), fac(lfac$(14)), netqty$(14%,t%)      , ch(10),~
                                                                         ~
               at (06,71), fac(dfac$( 1)), bommkr$(line%+ 1%)   , ch(02),~
               at (07,71), fac(dfac$( 2)), bommkr$(line%+ 2%)   , ch(02),~
               at (08,71), fac(dfac$( 3)), bommkr$(line%+ 3%)   , ch(02),~
               at (09,71), fac(dfac$( 4)), bommkr$(line%+ 4%)   , ch(02),~
               at (10,71), fac(dfac$( 5)), bommkr$(line%+ 5%)   , ch(02),~
               at (11,71), fac(dfac$( 6)), bommkr$(line%+ 6%)   , ch(02),~
               at (12,71), fac(dfac$( 7)), bommkr$(line%+ 7%)   , ch(02),~
               at (13,71), fac(dfac$( 8)), bommkr$(line%+ 8%)   , ch(02),~
               at (14,71), fac(dfac$( 9)), bommkr$(line%+ 9%)   , ch(02),~
               at (15,71), fac(dfac$(10)), bommkr$(line%+10%)   , ch(02),~
               at (16,71), fac(dfac$(11)), bommkr$(line%+11%)   , ch(02),~
               at (17,71), fac(dfac$(12)), bommkr$(line%+12%)   , ch(02),~
               at (18,71), fac(dfac$(13)), bommkr$(line%+13%)   , ch(02),~
               at (19,71), fac(dfac$(14)), bommkr$(line%+14%)   , ch(02),~
                                                                         ~
               at (06,75), fac(lfac$( 1)), op$(line%+ 1%)       , ch(01),~
               at (07,75), fac(lfac$( 2)), op$(line%+ 2%)       , ch(01),~
               at (08,75), fac(lfac$( 3)), op$(line%+ 3%)       , ch(01),~
               at (09,75), fac(lfac$( 4)), op$(line%+ 4%)       , ch(01),~
               at (10,75), fac(lfac$( 5)), op$(line%+ 5%)       , ch(01),~
               at (11,75), fac(lfac$( 6)), op$(line%+ 6%)       , ch(01),~
               at (12,75), fac(lfac$( 7)), op$(line%+ 7%)       , ch(01),~
               at (13,75), fac(lfac$( 8)), op$(line%+ 8%)       , ch(01),~
               at (14,75), fac(lfac$( 9)), op$(line%+ 9%)       , ch(01),~
               at (15,75), fac(lfac$(10)), op$(line%+10%)       , ch(01),~
               at (16,75), fac(lfac$(11)), op$(line%+11%)       , ch(01),~
               at (17,75), fac(lfac$(12)), op$(line%+12%)       , ch(01),~
               at (18,75), fac(lfac$(13)), op$(line%+13%)       , ch(01),~
               at (19,75), fac(lfac$(14)), op$(line%+14%)       , ch(01),~
                                                                         ~
               at (06,77), fac(lfac$( 1)), step_uom$    ( 1%)   , ch(04),~
               at (07,77), fac(lfac$( 2)), step_uom$    ( 2%)   , ch(04),~
               at (08,77), fac(lfac$( 3)), step_uom$    ( 3%)   , ch(04),~
               at (09,77), fac(lfac$( 4)), step_uom$    ( 4%)   , ch(04),~
               at (10,77), fac(lfac$( 5)), step_uom$    ( 5%)   , ch(04),~
               at (11,77), fac(lfac$( 6)), step_uom$    ( 6%)   , ch(04),~
               at (12,77), fac(lfac$( 7)), step_uom$    ( 7%)   , ch(04),~
               at (13,77), fac(lfac$( 8)), step_uom$    ( 8%)   , ch(04),~
               at (14,77), fac(lfac$( 9)), step_uom$    ( 9%)   , ch(04),~
               at (15,77), fac(lfac$(10)), step_uom$    (10%)   , ch(04),~
               at (16,77), fac(lfac$(11)), step_uom$    (11%)   , ch(04),~
               at (17,77), fac(lfac$(12)), step_uom$    (12%)   , ch(04),~
               at (18,77), fac(lfac$(13)), step_uom$    (13%)   , ch(04),~
               at (19,77), fac(lfac$(14)), step_uom$    (14%)   , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key(keyhit%)

               if keyhit% <> 10% then L46730
                  call "RTEDSPLY" (assypart$, rteid$, #8, #4)
                  goto L45610

L46730:        if keyhit% <> 13% then L46770
                  call "MANUAL" ("BOMINPUT")
                  goto L45610

L46770:        if keyhit% <> 15% then L46794
                  call "PRNTSCRN"
                  goto L45610

L46794:        if keyhit% <> 23% then L46810
                  step_uom% = abs(step_uom% - 1%)
                  gosub step_uom_display
                  goto L45610

L46810:        close ws
               call "SCREEN" addr ("C", 3%, "I", i$(), cursor%())
               return

        step_uom_display
            for i% = 1% to 14%
                if step_uom% = 0% then step_uom$(i%) = step$(line% + i%) ~
                                  else step_uom$(i%) = uom$(line% + i%)
            next i%
            if step_uom% = 0% then str(header1$,76%,4%) = "Bfor"         ~
                              else str(header1$,76%,4%) = "UOM "
            if step_uom% = 0% then str(header2$,76%,4%) = "Pick"         ~
                              else str(header2$,76%,4%) = "Stkg"
            return

        REM *************************************************************~
            *     L I N E   I T E M   H A N D L I N G   S C R E E N     *~
            *               -  R E F E R E N C E S  -                   *~
            * THIS SCREEN HANDLES INPUT, EDIT, INSERT AND DELETE MODES  *~
            * FOR THE REFERENCE TABLE.                                  *~
            *************************************************************

            deffn'204(fieldnr%)  /* Insert Mode */
                init (hex(8c)) tfac$(), fcc$() : dfac$(1) = hex(ac)
                pfktext$(1) = "(1)Start Over                           "&~
                              "                       (13)Instructions"
                pfktext$(2) = "                                        "&~
                              "                       (15)Print Screen"
                pfktext$(3) = "                                        "&~
                              "                       (16)Edit Mode"
               if insertrtn%=0% then str(pfktext$(3),64)="(16)Next Comp."
               pfkeys$ = hex(00010d0f10)
               goto L47620

            deffn'214(fieldnr%)  /* Edit Mode */
                tfac$()=all(hex(8e)):fcc$()=all(hex(84)):dfac$(1)=hex(ae)
                if no_mod% = 1 and errormsg$ = " " then errormsg$ =      ~
                       hex(84) & "BOM can be reviewed but not modified."
                pfktext$(1) = "(1)Start Over  (8,10)Prev,Next Comp.      ~
        ~      (11)Insert     (13)Instructions"
                pfktext$(2) = "(2)First Ref.  (4)Prev  (6)Down One  (9)He~
        ~ader  (12)Delete     (15)Print Screen"
                pfktext$(3) = "(3)Last Ref.   (5)Next  (7)Up One         ~
        ~      (28)Delete All (16)Return      "
                pfkeys$ = hex(000102040603050708090b0c0d0f101c0a)

                REM Flip Off Appropriate Fields
                if no_mod% <> 1% then L47320
                  str(pfktext$(1%),49%,10%) = " " /* Shut Off Insert */
                  str(pfkeys$,11%,1%) = hex(ff)
                  goto L47330          /* Shut Off Delete & Delete ALL */
L47320:         if edtmax% > 0 then L47360
L47330:           str(pfktext$(2),49,10) = " " /* Shut Off Delete */
                  str(pfktext$(3),49,14) = " " /* Shut Off Delete All */
                  str(pfkeys$,12,1), str(pfkeys$,16,1) = hex(ff)
L47360:         if l% > 0 then L47390
                  str(pfktext$(2),,36)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,3,3) = hex(ffffff)
L47390:         if l%+36 < edtmax% then L47420
                  str(pfktext$(3),,36)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,6,3) = hex(ffffff)
L47420:         if fieldnr% = 0 then L47620
                  init (hex(8c)) tfac$(), fcc$() : dfac$(1)=hex(ac)
                  str(pfktext$(1),14,48) = " "
                  str(pfktext$(2),,62), pfktext$(3) = " "
                  pfkeys$ = hex(00010d0f)
                  if admin% <> 1% then L47620
                     str(pfktext$(3),36,11) = "(27)SYS Flg"
                     str(pfkeys$, 5,1) = hex(1b)
                  goto L47620

            deffn'234(ss%)  /* Delete Lines From Memory */
                init (hex(8c)) fcc$(), tfac$() : dfac$(1) = hex(ac)
                pfktext$(1) = "(1)Cancel Delete Request                  ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "(ENTER) Delete Flashing Line(s)"
                pfkeys$ = hex(00010d0f10)
                if ss% = 0% then init(hex(94)) fcc$(), tfac$()           ~
                                 else fcc$(ss%), tfac$(ss%) = hex(94)
                message$ = " "
                goto L47700

L47620:         REM Things come together again here...
                header1$   = "Seq.  Part Number                Quantity  ~
        ~   Marker           Op?  Pick Before"
                header$ = "Parent: " & assypart$ & "  BOM Id: " & bomid$
                str(header$,62) = "BOMINPUT: " & cms2v$
                hdr$(1) = "Ref#" : hdr$(2) = "Location"
                hdr$(3) = "Quantity"
                  on fieldnr% gosub L47730          /* LOCATION & QTY   */
L47700:           str(tfac$(), min(37, (edtmax%-l%)+1)) = all(hex(9c))
                  goto L47770

L47730:           REM SET FAC'S FOR UPPER CASE ONLY INPUT.
                      fcc$(ss%) = hex(81)
                      return

L47770:     accept                                                       ~
               at (01,02),"BOM Component Location Reference Designators",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (03,02), fac(hex(ac)), header1$               , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), seq$     (c%)          , ch(05),~
               at (04,08), fac(hex(8c)), part$    (c%)          , ch(25),~
               at (04,35), fac(hex(8c)), quantity$(c%)          , ch(10),~
               at (04,47), fac(hex(8c)), mkrdescr$              , ch(18),~
               at (04,66), fac(hex(8c)), op$(c%)                , ch(01),~
               at (04,74), fac(hex(8c)), step$(c%)              , ch(04),~
                                                                         ~
               at (06,02), fac(hex(94)), errormsg$              , ch(79),~
               at (07,02), fac(dfac$(1)), hdr$(1)               , ch(04),~
               at (07,08), fac(hex(ac)), hdr$(2)                , ch(08),~
               at (07,17), fac(hex(ac)), hdr$(3)                , ch(08),~
               at (07,30), fac(hex(ac)), hdr$(1)                , ch(04),~
               at (07,36), fac(hex(ac)), hdr$(2)                , ch(08),~
               at (07,45), fac(hex(ac)), hdr$(3)                , ch(08),~
               at (07,58), fac(hex(ac)), hdr$(1)                , ch(04),~
               at (07,64), fac(hex(ac)), hdr$(2)                , ch(08),~
               at (07,73), fac(hex(ac)), hdr$(3)                , ch(08),~
                                                                         ~
               at (08,02), fac(tfac$( 1%)), seq$  (l%+ 1%)      , ch(05),~
               at (09,02), fac(tfac$( 2%)), seq$  (l%+ 2%)      , ch(05),~
               at (10,02), fac(tfac$( 3%)), seq$  (l%+ 3%)      , ch(05),~
               at (11,02), fac(tfac$( 4%)), seq$  (l%+ 4%)      , ch(05),~
               at (12,02), fac(tfac$( 5%)), seq$  (l%+ 5%)      , ch(05),~
               at (13,02), fac(tfac$( 6%)), seq$  (l%+ 6%)      , ch(05),~
               at (14,02), fac(tfac$( 7%)), seq$  (l%+ 7%)      , ch(05),~
               at (15,02), fac(tfac$( 8%)), seq$  (l%+ 8%)      , ch(05),~
               at (16,02), fac(tfac$( 9%)), seq$  (l%+ 9%)      , ch(05),~
               at (17,02), fac(tfac$(10%)), seq$  (l%+10%)      , ch(05),~
               at (18,02), fac(tfac$(11%)), seq$  (l%+11%)      , ch(05),~
               at (19,02), fac(tfac$(12%)), seq$  (l%+12%)      , ch(05),~
                                                                         ~
               at (08,08), fac(fcc$( 1)),   loc$(locedt%(l%+ 1)), ch(06),~
               at (09,08), fac(fcc$( 2)),   loc$(locedt%(l%+ 2)), ch(06),~
               at (10,08), fac(fcc$( 3)),   loc$(locedt%(l%+ 3)), ch(06),~
               at (11,08), fac(fcc$( 4)),   loc$(locedt%(l%+ 4)), ch(06),~
               at (12,08), fac(fcc$( 5)),   loc$(locedt%(l%+ 5)), ch(06),~
               at (13,08), fac(fcc$( 6)),   loc$(locedt%(l%+ 6)), ch(06),~
               at (14,08), fac(fcc$( 7)),   loc$(locedt%(l%+ 7)), ch(06),~
               at (15,08), fac(fcc$( 8)),   loc$(locedt%(l%+ 8)), ch(06),~
               at (16,08), fac(fcc$( 9)),   loc$(locedt%(l%+ 9)), ch(06),~
               at (17,08), fac(fcc$(10)),   loc$(locedt%(l%+10)), ch(06),~
               at (18,08), fac(fcc$(11)),   loc$(locedt%(l%+11)), ch(06),~
               at (19,08), fac(fcc$(12)),   loc$(locedt%(l%+12)), ch(06),~
                                                                         ~
               at (08,17), fac(fcc$( 1)),locqty$(locedt%(l%+ 1)), ch(08),~
               at (09,17), fac(fcc$( 2)),locqty$(locedt%(l%+ 2)), ch(08),~
               at (10,17), fac(fcc$( 3)),locqty$(locedt%(l%+ 3)), ch(08),~
               at (11,17), fac(fcc$( 4)),locqty$(locedt%(l%+ 4)), ch(08),~
               at (12,17), fac(fcc$( 5)),locqty$(locedt%(l%+ 5)), ch(08),~
               at (13,17), fac(fcc$( 6)),locqty$(locedt%(l%+ 6)), ch(08),~
               at (14,17), fac(fcc$( 7)),locqty$(locedt%(l%+ 7)), ch(08),~
               at (15,17), fac(fcc$( 8)),locqty$(locedt%(l%+ 8)), ch(08),~
               at (16,17), fac(fcc$( 9)),locqty$(locedt%(l%+ 9)), ch(08),~
               at (17,17), fac(fcc$(10)),locqty$(locedt%(l%+10)), ch(08),~
               at (18,17), fac(fcc$(11)),locqty$(locedt%(l%+11)), ch(08),~
               at (19,17), fac(fcc$(12)),locqty$(locedt%(l%+12)), ch(08),~
                                                                         ~
               at (08,30), fac(tfac$(13%)), seq$  (l%+13%)      , ch(05),~
               at (09,30), fac(tfac$(14%)), seq$  (l%+14%)      , ch(05),~
               at (10,30), fac(tfac$(15%)), seq$  (l%+15%)      , ch(05),~
               at (11,30), fac(tfac$(16%)), seq$  (l%+16%)      , ch(05),~
               at (12,30), fac(tfac$(17%)), seq$  (l%+17%)      , ch(05),~
               at (13,30), fac(tfac$(18%)), seq$  (l%+18%)      , ch(05),~
               at (14,30), fac(tfac$(19%)), seq$  (l%+19%)      , ch(05),~
               at (15,30), fac(tfac$(20%)), seq$  (l%+20%)      , ch(05),~
               at (16,30), fac(tfac$(21%)), seq$  (l%+21%)      , ch(05),~
               at (17,30), fac(tfac$(22%)), seq$  (l%+22%)      , ch(05),~
               at (18,30), fac(tfac$(23%)), seq$  (l%+23%)      , ch(05),~
               at (19,30), fac(tfac$(24%)), seq$  (l%+24%)      , ch(05),~
                                                                         ~
               at (08,36), fac(fcc$(13)),   loc$(locedt%(l%+13)), ch(06),~
               at (09,36), fac(fcc$(14)),   loc$(locedt%(l%+14)), ch(06),~
               at (10,36), fac(fcc$(15)),   loc$(locedt%(l%+15)), ch(06),~
               at (11,36), fac(fcc$(16)),   loc$(locedt%(l%+16)), ch(06),~
               at (12,36), fac(fcc$(17)),   loc$(locedt%(l%+17)), ch(06),~
               at (13,36), fac(fcc$(18)),   loc$(locedt%(l%+18)), ch(06),~
               at (14,36), fac(fcc$(19)),   loc$(locedt%(l%+19)), ch(06),~
               at (15,36), fac(fcc$(20)),   loc$(locedt%(l%+20)), ch(06),~
               at (16,36), fac(fcc$(21)),   loc$(locedt%(l%+21)), ch(06),~
               at (17,36), fac(fcc$(22)),   loc$(locedt%(l%+22)), ch(06),~
               at (18,36), fac(fcc$(23)),   loc$(locedt%(l%+23)), ch(06),~
               at (19,36), fac(fcc$(24)),   loc$(locedt%(l%+24)), ch(06),~
                                                                         ~
               at (08,45), fac(fcc$(13)),locqty$(locedt%(l%+13)), ch(08),~
               at (09,45), fac(fcc$(14)),locqty$(locedt%(l%+14)), ch(08),~
               at (10,45), fac(fcc$(15)),locqty$(locedt%(l%+15)), ch(08),~
               at (11,45), fac(fcc$(16)),locqty$(locedt%(l%+16)), ch(08),~
               at (12,45), fac(fcc$(17)),locqty$(locedt%(l%+17)), ch(08),~
               at (13,45), fac(fcc$(18)),locqty$(locedt%(l%+18)), ch(08),~
               at (14,45), fac(fcc$(19)),locqty$(locedt%(l%+19)), ch(08),~
               at (15,45), fac(fcc$(20)),locqty$(locedt%(l%+20)), ch(08),~
               at (16,45), fac(fcc$(21)),locqty$(locedt%(l%+21)), ch(08),~
               at (17,45), fac(fcc$(22)),locqty$(locedt%(l%+22)), ch(08),~
               at (18,45), fac(fcc$(23)),locqty$(locedt%(l%+23)), ch(08),~
               at (19,45), fac(fcc$(24)),locqty$(locedt%(l%+24)), ch(08),~
                                                                         ~
               at (08,58), fac(tfac$(25%)), seq$  (l%+25%)      , ch(05),~
               at (09,58), fac(tfac$(26%)), seq$  (l%+26%)      , ch(05),~
               at (10,58), fac(tfac$(27%)), seq$  (l%+27%)      , ch(05),~
               at (11,58), fac(tfac$(28%)), seq$  (l%+28%)      , ch(05),~
               at (12,58), fac(tfac$(29%)), seq$  (l%+29%)      , ch(05),~
               at (13,58), fac(tfac$(30%)), seq$  (l%+30%)      , ch(05),~
               at (14,58), fac(tfac$(31%)), seq$  (l%+31%)      , ch(05),~
               at (15,58), fac(tfac$(32%)), seq$  (l%+32%)      , ch(05),~
               at (16,58), fac(tfac$(33%)), seq$  (l%+33%)      , ch(05),~
               at (17,58), fac(tfac$(34%)), seq$  (l%+34%)      , ch(05),~
               at (18,58), fac(tfac$(35%)), seq$  (l%+35%)      , ch(05),~
               at (19,58), fac(tfac$(36%)), seq$  (l%+36%)      , ch(05),~
                                                                         ~
               at (08,64), fac(fcc$(25)),   loc$(locedt%(l%+25)), ch(06),~
               at (09,64), fac(fcc$(26)),   loc$(locedt%(l%+26)), ch(06),~
               at (10,64), fac(fcc$(27)),   loc$(locedt%(l%+27)), ch(06),~
               at (11,64), fac(fcc$(28)),   loc$(locedt%(l%+28)), ch(06),~
               at (12,64), fac(fcc$(29)),   loc$(locedt%(l%+29)), ch(06),~
               at (13,64), fac(fcc$(30)),   loc$(locedt%(l%+30)), ch(06),~
               at (14,64), fac(fcc$(31)),   loc$(locedt%(l%+31)), ch(06),~
               at (15,64), fac(fcc$(32)),   loc$(locedt%(l%+32)), ch(06),~
               at (16,64), fac(fcc$(33)),   loc$(locedt%(l%+33)), ch(06),~
               at (17,64), fac(fcc$(34)),   loc$(locedt%(l%+34)), ch(06),~
               at (18,64), fac(fcc$(35)),   loc$(locedt%(l%+35)), ch(06),~
               at (19,64), fac(fcc$(36)),   loc$(locedt%(l%+36)), ch(06),~
                                                                         ~
               at (08,73), fac(fcc$(25)),locqty$(locedt%(l%+25)), ch(08),~
               at (09,73), fac(fcc$(26)),locqty$(locedt%(l%+26)), ch(08),~
               at (10,73), fac(fcc$(27)),locqty$(locedt%(l%+27)), ch(08),~
               at (11,73), fac(fcc$(28)),locqty$(locedt%(l%+28)), ch(08),~
               at (12,73), fac(fcc$(29)),locqty$(locedt%(l%+29)), ch(08),~
               at (13,73), fac(fcc$(30)),locqty$(locedt%(l%+30)), ch(08),~
               at (14,73), fac(fcc$(31)),locqty$(locedt%(l%+31)), ch(08),~
               at (15,73), fac(fcc$(32)),locqty$(locedt%(l%+32)), ch(08),~
               at (16,73), fac(fcc$(33)),locqty$(locedt%(l%+33)), ch(08),~
               at (17,73), fac(fcc$(34)),locqty$(locedt%(l%+34)), ch(08),~
               at (18,73), fac(fcc$(35)),locqty$(locedt%(l%+35)), ch(08),~
               at (19,73), fac(fcc$(36)),locqty$(locedt%(l%+36)), ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys (pfkeys$),                                           ~
               key  (keyhit%)
L49260:
               if keyhit% <> 13 then L49310
                  call "MANUAL" ("BOMINPUT")
                  goto L47770

L49310:        if keyhit% <> 15 then L49350
                  call "PRNTSCRN"
                  goto L47770

L49350:        if fieldnr% <> 0 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            * SPECIAL HIDDEN CODE TO OVERRIDE NO MOD FLAG               *~
            *************************************************************

        override:
            keyhit% = 2% /* Window at bottom */
            call "ASKUSER" (keyhit%, "*** PROCEED WITH CAUTION ***",     ~
                "This feature will permit you to MODIFY A BOM that is " &~
                "flagged to hold as is.", "Press (RETURN) to abort this"&~
                " action.    -- OR --", "Press PF(24) to proceed to mod"&~
                "ify the bill.")

                if keyhit% <> 15 then L49610
                   call "PRNTSCRN"
                   goto L49260

L49610:         if keyhit% <> 24% then goto L49660
                    msg$ = " "
                init(hex(00)) readkey$
                str(readkey$,1,25) = str(assypart$,,)
                str(readkey$,26,03) = str(bomid$,,)

                call "PLOWNEXT"(#14, readkey$, 28%, f1%(14))
                  if f1%(14) = 0% then L49650
                keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** WARNING ** WARNING ***",       ~
                "There are option configurations on file based on thi"  &~
                "s bill of materials.",   "Press (RETURN) to abort this"&~
                " action.    -- OR --", "Press PF(25) to continue and b"&~
                "low your foot off.")
                if keyhit% <> 25% then goto L49660
L49650:             no_mod% = 0%
                    goto L49680

L49660:         if keyhit% <>  0% then goto L49260
L49680:         keyhit% = 99
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* ASSEMBLY PART #  */~
                                    L50260,         /* WHICH ALT BOM?   */~
                                    L50140,         /* BOM DESCRIPTION  */~
                                    L50450,         /* BATCH SIZE       */~
                                    L50600,         /* WHICH WC ROUTE   */~
                                    L50140,         /* DRAWING SIZE     */~
                                    L50140,         /* REVISION LEVEL   */~
                                    L50790          /* Purch's Job Item */
L50140:              return
L50150:     REM TEST DATA FOR ASSEMBLY PART NUMBER
                call "GETCODE"(#4, assypart$, assypartdescr$,0%,0,f1%(4))
                     if f1%(4) <> 0 then L50210
                     errormsg$ = "Assembly Part Number Not On File: "    ~
                                  & assypart$
                     return
L50210:         get #4, using L50220, assytype$
L50220:         FMT XX(179), CH(3)
                temp$ = date
                call "BOMFIND" (assypart$, str(temp$,,6%), #10, #01,     ~
                                                                eff_bom$)
                gosub set_ecr_prompt
                return

L50260:     REM TEST DATA FOR BOM STRUCTURE ID
                bompaging$ = " "
                if bomid$ <> " " then L50310
                   errormsg$ = "Alternate BOM Id Cannot Be Blank!!"
                   return
L50310:         REM NOW TRY FOR IT IN BOM FILE.
                no_mod% = 0
                gosub check_usage
                if used% <> 0 then no_mod% = 1
                msg$ = " "
                if used% = 1 then L50380
                     gosub check_cad_dwgs
                     if cadmsg$ <> " " then msg$ = cadmsg$
                     goto L50385
L50380:         msg$= "BOM Can Be Reviewed But NOT Modified, Since "     ~
                                                              & errormsg$
L50385:         errormsg$ = " "
                gosub L30000          /* LOAD BOM RECORD.           */
                bom$(), bomdescr$() = " "
                if maxlines% = 0 then return
                return clear all
                goto L12000        /* EDIT HEADER MODE.          */

L50450:     REM TEST DATA FOR BATCH SIZE
                call "NUMTEST" (batch$, 1, 9e7, errormsg$, 0.4, newbatch)
                   if errormsg$ <> " " then return
                if oldbatch = newbatch then return
                if maxlines% = 0% then L50570
                   for i% = 1% to maxlines%
                       xused, quantity = 0
                       convert quantity$(i%) to quantity, data goto L50530
L50530:                convert size$    (i%) to xused   , data goto L50540
L50540:                if xused <= 0 then xused = 1
                       quantity = (quantity*xused*newbatch)/oldbatch
                       quantity = round(quantity, 4)
                       xused = 1
                       call "CONVERT" (quantity, -0.4, quantity$(i%))
                       call "CONVERT" (xused, -0.4, size$(i%))
                   next i%
L50570:            oldbatch = newbatch
                   return

L50600: REM TEST DATA FOR ROUTE ID
            init(" ") readkey$, lockey$, hdr$()
            if rteid$ = " " then L50725
                if str(rteid$,1,1) = "?" then rteid$ = " "
                readkey$ = str(assypart$,,25) & str(rteid$,,3)
                lockey$ = hex(06) & "Select the Route to use for Part "  ~
                                  & assypart$
                call "PLOWCODE" (#8, readkey$, lockey$, 2025%, 0, f1%(8),~
                                                            hdr$(), 3.00)
                if f1%(8) = 1% then L50715
                     errormsg$ = "Undefined Route, Please Re-Enter"
                     goto L50725
L50715:         rteid$ = str(readkey$,26,3)
                gosub check_route
L50725:     return

L50790: REM TEST DATA FOR PURCHASED JOB ITEM
            if pjflag$ = "N" then return
            if pjflag$ = "Y" then L50800
                errormsg$ = "Purchased Job Item MUST Be Y, Or N"
                return
L50800:     get #4 using L50802, ptype$
L50802:         FMT POS(180), CH(3)
            if ptype$ <> "000" then return
                errormsg$ = "Part Type 000 (Generic) CANNOT be a Purcha"&~
                            "sed Job Item"
            return

        check_cad_dwgs
            cadmsg$ = " "        /* Initialize */
            /* Check for CMS to CAD Drawing Activity */
            readkey$ = "C" &  str(assypart$) & str(bomid$) &  hex(00)
            call "PLOWNEXT" (#24, readkey$, 29%, f1%(24%))
               if f1%(24%) = 0% then L50940 /* Not Exported to a CAD Dwg */
            get #24 using L50885, cad_rsltflg$, cad_exdate$, cad_indate$
L50885:       FMT POS(109), CH(1), CH(6), CH(6)
            call "DATEFMT" (cad_exdate$)
            call "DATEFMT" (cad_indate$)
            if cad_rsltflg$ = "0" then                                   ~
                cadmsg$ = "This BOM Extracted to Drawing Buffer File on" ~
                        & " " & cad_exdate$   else                       ~
                cadmsg$ = "This BOM Placed in a CAD Drawing on "         ~
                                                            & cad_indate$
                goto L50992
            /* Check for CAD Drawing to CMS Activity */
L50940:     readkey$ = "D" &  str(assypart$) & str(bomid$) &  hex(00)
            call "PLOWNEXT" (#24, readkey$, 29%, f1%(24%))
               if f1%(24%) = 0% then return   /* Its OK, Not in CAD Dwg */
            get #24 using L50885, cad_rsltflg$, cad_exdate$, cad_indate$
            call "DATEFMT" (cad_exdate$)
            call "DATEFMT" (cad_indate$)
            if cad_rsltflg$ = "0" then                                   ~
                cadmsg$ = "This BOM Extracted from a Drawing to CMS Buf" ~
                           & "fer File on " & cad_exdate$        else    ~
                cadmsg$ = "This BOM Imported from a CAD Drawing on "     ~
                                                            & cad_indate$
L50992:     cadmsg$ = "CAUTION: " & cadmsg$
            return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TESTS DATA FOR THE LINE ITEM INFORMATION, INCLUDING       *~
            * PART NUMBER VALID, AND POSITIVE QUANTITY, SIZE (X USED)   *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51200,         /* PART NUMBER      */~
                                    L51400,         /* OPTION FLAG      */~
                                    L51670,         /* QUANTITY         */~
                                    L51760,         /* TIMES USED(SIZE) */~
                                    L51830,         /* ADDED OVERAGE    */~
                                    L51890,         /* FIXED QUANTITY   */~
                                    L51930,         /* BOM MARKER       */~
                                    L52220,         /* SPEC BOM         */~
                                    L52280          /* PICK STEP        */
                     return

L51200:     REM Test Data For Component Part Number
                if editmode% <> 2% then L51210
                     if oldpart$ <> part$(c%) then L51210
                          linemode%, editmode% = 1%
                          return clear all
                          goto editpg3
L51210:         call "GETCODE" (#4, part$(c%), descr$, 1%, 0, f1%(4))
                if part$(c%) <> " " then L51250
                       errormsg$ = "Part Number Can't Be Blank"
                       return
L51250:         if part$(c%) <> assypart$ then L51281
                       errormsg$ = "Component Part# Can't Be The Same As ~
        ~Assembly Part# !!"
                       return
L51281:         call "DESCRIBE" (#4, part$(c%), desc$(c%), 0%, f1%(4))
                gosub describe_part
                if type$ = "NS " then bommkr$(c%) = "RE"
                if type$ > "000" and type$ < "200" then bommkr$(c%) = "SP"
                if type$ > "489" and type$ < "500" then bommkr$(c%) = "TL"
                if type$ > "789" and type$ < "800" then bommkr$(c%) = "TL"
                if type$ <> "000" then return
                if assytype$ = "000" then return
                    errormsg$ = "Assembly Part Must Be Type '000'"
                    return

L51400:     REM Test Data For Option Flag...
                if op$(c%) = " " then op$(c%) = "N"
                if op$(c%) = "N" then return
                if op$(c%) = "Y" then L51452
                   errormsg$ = "Please Enter 'Y' or 'N'"
                   return
L51452:     convert quantity$(c%) to temp, data goto L51460
                if temp >= 0 then L51460
                   op_by_prod_error :
                   errormsg$ = "Options can't be By-products!"
                   return
L51460:     init (hex(00)) lockey$     /* Check For BOM Specific */
            str(lockey$,,53) = str(assypart$) &bomid$ &str(part$(c%),,25)
            call "PLOWNEXT" (#7, lockey$, 53%, f1%(7))
                if f1%(7) <> 0 then L51590
            str(lockey$,,28) = " "     /* Check For Part Specific */
            call "PLOWNEXT" (#7, lockey$, 53%, f1%(7))
                if f1%(7) <> 0 then L51590
                errormsg$ = hex(84) & "Warning:" & hex(94)
                errormsg$ = errormsg$ & "Part Has No Options On File"
                return
             REM Test For Redundant Replacements...
L51570:      call "PLOWNEXT" (#7, lockey$, 53%, f1%(7))
                if f1%(7) = 0 then L51640
L51590:      get #7, using L51600, errormsg$
L51600:      FMT POS(55), CH(25)
             if errormsg$ <> assypart$ then L51570
                errormsg$ = "Replacment List Has Redundant Part In It"
                return
L51640:     errormsg$ = " "
            return

L51670:     REM Test Data For Quantity Field...
                fastload% = 0
                if quantity$(c%) = " " then fastload% = 1
                if quantity$(c%) = " " then quantity$(c%) = "1"
                call "NUMTEST" (quantity$(c%),-9e7,9e7,errormsg$,0.4,q)
                     if errormsg$ <> " " then return
                convert quantity$(c%) to temp
                if temp < 0 and op$(c%) = "Y" then op_by_prod_error
                  if linemode% = 1% then gosub describe_ext
                  if editmode% <> 1% or linemode% = 0% then return
                     if temp >= 0 then L51744
                     if bommkr$(c%) = "ST" or bommkr$(c%) = "RE" or      ~
                        bommkr$(c%) = "AS" or bommkr$(c%) = "BP" then    ~
                                                                   return
                          errormsg$ = "By-Products may have Markers of '"~
                                       & "BP', 'ST', 'RE', or 'AS' ONLY."
L51740:                   fieldnr% = 7%
                          return
L51744:              if bommkr$(c%) <> "BP" then return
                          errormsg$ = "'BP' Marker must have negative "& ~
                                      "quantity."
                          goto L51740

L51760:     REM Test Data For Size (Times Used) Field...
                if size$(c%) = " " then size$(c%) = "1"
                call "NUMTEST" (size$(c%), -9e7, 9e7, errormsg$, 0.4, q)
                     if errormsg$ <> " " then return
                if linemode% = 1 then gosub describe_ext
                return

L51830:     REM Test Data For Added Overage...
                call "NUMTEST" (over$(c%), -9e7, 9e7, errormsg$, 0.4, q)
                     if errormsg$ <> " " then return
                gosub describe_ext
                return

L51890:     REM Test Data For Fixed Quantity Per Run...
                call "NUMTEST" (fixed$(c%), -9e7, 9e7, errormsg$, 0.2, 0)
                return

L51930:     REM TEST DATA FOR BOM MARKER FIELD
                gosub describe_part
                readkey$ = "TABLE01:" & bommkr$(c%)
                call "PLOWCODE" (#1, readkey$, mkrdescr$, 8%, .3, f1%(1))
                     if f1%(1) <> 0 then L52000
                     errormsg$ = "Invalid Marker: " & bommkr$(c%)
                     return
L52000:         bommkr$(c%) = str(readkey$,9)
                call "PUTPAREN" (mkrdescr$)
                convert quantity$(c%) to temp
                if temp >= 0 then L52020
                     if bommkr$(c%) = "ST" or bommkr$(c%) = "RE" or      ~
                        bommkr$(c%) = "AS" or bommkr$(c%) = "BP" then    ~
                                                                    L52028
                          errormsg$ = "By Products may have Markers of '"~
                                       & "BP', 'ST', 'RE', or 'AS' ONLY."
                          return
L52020:         if bommkr$(c%) <> "BP" then L52028
                     errormsg$ = "'BP' Marker must have negative quantity"
                     return
L52028:         if type$ <> "NS " then L52060
                if bommkr$(c%) = "RE" then return
                    errormsg$ = "Non-Stocked Part, Marker Must Be 'RE'"
                    return
L52060:         if type$ > "199" then L52130
                if type$ = "000" then L52130
                if bommkr$(c%) = "SP" then return
                if bommkr$(c%) = "RE" then return
                    errormsg$ = "Non-Planned Part, Marker Must Be 'SP' or~
        ~ 'RE'"
                    return
L52130:         if type$>"489" and type$<"500" then L52180
                if type$>"789" and type$<"800" then L52180
                   if bommkr$(c%) <> "TL" then L52201
                      errormsg$ = "Wrong Part Type For Tool: " & type$
                      return
L52180:         if bommkr$(c%) = "TL" then return
                   errormsg$ = "Part Must Be Marked As Tool, 'TL'"
                   return
L52201:         if bommkr$(c%) <> "PA" or rteid$ <> " " then return
                   errormsg$ = "Bom Has No Attached Route"
                   return

L52220:     REM Test Data For Specific BOM Id.
                if cbom$(c%) = " " then L52248
                     hdr$(1%) = "  BOMID      BOM Description"
                     hdr$(2%), hdr$(3%) = "  "
                     readkey$ = str(part$(c%),,25%) & str(cbom$(c%))
                     cbomdescr$ = hex(06) & part$(c%) & "'s BOMs"
                     call "PLOWCODE" (#5, readkey$, cbomdescr$, 2025%,   ~
                                      0.30, f1%(5%), hdr$(), 3.0)
                         if f1%(5%) = 0% then L52255
                     cbom$(c%) = str(readkey$,26%,3%)
                     return
L52248:         gosub describe_cbom
                if str(cbomdescr$,,1) <> hex(94) then return
L52255:              errormsg$ = "BOM not found, please re-specify"
                     return

L52280:     REM Test Data For Step Number
                stepdescr$ = " "
                if step$(c%) <> " " then L52310
                   if bommkr$(c%) <> "PA" then L52360
                   errormsg$="Blank Step Not Allowed On Phantom Assembly"
                   return
L52310:         readkey$ = str(assypart$,,25) & rteid$
                readkey$ = str(readkey$,,28) & step$(c%)
                call "PLOWCODE" (#13, readkey$, " ", 28%, 0.0, f1%(13))
                     if f1%(13) = 0 then L52400
                step$(c%) = str(readkey$,29,4)
L52360:         gosub describe_rte_step
                if str(stepdescr$,,1) = hex(94) then L52400 /* ?? */
                gosub describe_ext
                return
L52400:            put errormsg$, using L52410, step$(c%)
L52410:            %Illegal Pick Step: ####  (step must exist on route)
                   return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M        *~
            *              R E F E R E N C E   I N F O                  *~
            *                                                           *~
            * TESTS LOCATION & QUANTITY                                 *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53120          /* LOCATION         */
                     return

L53120:     REM TEST LOCATION & QUANTITY
                if loc$(cc%) <> " " then L53160
                   errormsg$ = "Location Can't Be Blank"
                   return
L53160:         call "NUMTEST" (locqty$(cc%), 0, 9e7, errormsg$, 0.2, q1)
                     if errormsg$ <> " " then return
                convert quantity$(c%) to temp
                ref_total = 0
                for i%=1 to edtmax%
                    convert locqty$(locedt%(i%)) to q,data goto L53310
                    temp = round(temp-q, 2)
                    ref_total = ref_total + q
                    REM Look For Dups...
                    if locedt%(i%) = cc% then L53310
                    if loc$(locedt%(i%)) <> loc$(cc%) then L53310
                    errormsg$ = "Location " & loc$(cc%)
                    errormsg$ = errormsg$ & " is already entered (Ref#" &~
                                                                 seq$(i%)
                    i% = edtmax%
L53310:         next i%
                if errormsg$ <> " " then return
                if temp >= 0 then return
                if abs(temp) > q1 then return  /* Avoid Lock out */
                call "CONVERT" (abs(temp), -0.2, temp$)
                errormsg$ = "Total Exceeds BOM Quantity By: " & temp$
                return

        view_ecr_info                    /* View ECR History this Part */
            call "ECRINQSB" ("A",        /* "A" - Show ALL ECRs this   */~
                                         /*    part, open or closed.   */~
                             assypart$,  /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                             #01,        /* SYSFILE2                   */~
                             #04)        /* HNYMASTR                   */
            return

        set_ecr_prompt
*        Set PF Key prompt for ECR Inquiry (if any ECRs)
            ecrpfk$ = "(11)"
            call "ECRINQSB" ("C",        /* "C" to Check for ECR Info  */~
                                         /*    and return PFKey Prompt */~
                             assypart$,  /* Part to Do Inquiry/Check on*/~
                             ecrpfk$,    /* IN:  PFKey # to Use        */~
                                         /* OUT: Formatted PFKey Prompt*/~
                                         /*    Will be BLANK if no ECRs*/~
                             #01,        /* SYSFILE2                   */~
                             #04)        /* HNYMASTR                   */
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
