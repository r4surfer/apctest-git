        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  EEEEE   SSS   TTTTT   CCC    SSS   TTTTT   SSS   BBBB    *~
            *  E      S        T    C   C  S        T    S      B   B   *~
            *  EEEE    SSS     T    C       SSS     T     SSS   BBBB    *~
            *  E          S    T    C   C      S    T        S  B   B   *~
            *  EEEEE   SSS     T     CCC    SSS     T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ESTCSTSB - ESTIMATE BOM COMPONENT COST SUB.  DISPLAYS THE *~
            *            COSTING EXPLOSION FOR BOM COMPONENTS FOR AN    *~
            *            ESTIMATE BOM.  ALLOWS SELECTION OF A PART AND  *~
            *            UNDER CERTAIN CONDITIONS ALLOWS THE            *~
            *            MODIFICATION OF COSTS AND MARKERS              *~
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
            * 05/11/95 ! Original (Largly a Clone of BOMBRWSB)    ! JBK *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "ESTCSTSB" (#1,            /* Phantom Work File            */~
                        #2,            /* ENGMASTR                     */~
                        #3,            /* SYSFILE2                     */~
                        #4,            /* HNYMASTR                     */~
                        #5,            /* BOMMASTR                     */~
                        estimate$,     /* Estimate Number              */~
                        line$,         /* Estimate Line Number         */~
                        parent$,       /* Parent part passed in        */~
                        parentbom$,    /* BOM (use effective if blank) */~
                        mode%,         /* 1 = display,2 =display/select*/~
                        selectpart$,   /* part selected, passed back   */~
                        err$,          /* blank means all OK           */~
                                       /*  else       array overflow   */~
                                       /*             no effective bom */~
                                       /*             no part master   */~
                                       /*             no BOM found     */~
                        changed%)      /* Changed did or did not Ocurr*/

        dim                                                              ~
            bom$(490)3,                  /* Component BOM array        */~
            colheader1$79,               /* Column Header              */~
            colheader2$79,               /* Column Header              */~
            cursor%(2),                  /* Cursor location for edit   */~
            cpart$25,                    /* Detail Component Part #    */~
            cdescr$34,                   /* Component Part Description */~
            cmkrdescr$30,                /* Component BOM Marker Descr */~
            cstkuom$25,                  /* Component UOM Message      */~
            ctypedescr$32,               /* Component Part Type Descr  */~
            cmarker$2,                   /* Detail BOM Marker          */~
            cbom$3,                      /* Detail BOM Reference       */~
            cbomcstsrce$34,              /* Comp. Part BOM Cost Source */~
            cbomdescr$32,                /* Component Part BOM ID Descr*/~
            crstep$4,                    /* Detail Pick Before RTE Step*/~
            coption$1,                   /* Detail Option Part         */~
            cquantity$10,                /* Detail Quantity Used       */~
            csize$10,                    /* Detail Times Used          */~
            cover$10,                    /* Detail Overage             */~
            cfixed$10,                   /* Detail Fixed Quantity      */~
            crequired$10,                /* Required Quantity via Expl */~
            cucost$10,                   /* Detail Unit Cost           */~
            cucost(12),                  /* Detail Unit Cost Array     */~
            ctype$3,                     /* Detail Part Type           */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            edtmessage$79,               /* Edit screen message        */~
            detailmsg$79,                /* Detail Message             */~
            err$79,                      /* Message returned to caller */~
            estimate$8,                  /* Estimate Number            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$3,                      /* Estimate Line Number       */~
            line2$79,                    /* Screen Line #2             */~
            marker$2,                    /* BOM marker                 */~
            optn$1,                      /* BOM component is option flg*/~
            oldcmarker$2,                /* Old Detail Marker          */~
            ovrrdecost$1,                /* Allow Ovrrde of valid cost */~
            parent$25,                   /* Parent Part Number         */~
            parentbom$3,                 /* Bill Version               */~
            parentdescr$32,              /* Parent Part Description    */~
            phcstdesc$23,                /* Component Costing Descr.   */~
            phcstsrc$6,                  /* Component Costing Source   */~
            pldate$6,                    /* Planning Base Date         */~
            pf$(3)79,                    /* PF Screen Literals         */~
            phbom$3,                     /* Component Part's BOM       */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read         */~
            selectpart$25,               /* Part Selected              */~
            seq$3,                       /* Sequence number            */~
            scrn$(15)79,                 /* Screen lines               */~
            temp$79,                     /* Temporary variable         */~
            today$6,                     /* Todays date                */~
            userid$3                     /* Current User Id            */~

        REM If you need to display bigger BOMs, just boost the size of   ~
            the following arrays.  However this thing will get to be a   ~
            segment 2 hog if you go much farther.  No other changes need ~
            be made as there is logic to sense the dimension of the array

            dim                                                          ~
            active$(1000)1,              /* Tracks free slots in PLIST$*/~
            plist$(2,1000)32,            /* List of parts & descrips   */~
            indent%(1000),               /* Indent level for parts     */~
            marker$(1000)2,              /* BOM marker                 */~
            optn$(1000)1,                /* Component is option flag   */~
            ebom%(1000),                 /* Effective BOM for parts    */~
            plus$(1000)1,                /* Indicates levels below     */~
            seq$(1000)3,                 /* Component sequence number  */~
            tptr%(1000),                 /* Temp pointers to new parts */~
            qty$(1000)10,                /* Extended comp. qty/parent  */~
            phcstsrc$(1000)6,            /* Source of Explosion Cost   */~
            phcstdesc$(1000)23,          /* Descr of Explosion Cost    */~
            cost$(1000)10,               /* Unit Cost                  */~
            phbom$(1000)3,               /* Sub Assembly BOM           */~
            type$(1000)3,                /* Options flag  for parts    */~
            idx%(1000)                   /* Position index             */


        dim f1%(64)                                                      ~


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            today$, date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            colheader1$  = "   1  2  3  4  5  6  7  8  9  10           "&~
                           "                      Qty Req'd Mk O"
            colheader2$  = "   Cost Source & Description               "&~
                           "                      Unit Cost BOM "
            err$ = " "
            str(line2$,62%) = "ESTCSTSB: " & str(cms2v$,,8%)

            call "READ100" (#3, "MONTHS OPEN", f1%(3%))
                if f1%(3%) = 0% then L65000
            get #3, using L09170, pldate$
L09170:         FMT XX(32), CH(6)

            ret% = 0%
            call "PIPINDEX" (#3, today$, todayindex%, ret%)
                if ret% =  0% then L09210
                err$ = "ERROR - Unable to find todays date in planning"  ~
                       & " calendar."
                goto exit_program
L09210:     d% = dim(plist$(),2) /* elements in array, used often below.*/

*        Check for Estimate defaults
            readkey$ = "SWITCHS.EST"
            ovrrdecost$ = "Y"
            readkey$ = "SWITCHS.EST"
            call "READ100" (#3, readkey$, f1%(3%))
                if f1%(3%) = 0% then L09330
            get #3 using L09310, ovrrdecost$
L09310:         FMT POS(251), CH(1)

L09330:     changed%, cchanged% = 0%

        REM *************************************************************~
            *       G E T   S E T   U P                                 *~
            *-----------------------------------------------------------*~
            * Get descriptions, and effective bom if bom is blank.      *~
            * Test to see if they slipped us any bad data and if so,    *~
            * set RET% and blow out.                                    *~
            *************************************************************

*        Get the effective BOM if BOM passed in is blank
            if parentbom$ <> " " then L10270

            init(hex(00)) readkey$
            str(readkey$,1%,29%) = str(parent$,1%,25%) & "1" & "001"
            call "READ100" (#2, readkey$, f1%(2%))   /* engmastr */
                 if f1%(2%) <> 1% then L10132
                   get #2, using L10120,  bom$()
L10120:               FMT POS(30), 490 * CH(3)
            parentbom$  = bom$(todayindex%)
            goto L10270
L10132:         err$ = "ERROR - No effective BOM found."
                goto exit_program

L10270:     call "DESCRIBE" (#4, parent$, parentdescr$,0%, f1%(4%))
                 if f1%(4%) =  1% then L10320
                 err$ = "ERROR - No part master file record found."
                 goto exit_program

L10320:     init(hex(00)) readkey$
                 str(readkey$,1%, 8%) = str(estimate$,1%, 8%)
                 str(readkey$,9%, 3%) = str(line$,1%,3%)

            call "PLOWNEXT" (#1, readkey$, 11%, f1%(1%))
            if f1%(1%) <> 0% then L10410
                 err$ = "ERROR - No bill of material record found."
                 goto exit_program

L10410:     goto first_screenload  /* all is OK, lets get on with it */


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21200,         /* Part Number            */~
                              L21230,         /* Optional Componen      */~
                              L21260,         /* Quantity               */~
                              L21290,         /* Size                   */~
                              L21320,         /* Added Overage per      */~
                              L21350,         /* Fixed Quantity pe      */~
                              L21380,         /* Required Quantity      */~
                              L21410,         /* BOM Marker             */~
                              L21440,         /* Specific BOM Id.       */~
                              L21470,         /* Pick Before RTE S      */~
                              L21500          /* Unit Cost              */
            return
L21200: REM Def/Enable Part Number                 CPART$
            enabled% = 0%
            return

L21230: REM Def/Enable Optional Component?         COPTION$
            enabled% = 0%
            return

L21260: REM Def/Enable Quantity                    CQUANTITY$
            enabled% = 0%
            return

L21290: REM Def/Enable Size                        CSIZE$
            enabled% = 0%
            return

L21320: REM Def/Enable Added Overage per Unit      COVER$
            enabled% = 0%
            return

L21350: REM Def/Enable Fixed Quantity per Run      CFIXED$
            enabled% = 0%
            return

L21380: REM Def/Enable Required Quantity           CREQUIRED$
            enabled% = 0%
            return

L21410: REM Def/Enable BOM Marker                  CMARKER$
            oldcmarker$ = cmarker$
            if cmarker$ = "RE" or cmarker$ = "RC" then return
                enabled% = 0%
                return

L21440: REM Def/Enable Specific BOM Id.            CBOM$
            enabled% = 0%
            return

L21470: REM Def/Enable Pick Before RTE Step        CRSTEPS$
            enabled% = 0%
            return

L21500: REM Def/Enable Unit Cost                   CUCOST$
            if modify% = 1% then L21530
                enabled% = 0%
                return
L21530:     if cucost = 0 then L21580
            if str(phcstsrc$,4%,3%) = "MAN" then L21580
            if ovrrdecost$ = "Y" then L21580
                enabled% = 0%  :  goto L21590

L21580:     oldcucost  = cucost
L21590:     return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L27910
                inpmessage$ = edtmessage$
                return

L27910
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Part Number                                            ",~
         "Enter Optional Component?                                    ",~
         "Enter Quantity                                               ",~
         "Enter Size                                                   ",~
         "Enter Added Overage per Unit                                 ",~
         "Enter Fixed Quantity per Run                                 ",~
         "Enter Required Quantity                                      ",~
         "Enter BOM Marker                                             ",~
         "Enter Specific BOM Id.                                       ",~
         "Enter Pick Before RTE Step                                   ",~
         "Enter Unit Cost                                              "

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

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto exit_program


        REM *************************************************************~
            *               S C R E E N   P R O C E S S I N G           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        REM PLIST$() is a big list of parts, each of which is a component~
            (at some level) of the top level parent.  Element 1 is part  ~
            number, element 2 is description.  The logical order         ~
            of sequence, down through the BOMs is kept in IDX%() array.  ~
            This array controls the order the user sees on the screen.   ~
            When a parent part is selected to be exploded, its components~
            are loaded into open space in PLIST$(), anywhere they fit.   ~
            Each element of IDX%() contains the number of the element    ~
            in PLIST$() that belongs at that position in the IDX array.  ~
            The INDENT%() array tracks the cascading layers of the BOM   ~
            indentions.  TPTR%() is a temporary pointer into PLIST$()    ~
            that holds the locations of new parts in the list until      ~
            we are ready to update IDX%() with those locations in the    ~
            insert process. First we make room in the array, then insert.~
            Related arrays are EBOM%() which contains the effective bom  ~
            version for the part (if one is effective), the TYPE$()      ~
            array which tracks part type.                                ~
                               PLUS$() is '+', '-' to indicate whether   ~
            a parent has been exploded or not.                           ~
            All these arrays are parallel to PLIST$() and therefore are  ~
            indexed into by IDX%().  The screen lines                    ~
            are simply the records displayed in order by IDX%.  This     ~
            allows scrolling up and down the list.  The real trick is    ~
            reclaiming space in PLIST$() when they collapse the IDX%()   ~
            or "undo" the explosion.


        REM *************************************************************~
            *        L O A D   S C R E E N   F I R S T   T I M E        *~
            *-----------------------------------------------------------*~
            * Get ready and then load components of top level parent.   *~
            *************************************************************

        first_screenload
            str(line2$,2%,55%)  = "Components of " & parent$ &           ~
                                    ", BOM Version " &  parentbom$

            init(hex(00)) plowkey$, plist$()
            init(" ") type$(), plus$(), optn$(), marker$(), seq$(),      ~
                      qty$(), cost$(), phcstsrc$(), phcstdesc$()
            mat indent% = con
            init(hex(ff)) active$()
            mat tptr%  = zer
            mat idx%   = zer
            mat ebom%  = zer
            str(plowkey$, 1%,  8%) = estimate$
            str(plowkey$, 9%,  3%) = line$
            str(plowkey$,12%,  3%) = bin(1%,3%)

            top% = 1%  /* Tracks which array element is top of screen */
            max% = 0%  /* Maximum elements in big parts array */
            c% = 0%    /* Current position in index array */
            t% = 1%    /* part/descrip toggle set to show part number */
            indent_level% = 1%   /* level of components just loaded */

            goto screen_control

        REM *************************************************************~
            *               L O A D   C O M P O N E N T S               *~
            *-----------------------------------------------------------*~
            * Plow to add new components to the part list.              *~
            *************************************************************

        plow_comps
            new% = 0%
            oldmax% = max%

L30680:     call "PLOWALTS" (#1, plowkey$, 1%, 14%, f1%(1%))
            if f1%(1%) <> 1% then return

            get #1 using L30730, cbomid%, temp$, seq$, marker$, optn$,    ~
                                phbom$, cost, phcstsrc$, phcstdesc$, qty
L30730:         FMT POS(12), BI(3), CH(25), POS(82), CH(3), POS(117),    ~
                    CH(2), CH(1), CH(3), POS(179), PD(14,4), POS(283),   ~
                    CH(6), CH(23), PD(14,4)




            new% = new% + 1%
            max% = max% + 1%
            if max% > d% then abort_message     /* exceeded dimension */

            a% = pos(str(active$()) = hex(ff)) /* find a hole in array */
            plist$(1%, a%) = str(temp$,1,25)   /* put it in the hole   */
            tptr%(new%) = a%                   /* record where it is   */
            active$(a%) = " "                  /* mark hole as filled  */
            temp$ = " "
            ebom%(a%) = cbomid%
            indent%(a%) = indent_level%
            marker$(a%) = marker$
            seq$(a%) = seq$
            optn$(a%) = optn$
            call "CONVERT"(qty, 0.4, qty$(a%))
            call "CONVERT"(cost, 2.4, cost$(a%))
            phbom$(a%) = phbom$
            phcstsrc$(a%) = phcstsrc$
            phcstdesc$(a%) = phcstdesc$

            gosub check_for_bom    /* find effective bom if exists  */
            call "DESCRIBE" (#4, plist$(1%,a%), plist$(2%,a%),0%, f1%(4%))
            goto L30680

        check_for_bom /* does component have effective bom?*/
            init(hex(00)) readkey$
            str(readkey$,1%,39%) = str(estimate$) & str(line$) &         ~
                                   bin(ebom%(a%),3%) &                   ~
                                   str(plist$(1%,a%),1%,25%)
            call "PLOWALTS" (#1, readkey$, 1%, 39%, f1%(1%)) /* Wrk File*/
                 if f1%(1%) <> 1% then return



            plus$(a%) = "+"
            call "READ100" (#4, plist$(1%, a%), f1%(4%)) /* hnymastr*/
                 if f1%(4%) <> 1% then return
                   get #4, using L30970,  type$(a%)
L30970:               FMT POS(180), CH(3)

            return

        REM *************************************************************~
            *                S E T  P L O W  K E Y                      *~
            *-----------------------------------------------------------*~
            *   Set key to plow for components of selected parent part. *~
            *************************************************************

        set_key
            init (hex(00)) plowkey$
            str(plowkey$, 1%, 11%) = str(estimate$) & str(line$)
            str(plowkey$,12%,  3%) = bin(ebom%(idx%(c%)),3%)
            indent_level% = indent%(idx%(c%)) + 1% /* in another level */
            plus$(idx%(c%)) = "-"
            return


        REM *************************************************************~
            *   M A I N   S C R E E N   C O N T R O L  S E C T I O N    *~
            *-----------------------------------------------------------*~
            * This section controls most PF selections on display screen*~
            *************************************************************

        screen_control
             gosub plow_comps
             gosub insert_in_index_array


        newscreen
             gosub make_screen_lines

        redisplay
L31310:      gosub display_the_screen
                 if keyhit% = 1% then  gosub  startover
                 if keyhit% = 16% then exit_program
                 if keyhit% <> 8% then L31330
                        t% = 1% + mod(t%,2%)  /* toggle it */
                        goto newscreen
L31330:          if keyhit% <> 2%  then  L31360
                        top% = 1%
                        goto newscreen
L31360:          if keyhit% <> 3%  then  L31390
                        top% = max(max% - 6%, 1%)
                        goto newscreen
L31390:          if keyhit% <> 4%  then  L31420
                        top% = max(1%, top% - 6%)
                        goto newscreen
L31420:          if keyhit% <> 5%  then  L31450
                        top% = min(max% - 6%, top% + 7%)
                        goto newscreen
L31450:          if keyhit% <> 6%  then  L31480
                        top% = max(1%, top% - 1%)
                        goto newscreen
L31480:          if keyhit% <> 7%  then  L31502
                        top% = min(max%, top% + 1%)
                        goto newscreen

L31502:          if keyhit% <> 10%  then  L31510
                        goto first_screenload

L31510:          line% = cursor%(1%) - 5%
                 if line% < 1% or line% > 14% then L31310
                 line% = int((line% + 1%) / 2%)
                 c% = top% + line% - 1%

                 if keyhit% <> 0% then L31630
                 if ebom%(idx%(c%)) = 0% then redisplay  /* no bom */
                 if plus$(idx%(c%)) <> "-" then L31600
                     gosub collapse_index_array
                     goto newscreen
L31600:          if plus$(idx%(c%)) <> "+" then redisplay
                     gosub set_key
                     goto screen_control

L31630:          if keyhit% <> 9% then redisplay
                     gosub maintain_record
                     if cchanged% = 1% then newscreen
                     goto redisplay


        REM *************************************************************~
            *            I N S E R T   I N D E X   A R R A Y            *~
            *-----------------------------------------------------------*~
            * We update the array for insert by shoving all elements    *~
            * beyond start point of insert down to the bottom of array  *~
            * and then with second call to MXST4PT move the whole       *~
            * bunch back up to their new position below the end of the  *~
            * insert. Then we update the array with the positions of the*~
            * new elements at their proper (inserted) positions in the  *~
            * array.                                                    *~
            *************************************************************

        insert_in_index_array

            if c% = 0% then L31890  /* skip the down/up moves first time */

            call"MXST4PT"addr(idx%(d%-(oldmax%-c%)),idx%(c%+1%),         ~
                                 oldmax%-c%)
            call "MXST4PT" addr(idx%(c%+1%+new%),idx%(d%-(oldmax%-c%)),  ~
                                 oldmax%-c%)

L31890:     for i% = 1% to new%
                 idx%(c% + i%) = tptr%(i%)
            next i%

            mat tptr% = zer

            return


        abort_message
            k% = 2%
            call "ASKUSER" (k%, " * * * ARRAY OVERFLOW * * *",           ~
                      "Too many components for this program to handle" , ~
                      "Don't drill down so far, or have your SA change", ~
                      "the array size.  Press any key to continue.")

            ret% = 1%
            goto exit_program

        REM *************************************************************~
            *       M A N A G E   T H E   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Build screen lines by positioning the data in the TEMP$   *~
            * and then place TEMP$ into the screen line according to    *~
            * level as dictated by the INDENT%() for each item in the   *~
            * index.                                                    *~
            *************************************************************

        make_screen_lines
             init(hex(20)) scrn$()

             for i% = 1% to min(max%+1%-top%,7%)

                j% =  top% +i% -1%   /* where we are */
                k% = 2%*i% - 1%
                if ebom%(idx%(j%)) <> 0%  then                           ~
                            str(temp$,1,1) = plus$(idx%(j%))

                str(temp$, 2%, 3%) =  seq$(idx%(j%))
                str(temp$, 5%, 1%) =  hex(29)
                str(temp$, 7%,32%) =  plist$(t%, idx%(j%))

                str(scrn$(k%),indent%(idx%(j%))* 3% -2%,79%)= str(temp$,,)

                str(scrn$(k%),65%,10%) =  qty$(idx%(j%))
                str(scrn$(k%),76%,02%) =  marker$(idx%(j%))
                str(scrn$(k%),79%,01%) =  optn$(idx%(j%))
                temp$ = " "
                k% = k% + 1%
                str(temp$, 7%,32%) =  str(phcstsrc$(idx%(j%))) & "  " &  ~
                                      phcstdesc$(idx%(j%))
                str(scrn$(k%), indent%(idx%(j%))* 3% -2%,79%) =          ~
                                                             str(temp$,,)
                str(scrn$(k%),65%,10%) =  cost$(idx%(j%))
                str(scrn$(k%),76%,03%) =  phbom$(idx%(j%))
                temp$ = " "
             next i%
             return


        REM *************************************************************~
            *        C O L L A P S E   I N D E X   A R R A Y            *~
            *-----------------------------------------------------------*~
            * At this point they have selected a parent that has been   *~
            * exploded.  We search downward, removing all parts from    *~
            * PLIST$() (by way of the index) until we find a part at    *~
            * the same indent level as the selected parent, at which    *~
            * point we've gone as deep as there is to go.  We then      *~
            * move the items in the index up, closing the gap.  This    *~
            * collapses the array one or more levels, depending on the  *~
            * level of the selected parent and how much is below it.    *~
            *                                                           *~
            *************************************************************

        collapse_index_array
*          IDX%(C%) is the parent  INDENT%(IDX%(C%) is its level
            plus$(idx%(c%)) = "+"
            oldmax% = max%
            del% = 0%
*        count the suckers and clear each one out
            for s% = 1% to max%-c%
                 if indent%(idx%(c% + s%)) <= indent%(idx%(c%))          ~
                        then hit_bottom
                 del% = del% + 1%
                 active$(idx%(c% + s%)) = hex(ff)  /* reclaim space */
                 if plus$(idx%(c% + s%)) = "-" then                      ~
                                    plus$(idx%(c% + s%)) = "+"
                 ebom%(idx%(c% + s%)) = 0%
               /* if strange values persist, this is where to clear em */
            next s%

        hit_bottom
*        Close up the gap in the index
            call "MXST4PT" addr(idx%(c%+1%),idx%(c% + del% + 1%),        ~
                                 max%-c%-del%)
            max% = oldmax% - del%


            return



        maintain_record
            cchanged% = 0%  :  modify% = 1%
            if active$(idx%(c%)) = hex(ff) then return
            init (hex(00))  readkey$
            readkey$ = str(estimate$) & str(line$) &                     ~
                                                  bin(ebom%(idx%(c%)),3%)
            call "READ100" (#1, readkey$, f1%(1%))
                if f1%(1%) <> 1% then return
            get #1 using L35090, cpart$, cquantity, csize, cfixed, cover, ~
                                cmarker$, coption$, cbom$, crstep$,      ~
                                ctype$, cucost, cucost(), phcstsrc$,     ~
                                phcstdesc$, crequired
L35090:         FMT POS(15), CH(25), POS(85), 4*PD(14,4), CH(2), CH(1),  ~
                    CH(3), POS(127), CH(4), CH(3), POS(179), 13*PD(14,4),~
                    CH(6), CH(23), PD(14,4)

            call "CONVERT" (cquantity, 0.4, cquantity$)
            call "CONVERT" (csize, 0.4, csize$)
            call "CONVERT" (cfixed, 0.2, cfixed$)
            call "CONVERT" (cover,  0.4, cover$)
            call "CONVERT" (cucost, 2.4, cucost$)
            call "CONVERT" (crequired, 0.4, crequired$)

            cdescr$, ctypedescr$, cstkuom$, cuom$ = " "
            call "DESCRIBE" (#4, cpart$, cdescr$, 1%, f1%(4%))
                if f1%(4%) = 1% then L35345
                     cdescr$ = "(Non-Stocked Part)"
                     goto L35420
L35345:     get #4 using L35346, cuom$
L35346:         FMT POS(74), CH(4)
            if ctype$ = " " then get #4 using L35348, ctype$
L35348:         FMT POS(180), CH(3)
            ctypedescr$ = hex(94) & "(Unknown Part Type)"
            convert  ctype$ to ctype%, data goto L35380
            call "HNYTYPE" (ctype%, ctypedescr$, 1%)
L35380:     cstkuom$ = "(Stocking UOM = " & cuom$ & ")"

L35420:     cmkrdescr$ = " "
            if cmarker$ <> "RC" then L35460
                cmkrdescr$ = "Reference Only, Costs Included"
                goto L35510
L35460:     readkey$ = "TABLE01:" & cmarker$
            call "DESCRIBE" (#3, readkey$, cmkrdescr$, 1%, f1%(3%))

L35510:     cbomdescr$ = " " : if cbom$ = " " then L35580
            readkey$ = str(cpart$,,25%) & str(cbom$) & "  0"
            call "DESCRIBE" (#5, readkey$, cbomdescr$, 1%, f1%(5%))
                if f1%(5%) = 0% then cbomdescr$ = "(Not On File)"

L35580:     cbomcstsrce$ = "(" & phcstdesc$ & " - " & phcstsrc$ & ")"

*        Check for Explosion under this part
            detailmsg$ = " "
            init(hex(00)) readkey$
            str(readkey$,1%,39%) = str(estimate$) & str(line$) &         ~
                                   bin(ebom%(idx%(c%)),3%) &             ~
                                   str(plist$(1%,idx%(c%)),1%,25%)
            call "PLOWALTS" (#1, readkey$, 1%, 39%, f1%(1%)) /* Wrk File*/
                 if f1%(1%) <> 1% then goto L36000
                     detailmsg$ = "Upper Level Component CANNOT be Mod" &~
                                  "ified.  Cost May be Modified at Low" &~
                                  "er Level."
                     modify% = 0%

L36000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       savedetail
                  if keyhit% <>  0% then       editpg1
L36120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 11% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L36170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L36170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L36170
                  lastfieldnr% = fieldnr%
            goto L36120

        savedetail
            if cchanged% = 0% then return
            init (hex(00))  readkey$
            readkey$ = str(estimate$) & str(line$) &                     ~
                                                  bin(ebom%(idx%(c%)),3%)
            call "READ101" (#1, readkey$, f1%(1%))
                if f1%(1%) <> 1% then return
            convert cucost$ to cucost
            put #1 using L36330, cmarker$, cucost, cucost(), phcstsrc$,   ~
                                phcstdesc$
L36330:         FMT POS(117), CH(2), POS(179), 13*PD(14,4), CH(6), CH(23)
            rewrite #1

            marker$(idx%(c%)) = cmarker$
            phcstsrc$(idx%(c%)) = phcstsrc$
            phcstdesc$(idx%(c%)) = phcstdesc$
            call "CONVERT" (cucost, 2.4, cost$(idx%(c%)))
            cbomcstsrce$ = "(" & phcstdesc$ & " - " & phcstsrc$ & ")"
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        display_the_screen
              inpmessage$ = "Position Cursor & RETURN at '+' to Explod"& ~
                            "e or at '-' to Collapse.           "
              gosub set_pf2
              init(hex(8c)) lfac$()


L41090:     accept                                                       ~
               at (01,02),                                               ~
                  "Estimate Component Cost Browser",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02), fac(hex(8c)), colheader1$            , ch(79),~
               at (05,02), fac(hex(ac)), colheader2$            , ch(79),~
               at (06,02), fac(lfac$( 1%)), scrn$(       1%)    , ch(79),~
               at (07,02), fac(lfac$( 2%)), scrn$(       2%)    , ch(79),~
               at (08,02), fac(lfac$( 3%)), scrn$(       3%)    , ch(79),~
               at (09,02), fac(lfac$( 4%)), scrn$(       4%)    , ch(79),~
               at (10,02), fac(lfac$( 5%)), scrn$(       5%)    , ch(79),~
               at (11,02), fac(lfac$( 6%)), scrn$(       6%)    , ch(79),~
               at (12,02), fac(lfac$( 7%)), scrn$(       7%)    , ch(79),~
               at (13,02), fac(lfac$( 8%)), scrn$(       8%)    , ch(79),~
               at (14,02), fac(lfac$( 9%)), scrn$(       9%)    , ch(79),~
               at (15,02), fac(lfac$(10%)), scrn$(      10%)    , ch(79),~
               at (16,02), fac(lfac$(11%)), scrn$(      11%)    , ch(79),~
               at (17,02), fac(lfac$(12%)), scrn$(      12%)    , ch(79),~
               at (18,02), fac(lfac$(13%)), scrn$(      13%)    , ch(79),~
               at (19,02), fac(lfac$(14%)), scrn$(      14%)    , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41200
                  call "MANUAL" ("ESTCSTSB") : goto L41090

L41200:        if keyhit% <> 15% then L41215
                  call "PRNTSCRN" : goto L41090

L41215:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
                                     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "(8)Toggle Part/Descr   (13)Instructions"
            pf$(2%)= "(2)First  (4)Prev. Screen  (6)Up One    " &        ~
                     "(9)Select Part         (15)Print Screen"
            pf$(3%)= "(3)Last   (5)Next Screen   (7)Down One (" &        ~
                     "10)Collapse All        (16)Return      "
            pfkeys$ = hex(0102030405060708090affff0dff0f1000)

            if mode% = 2% then L41280
                str(pf$(2),41,14) = " "  :  str(pfkeys$,9,1) = hex(ff)
L41280:     if top% > 1% then L41290
                str(pf$(2),1,38)  = " "  :  str(pfkeys$,2,1) = hex(ff)
                str(pfkeys$,4,1) = hex(ff): str(pfkeys$,6,1) = hex(ff)
L41290:     if top% + 7% <= max%  then L41300
                str(pf$(3),1,38)  = " "  :  str(pfkeys$,3,1) = hex(ff)
                str(pfkeys$,5,1) = hex(ff): str(pfkeys$,7,1) = hex(ff)
L41300:     if pos(str(plus$()) = "-") <> 0% then L41303 /* collapsable?*/
                str(pf$(3),40,16) = " "  :  str(pfkeys$,10,1) = hex(ff)
L41303:     return

*        IF FIELDNR% > 0% THEN 41355  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            return
                                     /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42250,         /* Part Number       */   ~
                                L42250,         /* Optional Componen */   ~
                                L42260,         /* Quantity          */   ~
                                L42260,         /* Size              */   ~
                                L42260,         /* Added Overage per */   ~
                                L42260,         /* Fixed Quantity pe */   ~
                                L42260,         /* Required Quantity */   ~
                                L42250,         /* BOM Marker        */   ~
                                L42250,         /* Specific BOM Id.  */   ~
                                L42250,         /* Pick Before RTE S */   ~
                                L42260          /* Unit Cost         */
              goto L42280

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42250:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42260:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42280:     accept                                                       ~
               at (01,02),                                               ~
                  "Estimate BOM Component Cost Maintenance",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,15), fac(lfac$( 1)), cpart$               , ch(25),~
               at (06,41), fac(hex(8c)),   cdescr$              , ch(34),~
                                                                         ~
               at (07,02), "Optional Component?",                        ~
               at (07,25), fac(lfac$( 2)), coption$             , ch(01),~
               at (07,41), fac(hex(8c)),  ctypedescr$           , ch(32),~
                                                                         ~
               at (08,02), "Quantity",                                   ~
               at (08,25), fac(lfac$( 3)), cquantity$           , ch(10),~
               at (08,41), fac(hex(8c)),   cstkuom$             , ch(25),~
                                                                         ~
               at (09,02), "Size",                                       ~
               at (09,25), fac(lfac$( 4)), csize$               , ch(10),~
                                                                         ~
               at (10,02), "Added Overage per Unit",                     ~
               at (10,25), fac(lfac$( 5)), cover$               , ch(10),~
                                                                         ~
               at (11,02), "Fixed Quantity per Run",                     ~
               at (11,25), fac(lfac$( 6)), cfixed$              , ch(10),~
                                                                         ~
               at (12,02), "Required Quantity",                          ~
               at (12,25), fac(lfac$( 7)), crequired$           , ch(10),~
                                                                         ~
               at (13,02), "BOM Marker",                                 ~
               at (13,25), fac(lfac$( 8)), cmarker$             , ch(02),~
               at (13,41), fac(hex(8c)),   cmkrdescr$           , ch(30),~
                                                                         ~
               at (14,02), "Specific BOM Id.",                           ~
               at (14,25), fac(lfac$( 9)), cbom$                , ch(03),~
               at (14,41), fac(hex(8c)),   cbomdescr$           , ch(32),~
                                                                         ~
               at (15,02), "Pick Before RTE Step",                       ~
               at (15,25), fac(lfac$(10)), crstep$              , ch(04),~
                                                                         ~
               at (16,02), "Unit Cost",                                  ~
               at (16,25), fac(lfac$(11)), cucost$              , ch(10),~
               at (16,41), fac(hex(8c)),    cbomcstsrce$        , ch(34),~
                                                                         ~
               at (18,02), fac(lfac$(12)), detailmsg$           , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42790
                  call "MANUAL" ("ESTINPUT") : goto L42280

L42790:        if keyhit% <> 15 then L42820
                  call "PRNTSCRN" : goto L42280

L42820:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L43010     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42970
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42970:     if fieldnr% > 2% then L42990  :  str(pfkeys$,16,1) = hex(ff)
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42990:     return

L43010: if fieldnr% > 0% then L43100  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L43100:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50175,         /* Part Number            */~
                              L50205,         /* Optional Componen      */~
                              L50235,         /* Quantity               */~
                              L50265,         /* Size                   */~
                              L50295,         /* Added Overage per      */~
                              L50325,         /* Fixed Quantity pe      */~
                              L50355,         /* Required Quantity      */~
                              L50385,         /* BOM Marker             */~
                              L50415,         /* Specific BOM Id.       */~
                              L50445,         /* Pick Before RTE S      */~
                              L50475          /* Unit Cost              */
            return
L50175: REM Test for Part Number                  CPART$
            return

L50205: REM Test for Optional Component?          COPTION$
            return

L50235: REM Test for Quantity                     CQUANTITY$
            return

L50265: REM Test for Size                         CSIZE$
            return

L50295: REM Test for Added Overage per Unit       COVER$
            return

L50325: REM Test for Fixed Quantity per Run       CFIXED$
            return

L50355: REM Test for Required Quantity            CREQUIRED$
            return

L50385: REM Test for BOM Marker                   CMARKER$
            if cmarker$ = "RE" or cmarker$ = "RC" then L50390
                errormsg$ = "Marker Must be Either 'RE' or 'RC'."
                return
L50390:     if cmarker$ = oldcmarker$ then L50395
                changed%, cchanged% = 1%
L50395:     return

L50415: REM Test for Specific BOM Id.             CBOM$
            return

L50445: REM Test for Pick Before RTE Step         CRSTEPS$
            return

L50475: REM Test for Unit Cost                    CUCOST$
            call "NUMTEST" (cucost$, 0, 9e7, errormsg$,-2.4,test)
                if errormsg$ <> " " then return
            call "CONVERT" (test, 2.4, cucost$)
            if oldcucost = test then return
                changed%, cchanged% = 1%
                if oldcucost = 0 then L50545
                     str(phcstsrc$,4%,3%) = "MOR"
                     phcstdesc$ = "Manual Override by " & userid$
                     cbomcstsrce$ = "(" & phcstdesc$ & " - " & phcstsrc$ ~
                                    & ")"
                     return
L50545:         str(phcstsrc$,4%,3%) = "MAN"
                phcstdesc$ = "Manual Entry by " & userid$
                cbomcstsrce$ = "(" & phcstdesc$ & " - " & phcstsrc$ & ")"
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
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
*          CALL "SHOSTAT" ("One Moment Please")

            end
