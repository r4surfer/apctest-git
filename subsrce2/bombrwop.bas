        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M  BBBB   RRRR   W     W   OOO   PPPP  *~
            *  B   B  O   O  MM MM  B   B  R   R  W     W  O   O  P   P *~
            *  BBBB   O   O  M M M  BBBB   RRRR   W     W  O   O  PPPP  *~
            *  B   B  O   O  M   M  B   B  R   R  W  W  W  O   O  P     *~
            *  BBBB    OOO   M   M  BBBB   R   R   WW WW    OOO   P     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMBRWOP - Brouse BOM Options. Cloned from BOMBRWSB.      *~
                         Displays the components of the parent part     *~
            *            (and bill version) passed in.  Then allows     *~
            *            the user to select a parent and drill down     *~
            *            to see its components, etc.  Shows generic     *~
            *            parts as bright and the option marker column   *~
                         flags those parts that are options in the bill *~
            *            for which replacements can be selected.  If    *~
            *            you pick one and press PF 12 you get to see    *~
            *            the list of replacements.  If you select one   *~
            *            the explosion continues from there and you can *~
            *            move down the bill below the replacement part. *~
            *            You can undo a replacement.  Note that when    *~
            *            you collapse the explosion, all replacements   *~
            *            below the collapse point are undone.           *~
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
            * 08/12/93 ! Original Cloned from BOMBRWSB            ! WPH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BOMBRWOP" (#1,            /* BOMMASTR                     */~
                        #2,            /* ENGMASTR                     */~
                        #3,            /* SYSFILE2                     */~
                        #4,            /* HNYMASTR                     */~
                        #5,            /* HNYOPTNS                     */~
                        parent$,       /* Parent part passed in        */~
                        parentbom$,    /* BOM (use effective if blank) */~
                        err$)          /* blank means all OK           */~
                                       /*  else       array overflow   */~
                                       /*             no effective bom */~
                                       /*             no part master   */~
                                       /*             no BOM found     */~

        dim                                                              ~
            bom$(490)3,                  /* Component BOM array        */~
            cbomid$3,                    /* Component BOM ID           */~
            colheader$79,                /* Column Header              */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            pflag$(114)1,                /* Option print flag          */~
            desc$(114)32,                /* Option replacement descpts */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            err$79,                      /* Message returned to caller */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            line3$79,                    /* Screen Line #2 (another one*/~
            marker$2,                    /* BOM marker                 */~
            optn$1,                      /* BOM component is option flg*/~
            obom$(114)3,                 /* BOM for each option        */~
            otype$(114)3,                /* Part type for each option  */~
            opheader$79,                 /* Column header on optn scrn */~
            parent$25,                   /* Parent Part Number         */~
            parentbom$3,                 /* Bill Version               */~
            parentdescr$32,              /* Parent Part Description    */~
            part$(114)25,                /* Option replacement parts   */~
            parenttype$3,                /* Parent Part Type           */~
            pldate$6,                    /* Planning Base Date         */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            qty(4),                      /* Component quantities       */~
            readkey$99,                  /* Miscellaneous Read         */~
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
            ebom$(1000)3,                /* Effective BOM for parts    */~
            plus$(1000)1,                /* Indicates levels below     */~
            seq$(1000)3,                 /* Component sequence number  */~
            tptr%(1000),                 /* Temp pointers to new parts */~
            qty$(1000)10,                /* Extended comp. qty/parent  */~
            type$(1000)3,                /* Part type for options flag */~
            rbom$(1000)3,                /* Replacement parts BOM      */~
            rlist$(2,1000)32,            /* List of replacement parts  */~
            rtype$(1000)3,               /* Part type for replacement  */~
            idx%(1000)                   /* Position index             */


        dim f1%(64)                                                      ~


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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

                        /*was  1  2  3  4  5  6  7  8  9  10    */
            colheader$   = "   1 2 3 4 5 6 7 8 9 10                    "&~
                           "                       Quantity Mk O"
            err$ = " "
            str(line2$,62%) = "BOMBRWOP: " & str(cms2v$,,8%)
            str(line3$,62%) = "BOMBRWOP: " & str(cms2v$,,8%)

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
                 if f1%(4%) =  1% then L20302
                 err$ = "ERROR - No part master file record found."
                 goto exit_program

L20302:     init(hex(00)) readkey$
                 str(readkey$,1%,25%) = str(parent$,1%,25%)
                 str(readkey$,26%,3%) = str(parentbom$,1%,3%)
                 str(readkey$,29%,3%) = "  0"
            call "READ100" (#1, readkey$, f1%(1%))
            if f1%(1%) <> 0% then L20350
                 err$ = "ERROR - No bill of material record found."
                 goto exit_program

L20350:     call "READ100" (#4, parent$, f1%(4%)) /* hnymastr*/
                 if f1%(4%) =  1% then L20357
                 err$ = "ERROR - No Item Master File record found."
                 goto exit_program
L20357:     get #4, using L20358, parenttype$
L20358:          FMT POS(180), CH(3)

            if parenttype$ = "000" then L20430
            k% = 2%
            call "ASKUSER" (k%, " NOT A GENERIC PART ",                  ~
                         "The selected part is not a 'generic' part.",   ~
                       "Therefore it has no options below it to review.",~
                       "Press any key to acknowledge.")
            goto exit_program
L20430:     goto first_screenload  /* all is OK, lets get on with it */

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
            Related arrays are EBOM$() which contains the effective bom  ~
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

            str(line2$,1%,55%)  = "For Generic Part: "  & parent$ &      ~
                                    ", BOM: " & parentbom$

            init(hex(00)) plowkey$, plist$()
            init(" ") ebom$(), type$(), plus$(), optn$(), rtype$(),      ~
                      marker$(), seq$(), rlist$(), rbom$()
            mat indent% = con
            init(hex(ff)) active$()
            mat tptr% = zer
            mat idx%  = zer
            str(plowkey$, 1%, 25%) = parent$
            str(plowkey$,26%,  3%) = parentbom$

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

L30680:     call "PLOWNEXT" (#1, plowkey$, 28%, f1%(1%))
            if f1%(1%) <> 1% then return

            get #1, using L30720, temp$, seq$
L30720:         FMT CH(25), POS(54), CH(3)

            if seq$ <> "  0" then L30740
                get #1, using L30732, batch
L30732:         FMT POS(107), PD(14,4)
                goto L30680               /* skip header records */

L30740:     get #1, using L30741, temp$,seq$,qty(),marker$,optn$, cbomid$
L30741:         FMT CH(25), POS(54), CH(3), 4*PD(14,4),CH(2),CH(1), CH(3)

            new% = new% + 1%
            max% = max% + 1%
            if max% > d% then abort_message     /* exceeded dimension */

            a% = pos(str(active$()) = hex(ff)) /* find a hole in array */
            plist$(1%, a%) = str(temp$,1,25)   /* put it in the hole   */
            tptr%(new%) = a%                   /* record where it is   */
            active$(a%) = " "                  /* mark hole as filled  */
            temp$ = " "
            ebom$(a%) = cbomid$
            indent%(a%) = indent_level%
            marker$(a%) = marker$
            seq$(a%) = seq$
            if optn$ = "Y" then optn$(a%) = optn$
            total =  (batch * ((qty(1%)* qty(2%)) + qty(4%)))/*+QTY(3)*/
            call "CONVERT"(total, 0.4, qty$(a%))

            gosub check_for_bom    /* find effective bom if exists  */
            call "DESCRIBE" (#4, plist$(1%,a%), plist$(2%,a%),0%, f1%(4%))
            goto L30680

        check_for_bom /* does component have effective bom?*/
            if ebom$(a%) <> " " then L30930 /*using 'bill to use' instead*/
            init(hex(00)) readkey$
            str(readkey$,1%,29%) = str(plist$(1%,a%),1%,25%)& "1" & "001"
            call "READ100" (#2, readkey$, f1%(2%))  /* engmastr */
                 if f1%(2%) <> 1% then return
                   get #2, using L30910,  bom$()
L30910:               FMT POS(30), 490 * CH(3)
            ebom$(a%) = bom$(todayindex%)
L30930:     plus$(a%) = "+"
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
            if rlist$(1%, idx%(c%)) <> " " then L31140

            str(plowkey$, 1%, 25%) = plist$(1%, idx%(c%))
            str(plowkey$,26%,  3%) = ebom$(idx%(c%))
            goto L31150

L31140:     str(plowkey$, 1%, 25%) = rlist$(1%, idx%(c%))
            str(plowkey$,26%,  3%) = rbom$(idx%(c%))

L31150:     indent_level% = indent%(idx%(c%)) + 1% /* in another level */
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
                        top% = max(max% - 14%, 1%)
                        goto newscreen
L31390:          if keyhit% <> 4%  then  L31420
                        top% = max(1%, top% - 14%)
                        goto newscreen
L31420:          if keyhit% <> 5%  then  L31450
                        top% = min(max% - 14%, top% + 15%)
                        goto newscreen
L31450:          if keyhit% <> 6%  then  L31480
                        top% = max(1%, top% - 1%)
                        goto newscreen
L31480:          if keyhit% <> 7%  then  L31502
                        top% = min(max%, top% + 1%)
                        goto newscreen

L31502:          if keyhit% <> 10%  then  L31510
                        goto first_screenload

L31510:          line% = cursor%(1%) - 4%
                 if line% < 1% or line% > 15% then L31310
                 c% = top% + line% - 1%

                 if keyhit% <> 0% then L31660
                 if rlist$(1%, idx%(c%)) = " " then L31560
                     if rbom$(idx%(c%)) = " " then redisplay /* no bom */
                     if plus$(idx%(c%)) <> "-" then L31600
                     gosub collapse_index_array
                     goto newscreen

L31560:          if ebom$(idx%(c%)) = " " then redisplay /* no bom */
                 if plus$(idx%(c%)) <> "-" then L31600
                     gosub collapse_index_array
                     goto newscreen
L31600:          if plus$(idx%(c%)) <> "+" then redisplay
                     if indent%(idx%(c%)) < 10% then L31610
                     k% = 2%
                     call "ASKUSER"(k%, "* * * LIMIT REACHED * * *",     ~
                      "This routine cannot display an explosion deeper", ~
                      "than 10 levels.  You've hit this limit, sorry.",  ~
                      "Press any key to acknowledge.")
            /* note - no big deal if limit exceeded, screen just wraps */
                     goto redisplay

L31610:              gosub set_key
                     goto screen_control

L31660:          if keyhit% <> 12% then redisplay
                     if optn$(idx%(c%)) = " " then redisplay
                     goto option_control

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
             temp$ = " "
             for i% = 1% to min(max%+1%-top%,15%)

                j% =  top% +i% -1%   /* where we are */

                if rlist$(1%,idx%(j%)) <> " " then L32200
                if ebom$(idx%(j%)) <> " " then                           ~
                            str(temp$,1,1) = plus$(idx%(j%))
                goto L32203

L32200:         if rbom$(idx%(j%)) <> " " then                           ~
                            str(temp$,1,1) = plus$(idx%(j%))

L32203:         str(temp$, 2%, 3%) =  seq$(idx%(j%))
                str(temp$, 5%, 1%) =  hex(29)          /* the ')' */
                if type$(idx%(j%)) = "000" then str(temp$,6%,1%) = hex(84)
                str(temp$, 7%,32%) =  plist$(t%, idx%(j%))

               /* do the swap of replacement part */
                if rlist$(t%, idx%(j%)) = " " then L32215
                   str(temp$, 7%,32%) =  rlist$(t%, idx%(j%))
                   str(temp$,6%,1%) = hex(8c)
                   if rtype$(idx%(j%)) = "000" then                      ~
                           str(temp$,6%,1%) = hex(84)

L32215:         str(scrn$(i%),indent%(idx%(j%))* 2% -1%,79%)= str(temp$,,)
                str(scrn$(i%),64%,1%) =  hex(8c)
                str(scrn$(i%),65%,10%) =  qty$(idx%(j%))
                str(scrn$(i%),76%,02%) =  marker$(idx%(j%))
*              IF OPTN$(IDX%(J%))<>" "THEN STR(SCRN$(I%),78%,1%)= HEX(84)
                str(scrn$(i%),78%,1%)= hex(84)
                str(scrn$(i%),79%,01%) =  optn$(idx%(j%))
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
                 ebom$(idx%(c% + s%)) = " "
                 plist$(1, idx%(c% + s%)) = " "  /* ? */
                 plist$(2, idx%(c% + s%)) = " "
                 rlist$(1, idx%(c% + s%)) = " "
                 rlist$(2, idx%(c% + s%)) = " "
               /* if strange values persist, this is where to clear em */
            next s%

        hit_bottom
*        Close up the gap in the index
            call "MXST4PT" addr(idx%(c%+1%),idx%(c% + del% + 1%),        ~
                                 max%-c%-del%)
            max% = oldmax% - del%


            return



        REM *************************************************************~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************~

        option_control
            init(" ") part$(), pflag$(), otype$(), obom$(), desc$()
*       First we find the parent of the selected part
            for up% = c% to 1% step -1
               if plus$(up%) = "-" then L34052
            next up%

*       Got it, so now we can plow HNYOPTNS for the list of options
L34052:     maxlines%, l% = 0%
            init(hex(00)) readkey$
            str(readkey$,,28) = str(plist$(1%,up%),,25)& str(ebom$(up%))
            str(readkey$,29,26) = str(plist$(1%,idx%(c%)),,25) & hex(00)
            call "PLOWNEXT" (#5, readkey$, 53%, f1%(5))
                if f1%(5) <> 0% then  L34072
*       No assembly specific options, so look for part specific
            str(readkey$,,28) = " "
L34062:     call "PLOWNEXT" (#5, readkey$, 53%, f1%(5))
                if f1%(5) <> 0% then  L34072
            if l% <> 0% then L34067
                part$(1%) = "* No Replacements Found *"
                maxlines%, l% = 1%
L34067:     goto option_screen_control

L34072:     if l% = 0% then print at(4,1,80);hex(84);                    ~
                                           "Loading Replacements List..."
            l%, maxlines% = l% + 1
            get #5, using L34082, part$(l%),pflag$(l%),                   ~
                                                otype$(l%), obom$(l%)

L34082:        FMT XX(54), CH(25), CH(1), CH(3), CH(3)
            call "DESCRIBE" (#4, part$(l%), desc$(l%), 0% , f1%(4))
            if pflag$(l%)<>"Y" and pflag$(l%)<>"D" then pflag$(l%)="N"
            goto L34062

        option_screen_control
            gosub display_options
            if keyhit% = 16% then redisplay
            if keyhit% = 10% then undo_replacement

            if keyhit% <> 0% then redisplay
                sl% = cursor%(1%) - 4%
                if sl% < 1% or sl% > 15% then option_screen_control
*       We record the replacement, read HNYMASTR for descr & type
                rlist$(1%, idx%(c%)) = part$(sl%)
                call "READ100" (#4, rlist$(1%,idx%(c%)),f1%(4%))
                     if f1%(4%) <> 1% then return
                get #4, using L34104, rlist$(2%,idx%(c%)),                ~
                                                       rtype$(idx%(c%))
L34104:              FMT POS(26), CH(32),   POS(180), CH(3)
                optn$(idx%(c%)) = "R" /* to flag replacement made */
*       Clear out lower levels to avoid confusion
                if plus$(idx%(c%)) = "-" then gosub collapse_index_array
*       Does the replacement have a BOM?
                plus$(idx%(c%)) = " "
                init(hex(00)) readkey$
                str(readkey$,1%,29%) = str(rlist$(1%,idx%(c%)),          ~
                                            1%, 25%) & "1" & "001"
                call "READ100" (#2, readkey$, f1%(2%))  /* engmastr */
                    if f1%(2%) <> 1% then L34125
                       get #2, using L34116,  bom$()
L34116:                   FMT POS(30), 490 * CH(3)
                rbom$(idx%(c%)) = bom$(todayindex%)
                plus$(idx%(c%)) = "+"
L34125:         goto newscreen

        undo_replacement   /* undo the option part replacement */
            init(" ") rlist$(1%,idx%(c%)), rlist$(2%,idx%(c%)),          ~
                         rtype$(idx%(c%)), rbom$(idx%(c%))
            optn$(idx%(c%)) = "Y" /* reset to flag option part */

            if ebom$(idx%(c%)) <> " " then  L34150
                plus$(idx%(c%)) = " "
                goto L34170

L34150:     if plus$(idx%(c%)) <> "-" then L34170
            gosub collapse_index_array
            /* later, remember to clear lower replacements here also */
L34170:     goto newscreen

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        display_the_screen
              inpmessage$ = "Cursor & RETURN at '+' to Explode or at '"& ~
                            "-' to Collapse; or PF 12 for Options."
              errormsg$ = "Generic Parts are Highlighted; 'O' Column " & ~
                           "Flags Options, Replacements"

              gosub set_pf2
              init(hex(8c)) lfac$()


L41090:     accept                                                       ~
               at (01,02),                                               ~
                  "Option Bill of Materials Review",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), errormsg$              , ch(79),~
               at (04,02), fac(hex(ac)), colheader$             , ch(79),~
               at (05,02), fac(lfac$( 1%)), scrn$(       1%)    , ch(79),~
               at (06,02), fac(lfac$( 2%)), scrn$(       2%)    , ch(79),~
               at (07,02), fac(lfac$( 3%)), scrn$(       3%)    , ch(79),~
               at (08,02), fac(lfac$( 4%)), scrn$(       4%)    , ch(79),~
               at (09,02), fac(lfac$( 5%)), scrn$(       5%)    , ch(79),~
               at (10,02), fac(lfac$( 6%)), scrn$(       6%)    , ch(79),~
               at (11,02), fac(lfac$( 7%)), scrn$(       7%)    , ch(79),~
               at (12,02), fac(lfac$( 8%)), scrn$(       8%)    , ch(79),~
               at (13,02), fac(lfac$( 9%)), scrn$(       9%)    , ch(79),~
               at (14,02), fac(lfac$(10%)), scrn$(      10%)    , ch(79),~
               at (15,02), fac(lfac$(11%)), scrn$(      11%)    , ch(79),~
               at (16,02), fac(lfac$(12%)), scrn$(      12%)    , ch(79),~
               at (17,02), fac(lfac$(13%)), scrn$(      13%)    , ch(79),~
               at (18,02), fac(lfac$(14%)), scrn$(      14%)    , ch(79),~
               at (19,02), fac(lfac$(15%)), scrn$(      15%)    , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41200
                  call "MANUAL" ("BOMDRILL") : goto L41090

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
                     "                       (15)Print Screen"
            pf$(3%)= "(3)Last   (5)Next Screen   (7)Down One (" &        ~
                     "10)Collapse All        (16)Return      "
            pfkeys$ = hex(0102030405060708ff0aff0c0dff0f1000)



            if top% > 1% then L41290
                str(pf$(2),1,38)  = " "  :  str(pfkeys$,2,1) = hex(ff)
                str(pfkeys$,4,1) = hex(ff): str(pfkeys$,6,1) = hex(ff)
L41290:     if top% + 15% <= max%  then L41300
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
            *           S C R E E N   P A G E  2                        *~
            *-----------------------------------------------------------*~
            * Display the replacement parts for selected option part.   *~
            *************************************************************

        display_options
              inpmessage$ = "Position Cursor & RETURN to Select Replac"& ~
                            "ement Part, or PF 16 to Return.    "
              opheader$ =  "Part Code                     Description"
              errormsg$ = "  "
              gosub set_pfo
              init(hex(8c)) lfac$()
              if rlist$(1%,idx%(c%)) = " " then L42115
              errormsg$ = "Current Replacement is Highlighted"
              l% = 0%
              init(hex(8c)) lfac$()
                  for i% = l%+1% to l%+15%
                    if part$(i%) <> rlist$(1%,idx%(c%)) then next i%
                  lfac$(i%) = hex(84)  /* brighten the selected one */
L42115:       l% = 0%
              str(line3$,1,60) = "Replacements for Option Part: "        ~
                                & plist$(1%,idx%(c%))

L42130:     accept                                                       ~
               at (01,02),                                               ~
                  "Option Bill of Materials Review",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line3$                 , ch(79),~
               at (03,02), fac(hex(84)), errormsg$              , ch(79),~
               at (04,02), fac(hex(ac)), opheader$              , ch(79),~
               at (05,02), fac(lfac$(l% +  1%)), part$(l% +  1%), ch(25),~
               at (06,02), fac(lfac$(l% +  2%)), part$(l% +  2%), ch(25),~
               at (07,02), fac(lfac$(l% +  3%)), part$(l% +  3%), ch(25),~
               at (08,02), fac(lfac$(l% +  4%)), part$(l% +  4%), ch(25),~
               at (09,02), fac(lfac$(l% +  5%)), part$(l% +  5%), ch(25),~
               at (10,02), fac(lfac$(l% +  6%)), part$(l% +  6%), ch(25),~
               at (11,02), fac(lfac$(l% +  7%)), part$(l% +  7%), ch(25),~
               at (12,02), fac(lfac$(l% +  8%)), part$(l% +  8%), ch(25),~
               at (13,02), fac(lfac$(l% +  9%)), part$(l% +  9%), ch(25),~
               at (14,02), fac(lfac$(l% + 10%)), part$(l% + 10%), ch(25),~
               at (15,02), fac(lfac$(l% + 11%)), part$(l% + 11%), ch(25),~
               at (16,02), fac(lfac$(l% + 12%)), part$(l% + 12%), ch(25),~
               at (17,02), fac(lfac$(l% + 13%)), part$(l% + 13%), ch(25),~
               at (18,02), fac(lfac$(l% + 14%)), part$(l% + 14%), ch(25),~
               at (19,02), fac(lfac$(l% + 15%)), part$(l% + 15%), ch(25),~
                                                                         ~
               at (05,32), fac(lfac$(l% +  1%)), desc$(l% +  1%), ch(32),~
               at (06,32), fac(lfac$(l% +  2%)), desc$(l% +  2%), ch(32),~
               at (07,32), fac(lfac$(l% +  3%)), desc$(l% +  3%), ch(32),~
               at (08,32), fac(lfac$(l% +  4%)), desc$(l% +  4%), ch(32),~
               at (09,32), fac(lfac$(l% +  5%)), desc$(l% +  5%), ch(32),~
               at (10,32), fac(lfac$(l% +  6%)), desc$(l% +  6%), ch(32),~
               at (11,32), fac(lfac$(l% +  7%)), desc$(l% +  7%), ch(32),~
               at (12,32), fac(lfac$(l% +  8%)), desc$(l% +  8%), ch(32),~
               at (13,32), fac(lfac$(l% +  9%)), desc$(l% +  9%), ch(32),~
               at (14,32), fac(lfac$(l% + 10%)), desc$(l% + 10%), ch(32),~
               at (15,32), fac(lfac$(l% + 11%)), desc$(l% + 11%), ch(32),~
               at (16,32), fac(lfac$(l% + 12%)), desc$(l% + 12%), ch(32),~
               at (17,32), fac(lfac$(l% + 13%)), desc$(l% + 13%), ch(32),~
               at (18,32), fac(lfac$(l% + 14%)), desc$(l% + 14%), ch(32),~
               at (19,32), fac(lfac$(l% + 15%)), desc$(l% + 15%), ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L42460
                  call "MANUAL" ("BOMDRILL") : goto L42130

L42460:        if keyhit% <> 15% then L42490
                  call "PRNTSCRN" : goto L42130

L42490:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pfo
                                     /*  Review Mode            */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "(2)First  (4)Prev. Screen  (6)Up One    " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "(3)Last   (5)Next Screen   (7)Down One (" &        ~
                     "10)Undo Replacement    (16)Return      "
            pfkeys$ = hex(01020304050607ffff0aff0c0dff0f1000)


            if maxlines% <= 15% then L42660
            if l%  >  1% then L42680
L42660:         str(pf$(2),1,38)  = " "  :  str(pfkeys$,2,1) = hex(ff)
                str(pfkeys$,4,1) = hex(ff): str(pfkeys$,6,1) = hex(ff)
L42680:     if l%   + 15% <= maxlines%  then L42710
                str(pf$(3),1,38)  = " "  :  str(pfkeys$,3,1) = hex(ff)
                str(pfkeys$,5,1) = hex(ff): str(pfkeys$,7,1) = hex(ff)
L42710:     if rlist$(1%,idx%(c%)) <> " " then L42730
                str(pf$(3),39,21) = " "  :  str(pfkeys$,10,1) = hex(ff)
L42730:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************









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
