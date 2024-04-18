        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   M   M   AAA   PPPP    SSS   BBBB    *~
            *  S        T    C   C  MM MM  A   A  P   P  S      B   B   *~
            *   SSS     T    C      M M M  AAAAA  PPPP    SSS   BBBB    *~
            *      S    T    C   C  M   M  A   A  P          S  B   B   *~
            *   SSS     T     CCC   M   M  A   A  P       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCMAPSB - Allows Definition and Modification of Standard *~
            *            Cost Fold-In Mappings.                         *~
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
            * 03/30/87 ! Original                                 ! ERN *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCMAPSB"   (set$, set_descr$, mod$, bucket_ids$(),         ~
                          bucket_descrs$(), buckets%, #14)

        dim                                                              ~
            bucket_descrs$(12)20,        /* Cost Bucket Descriptions   */~
            bucket_ids$(12)10,           /* Cost Bucket Names          */~
            bucket_nrs$(12)10,           /* Cost Bucket Numbers (1-12) */~
            company$60,                  /* Company Name for Report    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$(4)20,                   /* Screen Column Headings     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            map%(12), map$(12)2,         /* Fold-In Mapping            */~
            map_id$8, map_descr$30,      /* Map ID and Description     */~
            map_ids$(12)10,              /* The Map in humanieeze      */~
            map_descrs$(12)20,           /*                            */~
            mod$1, mod_flag$30,          /* Modification Flags         */~
            p%(1),                       /* Rcvr argument for Serach   */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* A Plow Key                 */~
            prt_class$4, prt_descr$30,   /* Cose and Descr for Report  */~
            runtime$8                    /* Report Run Time            */

        dim f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #14 ! STCMAPNG ! Standard Cost Set Mappings               *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            line2$ = "Cost Set: " & set$ & " (" & set_descr$ & ")"
            str(line2$,62) = "STCMAPSB: " & str(cms2v$,,8)
            init (" ") bucket_nrs$()
            for b% = 1% to buckets%
                convert b% to bucket_nrs$(b%), pic(#0)
            next b%

            hdr$(1) = "##"
            hdr$(2) = "Bucket ID"
            hdr$(3) = "Bucket Description"
            hdr$(4) = "==>"

            mod_flag$ = " "
            if mod$ = "F" then mod_flag$ = "Set Frozen: Edit not Allowed"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000

            for fieldnr% = 1% to buckets% + 2%
L10100:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10150
                         fieldnr% = max(2%, fieldnr% - 1%)
                         goto L10100
L10150:               if keyhit% = 14 then print_listing
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10100
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10100
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
          if mod_flag$ > " " then display_only
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then gosub delete_map
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
L11140:     if cursor%(1) <> 5% then L11160
                fieldnr% = 2% : goto L11170
L11160:     fieldnr% = cursor%(1) - 5%
L11170:     if fieldnr% < 2% or fieldnr% > 2% + buckets% then editmode
            if fieldnr% = lastfieldnr% then editmode
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
                  goto L11140


        delete_map
L11290:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE",                               ~
                  "Press RETURN to DELETE -or- PF-1 to cancel deletion.",~
                  "NOTE: Any remaining references to this Map ID will ", ~
                  "      be considered as one-to-one fold-in mappings.")
            if u3% = 1% then return
                if u3% <> 0% then L11290
                call "DELETE" (#14, map_id$, 8%)
                return clear all
                goto inputmode


        display_only /* Set frozen- just allow display of Mapping      */
            gosub'101(0%, 3%)
            goto inputmode


        REM *************************************************************~
            *               P R I N T    L I S T I N G                  *~
            * --------------------------------------------------------- *~
            * Print Listing of Maps.                                    *~
            *************************************************************
        print_listing
            call "SHOSTAT" ("Printing Maps Listing")
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            page% = 0% : line% = 857%
            select printer(134)
            call "SETPRNT" ("STC004", " ", 0%, 0%)
            plowkey$ = hex(00)
            count%   = 0%

        report_loop
            call "PLOWNEXT" (#14, plowkey$, 0%, f1%(14))
            if f1%(14) = 0% then end_report

            get #14 using L12200, map_id$, map_descr$, map%()
L12200:         FMT CH(8), CH(30), 12*BI(1)
            for f% = 1% to 12%
                gosub describe_map_element
            next f%
            prt_map$ = map_id$ : prt_descr$ = map_descr$

            for f% = 1% to buckets%
                if line% > 50% then gosub page_heading
                print using L12800, prt_map$, prt_descr$, bucket_ids$(f%),~
                                   bucket_descrs$(f%),   bucket_nrs$(f%),~
                                   map$(f%), map_ids$(f%),map_descrs$(f%)
                line% = line% + 1%
                prt_map$, prt_descr$ = " "
            next f%
            count% = count% + 1%
            print
            line% = line% + 1%
            goto report_loop

        end_report
            if count% > 0% then L12460
                call "ASKUSER" (0%, "NOTHING TO PRINT",                  ~
                                "There are no Maps to print.",  " ",     ~
                                "Press RETURN to Continue...")
                goto L12490
L12460:     print using L12820, count%
            print
            print "** END OF REPORT **"
L12490:     call "SETPRNT" ("STC004", " ", 0%, 1%)
            close printer
            goto inputmode

        page_heading
            page% = page% + 1%  :  line% = 8%
            print page
            print using L12680, date$, runtime$, company$
            print using L12700, page%
            print
            print using L12720, set$, set_descr$
            print
            print using L12740
            print using L12760
            print using L12780
            prt_class$ = map_id$ : prt_descr$ = map_descr$
            return


L12680: %RUN DATE: ######## ########             ########################~
        ~####################################              STCLBRSB:STC004
L12700: %                                                 STANDARD COSTIN~
        ~G FOLD-IN MAPPINGS LISTING                            PAGE: ##
L12720: %COST SET: ########  ##############################

L12740: %                                                 ----- STANDARD ~
        ~COST BUCKETS ------  --------  FOLD-IN MAPPING --------
L12760: %      MAP NAME  MAP DESCRIPTION                  BUCKET ID  BUCK~
        ~ET DESCRIPTION   NO  NO BUCKET ID  BUCKET DESCRIPTION
L12780: %      --------  ------------------------------   ---------- ----~
        ~---------------- --  -- ---------- --------------------
L12800: %      ########  ##############################   ########## ####~
        ~################ ##  ## ########## ####################
L12820: %#### MAPS LISTED

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

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
            f% = fieldnr% : if f% > 2% then f% = 3%
            if scrnr% = 1% then restore line = scrn1_msg, f%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Map ID.  Leave blank to see Mappings already on file.  ",~
         "Enter Map Description",                                        ~
         "Enter Bucket ID that this Bucket should fold into."

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, map_id$, map_descr$,       ~
                      map$(), map_ids$(), map_descrs$()
            mat map% = zer
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data
            get #14 using L30070, map_descr$, map%()
L30070:         FMT XX(8), CH(30), 12*BI(1)
            for f% = 1% to 12%
                gosub describe_map_element
            next f%
            return

        describe_map_element
*        Describe Map element F% defined by MAP%(F%)
          map_ids$(f%), map_descrs$(f%), map$(f%) = " "
          if map%(f%) < 1%       then return
          if map%(f%) > buckets% then return
            map_ids$   (f%) = bucket_ids$   (map%(f%))
            map_descrs$(f%) = bucket_descrs$(map%(f%))
            map$       (f%) = bucket_nrs$   (map%(f%))
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "READ101" (#14, map_id$, f1%(14))
            put #14 using L31080, map_id$, map_descr$, map%(), " "
L31080:         FMT CH(8), CH(30), 12*BI(1), CH(50)
            if f1%(14) = 0% then write #14 else rewrite #14
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
              if fieldnr% = 1% then gosub L40170    /* Map ID           */
              if fieldnr% = 2% then gosub L40160    /* Map Description  */
              if fieldnr% > 2% then gosub L40170    /* Mapping          */
              goto L40200

L40160:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Define Standard Cost Fold-In Mappings",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Fold-in Map Name",                           ~
               at (04,30), fac(lfac$( 1)), map_id$              , ch(08),~
               at (04,49), fac(hex(84))  , mod_flag$            , ch(30),~
                                                                         ~
               at (05,02), "Description",                                ~
               at (05,30), fac(lfac$( 2)), map_descr$           , ch(30),~
                                                                         ~
               at (07,02), fac(hex(ac)),   hdr$(1)              , ch(02),~
               at (07,05), fac(hex(ac)),   hdr$(2)              , ch(10),~
               at (07,16), fac(hex(ac)),   hdr$(3)              , ch(20),~
               at (07,38), fac(hex(8c)),   hdr$(4)              , ch(03),~
               at (07,42), fac(hex(ac)),   hdr$(2)              , ch(10),~
               at (07,53), fac(hex(ac)),   hdr$(3)              , ch(20),~
               at (07,74), fac(hex(ac)),   hdr$(1)              , ch(02),~
                                                                         ~
               at (08,02), fac(hex(8c)),   bucket_nrs$( 1)      , ch(02),~
               at (09,02), fac(hex(8c)),   bucket_nrs$( 2)      , ch(02),~
               at (10,02), fac(hex(8c)),   bucket_nrs$( 3)      , ch(02),~
               at (11,02), fac(hex(8c)),   bucket_nrs$( 4)      , ch(02),~
               at (12,02), fac(hex(8c)),   bucket_nrs$( 5)      , ch(02),~
               at (13,02), fac(hex(8c)),   bucket_nrs$( 6)      , ch(02),~
               at (14,02), fac(hex(8c)),   bucket_nrs$( 7)      , ch(02),~
               at (15,02), fac(hex(8c)),   bucket_nrs$( 8)      , ch(02),~
               at (16,02), fac(hex(8c)),   bucket_nrs$( 9)      , ch(02),~
               at (17,02), fac(hex(8c)),   bucket_nrs$(10)      , ch(02),~
               at (18,02), fac(hex(8c)),   bucket_nrs$(11)      , ch(02),~
               at (19,02), fac(hex(8c)),   bucket_nrs$(12)      , ch(02),~
                                                                         ~
               at (08,05), fac(hex(8c)),   bucket_ids$( 1)      , ch(10),~
               at (09,05), fac(hex(8c)),   bucket_ids$( 2)      , ch(10),~
               at (10,05), fac(hex(8c)),   bucket_ids$( 3)      , ch(10),~
               at (11,05), fac(hex(8c)),   bucket_ids$( 4)      , ch(10),~
               at (12,05), fac(hex(8c)),   bucket_ids$( 5)      , ch(10),~
               at (13,05), fac(hex(8c)),   bucket_ids$( 6)      , ch(10),~
               at (14,05), fac(hex(8c)),   bucket_ids$( 7)      , ch(10),~
               at (15,05), fac(hex(8c)),   bucket_ids$( 8)      , ch(10),~
               at (16,05), fac(hex(8c)),   bucket_ids$( 9)      , ch(10),~
               at (17,05), fac(hex(8c)),   bucket_ids$(10)      , ch(10),~
               at (18,05), fac(hex(8c)),   bucket_ids$(11)      , ch(10),~
               at (19,05), fac(hex(8c)),   bucket_ids$(12)      , ch(10),~
                                                                         ~
               at (08,16), fac(hex(8c)),   bucket_descrs$( 1)   , ch(20),~
               at (09,16), fac(hex(8c)),   bucket_descrs$( 2)   , ch(20),~
               at (10,16), fac(hex(8c)),   bucket_descrs$( 3)   , ch(20),~
               at (11,16), fac(hex(8c)),   bucket_descrs$( 4)   , ch(20),~
               at (12,16), fac(hex(8c)),   bucket_descrs$( 5)   , ch(20),~
               at (13,16), fac(hex(8c)),   bucket_descrs$( 6)   , ch(20),~
               at (14,16), fac(hex(8c)),   bucket_descrs$( 7)   , ch(20),~
               at (15,16), fac(hex(8c)),   bucket_descrs$( 8)   , ch(20),~
               at (16,16), fac(hex(8c)),   bucket_descrs$( 9)   , ch(20),~
               at (17,16), fac(hex(8c)),   bucket_descrs$(10)   , ch(20),~
               at (18,16), fac(hex(8c)),   bucket_descrs$(11)   , ch(20),~
               at (19,16), fac(hex(8c)),   bucket_descrs$(12)   , ch(20),~
                                                                         ~
               at (08,42), fac(lfac$( 3)), map_ids$( 1)         , ch(10),~
               at (09,42), fac(lfac$( 4)), map_ids$( 2)         , ch(10),~
               at (10,42), fac(lfac$( 5)), map_ids$( 3)         , ch(10),~
               at (11,42), fac(lfac$( 6)), map_ids$( 4)         , ch(10),~
               at (12,42), fac(lfac$( 7)), map_ids$( 5)         , ch(10),~
               at (13,42), fac(lfac$( 8)), map_ids$( 6)         , ch(10),~
               at (14,42), fac(lfac$( 9)), map_ids$( 7)         , ch(10),~
               at (15,42), fac(lfac$(10)), map_ids$( 8)         , ch(10),~
               at (16,42), fac(lfac$(11)), map_ids$( 9)         , ch(10),~
               at (17,42), fac(lfac$(12)), map_ids$(10)         , ch(10),~
               at (18,42), fac(lfac$(13)), map_ids$(11)         , ch(10),~
               at (19,42), fac(lfac$(14)), map_ids$(12)         , ch(10),~
                                                                         ~
               at (08,53), fac(hex(8c)),   map_descrs$( 1)      , ch(20),~
               at (09,53), fac(hex(8c)),   map_descrs$( 2)      , ch(20),~
               at (10,53), fac(hex(8c)),   map_descrs$( 3)      , ch(20),~
               at (11,53), fac(hex(8c)),   map_descrs$( 4)      , ch(20),~
               at (12,53), fac(hex(8c)),   map_descrs$( 5)      , ch(20),~
               at (13,53), fac(hex(8c)),   map_descrs$( 6)      , ch(20),~
               at (14,53), fac(hex(8c)),   map_descrs$( 7)      , ch(20),~
               at (15,53), fac(hex(8c)),   map_descrs$( 8)      , ch(20),~
               at (16,53), fac(hex(8c)),   map_descrs$( 9)      , ch(20),~
               at (17,53), fac(hex(8c)),   map_descrs$(10)      , ch(20),~
               at (18,53), fac(hex(8c)),   map_descrs$(11)      , ch(20),~
               at (19,53), fac(hex(8c)),   map_descrs$(12)      , ch(20),~
                                                                         ~
               at (08,74), fac(hex(8c)),   map$( 1)             , ch(02),~
               at (09,74), fac(hex(8c)),   map$( 2)             , ch(02),~
               at (10,74), fac(hex(8c)),   map$( 3)             , ch(02),~
               at (11,74), fac(hex(8c)),   map$( 4)             , ch(02),~
               at (12,74), fac(hex(8c)),   map$( 5)             , ch(02),~
               at (13,74), fac(hex(8c)),   map$( 6)             , ch(02),~
               at (14,74), fac(hex(8c)),   map$( 7)             , ch(02),~
               at (15,74), fac(hex(8c)),   map$( 8)             , ch(02),~
               at (16,74), fac(hex(8c)),   map$( 9)             , ch(02),~
               at (17,74), fac(hex(8c)),   map$(10)             , ch(02),~
               at (18,74), fac(hex(8c)),   map$(11)             , ch(02),~
               at (19,74), fac(hex(8c)),   map$(12)             , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41300
                  call "MANUAL" ("STCMAPSB") : goto L40200

L41300:        if keyhit% <> 15 then L41330
                  call "PRNTSCRN" : goto L40200

L41330:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 3% then L41730
        if edit% = 2% then L41540     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "   (14)Print Listing   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Routine"
            pfkeys$ = hex(01ffff04ffffffffffffffff0d0e0f1000)
            if fieldnr% = 1% then L41500
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                str(pf$(2),44,18) = " "  :  str(pfkeys$,14,1) = hex(ff)
L41500:     if fieldnr% > 2% then L41520
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41520:     return

L41540: if fieldnr% > 0% then L41630  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "              (12)Delete Mapping        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Mapping"
            pfkeys$ = hex(01ffffffffffffffffffff0c0dff0f1000)
            return
L41630:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L41730:     pf$() = " "              /*  Display Only           */
            pfkeys$ = hex(0102030405060708090a0b0c0d0e0f1000)
            inpmessage$ = "Press RETURN to Continue..."
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            if fieldnr% = 1% then L50130      /* Cost Bucket       */
            if fieldnr% = 2% then L50270      /* Map Description   */
            if fieldnr% > 2% then L50300      /* Mapping           */
            return

L50130
*        Test for MAP ID                       MAP_ID$
            if map_id$ > " "                                             ~
                then call "READ100" (#14, map_id$, f1%(14))              ~
                else call "GETCODE" (#14, map_id$, map_descr$, 0%, 0,    ~
                                                                 f1%(14))
            if map_id$ = " " then errormsg$ = hex(00)
            if mod_flag$ > " " and f1%(14) = 0% then                     ~
                                         errormsg$ = "Map ID Not on File"
            if f1%(14) = 0%  then return
                gosub load_data
                return clear all
                goto editmode
            return

L50270
*        Test for MAP DESCRIPTION              MAP_DESCR$
            return

L50300
*        Test for MAPPING                      MAP_IDS$()
            f% = fieldnr% - 2%
            if map_ids$(f%) = " " then map_ids$(f%) = bucket_ids$(f%)
            search str(bucket_ids$()) = map_ids$(f%) to p%() step 10%
            if p%(1) > 0% then L50370
                errormsg$ = "Invalid Bucket ID"
                return
L50370:     map%(f%) = 1% + ((p%(1) - 1%) / 10%)
            gosub describe_map_element
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
            end
