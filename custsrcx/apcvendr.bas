        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                  ( OLD VERSION - NOW APCVENDR ) 11/07/97  *~
            *  V   V  PPPP   RRRR   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  V   V  P   P  R   R    I    NN  N  P   P  U   U    T     *~
            *  V   V  PPPP   RRRR     I    N N N  PPPP   U   U    T     *~
            *   V V   P      R   R    I    N  NN  P      U   U    T     *~
            *    V    P      R   R  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VPRINPUT - Manages Vendor Part Number & Prices by Vendor  *~
            *            as opposed to by Part (Our Part).  Primary use *~
            *            intended for updating said information from a  *~
            *            Vendor's Price Catalogue as the source         *~
            *            document.  Allows tracking of current vendor   *~
            *            prices with keys to find all vendors that carry*~
            *            each of our parts with their current price and *~
            *            Leadtime.                                      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/24/83 ! ORIGINAL                                 ! GLW *~
            * 04/12/84 ! ADDED UNIT OF MEASURE STUFF              ! HES *~
            * 10/04/85 ! Changed VENDOR File Format               ! MJB *~
            * 05/05/86 ! Original (Total Rewrite of Program)      ! LDJ *~
            *          !   Changed File Layout (Next Price fields)!     *~
            *          !   and screens changed.                   !     *~
            * 06/09/86 ! Remarked out edit to allow entry of      ! LDJ *~
            *          !   manufactured parts (types 500 & above).!     *~
            * 08/28/86 ! PRR# 5290 - 7 DEC PLACES FOR VEN PRICE   ! LKM *~
            * 10/11/86 ! PRR# 5244 - Added Report Generation By   ! LDJ *~
            *          !   by Vendor or By Part. (P.S. requires   !     *~
            *          !   PLOWCODE version 04.17.01 for report   !     *~
            *          !   generation.)                           !     *~
            * 10/29/86 ! PRR # 5449 - "... By Part" Screen showed ! LDJ *~
            *          !   Part Description for the Vendor's Name.!     *~
            * 04/28/88 ! Added Calc. ability for CURRENT COST     ! TLJ *~
            * 06/09/88 ! 1. Fix conv factor losing precision      ! ERN *~
            *          ! 2. Fix division by zero error            !     *~
            * 11/08/88 ! Allow 4 decimal places                   ! KAB *~
            *          !                                          !     *~
            * 11/07/97 ! Revision Update For 60403                ! DJD *~
            *          !                                          !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date_effective$8,            /* Date 'Next Price' Effective*/~
            date_expires$8,              /* Date 'Next Price' Expires  */~
            descr_map(20),               /* Plowcode argument          */~
            discper$4,                   /* Discount Percent           */~
            disc$10,                     /* Discount                   */~
            eachmsg$30,                  /* Converted Price (Current)  */~
            eachmsg2$30,                 /* Converted Price (Current)  */~
            eachprice$10,                /* Converted Price (Current)  */~
            eachprice2$10,               /* Converted Price (Next   )  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$(2)133,                  /* Column Header Lines        */~
            i$(24)80,                    /* Screen Image               */~
            incl(1),                     /* Dummy PLOWCODE Argument    */~
            incl$(1)1,                   /* Dummy PLOWCODE Argument    */~
            inpmessage$79,               /* Informational Message      */~
            last_part$25,                /* Last Part Processed        */~
            last_vendor$9,               /* Last Vendor Processed      */~
            last_venpart$25,             /* Last Vendor Part Processed */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            list$10,                     /* List Price                 */~
            miscdescr$99,                /* Miscellaneous Description  */~
            nextprice$10,                /* Next or 'Special' Price    */~
            only_one$1,                  /* Only One Record? Y,N, or X */~
            oldvenppu$10,                /* Current Price - Vendor UOM */~
            option$40,                   /* PF Options Header          */~
            or$4,                        /* ACCEPT Message for disc %  */~
            part$25,                     /* Our Part Number            */~
            partdescr$32,                /* Our Part Number            */~
            pf$6,                        /* PF Keys Header             */~
            pf12$16,                     /* PF 12 Screen Literal       */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            pricdate$8,                  /* Date Last Changed          */~
            purunit$4,                   /* HNYMASTR Purchase U.O.M.   */~
            quan$10,                     /* Our Quantity Per Vendor UOM*/~
            unit$4,                      /* Vendor Unit of Measure     */~
            userid$3,                    /* Current User Id            */~
            venalt$3,                    /* Vendor's Average Leadtime  */~
            vencode$9,                   /* Vendor Code                */~
            vencodedescr$32,             /* Vendor Code                */~
            venpart$25,                  /* Vendor's Part Number       */~
            venppu$10,                   /* Current Price - Vendor UOM */~
            venslt$3                     /* Vendor's Stated Leadtime   */~

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
            cms2v$ = "R6.04.03 11/07/97 Patch Release                   "
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
            * #1  ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #2  ! VENDOR   ! Vendor Master File                       *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! GENCODES ! General Codes File                       *~
            * #5  ! HNYPROC  ! Inventory Procurements File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34          ~

            select #2,  "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup     ~

            select #3,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #5,  "HNYPROC",                                       ~
                        varc,     indexed,  recsize =  134,              ~
                        keypos =   32, keylen =  40,                     ~
                        alt key  1, keypos =    7, keylen =  65,         ~
                            key  2, keypos =    1, keylen =  40, dup,    ~
                            key  3, keypos =   41, keylen =  31, dup     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))

            if min(fs%()) < 0% then exit_program

            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 1%, rslt$(1 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            plowkey$ = all(hex(00))              /* If UOM file set up */
            str(plowkey$,10) = "UOM"             /* then edit entries. */
            call "READ100" (#4, plowkey$, uom%)
            pf$ = "PF Keys" : option$ = "Options"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        main_screen
            init(" ") errormsg$, inpmessage$, vencode$, vencodedescr$,   ~
                      part$, partdescr$

            for field% = 1% to 1%
                gosub'051(field%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L11030
                gosub'101(field%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       exit_program
                      if keyhit%  =  8 then gosub find_by_vendor
                      if keyhit%  =  9 then gosub find_by_part
                      if keyhit%  = 10 then gosub input_by_vendor
                      if keyhit%  = 11 then gosub input_by_part
                      if keyhit%  = 14 then gosub print_by_vendor
                      if keyhit%  = 30 then gosub print_by_part
                field% = field% - 1%
                errormsg$ = " "
            next field%

        find_by_vendor
            plowkey$ = vencode$
            incl(1) = 0
            hdr$(1) = "  Vendor    Vendor Name"
            call "PLOWCODE" (#1, plowkey$, vencodedescr$, -8009%, -2.3,  ~
                    f1%(1), hdr$(), 0,0, incl(), incl$(),"Y"," ",#2)
            if f1%(1) = 0% then return
            vencode$ = plowkey$
L10340:     part$, disc$, discper$, list$, or$ = " "
            call "VPRVSLCT" (part$,vencode$,venpart$,2%,only_one$,#1,#2, ~
                             #3, #5)
            plowkey$ = str(part$) & str(vencode$) & venpart$
            call "READ101" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then return
            gosub dataload
            add% = 0%
            gosub edtpg2
            if only_one$ <> "N" then return
            goto L10340

        find_by_part
            incl(1) = 0
            hdr$(1) = "  Part Code                     Description"
            plowkey$ = part$
            call "PLOWCODE" (#1, plowkey$, partdescr$, -8025%, -.32,     ~
                    f1%(1), hdr$(), 0,0, incl(), incl$(),"Y"," ",#3)
            if f1%(1) = 0% then return
            part$ = plowkey$
L10540:     vencode$, disc$, discper$, or$, list$ = " "
            call "VPRPSLCT" (part$,vencode$,venpart$,2%,only_one$,#1,#3, ~
                             #2, #5)
            plowkey$ = str(part$) & str(vencode$) & venpart$
            call "READ101" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then return
            gosub dataload
            add% = 0%
            gosub edtpg3
            if only_one$ <> "N" then return
            goto L10540

        input_by_vendor
            add% = 1%
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Return"
            pf12$ = " "
            last_vendor$ = vencode$
            last_venpart$ = venpart$
            list, disc = 0
            init(" ") errormsg$, inpmessage$, purunit$, or$,             ~
                      vencodedescr$               ,/* Vendor Code Descr*/~
                      venpart$                    ,/* Vendor's Part Num*/~
                      part$, partdescr$           ,/* Our Part Number  */~
                      venslt$                     ,/* Stated Leadtime  */~
                      venalt$                     ,/* Average Leadtime */~
                      pricdate$                   ,/* Date Last Changed*/~
                      unit$                       ,/* Vendor U.O.M.    */~
                      quan$                       ,/* Conversion Factor*/~
                      list$                       ,/* Discount         */~
                      discper$, disc$             ,/* Discount Percent */~
                      venppu$                     ,/* Current Price    */~
                      nextprice$                  ,/* Next or 'Special'*/~
                      date_effective$             ,/* Date Effective   */~
                      date_expires$               ,/* Date Expires     */~
                      eachmsg$, eachmsg2$          /* Each Prices      */

            for fieldnr% = 1 to 14
L10910:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L11030
L10930:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L11010
L10960:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10930
                         if fieldnr% = 1% then L10910
                         goto L10960
L11010:               if keyhit%  = 16 and fieldnr% < 3 then return
                      if keyhit% <>  0 then       L10930
L11030:         gosub'152(fieldnr%,1%)   /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10930
            next fieldnr%
            goto edtpg2

        input_by_part
            add% = 2%
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Return"
            pf12$ = " "
            last_vendor$ = vencode$
            last_part$ = part$
            list, disc = 0
            init(" ") errormsg$, inpmessage$, purunit$, or$,             ~
                      vencode$, vencodedescr$     ,/* Vendor Code      */~
                      venpart$                    ,/* Vendor's Part Num*/~
                      partdescr$                  ,/* Our Part Number  */~
                      venslt$                     ,/* Stated Leadtime  */~
                      venalt$                     ,/* Average Leadtime */~
                      pricdate$                   ,/* Date Last Changed*/~
                      unit$                       ,/* Vendor U.O.M.    */~
                      quan$                       ,/* Conversion Factor*/~
                      list$                       ,/* List             */~
                      discper$, disc$             ,/* Discount         */~
                      venppu$                     ,/* Current Price    */~
                      nextprice$                  ,/* Next or 'Special'*/~
                      date_effective$             ,/* Date Effective   */~
                      date_expires$               ,/* Date Expires     */~
                      eachmsg$, eachmsg2$          /* Each Prices      */

            for fieldnr% = 1 to 14
L11330:         gosub'053(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L11450
L11350:         gosub'103(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L11430
L11380:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L11350
                         if fieldnr% = 1% then L11330
                         goto L11380
L11430:               if keyhit%  = 16 and fieldnr% < 3 then return
                      if keyhit% <>  0 then       L11350
L11450:         gosub'153(fieldnr%,1%)  /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11350
            next fieldnr%
            goto edtpg3

        print_by_vendor
            plowkey$ = vencode$
            incl(1) = 0  : break% = 9000%
            if plowkey$ = " " then L11590           /* All Vendors      */
            hdr$(1) = "  Vendor    Vendor Name"
            call "PLOWCODE" (#1, plowkey$, vencodedescr$, -8009%, -2.3,  ~
                    f1%(1), hdr$(), 0,0, incl(), incl$(),"Y"," ",#2)
            if f1%(1) = 0% then return
            vencode$ = plowkey$ : break% = 9009%
L11590:     hdr$(1) = "Vendor    Vendor's Part Number      Our Part Numbe~
        ~r           Part Description               Vendor Price Ven Unit ~
        ~Our Unit  Our Qty"
            descr_map(1) = 01.09 : descr_map(2) = 01.00
            descr_map(3) = 44.25 : descr_map(4) = 11.00
            descr_map(5) = 10.25 : descr_map(6) = 37.00
            descr_map(7) =-26.32 : descr_map(8) = 63.00
            descr_map(9) = 69.08 : descr_map(10)= 96.1052
            descr_map(11)= 89.04 : descr_map(12)=109.000
            descr_map(13)=-78.04 : descr_map(14)=118.000
            descr_map(15)= 93.08 : descr_map(16)=123.104
            miscdescr$ = hex(06) & "Vendor Price List By Vendor"
            call "PLOWCODE" (#1, plowkey$, miscdescr$, break%, 2.30,     ~
            f1%(1), hdr$(),0,-10, incl(), incl$(),"r"," ",#3,descr_map())
            return

        print_by_part
            plowkey$ = part$
            incl(1) = 0  : break% = 9000%
            if plowkey$ = " " then L11840           /* All Parts        */
            hdr$(1) = "  Part Code                     Description"
            call "PLOWCODE" (#1, plowkey$, vencodedescr$, -8025%, -.32,  ~
                    f1%(1), hdr$(), 0,0, incl(), incl$(),"Y"," ",#3)
            if f1%(1) = 0% then return
            part$ = plowkey$ : break% = 9025%
L11840:     hdr$(1) = "Our Part Number           Vendor    Vendor's Part ~
        ~Number      Part Description               Vendor Price Ven Unit ~
        ~Our Unit  Our Qty"
            descr_map(1) = 01.09 : descr_map(2) = 27.00
            descr_map(3) = 44.25 : descr_map(4) = 37.00
            descr_map(5) = 10.25 : descr_map(6) = 01.00
            descr_map(7) =-26.32 : descr_map(8) = 63.00
            descr_map(9) = 69.08 : descr_map(10)= 96.1052
            descr_map(11)= 89.04 : descr_map(12)=109.000
            descr_map(13)=-78.04 : descr_map(14)=118.000
            descr_map(15)= 93.08 : descr_map(16)=123.104
            miscdescr$ = hex(06) & "Vendor Price List By Part"
            call "PLOWCODE" (#1, plowkey$, miscdescr$, break%, 0.30,     ~
            f1%(1), hdr$(),0,-10, incl(), incl$(),"r"," ",#3,descr_map())
            return

        REM *************************************************************~
            *        E D I T   M O D E   S C R E E N   2                *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screen 2.   *~
            *************************************************************

        edtpg2
            pf4$  = " "
            pf5$  = " "
            pf12$ = "(12)Delete Data"
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then       datakill
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg2
L12170:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 4 or fieldnr% > 14 then edtpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg2
                  pf4$, pf5$, pf16$ = " "
L12220:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12220
            gosub'152(fieldnr%,2%)       /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12220
                  if cursor%(1) - 5% <> fieldnr% then L12170
            goto edtpg2


        REM *************************************************************~
            *        E D I T   M O D E   S C R E E N   3                *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screen 3.   *~
            *************************************************************

        edtpg3
            pf4$  = " "
            pf5$  = " "
            pf12$ = "(12)Delete Data"
            pf16$ = "(16)Save Data"
            inpmessage$ = edtmessage$
            gosub'103(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then       datakill
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg3
L13170:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 4 or fieldnr% > 14 then edtpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg3
                  pf4$, pf5$, pf16$ = " "
L13220:     gosub'103(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13220
            gosub'153(fieldnr%,2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13220
                  if cursor%(1) - 5% <> fieldnr% then L13170
            goto edtpg3

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            on add% goto input_by_vendor, input_by_part
            return

        datakill
            keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** DELETE RECORD? ***",           ~
                "Press RETURN to Delete the current record", "- OR -",   ~
                "Press PF1 to CANCEL the Delete Function.")
            if keyhit% = 1% then L19200
            if keyhit% <> 0% then datakill
            plowkey$ = str(part$) & str(vencode$) & venpart$
            call "DELETE" (#1, plowkey$, 59%)
L19200:     on add% goto input_by_vendor, input_by_part
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  inpmessage$ = "Edit Existing Entries via PF8 or 9, " & ~
                                "Use PF11 or 12 to Add new Entries"
                  return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************


            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21240,         /* Vendor Code      */~
                                    L21280,         /* Vendor's Part Num*/~
                                    L21320,         /* Our Part Number  */~
                                    L21370,         /* Stated Leadtime  */~
                                    L21410,         /* Average Leadtime */~
                                    L21460,         /* Date Last Changed*/~
                                    L21500,         /* Vendor U.O.M.    */~
                                    L21550,         /* Conversion Factor*/~
                                    L21630,         /* List price       */~
                                    L21660,         /* Discount         */~
                                    L21740,         /* Current Price    */~
                                    L21800,         /* Next or 'Special'*/~
                                    L21860,         /* Date Effective   */~
                                    L21900          /* Date Expires     */
                     return
L21240:     REM Default/Enable for Vendor Code
                inpmessage$ = "Enter a Vendor Code or blank to Find"
                if vencode$ > " " and keyhit% <> 4% then enabled% = 0%
                return
L21280:     REM Default/Enable for Vendor's Part Number
                inpmessage$ = "Enter a Vendor Part Number or blank to " &~
                              "find an existing one"
                return
L21320:     REM Default/Enable for Our Part Number
                inpmessage$ = "Enter our Part Number for this Vendor's" &~
                              " part (blank to find an existing one)."
                if part$ > " " and keyhit% <> 4% then enabled% = 0%
                return
L21370:     REM Default/Enable for Vendor's Stated Leadtime
                inpmessage$ = "Enter the Vendor's Stated Leadtime for " &~
                              "this part"
                return
L21410:     REM Default/Enable for Vendor's Average Leadtime
                inpmessage$ = "Enter the Vendor's Average Leadtime for " ~
                            & "this part"
                if venalt$ = " " then venalt$ = venslt$
                return
L21460:     REM Default/Enable for Date Last Changed
                pricdate$ = date$
                inpmessage$ = " "
                return
L21500:     REM Default/Enable for Vendor Unit of Measure
                inpmessage$ = "Enter the Unit of Measure we purchase " & ~
                              "this part in from this Vendor"
                if unit$ = " " then unit$ = purunit$
                return
L21550:     REM Default/Enable for Our Quantity Per Vendor UOM
                inpmessage$ ="Enter the number of our " & purunit$ & "'s"~
                           & " that go into the Vendor's " & unit$ & "'s"
                if unit$ <> purunit$ or quan$ <> " " then return
                   quan = 1
                   enabled% = 0%
                   call "CONVERT" (quan,-0.4, quan$)
                   return
L21630
*        Default/Enable List Price           LIST$
                if disc$ <> " " then or$ = " "
                if discper$ <> " " then or$ = "%   "
                inpmessage$ = "Enter the List Price"
                return
L21660
*        Default/Enable for Discount% or Fixed Discount Amount
                if list$ <> " " then L21700
                   enabled% = 0%
                   return
L21700:         inpmessage$ = "Enter the Discount Percent or the"      & ~
                              " Fixed Dollar Amount to be discounted."
                or$ = "% OR"
                return
L21740
*        Default/Enable for Current Price - Vendor UOM
                oldvenppu$ = venppu$                
                if venppu$>" " then call "CONVERT" (venppu,-2.5, venppu$)
                inpmessage$ = "Enter the Vendor's Price for this Part " &~
                              "(Unit Price per Vendor U.O.M.)"
                return
L21800:     REM Default/Enable for Next or 'Special' Price
                inpmessage$ = "Enter the 'Special' (Contract) "        & ~
                              "Price for this Part (Optional)."
                if nextprice$ > " " then                                 ~
                   call "CONVERT" (nextprice,-2.4,nextprice$)
                return
L21860:     REM Default/Enable for Date 'Next Price' Effective
                inpmessage$ = "Enter the Beginning Date that 'Special " &~
                    "Price' will override the Current Price"
                return
L21900:     REM Default/Enable for Date 'Next Price' Expires
                inpmessage$ = "Enter the Ending Date that 'Special Price"~
                           & "' will override the Current Price"
                if date_effective$ = " " then enabled% = 0%
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************


            deffn'053(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21320,         /* Our Part Number  */~
                                    L21240,         /* Vendor Code      */~
                                    L21280,         /* Vendor's Part Num*/~
                                    L21370,         /* Stated Leadtime  */~
                                    L21410,         /* Average Leadtime */~
                                    L21460,         /* Date Last Changed*/~
                                    L21500,         /* Vendor U.O.M.    */~
                                    L21550,         /* Conversion Factor*/~
                                    L21630,         /* Current List     */~
                                    L21660,         /* Discount         */~
                                    L21740,         /* Current Price    */~
                                    L21800,         /* Next or 'Special'*/~
                                    L21860,         /* Date Effective   */~
                                    L21900          /* Date Expires     */
                     return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            if u3% <> 0% then startover
            return clear all
            goto main_screen

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            plowkey$ = key(#1)
            call "READ101" (#1, plowkey$, f1%(1))
            if f1%(1) = 0% then return

            get #1 using L35030,  /* FILE: VENPRICE                     */~
            vencode$,       /* Vendor Code                             */~
            part$,          /* Part code                               */~
            vencode$,       /* Vendor Code                             */~
            venpart$,       /* vendor part number                      */~
            venppu,         /* Vendor's current price per unit         */~
            pricdate$,      /* Date vendor price last updated          */~
            venslt$,        /* Vendor's stated leadtime                */~
            venalt$,        /* Vendor's average leadtime               */~
            unit$,          /* Unit of Measure                         */~
            quan,           /* Quantity per unit of measure            */~
            nextprice,      /* Vendor's Next or Special (Contract) Pric*/~
            date_effective$,/* Date something becomes Effective        */~
            date_expires$   /* Date 'something' expires                */

            call "DATEFMT" (pricdate$)
            call "DATEFMT" (date_effective$)
            call "DATEFMT" (date_expires$)
            if date_effective$ > " " then                                ~
               call "DATEOK" (date_effective$,efdate%, errormsg$)
            call "CONVERT" (venppu, 2.5, venppu$)      
            call "CONVERT" (quan, 0.4,quan$)
            call "CONVERT" (nextprice, 2.4, nextprice$)
            call "GETCODE" (#3, part$, partdescr$, 0%, 20, f1%(3))
            if f1%(3) = 1% then get #3 using L30410, purunit$
            call "DESCRIBE" (#2, vencode$, vencodedescr$, 0%, f1%(2))
            gosub price_each_message
            gosub price_each2_message

            return

L30410:     FMT POS(78), CH(4)


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "DATUNFMT" (pricdate$)
            call "DATUNFMT" (date_effective$)
            call "DATUNFMT" (date_expires$)
            venppu = round(venppu,5)       
            quan = round(quan,4)
            nextprice = round(nextprice,4)

            put #1 using L35030,  /* FILE: VENPRICE                     */~
            vencode$,       /* Vendor Code                             */~
            part$,          /* Part code                               */~
            vencode$,       /* Vendor Code                             */~
            venpart$,       /* vendor part number                      */~
            venppu,         /* Vendor's current price per unit         */~
            pricdate$,      /* Date vendor price last updated          */~
            venslt$,        /* Vendor's stated leadtime                */~
            venalt$,        /* Vendor's average leadtime               */~
            unit$,          /* Unit of Measure                         */~
            quan,           /* Quantity per unit of measure            */~
            nextprice,      /* Vendor's Next or Special (Contract) Pric*/~
            date_effective$,/* Date something becomes Effective        */~
            date_expires$,  /* Date 'something' expires                */~
            date,           /* The system date a file or record was las*/~
            userid$,        /* User ID of Last Modification            */~
            " "             /* Filler For Rest of Record or Internal Sp*/~

            if f1%(1) = 1% then rewrite #1
            if f1%(1) = 0% then write #1
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: VENPRICE                          */~
            CH(9),          /* Vendor Code                             */~
            CH(25),         /* Part code                               */~
            CH(9),          /* Vendor Code                             */~
            CH(25),         /* vendor part number                      */~
            PD(15,5),       /* Vendor's current price per unit         */~
            CH(6),          /* Date vendor price last updated          */~
            CH(3),          /* Vendor's stated leadtime                */~
            CH(3),          /* Vendor's average leadtime               */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,4),       /* Quantity per unit of measure            */~
            PD(14,4),       /* Vendor's Next or Special (Contract) Pric*/~
            CH(6),          /* Date something becomes Effective        */~
            CH(6),          /* Date 'something' expires                */~
            CH(6),          /* The system date a file or record was las*/~
            CH(3),          /* User ID of Last Modification            */~
            CH(127)         /* Filler For Rest of Record or Internal Sp*/~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$ = " "
                  str(line2$,63%) = "VPRINPUT:" & str(cms2v$,,8%)
                  pf16$ = "(16)Exit Program"

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Maintain Vendor Price Catalogue",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,22), fac(hex(ac)), pf$                    , ch(06),~
               at (06,30), fac(hex(ac)), option$                , ch(40),~
               at (07,23), "(8)",                                        ~
               at (07,30), "Find By Vendor:",                            ~
               at (07,46), fac(hex(81)),   vencode$             , ch(09),~
               at (08,23), "(9)",                                        ~
               at (08,30), "Find By Part  :",                            ~
               at (08,46), fac(hex(81)),   part$                , ch(25),~
               at (10,22), "(10)",                                       ~
               at (10,30), "Add Entries by Vendor",                      ~
               at (11,22), "(11)",                                       ~
               at (11,30), "Add Entries by Part",                        ~
               at (13,22), "(14)",                                       ~
               at (13,30), "Print Price List by Vendor",                 ~
               at (14,22), "(30)",                                       ~
               at (14,30), "Print Price List by Part",                   ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), "(8)Find By Vendor"                          ,~
               at (23,38), "(10)Add Entries By Vendor"                  ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,20), "(9)Find By Part  "                          ,~
               at (24,38), "(11)Add Entries By Part"                    ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000108090a0b0d0e0f101e)),                        ~
               key (keyhit%)

               if keyhit% <> 13 then L40550
                  call "MANUAL" ("VPRINPUT")
                  goto L40110

L40550:        if keyhit% <> 15 then L40590
                  call "PRNTSCRN"
                  goto L40110

L40590:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return


        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'102(fieldnr%)
                  if last_venpart$ = " " then line2$ = " " else          ~
                  line2$ = "Last Vendor: " & last_vendor$ &              ~
                           " Vendor P/N: " & last_venpart$
                  str(line2$,63%) = "VPRINPUT:" & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L42340,         /* Vendor Code      */~
                                    L42340,         /* Vendor's Part Num*/~
                                    L42340,         /* Our Part Number  */~
                                    L42370,         /* Stated Leadtime  */~
                                    L42370,         /* Average Leadtime */~
                                    L42340,         /* Date Last Changed*/~
                                    L42340,         /* Vendor U.O.M.    */~
                                    L42370,         /* Conversion Factor*/~
                                    L42370,         /* Current List     */~
                                    L42370,         /* Discount         */~
                                    L42370,         /* Current Price    */~
                                    L42370,         /* Next or 'Special'*/~
                                    L42340,         /* Date Effective   */~
                                    L42340          /* Date Expires     */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L42410

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L42340:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L42370:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L42410:     accept                                                       ~
               at (01,02),                                               ~
                  "Maintain Vendor Price Catalogue By Vendor",           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Vendor Code ",                                        ~
               at (06,30), fac(lfac$( 1)), vencode$             , ch(09),~
               at (06,49), fac(hex(8c)),   vencodedescr$        , ch(32),~
               at (07,02),                                               ~
                  "Vendor's Part Number",                                ~
               at (07,30), fac(lfac$( 2)), venpart$             , ch(25),~
               at (08,02),                                               ~
                  "Our Part Number",                                     ~
               at (08,20), fac(lfac$( 3)), part$                , ch(25),~
               at (08,49), fac(hex(8c)),   partdescr$           , ch(32),~
               at (09,02),                                               ~
                  "Vendor's Stated Leadtime",                            ~
               at (09,30), fac(lfac$( 4)), venslt$              , ch(03),~
               at (10,02),                                               ~
                  "Vendor's Average Leadtime",                           ~
               at (10,30), fac(lfac$( 5)), venalt$              , ch(03),~
               at (11,02),                                               ~
                  "Date Last Changed",                                   ~
               at (11,30), fac(lfac$( 6)), pricdate$            , ch(08),~
               at (12,02),                                               ~
                  "Vendor Unit of Measure",                              ~
               at (12,30), fac(lfac$( 7)), unit$                , ch(04),~
               at (12,49), "(Stock UOM =      )",                        ~
               at (12,62), fac(hex(8c)),   purunit$             , ch(04),~
               at (13,02),                                               ~
                  "Our Quantity Per Vendor UOM",                         ~
               at (13,30), fac(lfac$( 8)), quan$                , ch(10),~
               at (14,02),                                               ~
                  "Current List Price",                                  ~
               at (14,30), fac(lfac$( 9)), list$                , ch(10),~
               at (15,02),                                               ~
                  "Discount % or Fixed Amount",                          ~
               at (15,30), fac(lfac$(10)), discper$             , ch(04),~
               at (15,35), fac(hex(8c)),   or$                  , ch(04),~
               at (15,40), fac(lfac$(10)), disc$                , ch(10),~
               at (16,02),                                               ~
                  "Current Price - Vendor UOM",                          ~
               at (16,30), fac(lfac$(11)), venppu$              , ch(10),~
               at (16,49), fac(hex(8c))  , eachmsg$             , ch(30),~
               at (17,02),                                               ~
                  "Contract or 'Special' Price",                         ~
               at (17,30), fac(lfac$(12)), nextprice$           , ch(10),~
               at (17,49), fac(hex(8c))  , eachmsg2$            , ch(30),~
               at (18,02),                                               ~
                  "Date 'Special' Price Starts",                         ~
               at (18,30), fac(lfac$(13)), date_effective$      , ch(08),~
               at (19,02),                                               ~
                  "Date 'Special' Price Ends",                           ~
               at (19,30), fac(lfac$(14)), date_expires$        , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,20), fac(hex(84)), pf12$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050c0d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L43170
                  call "MANUAL" ("VPRINPUT")
                  goto L42410

L43170:        if keyhit% <> 15 then L43210
                  call "PRNTSCRN"
                  goto L42410

L43210:        if pf4$ > " " then return           /* Input Mode       */
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen 3.                         *~
            *************************************************************

            deffn'103(fieldnr%)
                  if last_part$ = " " then line2$ = " " else             ~
                  line2$ = "Last Vendor: " & last_vendor$ &              ~
                           " Part Code: " & last_part$
                  str(line2$,63%) = "VPRINPUT:" & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L44340,         /* Our Part Number  */~
                                    L44340,         /* Vendor Code      */~
                                    L44340,         /* Vendor's Part Num*/~
                                    L44370,         /* Stated Leadtime  */~
                                    L44370,         /* Average Leadtime */~
                                    L44340,         /* Date Last Changed*/~
                                    L44340,         /* Vendor U.O.M.    */~
                                    L44370,         /* Conversion Factor*/~
                                    L44370,         /* Current List     */~
                                    L44370,         /* Discount         */~
                                    L44370,         /* Current Price    */~
                                    L44370,         /* Next or 'Special'*/~
                                    L44340,         /* Date Effective   */~
                                    L44340          /* Date Expires     */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L44410

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L44340:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L44370:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L44410:     accept                                                       ~
               at (01,02),                                               ~
                  "Maintain Vendor Price Catalogue By Part",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Our Part Number",                                     ~
               at (06,20), fac(lfac$( 1)), part$                , ch(25),~
               at (06,49), fac(hex(8c)),   partdescr$           , ch(32),~
               at (07,02),                                               ~
                  "Vendor Code ",                                        ~
               at (07,30), fac(lfac$( 2)), vencode$             , ch(09),~
               at (07,49), fac(hex(8c)),   vencodedescr$        , ch(32),~
               at (08,02),                                               ~
                  "Vendor's Part Number",                                ~
               at (08,30), fac(lfac$( 3)), venpart$             , ch(25),~
               at (09,02),                                               ~
                  "Vendor's Stated Leadtime",                            ~
               at (09,30), fac(lfac$( 4)), venslt$              , ch(03),~
               at (10,02),                                               ~
                  "Vendor's Average Leadtime",                           ~
               at (10,30), fac(lfac$( 5)), venalt$              , ch(03),~
               at (11,02),                                               ~
                  "Date Last Changed",                                   ~
               at (11,30), fac(lfac$( 6)), pricdate$            , ch(08),~
               at (12,02),                                               ~
                  "Vendor Unit of Measure",                              ~
               at (12,30), fac(lfac$( 7)), unit$                , ch(04),~
               at (12,49), "(Stock UOM =      )",                        ~
               at (12,62), fac(hex(8c)),   purunit$             , ch(04),~
               at (13,02),                                               ~
                  "Our Quantity Per Vendor UOM",                         ~
               at (13,30), fac(lfac$( 8)), quan$                , ch(10),~
               at (14,02),                                               ~
                  "Current List Price",                                  ~
               at (14,30), fac(lfac$( 9)), list$                , ch(10),~
               at (15,02),                                               ~
                  "Discount % or Fixed Amount",                          ~
               at (15,30), fac(lfac$(10)), discper$             , ch(04),~
               at (15,35), fac(hex(8c)),   or$                  , ch(04),~
               at (15,40), fac(lfac$(10)), disc$                , ch(10),~
               at (16,02),                                               ~
                  "Current Price - Vendor UOM",                          ~
               at (16,30), fac(lfac$(11)), venppu$              , ch(10),~
               at (16,49), fac(hex(8c))  , eachmsg$             , ch(30),~
               at (17,02),                                               ~
                  "Contract or 'Special' Price",                         ~
               at (17,30), fac(lfac$(12)), nextprice$           , ch(10),~
               at (17,49), fac(hex(8c))  , eachmsg2$            , ch(30),~
               at (18,02),                                               ~
                  "Date 'Special' Price Starts",                         ~
               at (18,30), fac(lfac$(13)), date_effective$      , ch(08),~
               at (19,02),                                               ~
                  "Date 'Special' Price Ends",                           ~
               at (19,30), fac(lfac$(14)), date_expires$        , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,20), fac(hex(84)), pf12$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050c0d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L45170
                  call "MANUAL" ("VPRINPUT")
                  goto L44410

L45170:        if keyhit% <> 15 then L45210
                  call "PRNTSCRN"
                  goto L44410

L45210
*             IF FIELDNR% > 0% THEN RETURN
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%, edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L50230,         /* Vendor Code      */~
                                    L50280,         /* Vendor's Part Num*/~
                                    L50480,         /* Our Part Number  */~
                                    L50590,         /* Stated Leadtime  */~
                                    L50640,         /* Average Leadtime */~
                                    L50690,         /* Date Last Changed*/~
                                    L50730,         /* Vendor U.O.M.    */~
                                    L50850,         /* Conversion Factor*/~
                                    L50920,         /* List Price       */~
                                    L50970,         /* Discount         */~
                                    L51230,         /* Current Price    */~
                                    L51310,         /* Next or 'Special'*/~
                                    L51360,         /* Date Effective   */~
                                    L51500          /* Date Expires     */
                  return
L50230
*        Test Data for Vendor Code
                call "GETCODE" (#2,vencode$,vencodedescr$, 0%,1.3,f1%(2))
                if f1%(2) = 1% then return
                errormsg$ = "Vendor Code Not Found / Selected"
                return
L50280
*        Test Data for Vendor's Part Number
                if venpart$ > " " and venpart$ <> "?" then L50380
                plowkey$ = vencode$
                miscdescr$ = hex(06) & "Select The Vendor Part to Edit fo~
        ~r Vendor: " & vencode$
                hdr$(1) = "  Vendor Part Number"
                call "PLOWCODE" (#1, plowkey$,miscdescr$,1009%,2,f1%(1), ~
                                 hdr$())
                if f1%(1) = 0% then L50450
                goto L50410
L50380:         plowkey$ = str(vencode$) & venpart$
                call "REDALT0" (#1, plowkey$, 2%, f1%(1))
                if f1%(1) = 0% then return
L50410:         gosub dataload
                if f1%(1) = 0% then L50450
                   fieldnr% = 99%
                   return
L50450:         if venpart$ <> " " then return
                errormsg$ = "Vendor Part Number Cannot be Blank!"
                return
L50480
*        Test Data for Our Part Number
                call "GETCODE" (#3, part$, partdescr$, 0%,.32, f1%(3))
                if f1%(3) = 0% then L50560
                get #3 using L50580, purunit$, ptype$
                if ptype$ >= "200" and ptype$ < "500" then return
        REM     ERRORMSG$ = "Not a Purchased Part (Part Type = " &       ~
                            PTYPE$ & ")"
                return
L50560:         errormsg$ = "No Such Part on File: " & part$
                return
L50580:         FMT POS(74), CH(4), POS(180), CH(3)
L50590
*        Test Data for Vendor's Stated Leadtime
                call "NUMTEST" (venslt$,0,999,errormsg$,0,junk)
                if errormsg$ = " " then return
                errormsg$ = "Invalid Stated Leadtime, Please ReEnter"
                return
L50640
*        Test Data for Vendor's Average Leadtime
                call "NUMTEST" (venalt$,0,999,errormsg$,0,junk)
                if errormsg$ = " " then return
                errormsg$ = "Invalid Average Leadtime, Please ReEnter"
                return
L50690
*        Test Data for Date Last Changed
                call "DATEOK" (pricdate$, date%, errormsg$)
                return
                date% = date%
L50730
*        Test Data for Vendor Unit of Measure
                if unit$ = " " and uom% > 0% then L50770
                if unit$ = " " then L50830
                if uom% = 0% then return
L50770:         plowkey$ = "UOM      " & unit$
                selectmsg$ = hex(06) & "Unit of Measure"
                call "PLOWCODE" (#4,plowkey$,selectmsg$,9%,.30, f1%(4))
                if f1%(4) = 1% then unit$ = str(plowkey$,10%)            ~
                else errormsg$ = "Unknown Unit of Measure Code!"
                return
L50830:         errormsg$ = "Vendor Unit of Measure Cannot be Blank!"
                return
L50850
*        Test Data for Our Quantity Per Vendor UOM
                call "NUMTEST" (quan$,.0001,9e7,errormsg$,-.4,quan)
                if errormsg$ <> " " then return
                if eachmsg$ = " " then return  /* We're in Inputmode */
                gosub price_each_message
                gosub price_each2_message
                return
L50920
*        Test Data Current List
                disc = 0
                if list$ <> " " then L50940
                      disc$, discper$, or$ = " "
                      return
L50940:         call "NUMTEST" (list$,0,9e7,errormsg$,-2.4,list)
                if errormsg$ <> " " then L50950
L50950:         gosub calc_price
                return
L50970
*        Test Data Discount % or Fixed Discount Amount
                if list$ = " " then return
                if discper$ = " " then L51051
                   call"NUMTEST"(discper$,0,99.9,errormsg$,-1.1,discper)
                   if errormsg$ <> " " then return
                   or$ = "%   "
                   disc$ = " "
                   goto L51090
L51051:         if disc$ = " " then disc$ = "0"
                call "NUMTEST"(disc$,0,9e7,errormsg$,-2.4,disc)
                if errormsg$ <> " " then return
                or$ = "    "
L51090:         gosub calc_price
                return

        calc_price:
                if discper$ = " " then L51134
                   disc = (list * (discper / 100))
                   goto L51180
L51134:         if list >= disc then L51180
                   errormsg$ = "DISCOUNT may not be greater than LIST " &~
                                                              "PRICE"
                   or$ = "% OR"
                   return
L51180:         venppu = list - round(disc, 5)         
                call "CONVERT" (venppu, -2.5, venppu$) 
                call "NUMTEST" (venppu$,0,9e7,errormsg$,-2.5,venppu)
                gosub price_each_message               
                return

L51230
*        Test Data for Current Price - Vendor UOM    
                call "NUMTEST" (venppu$,0,9e7,errormsg$,-2.5,venppu)
                if errormsg$ <> " " then return
                if oldvenppu$ = venppu$ then L51290
                   list$, or$, disc$, discper$ = " "
                   disc, discper = 0
L51290:         gosub price_each_message
                return
L51310
*        Test Data for Next or 'Special' Price
                call "NUMTEST" (nextprice$,0,9e7,errormsg$,-2.4,nextprice)
                if errormsg$ <> " " then return
                gosub price_each2_message
                return
L51360
*        Test Data for Date 'Next Price' Effective
                if date_effective$ > " " then L51440
                if nextprice = 0 then L51420
                errormsg$ = "Sorry, Effective Date must be given when " &~
                            "Special Price is not zero!"
                return
L51420:         date_expires$ = " "
                return
L51440:         call "DATEOK" (date_effective$,efdate%, errormsg$)
                if errormsg$ > " " then return
                if pf4$ <> " " then return
                gosub'152(fieldnr% + 1%,2%)
                if errormsg$ = " " then fieldnr% = fieldnr% - 1%
                return
L51500
*        Test Data for Date 'Next Price' Expires
                if date_expires$ > " " then L51560
                if date_effective$ = " " then return
                errormsg$ = "Expiration Date must be given when " &      ~
                            "Effective Date is Present!"
                return
L51560:         call "DATEOK" (date_expires$,exdate%, errormsg$)
                if errormsg$ > " " then return
                if exdate% >= efdate% then L51630
                errormsg$ = "Expiration Date must be Greater Than or " & ~
                            "Equal to the Effective Date: " &            ~
                            date_effective$
                return
L51630:         call "DATUNFMT" (date_expires$)
                if date_expires$ < date then                             ~
                errormsg$ = "Expiration Date must be Greater Than or " & ~
                            "Equal to Today's Date"
                call "DATEFMT" (date_expires$)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%,edit%)
                  errormsg$ = " "
                  on fieldnr% gosub L50480,         /* Our Part Number  */~
                                    L50230,         /* Vendor Code      */~
                                    L50280,         /* Vendor's Part Num*/~
                                    L50590,         /* Stated Leadtime  */~
                                    L50640,         /* Average Leadtime */~
                                    L50690,         /* Date Last Changed*/~
                                    L50730,         /* Vendor U.O.M.    */~
                                    L50850,         /* Conversion Factor*/~
                                    L50920,         /* Current List     */~
                                    L50970,         /* Discount         */~
                                    L51230,         /* Current Price    */~
                                    L51310,         /* Next or 'Special'*/~
                                    L51360,         /* Date Effective   */~
                                    L51500          /* Date Expires     */
                  return

        price_each_message
                call "CONVERT" (venppu/quan,2.7,eachprice$)
                eachmsg$ = "$" & eachprice$ & " Each"
                call "SPCESMSH" (eachmsg$,1%)
                call "PUTPAREN" (eachmsg$)
                return

        price_each2_message
                call "CONVERT" (nextprice/quan,2.4,eachprice2$)
                eachmsg2$ = "$" & eachprice2$ & " Each"
                call "SPCESMSH" (eachmsg2$,1%)
                call "PUTPAREN" (eachmsg2$)
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end