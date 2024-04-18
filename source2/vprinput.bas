        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
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
            * 09/13/88 ! Added Free Text to VENPRICE.             ! JIM *~
            * 10/25/88 ! UOM conv factor no longer uses HNYMASTR. ! JDH *~
            * 04/15/91 ! PRR 11396/11734 Allow users to input     ! SID *~
            *          !     Vendor or Part range.                !     *~
            * 06/14/91 ! Added CALL 'ALLFREE'/Report Ranges on    ! SID *~
            *          !     the report heading.                  !     *~
            * 08/01/91 ! PRR 12104 Changed so that the Vendor     ! SID *~
            *          !     Price Record Saved is of the Stocking!     *~
            *          !     UOM and not Pricing UOM.             !     *~
            * 01/26/93 ! PRR 12135 Removed '$' from price display.! MLJ *~
            *          ! PRR 12220 Now saves CURRENT LIST PRICE,  !     *~
            *          !     DISCOUNT PERCENT and DISCOUNT AMOUNT !     *~
            *          !     in VENPRICE file.                    !     *~
            *          ! PRR 12238 (28)Edit Free Text changed to  !     *~
            *          !     (28)Manage Text.  Highlighted if text!     *~
            *          !     exists.  Also removed highlight from !     *~
            *          !     (12)Delete Data.                     !     *~
            *          ! PRR 10241 Added display of last Mod Date !     *~
            *          !     and User ID to detail screen.        !     *~
            *          ! PRR 12529 The price per (shown in parens)!     *~
            *          !     is now displayed in the stocking UOM !     *~
            *          !     instead of a hardcoded EACH.         !     *~
            *          ! MISC - Added TIME$ to end of report,     !     *~
            *          !     periods to input messages, updated   !     *~
            *          !     error messages and fixed implied     !     *~
            *          !     integers.                            !     *~
            * 02/08/93 ! Multi-Currency in Vendor Price Catalog.  ! JDH *~
            *          !   This resolves PRR 12162.               !     *~
            * 10/22/93 ! PRR 13035. Used the 'last modified' rule ! JDH *~
            *          !   in price entry and calculations.       !     *~
            * 01/18/94 ! PRR 12905,12962 Added Flag price for STC.! JDH *~
            * 01/20/94 ! PRR 13091 Added PF key Next Part/Vendor. ! JDH *~
            *          !   Added PF3 Copy functionality.          !     *~
            * 04/12/94 ! Added PF6 Previous Part to default entry.! JDH *~
            * 04/29/94 ! Added call to VPRCHGSB for price changes.! JDH *~
            * 09/12/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank unfmt date           */~
            curr$4, curr_descr$32,       /* Currency Code & Description*/~
            curr_p$4, curr_descr_p$32,   /* Currency Code & Description*/~
            curr_rpt$(2)4,               /* Currency Literals for Rpt  */~
            cursor%(2),                  /* Cursor location for edit   */~
            company_name$60,             /* Company name from COMPNAME */~
            date$8,                      /* Date for screen display    */~
            date_effective$8,            /* Date 'Next Price' Effective*/~
            date_effective_p$8,          /* Date 'Next Price' Effective*/~
            date_expires$8,              /* Date 'Next Price' Expires  */~
            date_expires_p$8,            /* Date 'Next Price' Expires  */~
            discper$6,                   /* Discount Percent           */~
            discper_p$6,                 /* Discount Percent           */~
            disc$10,                     /* Discount                   */~
            disc_p$10,                   /* Discount                   */~
            eachmsg$30,                  /* Converted Price (Current)  */~
            eachmsg2$30,                 /* Converted Price (Current)  */~
            eachprice$10,                /* Converted Price (Current)  */~
            eachprice2$10,               /* Converted Price (Next   )  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmpart$25, topart$25,        /* Part Range                 */~
            fmvendor$9, tovendor$9,      /* Vendor Range               */~
            hdr$(2)133,                  /* Column Header Lines        */~
            hipart$25, lopart$25,        /* Part Range                 */~
            hivendor$9, lovendor$9,      /* Vendor Range               */~
            i$(24)80,                    /* Screen Image               */~
            incl(1),                     /* Dummy PLOWCODE Argument    */~
            incl$(1)1,                   /* Dummy PLOWCODE Argument    */~
            inpmessage$79,               /* Informational Message      */~
            last_moddate$8,              /* Last Modified Date         */~
            last_userid$3,               /* Last Modified By User ID   */~
            last_part$25,                /* Last Part Processed        */~
            last_vendor$9,               /* Last Vendor Processed      */~
            last_venpart$25,             /* Last Vendor Part Processed */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line3$131,                   /* Third  Line of Screen Headr*/~
            list$10,                     /* List Price                 */~
            list_p$10,                   /* List Price                 */~
            mask$134,                    /* Text mask                  */~
            miscdescr$99,                /* Miscellaneous Description  */~
            nextprice$10,                /* Next or 'Special' Price    */~
            nextprice_p$10,              /* Next or 'Special' Price    */~
            only_one$1,                  /* Only One Record? Y,N, or X */~
            oldvenppu$10,                /* Current Price - Vendor UOM */~
            option$40,                   /* PF Options Header          */~
            or$4,                        /* ACCEPT Message for disc %  */~
            pvndr$9, ppart$25, pvprt$25, pvuom$4, pdesc$32, pouom$4,     ~
            ptext$4,                     /* Print variables            */~
            part$25,                     /* Our Part Number            */~
            partdescr$32,                /* Our Part Number            */~
            pf$6,                        /* PF Keys Header             */~
            pf12$16,                     /* PF 12 Screen Literal       */~
            pf12mc$39, pf12mck$1,        /* PF 12 MC Delete Literal    */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf28m$18, pf28k$1, pf28f$1,  /* For PF(28)/Free Text       */~
            pf3$7, pf3k$1,               /* PF 3 Screen Literal        */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf6$16, pf6k$1,              /* PF 6 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            pricdate$8,                  /* Date Last Changed          */~
            pricdate_p$8,                /* Date Last Changed          */~
            purunit$4,                   /* HNYMASTR Purchase U.O.M.   */~
            purunit_p$4,                 /* HNYMASTR Purchase U.O.M.   */~
            pvppu$12,                    /* Formatted Price for printng*/~
            quan$10, quan_p$10,          /* Our Quantity Per Vendor UOM*/~
            save_part$25, save_partd$32, /* Part for copy function     */~
            save_vencode$9, save_vend$30,/* Vendor for copy function   */~
            save_venpart$25,             /* Ven Part - copy function   */~
            stat$4,                      /* Statutory Currency         */~
            stc_flag$1, stc_who$35,      /* Std Cost Build-up info     */~
            stc_flag_p$1,                /* Std Cost Build-up info     */~
            text$1,                      /* Text Printing Flags (Y/N)  */~
            textid$4, text$(113,1)70,    /* VENPRICE Free Text stuff   */~
            time$8,                                                      ~
            unit$4,                      /* Vendor Unit of Measure     */~
            unit_p$4,                    /* Vendor Unit of Measure     */~
            userid$3,                    /* Current User Id            */~
            venalt$3,                    /* Vendor's Average Leadtime  */~
            venalt_p$3,                  /* Vendor's Average Leadtime  */~
            vencode$9,                   /* Vendor Code                */~
            vencodedescr$30,             /* Vendor Code                */~
            venpart$25,                  /* Vendor's Part Number       */~
            venppu$10,                   /* Current Price - Vendor UOM */~
            venppu_p$10,                 /* Current Price - Vendor UOM */~
            venslt_p$3,                  /* Vendor's Stated Leadtime   */~
            venslt$3                     /* Vendor's Stated Leadtime   */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #9  ! TXTFILE  ! System Text File                         *~
            * #12 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
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

            select #9 , "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #12,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%), 0%, rslt$(2%))
                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, rslt$(3%))

            if min(fs%()) < 0% then exit_program

            call "OPENCHCK" (#1,  fs%(1%), f2%(1%), 1%, rslt$(1%))
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#9,  fs%(9%), f2%(9%), 0%, rslt$(9%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            or$ = "% OR"
            plowkey$ = all(hex(00))              /* If UOM file set up */
            str(plowkey$,10%) = "UOM"            /* then edit entries. */
            call "READ100" (#4, plowkey$, uom%)
            pf$ = "PF Keys" : option$ = "Options"
            pf28m$ = "(28)Manage Text"
            max_lines% = 54%
            call "COMPNAME" (12%, company_name$, comp%)

*        Check for Multi-Currency
            mc_on$ = "N" : stat$ = " "
            pf12mc$, curr_rpt$() = " " : pf12mck$ = hex(ff)
            readkey$ = "SWITCHS.CUR         "
            call "READ100" (#12, readkey$, f1%(12%))
            if f1%(12%) <> 0% then get #12 using L09240, mc_on$, stat$
L09240:         FMT POS(21), CH(1), CH(4)
            if mc_on$ <> "Y" then stat$ = " "
            if mc_on$ <> "Y" then L10000
                call "OPENCHCK" (#40, 0%, f2%(40%), f1%(40%), " ")
                pf12mc$ = "(12)    Delete ALL Non-Statutory Prices"
                pf12mck$ = hex(0c)
                curr_rpt$(1%) = "Curr" : curr_rpt$(2%) = "----"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        main_screen
            init(" ") errormsg$, inpmessage$, vencode$, vencodedescr$,   ~
                      part$, partdescr$
            call "ALLFREE"

            for field% = 1% to 1%
                gosub'051(field%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L11050
                gosub'101(field%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then       exit_program
                      if keyhit%  =  8% then gosub find_by_vendor
                      if keyhit%  =  9% then gosub find_by_part
                      if keyhit%  = 10% then gosub input_by_vendor
                      if keyhit%  = 11% then gosub input_by_part
                      if keyhit%  = 12% then gosub delete_non_stat_stuff
                      if keyhit%  = 14% then gosub print_by_vendor
                      if keyhit%  = 30% then gosub print_by_part
                field% = field% - 1%
                errormsg$ = " "
            next field%

        find_by_vendor
            plowkey$ = vencode$
            incl(1%) = 0
            hdr$(2%) = "  Vendor      Vendor Name"
            call "PLOWCODE" (#1, plowkey$, vencodedescr$, -8009%, -2.3,  ~
                    f1%(1%), hdr$(), 0,0, incl(), incl$(),"Y"," ",#2)
            if f1%(1%) = 0% then return
            vencode$ = plowkey$
L10340:     part$, disc$, discper$, list$ = " "
            call "VPRVSLCT" (part$,vencode$,venpart$,2%,only_one$,#1,#2, ~
                             #3, #5)
            plowkey$ = str(part$) & str(vencode$) & venpart$
            call "READ101" (#1, plowkey$, f1%(1%))
            if f1%(1%) = 0% then return
            gosub dataload
            if copy% = 1% then return
            add% = 3%
            gosub edtpg2
            if only_one$ <> "N" then return
            goto L10340

        find_by_part
            incl(1%) = 0
            hdr$(2%) = "  Part Code                   Description"
            plowkey$ = part$
            call "PLOWCODE" (#1, plowkey$, partdescr$, -8025%, -.32,     ~
                    f1%(1%), hdr$(), 0,0, incl(), incl$(),"Y"," ",#3)
            if f1%(1%) = 0% then return
            part$ = plowkey$
L10540:     vencode$, disc$, discper$, list$ = " "
            call "VPRPSLCT" (part$,vencode$,venpart$,2%,only_one$,#1,#3, ~
                             #2, #5)
            plowkey$ = str(part$) & str(vencode$) & venpart$
            call "READ101" (#1, plowkey$, f1%(1%))
            if f1%(1%) = 0% then return
            gosub dataload
            if copy% = 1% then return
            add% = 4%
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
            init(" ") errormsg$, inpmessage$, purunit$,                  ~
                      vencodedescr$               ,/* Vendor Code Descr*/~
                      venpart$                    ,/* Vendor's Part Num*/~
                      part$, partdescr$           ,/* Our Part Number  */~
                      venslt$                     ,/* Stated Leadtime  */~
                      venalt$                     ,/* Average Leadtime */~
                      pricdate$                   ,/* Date Last Changed*/~
                      unit$                       ,/* Vendor U.O.M.    */~
                      quan$                       ,/* Conversion Factor*/~
                      curr$, curr_descr$          ,/* Currency Code    */~
                      list$                       ,/* Discount         */~
                      discper$, disc$             ,/* Discount Percent */~
                      venppu$                     ,/* Current Price    */~
                      nextprice$                  ,/* Next or 'Special'*/~
                      date_effective$             ,/* Date Effective   */~
                      date_expires$               ,/* Date Expires     */~
                      last_moddate$, last_userid$ ,/* Last Date & User */~
                      eachmsg$, eachmsg2$,         /* Each Prices      */~
                      stc_flag$, stc_who$          /* Stc Build-up     */
            stc_flagged% = 0%
            call "ALLFREE"

            init (hex(ff)) textid$
            call "TXTFUTIL" (#9, f2%(9%), "INTL", textid$)

            for fieldnr% = 1% to 16%
L10930:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L11050
L10950:         gosub'102(fieldnr%,1%)  /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  3% then copy_vendor : copy_v_abort
                      if keyhit% <>  6% then       L10970
                         gosub'062(fieldnr%) /* Set Field From Prev */
                         goto L11050
L10970:               if keyhit% <>  4% then       L11030
L10980:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10950
                         if fieldnr% = 1% then L10930
                         goto L10980
L11030:               if keyhit%  = 16% and fieldnr% < 3% then return
                      if keyhit% <>  0% then       L10950
L11050:         gosub'152(fieldnr%,1%)   /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10950
            next fieldnr%
            goto edtpg2

        input_by_part
            add% = 2%
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Return"
            pf12$ = " "
            last_vendor$ = vencode$
            last_part$ = part$
            if last_part$ = "?" then last_part$ = " "
            list, disc = 0
            init(" ") errormsg$, inpmessage$, purunit$,                  ~
                      vencode$, vencodedescr$     ,/* Vendor Code      */~
                      venpart$                    ,/* Vendor's Part Num*/~
                      partdescr$                  ,/* Our Part Number  */~
                      venslt$                     ,/* Stated Leadtime  */~
                      venalt$                     ,/* Average Leadtime */~
                      pricdate$                   ,/* Date Last Changed*/~
                      unit$                       ,/* Vendor U.O.M.    */~
                      quan$                       ,/* Conversion Factor*/~
                      curr$, curr_descr$          ,/* Currency Code    */~
                      list$                       ,/* List             */~
                      discper$, disc$             ,/* Discount         */~
                      venppu$                     ,/* Current Price    */~
                      nextprice$                  ,/* Next or 'Special'*/~
                      date_effective$             ,/* Date Effective   */~
                      date_expires$               ,/* Date Expires     */~
                      last_moddate$, last_userid$ ,/* Last Date & User */~
                      eachmsg$, eachmsg2$,         /* Each Prices      */~
                      stc_flag$, stc_who$          /* Stc Build-up     */
            stc_flagged% = 0%
            call "ALLFREE"

            init (hex(ff)) textid$
            call "TXTFUTIL" (#9, f2%(9%), "INTL", textid$)

            for fieldnr% = 1% to 16%
L11370:         gosub'053(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L11490
L11390:         gosub'103(fieldnr%,1%)  /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  3% then copy_part : copy_p_abort
                      if keyhit% <>  6% then       L11410
                         gosub'062(fieldnr%) /* Set Field From Prev */
                         goto L11490
L11410:               if keyhit% <>  4% then       L11470
L11420:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L11390
                         if fieldnr% = 1% then L11370
                         goto L11420
L11470:               if keyhit%  = 16% and fieldnr% < 3% then return
                      if keyhit% <>  0% then       L11390
L11490:         gosub'153(fieldnr%,1%)  /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11390
            next fieldnr%
            goto edtpg3

        print_by_vendor
            print% = 0% : init(" ") str(line3$)
            gosub select_vendor_range
            if print% = 0% then return
            str(line3$,1%,) = "Vendor Range  " & fmvendor$ & "  TO  " &  ~
                             tovendor$
            k%= len(line3$)
            if fmvendor$ = "ALL" then k% = k% - 3%
            if text$ = "N" then str(line3$,k%+2%,) = "No Text Printed  " ~
                           else str(line3$,k%+2%,) = "With Text Printed"
            call "STRING" addr("CT", line3$, 132%, line3$)
            call "SHOSTAT" ("Printing Vendor Prices by VENDOR report")
            gosub set_up_report
            plowkey$ = lovendor$ & hex(00)
            akey% = 2%                        /* Indicate Vendor report*/
            gosub print_the_report
            return

        print_by_part
            print% = 0% : init(" ") str(line3$)
            gosub select_part_range
            if print% = 0% then return

            str(line3$,1%,) = "Part Range    " & fmpart$ & "  TO  " &    ~
                              topart$
            k% = len(line3$)
            if fmpart$ = "ALL" then k% = k% - 3%
            if text$ = "N" then str(line3$,k%+2%,) = "No Text Printed  " ~
                           else str(line3$,k%+2%,) = "With Text Printed"
            call "STRING" addr("CT", line3$, 132%, line3$)

            call "SHOSTAT" ("Printing Vendor Prices by PART report")
            gosub set_up_report
            plowkey$ = lopart$ & hex(00)
            akey% = 0%                           /* Indicate Part report*/
            gosub print_the_report
            return

        set_up_report
            page_nbr% = 0% : nbr_lines% = 99%
            time$ = " "
            call "TIME" (time$)
            select printer (134)
            call "SETPRNT" ("VPR001", " ", 0%, 0%)
            return

        print_the_report
            call "REDALT2" (#1, plowkey$, akey%, f1%(1%))
            goto L12130
L12120:     call "READNEXT" (#1, f1%(1%))
L12130:         if f1%(1%) = 0% then goto end_of_report
            pouom$ = " " : poqty = 0
            pdesc$ = "** Not on File **"
            get #1 using L12170, pvndr$, ppart$, pvprt$, pvppu, pvuom$,   ~
                poqty, ptext$, curr$, stc_flag$
L12170:         FMT CH(9), CH(25), POS(44), CH(25), PD(14,4), POS(89),   ~
                    CH(4), PD(14,4), POS(130), CH(4), POS(158), CH(4),   ~
                    CH(1)
            if stc_flag$ <> "Y" then stc_flag$ = " "
            if curr$ = " " and mc_on$ = "Y" then curr$ = stat$
            if mc_on$ <> "Y" then call "CONVERT" (pvppu, 2.4, pvppu$)    ~
                          else call "CURRFMT" (pvppu, curr$, pvppu$, "Y")
            if akey% = 0% then L12188
               if pvndr$ > hivendor$ then end_of_report else L12190
L12188:        if ppart$ > hipart$ then end_of_report
L12190:     call "READ100" (#3, ppart$, f1%(3%))
            if f1%(3%) <> 0% then get #3 using L12210, pdesc$, pouom$
L12210:         FMT POS(26), CH(32), POS(74), CH(4)
            if nbr_lines% > max_lines% then gosub print_headings
            if akey% = 0%                                                ~
                then print using L15160, ppart$, pvndr$, pvprt$, pdesc$,  ~
                          curr$, pvppu$, stc_flag$, pvuom$, pouom$, poqty~
                else print using L15182, pvndr$, pvprt$, ppart$, pdesc$,  ~
                          curr$, pvppu$, stc_flag$, pvuom$, pouom$, poqty
            nbr_lines% = nbr_lines% + 1%
            if text$ = "Y" then gosub print_the_text
            goto L12120

        end_of_report
            print
                time$ = " "
                call "TIME" (time$)
            print "*****  END OF REPORT @ " & time$ & "  *****"
            close printer
            call "SETPRNT" ("VPR001", " ", 0%, 1%)
            return

        print_the_text
            if ptext$ = hex(ffffffff) then return
            if ptext$ = hex(00000000) then return
            if ptext$ = " " then return
                txt% = (page_nbr% * 100%) + nbr_lines%
                comp% = 0%
L12410:         call "TXTPRINT" (#9, f2%(9%),134%, ptext$, "VPR001", 10%,~
                     nbr_lines%, max_lines%, "T", mask$, comp%)
                if comp% = 0% then goto L12460
                     gosub print_headings
                     goto L12410
L12460:         if txt% = (page_nbr% * 100%) + nbr_lines% then return
                     print
                     nbr_lines% = nbr_lines% + 1%
                     return

        print_headings
            page_nbr% = page_nbr% + 1% : nbr_lines% = 0%
            print page
            print using L15040, date$, time$, company_name$, "-VPR001"
            if akey% = 0%                          /* 2ND HEADER LINE */ ~
                then print using L15070, page_nbr%                        ~
                else print using L15092, page_nbr%
            print using L15220, line3$
            print
            print using L15098
            if akey% = 0%                            /* COLUMNS HEADS */ ~
                then print using L15100, curr_rpt$(1%)                    ~
                else print using L15122, curr_rpt$(1%)
            if akey% = 0%                              /* UNDERSCORES */ ~
                then print using L15130, curr_rpt$(2%)                    ~
                else print using L15152, curr_rpt$(2%)
            return

        select_vendor_range
            fmvendor$, tovendor$ = " " : text$ = "N"
L12680:     gosub'104(1%)
                if keyhit% =   1% then select_vendor_range
                if keyhit% =  16% then return
                if keyhit% <>  0% then L12680
            gosub'154
                if errormsg$ <> " " then L12680
            goto edtpg4

        select_part_range
            fmpart$, topart$ = " " : text$ = "N"
L12770:     gosub'105(1%)
                if keyhit% =   1% then select_part_range
                if keyhit% =  16% then return
                if keyhit% <>  0% then L12770
            gosub'155
                if errormsg$ <> " " then L12770
            goto edtpg5

        copy_vendor
            type% = 1% : goto copy_start
        copy_part
            type% = 2%
        copy_start
            copied% = 0%
            copy% = 1%
            save_vencode$ = vencode$
            save_vend$    = vencodedescr$
            save_part$    = part$  : save_venpart$ = venpart$
            save_partd$   = partdescr$
            save_venpart$ = venpart$
            vencode$, vencodedescr$, part$, partdescr$, venpart$ = " "
            on type% gosub find_by_vendor, find_by_part
            copy% = 0%
            vencode$      = save_vencode$
            vencodedescr$ = save_vend$
            part$         = save_part$
            partdescr$    = save_partd$
            venpart$      = save_venpart$
            if copied% <> 0% then L12950
                on type% goto copy_v_abort, copy_p_abort
L12950:     on type% goto edtpg2, edtpg3

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
            gosub'102(0%,2%)            /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then       datakill
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 28% then gosub edit_text_vendor_part
                  if keyhit% <>  0% then       edtpg2
L13180:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 4% or fieldnr% > 16% then edtpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg2
                  pf4$, pf5$, pf16$ = " "
L13230:     gosub'102(fieldnr%,2%)      /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13230
            gosub'152(fieldnr%,2%)       /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13230
                  if cursor%(1%) - 3% <> fieldnr% then L13180
            goto edtpg2

        edit_text_vendor_part
            call "TXTINSUB" (#9, f2%(9%), "024",                         ~
                "Free Text for Vendor: " & vencode$ & " / Part: " &      ~
                part$, textid$, text$())
            return

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
            gosub'103(0%,2%)            /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then       datakill
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 28% then gosub edit_text_vendor_part
                  if keyhit% <>  0% then       edtpg3
L13550:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 4% or fieldnr% > 16% then edtpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg3
                  pf4$, pf5$, pf16$ = " "
L13600:     gosub'103(fieldnr%,2%)      /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13600
            gosub'153(fieldnr%,2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13600
                  if cursor%(1%) - 3% <> fieldnr% then L13550
            goto edtpg3

        edtpg4
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            s% = 0%
            gosub'104(s%)
                  if keyhit% =   1% then gosub startover
                  if keyhit% <> 16% then       L13790
                     print% = 1%
                     return
L13790:           if keyhit% <>  0% then       edtpg4
                  k% = cursor%(1%) - 5%
                  if k% = 1% or k% = 2% then L13830                       ~
                                        else goto edtpg4
L13830:     gosub'104(1%)
                  if keyhit% =   1% then select_vendor_range
                  if keyhit% <>  0% then L13830
            gosub'154
                  if errormsg$ <> " " then L13830
            goto edtpg4

        edtpg5
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            s% = 0%
            gosub'105(s%)
                  if keyhit% =   1% then gosub startover
                  if keyhit% <> 16% then       L13990
                     print% = 1%
                     return
L13990:           if keyhit% <>  0% then       edtpg5
                  k% = cursor%(1%) - 5%
                  if k% = 1% or k% = 2% then L14012                       ~
                                        else edtpg5
L14012:     gosub'105(1%)
                  if keyhit% =   1% then select_part_range
                  if keyhit% <>  0% then L14012
            gosub'155
                  if errormsg$ <> " " then L14012
            goto edtpg5

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *************************************************************

L15040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                      VPRINPUT####~
        ~###
L15070: %                                        V E N D O R   P R I C E ~
        ~  L I S T   B Y   P A R T                                 PAGE: #~
        ~###
L15092: %                                      V E N D O R   P R I C E   ~
        ~L I S T   B Y   V E N D O R                               PAGE: #~
        ~###
L15098: %                                                                ~
        ~                                                 S Vend Our
L15100: %Our Part Number           Vendor    Vendor's Part Number      Pa~
        ~rt Description                 #### Vendor Price TC UOM UOM  Our ~
        ~Qty
L15122: %Vendor    Vendor's Part Number      Our Part Number           Pa~
        ~rt Description                 #### Vendor Price TC UOM UOM  Our ~
        ~Qty
L15130: %------------------------- --------- ------------------------- --~
        ~------------------------------ #### ------------ - ---- ---- ----~
        ~---
L15152: %--------- ------------------------- ------------------------- --~
        ~------------------------------ #### ------------ - ---- ---- ----~
        ~---
L15160: %######################### ######### ######################### ##~
        ~############################## #### ############ # #### #### ####~
        ~###
L15182: %######### ######################### ######################### ##~
        ~############################## #### ############ # #### #### ####~
        ~###

L15220: %################################################################~
        ~#################################################################~
        ~###
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
            if keyhit% = 1% then L19202
            if keyhit% <> 0% then datakill
            plowkey$ = str(part$) & str(vencode$) & venpart$
            call "DELETE" (#1, plowkey$, 59%)
            call "TXTFUTIL" (#9, f2%(9%), "DELE", textid$)
            on add% goto input_by_vendor, input_by_part
            return
L19202:     on add% goto , , edtpg2, edtpg3
            return

        delete_non_stat_stuff
            if mc_on$ <> "Y" then return
L19520:         u3% = 0%
                call "ASKUSER" (u3%, "WARNING!!!", "This was designed " &~
                     "for the rare occurence of turning off Multi-Curr" &~
                     "ency.",                                            ~
                     "Do you really want to delete ALL foreign prices?", ~
                     "Press PF28 to Delete -or- Press RETURN to Abort.")
                if u3%  =  0% then return
                if u3% <> 28% then L19520
            plowkey$ = all(hex(00))
            call "SHOSTAT" ("Deleting Foreign Prices...")
          delete_loop
            call "PLOWNXT1" (#01, plowkey$, 0%, f1%(1%))
            if f1%(1%) = 0% then return
                get #01 using L19645, curr$
L19645:              FMT POS(158), CH(4)
                if curr$ = " " then delete_loop
                     if curr$ <> stat$ then L19700
                          put #01 using L19645, " "
                          rewrite #01
                          goto delete_loop
L19700:              delete #01
                     goto delete_loop

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  inpmessage$ = "Edit Existing Entries via PF8 or 9; " & ~
                                "Use PF10 or 11 to Add new Entries."
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
                                    L21960,         /* Currency Code    */~
                                    L21630,         /* List price       */~
                                    L21660,         /* Discount         */~
                                    L21740,         /* Current Price    */~
                                    L21800,         /* Next or 'Special'*/~
                                    L21860,         /* Date Effective   */~
                                    L21900,         /* Date Expires     */~
                                    L21996          /* STC Build-up     */
                     return
L21240:     REM Default/Enable for Vendor Code
                inpmessage$ ="Enter Vendor Code or leave blank to search."
                if vencode$ > " " and keyhit% <> 4% then enabled% = 0%
                return
L21280:     REM Default/Enable for Vendor's Part Number
                inpmessage$ = "Enter Vendor Part Number or leave blank "&~
                              "to search."
                return
L21320:     REM Default/Enable for Our Part Number
                inpmessage$ = "Enter our Part Number for this Vendor's" &~
                              " part or leave blank to search."
                if part$ > " " and keyhit% <> 4% then enabled% = 0%
                return
L21370:     REM Default/Enable for Vendor's Stated Leadtime
                inpmessage$ = "Enter the Vendor's Stated Leadtime for " &~
                              "this part."
                return
L21410:     REM Default/Enable for Vendor's Average Leadtime
                inpmessage$ = "Enter the Vendor's Average Leadtime for "&~
                              "this part."
                if venalt$ = " " then venalt$ = venslt$
                return
L21460:     REM Default/Enable for Date Last Changed
                pricdate$ = date$
                inpmessage$ = "Enter the Date that Information was last"&~
                              " changed."
                return
L21500:     REM Default/Enable for Vendor Unit of Measure
                inpmessage$ = "Enter the Unit of Measure we purchase " & ~
                              "this part in from this Vendor."
                if unit$ = " " then unit$ = purunit$
                return
L21550:     REM Default/Enable for Our Quantity Per Vendor UOM
                inpmessage$ ="Enter the Number of Our " & purunit$ &     ~
                             " that go into the Vendor's " & unit$ & "."
                if unit$ <> purunit$ or quan$ <> " " then return
                    quan     = 1
                    enabled% = 0%
                    call "CONVERT" (quan,-0.4, quan$)
                return
L21630:     REM Default/Enable for Current List Price
                inpmessage$ = "Enter the Current List Price."
                return
L21660:     REM Default/Enable for Discount% or Fixed Discount Amount
                inpmessage$ = "Enter the Discount Percent or the"      & ~
                              " Fixed Discount Amount."
                return
L21740:     REM Default/Enable for Current Price - Vendor UOM
                oldvenppu$ = venppu$
                if venppu$>" " then call "CONVERT" (venppu,-2.4, venppu$)
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
                    "Price' will override the Current Price."
                return
L21900:     REM Default/Enable for Date 'Next Price' Expires
                inpmessage$ = "Enter the Ending Date that 'Special Price"~
                           & "' will override the Current Price."
                if date_effective$ = " " or date_effective$ = blankdate$ then~
                   enabled% = 0%
                return

L21960:     REM Default/Enable for Currency Code   CURR$
                enabled% = 0%
                if mc_on$ <> "Y" then return
                     enabled% = 1%
                     inpmessage$ = "Enter Currency Code for this Price."
                if curr$ <> " " then return
                     get #02 using L21984, curr$
L21984:                   FMT POS(528), CH(4)
                     call "DESCRIBE" (#40, curr$, curr_descr$, 1%,       ~
                                                                f1%(40%))
                     if f1%(40%) = 0% then curr_descr$ = "* Not on File *"
                return

L21996:     REM Std Cost Build-up Flag             STC_FLAG$
                inpmessage$ = "Use in Std Cost Build-up? ('Y' or 'N')"
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
                                    L21960,         /* Currency Code    */~
                                    L21630,         /* Current List     */~
                                    L21660,         /* Discount         */~
                                    L21740,         /* Current Price    */~
                                    L21800,         /* Next or 'Special'*/~
                                    L21860,         /* Date Effective   */~
                                    L21900,         /* Date Expires     */~
                                    L21996          /* STC Build-up     */
                     return

        REM *************************************************************~
            *     D E F A U L T   F I E L D   E N T R Y                 *~
            *-----------------------------------------------------------*~
            * Sets Field Entry from Previous Part Called Up or Saved.   *~
            *************************************************************

            deffn'062(fieldnr%)
                  on fieldnr% gosub      ,         /* Our Part Number  */~
                                         ,         /* Vendor Code      */~
                                         ,         /* Vendor's Part Num*/~
                                    L23300,         /* Stated Leadtime  */~
                                    L23350,         /* Average Leadtime */~
                                    L23400,         /* Date Last Changed*/~
                                    L23450,         /* Vendor U.O.M.    */~
                                    L23500,         /* Conversion Factor*/~
                                    L23550,         /* Currency Code    */~
                                    L23600,         /* Current List     */~
                                    L23650,         /* Discount         */~
                                    L23700,         /* Current Price    */~
                                    L23750,         /* Next or 'Special'*/~
                                    L23800,         /* Date Effective   */~
                                    L23850,         /* Date Expires     */~
                                    L23900          /* STC Build-up     */
                     return

L23300
*        Previous Entry for Stated Leadtime
            venslt$ = venslt_p$
            return
L23350
*        Previous Entry for Average Leadtime
            venalt$ = venalt_p$
            return
L23400
*        Previous Entry for Date Last Changed
            pricdate$ = pricdate_p$
            return
L23450
*        Previous Entry for Vendor U.O.M.
            unit$ = unit_p$
            purunit$ = purunit_p$
            return
L23500
*        Previous Entry for Conversion Factor
            quan$ = quan_p$
            return
L23550
*        Previous Entry for Currency Code
            curr$ = curr_p$
            curr_descr$ = curr_descr_p$
            return
L23600
*        Previous Entry for Current List
            list$ = list_p$
            return
L23650
*        Previous Entry for Discount
            discper$ = discper_p$
            disc$ = disc_p$
            return
L23700
*        Previous Entry for Current Price
            venppu$ = venppu_p$
            return
L23750
*        Previous Entry for Next or 'Special'
            nextprice$ = nextprice_p$
            return
L23800
*        Previous Entry for Date Effective
            date_effective$ = date_effective_p$
            return
L23850
*        Previous Entry for Date Expires
            date_expires$ = date_expires_p$
            return
L23900
*        Previous Entry for STC Build-up
            stc_flag$ = stc_flag_p$
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
            call "READ100" (#1, plowkey$, f1%(1%))
            if f1%(1%) = 0% then return

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
            date_expires$,  /* Date 'something' expires                */~
            last_moddate$,  /* Last Modified Date                      */~
            last_userid$,   /* Last Modified By User ID                */~
            textid$,        /* Free text pointer                       */~
            list,           /* Current List Price                      */~
            discper,        /* Discount Percent                        */~
            disc,           /* Fixed Discount Amount                   */~
            curr$,          /* Currency Code                           */~
            stc_flag$       /* Std Cost Build-up Flag                  */

            call "DATEFMT"  (pricdate$)
            call "DATEFMT" (date_effective$)
            call "DATEFMT" (date_expires$)
            call "DATEFMT"  (last_moddate$)
            if date_effective$ <> " " and date_effective$ <> blankdate$ then ~
               call "DATEOK" (date_effective$,efdate%, errormsg$)
            if curr$ = " " and mc_on$ = "Y" then curr$ = stat$
            if mc_on$ <> "Y" then curr$ = " "
            call "DESCRIBE" (#40, curr$, curr_descr$, 1%, f1%(40%))
            if stc_flag$ = " " then stc_flag$ = "N"
            call "CONVERT" (venppu, 2.4, venppu$)
            call "CONVERT" (list, 2.4, list$)
            call "CONVERT" (disc, -2.4, disc$)
            call "CONVERT" (nextprice, 2.4, nextprice$)
            call "CONVERT" (discper, 2.2, discper$)
            call "CONVERT" (quan, 0.4,quan$)
            call "GETCODE" (#3, part$, partdescr$, 0%, 20, f1%(3%))
            if f1%(3%) = 1% then get #3 using L30410, purunit$
            if f1%(3%) = 0% then partdescr$ = "** Not on File **"
            call "DESCRIBE" (#2, vencode$, vencodedescr$, 0%, f1%(2%))
            gosub price_each_message
            gosub price_each2_message
            if copy% <> 1% then L30370
                stc_flag$ = "N" : pricdate$ = date$
                last_moddate$, last_userid$ = " "
                if textid$ = " " or textid$ = hex(ffffffff) or           ~
                                    textid$ = hex(00000000) then L30364
L30358:         ask% = 2%
                call "ASKUSER" (ask%, "***** COPY TEXT? *****",          ~
                     "Press PF-28 to INCLUDE Text in the Copy", "- or-", ~
                     "PF-16 to NOT Copy Text")
                if ask% = 28% then L30365
                if ask% <> 16% then L30358
L30364:              init (hex(ff)) textid$ : goto L30380
L30365:         call "TXTFUTIL" (#09, f2%(9%), "INTL", " ")
                call "TXTFUTIL" (#09, f2%(9%), "COPY", textid$)
                goto L30380
L30370:     call "TXTFUTIL" (#9, f2%(9%), "LOAD", textid$)
L30380:     gosub whose_flagged_for_stc
            copied% = 1%
            gosub set_previous_info
            return

L30410:     FMT POS(74), CH(4)

        whose_flagged_for_stc
            stc_who$ = " " : stc_flagged% = 0%
            if stc_flag$ <> "Y" then L30480
                stc_flagged% = 1%
                return
L30480:     plowkey$ = part$
L30490:     call "PLOWNEXT" (#1, plowkey$, 25%, f1%(1%))
                if f1%(1%) = 0% then return
            get #1 using L30520, temp$
L30520:         FMT POS(162), CH(1)
            if temp$ <> "Y" then L30490
                get #1 using L30550, str(stc_who$,,9%),                   ~
                                    str(stc_who$,11%,25%)
L30550:              FMT POS(35), CH(9), CH(25)
                return

        set_previous_info
            venslt_p$ = venslt$
            venalt_p$ = venalt$
            pricdate_p$ = date$
            unit_p$ = unit$
            purunit_p$ = purunit$
            quan_p$ = quan$
            curr_p$ = curr$
            curr_descr_p$ = curr_descr$
            list_p$ = list$
            discper_p$ = discper$
            disc_p$ = disc$
            venppu_p$ = venppu$
            nextprice_p$ = nextprice$
            date_effective_p$ = date_effective$
            date_expires_p$ = date_expires$
            stc_flag_p$ = "N"
            previous_info_set% = 1%
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            plowkey$ = str(part$) & str(vencode$) & venpart$
            call "READ101" (#1, plowkey$, f1%(1%))
               pricdate$ = date$
            call "DATUNFMT" (pricdate$)
            call "DATUNFMT" (date_effective$)
            call "DATUNFMT" (date_expires$)
            venppu = round(venppu,4%)
            quan = round(quan,4%)
            nextprice = round(nextprice,4%)
            list = round(list,4%)
            discper = round(discper,4%)
            disc = round(disc,4%)
            if mc_on$ <> "Y" then curr$ = " "

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
            textid$,        /* Free text pointer                       */~
            list,           /* Current List Price                      */~
            discper,        /* Discount Percent                        */~
            disc,           /* Fixed Discount Amount                   */~
            curr$,          /* Currency Code                           */~
            stc_flag$,      /* Std Cost Build-up                       */~
            " "             /* Filler For Rest of Record or Internal Sp*/

            if f1%(1%) = 0% then write #1 else rewrite #1
            call "TXTFUTIL" (#9, f2%(9%), "TOS2", textid$)
            gosub set_previous_info
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
            PD(14,4),       /* Vendor's current price per unit         */~
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
            CH(4),          /* Free text pointer                       */~
            PD(14,4),       /* Current List Price                      */~
            PD(14,4),       /* Discount Percent                        */~
            PD(14,4),       /* Fixed Discount Amount                   */~
            CH(4),          /* Currency Code                           */~
            CH(1),          /* Std Cost Build-up                       */~
            CH(94)          /* Filler For Rest of Record or Internal Sp*/

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$ = " "
                  str(line2$,62%) = "VPRINPUT: " & str(cms2v$,,8%)
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
               at (16,22), " (4)",                                       ~
               at (16,30), "Change Prices by Range",                     ~
               at (17,22), fac(hex(8c)),   pf12mc$              , ch(39),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)StartOvr",                                         ~
               at (22,14), "(6)Next Vendor"                             ,~
               at (22,29), "(8)Find By Vendor"                          ,~
               at (22,47), "(10)Add By Vendor"                          ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (23,14), "(7)Next Part"                               ,~
               at (23,29), "(9)Find By Part  "                          ,~
               at (23,47), "(11)Add By Part"                            ,~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104060708090a0b0d0e0f101e) & pf12mck$),       ~
               key (keyhit%)

               if keyhit% <> 4% then L40500
                  call "VPRCHGSB" (#01, #02, #03)
                  goto L40110

L40500:        if keyhit% <> 6% then L40514
                  call "READ102" (#2, vencode$, f1%(2%))
                  if f1%(2%) = 1% then L40508
                     vencode$ = " " : goto L40110
L40508:           get #2, vencode$
                  goto L40110

L40514:        if keyhit% <> 7% then L40526
                  call "READ102" (#3, part$, f1%(3%))
                  if f1%(3%) = 1% then L40522
                     part$ = " " : goto L40110
L40522:           get #3, part$
                  goto L40110

L40526:        if keyhit% <> 13% then L40550
                  call "MANUAL" ("VPRINPUT")
                  goto L40110

L40550:        if keyhit% <> 15% then L40590
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

            deffn'102(fieldnr%, edit%)
                  if last_venpart$ = " " then line2$ = " " else          ~
                  line2$ = "Last Vendor: " & last_vendor$ &              ~
                           " Vendor P/N: " & last_venpart$
                  str(line2$,62%) = "VPRINPUT: " & str(cms2v$,,8%)
                  pf3$ = " " : pf3k$ = hex(ff)
                  pf6$ = " " : pf6k$ = hex(ff)
                  if edit% <> 1% then L42101
                      if fieldnr% <> 4% then L42086
                          pf3$ = "(3)Copy" : pf3k$ = hex(03)
L42086:               if fieldnr% < 4% or previous_info_set%=0% then L42101
                          pf6$ = "(6)Previous Part" : pf6k$ = hex(06)
L42101:           if fieldnr% = 0% then goto L42104
                          pf28k$ = hex(ff) : pf28f$ = hex(9c) /* OFF */
                          goto L42110
L42104:           pf28k$ = hex(1c) :  pf28f$ = hex(84)
                  if textid$ = hex(00000000) or textid$ = hex(ffffffff)  ~
                      or textid$ = " " then pf28f$ = hex(8c)
L42110:           if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L42340,         /* Vendor Code      */~
                                    L42340,         /* Vendor's Part Num*/~
                                    L42340,         /* Our Part Number  */~
                                    L42370,         /* Stated Leadtime  */~
                                    L42370,         /* Average Leadtime */~
                                    L42340,         /* Date Last Changed*/~
                                    L42340,         /* Vendor U.O.M.    */~
                                    L42370,         /* Conversion Factor*/~
                                    L42340,         /* Currency Code    */~
                                    L42370,         /* Current List     */~
                                    L42340,         /* Discount         */~
                                    L42370,         /* Current Price    */~
                                    L42370,         /* Next or 'Special'*/~
                                    L42340,         /* Date Effective   */~
                                    L42340,         /* Date Expires     */~
                                    L44340          /* STC Build-up     */
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
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02),                                               ~
                  "Vendor Code ",                                        ~
               at (04,30), fac(lfac$(1%)), vencode$             , ch(09),~
               at (04,49), fac(hex(8c)),   vencodedescr$        , ch(32),~
               at (05,02),                                               ~
                  "Vendor's Part Number",                                ~
               at (05,30), fac(lfac$(2%)), venpart$             , ch(25),~
               at (06,02),                                               ~
                  "Our Part Number",                                     ~
               at (06,20), fac(lfac$(3%)), part$                , ch(25),~
               at (06,49), fac(hex(8c)),   partdescr$           , ch(32),~
               at (07,02),                                               ~
                  "Vendor's Stated Leadtime",                            ~
               at (07,30), fac(lfac$(4%)), venslt$              , ch(03),~
               at (08,02),                                               ~
                  "Vendor's Average Leadtime",                           ~
               at (08,30), fac(lfac$(5%)), venalt$              , ch(03),~
               at (09,02),                                               ~
                  "Date Last Changed",                                   ~
               at (09,30), fac(lfac$(6%)), pricdate$            , ch(08),~
               at (10,02),                                               ~
                  "Vendor Unit of Measure",                              ~
               at (10,30), fac(lfac$(7%)), unit$                , ch(04),~
               at (10,49), "(Stock UOM =      )",                        ~
               at (10,62), fac(hex(8c)),   purunit$             , ch(04),~
               at (11,02),                                               ~
                  "Our Quantity Per Vendor UOM",                         ~
               at (11,30), fac(lfac$(8%)), quan$                , ch(10),~
               at (12,02), "Currency Code",                              ~
               at (12,30), fac(lfac$(9%)), curr$                , ch(04),~
               at (12,49), fac(hex(8c)),   curr_descr$          , ch(32),~
               at (13,02),                                               ~
                  "Current List Price",                                  ~
               at (13,30), fac(lfac$(10%)), list$               , ch(10),~
               at (14,02),                                               ~
                  "Discount % or Fixed Amount",                          ~
               at (14,30), fac(lfac$(11%)), discper$            , ch(06),~
               at (14,37), fac(hex(8c)),   or$                  , ch(04),~
               at (14,42), fac(lfac$(11%)), disc$               , ch(10),~
               at (15,02),                                               ~
                  "Current Price - Vendor UOM",                          ~
               at (15,30), fac(lfac$(12%)), venppu$             , ch(10),~
               at (15,49), fac(hex(8c))  , eachmsg$             , ch(30),~
               at (16,02),                                               ~
                  "Contract or 'Special' Price",                         ~
               at (16,30), fac(lfac$(13%)), nextprice$          , ch(10),~
               at (16,49), fac(hex(8c))  , eachmsg2$            , ch(30),~
               at (17,02),                                               ~
                  "Date 'Special' Price Starts",                         ~
               at (17,30), fac(lfac$(14%)), date_effective$     , ch(08),~
               at (17,56), "Last Modified",                              ~
               at (17,70), fac(hex(8c)),   last_moddate$        , ch(08),~
               at (18,02),                                               ~
                  "Date 'Special' Price Ends",                           ~
               at (18,30), fac(lfac$(15%)), date_expires$       , ch(08),~
               at (18,56), "Modified By  ",                              ~
               at (18,70), fac(hex(8c)),   last_userid$         , ch(03),~
               at (19,02), "Use for Std Cost Build-up?",                 ~
               at (19,30), fac(lfac$(16%)), stc_flag$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,02), fac(hex(8c)), pf3$                           ,~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02), fac(hex(8c)), pf6$                           ,~
               at (24,40), fac(pf28f$),  pf28m$                         ,~
               at (24,20), fac(hex(8c)), pf12$                          ,~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050c0d0f10) & pf28k$ & pf3k$ & pf6k$),     ~
                                                             key(keyhit%)

               if keyhit% <> 13% then L43170
                  call "MANUAL" ("VPRINPUT")
                  goto L42410

L43170:        if keyhit% <> 15% then L43210
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

            deffn'103(fieldnr%, edit%)
                  if last_part$ = " " then line2$ = " " else             ~
                  line2$ = "Last Vendor: " & last_vendor$ &              ~
                           " Part Code: " & last_part$
                  str(line2$,62%) = "VPRINPUT: " & str(cms2v$,,8%)
                  pf3$ = " " : pf3k$ = hex(ff)
                  pf6$ = " " : pf6k$ = hex(ff)
                  if edit% <> 1% then L44102
                      if fieldnr% <> 4% then L44086
                          pf3$ = "(3)Copy" : pf3k$ = hex(03)
L44086:               if fieldnr% < 4% or previous_info_set%=0% then L44102
                          pf6$ = "(6)Previous Part" : pf6k$ = hex(06)
L44102:           if fieldnr% = 0% then goto L44107
                          pf28k$ = hex(ff) : pf28f$ = hex(9c) /* OFF */
                          goto L44110
L44107:           pf28k$ = hex(1c) :  pf28f$ = hex(84)
                  if textid$ = hex(00000000) or textid$ = hex(ffffffff)  ~
                      or textid$ = " " then pf28f$ = hex(8c)
L44110:           if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L44340,         /* Our Part Number  */~
                                    L44340,         /* Vendor Code      */~
                                    L44340,         /* Vendor's Part Num*/~
                                    L44370,         /* Stated Leadtime  */~
                                    L44370,         /* Average Leadtime */~
                                    L44340,         /* Date Last Changed*/~
                                    L44340,         /* Vendor U.O.M.    */~
                                    L44370,         /* Conversion Factor*/~
                                    L44340,         /* Currency Code    */~
                                    L44370,         /* Current List     */~
                                    L44340,         /* Discount         */~
                                    L44370,         /* Current Price    */~
                                    L44370,         /* Next or 'Special'*/~
                                    L44340,         /* Date Effective   */~
                                    L44340,         /* Date Expires     */~
                                    L44340          /* STC Build-up     */
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
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,02),                                               ~
                  "Our Part Number",                                     ~
               at (04,20), fac(lfac$(1%)), part$                , ch(25),~
               at (04,49), fac(hex(8c)),   partdescr$           , ch(32),~
               at (05,02),                                               ~
                  "Vendor Code ",                                        ~
               at (05,30), fac(lfac$(2%)), vencode$             , ch(09),~
               at (05,49), fac(hex(8c)),   vencodedescr$        , ch(32),~
               at (06,02),                                               ~
                  "Vendor's Part Number",                                ~
               at (06,30), fac(lfac$(3%)), venpart$             , ch(25),~
               at (07,02),                                               ~
                  "Vendor's Stated Leadtime",                            ~
               at (07,30), fac(lfac$(4%)), venslt$              , ch(03),~
               at (08,02),                                               ~
                  "Vendor's Average Leadtime",                           ~
               at (08,30), fac(lfac$(5%)), venalt$              , ch(03),~
               at (09,02),                                               ~
                  "Date Last Changed",                                   ~
               at (09,30), fac(lfac$(6%)), pricdate$            , ch(08),~
               at (10,02),                                               ~
                  "Vendor Unit of Measure",                              ~
               at (10,30), fac(lfac$(7%)), unit$                , ch(04),~
               at (10,49), "(Stock UOM =      )",                        ~
               at (10,62), fac(hex(8c)),   purunit$             , ch(04),~
               at (11,02),                                               ~
                  "Our Quantity Per Vendor UOM",                         ~
               at (11,30), fac(lfac$(8%)), quan$                , ch(10),~
               at (12,02), "Currency Code",                              ~
               at (12,30), fac(lfac$(9%)), curr$                , ch(04),~
               at (12,49), fac(hex(8c)),   curr_descr$          , ch(32),~
               at (13,02),                                               ~
                  "Current List Price",                                  ~
               at (13,30), fac(lfac$(10%)), list$               , ch(10),~
               at (14,02),                                               ~
                  "Discount % or Fixed Amount",                          ~
               at (14,30), fac(lfac$(11%)), discper$            , ch(06),~
               at (14,37), fac(hex(8c)),   or$                  , ch(04),~
               at (14,42), fac(lfac$(11%)), disc$               , ch(10),~
               at (15,02),                                               ~
                  "Current Price - Vendor UOM",                          ~
               at (15,30), fac(lfac$(12%)), venppu$             , ch(10),~
               at (15,49), fac(hex(8c))  , eachmsg$             , ch(30),~
               at (16,02),                                               ~
                  "Contract or 'Special' Price",                         ~
               at (16,30), fac(lfac$(13%)), nextprice$          , ch(10),~
               at (16,49), fac(hex(8c))  , eachmsg2$            , ch(30),~
               at (17,02),                                               ~
                  "Date 'Special' Price Starts",                         ~
               at (17,30), fac(lfac$(14%)), date_effective$     , ch(08),~
               at (17,56), "Last Modified",                              ~
               at (17,70), fac(hex(8c)),   last_moddate$        , ch(08),~
               at (18,02),                                               ~
                  "Date 'Special' Price Ends",                           ~
               at (18,30), fac(lfac$(15%)), date_expires$       , ch(08),~
               at (18,56), "Modified By  ",                              ~
               at (18,70), fac(hex(8c)),   last_userid$         , ch(03),~
               at (19,02), "Use for Std Cost Build-up?",                 ~
               at (19,30), fac(lfac$(16%)), stc_flag$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,02), fac(hex(8c)), pf3$                           ,~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02), fac(hex(8c)), pf6$                           ,~
               at (24,20), fac(hex(8c)), pf12$                          ,~
               at (24,40), fac(pf28f$),  pf28m$                         ,~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050c0d0f10) & pf28k$ & pf3k$ & pf6k$),     ~
                                                             key(keyhit%)

               if keyhit% <> 13% then L45170
                  call "MANUAL" ("VPRINPUT")
                  goto L44410

L45170:        if keyhit% <> 15% then L45210
                  call "PRNTSCRN"
                  goto L44410

L45210
*             IF FIELDNR% > 0% THEN RETURN
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'104(s%)
              line2$ = " "
              str(line2$,62%) = "VPRINPUT: " & str(cms2v$,,8%)
              if s% <> 0% then L45331
                 inpmessage$ = "Position the cursor and Press Return "   ~
                             & "to modify."
                 pf16$ = "(16)Print Report"
                 lfac$(1%) = hex(8c)
                 goto L45370
L45331:       inpmessage$ = "Enter Vendor Range and Print Free Text "    ~
                          & "Options."
              pf16$ = "(16)Return      "
              lfac$(1%) = hex(81)


L45370:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Price List by Vendor",                          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Vendor Range"                       ,        ~
               at (06,16), fac(lfac$(1%)),fmvendor$             , ch(09),~
               at (06,27), "TO"                                 ,        ~
               at (06,31), fac(lfac$(1%)),tovendor$             , ch(09),~
               at (07,02), "Include Free Text (Y/N)"            ,        ~
               at (07,27), fac(lfac$(1%)),text$                 , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over"                      ,        ~
               at (22,65), "(13)Instructions"                   ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
               at (24,65), fac(hex(84)), pf16$                  ,        ~
                                                                         ~
               keys(hex(00010d0f10))                            ,        ~
               key (keyhit%)

               if keyhit% <> 13% then L45810
                  call "MANUAL" ("VPRINPUT")
                  goto L45370

L45810:        if keyhit% <> 15% then L45860
                  call "PRNTSCRN"
                  goto L45370

L45860:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   5                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'105(s%)
              line2$ = " "
              str(line2$,62%) = "VPRINPUT: " & str(cms2v$,,8%)
              if s% <> 0% then L46080
                 inpmessage$ = "Position the cursor and Press Return "   ~
                             & "to modify."
                 pf16$ = "(16)Print Report"
                 lfac$(1%) = hex(8c)
                 goto L46140
L46080:       inpmessage$ = "Enter Part Range and Print Free Text "      ~
                          & "Options."
              pf16$ = "(16)Return      "
              lfac$(1%) = hex(81)

L46140:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Price List by Part"                    ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Part Range"                         ,        ~
               at (06,16), fac(lfac$(1%)),fmpart$               , ch(25),~
               at (06,43), "TO"                                 ,        ~
               at (06,47), fac(lfac$(1%)),topart$               , ch(25),~
               at (07,02), "Include Free Text (Y/N)"            ,        ~
               at (07,27), fac(lfac$(1%)),text$                 , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over"                      ,        ~
               at (22,65), "(13)Instructions"                   ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
               at (24,65), fac(hex(84)), pf16$                  ,        ~
                                                                         ~
               keys(hex(00010d0f10))                            ,        ~
               key (keyhit%)

               if keyhit% <> 13% then L46640
                  call "MANUAL" ("VPRINPUT")
                  goto L46140

L46640:        if keyhit% <> 15% then L46740
                  call "PRNTSCRN"
                  goto L46140

L46740:           close ws
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
                                    L51700,         /* Currency Code    */~
                                    L50920,         /* List Price       */~
                                    L50970,         /* Discount         */~
                                    L51230,         /* Current Price    */~
                                    L51310,         /* Next or 'Special'*/~
                                    L51360,         /* Date Effective   */~
                                    L51500,         /* Date Expires     */~
                                    L51770          /* STC Build-up     */
                  return
L50230
*        Test Data for Vendor Code
                call "GETCODE" (#2,vencode$,vencodedescr$,0%,1.3,f1%(2%))
                if f1%(2%) = 1% then return
                errormsg$ = "Vendor Code NOT found or selected"
                return
L50280
*        Test Data for Vendor's Part Number
                if venpart$ > " " and venpart$ <> "?" then L50380
                plowkey$ = vencode$
                miscdescr$ = hex(06) & "Select The Vendor Part to Edit fo~
        ~r Vendor: " & vencode$
                hdr$(1%) = "  Vendor Part Number"
                call "PLOWCODE" (#1, plowkey$,miscdescr$,1009%,2,f1%(1%),~
                                 hdr$())
                if f1%(1%) = 0% then L50450
                goto L50410
L50380:         plowkey$ = str(vencode$) & venpart$
                call "REDALT0" (#1, plowkey$, 2%, f1%(1%))
                if f1%(1%) = 0% then return
L50410:         gosub dataload
                if f1%(1%) = 0% then L50450
                   fieldnr% = 99%
                   return
L50450:         if venpart$ <> " " then return
                errormsg$ = "Vendor Part Number CANNOT be blank"
                return
L50480
*        Test Data for Our Part Number
                if part$ = "?" then part$ = " "
                call "GETCODE" (#3, part$, partdescr$, 0%,.32, f1%(3%))
                if f1%(3%) = 0% then L50560
                get #3 using L50580, purunit$, ptype$
                if ptype$ >= "200" and ptype$ < "500" then return
        REM     ERRORMSG$ = "Not a Purchased Part (Part Type = " &       ~
                            PTYPE$ & ")"
                return
L50560:         errormsg$ = "Part Number NOT on file"
                return
L50580:         FMT POS(74), CH(4), POS(180), CH(3)
L50590
*        Test Data for Vendor's Stated Leadtime
                call "NUMTEST" (venslt$,0,999,errormsg$,0,junk)
                if errormsg$ = " " then return
                errormsg$ = "Invalid Stated Leadtime"
                return
L50640
*        Test Data for Vendor's Average Leadtime
                call "NUMTEST" (venalt$,0,999,errormsg$,0,junk)
                if errormsg$ = " " then return
                errormsg$ = "Invalid Average Leadtime"
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
                call "PLOWCODE" (#4,plowkey$,selectmsg$,9%,.30, f1%(4%))
                if f1%(4%) = 1% then unit$ = str(plowkey$,10%)           ~
                else errormsg$ = "Unknown Unit of Measure Code!"
                return
L50830:         errormsg$ = "Vendor Unit of Measure CANNOT be Blank!"
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
             if list$ = " " then list$ = "0.00"
             call "NUMTEST" (list$,0,9e9,errormsg$,-2.4,list)
                 if errormsg$ <> " " then return
             if list >= 0 then L50950
                errormsg$ = "List Price CANNOT be Negative."
                return
L50950:      if venppu$ = " " then gosub calc_price
             return

L50970
*        Test Data Discount % or Fixed Discount Amount
             discper, disc = 0
             if list$ = "0.00" then return
                 if discper$ = " " then discper$ = "0.00"
                 if disc$    = " " then disc$ = "0.00"
             if discper$ = "0.00" or disc$    = "0.00" then L51010
                errormsg$ = "Enter either Discount % -OR- Fixed Amount,"&~
                             " NOT both" : return
             /* See if we should round to 2 or 4 decimals */
L51010:      slash% = pos(discper$ = "/") : decimals% = 4%
             if slash% = 0% then L51030
                temp$ = str(discper$, slash% + 1%,)
                convert temp$ to decimals%, data goto L51020
                if decimals% = 2% or decimals% = 4% then L51024
L51020:              errormsg$ = "Rounding parameter after '/' must be "&~
                                 "2 or 4." : return
L51024:         if decimals% <> 4% then decimals% = 2%
                discper$ = str(discper$,,slash% - 1%)
L51030:      call "NUMTEST" (discper$,0,99.99,errormsg$,-2.2,discper)
                 if errormsg$ <> " " then return
             call "NUMTEST" (disc$,0,9e7,errormsg$,2.4,disc)
                 if errormsg$ <> " " then return
             gosub calc_price
             return

        calc_price
            if discper = 0 then L51134
                damt = list * (discper / 100%)
                if damt > list then L51190
                    venppu = list - round(damt, decimals%)
                    venppu = round(venppu, decimals%)
                    goto L51150
L51134:     if disc > list then L51190
                venppu = list - round(disc, 4%)
L51150:         call "CONVERT" (venppu, 2.4, venppu$)
                gosub price_each_message
                return
L51190:     errormsg$ = "Discount CANNOT be greater than List Price"
            return

L51230
*        Test Data for Current Price - Vendor UOM
            if venppu$ = " " then venppu = 0
            if venppu$ = " " then gosub calc_price
            call "NUMTEST" (venppu$,0,9e9,errormsg$,-2.4,venppu)
                if errormsg$ <> " " then return
            if venppu >= 0 then L51260
                errormsg$ = "Current Price CANNOT be Negative."
                return
L51260:     gosub price_each_message
            return

L51310
*        Test Data for Next or 'Special' Price
                call "NUMTEST" (nextprice$,0,9e7,errormsg$,-2.4,nextprice)
                if errormsg$ <> " " then return
                gosub price_each2_message
                return
L51360
*        Test Data for Date 'Next Price' Effective
                if date_effective$ <> " " and date_effective$ <> blankdate$ ~
                   then L51440
                if nextprice = 0 then L51420
                errormsg$ = "Effective Date is required when using Spec"&~
                            "ial Price"
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
                if date_expires$ <> " " and date_expires$ <> blankdate$ ~
                   then L51560
                if date_effective$ = " " or date_effective$ = blankdate$ ~
                   then return
                errormsg$ = "Expiration Date is required when Effective"&~
                            " Date is present"
                return
L51560:         call "DATEOK" (date_expires$,exdate%, errormsg$)
                if errormsg$ > " " then return
                if exdate% >= efdate% then L51630
                errormsg$ = "Expiration Date must be equal or greater t"&~
                            "han the Effective Date"
                return
L51630:         call "DATUNFMT" (date_expires$)
                if date_expires$ < date then                             ~
                errormsg$ = "Expiration Date must be equal or greater t"&~
                            "han today's date"
                call "DATEFMT" (date_expires$)
                return

L51700
*        Currency code                       CURR$
            if enabled% = 0% then return
            if curr$ = " " then curr$ = stat$
            call "GETCODE" (#40, curr$, curr_descr$, 1%, 0, f1%(40%))
            if f1%(40%) <> 0% then return
                errormsg$ = "Invalid Currency Code."
                return

L51770
*        Std Cost Flag for Build-up          STC_FLAG$
            if stc_flag$ = " " then stc_flag$ = "N"
            if stc_flag$ = "Y" or stc_flag$ = "N" then L51810
                errormsg$ = "Invalid Entry; Must be 'Y' or 'N'."
                return
L51810:     if stc_flagged% = 1% then return
                if stc_who$ = " " then return
                     if stc_flag$ = "Y" then                             ~
                     errormsg$ = stc_who$ & " is already flagged for " & ~
                                 "Std Cost Build-up."
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
                                    L51700,         /* Currency Code    */~
                                    L50920,         /* Current List     */~
                                    L50970,         /* Discount         */~
                                    L51230,         /* Current Price    */~
                                    L51310,         /* Next or 'Special'*/~
                                    L51360,         /* Date Effective   */~
                                    L51500,         /* Date Expires     */~
                                    L51770          /* STC Build-up     */
                  return

        price_each_message
                call "CONVERT" (venppu/quan,2.7,eachprice$)
                eachmsg$ = eachprice$ & "/" & purunit$
                call "SPCESMSH" (eachmsg$,1%)
                call "PUTPAREN" (eachmsg$)
                return

        price_each2_message
                call "CONVERT" (nextprice/quan,2.4,eachprice2$)
                eachmsg2$ = eachprice2$ & "/" & purunit$
                call "SPCESMSH" (eachmsg2$,1%)
                call "PUTPAREN" (eachmsg2$)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

            deffn'154
                errormsg$ = " "
                call "TESTRNGE" (fmvendor$, tovendor$,                   ~
                                 lovendor$, hivendor$, errormsg$, #02)
                if errormsg$ <> " " then L60240
                if text$ = "Y" or text$ = "N" then L60240
                   errormsg$ = "Free Text must be 'Y' or 'N'"
L60240:         return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 5.                      *~
            *************************************************************

            deffn'155
                errormsg$ = " "
                call "TESTRNGE" (fmpart$, topart$,                       ~
                                 lopart$, hipart$, errormsg$, #03)
                if errormsg$ <> " " then L60360
                if text$ = "Y" or text$ = "N" then L60360
                   errormsg$ = "Free Text must be 'Y' or 'N'"

L60360:         return

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

            end
