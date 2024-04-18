        REM *************************************************************~
            *                                                           *~
            *  IIIII  N   N  V   V  DDDD    SSS   PPPP   L      Y   Y   *~
            *    I    NN  N  V   V  D   D  S      P   P  L      Y   Y   *~
            *    I    N N N  V   V  D   D   SSS   PPPP   L       YYY    *~
            *    I    N  NN   V V   D   D      S  P      L        Y     *~
            *  IIIII  N   N    V    DDDD    SSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * INVDSPLY - Driver to display part movement history.       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/29/86 ! Original                                 ! ERN *~
            * 04/08/86 ! Standard Cost Project Modifications      ! ERN *~
            * 06/01/86 ! Added Call to PIPATCSB & files req'd     ! MJB *~
            * 11/23/87 ! Add generic xref to part # input         ! JDH *~
            * 01/09/88 ! Argument change to HNYDDISP              ! ERN *~
            * 02/23/89 ! Changed READ with hold on SYSFILE2 to    ! MJB *~
            *          !  standard READ, hold not required        !     *~
            * 05/30/89 ! Changed CPRPRICE to record length of 700 ! GGO *~
            * 08/01/90 ! Modified to call HNYMSTSB                ! FMZ *~
            * 06/03/91 ! PRR 11522 Added a call to 'CPRUPDSB' to  ! SID *~
            *          !     check/update(if require) the future  !     *~
            *          !     prices to current prices.            !     *~
            *          !     11633 Modified tacky file channel    !     *~
            *          !     variables.                           !     *~
            * 03/20/92 ! Moved call to CPRUPDSB to HNYPRDSP.      ! JDH *~
            *          ! PRR 11460.  Added call to PIPTSUB.       !     *~
            *          ! PRR 10853.  Added call to BOMXPLOD.      !     *~
            * 06/15/92 ! Added PIP, Shelf, & ATCs to screen.      ! JDH *~
            * 07/24/92 ! MPS/PFM - Added call to HNYUSDSP.        ! MLJ *~
            * 11/10/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 06/29/93 ! PRR 12761.  BOMXPLOD replaced w/BOMBRWSB.! JDH *~
            *          ! PRR 12982.  Added PF21 to RTEDSPLY.      !     *~
            *          ! Added PFKEYSUB for expanded PF prompts.  !     *~
            * 07/08/93 ! Added PF22 on input to allow entry of a  ! MLJ *~
            *          !  Customer part number.  Calls PTXREFSB.  !     *~
            * 09/28/93 ! PF11 Alternates is hilit if alts on file.! JDH *~
            * 02/17/94 ! Added ATC values which honor the horizon.! JDH *~
            * 03/30/94 ! Added PF28 View Serial Numbers in Inv.   ! JDH *~
            * 06/24/96 ! Added temp date var to pass to subroutns ! DER *~
            * 04/06/98 ! Y2K COMPLIANCE AND 60403 CHANGES (NONE)  ! DJD *~
            * 04/20/99 ! Add Primary Bin Loc to 1st Screen(EWD001)! BWS *~
            *************************************************************~
            * 01/30/06 ! (PAR000) CR347 New Display Program for   ! RHH *~
            *          !    New Sub Part Number. New version of   !     *~
            *          !    HNYDSPLY.                             !     *~
            ************************************************************* 

        dim                                                              ~
            atc1$8, atch1$8,             /* ATC 1 Quantity             */~
            atc2$8, atch2$8,             /* ATC 2 Quantity             */~
            bomid$3,                     /* BOM ID                     */~
            bom$(490%)3,                 /* Effective BOM's            */~
            cat$4, catdescr$30,          /* Part Category              */~
            cursor%(2%),                 /* Cursor Location            */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Part Description           */~
            descrp$80,                   /* PLOWCODE Argument          */~
            descr_map(06%),              /* PLOWCODE Argument          */~
            dsnb$1,                      /* Detl, Sumy, Neither, Both? */~
            errormsg$79,                 /* Error message              */~
            generic$16,                  /* Generic Descriptor         */~
            header$(3%)80,               /* PLOWCODE Argument          */~
            horz$4,                      /* ATC Horizon                */~
            i$(24%)80,                   /* Screen Image               */~
            incl_excl(3%),               /* PLOWCODE Argument          */~
            incl_excl$(3%)25,            /* PLOWCODE Argument          */~
            inpmessage$79,               /* Informational Message      */~
            key$50,                      /* Record Key                 */~
            lfac$1,                      /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
/*EWD001*/  loc$8,                       /* Primary Bin Location       */~
            part$25, part1$25,           /* Part Number        (PAR000)*/~
            part_key$45,                 /* New Part Number    (PAR000)*/~   
            sub_part$20,                 /* New Part Numbers   (PAR000)*/~   
            pf$(3%)79, pfkeys$32,        /* PF Keys                    */~
            pf_descr$(32%)32,            /* PF Keys Descriptions       */~
            pip$8,                       /* PIP Quantity               */~
            plowkey$99,                  /* A Plow Key                 */~
            readkey$99,                  /* A Read Key                 */~
            refcode$9,                   /* Cust or Mfg Code  - XREF   */~
            refpart$25,                  /* Cust or Mfg Part  - XREF   */~
            reftype$1,                   /* 'C'ust or 'M'fg   - XREF   */~
            rteid$3,                     /* Route ID                   */~
            serialed$1,                  /* Is Part Serial Numbered?   */~
            shelf$8,                     /* Shelf Quantity             */~
            spec$4,                      /* Special / Obsolete Flag    */~
            text$(196%,1%)70,            /* Text Matrix for TXTDSPLY   */~
            textid$4,                    /* Part Text ID               */~
            tmp_date$8,                  /* Temp date passed to subrout*/~
            type$3,                      /* Part Type                  */~
            uom$4, uomdescr$30,          /* Stocking Unit of Measure   */~
            xfac$(2%)1                   /* Cust Type & Code FAC       */

        dim f2%(32%),                    /* = 0 if the file is open    */~
            f1%(32%),                    /* = 1 if READ was successful */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "REV: 01.00 01/30/06 New Part Number Revision      "
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
            * #1  ! INVMASTR ! Inventory Master File            (PAR000)*~
            * #2  ! TXTFILE  ! System Text File                         *~
            * #3  ! CATEGORY ! Inventory Category Codes File            *~
            * #4  ! STORNAME ! Store Information File                   *~
            * #5  ! INVQUAN  ! Inventory Quantity File          (PAR000)*~
            * #6  ! INVDETAL ! Inventory Details                (PAR000)*~
            * #7  ! INVHSTRY ! Inventory Usage History          (PAR000)*~
            * #8  ! GENCODES ! System General Codes file.               *~
            * #9  ! HNYALTRS ! Alternate Parts File                     *~
            * #10 ! INVPOOL  ! Inventory Pool File              (PAR000)*~
            * #11 ! SYSFILE2 ! System Information File                  *~
            * #12 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #13 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #14 ! CALMASTR ! Planning Production Calendar File        *~
            * #15 ! PIPIN    ! Planned inventory additions detail       *~
            * #16 ! PIPOUT   ! Planned inventory use detail             *~
            * #17 ! DEMMASTR ! Demand Master File                       *~
            * #18 ! PIPCROSS ! hard peg cross reference file            *~
            * #19 ! INVHSTRF ! Inventory History on Fiscal Calendar(PAR000)*~
            * #20 ! HNYGENER ! Generic part xref file                   *~
            * #21 ! CPRPRICE ! Customer Part Prices                     *~
            * #22 ! GLMAIN   ! General Ledger Main File                 *~
            * #23 ! VENDOR   ! Vendor Master File                       *~
            * #24 ! HNYPROC  ! Procurement History File                 *~
            * #25 ! VENPRICE ! Vendor Price Catalogue File              *~
            * #26 ! ENGMASTR ! Engineering Master Filer                 *~
            * #27 ! BOMMASTR ! Bill of Materials Master file            *~
            * #28 ! JBMASTR2 ! Production job master file               *~
            * #29 ! COREXREF ! Core Tracking Cross-Reference file       *~
            * #30 ! RTEMASTR ! STANDARD ROUTING FILE W/ALTERNATE ROUTES *~
            * #31 ! CUSTOMER ! Customer Master File                     *~
            * #32 ! SERMASTR ! Serial Number Tracking Master File       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
                                                           /* (PAR000) */
            select #1,  "INVMASTR",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =  45,                     ~
                        alt key  1, keypos =  122, keylen =   9, dup,    ~
                            key  2, keypos =  110, keylen =   4, dup,    ~
                            key  3, keypos =   46, keylen =  32, dup
                                                           /* (PAR000) */
            select #2,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select #3,  "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select #4,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~
                                                            /* (PAR000) */ 
            select #5,  "INVQUAN",                                       ~
                        varc,     indexed,  recsize =  768,              ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64          ~
                                                            /* PAR000)  */
            select #6,  "INVDETAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  62,                     ~
                        alt key  1, keypos =   63, keylen =   6, dup,    ~
                            key  2, keypos =   69, keylen =   2, dup     ~
                                                            /* (PAR000) */
                                                            /* (PAR000) */
            select #7,  "INVHSTRY",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  50                      ~
                                                            /* (PAR000) */
            select #8,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #9,  "HNYALTRS",                                      ~
                        varc,     indexed,  recsize =  60,               ~
                        keypos =    1, keylen =  33
                                                            /* (PAR000) */
            select #10, "INVPOOL",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =  58
                                                            /* (PAR000) */
            select #11, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select #12, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #13, "SFCUM2",                                        ~
                        varc,     indexed,  recsize =  1985,             ~
                        keypos =    1, keylen =  25

            select #14, "CALMASTR",                                      ~
                        varc,     indexed,  recsize = 1962,              ~
                        keypos =    1, keylen =   2

            select #15, "PIPIN",                                         ~
                        varc,     indexed,  recsize =    60,             ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #16, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #17, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key 1, keypos =10, keylen = 19,              ~
                            key 2, keypos = 1, keylen = 28

            select #18, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33
                                                            /* (PAR000) */
            select #19, "INVHSTRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  52                      ~
                                                            /* (PAR000) */ 
            select #20, "HNYGENER",                                      ~
                        varc,     indexed,  recsize =   100,             ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41

            select #21, "CPRPRICE",                                      ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =     1, keylen =  47

            select #22, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #23,  "VENDOR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos = 1, keylen = 9,                         ~
                         alt key 1, keypos = 10, keylen = 30, dup

            select #24, "HNYPROC",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 134,                                  ~
                         keypos =32, keylen = 40,                        ~
                         alternate key 1, keypos = 7, keylen = 65,       ~
                                   key 2, keypos = 1, keylen = 40, dup,  ~
                                   key 3, keypos =41, keylen = 31, dup

            select #25, "VENPRICE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 10, keylen = 59,                       ~
                         alternate key 1, keypos = 1, keylen = 34, dup,  ~
                                   key 2, keypos =35, keylen = 34

            select #26, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select #27, "BOMMASTR",                                      ~
                        varc, indexed, recsize = 150,                    ~
                        keypos =   26,  keylen = 31,                     ~
                        alt key  1, keypos =    1, keylen = 56

            select #28, "JBMASTR2",                                      ~
                        varc, indexed, recsize = 1300,                   ~
                        keypos =    1, keylen =   8                      ~

            select #29, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #30, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                         keypos =   5, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 35

            select #31, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #32, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (#2,  fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (#3,  fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (#4,  fs%( 4%), f2%( 4%), 0%, rslt$( 4%))
            call "OPENCHCK" (#5,  fs%( 5%), f2%( 5%), 0%, rslt$( 5%))
            call "OPENCHCK" (#6,  fs%( 6%), f2%( 6%), 0%, rslt$( 6%))
            call "OPENCHCK" (#7,  fs%( 7%), f2%( 7%), 0%, rslt$( 7%))
            call "OPENCHCK" (#8,  fs%( 8%), f2%( 8%), 0%, rslt$( 8%))
            call "OPENCHCK" (#9,  fs%( 9%), f2%( 9%), 0%, rslt$( 9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%), 0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%), 0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%), 0%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%), 0%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%), 0%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%), 0%, rslt$(22%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%), 0%, rslt$(23%))
            call "OPENCHCK" (#24, fs%(24%), f2%(24%), 0%, rslt$(24%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%), 0%, rslt$(25%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%), 0%, rslt$(26%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%), 0%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%), 0%, rslt$(28%))
            call "OPENCHCK" (#30, fs%(30%), f2%(30%), 0%, rslt$(30%))
            call "OPENCHCK" (#31, fs%(31%), f2%(31%), 0%, rslt$(31%))
            call "OPENCHCK" (#32, fs%(32%), f2%(32%), 0%, rslt$(32%))

            call "READ100" (#11, "SWITCHS.COR", core_track%)/* SYSFILE2 */
            if core_track% = 1% then   /* Do we need to open COREXREF? */~
                call "OPENCHCK" (#29, fs%(29%), f2%(29%), 0%, rslt$(29%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            tmp_date$ = date
            str(line2$,62) = "INVDSPLY: " & str(cms2v$,,8)

            select printer     /* Required to get proper print file    */
                               /* names for subrtn reports!!!!         */

            call "READ100" (#11, "SWITCHS.HNY", f1%(11))
            if f1%(11) = 1% then get #11 using L09160, dsnb$
L09160:         FMT POS(110), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, descr$, generic$,          ~
                      cat$, catdescr$, type$, uom$, uomdescr$, spec$,    ~
                      pip$, shelf$, atc1$, atc2$, refcode$, refpart$,    ~
                      reftype$, atch1$, atch2$, horz$, loc$ /*EWD001*/
            ref% = 0%
            alts% = 0%
                                                            /* (PAR000) */
            call "PLOWNEXT" (#1, part_key$, 0%, f1%(1%))
            if f1%(1%) = 1% then get #1 using L10130, part$, sub_part$, descr$        ~
                           else let part$, sub_part$, descr$ = " "
L10130:         FMT CH(25), CH(20), CH(32)
                                                            /* (PAR000) */
*        Get which Part Number to Display
L10160:     gosub'101               /* Display & Accept Screen    */
                if keyhit%  =  1% then inputmode
                if keyhit%  = 14% then detail_report
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then L10160
            gosub'151               /* Edit Field for Valid Entry */
                if errormsg$ <> " " then L10160
            goto selections

        detail_report
                                                           /* (PAR000) */
            call "INVDDISP" (" ",   "R", " ", " ", " ", " ", " ", " ",   ~
                             " ", #1, #6)
            goto L10160
                                                           /* (PAR000) */

*        Now get which option is desired to be executed.
        selections
            gosub'111                   /* Display Screen - No Entry   */
        selections_2
                if keyhit%  =  0% then inputmode
                if keyhit%  =  1% then inputmode
                                                           /* (PAR000) */
        REM        if keyhit%  =  3% then core_track
        REM        if keyhit%  =  5% then call_bombrwsb
        REM        if keyhit%  =  6% then call_piptsub
                                                           /* (PAR000) */
                if keyhit%  =  7% then see_master
                if keyhit%  =  8% then movement_display
                if keyhit%  =  9% then usage_summary
                if keyhit%  = 10% then quantity_info
                                                           /* (PAR000) */
        REM        if keyhit%  = 11% then see_alternates
                if keyhit%  = 12% then see_atc
                                                           /* (PAR000) */
        REM        if keyhit%  = 14% then see_prices
                if keyhit%  = 16% then exit_program
                                                           /* (PAR000) */
        REM        if keyhit%  = 21% then call_rtedsply
                if keyhit%  = 26% then display_text
                if keyhit%  = 27% then mps_usage_summary
                if keyhit%  = 28% then see_serial_numbers
                if keyhit%  = 29% then extra_pf_info
                goto selections

        core_track
            call "CORXRFSB" (part$, 2%)                /* Display only */
            goto selections
                                                            /* (PAR000) */
        movement_display
            call "INVDDISP" (part_key$, "D", " ", " ", " ", " ", " ", " ",~
                             " ", #1, #6)
                                                            /* (PAR000) */
            goto selections

        mps_usage_summary
           call "HNYUSDSP" (part$, descr$)
           goto selections

        usage_summary
            call "READ100" (#11, "SWITCHS.HNY", f1%(11))
            if f1%(11) <> 1 then selections
               get #11 using L10550, cal$
L10550:        FMT POS(95), CH(1)
            if cal$ <> " " then L10570
               errormsg$ = "No usage history on file"
               goto selections
L10570:     if cal$ = "G" then L10610
            if cal$ = "F" then L10631
L10590:         kh% = 2
                call "ASKUSER" (kh%, "CHOOSE CALENDAR STRUCTURE","Press P~
        ~F1 for Gregorian Calendar", "- or -", "Press PF2 for Fiscal Calen~
        ~dar.")
                if kh% = 1 then L10610
                if kh% = 2 then L10631
                goto L10590

L10610:                                                   /* (PAR000)  */

            call "INVUDISP" (part_key$, #1, #7, #4, #6)
            goto selections

L10631:                                                   /* (PAR000)  */

            call "INVUDSPF" (part_key$, #1, #19, #4, #6)
            goto selections

        quantity_info                                     /* (PAR000)  */

            call "INVQDISP" (part_key$, #1, #5, #10, #11)
            goto selections


        display_text                                      /* (PAR000)  */
            call "TXTDSPLY" (#2, f2%(2), "001", "Part: " & part_key$ & ", " &~
                             descr$, textid$, text$())
            goto selections


        see_alternates
            plowkey$ = str(part$) & hex(00)
            call "PLOWCODE" (#9, plowkey$, " ", 25%, 0.32, f1%(9))
            if f1%(9) = 0% then selections
                get #9 using L10810, part1$
L10810:              FMT POS(34), CH(25)
                call "READ100" (#1, part1$, f1%(1))
                if f1%(1) = 0% then selections
                     gosub load_part
                     goto selections

        see_atc
            call "PIPATCSB" (part$, #12, #1, #13, #14, #15, #16, #6,     ~
                                    #17, #18)
            goto selections

        see_prices
            key$ = "C" & str(part$)
            call "HNYPRDSP" (part$, #21, #1, #11, #8)
            goto selections

        see_master
                                                       /* (PAR000)    */
            call "INVMSTSB" (part_key$,                                  ~
              #11, /* SYSFILE2 ! System Default Information File      */ ~
               #1, /* INVMASTR ! Inventory Master File        (PAR000)*/ ~
               #3, /* CATEGORY ! Inventory Category File              */ ~
              #22, /* GLMAIN   ! General Ledger Main File             */ ~
               #9, /* HNYALTRS ! Inventory Alternate Parts File       */ ~
              #23, /* VENDOR   ! Vendor Master File                   */ ~
              #24, /* HNYPROC  ! Procurement History File             */ ~
              #25, /* VENPRICE ! Vendor Price Catalogue File          */ ~
              #20, /* HNYGENER ! Generic Part Cross Reference File    */ ~
              #12, /* PIPMASTR ! Planned Inventory Position Master    */ ~
              #26, /* ENGMASTR ! Engineering Master Filer             */ ~
              #15, /* PIPIN    ! PIPIN File                           */ ~
              #16, /* PIPOUT   ! PIPOUT File                          */ ~
               #5, /* INVQUAN  ! Inventory Quantities File    (PAR000)*/ ~
              #13, /* SFCUM2   ! Cumulative Forecast File             */ ~
               #2, /* TXTFILE  ! System Text File                     */ ~
               #8, /* GENCODES ! General Codes File                   */ ~
              #27) /* BOMMASTR ! Bill of Materials Master file        */ ~
                                                       /* (PAR000)    */
            goto selections

        call_piptsub
            call "PIPTSUB" (part$, #11, #15, #16, #12, #1, #28, #17)
            goto selections

        effective_bom
*        Try to get an effective BOM for today
            bomid$ = " "
            call "PIPINDEX" (#11, tmp_date$, dateindex%, ret%)
            if ret% = 1% then L11370 /* Planning Calendar Messed Up!   */
            if dateindex% = 0% then L11370 /* Off Planning Calendar!   */
            readkey$ = str(part$,,25) & "1"
            call "READ102" (#26, readkey$, f1%(26))
            if f1%(26) <> 1% then L11370
            get #26, using L11330, readkey$, bom$()
L11330:        FMT CH(29), 490 * CH(3)
            if str(readkey$,,25) <> str(part$,,25) then L11370
            bomid$ = bom$(dateindex%)
            return

L11370
*        No Effective BOM
            errormsg$ = "ERROR - Problem Finding Effective BOM."
            goto L11570

        call_bombrwsb
            gosub effective_bom
            call "BOMBRWSB" (#27, #26, #11, #1, part$, bomid$, 1%, " ",  ~
                             errormsg$)

            if errormsg$ = " " then goto selections

L11570:     u% = 2%
            call "ASKUSER" (u%, "* * * *  NOTE  * * * *",                ~
                   errormsg$, "Part Number: " & part$,                   ~
                   "Press any key to acknowledge.")
            errormsg$ = " "
            goto selections

        extra_pf_info
            call "PFKEYSUB" (i$(), pf$(), pf_descr$(), keyhit%)
            goto selections_2

        call_rtedsply
            gosub effective_bom
            readkey$ = str(part$,,25) & str(bomid$,,3) & "  0"
            call "READ100" (#27, readkey$, f1%(27%))
                if f1%(27%) = 0% then L11900
            get #27, using L11860, rteid$
L11860:         FMT POS(87), CH(3)
            if rteid$ = " " then L11920
            call "RTEDSPLY" (part$, rteid$, #30, #1)
            goto selections
L11900:         errormsg$ = "ERROR - Effective BOM not on file."
                goto L11570
L11920:         errormsg$ = "ERROR - No Route for Effective BOM - " &    ~
                            bomid$
                goto L11570

        see_serial_numbers
            mat incl_excl = zer  : incl_excl$() = " "
            header$ (1%) = "  Serial Number         Store Lot"
            header$ (3%) = "  All Serial Numbers in Stock for " & part$
            brk% = 25% : kee = 0.80 : dlen = 20
            incl_excl(1%) = 01.01 : incl_excl$(1%) = "2"
            descr_map(1%) = 02.03 : descr_map(2%) = 01.00
            descr_map(3%) = 05.16 : descr_map(4%) = 06.00
            plowkey$ = part$
            descrp$= hex(06) & "When Done with View, Press PF16 to Return"
            call "PLOWCODE" (#32, plowkey$, descrp$, 9000%+brk%, kee,    ~
                             f1%(32%), header$(), dlen, 0, incl_excl(),  ~
                             incl_excl$(), "Y", " ", #32, descr_map())
            goto selections

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads Part Info from HNYMASTR and Describes fields.       *~
            *************************************************************
                                                            /* (PAR000) */
        load_part
            get #1 using L30080, part_key$, descr$, generic$, uom$, cat$, ~
 /*EWD001*/                     textid$, serialed$, loc$, spec$, type$
L30080:         FMT CH(45), CH(32), CH(16), CH(4), POS(110), CH(4), XX(4),~
                    CH(4), POS(151), CH(1), POS(175), CH(8), /*EWD001*/  ~
                    POS(186), CH(4), XX(10), CH(3)
                                                            /* (PAR000) */
            call "DESCRIBE" (#3, cat$, catdescr$, 0%, f1%)
            call "DESCRIBE" (#8, "UOM      " & uom$, uomdescr$, 0%, f1%)

            plowkey$ = part$  /* Test if alternates are on file */
            call "PLOWNEXT" (#9, plowkey$, 25%, alts%)

            call "PIPATCDZ" (part$, tmp_date$, #12, #11, #13, pip%, shelf%,  ~
                             atc1%, atc2%, err%, atch1%, atch2%, horz%)
            horz$ = " "
            if err% <> 1% then L30180
                pip$, shelf$, atc1$, atc2$, atch1$, atch2$ = "Date Err"
                goto L30250
L30180:     if err% <> 2% then L30200
                pip$, shelf$, atc1$, atc2$, atch1$, atch2$ = "Non-Plan"
                goto L30250
L30200:     convert pip%   to pip$,   pic(-#######)
            convert shelf% to shelf$, pic(-#######)
            convert atc1%  to atc1$,  pic(-#######)
            convert atc2%  to atc2$,  pic(-#######)
            convert atch1% to atch1$, pic(-#######)
            convert atch2% to atch2$, pic(-#######)
            convert horz%  to horz$,  pic(####)

L30250:     if core_track% = 0% then return       /* C/T not installed */
            if fs%(29) <> 1% then return    /* COREXREF not open/avail */
*        Read COREXREF on PK & AK1 -- see if this part is there.
            plowkey$ = str(part$) & hex(00)
            call "PLOWALTS" (#29, plowkey$, 0%, 25%, f1%(29))/* Primary*/
            if f1%(29) <> 0% then return
            call "PLOWALTS" (#29, plowkey$, 1%, 25%, f1%(29))   /* AK1 */
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input Screen.                                    *~
            *************************************************************

        deffn'101                                  /* Get Part Number  */
            if ref% = 0% then init(hex(9c)) xfac$()
            if err% = 1% then lfac$ = hex(84) else lfac$ = hex(81)
            gosub set_pf_input
                                                         /* (PAR000)   */
L40150:     accept                                                       ~
               at (01,02), "Inventory Movement & Quantities Display",    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,30), fac(lfac$    ), part$                , ch(25),~
               at (06,57), fac(lfac$    ), sub_part$            , ch(20),~
                                                                         ~
               at (07,02), "Part Description",                           ~
               at (07,30), fac(hex(84))  , descr$               , ch(32),~
                                                                         ~
               at (08,02), "Generic Descriptor",                         ~
               at (08,30), fac(hex(84))  , generic$             , ch(16),~
                                                                         ~
               at (09,02), "Part Category",                              ~
               at (09,30), fac(hex(84))  , cat$                 , ch(04),~
               at (09,49), fac(hex(8c))  , catdescr$            , ch(30),~
                                                                         ~
               at (10,02), "Part Type",                                  ~
               at (10,30), fac(hex(84))  , type$                , ch(03),~
                                                                         ~
               at (11,02), "Stocking Unit of Measure",                   ~
               at (11,30), fac(hex(84))  , uom$                 , ch(04),~
               at (11,49), fac(hex(8c))  , uomdescr$            , ch(30),~
                                                                         ~
               at (12,02), "Special / Obsolete Flag",                    ~
               at (12,30), fac(hex(84))  , spec$                , ch(04),~
                                                                         ~
               at (14,04), "Planning Quantities for xxxxxxxx",           ~
               at (14,28), fac(hex(84))  , date$                , ch(08),~
               at (15,30), "PIP",                                        ~
               at (15,36), fac(hex(84))  , pip$                 , ch(08),~
               at (16,30), "Shelf",                                      ~
               at (16,36), fac(hex(84))  , shelf$               , ch(08),~
               at (17,30), "ATC 1",                                      ~
               at (17,36), fac(hex(84))  , atc1$                , ch(08),~
               at (18,30), "ATC 2",                                      ~
               at (18,36), fac(hex(84))  , atc2$                , ch(08),~
               at (15,50), "With ATC Horizon",                           ~
               at (16,50), "Of xxxx Days",                               ~
               at (16,53), fac(hex(84))  , horz$                , ch(04),~
               at (17,52), fac(hex(84))  , atch1$               , ch(08),~
               at (18,52), fac(hex(84))  , atch2$               , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               at (24,34), fac(xfac$(1%)), reftype$             , ch(01),~
               at (24,37), fac(xfac$(2%)), refcode$             , ch(09),~
                   keys(pfkeys$),  key (keyhit%)
                                                            /* (PAR000) */
               if keyhit% <> 13% then L40550
                  call "MANUAL" ("HNYDSPLY")
                  goto L40150

L40550:        if keyhit% <> 15% then L40560
                  call "PRNTSCRN"
                  goto L40150

L40560:        if keyhit% <> 22% then L40574
                  ref% = 1%
                  init(hex(81)) xfac$()  :  init(hex(84)) lfac$
                  inpmessage$ = "Enter 'C' or 'M' and the Customer or M"&~
                                "anufacturer's Code"
                  goto L40150

L40574:        close ws
               call "SCREEN" addr("C", u%, "I", i$(), cursor%())
               return

        set_pf_input
           if err% = 1% then                                             ~
               inpmessage$ = "Enter 'C' or 'M' and the Customer or Manu"&~
                             "facturer's Code" else                      ~
               inpmessage$ = "Enter Part Number (leave blank to see Par"&~
                             "ts on File."
           pf$(1) = "(1)Next Part                                      "&~
                    "             (13)Instructions"
           pf$(2) = "                (14)Detail Movements Report       "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f10ffffffff00)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Display And Selection Screen.                    *~
            *************************************************************

        deffn'111                                  /* Get Part Number  */
            lfac$ = hex(84)
            gosub set_pf_edit
                                                   /* (PAR000)         */
L42110:     accept                                                       ~
               at (01,02), "Inventory Movement & Quantities Display",    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,30), fac(lfac$    ), part$                , ch(25),~
               at (06,57), fac(lfac$    ), sub_part$            , ch(20),~ 
                                                                         ~
               at (07,02), "Part Description",                           ~
               at (07,30), fac(hex(84))  , descr$               , ch(32),~
                                                                         ~
               at (08,02), "Generic Descriptor",                         ~
               at (08,30), fac(hex(84))  , generic$             , ch(16),~
                                                                         ~
               at (09,02), "Part Category",                              ~
               at (09,30), fac(hex(84))  , cat$                 , ch(04),~
               at (09,49), fac(hex(8c))  , catdescr$            , ch(30),~
                                                                         ~
               at (10,02), "Part Type",                                  ~
               at (10,30), fac(hex(84))  , type$                , ch(03),~
                                                                         ~
               at (11,02), "Stocking Unit of Measure",                   ~
               at (11,30), fac(hex(84))  , uom$                 , ch(04),~
               at (11,49), fac(hex(8c))  , uomdescr$            , ch(30),~
                                                                         ~
               at (12,02), "Special / Obsolete Flag",                    ~
               at (12,30), fac(hex(84))  , spec$                , ch(04),~
/*EWD001*/     at (12,49), "Primary Bin Location",                       ~
/*EWD001*/     at (12,70), fac(hex(84))  , loc$                 , ch(08),~
                                                                         ~
               at (14,04), "Planning Quantities for xxxxxxxx",           ~
               at (14,28), fac(hex(84))  , date$                , ch(08),~
               at (15,30), "PIP",                                        ~
               at (15,36), fac(hex(84))  , pip$                 , ch(08),~
               at (16,30), "Shelf",                                      ~
               at (16,36), fac(hex(84))  , shelf$               , ch(08),~
               at (17,30), "ATC 1",                                      ~
               at (17,36), fac(hex(84))  , atc1$                , ch(08),~
               at (18,30), "ATC 2",                                      ~
               at (18,36), fac(hex(84))  , atc2$                , ch(08),~
               at (15,50), "With ATC Horizon",                           ~
               at (16,50), "Of xxxx Days",                               ~
               at (16,53), fac(hex(84))  , horz$                , ch(04),~
               at (17,52), fac(hex(84))  , atch1$               , ch(08),~
               at (18,52), fac(hex(84))  , atch2$               , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13% then L42640
                  call "MANUAL" ("HNYDSPLY")
                  goto L42110

L42640:        if keyhit% <> 15% then L42750
                  call "PRNTSCRN"
                  goto L42110

L42750:        close ws
               call "SCREEN" addr("C", u%, "I", i$(), cursor%())
               return

        set_pf_edit
           inpmessage$ = "Press PF Key for Desired Display"
                                                         /* (PAR000)   */ 
           pf$(1) = "(1)Next Part                       (8)Movements   "&~
                    "             (13)Instructions"
           pf$(2) = "(26)See Text                 (9)Usage Summary   (1"&~
                    "2)See A.T.C. (15)Print Screen"
           pf$(3) = "(27)MPS Use  (7)Master Data (10)Qtys (28)Ser #s   "&~
                    "             (16)Exit Program"
           str(pf$(1%),63%,1%) = hex(84)  /* Extra PF Info Available */
           gosub set_pf_extra
           pfkeys$ = hex(01ffffffffff0708090aff0c0d0e0f10151a1b1c1d00)
                                                         /* (PAR000)    */
           if serialed$ = "Y" then L43030
                str(pf$(3),38,10) = " "  :  str(pfkeys$,20,1) = hex(ff)
                pf_descr$(28%) = " "
L43030:    if core_track% = 1% and f1%(29) = 1% then goto L43060
                str(pf$(1),14, 7) = " "  :  str(pfkeys$, 3,1) = hex(ff)
                pf_descr$(03%) = " "
L43060:    if textid$ <> hex(ffffffff) and textid$ <> " " and            ~
              textid$ <> hex(00000000) then goto L43110
                str(pf$(2),1,12) = " "
                str(pfkeys$,18,1) = hex(ff)
                pf_descr$(26%) = " "
L43110:    if dsnb$ = "S" or dsnb$ = "B" then L43140
               str(pf$(3),1,13) = " " : str(pfkeys$,19,1) = hex(ff)
               pf_descr$(27%) = " "
L43140:    if alts% = 0% then str(pf$(1%),48%, 1%) = hex(8c)
           if alts% <> 1% then return
                str(pf$(1%),48%, 1%) = hex(84)
                return

        set_pf_extra
            init (" ") pf_descr$()
            pf_descr$(01%) = "Display the Next Part on File   "
            pf_descr$(02%) = "                                "
            pf_descr$(03%) = "View Core Cross References      "
            pf_descr$(04%) = "                                "
            pf_descr$(05%) = "View Effective Bill of Materials"
            pf_descr$(06%) = "Examine the Sources & Uses (PIP)"
            pf_descr$(07%) = "View General Part Information   "
            pf_descr$(08%) = "View Detailed Part Movement Info"
            pf_descr$(09%) = "View Part Usage History         "
            pf_descr$(10%) = "View Part Quantity Information  "
            pf_descr$(11%) = "View List of Alternate Parts    "
            pf_descr$(12%) = "View PIP and ATC Information    "
            pf_descr$(13%) = "View Manual or Re-enter Menus   "
            pf_descr$(14%) = "View Product Selling Prices     "
            pf_descr$(15%) = "Print Screen                    "
            pf_descr$(16%) = "Exit Program                    "
            pf_descr$(17%) = "                                "
            pf_descr$(18%) = "                                "
            pf_descr$(19%) = "                                "
            pf_descr$(20%) = "                                "
            pf_descr$(21%) = "View Route for Effective BOM    "
            pf_descr$(22%) = "                                "
            pf_descr$(23%) = "                                "
            pf_descr$(24%) = "                                "
            pf_descr$(25%) = "                                "
            pf_descr$(26%) = "View Text Associated with Part  "
            pf_descr$(27%) = "View Usage Captured for MPS Info"
            pf_descr$(28%) = "View Serial Numbers in Inventory"
            pf_descr$(29%) = "Expanded PF Key Explanations    "
            pf_descr$(30%) = "                                "
            pf_descr$(31%) = "                                "
            pf_descr$(32%) = "                                "
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Edit Part Number Entered.                                 *~
            *************************************************************

        deffn'151
            errormsg$ = " "

*        Part Number                           PART$
                                                     /* (PAR000)        */
            part_key$ = all(hex(00))
            str(part_key$,1%,25%)  = part$
            str(part_key$,26%,20%) = sub_part$
            if ref% = 1% then L50260                 /* Xref Part Number */
            descr$ = hex(06) & "Select Part for Display"
            if part$ = " " then L50190
                call "READ100" (#1, part_key$, f1%(1%))
                if f1%(1) = 1% then L50230
                     call "HNYGREF" (part$, #20, #1, f1%(1%))
                     if f1%(1%) = 0% then L50190
                          call "READ100" (#1, part$, f1%(1%))
                          if f1%(1%) = 1% then L50230
L50190:     call "GETCODE" (#1, part_key$, descr$, 0%, 0.32, f1%(1%))
            part$     = str(part_key$,1%,25%)
            sub_part$ = str(part_key$,26%,20%)

            if f1%(1%) = 1% then L50230
                errormsg$ = "Sorry, Part Not on File.  Please Re-enter."
                return
L50230:     gosub load_part
            return
                                                     /* (PAR000)         */ 
L50260: REM Edit Xref Info From PF(22)...
            err% = 0%
            if reftype$ = "C" or reftype$ = "M" then L50320
                errormsg$ = "Please Enter 'C' or 'M'"
                err% = 1%
                return
L50320:     if refcode$ = "?" then refcode$ = " "
            if reftype$ = "M" then L50400
            descr$ = hex(06) & hex(84) & "Select Customer's Code"
                call "GETCODE" (#31, refcode$, descr$, 0%, 1, f1%(31%))
                    if f1%(31%) = 1% then L50480
                        errormsg$ = "Customer Code Does NOT Exist"
                        err% = 1%
                        return
L50400:     plowkey$ = "MFG CODES" & str(refcode$)
            descr$ = hex(06) & hex(84) & "Select Manufacturer's Code"
            call "PLOWCODE" (#8, plowkey$, descr$, 9%, 0.30, f1%(8%))
                if f1%(8%) = 1% then L50470
                    errormsg$ = " Manufacturer's Code Does NOT Exist"
                    err% = 1%
                    return
L50470:         refcode$ = str(plowkey$,10%,9%)
L50480: REM Get CMS Part For This Reference Part...
            refpart$ = str(part$)  :  ret% = 0%
            call "PTXREFSB" (1%, reftype$, str(refcode$), str(refpart$), ~
                            " ", part$, descr$, #1, ret%)
            if ret% <> 0% then L50560
                errormsg$ = "No Cross Reference For This Part"
                ref% = 0%
                return
L50560:     reftype$, refcode$ = " "
            init(hex(8c)) xfac$()
            goto L50230


        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
