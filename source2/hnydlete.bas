        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  DDDD   L      EEEEE  TTTTT  EEEEE   *~
            *  H   H  NN  N   Y Y   D   D  L      E        T    E       *~
            *  HHHHH  N N N    Y    D   D  L      EEEE     T    EEEE    *~
            *  H   H  N  NN    Y    D   D  L      E        T    E       *~
            *  H   H  N   N    Y    DDDD   LLLLL  EEEEE    T    EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYDLETE - Part Deletion program (with lots of checks).   *~
            *          - Attempts to verify if part to be deleted is in *~
            *            use. If not deletes part from system. If in use*~
            *            displays all the areas of use.                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/02/84 ! ORIGINAL                                 ! ERN *~
            * 09/17/85 ! ADDED DELETE TO TEXT IN TXTFILE.         ! ERN *~
            * 02/06/86 ! Added check against Part being counted   ! LDJ *~
            *          !   (Physical Inventory module), now calls !     *~
            *          !   renamed version of PRTNUSE - now       !     *~
            *          !   HNYINUSE. Prettied up the screens.     !     *~
            * 03/12/86 ! Added subroutine CDANPOST in data save.  ! LDJ *~
            * 05/09/86 ! Changed VENPRICE Select Statement, small ! LDJ *~
            *          !   screen changes.                        !     *~
            * 09/19/86 ! Added delete to pricing record           ! ERN *~
            * 10/04/86 ! Added checking of Sales Analysis Files   ! ERN *~
            * 02/02/87 ! Special Pricing Expansion                ! ERN *~
            * 03/02/87 ! Add check for Serial numbers             ! JIM *~
            * 05/13/87 ! Std Cost Changes                         ! ERN *~
            * 03/16/88 ! Delete part from all standard cost sets. ! JIM *~
            * 02/02/89 ! Proj 7880714 changed CPRPRICE record     ! JIM *~
            *          !   length to 700 for eff date on customer !     *~
            *          !   pricing - effective in R6.00.00.       !     *~
            *          !   Has no impact on processing.           !     *~
            * 06/09/89 ! Added Check for Option Selection         ! MJB *~
            * 02/27/90 ! Added deletion of ROPHNY record, type "C"! LAB *~
            *          ! and "S" records in CPRCURCY & type "S" in! LAB *~
            *          ! CPRPRICE                                 ! LAB *~
            * 03/09/90 ! Added 4th alt. Key for HNYLOCNS          ! JEF *~
            * 04/10/90 ! Fixed PF-13 from 1st screen.             ! MJB *~
            * 05/30/90 ! Delete HNYOPTN2 file records. Did away   ! JDH *~
            *          !  with STDDETAL; it's obsolete. Opened    !     *~
            *          !  files exclusively.                      !     *~
            * 05/24/91 ! PRR 11963 REM'd 'extraneous' SHOSTATs.   ! JIM *~
            * 05/28/91 ! ALLFREE. 06/13/91 Removed STRTRLSEs.     ! JIM *~
            * 10/30/91 ! CMS/DEC 'MASK' Project                   ! SID *~
            * 07/20/92 ! Added Cycle Count Files deletion logic.  ! RJH *~
            * 08/14/92 ! Added MPS/PFM (#40-#45) deletion Logic.  ! MLJ *~
            * 09/03/92 ! Chged MPS/PFM (#41&#43) deletion Logic.  ! JDH *~
            * 09/29/92 ! MPS/PFM - outputs 'H' source code & sets ! MLJ *~
            *          !   Old, New & Net qty's to zero in audit  !     *~
            *          !   file.                                  !     *~
            * 11/10/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 12/01/92 ! Check for Use as Core Item.              ! KAB *~
            *          !   Remove long obsolete files (#20, #21)  !     *~
            * 08/03/93 ! Added Cus Part X-Ref file (CUSPTXRF).    ! JDH *~
            * 02/01/94 ! Added Option to Delete if SA info is there JDH *~
            * 03/08/94 ! Changed record length for BOMSPEC and    ! WPH *~
            *          ! removed HNYOPTN2 file & autoreplace logic!     *~
            * 12/14/94 ! Changed to respect SIZERUNS file.        ! WPH *~
            * 01/27/95 ! Delete Precious Metal Records (HNYPMTBL) ! RJH *~
            *************************************************************

        dim                                                              ~
            a_key$7,                     /* Usage Audit - Prime Key    */~
            a_moddate$6,                 /* Usage Audit - Mod Date     */~
            a_modid$3,                   /* Usage Audit - Mod ID       */~
            a_text$(2)45,                /* Usage Audit - Mod Text     */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            hit%(1),                     /* Flag for Size Run got a hit*/~
            inpmessage$79,               /* INPUT MESSAGE              */~
            inuse$3,                     /* IN USE FLAG (YES/NO)       */~
            key$100,                     /* MISC KEY USAGE             */~
            line2$79,                    /* Second line of Screen      */~
            lfac$(1)1,                   /* Field Fac Variable         */~
            mode$5,                      /* Mode for opening files     */~
            part$25,                     /* PART CODE                  */~
            partdesc$32,                 /* PART DESCRIPTION           */~
            plowkey$100,                 /* MISC KEY USAGE             */~
            prname$8,                    /* PR Name for SA Summary     */~
            readkey$50,                  /* READ KEY                   */~
            record$(72)25,               /* Size Run Parts Array       */~
            sa_option$1,                 /* Delete if SA info there?   */~
            set$8,                       /* Std Cost Set name          */~
            text$4, textid$4,            /* TEXT ID XREFs              */~
            use$(16)75                   /* DISPLAY ARRAY FOR INUSE MSG*/

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *  DISPLAY WARNING SCREEN AND ALLOW EXIT BEFORE THIS GETS   *~
            *  TOO SERIOUS.                                             *~
            *************************************************************

L01982:     gosub warnscreen
                mode$ = "IO   "
                if keyhit% = 16% then end
                if keyhit% = 17% then mode$ = "SHARE"
                if keyhit% <> 0% and keyhit% <> 17% then L01982


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! HNYALTRS ! Inventory Alternate Part File            *~
            * #03 ! HNYDETAL ! Inventory detail file                    *~
            * #04 ! HNYGENER ! generic xref file                        *~
            * #05 ! HNYMAST2 ! Inventory Master File Appendix           *~
            * #06 ! HNYOPTNS ! Stores possible Options for a paticular  *~
            * #07 ! HNYPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #08 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #09 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #10 ! PIPIN    ! Planned inventory additions detail       *~
            * #11 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #12 ! BOMMASTR ! BOM relationship file                    *~
            * #13 ! RTEMASTR ! Production routing master file           *~
            * #14 ! CUSPTXRF ! Customer Part Number Cross Reference file*~
            * #15 ! SIZERUNS ! Size runs matrix file for BCKs           *~
            * #16 ! ENGMASTR ! Engineering Master Filer                 *~
            * #17 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #18 ! SFMASTR2 ! Sales forecast master file               *~
            * #19 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #22 ! TXTFILE  ! System Text File                         *~
            * #23 ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #24 ! HNYLOCNS ! Stock location master file               *~
            * #25 ! CPRPRICE ! Customer Pricing File                    *~
            * #26 ! SASUMRY# ! Sales Analysis Summary File              *~
            * #27 ! SYSFILE2 ! System Information File                  *~
            * #28 ! SERMASTR ! Serial Number Master File                *~
            * #29 ! SERDETAL ! Serial Number Detail File                *~
            * #30 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #31 ! STCBOMXF ! Standard Cost / BOM-RTE Cross Reference  *~
            * #32 ! BOMSPEC  ! Option Specifications                    *~
            * #33 ! ROPHNY   ! Reorder Point Inventory file             *~
            * #34 ! CPRCURCY ! Customer/Currency Pricing File           *~
            * #35 ! HNYCCMST ! Cycle Count Master File                  *~
            * #36 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #40 ! HNYUADET ! MPS/PFM Actual Usage Detail File         *~
            * #41 ! HNYUASUM ! MPS/PFM Actual Usage Summary File        *~
            * #42 ! HNYURDET ! MPS/PFM Requested Usage Detail File      *~
            * #43 ! HNYURSUM ! MPS/PFM Requested Usage Summary File     *~
            * #44 ! MPSITEMS ! MPS/PFM Items Master File                *~
            * #45 ! HNYUAUDT ! MPS/PFM Usage Detail Audit File          *~
            * #50 ! COREXREF ! Core Tracking Cross-Reference file       *~
            * #51 ! HNYPMTBL ! Precious Metal Item Table                *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #02, "HNYALTRS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =    1, keylen =  33                      ~

            select #03, "HNYDETAL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup     ~

            select #04, "HNYGENER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41          ~

            select #05, "HNYMAST2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  227,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #06, "HNYOPTNS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =    1, keylen =  54                      ~

            select #07, "HNYPOOL",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  38                      ~

            select #08, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44         ~

            select #09, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #10, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #11, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #12, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #13, "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #14, "CUSPTXRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   26, keylen =  35,                     ~
                        alt key  1, keypos =    1, keylen =  60          ~

            select #15, "SIZERUNS",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  14

            select #16, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            select #17, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #18, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #19, "VENPRICE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   256,                                 ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34          ~

            select #22, "TXTFILE",                                       ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 1, keylen = 11

            select #23, "HNYPITKT",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  42, dup,    ~
                            key  2, keypos =  313, keylen =  16          ~

            select #24, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42          ~

            select #25, "CPRPRICE",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =     1, keylen =  47

            select #26, "SASUMRY#",                                      ~
                        varc, indexed, recsize = 1048,                   ~
                        keypos =      1, keylen =  56,                   ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #27, "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos =      1, keylen =  20

            select #28, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #29, "SERDETAL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

            select #30, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

            select #31, "STCBOMXF",                                      ~
                        varc,     indexed,  recsize = 72,                ~
                        keypos =   29, keylen =  33,                     ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select #32, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #33, "ROPHNY",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen = 4, dup

            select #34  "CPRCURCY",                                      ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =     1, keylen =  51

            select #35, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup     ~

            select #36, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize = 436,               ~
                        keypos =    1,  keylen = 41,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup     ~

            select #40, "HNYUADET",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1,  keylen = 39,                     ~
                        alt key  1, keypos =   34, keylen =   6, dup,    ~
                            key  2, keypos =  147, keylen =   6, dup,    ~
                            key  3, keypos =    4, keylen =  25, dup

            select #41, "HNYUASUM",                                      ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =    4, keylen =  41,         ~
                            key  2, keypos =   37, keylen =   8, dup

            select #42, "HNYURDET",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1,  keylen = 39,                     ~
                        alt key  1, keypos =   34, keylen =   6, dup,    ~
                            key  2, keypos =  147, keylen =   6, dup,    ~
                            key  3, keypos =    4, keylen =  25, dup

            select #43, "HNYURSUM",                                      ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =    4, keylen =  41,         ~
                            key  2, keypos =   37, keylen =   8, dup

            select #44, "MPSITEMS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1,  keylen = 25,                     ~
                        alt key  1, keypos =   26, keylen =   8, dup,    ~
                            key  2, keypos =   34, keylen =   6, dup,    ~
                            key  3, keypos =   40, keylen =   2, dup,    ~
                            key  4, keypos =   42, keylen =  33

            select #45, "HNYUAUDT",                                      ~
                        varc,     indexed,  recsize =  250,              ~
                        keypos =    1,  keylen =  7,                     ~
                        alt key  1, keypos =    8, keylen =  40, dup

            select #50, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            select #51, "HNYPMTBL",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 35,                      ~
                        alt key  1, keypos =   26, keylen =  10, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, mode$, f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, mode$, f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, mode$, f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#04, mode$, f2%(04), rslt$(04), axd$(04))
            call "OPENFILE" (#05, mode$, f2%(05), rslt$(05), axd$(05))
            call "OPENFILE" (#06, mode$, f2%(06), rslt$(06), axd$(06))
            call "OPENFILE" (#07, mode$, f2%(07), rslt$(07), axd$(07))
            call "OPENFILE" (#08, mode$, f2%(08), rslt$(08), axd$(08))
            call "OPENFILE" (#09, mode$, f2%(09), rslt$(09), axd$(09))
            call "OPENFILE" (#10, mode$, f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, mode$, f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, mode$, f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#13, mode$, f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#14, mode$, f2%(14), rslt$(14), axd$(14))
            call "OPENFILE" (#15, mode$, f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, mode$, f2%(16), rslt$(16), axd$(16))
            call "OPENFILE" (#17, mode$, f2%(17), rslt$(17), axd$(17))
            call "OPENFILE" (#18, mode$, f2%(18), rslt$(18), axd$(18))
            call "OPENFILE" (#19, mode$, f2%(19), rslt$(19), axd$(19))
            call "OPENFILE" (#23, mode$, f2%(23), rslt$(23), axd$(23))
            call "OPENFILE" (#24, mode$, f2%(24), rslt$(24), axd$(24))
            call "OPENFILE" (#25, mode$, f2%(25), rslt$(25), axd$(25))
            call "OPENFILE" (#27, "SHARE", f2%(27), rslt$(27), axd$(27))
            call "OPENFILE" (#28, mode$, f2%(28), rslt$(28), axd$(28))
            call "OPENFILE" (#29, mode$, f2%(29), rslt$(29), axd$(29))
            call "OPENFILE" (#31, mode$, f2%(31), rslt$(31), axd$(31))
            call "OPENFILE" (#32, mode$, f2%(32), rslt$(32), axd$(32))
            call "OPENFILE" (#33, mode$, f2%(33), rslt$(33), axd$(33))
            call "OPENFILE" (#34, mode$, f2%(34), rslt$(34), axd$(34))
            call "OPENFILE" (#35, mode$, f2%(35), rslt$(35), axd$(35))
            call "OPENFILE" (#36, mode$, f2%(36), rslt$(36), axd$(36))
            call "OPENFILE" (#40, mode$, f2%(40), rslt$(40), axd$(40))
            call "OPENFILE" (#41, mode$, f2%(41), rslt$(41), axd$(41))
            call "OPENFILE" (#42, mode$, f2%(42), rslt$(42), axd$(42))
            call "OPENFILE" (#43, mode$, f2%(43), rslt$(43), axd$(43))
            call "OPENFILE" (#44, mode$, f2%(44), rslt$(44), axd$(44))
            call "OPENFILE" (#45, mode$, f2%(45), rslt$(45), axd$(45))
            call "OPENFILE" (#50, mode$, f2%(50), rslt$(50), axd$(50))
            call "OPENFILE" (#51, mode$, f2%(51), rslt$(51), axd$(51))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            call "EXTRACT" addr("ID", a_modid$)
            a_moddate$, date$ = date
            call "DATEFMT" (date$)

            a_text$(1) = "Part Number Deleted Through HNYDLETE"
            edtmessage$ = "Enter Part Code to be Deleted then Press (RETU~
        ~RN)."

            gosub set_sa_file
            goto L10000
*        Find out which SA SUMMARY file to do checking with and open it
        set_sa_file
            readkey$ = "SA.FILES.SASUMRY" & hex(00)
L09140:     call "PLOWNEXT" (#27, readkey$, 16%, f1%(27))
            if f1%(27) = 0% then return
                get #27 using L09170, prname$, group1%, group2%
L09170:              FMT XX(9), CH(8), POS(53), 2*BI(1)
                if group1% <> 1% and group2% <> 1% then L09140
                     if group1% = 1% then sakey% = 2%
                     if group2% = 1% then sakey% = 1%
                     call "PUTPRNAM" addr (#26, prname$)
                     call "OPENFILE" (#26, mode$, f2%(26), rslt$(26),    ~
                                                              axd$(26))


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * GET PART NUMBER TO BE DELETED.                            *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, part$, partdesc$, sa_option$
            call "ALLFREE"
L10120:     gosub'101(1%)
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L10120
            gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
            gosub'101(0%)
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L10120

        REM *************************************************************~
            *        T E S T   I F   P A R T   I N   U S E              *~
            *                                                           *~
            *  CALL IN USE CHECK SUB-ROUTINE TO DETERMINE IF WE CAN     *~
            *  BLOW THIS SUCKER AWAY                                    *~
            *************************************************************
            gosub   check_sizeruns
            print at(21,2,79);" "
            call "SHOSTAT" ("Checking to see if Part Code is in use")
            save_sakey% = sakey%
            if pos("YK" = sa_option$) > 0% then sakey% = 0%
            call "HNYINUSE" (part$, inuse$,use$(),#8, #12, #13, #10, #11,~
                #23, #26, sakey%, #28, #32, #50)
            sakey% = save_sakey%
            if inuse$ = "NO" then goto confirmdelete

        REM *****  SHOW WHY PART CAN NOT BE DELETED  *********

            gosub inusescreen
                if keyhit% = 16 then goto L65000
L10340:     goto inputmode

        REM ***** CONFIRM IF REALLY TO DELETE PART OR NOT ******
        confirmdelete

            gosub confirmscreen
            if keyhit% <> 12% then L10340

            REM ******  GET RID OF PART  ********

                gosub undatasave
                goto inputmode

        EJECT
        REM *************************************************************~
            *       U N - S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * DELETE PART CODE WHERE-EVER WE DECIDED TO DELETE IT FROM. *~
            *************************************************************

        undatasave
        call "SHOSTAT" ("** Deleting Part Code from active data base **")

            gosub    hnymastr
            gosub    cprprice
            gosub    hnyaltrs
            gosub    hnydetal
            gosub    hnygener
            gosub    hnymast2
            gosub    hnyoptns
            gosub    hnypool
            gosub    hnyquan
            gosub    venprice
            gosub    engmastr
            gosub    pipmastr
            gosub    sfcum2
            gosub    sfmastr2
            gosub    txtfile
            gosub    hnylocns
            gosub    sermastr
            gosub    stchny_stcbomxf
            gosub    rophny
            gosub    hnyccmst
            gosub    hnyccdtl
            gosub    hnyuadet
            gosub    hnyuasum
            gosub    hnyurdet
            gosub    hnyursum
            gosub    mpsitems
            gosub    corexref
            gosub    cusptxrf
            gosub    sasummry
            gosub    hnypmtbl
        return

        EJECT
        REM *************************************************************~
            *    F I L E   A C C E S S I N G   R O U T I N E S          *~
            *************************************************************

        hnymastr
        REM CALL "SHOSTAT" ("Removing from Parts Master               ")
            delete #1
            call "CDANPOST" (#1, "D")
            return

        cprprice
        REM CALL "SHOSTAT" ("Removing from Customer Prices            ")
            key$ = "C" & part$
            call "DELETE" (#25, key$, 26%)
            plowkey$ = "S"
L30110:     call "PLOWNXT1" (#25, plowkey$, 1%, f1%(25%))
            if f1%(25%) = 0 then L30160
            key$ = plowkey$
            if str(key$,13,25) = part$ then call "DELETE" (#25, key$, 37%)
            goto L30110
L30160:     plowkey$ = "C"
L30170:     call "PLOWNXT1" (#34, plowkey$, 1%, f1%(34%))
            if f1%(34%) = 0 then L30220
            key$ = plowkey$
            if str(key$,6,25) = part$ then call "DELETE" (#34, key$, 30%)
            goto L30170
L30220:     plowkey$ = "S"
L30230:     call "PLOWNXT1" (#34, plowkey$, 1%, f1%(34%))
            if f1%(34%) = 0 then L30240
            key$ = plowkey$
            if str(key$,17,25) = part$ then call "DELETE" (#34, key$, 41%)
            goto L30230
L30240:     return

        hnyaltrs
        REM CALL "SHOSTAT" ("Removing from Alternate Parts file")
            call "DELETE" (#2, part$, 25%)         /* PRIMARY KEY STUFF */

            key$ = " "
            hnyaltrsloop
                call "PLOWNEXT" (#2, key$, 0%, f1%(2))
                     if f1%(2) = 0% then return
                get #2 using L30350, part1$
L30350:              FMT XX(33), CH(25)
                if part1$ <> part$ then goto hnyaltrsloop
                call "DELETE" (#2, key$, 33%)
                goto hnyaltrsloop

        hnydetal
        REM CALL "SHOSTAT" ("Removing from Inventory Detail file")
            call "DELETE" (#3, part$, 25%)
            return

        hnygener
        REM CALL "SHOSTAT" ("Removing from Generics file")
            call "DELETE" (#4, part$, 25%)
            return

        hnymast2
        REM CALL "SHOSTAT" ("Removing from Alternate Master file")
            call "DELETE" (#5, part$, 25%)
            return

        hnyoptns
        REM CALL "SHOSTAT" ("Removing from Optional Parts file")
            key$ = " "
            str(key$,29) = part$
            call "DELETE" (#6, key$, 53%)         /* STANDARD LIST     */
            call "DELETE" (#6, part$, 25%)        /* AS ASSEMBLY PARENT*/

            key$ = " "
            hnyoptnsloop
                call "PLOWNXT1" (#6, key$, 0%, f1%(6))
                     if f1%(6) = 0% then return
                get #6 using L30670, part1$
L30670:              FMT XX(54), CH(25)
                if part1$ = part$ then delete #6
                goto hnyoptnsloop

        hnypool
        REM CALL "SHOSTAT" ("Removing from Inventory Pool file")
            call "DELETE" (#7, part$, 25%)
            return

        hnyquan
        REM CALL "SHOSTAT" ("Removing Inventory Quantity records")
            call "DELETE" (#8, part$, 25%)
            return

        venprice
        REM CALL "SHOSTAT" ("Removing Vendor Prices for Part")
            call "DELETE" (#19, part$, 25%)
            return

        engmastr
        REM CALL "SHOSTAT" ("Removing from Calendar file")
            call "DELETE" (#16, part$, 25%)
            return

        pipmastr
        REM CALL "SHOSTAT" ("Removing Planned Inventory Position record")
            call "DELETE" (#9, part$, 25%)
            return

        sfcum2
        REM CALL "SHOSTAT" ("Removing Cumulative Sales Forecasting")
            call "DELETE" (#17, part$, 25%)
            return

        sfmastr2
        REM CALL "SHOSTAT" ("Removing Detail Sales History records")
            call "DELETE" (#18, part$, 25%)
            return

        txtfile
        REM CALL "SHOSTAT" ("Removing Part Text lines")
            call "TXTFUTIL" (#22, f2%(22), "DELE", text$)
            return

        hnylocns
        REM CALL "SHOSTAT" ("Removing Part Locations information")
            key$ = part$
L32840:     call "PLOWAL1" (#24, key$, 3%, 25%, f1%(24))
            if f1%(24) = 0% then return
            delete #24
            goto L32840

        sermastr
        REM CALL "SHOSTAT" ("Removing Serial # Masters & Details")
            key$ = str(part$,,25) & hex(00)
            call "DELETE" (#28, key$, 25%)
            key$ = str(part$,,25) & hex(00)
            call "DELETE" (#29, key$, 25%)
            return

        stchny_stcbomxf
        REM The part is deleted from all cost set STCHNY files (STCnnnnH).
        REM ... also from the STCBOMXF file.
        REM CALL "SHOSTAT" ("Removing Standard Cost Set entries")
            key$ = "STC.HDR." & hex(00)

        get_next_cost_set    /* Plow SYSFILE2 for all Cost Set headers */
            call "PLOWNEXT" (#27, key$, 8%, f1%(27))
                if f1%(27) = 0% then return /* Done with SYSFILE2 plow */
            set$ = str(key$,9,8)        /* Else get Std Cost Set hdr...*/
            call "STCFOPEN" (set$, "S     ", #27, errormsg$,             ~
                #30, #30, #30, #30, #30, #30) /*...open the STCHNY file*/
            if errormsg$ <> " " then goto get_next_cost_set
            call "DELETE" (#30, part$, 25%)       /* STCHNY (STCnnnnH) */

            call "READ101" (#31, str(set$) & part$, f1%(31))
                if f1%(31) <> 0% then delete #31           /* STCBOMXF */
            goto get_next_cost_set

        rophny
        REM CALL "SHOSTAT" ("Removing Reorder Point Master records")
            call "DELETE" (#33, part$, 25%)
            return

        hnyccmst
        REM CALL "SHOSTAT" ("Removing Cycle Count Master records")
            call "DELETE" (#35, part$, 25%)
            return

        hnyccdtl
        REM CALL "SHOSTAT" ("Removing Cycle Count Session records")
L33310:     plowkey$ = "P" &  part$ & hex(00)
            call "PLOWAL1" (#36, plowkey$, 1%, 26%, f1%(36%))
            if f1%(36%) = 0%   then  return
            delete #36
            goto L33310
            return

        hnyuadet
            if f2%(40%) <> 0% then return
               adet_count% = 0%
        REM    CALL "SHOSTAT" ("Removing Actual Usage Detail Records")
            plowkey$ = str(part$)
L33440:     call "REDALT1" (#40, plowkey$, 3%, f1%(40%))
            if f1%(40%) = 0% then return
                delete #40
                adet_count% = adet_count% + 1%
                goto L33440

        hnyuasum
            if f2%(41%) <> 0% then return
        REM    CALL "SHOSTAT" ("Removing Actual Usage Summary Records")
            plowkey$ = str(part$)
L33540:     call "PLOWAL1" (#41, plowkey$, 1%, 25%, f1%(41%))
            if f1%(41%) = 0% then return
                delete #41
                goto L33540

        hnyurdet
            if f2%(42%) <> 0% then usage_audit
            rdet_count% = 0%
        REM     CALL "SHOSTAT" ("Removing Requested Usage Detail Records")
            plowkey$ = str(part$)
L33640:     call "REDALT1" (#42, plowkey$, 3%, f1%(42%))
            if f1%(42%) = 0% then usage_audit
                delete #42
                rdet_count% = rdet_count% + 1%
                goto L33640

        hnyursum
            if f2%(43%) <> 0% then return
        REM    CALL "SHOSTAT" ("Removing Requested Usage Summary Records")
            plowkey$ = str(part$)
L33740:     call "PLOWAL1" (#43, plowkey$, 1%, 25%, f1%(43%))
            if f1%(43%) = 0% then return
                delete #43
                goto L33740

        mpsitems
            if f2%(43%) <> 0% then return
        REM     CALL "SHOSTAT" ("Removing MPS Items Master Records")
            plowkey$ = str(part$)
            call "READ101" (#44, plowkey$, f1%(44%))
                if f1%(44%) <> 1% then return
            delete #44
            return

        usage_audit
            if adet_count% = 0% and rdet_count% = 0% then return
            convert adet_count% to temp1$, pic(####)
L33910:     convert rdet_count% to temp2$, pic(####)
            a_text$(2) = "Deleted " & temp1$ & " Actual & " & temp2$ &   ~
                         " Req. Usage Records"
            call "GETDTTM" addr(a_key$)
            put #45 using L33940, a_key$, " ", part$, " ", 0, 0, 0, "H",  ~
                    a_moddate$, a_modid$, a_text$(1), a_text$(2), " "
L33940:     FMT CH(7), CH(4), CH(25), CH(11), 3*PD(14,4), CH(1), CH(6),  ~
                CH(3),  2*CH(45), CH(79)
            write #45, data goto L33910
            return

        corexref
            init (hex(00)) key$ : str(key$,,25%) = part$
            call "PLOWNXT1" (#50, key$, 25%, f1%(50))
            if f1%(50) = 0% then L34130              /* No Reman Record */
            get #50 using L34090, textid$
L34090:         FMT POS(166), CH(4) /* File #50- COREXREF Text Pointer */
            delete #50                              /* Delete COREXREF */
            call "TXTFUTIL" (#22, f2%(22), "DELE", textid$)  /* & text */

L34130:     init (hex(20)) key$ : str(key$,,25%) = part$
            call "REDALT1" (#50, key$, 1%, f1%(50))
            if f1%(50) = 0% then return             /* No Core Record  */
            get #50 using L34170, textid$
L34170:         FMT POS(166), CH(4) /* File #50- COREXREF Text Pointer */
            delete #50                              /* Delete COREXREF */
            call "TXTFUTIL" (#22, f2%(22), "DELE", textid$)  /* & text */
            return

        cusptxrf
        REM CALL "SHOSTAT" ("Removing Customer Part X-Refs")
            key$ = part$
L34370:     call "PLOWAL1" (#14, key$, 1%, 25%, f1%(14%))
            if f1%(14%) = 0% then return
            delete #14
            goto L34370

        hnypmtbl
            call "DELETE" (#51, part$, 25%)
            return

        sasummry
            if sa_option$ <> "Y" then return
        REM CALL "SHOSTAT" ("Removing Sales Analysis Summary")
            readkey$ = "SA.FILES.SASUMRY" & hex(00)
L34580:     call "PLOWNEXT" (#27, readkey$, 16%, f1%(27%))
            if f1%(27%) = 0% then L34750
                get #27 using L34610, prname$, group1%, group2%
L34610:              FMT XX(9), CH(8), POS(53), 2*BI(1)
                if group1% <> 1% and group2% <> 1% then L34580
                     if group1% = 1% then sakey% = 2%
                     if group2% = 1% then sakey% = 1%
                     if f2%(26%) = 0% then close #26
                     call "PUTPRNAM" addr (#26, prname$)
                     call "OPENFILE" (#26, mode$, f2%(26%), rslt$(26%),  ~
                                                              axd$(26%))
*        Now we've got a summary file that uses part, so blow them away
            plowkey$ = part$
L34700:     call "PLOWAL1" (#26, plowkey$, sakey%, 25%, f1%(26%))
                if f1%(26%) = 0% then L34580
            delete #26
            goto L34700

L34750
*        Now set up SA file for next part, just like before
            if f2%(26%) = 0% then close #26
            gosub set_sa_file
            return

        check_sizeruns
            call "SHOSTAT"("Searching for Part in Size Run Matrices")
            init (hex(00)) readkey$
            call "PLOWNEXT"(#15, readkey$, 0%, f1%(15))
                goto L34850
L34840:     call "READNEXT"(#15, f1%(15))
L34850:         if f1%(15%) = 0% then return
            get #15, using L34870, record$()
L34870:         FMT POS(225), 72 * CH(25)

            search str(record$(),1%) = str(part$,1%,25%)                 ~
                                             to hit%()    step 25%
            if hit%(1) = 0% then L34840  /* read next record */
            k% = 2%
            call "ASKUSER"(k%,"* * * C A N ' T  D E L E T E * * *",      ~
            "This part is mentioned in a Size Run Matrix.  Run SZRMATIN"&~
            " to ",                                                      ~
            "remove it from the matrix and then run this program again.",~
              "Press any key to acknowledge")

            return clear all
            goto inputmode

        EJECT
        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * GET PART CODE TO BE EXECUTED.                             *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "HNYDLETE: " & cms2v$
                if fieldnr% = 1% then lfac$(1) = hex(81)                 ~
                else                  lfac$(1) = hex(84)
                if fieldnr% = 1% then inpmessage$ = "Enter the Part Code ~
        ~to DELETE"                                                       ~
                else inpmessage$ = "Press (RETURN) to See If Eligible for~
        ~ Deletion Or PF(1) to Re-enter"

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Delete Part Code From System",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part Code",                                           ~
               at (06,30), fac(lfac$(1)),part$                  , ch(25),~
               at (07,02),                                               ~
                  "Part Description",                                    ~
               at (07,30), fac(hex(84)), partdesc$              , ch(32),~
               at (09,02), "Purge if SA Summary? (N=No, Y=Yes, K=Yes but ~
        ~Keep Summary):",                                                 ~
               at (09,62), fac(lfac$(1)),sa_option$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L40570
                  call "MANUAL" ("HNYDLETE")
                  goto L40220

L40570:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40220

        REM *************************************************************~
            *   D I S P L A Y   I N   U S E   S C R E E N               *~
            *                                                           *~
            * SHOW WHY PART CAN'T BE DELETED.                           *~
            *************************************************************
        inusescreen
                  line2$ = "Part: " & part$ & "  " & partdesc$
                  str(line2$,62%) = "HNYDLETE: " & cms2v$
                  inpmessage$ = "Press (RETURN) to Acknowledge & Continue"

        accept                                                           ~
               at (01,02), "Delete Part Code From System",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,03),                                               ~
        "This Part May Not be Deleted Because of the Following:",        ~
               at (05,03), fac(hex(84)), use$(1)       , ch(75),         ~
               at (06,03), fac(hex(84)), use$(2)       , ch(75),         ~
               at (07,03), fac(hex(84)), use$(3)       , ch(75),         ~
               at (08,03), fac(hex(84)), use$(4)       , ch(75),         ~
               at (09,03), fac(hex(84)), use$(5)       , ch(75),         ~
               at (10,03), fac(hex(84)), use$(6)       , ch(75),         ~
               at (11,03), fac(hex(84)), use$(7)       , ch(75),         ~
               at (12,03), fac(hex(84)), use$(8)       , ch(75),         ~
               at (13,03), fac(hex(84)), use$(9)       , ch(75),         ~
               at (14,03), fac(hex(84)), use$(10)      , ch(75),         ~
               at (15,03), fac(hex(84)), use$(11)      , ch(75),         ~
               at (16,03), fac(hex(84)), use$(12)      , ch(75),         ~
               at (17,03), fac(hex(84)), use$(13)      , ch(75),         ~
               at (18,03), fac(hex(84)), use$(14)      , ch(75),         ~
               at (19,03), fac(hex(84)), use$(15)      , ch(75),         ~
               at (20,03), fac(hex(84)), use$(16)      , ch(75),         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(000d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L41570
                  call "MANUAL" ("HNYDLETE")
                  goto inusescreen

L41570:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto inusescreen


        REM *************************************************************~
            * CONFIRM DELETE SCREEN                                     *~
            *                                                           *~
            * LAST CHANCE SCREEN BEFORE THIS GUY GOES BYE-BYE           *~
            *************************************************************
        confirmscreen
            inpmessage$ = "****  PLEASE CONFIRM DELETION ****"
                  line2$ = "Part: " & part$ & "  " & partdesc$
                  str(line2$,62%) = "HNYDLETE: " & cms2v$

L42070:     accept                                                       ~
               at (01,02), "Delete Part Code From System",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part Code",                                           ~
               at (06,30), fac(hex(84)), part$                  , ch(25),~
               at (07,02),                                               ~
                  "Part Description",                                    ~
               at (07,30), fac(hex(84)), partdesc$              , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02),                                               ~
                  "(RETURN) Do Not Delete Part ",                        ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65),                                               ~
                  "(12)DELETE PART!",                                    ~
                                                                         ~
               keys(hex(000c0f)),                                        ~
               key (keyhit%)

               if keyhit% <> 13 then L42400
                  call "MANUAL" ("HNYDLETE")
                  goto L42070

L42400:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto confirmscreen

        REM *************************************************************~
            *  FRONT-END WARNING SCREEN                                 *~
            *                                                           *~
            *  DISPLAY WHO, WHAT, WHERE, WHEN & WHY BEFORE ANY HARM IS  *~
            *  PERPETRATED.                                             *~
            *************************************************************
        warnscreen

        accept                                                           ~
               at (01,03),                                               ~
        "*****************  HNYDLETE: DELETE PART CODE FROM SYSTEM  *****~
        ~************",                                                   ~
               at (02,03),                                               ~
        "* This Program MUST be run with NO ONE ELSE on the System! Any a~
        ~ttempt to  *",                                                   ~
               at (03,03),                                               ~
        "* access any Part Code will be Denied as long as this program is~
        ~ running.  *",                                                   ~
               at (04,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (05,03),                                               ~
        "Program execution is as follows:",                              ~
               at (06,03),                                               ~
        "   (1) Enter the Part Code to be deleted.",                     ~
               at (07,03),                                               ~
        "   (2) The System will then attempt to determine if that part is~
        ~, in fact,",                                                     ~
               at (08,03),                                               ~
        "       Inactive.",                                              ~
               at (09,03),                                               ~
        "   (3) If Usage of the Part is detected, that Usage(s) is displa~
        ~yed.",                                                           ~
               at (10,03),                                               ~
        "   (4) If No Usage for the Part is found, You are given one last~
        ~ chance to",                                                     ~
               at (11,03),                                               ~
        "       abort. If You continue, the Part will be DELETED.",      ~
               at (12,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (13,03),                                               ~
        "*        DELETION CRITERIA            !        WHAT IS DELETED  ~
        ~           *",                                                   ~
               at (14,03),                                               ~
        "*-------------------------------------!-------------------------~
        ~-----------*",                                                   ~
               at (15,03),                                               ~
        "* - No BOMS or Routes. No AutoReplcmnt! - Part Master File Data ~
        ~           *",                                                   ~
               at (16,03),                                               ~
        "* - No Inventory, On-Order, In-Process! - Vendor Price Informati~
        ~on         *",                                                   ~
               at (17,03),                                               ~
        "* - No Demand For Part. No SA History ! - Customer Price Informa~
        ~tion       *",                                                   ~
               at (18,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (19,03),                                               ~
        "****    If in doubt, DON'T DELETE THE PART.  Subsequent re-use o~
        ~f the   ****",                                                   ~
               at (20,03),                                               ~
        "****    Part Code may cause history for the 'old' Part to appear~
        ~ for    ****",                                                   ~
               at (21,03),                                               ~
        "****    the 'new' Part.                                         ~
        ~        ****",                                                   ~
               at (22,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (23,03),                                               ~
        "PF KEYS:",                                                      ~
               at (24,03),                                               ~
        "(RETURN)Continue    (13)Instructions    (15)Print Screen    (16)~
        ~Exit Program",                                                   ~
                                                                         ~
               keys(hex(000d0f1011)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L43840
                  call "MANUAL" ("HNYDLETE")
                  goto warnscreen

L43840:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto warnscreen

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * CHECK THAT PART ON FILE AND GET DESCRIPTION.              *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                partdesc$ = hex(06) & "Select Part Code to Delete "
                call "GETCODE" (#1, part$, partdesc$, 0%, 0, f1%(1))
                if f1%(1) = 1% then L50081
                     errormsg$ = "Part Code not on file"      : return
L50081:         call "READ101" (#1, part$, f1%(1))
                get #1 using L50091, text$
L50091:              FMT POS(98), CH(4)
                if sa_option$ = " " then sa_option$ = "N"
                if pos("NYK" = sa_option$) > 0% then L50200
                     errormsg$ = "Invalid entry.  SA Option must be " &  ~
                                 "'N', 'Y' or 'K'."
L50200:         return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One moment please")
            end
