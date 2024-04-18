        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  BBBB   K   K  SSS   W   W  TTTTT   CCC   H   H    *~
            *  V   V  B   B  K  K  S      W   W    T    C   C  H   H    *~
            *  V   V  BBBB   KKK    SSS   W W W    T    C      HHHHH    *~
            *   V V   B   B  K  K      S  WW WW    T    C   C  H   H    *~
            *    V    BBBB   K   K  SSS   W   W    T     CCC   H   H    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKSWTCH - Retrieves switch requested. See Below, LDJ.    *~
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
            * 12/27/85 ! ORIGINAL                                 ! ERN *~
            * 01/04/89 ! Added defaults and setups for Rcv Reject ! MJB *~
            *          !  Codes and Default Receiving Mode        !     *~
            * 12/12/89 ! Added coding for receiving at std cost   ! JBK *~
            * 01/29/92 ! PRR 10715 - Added coding for Quantity to ! MLJ *~
            *          !  Pay to Exclude QC Quantity.             !     *~
            * 05/21/92 ! PRR 12028 - Added switches to "Print     ! MLJ *~
            *          !  Revision Date on PO" and "Print Received!     *~
            *          !  and Net Due Qtys on PO".                !     *~
            * 09/23/92 ! Added Receiver Ticket Size Option.       ! JDH *~
            * 02/23/93 ! Added Defer Cost at Directive time.      ! JDH *~
            * 06/17/94 ! Added 'Generate VSAs' option.            ! ERN *~
            * 06/20/94 ! Added 'Active VSAs'& 'Print VSAs' Option ! RJH *~
            * 11/18/94 ! Removed 'Print VSAs' Option.             ! LDJ *~
            * 04/04/95 ! PRR 13378 - Added switches to "Kit       ! JDH *~
            *          !  Complete for Purchase Jobs?".           !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "VBKSWTCH" (switch$, swval$, swval, ret%)

*        SWITCH$ = Keyword of switch to find.
*
*        SWVAL$  = String value of switch.  Returned blank if switch
*                  could not be derived.  Subroutine attempts to format
*                  VAL into VAL$ if switch is stored in a numeric format.
*
*        SWVAL   = Numeric value of switch.  Returned 0 if switch
*                  could not be derived -OR- if VAL$ converts to zero.
*                  IF switch is an Y/N then VAL returns as 1 if N,
*                  2 if Y (else 0).
*
*        RET%    = 0 - All A-OK fine.
*                  1 - Could not open SYSFILE2
*                  2 - Invalid value in SWITCH$
*                  IF PASSED IN < 0 WILL RETURN DEFAULT SETTING

        dim                                                              ~
            sw$(2)250,                   /* Switch record for Module   */~
            switch$8,                    /* Switch to retrieve         */~
            swval$10                     /* String Value of switch     */

        dim f2%(01),                     /* = 0 if the file is open    */~
            f1%(01)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            call "OPENCHCK" (#1, 0%, f2%(1%),   0%, " ")
            if f2%(1%) = 0% then L10000
                ret%  = 1%
                goto exit_program


L10000: REM *************************************************************~
            * I N T I A L I Z A T I O N                                 *~
            * --------------------------------------------------------- *~
            * Load switches and reset calling arguments.                *~
            *************************************************************

            gosub load_switchs           /* F1%(1) indicates status    */
                str(sw$()), swval$ = " " : swval = 0 : ret% = 0%

        REM *************************************************************~
            * V B K  -  P U R C H A S I N G   M O D U L E               *~
            *************************************************************

            if f1%(1) = 1% then L11090
                put #1 using L11070, 150%, "A", 240%, "m  ", "50.00", "N",~
                                    "1", "N", "0", " ", " ", "N", "N",   ~
                                    "N", "N", "N", "C", "N", "N", "N",   ~
                                    "Y"
L11070:              FMT XX(20), BI(2), CH(1), BI(2), CH(3), CH(5),      ~
                         16*CH(1)
L11090:         get #1, str(sw$())

            if switch$ = "POASSIGN" then vbk_poassign
            if switch$ = "RCPTSLMT" then vbk_rcptslmt
            if switch$ = "POHISTRY" then vbk_pohistry
            if switch$ = "POSORT  " then vbk_posort
            if switch$ = "POISWAS " then vbk_poiswas
            if switch$ = "RCVRPRNT" then vbk_rcvrprnt
            if switch$ = "IDLETIME" then vbk_idletime
            if switch$ = "SBMTWAIT" then vbk_sbmtwait
            if switch$ = "SBMTCLAS" then vbk_sbmtclas
            if switch$ = "POPROPT " then vbk_popropt
            if switch$ = "REJCODES" then vbk_rejcodes
            if switch$ = "RCVMODE " then vbk_rcvmode
            if switch$ = "POSTDCST" then vbk_postdcst
            if switch$ = "QTYTOPAY" then vbk_qtytopay
            if switch$ = "POPRDATE" then vbk_poprdate
            if switch$ = "POPRPND " then vbk_poprpnd
            if switch$ = "TKTSIZE " then rcv_tktsize
            if switch$ = "DEFERCST" then dir_defercost
            if switch$ = "VSA GEN " then vsa_generate
            if switch$ = "VSA ACTV" then vsa_active
            if switch$ = "PJ KIT C" then pj_kit_comp
                ret% = 2%  :  goto exit_program

        vbk_poassign : swval$ = str(sw$(), 26, 3)  :  goto exit_program
        vbk_rcptslmt : swval$ = str(sw$(), 29, 5)  :  goto aton
        vbk_pohistry : swval$ = str(sw$(), 34, 1)  :  goto yorn
        vbk_posort   : swval$ = str(sw$(), 35, 1)  :  goto aton
        vbk_poiswas  : swval$ = str(sw$(), 36, 1)  :  goto yorn
        vbk_rcvrprnt : swval$ = str(sw$(), 37, 1)  :  goto aton
        vbk_idletime
            get str(sw$()) using L11320, swval%
L11320:         FMT XX(23), BI(2)
            swval = swval%  :  convert swval% to swval$, pic(###)
            goto exit_program
        vbk_sbmtwait
            get str(sw$()) using L11360, swval%
L11360:         FMT XX(20), BI(2)
            swval = swval%  :  convert swval% to swval$, pic(###)
            goto exit_program
        vbk_sbmtclas : swval$ = str(sw$(), 23, 1)  :  goto exit_program
        vbk_popropt  : swval$ = str(sw$(), 38, 1)  :  goto aton
        vbk_rejcodes : swval$ = str(sw$(), 40, 1)  :  goto yorn
        vbk_rcvmode  : swval$ = str(sw$(), 39, 1)  :  goto exit_program
        vbk_postdcst : swval$ = str(sw$(), 41, 1)  :  goto yorn
        vbk_qtytopay : swval$ = str(sw$(), 42, 1)  :  goto yorn
        vbk_poprdate : swval$ = str(sw$(), 43, 1)  :  goto yorn
        vbk_poprpnd  : swval$ = str(sw$(), 44, 1)  :  goto yorn
        rcv_tktsize  : swval$ = str(sw$(), 45, 1)  :  goto tkt_size
        dir_defercost: swval$ = str(sw$(), 46, 1)  :  goto yorn
        vsa_active   : swval$ = str(sw$(), 47, 1)  :  goto yorn
        vsa_generate : swval$ = str(sw$(), 48, 1)  :  goto yorn
        pj_kit_comp  : swval$ = str(sw$(), 49, 1)  :  goto yorn

        REM *************************************************************~
            * L O A D   S W I T C H E S                                 *~
            * --------------------------------------------------------- *~
            * Move switches from disc to file buffer area               *~
            *************************************************************
        load_switchs
            if ret% < 0% then L30100
                if f1%(1) = 1% then return         /* Already loaded */
                     call "READ100" (#1, "SWITCHS.VBK", f1%(1))
                     return

L30100:     f1%(1) = 0%   /* Caller wants std default, not the   */
            return        /* value that is currently on file     */


        REM *************************************************************~
            * S O M E   S U B R O U T I N E S                           *~
            *************************************************************

        aton    /* Convert SWVAL$ to SWVAL, if possible.               */
            convert swval$ to swval, data goto exit_program
            goto exit_program

        yorn    /* Return SWVAL based on SWVAL$: 'N'=1, 'Y'=2, else 0  */
            if swval$ = "N" then swval = 1
            if swval$ = "Y" then swval = 2
            goto exit_program

        tkt_size /* Return SWVAL based on SWVAL$: 'A'=1, 'B'=3, 'C'=6   */
            if swval$ = "A" then swval = 1
            if swval$ = "B" then swval = 3
            if swval$ = "C" then swval = 6
            goto exit_program

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN
        exit_program
            end
