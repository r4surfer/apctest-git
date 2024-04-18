        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  SSS   W   W  TTTTT   CCC   H   H    *~
            *  B   B  C      K  K  S      W   W    T    C   C  H   H    *~
            *  BBBB   C      KKK    SSS   W W W    T    C      HHHHH    *~
            *  B   B  C      K  K      S  WW WW    T    C   C  H   H    *~
            *  BBBB    CCC   K   K  SSS   W   W    T     CCC   H   H    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKSWTCH - Retrieves switch requested for BCK, AR and SA. *~
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
            * 05/13/88 ! Added Allow Cancel SO to BCK section     ! JDH *~
            * 02/16/89 ! Proj 7890206 Mandatory Adj Reason added. ! JIM *~
            * 08/08/89 ! proj 7880907 Allow under/over shipments  ! LAB *~
            * 11/01/89 ! proj 7880907 Allow under/over shipments  ! JDH *~
            * 03/12/90 ! Added 2nd Screen for three new pick list ! JEF *~
            *          !   Bin Location printing parameters.      ! JEF *~
            * 06/01/90 ! Added Allow Duplicate Invoice Numbers.   ! JDH *~
            * 05/28/91 ! PRR 11906 Acknowledgement @ Cr Release?  ! JIM *~
            * 01/22/92 ! PRR 10609 Default SO Demand Type.        ! JDH *~
            * 06/05/92 ! PRR 12469 Auto-approvals of Export Schdls! JDH *~
            * 06/15/92 ! Added Credit Hold Override at order entry! JDH *~
            *          !  & Offset Days for due date and ship date! JDH *~
            * 08/20/92 ! PRR 12588.  Better primed for AR Read.   ! JDH *~
            * 02/12/93 ! PRRs 10886, 11079 Non-Stock Part option. ! JIM *~
            * 03/16/94 ! PRR 12840.  Use only A/R for Credit Hold.! JDH *~
            * 05/26/94 ! Allow neg. qty invoices to affect invntry! JDH *~
            * 04/04/95 ! Added 3 New Precious Metal Surcharge Flgs! RJH *~
            *          ! Added 2 Print flags for Xref Parts.      !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "BCKSWTCH" (mod$, switch$, swval$, swval, ret%)

*        MOD$    = Module to locate switch for (BCK, AR, SA).
*
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
            mod$3,                       /* Module to locate set for   */~
            readkey$99,                  /* Read key variable          */~
            sw$(2)250,                   /* Switch record for Module   */~
            switch$8,                    /* Switch to retrieve         */~
            swval$10                     /* String Value of switch     */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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

            call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")
            if f2%(1) = 0% then L10000
                ret%  = 1%
                goto L65000


L10000: REM *************************************************************~
            * I N T I A L I Z A T I O N                                 *~
            * --------------------------------------------------------- *~
            * Load switches and reset calling arguments.                *~
            *************************************************************

            if mod$ = "BCK" or mod$ = "AR" or mod$ = "SA" then L10100
                ret% = 2%
                goto L65000

L10100:     gosub load_switchs           /* F1%(1) indicates status    */
                str(sw$()), swval$ = " " : swval = 0 : ret% = 0%

            if mod$ = "BCK" then bck_switches
            if mod$ = "AR"  then ar_switches
            if mod$ = "SA"  then sa_switches

        REM *************************************************************~
            * B C K  -  S A L E S   O R D E R   M O D U L E             *~
            *************************************************************
        bck_switches           /* Sales Order Module Switches          */
            if f1%(1) = 1% then L11090
                put #1 using L11085, 150%,          /* Submit Wait      */~
                                    "A",           /* Submit Class     */~
                                    240%,          /* Idle Time        */~
                                    "m  ",         /* SO Assign        */~
                                    "N",           /* Credit Hold      */~
                                    "N",           /* Auto Allocate    */~
                                    "N",           /* Pick List Prt    */~
                                    "N",           /* BOL Print        */~
                                    "Y",           /* Discounts        */~
                                    "Y",           /* Allow Delete     */~
                                    "N",           /* Mand Adj Reason  */~
                                    "N",           /* Allow overship   */~
                                    "N",           /* Allow closing    */~
                                     0,            /* Dflt Overship %  */~
                                    "N",           /* Print Bins       */~
                                    "N",           /* Include Locations*/~
                                     0%,           /* # Bins to print  */~
                                    "N",           /* Acks @ Cr Release*/~
                                    "1",           /* SO Demand Type   */~
                                    "N",           /* AutoApprove Exprt*/~
                                    "N",           /* Cr Hold Override */~
                                    " ",           /* Offset due date  */~
                                    " ",           /* Offset ship date */~
                                    "N",           /* A/R Only - CrHold*/~
                                    "N",           /* Precious Metal ON*/~
                                    "N",           /* PM surcharge @ SO*/~
                                    "N",           /* PM surcharge @INV*/~
                                    "N",           /* Print Cust Xref  */~
                                    "N"            /* Print Mnfctr Xref*/


L11085:              FMT XX(20), BI(2), CH(1), BI(2), CH(3), 9*CH(1),    ~
                         PD(14,4), CH(1), CH(1), BI(1), CH(1), CH(1),    ~
                         CH(1), CH(1), 2*CH(3), CH(1), 5*CH(1)

L11090:         get #1, str(sw$())

            if switch$ = "SOASSIGN" then bck_soassign
            if switch$ = "CRHOLD  " then bck_crhold
            if switch$ = "ALLOCATE" then bck_allocate
            if switch$ = "PICKLIST" then bck_picklist
            if switch$ = "BOL     " then bck_bol
            if switch$ = "DISCS   " then bck_discs
            if switch$ = "IDLETIME" then bck_idletime
            if switch$ = "SBMTWAIT" then bck_sbmtwait
            if switch$ = "SBMTCLAS" then bck_sbmtclas
            if switch$ = "DELETE  " then bck_delete
            if switch$ = "MANADJRE" then bck_manadjre
            if switch$ = "OVRSHIP " then bck_ovrship
            if switch$ = "CLOSESO " then bck_closeso
            if switch$ = "OVRSHIP%" then bck_ovrship_pct
            if switch$ = "BINPRINT" then bck_binprint
            if switch$ = "INC_LOC " then bck_inc_loc
            if switch$ = "PRNTNUMB" then bck_prntnumb
            if switch$ = "ACKNWLDG" then bck_acknwldg
            if switch$ = "DEM_TYPE" then bck_dem_type
            if switch$ = "XAUTOAPP" then x_auto_apprv
            if switch$ = "OVRRIDCR" then ovrrid_crhld
            if switch$ = "OFFSTDUE" then offset_duedt
            if switch$ = "OFFSTSHP" then offset_shpdt
            if switch$ = "CRHOLDAR" then crhold_aronly
            if switch$ = "PM_ON   " then pm_on_flag
            if switch$ = "PM_SO   " then pm_so_flag
            if switch$ = "PM_INV  " then pm_inv_flag
            if switch$ = "XREF_CUS" then xref_customer
            if switch$ = "XREF_MNF" then xref_manufctr
                ret% = 2%  :  goto L65000

        bck_soassign : swval$ = str(sw$(), 26%, 3%)  :  goto L65000
        bck_crhold   : swval$ = str(sw$(), 29%, 1%)  :  goto yorn
        bck_allocate : swval$ = str(sw$(), 30%, 1%)  :  goto L65000
        bck_picklist : swval$ = str(sw$(), 31%, 1%)  :  goto yorn
        bck_bol      : swval$ = str(sw$(), 32%, 1%)  :  goto yorn
        bck_discs    : swval$ = str(sw$(), 33%, 1%)  :  goto yorn
        bck_delete   : swval$ = str(sw$(), 34%, 1%)  :  goto yorn
        bck_manadjre : swval$ = str(sw$(), 35%, 1%)  :  goto yorn
        bck_ovrship  : swval$ = str(sw$(), 36%, 1%)  :  goto yorn
        bck_closeso  : swval$ = str(sw$(), 37%, 1%)  :  goto yorn
        bck_acknwldg : swval$ = str(sw$(), 49%, 1%)  :  goto yorn
        bck_dem_type : swval$ = str(sw$(), 50%, 1%)  :  goto L65000
        x_auto_apprv : swval$ = str(sw$(), 51%, 1%)  :  goto yorn
        ovrrid_crhld : swval$ = str(sw$(), 52%, 1%)  :  goto yorn
        offset_duedt : swval$ = str(sw$(), 53%, 3%)  :  goto aton
        offset_shpdt : swval$ = str(sw$(), 56%, 3%)  :  goto aton
        crhold_aronly: swval$ = str(sw$(), 59%, 1%)  :  goto yorn
        pm_on_flag   : swval$ = str(sw$(), 60%, 1%)  :  goto yorn
        pm_so_flag   : swval$ = str(sw$(), 61%, 1%)  :  goto yorn
        pm_inv_flag  : swval$ = str(sw$(), 62%, 1%)  :  goto yorn
        xref_customer: swval$ = str(sw$(), 63%, 1%)  :  goto yorn
        xref_manufctr: swval$ = str(sw$(), 64%, 1%)  :  goto yorn

        bck_idletime
            get str(sw$()) using L11320, swval%
L11320:         FMT XX(23), BI(2)
            swval = swval%  :  convert swval% to swval$, pic(###)
            goto L65000
        bck_sbmtwait
            get str(sw$()) using L11360, swval%
L11360:         FMT XX(20), BI(2)
            swval = swval%  :  convert swval% to swval$, pic(###)
            goto L65000
        bck_sbmtclas : swval$ = str(sw$(), 23, 1)  :  goto L65000

        bck_ovrship_pct
            get str(sw$()) using L11430, swval
L11430:         FMT POS(38), PD(14,4)
            call "CONVERT" (swval, -1.2, swval$)
            goto L65000

        bck_binprint : swval$ = str(sw$(), 46, 1)  :  goto L65000
        bck_inc_loc  : swval$ = str(sw$(), 47, 1)  :  goto yorn
        bck_prntnumb
            get str(sw$()) using L11510, swval%
L11510:         FMT XX(47), BI(1)
            swval = swval%  :  convert swval% to swval$, pic(0)
            goto L65000

        REM *************************************************************~
            *  A / R    S W I T C H E S                                 *~
            *************************************************************
        ar_switches
            if f1%(1) = 1% then L12190
                put #1 using L12170, " ",           /* Inv # Store      */~
                                    "N",           /* Post Disc        */~
                                    "FC000001",    /* Next FC #        */~
                                    " ",           /* Shipping Acct    */~
                                    " ",           /* Freight          */~
                                    " ",           /* Sales Tax        */~
                                    " ",           /* F C Acct         */~
                                    " ",           /* Cash In Bank     */~
                                    " ",           /* Cash Discs       */~
                                    " ",           /* Unallowed Disc   */~
                                    "Y",           /* Inventory Active */~
                                    "Y",           /* A/R Active       */~
                                    "ACDGMO",      /* Types to Print   */~
                                    "Y",           /* Allow dup inv #  */~
                                    "N"            /* Neg qty affct inv*/
L12170:              FMT XX(20), CH(3), CH(1), CH(8), 7*CH(9), 2*CH(1),  ~
                         CH(10), CH(1), CH(1)

L12190:         get #1, str(sw$())

            if switch$ = "INVSTORE" then ar_invstore
            if switch$ = "POSTDISC" then ar_postdisc
            if switch$ = "NEXTFC  " then ar_nextfc
            if switch$ = "HNYACTVE" then ar_hnyactve
            if switch$ = "ARACTIVE" then ar_aractive
            if switch$ = "SHIPACCT" then ar_shipacct
            if switch$ = "FRTACCT " then ar_frtacct
            if switch$ = "TAXACCT " then ar_taxacct
            if switch$ = "FCACCT  " then ar_fcacct
            if switch$ = "CASHACCT" then ar_cashacct
            if switch$ = "DISCACCT" then ar_discacct
            if switch$ = "UNALDISC" then ar_unaldisc
            if switch$ = "PRNTTYPE" then ar_prnttype
            if switch$ = "DUP_INV " then ar_dup_inv
            if switch$ = "NEG_INV " then ar_neg_inv
                ret% = 2%  :  goto L65000

        ar_invstore : swval$ = str(sw$(), 21%, 3%)  :  goto L65000
        ar_postdisc : swval$ = str(sw$(), 24%, 1%)  :  goto yorn
        ar_nextfc   : swval$ = str(sw$(), 25%, 8%)  :  goto L65000
        ar_hnyactve : swval$ = str(sw$(), 96%, 1%)  :  goto yorn
        ar_aractive : swval$ = str(sw$(), 97%, 1%)  :  goto yorn
        ar_shipacct : swval$ = str(sw$(), 33%, 9%)  :  goto acct
        ar_frtacct  : swval$ = str(sw$(), 42%, 9%)  :  goto acct
        ar_taxacct  : swval$ = str(sw$(), 51%, 9%)  :  goto acct
        ar_fcacct   : swval$ = str(sw$(), 60%, 9%)  :  goto acct
        ar_cashacct : swval$ = str(sw$(), 69%, 9%)  :  goto acct
        ar_discacct : swval$ = str(sw$(), 78%, 9%)  :  goto acct
        ar_unaldisc : swval$ = str(sw$(), 87%, 9%)  :  goto acct
        ar_prnttype : swval$ = str(sw$(), 98%,10%)  :  goto L65000
        ar_dup_inv  : swval$ = str(sw$(),108%, 1%)  :  goto yorn
        ar_neg_inv  : swval$ = str(sw$(),109%, 1%)  :  goto yorn

        REM *************************************************************~
            *   S  A    S W I T C H E S                                 *~
            *************************************************************
        sa_switches
            if f1%(1) = 1% then L13180
                put #1 using L13160, "G",           /* Gross/Net Sales  */~
                                    "N",           /* SADETAIL         */~
                                    "N",           /* Book Invoices    */~
                                    "N"            /* Update Non-Stock?*/

L13160:              FMT XX(20), 4*CH(1)

L13180:         get #1, str(sw$())

            if switch$ = "SALES   " then ar_sales
            if switch$ = "SADTL   " then ar_sadtl
            if switch$ = "BOOKINV " then ar_bookinv
            if switch$ = "NONSTOCK" then ar_nonstock

                ret% = 2%  :  goto L65000

        ar_sales    : swval$ = str(sw$(), 21, 1)  :  goto L65000
        ar_sadtl    : swval$ = str(sw$(), 22, 1)  :  goto L65000
        ar_bookinv  : swval$ = str(sw$(), 23, 1)  :  goto yorn
        ar_nonstock : swval$ = str(sw$(), 24, 1)  :  goto yorn


        REM *************************************************************~
            * L O A D   S W I T C H E S                                 *~
            * --------------------------------------------------------- *~
            * Move switches from disc to file buffer area               *~
            *************************************************************
        load_switchs
            if ret% < 0% then L30100
                readkey$ = "SWITCHS." & mod$
                if mod$ = "AR" then readkey$ = "SWITCHS.ARM"
                call "READ100" (#1, readkey$, f1%(1))
                return

L30100:     f1%(1) = 0%   /* Caller wants std default, not the   */
            return        /* value that is currently on file     */


        REM *************************************************************~
            * S O M E   S U B R O U T I N E S                           *~
            *************************************************************

        goto aton    /* Save just in case          */
        aton    /* Convert SWVAL$ to SWVAL, if possible.               */
            convert swval$ to swval, data goto L65000  :  goto L65000


        yorn    /* Return SWVAL based on SWVAL$: 'N'=1, 'Y'=2, else 0  */
            if swval$ = "N" then swval = 1
            if swval$ = "Y" then swval = 2
            goto L65000

        acct    /* Return G/L Account after formatting it              */
            call "GLFMT" (swval$)
            goto L65000


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
