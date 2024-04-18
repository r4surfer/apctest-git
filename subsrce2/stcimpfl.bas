        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   IIIII  M   M  PPPP   FFFFF  L       *~
            *  S        T    C   C    I    MM MM  P   P  F      L       *~
            *   SSS     T    C        I    M M M  PPPP   FFFF   L       *~
            *      S    T    C   C    I    M   M  P      F      L       *~
            *   SSS     T     CCC   IIIII  M   M  P      F      LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCIMPFL - Remaps files' cost buckets for main program    *~
            *            STCIMPLE.  May be run in foreground or back-   *~
            *            ground but must start after STCIMPLE has       *~
            *            written out its SYSFILE2 control record.       *~
            *-----------------------------------------------------------*~
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
            * 07/22/87 ! Original                                 ! ERN *~
            * 08/25/92 ! Added CORADDTF from Core Deposit Tracking! JIM *~
            * 06/10/93 ! Added JBMASTC & Adjustments in JBMASTR2  ! KAB *~
            * 12/15/93 ! Restart Logic, if we can                 ! KAB *~
            * 07/14/94 ! Realign BcktsIds in VBKVSA&HNYACTXF Files! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCIMPFL"

        dim                                                              ~
            axd$4,                       /* AXD argument for OPENFILE  */~
            costs$96,                    /* Cost Breakdown String      */~
            file$8,                      /* File Name                  */~
            lib$8,                       /* Library Name               */~
            loc%(25,20),                 /* Costs location mapping     */~
                                         /* LOC%(max files, max occur) */~
            map%(12),                    /* Remapping mapping          */~
            record$(16,128),             /* Data Record area           */~
            rslt$20,                     /* Text from file opening     */~
            vol$6                        /* Volume Name                */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYADDTF ! Additions Buffer for Inventory           *~
            * #2  ! HNYRETTF ! Inventory Returns Transaction File       *~
            * #3  ! HNYWDWTF ! Withdrawal buffer for inv                *~
            * #4  ! JBCREDIT ! Production job credits received detail f *~
            * #5  ! JBMASTR2 ! Production job master file               *~
            * #6  ! JBMATER2 ! Production job material used detail file *~
            * #7  ! JBVALUE2 ! Production job value added detail file   *~
            * #8  ! JBTIF    ! SFC Background Posting Transaction Image *~
            * #9  ! PAYLINES ! Payables Line Item File                  *~
            * #10 ! PAYBUF2  ! Payables Line Item Transaction Image Fil *~
            * #11 ! PORLSE   ! Purchase Order Requisitions file         *~
            * #12 ! QTELINES ! Quotation Line Item File                 *~
            * #13 ! QTEPART  ! Info On Quoted Parts                     *~
            * #14 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #15 ! RCVTIF2  ! Receiver Line Items TIF File  (Purchasin *~
            * #16 ! SHPCOSTS ! Appendix file to ship lines to hold cost *~
            * #17 ! SHPHNYTF ! Shipping-Inventory Transaction File      *~
            * #18 ! VBKLINES ! Purchase Order Line Items File           *~
            * #19 ! VBKCHNGL ! PO Changes History - Line Items          *~
            * #20 ! VBKBUF2  ! PO Buffer- Line Items                    *~
            * #21 ! CORADDTF ! Core Deposit Additions buffer for HNY    *~
            * #22 ! JBMASTRC ! JBMASTR Core Appendix                    *~
            * #23 ! VBKVSA   ! Vendor Service Advice File               *~
            * #24 ! HNYACTXF ! Vendor Service Activity Cross Reference  *~
            * #64 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #64, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #1 , "HNYADDTF",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   6                      ~

            select #2 , "HNYRETTF",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  13                      ~

            select #3 , "HNYWDWTF",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   6                      ~

            select #4 , "JBCREDIT",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos = 23, keylen = 48

            select #5 , "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #6 , "JBMATER2",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos = 23, keylen = 48

            select #7 , "JBVALUE2",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  23                      ~

            select #8 , "JBTIF",                                         ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    9, keylen =  18,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #9 , "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47          ~

            select #10, "PAYBUF2",                                       ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47          ~

            select #11, "PORLSE",                                        ~
                        varc,     indexed,  recsize =  492,              ~
                        keypos =    1, keylen =  66,                     ~
                        alt key  1, keypos =   48, keylen =  19, dup,    ~
                            key  2, keypos =    5, keylen =  62, dup,    ~
                            key  3, keypos =   14, keylen =  53, dup,    ~
                            key  4, keypos =   39, keylen =  28, dup     ~

            select #12, "QTELINES",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    5, keylen =  18,                     ~
                        alt key  1, keypos =    1, keylen =  22,         ~
                            key  2, keypos =   23, keylen =  52          ~

            select #13, "QTEPART",                                       ~
                        varc,     indexed,  recsize =  758,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  30, dup     ~

            select #14, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #15, "RCVTIF2",                                       ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #16, "SHPCOSTS",                                      ~
                        varc,     indexed,  recsize = 1500,              ~
                        keypos =    1, keylen =  23                      ~

            select #17, "SHPHNYTF",                                      ~
                        varc,     indexed,  recsize =  572,              ~
                        keypos =    1, keylen =  46,                     ~
                        alt key  1, keypos =   47, keylen =  80          ~

            select #18, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #19, "VBKCHNGL",                                      ~
                        varc,     indexed,  recsize =  736,              ~
                        keypos =    1, keylen =  35                      ~

            select #20, "VBKBUF2",                                       ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #21, "CORADDTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 6

            select #22, "JBMASTRC",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen = 8

            select #23, "VBKVSA",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    5, keylen =   8,                     ~
                        alt key  6, keypos =   50, keylen =   4, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  1, keypos =    1, keylen =  12

            select #24, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  29,                     ~
                        alt key  1, keypos = 26, keylen = 4 , dup

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#64, c%, f2%, 0%, rslt$)
            if f2% <> 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "READ101" (#64, "STCIMPLE.CONTROL    ", f1%)
            if f1% = 0% then exit_program
                get #64 using L09090, map%(), rslt$, rfiles%
L09090:             FMT POS(30), 12*BI(1), POS(43), CH(1), POS(115), BI(4)
                if rslt$ <> " " then exit_program
                put #64 using L09120, "I"
L09120:              FMT POS(43), CH(1)
                rewrite #64

*        The following variables describe the files that need to be
*        remapped.  The first file channel to be done is #1; the last
*        is #FILES%.  The location of the cost array(s) on the records
*        is defined by putting the starting position(s) in the LOC%
*        array.  The array is subscripted (F,O) where F is the file
*        channel and O is the occurence of a cost array.  For example,
*        if file #7 has a cost array at pos 70 and another at pos 567,
*        the entries would be LOC%(7,1) = 70%, LOC%(7,2) = 567, and
*        LOC(7,3) = 0% (signifying end of occurrences).  If the first
*        occurence has a value of 0% then that file is not processed.

            l1% = dim(loc%(),1%)
            l2% = dim(loc%(),2%)

            files% = 22%    /* Last UFB channel of Reg.files to be done */
            if files% > l1% then exit_program
*        Remember to change DIM of LOC% if necessary
            files% = files% + 2% /* Process Vendor Service Files Bckt ID*/
            if rfiles% >= files% then mark_done

            loc%( 1%,1%) =   88%                           /* HNYADDTF */
            loc%( 2%,1%) =  132%                           /* HNYRETTF */
            loc%( 3%,1%) =   88%                           /* HNYWDWTF */
            loc%( 4%,1%) =   87% : loc%( 4%,2%) =  183%    /* JBCREDIT */
                                   loc%( 4%,3%) =  279%
            loc%( 5%,1%) =  240% : loc%( 5%,2%) =  336%    /* JBMASTR2 */
                                   loc%( 5%,3%) =  432%
                                   loc%( 5%,4%) =  536%
                                   loc%( 5%,5%) =  632%
                                   loc%( 5%,6%) =  728%
                                   loc%( 5%,7%) =  832%
                                   loc%( 5%,8%) =  928%
                                   loc%( 5%,9%) = 1024%
                                   loc%( 5%,10%)= 1155%
            loc%( 6%,1%) =   87% : loc%( 6%,2%) =  191%    /* JBMATER2 */
            loc%( 7%,1%) =   41%                           /* JBVALUE2 */
            loc%( 8%,1%) =  184%                           /* JBTIF    */
            loc%( 9%,1%) =  191%                           /* PAYLINES */
            loc%(10%,1%) =  191%                           /* PAYBUF2  */
            loc%(11%,1%) =  142%                           /* PORLSE   */
            loc%(12%,1%) = 1002%                           /* QTELINES */
            loc%(13%,1%) =  408%                           /* QTEPART  */
            loc%(14%,1%) =  416% : loc%(14%,2%) =  520%    /* RCVLINES */
            loc%(15%,1%) =  416% : loc%(15%,2%) =  520%    /* RCVTIF2  */
            loc%(16%,1%) =   24% : loc%(16%,2%) =  120%    /* SHPCOSTS */
                                   loc%(16%,3%) =  216%
                                   loc%(16%,4%) =  312%
                                   loc%(16%,5%) =  408%
                                   loc%(16%,6%) =  504%
                                   loc%(16%,7%) =  600%
                                   loc%(16%,8%) =  696%
                                   loc%(16%,9%) =  792%
                                   loc%(16%,10%)=  888%
                                   loc%(16%,11%)=  984%
                                   loc%(16%,12%)= 1080%
                                   loc%(16%,13%)= 1176%
                                   loc%(16%,14%)= 1272%
                                   loc%(16%,15%)= 1368%
            loc%(17%,1%) =  410%                           /* SHPHNYTF */
            loc%(18%,1%) =  429%                           /* VBKLINES */
            loc%(19%,1%) =  465%                           /* VBKCHNGL */
            loc%(20%,1%) =  429%                           /* VBKBUF2  */
            loc%(21%,1%) =   88%                           /* CORADDTF */
            loc%(22%,1%) =   17% : loc%(22%,2%)=  113%     /* JBMASTRC */
                                   loc%(22%,3%)=  217%
                                   loc%(22%,4%)=  321%
                                   loc%(22%,5%)=  425%

        REM *************************************************************~
            *          M A I N   P R O G R A M   L O G I C              *~
            * --------------------------------------------------------- *~
            * The hard part of the code has already been done while     *~
            * setting up the files' table in the initialization block   *~
            * above.  Now all we have to do is (a)loop through each     *~
            * file then (b) read in each record then (c) loop through   *~
            * each occurrence of costs and pass them to the remap       *~
            * sub and then (d) rewrite the record.                      *~
            *************************************************************

        for f% = rfiles% + 1% to files%

          /* Open the next file in the queue for re-mapping  */
            if loc%(f%,1%) = 0% then next_file
            call "OPENFILE" (#f%, "IO   ", f2%, rslt$, axd$)
            if f2% <> 0% then next_file
                call "GETNAMES" addr(#f%, file$, lib$, vol$)
                call "READFDR" addr(file$, lib$, vol$, 0%, "RS", rs%, c%)
                call "SHOSTAT" ("Remapping File: " & file$)

            /* Skip over any file header records   */
                if f% = 17% then call "READNXT1" (#f%, f1%)  /*SHPHNYTF*/

            record_loop
                call "READNXT1" (#f%, f1%)
                if f1% = 0% then file_done

                if f% < 23% then L10285
                     goto  reset_bucket_id_for_vendor_service_files
L10285:         get #f%, str(record$(),,rs%)
                for c% = 1% to l2%
                     if loc%(f%,c%) = 0% then L10350
                          costs$ = str(record$(),loc%(f%,c%),96%)
                          call "STCREMAP" (map%(), costs$)
                          str(record$(),loc%(f%,c%),96%) = costs$
                next c%
L10350:         put #f%, str(record$(),,rs%)
                rewrite #f%
                goto record_loop

            file_done
                close #f%

        next_file
*        Log file as completed.
            call "READ101" (#64, "STCIMPLE.CONTROL    ", f1%)
            put #64 using L10460, f%
L10460:         FMT POS(115), BI(4)
            rewrite #64
            next f%

        mark_done
*        Log program as completed.
            call "READ101" (#64, "STCIMPLE.CONTROL    ", f1%)
            put #64 using L10530, "C", files% + 1%
L10530:         FMT POS(43), CH(1), POS(115), BI(4)
            get #64 using L10550, rslt$
L10550:         FMT POS(42), CH(3)
            if rslt$ = "CCC" then delete #64 else rewrite #64

            goto exit_program

        reset_bucket_id_for_vendor_service_files
           /* VBKVSA File */
            if f% > 23% then L11100  /* Do the HNYACTXF File */
            get #f% using L11030, old_bckt%
L11030:       FMT POS(154), BI(4)
            new_bckt% = map%(oldbckt%)
            if new_bckt% = old_bckt% then record_loop /* Back for More */
            put #f% using L11030, new_bckt%
            rewrite #f%
            goto record_loop

           /* HNYACTXF File */
L11100:     if f% > 24% then file_done  /* Shouldn't Happen */
            get #f% using L11120, old_bckt%
L11120:       FMT POS(53), BI(4)
            new_bckt% = map%(oldbckt%)
            if new_bckt% = old_bckt% then record_loop /* Back for More */
            put #f% using L11120, new_bckt%
            rewrite #f%
            goto record_loop

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
