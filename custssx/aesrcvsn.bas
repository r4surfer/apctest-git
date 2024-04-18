        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   EEEEE   SSSS  RRRR    CCC   V   V   SSS    CCC    *~
            *  A   A  ER     S      R   R  C   C  V   V  S   S  C       *~
            *  AAAAA  EEE    SSSS   RRRR   C      V   V  SSS    C       *~
            *  A   A  E          S  R   R  C   C   V V       S  C       *~
            *  A   A  EEEEE  SSSS   R   R   CCC     V    SSSS    CCC    *~
            * -- <PF28> Overrides Restrictions for Admins           --  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/01/04 ! Original                                 ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "AESRCVSN" (vendor$,         /* Vendor Code                */~
                        po$,             /* Purchase Order Number      */~
                        line$,           /* Purchase Order Line        */~
                        qty_rec,         /* Qty Received               */~
                        serial_number$,  /* Serial Number              */~
                        #1,              /* RCVSCN                     */~
                        #2,              /* RCVSCN2                    */~
                        err%  )          /* Error Code                 */

        dim readkey$28,                  /* Read key                   */~
            vendor$9,                    /* Vendor Code                */~
            po$16,                       /* Purchase order number      */~
            line$3,                      /* PO Line Number             */~
            bol$30,                      /* Bill of Lading             */~
            cardescr$30,                 /* Carrier Description        */~
            carvencode$9,                /* Carrier Vendor Code        */~
            datetime$8,                  /* Date and Time Stamp        */~
            line$3,                      /* PO line number             */~
            ohpost$1,                    /* Receipt mode option        */~
            po$16,                       /* PO Number                  */~
            rcvddate$8,                  /* Default Received Date      */~
            rcvtime$8,                   /* Time of Receipt            */~
            rcvcomment$30,               /* Whatever                   */~
            userid$3                     /* Current User Id            */


        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! RCVSCN   ! Receiver Master File TIF                 *~
            * #2  ! RCVSCN2  ! Receiver Line Items  TIF                 *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *


REM            select #1 , "RCVSCN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos= 12, keylen = 16,                         ~
                        alt key 1, keypos = 1, keylen = 11              

REM            select #2 , "RCVSCN2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 1, keylen = 28

REM         call "OPENCHCK" (#1 , fs%(1), f2%(1), 100%, rslt$(1))
REM         call "OPENCHCK" (#2 , fs%(2), f2%(2), 300%, rslt$(2))



            init(" ") readkey$
            str(readkey$,1%,16%) = po$

            call "READ101" (#1, readkey$, f1%(1))

            call "GETDTTM" addr(str(datetime$,2,7))
            str(datetime$,1,1) = hex(00)

            put #1, using L01000, userid$, datetime$, po$,               ~
                     vendor$, cardescr$, bol$, rcvddate$, ohpost$,       ~
                     " ", rcvtime$, rcvcomment$, " "

            if f1%(1) = 0% then write #1 else rewrite #1


            line% = 0%
            qtyrecd = qty_rec
            convert line$ to line%, data goto L00100

L00100:     

            convert line% to line$, pic(##0)
            init(" ") readkey$
            str(readkey$,1%,16%) = po$
            str(readkey$,17%,9%) = vendor$
            str(readkey$,26%,3%) = line$


            call "READ101" (#2, readkey$, f1%(2))
                  if f1%(2) <> 1% then goto L00200
                      get #2, using L00300, qtyrecd

L00300:                    FMT POS(37), PD(15,4)

                           qtyrecd = qtyrecd + qty_rec
 
L00200:

            put #2 , using L02000, po$, vendor$,  line$, datetime$,   ~
                       qtyrecd, 0,0,0,0,0,0, serial_number$


            if f1%(2) = 0% then write #2 else rewrite #2

            exit_sub

                 end

L01000:             FMT CH(03),                        /* User ID     */  ~
                        CH(08),                        /* Date & Time */  ~
                        CH(16),                        /* PO Number   */  ~
                        CH(09),                        /* Vendor Num  */  ~
                        CH(30),                        /* Vend Descr  */  ~
                        CH(30),                        /* Bill-Lading */  ~
                        CH(06),                        /* Date Recd   */  ~
                        CH(01),                        /* Dflt Dis    */  ~
                        CH(04),                        /* Text ID     */  ~
                        CH(08),                        /* TIme Entered*/  ~
                        CH(30),                        /* Comments    */  ~
                        CH(05)                         /* Filler      */

L02000:              FMT CH(16),                       /* PO Number   */  ~
                         CH(09),                       /* Vendor Code */  ~
                         CH(03),                       /* Line Number */  ~
                         CH(08),                       /* Time        */  ~
                         PD(15,4),                     /* Tot Rec     */  ~
                         PD(15,4),                     /* Rec Hold    */  ~
                         PD(15,4),                     /* Tot PEND QC */  ~
                         PD(15,4),                     /* Tot QC      */  ~
                         PD(15,4),                     /* Tot QC      */  ~
                         PD(15,4),                     /* Tot Reject  */  ~
                         PD(15,4),                     /* Moved ONHAND*/  ~
                         CH(08),                       /* Serial Num  */  ~
                         CH(700)                       /* Filler      */


