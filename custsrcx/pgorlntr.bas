        REM *************************************************************~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *  PPPP    GGG    OOO   RRRR   L      N   N  TTTTT  RRRR    *~
            *  P   P  G      O   O  R   R  L      NN  N    T    R   R   *~
            *  PPPP   G GG   O   O  RRRR   L      N N N    T    RRRR    *~
            *  P      G   G  O   O  R  R   L      N  NN    T    R  R    *~
            *  P       GGG    OOO   R   R  LLLLL  N   N    T    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PGORLNTR - Move Sales Order from the buffers to the master*~
            *            files.  Update Demands and HNYQUAN records for *~
            *            the backorder quantities.                      *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *10/11/2018! Original                                 ! CMN *~
            *04/29/2019! Set SKU to 10                            ! RDB *~ 
            *05/21/2019! (CR2022) Order & Line cancel per Tim do  ! CMN *~
            *          !    not sent customer file                !     *~  
            *06/04/2019! (CR2060) Create new record of BillTo Info! RDB *~            
            *06/24/2019! (CR2094) Set Mull group counts           ! RDB *~
            *08/08/2019! CR2173 Set Invoiced status               ! RDB *~
            *08/05/2020! CR2633 Fix Cube calculation for buyout   ! RDB *~
            *11/23/2020! CR2716 Alignment issue with file         ! RDB *~
            *01/25/2021! CR2755 New ship_id position              ! RDB *~
            *11/23/2021! CR2756 eCat change                       ! RDB *~
            *************************************************************

        dim  f2%(32%),                   /* = 0 if the file is open    */~
             f1%(32%),                   /* = 1 if READ was successful */~
             fs%(32%),                   /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
             rslt$(32%)20                /* result                     */


        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            hdr$45, msg$(3%)79           /* Askuser - Var's            */

        dim delim$1,                     /* Delimiter for transmit file*/~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim date$6,                      /* SysDate                    */~
            tme$8,                       /* SysTime                    */~
            fmtDate$10,                  /* Formated Date              */~
            fmtTime$10,                  /* Formated Time              */~
            company$20,                  /* Company                    */~
            ordertype$10                 /* Order Type                 */

        dim pgorlntrKey$44,              /* PGORLNTR Readkey           */~
            pgorlntrRec$256,             /* PGORLNTR Readrec           */~
            readtme$6                    /* Read Time adjusted current */

        dim readkey$100,                 /* Generic Readkey            */~
            desc$30,                     /* Description                */~
            filetype$20,                 /* Trigger File Type          */~
            salesorder$8,                /* Sales Order                */~
            soLne$3,                     /* Sales Order Line           */~
            scLne$2,                     /* APCPLNSC Line Zerofill     */~
            scStatus$2,                  /* APCPLNSC Status            */~
            linetypeid$4,                /* WW Line Type ID            */~
            l1mutype$50,                 /* L1 Mu Type                 */~
            l3mulltype$25,               /* L3 Mull Type               */~
            l1desc$200,                  /* L1 Description             */~
            l2desc$(2%)256,              /* L2 Description             */~
            part$25,                     /* part number                */~
            partDesc$32,                 /* part number                */~
            item$25,                     /* Item                       */~
            itemDesc1$50,                /* Item Description 1         */~
            itemDesc2$50,                /* Item Description 2         */~
            purchaseOrder$16,            /* PurchaseOrder              */~
            plygem_po$20,                /* PlyGem PO                  */~
            orderid$10,                  /* Order ID                   */~
            qtyOrdered$8,                /* Quantity Ordered           */~
            uom$4,                       /* Quantity Unit of Measure   */~
            orderDate$10,                /* Order Date                 */~
            loadDate$10,                 /* Load Date                  */~
            deliveryDate$10,             /* Delivery Date              */~
            load$5,                      /* Truck / Load               */~
            stop$5,                      /* Truck / Load Stop          */~
            billto$9,                    /* Bill To Customer           */~
            shipto$9,                    /* Ship To Customer           */~
            parent$9,                    /* Parent Customer            */~
            series$20,                   /* Family / Series            */~
            style$10,                    /* Style                      */~
            seriesStyle$100,             /* Series Style  !!!!         */~
            model$3,                     /* Model                      */~
            family$25,                   /* Family                     */~
            lastStatus$10,               /* last Status                */~
            nextStatus$10,               /* next Status                */~
            orderLineType$5,             /* Order Line Type            */~
            sku$10,                      /* Sku Number                 */~
            shortItem$10,                /* Short Item                 */~
            pickSlip$10,                 /* Pick Slip                  */~
            parentLineNumber$3,          /* Parent Line Item           */~
            setParentLineNbr$3,          /* First line number of group */~
            itemType$10,                 /* Item Type                  */~
            delInst1$50,                 /* Delivery Instruction 1     */~
            delInst2$50,                 /* Delivery Instruction 2     */~
            cancel$10,                   /* Cancel true / false        */~
            custname$30,                 /* Customer Name              */~
            address1$30,                 /* Address 1                  */~
            address2$30,                 /* Address 2                  */~
            address3$30,                 /* Address 3                  */~
            address4$30,                 /* Address 4                  */~
            address5$30,                 /* Address 5                  */~
            address6$30,                 /* Address 6                  */~
            city$20,                     /* City                       */~
            state$2,                     /* state                      */~
            zip$10,                      /* Zip                        */~
            country$10,                  /* Country                    */~
            phone$10,                    /* Phone                      */~
            fax$10,                      /* Fax                        */~
            email$20,                    /* Email                      */~
            contact$20,                  /* Contact                    */~
            geocode$10,                  /* Geocode                    */~
            custtype$10,                 /* Customer Type              */~
            shipcomplete$10,             /* Shipped Complete           */~
            loadbypo$10,                 /* Load by PO                 */~
            custlabel$10,                /* Customer Label             */~
            legCust$9,                   /* Caelus Customer            */~
            leg2Cust$9,                  /* Caelus shipto customer     */~
            status$2,                    /* Status                     */~
            pgm$10,                      /* Program                    */~
            upddte$8,                    /* Update Date                */~
            updtime$6,                   /* Update Time                */~
            wcnt$6,                      /* Write Cnt                  */~
            eof$5,                       /* EOF statement              */~
            filler1$256,                 /* Filler Area                */~
            wood$3,                      /* Wood Surround Code         */~
            wood_scrpt$2,                /* Wood Surround Flags        */~
            wdesc$30,                    /* APC Wood Description       */~
            uCube$8,                     /* Cube                       */~
            uWidth$8,                    /* Unit Width                 */~
            uHeight$8,                   /* Unit Height                */~
            uDepth$8,                    /* Unit Depth                 */~
            howship$20,                  /* BCKMASTR How Ship          */~
            shipid$6,                    /* Order ShipID               */~
            userwhoenter$3,              /* User who entered order     */~
            dispatchDte$10,              /* Dispatch Date              */~
            dispatchTme$4,               /* Dispatch Time              */~
            orHows$2,                    /* APCPLNOR How Ship          */~
            orRegion$2,                  /* APCPLNOR Region            */~
            orRoute$5,                   /* APCPLNOR Route             */~
            uUnits$8,                    /* Unit Units APCPLNSC        */~
            uValue$8,                    /* Unit Value APCPLNSC        */~
            uQty$8,                      /* Unit Quantity APCPLNSC     */~
            cutoff$2,                    /* Customer Cutoff            */~
            deliver$2,                   /* Customer Delivery          */~
            rtepln$10,                   /* Route Plan Customer        */~
            bname$30,                    /* Bill To Customer Name      */~
            baddress1$30,                /* Address 1                  */~
            baddress2$30,                /* Address 2                  */~
            baddress3$30,                /* Address 3                  */~
            baddress4$30,                /* Address 4                  */~
            bcity$20,                    /* City                       */~
            bstate$2,                    /* state                      */~
            bzip$10,                     /* Zip                        */~
            bphone$10,                   /* Phone                      */~
            bfax$10,                     /* Fax                        */~
            bfiller$1,                   /* filler space               */~
            bcontact$20,                 /* Contact                    */~
/* CR2094 */                                                             ~
            hld_solne$03,                /* Hold Caelus Line for Mull  */~
            mull_line$10,                /* Mull number                */~
            totmull$10,                  /* Total of units in mull     */~
            bckso$16,                    /* Bcklines SO for mulling    */~
            bckln$03                     /* Bcklines Line for mulling  */
            
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
            * #1  ! PGORLNTR ! PlyGem ATLaS Trigger Remote Order Line Fi*~
            * #2  ! PGORLNTR ! Remote Order Line Transmit File          *~
            * #3  ! PGCUSTTR ! Remote Customer Transmit File            *~
            * #5  ! BCKMASTR ! Backlog master file                      *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #10 ! CUSTOMER ! Customer Master File                     *~
            * #11 ! APCPLNOR ! APC Planning S.O. Header History         *~
            * #12 ! APCPLNSC ! APC Planning Master Scheduling File      *~
            * #20 ! GENCODES ! Master Code Table Files                  *~
            * #21 ! ORADESC2 ! WW Description File                      *~
            * #22 ! BCKSUBPT ! Sub Part File                            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "PGORLNTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup

            select #2, "PGORLNTR",    /* transmit file */                ~
                        varc,     indexed, recsize = 768,                ~
                        keypos = 1,    keylen = 70

            select #3, "PGCUSTTR",    /* transmit file */                ~
                        varc,     indexed, recsize = 786,                ~
                        keypos = 1,    keylen = 66

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #10,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #11,  "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #12,  "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #14, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

             select #15, "AWDAPPLD"                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15


            select #20, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #21, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11


            select #22, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 25%, rslt$(1%))
            filename$ = "BCKMASTR" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLD" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ORADESC2" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error

            gosub initializeVariables
            gosub openTransmitFile
readNext:
            gosub readPgorlntr
              if eod% = 1% then goto exitProgram
              if pgorlntr% = 0% then goto readNext
            gosub readPlanStatus                
/*if no plan data then dont think should send */
              if planData% = 0% then goto readNext
              if lastStatus$ <> " " then goto writeOrder
                 gosub updatePgorlntr
                 goto readNext
writeOrder:              
            gosub readLoad                         
            gosub readAppLoad
            gosub readBckmaster
            gosub readBcklines
            gosub readCustomer            
REM 2633            gosub calcCube
            init(" ") mull_line$, totmull$
            gosub readMullBcklines         /* CR2094  */
            gosub readBcksubpt
            gosub updatePgorlntr
            gosub readOradesc2
            gosub calcCube                /* CR2633 */

            gosub writePgorlntr
            goto readNext

            goto exitProgram

            openTransmitFile
              file$ = "PGORLNTR"
              ff%   = 2%
              gosub openFile

              file$ = "PGCUSTTR"
              ff%   = 3%
              gosub openFile
            return

            openFile
              open nodisplay #ff%, output, space = 100%,                  ~
                 dpack   = 100%, ipack = 100%, file = file$,              ~
                 library = library$, volume = volume$, blocks = 5%
            return

            initializeVariables
              init(" ") delim$, file$, library$, volume$, date$, tme$,  ~
                   fmtDate$, fmtTime$, company$, ordertype$, filename$, ~
                   hdr$, msg$(), pgorlntrKey$, pgorlntrRec$, readtme$,  ~
                   wcnt$, status$, pgm$, upddte$, updtime$, filler1$

              cnt%, ccnt%, lcnt% = 0%         /* Customer & Line cnt */
              date$    = date
              fmtDate$ = date
              tme$     = time
              readtme$ = time
              call "DATFMTC" (fmtDate$)
              delim$   = "|"
              library$ = "ORAFILES"
              volume$  = "NE2"
              company$ = "DALLAS"
              ordertype$ = "SO"

              init(" ") salesorder$, soLne$, item$, itemDesc1$, itemDesc2$,   ~
                   purchaseOrder$, qtyOrdered$, uom$, orderDate$, loadDate$,  ~
                   deliveryDate$, load$, stop$, billto$, shipto$, parent$,    ~
                   series$, model$, lastStatus$, nextStatus$, orderLineType$, ~
                   sku$, shortItem$, pickSlip$, parentLineNumber$, itemType$, ~
                   delInst1$, delInst2$, cancel$, scLne$, scStatus$, desc$,   ~
                   filetype$, custname$, address1$, address2$, address3$,     ~
                   address4$, address5$, address6$, city$, state$, zip$,      ~
                   country$, phone$, fax$, email$, contact$, geocode$,        ~
                   custtype$, shipcomplete$, loadbypo$, custlabel$, legCust$, ~
                   style$, seriesStyle$, part$, partDesc$, family$,           ~
                   wood_scrpt$, wood$, wdesc$, uCube$, uWidth$, uHeight$,     ~
                   uDepth$, howship$, shipid$, userwhoenter$, dispatchDte$,   ~
                   dispatchTme$, orHows$, orRegion$, orRoute$, uUnits$,       ~
                   uValue$, uQty$, cutoff$, deliver$, rtepln$, plygem_po$,    ~
                   orderid$, bfax$, bname$, baddress1$, baddress2$,           ~
                   baddress3$, baddress4$, bcity$, bstate$, bfiller$, bzip$,  ~
                   bcontact$, bphone$, leg2Cust$, mull_line$, totmull$,       ~
                   setParentLineNbr$

              uom$ = "EA"

              pgorlntrKey$ = all(hex(00))
              cancel$ = "false"
              shipcomplete$ = "false"
              loadbypo$ = "true"
              rtepln$ = "true"
            return

            readPgorlntr
              pgorlntr% = 0%
              eod% = 0%
              if cnt% > 0% then goto pgorlntrNxt             /* #1-PGORLNTR */
              read #1, hold, key > pgorlntrkey$, using PGORLNTR_FMT,         ~
                                           pgorlntrrec$, eod goto pgorlntrdone
                 goto pgorlntrFrst
            pgorlntrNxt
              read #1, hold, using PGORLNTR_FMT, pgorlntrRec$,               ~
                                                         eod goto pgorlntrDone
PGORLNTR_FMT:            FMT CH(256)

pgorlntrFrst:
                if str(pgorlntrRec$,21%,01%) <> "0" then goto pgorlntrDone
                if str(pgorlntrRec$,01%,09%) <> "ORDERLINE" then             ~
                                                          goto pgorlntrDone
                if str(pgorlntrRec$,22%,06%) > str(date$,01%,06%) then       ~
                                                             goto pgorlntrDone

                str(pgorlntrKey$,01%,44%) = str(pgorlntrRec$,21%,44%)
                status$ = str(pgorlntrRec$,77%,02%)
                pgm$ = str(pgorlntrRec$,79%,10%)
                pgorlntr% = 1%
                bckmastr% = 0%
                if str(pgorlntrRec$,54%,08%) <> salesorder$ then             ~
                                                             bckmastr% = 1%
                salesorder$ = str(pgorlntrRec$,54%,08%)
                soLne$      = str(pgorlntrRec$,62%,03%)
                solne% = 0%
                convert soLne$ to solne%, data goto badLine

                convert solne% to soLne$, pic(##0)
                convert solne% to scLne$, pic(00)
                shipto$ = str(pgorlntrRec$,89%,09%)
                cnt% = cnt% + 1%
            return
            pgorlntrDone
              eod% = 1%
            return
badLine:
              pgorlntr% = 0%
            return

            updatePgorlntr
              str(upddte$,1%,6%) = date
              updtime$ = time

              str(pgorlntrRec$,21%,01%) = "1"
              str(pgorlntrRec$,65%,06%) = upddte$
              str(pgorlntrRec$,71%,06%) = updtime$

              delete #1

              put #1, using PGORLNTR_FMT, pgorlntrRec$
              write #1, eod goto updateDone

            updateDone
            return

            readPlanStatus
              planData% = 0%
              init(" ") readkey$
              str(readkey$,1%,8%) = salesorder$              /* #11-APCPLNOR */
              read #11,key 4% = readkey$, using OR_FMT, orRegion$, orRoute$,  ~
                                orHows$, eod goto planHeaderDone
OR_FMT:             FMT POS(09), CH(02), CH(05), POS(92), CH(02)   


              planData% = 1%              /* APCPLNOR Found */
              
              init(" ") readkey$
              uQty% = 0%
              uUnits, uValue = 0.00
              str(readkey$,01%,08%) = salesorder$
              str(readkey$,09%,02%) = scLne$
                                                             /* #12-APCPLNSC */
              read #12, key = readkey$, using SC_FMT, load$, stop$, uQty%,    ~
                                                    uUnits, uValue, scStatus$,~
                                                       eod goto planStatusDone

SC_FMT:              FMT POS(07), CH(05), POS(17), CH(02), POS(68), BI(02),   ~
                         POS(76), PD(14,4), POS(92), PD(14,4), POS(110), CH(02)

               convert uQty% to uQty$, pic(#######0)
               convert uUnits to uUnits$, pic(####0.00)
               convert uValue to uValue$, pic(####0.00)
                                   /* As long as SC qty send status as PLAN */
REM IF STATUS$ < "12" THEN LASTSTATUS$ = "PLAN"    
               lastStatus$ = "PLAN"
               if status$ = "20" then lastStatus$ = "INVOICED"   /* CR2173 */
               nextStatus$ = lastStatus$

               planData% = 2%              /* APCPLNSC Found */
            return
REM ---- @@TURN OFF STATUS LOOKUP FROM PLAN STATUS FOR NOW@@ ----

                init(" ") readkey$, desc$
                str(readkey$,01%,09%) = "PLAN STATUS"
                str(readkey$,10%,02%) = scStatus$
                gosub lookupGencodes

                lastStatus$ = desc$
                nextStatus$ = desc$

                lastStatus$ = "PLAN"
                nextStatus$ = "PLAN"
                planData% = 1%
            return
            planHeaderDone
            planStatusDone
               if status$ < "12" then return
REM IF STATUS$ < "12" THEN GOTO UPDATETRIGGER
               planData% = 98%
               lastStatus$ = "CANCELED"
               nextStatus$ = lastStatus$
            return
            updateTrigger         /* Means order does not exist in APCPLNOR */
              gosub updatePgorlntr
            return

            readLoad                                         /* #14-APCPLNLD */
              read #14,key = load$, using LD_FMT, loadDate$, eod goto loadError
LD_FMT:                FMT POS(62), CH(06)


              call "DATFMTC" (loadDate$)
            loadError
            return

            readAppLoad                                      /* #15-AWDAPPLD */
               read #15,key = load$, using APPLD_FMT, dispatchDte$,    ~
                                           dispatchTme$, eod goto appLoadError
APPLD_FMT:              FMT POS(02), CH(06), CH(04)

              call "DATFMTC" (dispatchDte$)
              
/* CR2716 make certain time is 4 characters with leading zeros */
              convert dispatchTme$ to dispatchTme%, data goto appTimeError
              convert dispatchTme% to dispatchTme$, pic(0000)

            appLoadError
            return

/* CR2716 */
            appTimeError  
              dispatchTme$ = "9999"
            return

            readCustomer                                     /* #10-CUSTOMER */
              read #10, key = shipto$, using CUSTOMER_FMT, fax$, contact$,    ~
                              phone$, parent$, billto$, cutoff$, deliver$,    ~
                                                         eod goto customerDone

CUSTOMER_FMT:       FMT POS(228), CH(10), POS(433), CH(20), CH(10),           ~
                        POS(771), CH(09), CH(09), POS(860), CH(02),           ~
                        POS(880), CH(02)

                legCust$ = shipto$

                init(" ") readkey$, desc$
                cutoff% = 0%
                str(readkey$,01%,09%) = "PLAN CUTO"
                str(readkey$,10%,02%) = cutoff$
                gosub lookupGencodes
                if gen% = 0% then goto noCutoff

                  convert str(desc$,1%,1%) to cutoff%, data goto noCutoff

noCutoff:
                  convert cutoff% to cutoff$, pic(00)

                init(" ") readkey$, desc$
                p%, deliver% = 0%
                str(readkey$,01%,09%) = "PLAN DELV"
                str(readkey$,10%,02%) = deliver$
                gosub lookupGencodes
                if gen% = 0% then return

                  p% = pos(desc$ = "(")
                  if p% = 0% then goto noDeliver
                  convert str(desc$,(p%+1%),2%) to deliver%, data goto noDeliver

noDeliver:
                  convert deliver% to deliver$, pic(00)
            customerDone
            return
            
/* CR2060 */
            readBillToCustomer                              /* #10-CUSTOMER */
              read #10, key = billto$, using BCUSTOMER_FMT, bname$,           ~
                              baddress1$, baddress2$, baddress3$, baddress4$, ~
                              bcity$, bstate$, bfiller$, bzip$, bfax$,        ~
                              bcontact$, bphone$,      eod goto BcustomerDone

BCUSTOMER_FMT:      FMT POS(40), CH(30), CH(30), CH(30),                      ~
                          CH(30), CH(30), CH(18), CH(02), CH(01), CH(09),     ~
                        POS(228), CH(10),                                     ~
                        POS(433), CH(20), CH(10)
                        
                  billfound% = 1%
                  leg2Cust$ = billto$
                  
            BcustomerDone
            return
            
            readBckmaster
              if bckmastr% = 0% then return

              init(" ") readkey$
              str(readkey$,01%,09%) = shipto$
              str(readkey$,10%,08%) = salesorder$
                                                             /* #5-BCKMASTR */
              read #5, key = readkey$, using BCKMASTR_FMT, shipto$,           ~
                             purchaseOrder$, custname$, address1$, address2$, ~
                             address3$, address4$, address6$, howship$,       ~
                             delInst1$, delInst2$, orderid$, orderDate$,      ~
                             deliveryDate$, userwhoenter$, shipid$, plygem_po$,~
                                                        eod goto bckmasterDone

BCKMASTR_FMT:       FMT CH(09), POS(26),CH(16), 6*CH(30),                     ~
                        POS(422), CH(20),                                     ~
                        POS(462), CH(50), CH(50), POS(599), CH(10), POS(806), ~
                        CH(06), POS(818), CH(06), POS(836), CH(03), POS(984), ~
                        CH(06), POS(920), CH(20)

                orderid% = 0%            /* BCKMASTER - variableField1 = M */
                convert str(orderid$,02%,19%) to orderid%, data goto badOrderid
           
badOrderid:
                convert orderid% to orderid$, pic(##########)  
                if plygem_po$ > " " then purchaseOrder$ = plygem_po$
/* CR2755 */
REM                shipid$ = "0" & shipid$
                shipid$ = str(shipid$,3%,4%)  /* move last 4 bytes of ship_id */
                
                city$  = str(address6$,01%,18%)
                state$ = str(address6$,19%,02%)
                zip$   = str(address6$,22%,09%)
              
                call "DATFMTC" (orderDate$)
                call "DATFMTC" (deliveryDate$)
            bckmasterDone
            return

            readBcklines
              init(" ") readkey$              
              str(readkey$,01%,16%) = salesorder$
              str(readkey$,17%,03%) = soLne$
              qtyOrdered = 0.00
              qtyOrdered% = 0%
                                                             /* #6-BCKLINES */
              read #6, key = readkey$, using BCKLINES_FMT, part$, partDesc$ , ~
                             qtyOrdered, parentLineNumber$, sku$,             ~
                                                         eod goto bcklinesDone

BCKLINES_FMT:        FMT POS(32), CH(25), CH(32), POS(93), PD(14,4),          ~
                         POS(284), CH(03), POS(290), CH(10)
REM ? Nothing from BCKLINES
                model$ = str(part$,01%,03%)

                family$ = part$
                itemDesc2$ = partDesc$
                qtyOrdered% = int(qtyOrdered)
                convert qtyOrdered% to qtyOrdered$, pic(#######0)

                gosub check_sku           /* RDB 20190402 */
                if sosexits% = 1% then init(" ") sku$
                if sku$ = "0" then init (" ") sku$
                if userwhoenter$ = "ECT" then init (" ") sku$   /* CR2756 */

            bcklinesDone
            return
 
/* CR2094  read for mull counts, line numbers and parent setting  */ 
            readMullBcklines
              init(" ") readkey$, mull_line$, totmull$, hld_solne$
              str(readkey$,01%,16%) = salesorder$
              str(readkey$,17%,03%) = "  0"
              mull_line% = 0%
              totmull% = 0%
              first_mull% = 0%
              
            readNxtBcklines
                                                             /* #6-BCKLINES */
              read #6, key > readkey$, using Mull_BCKLINES_FMT, bckso$,      ~
                             bckln$, groupcode$, configtype$, configline$,    ~
                                                         eod goto mullbckDone

Mull_BCKLINES_FMT:   FMT POS(10), CH(16), CH(03), POS(279), CH(01), CH(02), ~
                         POS(284), CH(03)
              
              str(readkey$,01%,16%) = bckso$
              str(readkey$,17%,03%) = bckln$  
              
              if salesorder$ <> bckso$ then goto mullbckDone
              
              if parentLineNumber$ <> configline$  then readNxtBcklines

              totmull% = totmull% + 1%
              if bckln$ = soLne$ then mull_line% = totmull% 
              if first_mull% = 0% then hld_solne$ = bckln$
              first_mull% = 1%
              
              goto readNxtBcklines
              
            mullbckDone
              if totmull% = 0% then return
            
              convert mull_line% to mull_line$, pic(#########0)
              convert totmull%   to totmull$,   pic(#########0)
              setParentLineNbr$ = hld_solne$            /* CR2094 */
            return
            
            check_sku                          /* RDB 20190402 */
              sosexits% = 0%
              init(" ") readkey$
              str(readkey$,1%,9%)   = "SOS SKU"
              str(readkey$,10%,15%) = sku$                   /* #20-GENCODES */
              read #20,key = readkey$, using L07310, desc$, eod goto L07330
L07310:          FMT POS(25), CH(30)
               sosexits% = 1%

L07330:     return                             /* RDB 20190402 */

            calcCube
              cube, fact, uWidth, uHeight, uDepth = 0.0
              a1%, a2%, sc% = 0%
              fact = 3.25                     /* Set Default Jamb         */
REM IF LEN(PART$) < 19% THEN RETURN
              buyout% = 0%
              if len(part$) < 19% then gosub checkBuyout
              if buyout% = 1% then goto buyout_cont
              
              init(" ") readkey$, desc$
              str(readkey$,01%,09%) = "SYS CODES"
              str(readkey$,10%,03%) = str(part$,1%,3%)   
              gosub lookupGencodes
              if gen% = 0% then goto calc_fact

              convert str(desc$,21%,5%) to fact, data goto calc_fact

            calc_fact                                /* Convert Width      */
              convert str(part$,13%,4%) to a1%,data goto calc_width

            calc_width                               /* Convert Height     */
              convert str(part$,17%,3%) to a2%,data goto calc_height
L07340:
            calc_height                              /* Always Round Up    */
              a1% = a1% + 10%
              a2% = a2% + 10%

/* is this TSO, BSO, FGO if so take half height */
              convert str(part$,11%,1%) to sc%, data goto no_screen
            no_screen
              if sc% = 4% or sc% = 5% then a2% = round(a2% / 2%, 0)
              if sc% = 6% or sc% = 7% then a2% = round(a2% / 2%, 0)

/* double if 9 for bay/bow */
              if str(part$,1%,1%) = "9" then a2% = round(a2% * 2%, 0)

              unit_i%  = int(a1%/10) * int(a2%/10)
buyout_cont:                                       /* CR2633 */        
                                                   /* Is this Wood Surround */
              gosub lookup_mull
              if cubic_mull% = 1% then fact = fact + wd_sze

              fact = round(fact,2)          /* Calc Line Item Units         */
                                            /* Divide by 144 to turn inches */
                                            /* back to feet                 */
              cube = round( ( (fact * unit_i%) / 1728.0 ) * uQty%,2)
REM  OR_UNITS = ROUND( OR_UNITS + SC_UNITS, 2) /* TOT S.O. UNITS*/
              uWidth = int(a1% / 10%)
              uHeight = int(a2% / 10%)
              uDepth = fact

              convert cube to uCube$, pic(####0.00)
              convert uWidth to uWidth$, pic(####0.00)
              convert uHeight to uHeight$, pic(####0.00)
              convert uDepth to uDepth$, pic(####0.00)

            return
            checkBuyout
              if widthes <= 0.00 or heightes <= 0 then return
              buyout% = 1%
              a1% = int(widthes) + 1%       /* CR2633 change to make Atrium */  
              a2% = int(heightes) + 1%      /* CR2633 change to make Atrium */
              unit_i% = int(a1%) * int(a2%) /* CR2633 change to make Atrium */
REM 2633           goto calc_height
            return
            
            lookup_mull
               call "APCAMULL" (part$, mull%, wood$, wdesc$, wood_scrpt$, ~
                                #20, error%)
               if mull% = 0% then return
               gosub check_app_mull
            return

            check_app_mull
              cubic_mull% = 0%
              wd_sze = 4.00                    /* Set Default Jamb         */
              init(" ") readkey$, desc$
              str(readkey$,01%,09%) = "AWD WOOD "
              str(readkey$,10%,02%) = wood$
              gosub lookupGencodes
              if gen% = 0% then goto no_app_mull
                convert str(desc$,25%,6%) to wd_sze, data goto invalid_size
invalid_size:

              cubic_mull% = 1%
            no_app_mull
            return

            readBcksubpt
              init(" ") readkey$
              str(readkey$,01%,08%) = salesorder$
              str(readkey$,09%,03%) = soLne$
                                                             /* #22-BCKSUBPT */
              read #22, key = readkey$, using BCKSUBPT_FMT, series$, style$,  ~
                              itemType$, eod goto bcksubptDone

BCKSUBPT_FMT:       FMT POS(169), CH(16), CH(10), CH(10)

                item$ = str(series$,01%,len(series$)) &                 ~
                               str(style$,01%,len(style$))
                itemDesc1$ = str(series$,01%,len(series$)) &  " " &     ~
                               str(style$,01%,len(style$))
            bcksubptDone
            return

            readOradesc2
              linetypeid% = 0%
REM              widthes, heightes = 0.00
              init(" ") readkey$
              str(readkey$,01%,08%) = salesorder$
              str(readkey$,09%,03%) = soLne$
                                                             /* #21-ORADESC2 */
              read #21, key = readkey$, using ORADESC2_FMT, widthes, heightes,~
                              l1desc$, l2desc$(), linetypeid%, l1mutype$,     ~
                              l3mulltype$, eod goto oradesc2Done

ORADESC2_FMT:       FMT POS(55), PD(14,4), PD(14,4), POS(71), CH(250), CH(500),~
                        CH(02), CH(50), CH(25)

                put linetypeid$, using BIN_FMT, linetypeid%
BIN_FMT:            FMT BI(02)

            oradesc2Done
            return

            writePgorlntr
               convert lcnt% to wcnt$, pic(000000)

               put #2, using RMORDLNE_FMT,        /* RECLEN 768             */~
                       fmtDate$,                  /* Current Date           */~
                       tme$,                      /* Current Time           */~
                       wcnt$,                     /* Counter                */~
                       delim$,                    /* Delimiter              */~
                       company$,                  /* Company                */~
                       delim$,                    /* Delimiter              */~
                       ordertype$,                /* Order Type             */~
                       delim$,                    /* Delimiter              */~
                       salesorder$,               /* Sales Order Number     */~
                       delim$,                    /* Delimiter              */~
                       soLne$,                    /* SO Line                */~
                       delim$,                    /* Delimiter              */~
                       item$,                     /* Item                   */~
                       delim$,                    /* Delimiter              */~
                       itemDesc1$,                /* Item Description 1     */~
                       delim$,                    /* Delimiter              */~
                       itemDesc2$,                /* Item Description 2     */~
                       delim$,                    /* Delimiter              */~
                       purchaseOrder$,            /* Purchase Order         */~
                       delim$,                    /* Delimiter              */~
                       uQty$,                     /* Quantity Order APCPLNSC*/~
                       delim$,                    /* Delimiter              */~
                       uom$,                      /* UOM                    */~
                       delim$,                    /* Delimiter              */~
                       orderDate$,                /* Order Date             */~
                       delim$,                    /* Delimiter              */~
                       loadDate$,                 /* Load Date              */~
                       delim$,                    /* Delimiter              */~
                       deliveryDate$,             /* Delivery Date          */~
                       delim$,                    /* Delimiter              */~
                       load$,                     /* Load                   */~
                       delim$,                    /* Delimiter              */~
                       stop$,                     /* Stop                   */~
                       delim$,                    /* Delimiter              */~
                       billto$,                   /* bill to                */~
                       delim$,                    /* Delimiter              */~
                       shipto$,                   /* ship to                */~
                       delim$,                    /* Delimiter              */~
                       parent$,                   /* parent account         */~
                       delim$,                    /* Delimiter              */~
                       family$,                   /* PartNumber             */~
                       delim$,                    /* Delimiter              */~
                       model$,                    /* Model                  */~
                       delim$,                    /* Delimiter              */~
                       lastStatus$,               /* last status            */~
                       delim$,                    /* Delimiter              */~
                       nextStatus$,               /* next status            */~
                       delim$,                    /* Delimiter              */~
                       orderLineType$,            /* order line type        */~
                       delim$,                    /* Delimiter              */~
                       sku$,                      /* sku number             */~
                       delim$,                    /* Delimiter              */~
                       shortItem$,                /* short item             */~
                       delim$,                    /* Delimiter              */~
                       pickSlip$,                 /* pick slip              */~
                       delim$,                    /* Delimiter              */~
                       parentLineNumber$,         /* parent line number     */~
                       delim$,                    /* Delimiter              */~
                       itemType$,                 /* item type              */~
                       delim$,                    /* Delimiter              */~
                       delInst1$,                 /* delivery instructions 1*/~
                       delim$,                    /* Delimiter              */~
                       delInst2$,                 /* delivery instructions 2*/~
                       delim$,                    /* Delimiter              */~
                       cancel$,                   /* cancel true false      */~
                       delim$,                    /* Delimiter              */~
                       dispatchDte$,              /* Dispatch Date          */~
                       delim$,                    /* Delimiter              */~
                       dispatchTme$,              /* Dispatch Time          */~
                       delim$,                    /* Delimiter              */~
                       uCube$,                    /* Cube Units             */~
                       delim$,                    /* Delimiter              */~
                       uWidth$,                   /* Calc Cube Width        */~
                       delim$,                    /* Delimiter              */~
                       uHeight$,                  /* Calc Cube Height       */~
                       delim$,                    /* Delimiter              */~
                       uDepth$,                   /* Calc Cube Depth        */~
                       delim$,                    /* Delimiter              */~
                       uValue$,                   /* Line Value APCPLNSC    */~
                       delim$,                    /* Delimiter              */~
                       uUnits$,                   /* Line Units APCPLNSC    */~
                       delim$,                    /* Delimiter              */~
                       orderid$,                  /* WW OrderId             */~
                       delim$,                    /* Delimiter              */~
                       setParentLineNbr$,         /* set parent line number */~
                       delim$,                    /* Delimiter              */~
                       filler1$                   /* Filler                 */

               write #2

               lcnt% = lcnt% + 1%
/* Only Write Customer Once Per Order */
               if bckmastr% = 0% then return
               if planData% = 98% then return                  /* (CR2022)  */
               convert ccnt% to wcnt$, pic(000000)
               
               put #3, using RMCUST_FMT,          /* RECLEN 786             */~
                       fmtDate$,                  /* Current Date           */~
                       tme$,                      /* Current Time           */~
                       wcnt$,                     /* Counter                */~
                       delim$,                    /* Delimiter              */~
                       company$,                  /* Company                */~
                       delim$,                    /* Delimiter              */~
                       ordertype$,                /* Order Type             */~
                       delim$,                    /* Delimiter              */~
                       salesorder$,               /* Sales Order Number     */~
                       delim$,                    /* Delimiter              */~
                       shipto$,                   /* Customer Ship To       */~
                       delim$,                    /* Delimiter              */~
                       custname$,                 /* Customer Name          */~
                       delim$,                    /* Delimiter              */~
                       address1$,                 /* Address1               */~
                       delim$,                    /* Delimiter              */~
                       address2$,                 /* Address2               */~
                       delim$,                    /* Delimiter              */~
                       address3$,                 /* Address3               */~
                       delim$,                    /* Delimiter              */~
                       address4$,                 /* Address4               */~
                       delim$,                    /* Delimiter              */~
                       city$,                     /* City                   */~
                       delim$,                    /* Delimiter              */~
                       state$,                    /* State                  */~
                       delim$,                    /* Delimiter              */~
                       zip$,                      /* Zip                    */~
                       delim$,                    /* Delimiter              */~
                       country$,                  /* Country                */~
                       delim$,                    /* Delimiter              */~
                       phone$,                    /* Phone                  */~
                       delim$,                    /* Delimiter              */~
                       fax$,                      /* Fax                    */~
                       delim$,                    /* Delimiter              */~
                       email$,                    /* Email                  */~
                       delim$,                    /* Delimiter              */~
                       contact$,                  /* Contact                */~
                       delim$,                    /* Delimiter              */~
                       geocode$,                  /* Geocode                */~
                       delim$,                    /* Delimiter              */~
                       custtype$,                 /* Customer Type          */~
                       delim$,                    /* Delimiter              */~
                       shipcomplete$,             /* Ship Complete          */~
                       delim$,                    /* Delimiter              */~
                       loadbypo$,                 /* Load By PO             */~
                       delim$,                    /* Delimiter              */~
                       custlabel$,                /* Customer Label         */~
                       delim$,                    /* Delimiter              */~
                       legCust$,                  /* Caelus Customer        */~
                       delim$,                    /* Delimiter              */~
                       shipid$,                   /* ShipID BCKMASTR        */~
                       delim$,                    /* Delimiter              */~
                       orRoute$,                  /* Route APCPLNOR         */~
                       delim$,                    /* Delimiter              */~
                       orRegion$,                 /* Region APCPLNOR        */~
                       delim$,                    /* Delimiter              */~
                       orHows$,                   /* HowShip APCPLNOR       */~
                       delim$,                    /* Delimiter              */~
                       cutoff$,                   /* Cust Cutoff CUSTOMER   */~
                       delim$,                    /* Delimiter              */~
                       deliver$,                  /* Cust Delivery CUSTOMER */~
                       delim$,                    /* Delimiter              */~
                       rtepln$,                   /* Route Plan Customer    */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Filler                 */~
                       filler1$                   /* Filler                 */


               write #3
               ccnt% = ccnt% + 1%
/* CR2060 */
               billfound% = 0%
               gosub readBillToCustomer
               
/* writing shipto record into the customer file to Atlas */
               convert ccnt% to wcnt$, pic(000000)         
               put #3, using RMCUST_FMT,          /* RECLEN 786             */~
                       fmtDate$,                  /* Current Date           */~
                       tme$,                      /* Current Time           */~
                       wcnt$,                     /* Counter                */~
                       delim$,                    /* Delimiter              */~
                       company$,                  /* Company                */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Order Type             */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Sales Order Number     */~
                       delim$,                    /* Delimiter              */~
                       billto$,                   /* Customer Bill To       */~
                       delim$,                    /* Delimiter              */~
                       bname$,                    /* Customer Name          */~
                       delim$,                    /* Delimiter              */~
                       baddress1$,                /* Address1               */~
                       delim$,                    /* Delimiter              */~
                       baddress2$,                /* Address2               */~
                       delim$,                    /* Delimiter              */~
                       baddress3$,                /* Address3               */~
                       delim$,                    /* Delimiter              */~
                       baddress4$,                /* Address4               */~
                       delim$,                    /* Delimiter              */~
                       bcity$,                    /* City                   */~
                       delim$,                    /* Delimiter              */~
                       bstate$,                   /* State                  */~
                       delim$,                    /* Delimiter              */~
                       bzip$,                     /* Zip                    */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Country                */~
                       delim$,                    /* Delimiter              */~
                       bphone$,                   /* Phone                  */~
                       delim$,                    /* Delimiter              */~
                       bfax$,                     /* Fax                    */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Email                  */~
                       delim$,                    /* Delimiter              */~
                       bcontact$,                 /* Contact                */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Geocode                */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Customer Type          */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Ship Complete          */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Load By PO             */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Customer Label         */~
                       delim$,                    /* Delimiter              */~
                       leg2Cust$,                 /* Caelus Customer        */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* ShipID BCKMASTR        */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Route APCPLNOR         */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Region APCPLNOR        */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* HowShip APCPLNOR       */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Cust Cutoff CUSTOMER   */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Cust Delivery CUSTOMER */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Route Plan Customer    */~
                       delim$,                    /* Delimiter              */~
                       filler1$,                  /* Filler                 */~
                       filler1$                   /* Filler                 */

               write #3      
               ccnt% = ccnt% + 1%
            return

RMORDLNE_FMT:      FMT CH(10),                    /* Current Date           */~
                       CH(08),                    /* Current Time           */~
                       CH(06),                    /* Counter                */~
                       CH(01),                    /* Delimiter              */~
                       CH(20),                    /* Company                */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Order Type             */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Sales Order Number     */~
                       CH(01),                    /* Delimiter              */~
                       CH(03),                    /* SO Line                */~
                       CH(01),                    /* Delimiter              */~
                       CH(25),                    /* Item                   */~
                       CH(01),                    /* Delimiter              */~
                       CH(50),                    /* Item Description 1     */~
                       CH(01),                    /* Delimiter              */~
                       CH(50),                    /* Item Description 2     */~
                       CH(01),                    /* Delimiter              */~
                       CH(16),                    /* Purchase Order         */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Quantity Ordered       */~
                       CH(01),                    /* Delimiter              */~
                       CH(04),                    /* UOM                    */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Order Date             */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Load Date              */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Delivery Date          */~
                       CH(01),                    /* Delimiter              */~
                       CH(05),                    /* Load                   */~
                       CH(01),                    /* Delimiter              */~
                       CH(05),                    /* Stop                   */~
                       CH(01),                    /* Delimiter              */~
                       CH(09),                    /* bill to                */~
                       CH(01),                    /* Delimiter              */~
                       CH(09),                    /* ship to                */~
                       CH(01),                    /* Delimiter              */~
                       CH(09),                    /* parent account         */~
                       CH(01),                    /* Delimiter              */~
                       CH(65),                    /* part number            */~
                       CH(01),                    /* Delimiter              */~
                       CH(03),                    /* Model                  */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* last status            */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* next status            */~
                       CH(01),                    /* Delimiter              */~
                       CH(05),                    /* order line type        */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* sku number             */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* short item             */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* pick slip              */~
                       CH(01),                    /* Delimiter              */~
                       CH(03),                    /* parent line number     */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* item type              */~
                       CH(01),                    /* Delimiter              */~
                       CH(50),                    /* delivery instructions 1*/~
                       CH(01),                    /* Delimiter              */~
                       CH(50),                    /* delivery instructions 2*/~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* cancel true false      */~
                       CH(01),                    /* Delimiter    POS(563)  */~
                       CH(10),                    /* Dispatch Date          */~
                       CH(01),                    /* Delimiter              */~
                       CH(04),                    /* Dispatch Time          */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Cube Units             */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Calc Cube Width        */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Calc Cube Height       */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Calc Cube Depth        */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Line Value APCPLNSC    */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Line Units APCPLNSC    */~
                       CH(01),                    /* Delimiter    POS(633)  */~
                       CH(10),                    /* OrderID                */~
                       CH(01),                    /* Delimiter              */~
                       CH(03),                    /* Set Parent Line Nbr    */~
                       CH(01),                    /* Delimiter              */~
                       CH(120)                    /* Filler    RECLEN768    */

RMCUST_FMT:        FMT CH(10),                    /* Current Date           */~
                       CH(08),                    /* Current Time           */~
                       CH(06),                    /* Counter                */~
                       CH(01),                    /* Delimiter              */~
                       CH(20),                    /* Company                */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Order Type             */~
                       CH(01),                    /* Delimiter              */~
                       CH(08),                    /* Sales Order Number     */~
                       CH(01),                    /* Delimiter              */~
                       CH(09),                    /* Customer Ship To       */~
                       CH(01),                    /* Delimiter              */~
                       CH(30),                    /* Customer Name          */~
                       CH(01),                    /* Delimiter              */~
                       CH(30),                    /* Address1               */~
                       CH(01),                    /* Delimiter              */~
                       CH(30),                    /* Address2               */~
                       CH(01),                    /* Delimiter              */~
                       CH(30),                    /* Address3               */~
                       CH(01),                    /* Delimiter              */~
                       CH(30),                    /* Address4               */~
                       CH(01),                    /* Delimiter              */~
                       CH(30),                    /* City                   */~
                       CH(01),                    /* Delimiter              */~
                       CH(02),                    /* State                  */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Zip                    */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Country                */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Phone                  */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Fax                    */~
                       CH(01),                    /* Delimiter              */~
                       CH(20),                    /* Email                  */~
                       CH(01),                    /* Delimiter              */~
                       CH(20),                    /* Contact                */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Geocode                */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Customer Type          */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Ship Complete          */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Load By PO             */~
                       CH(01),                    /* Delimiter              */~
                       CH(10),                    /* Customer Label         */~
                       CH(01),                    /* Delimiter              */~
                       CH(09),                    /* Caelus Customer        */~
                       CH(01),                    /* Delimiter    POS(416)  */~
                       CH(04),                    /* ShipID BCKMASTR        */~
                       CH(01),                    /* Delimiter              */~
                       CH(05),                    /* Route APCPLNOR         */~
                       CH(01),                    /* Delimiter              */~
                       CH(02),                    /* Region APCPLNOR        */~
                       CH(01),                    /* Delimiter              */~
                       CH(02),                    /* HowShip APCPLNOR       */~
                       CH(01),                    /* Delimiter              */~
                       CH(02),                    /* Cust Cutoff CUSTOMER   */~
                       CH(01),                    /* Delimiter              */~
                       CH(02),                    /* Cust Delivery CUSTOMER */~
                       CH(01),                    /* Delimiter    POS(439)  */~
                       CH(10),                    /* Routed Customer        */~
                       CH(01),                    /* Delimiter    POS(449)  */~
                       CH(256),                   /* Filler                 */~
                       CH(80)                     /* Filler    RECLEN786    */
                       
           
            lookupGencodes
              gen% = 0%
              read #20, key = readkey$, using GENCODES_FMT, desc$, eod goto ~
                                             gencodesDone

GENCODES_FMT:   FMT POS(25), CH(30)
              gen% = 1%
            gencodesDone
            return

            return

            write_eof
              convert lcnt%  to wcnt$, pic(000000)
              eof$ = "@EOF#"
              init(" ") filler1$
              write #2, using EOF_FMT, eof$, wcnt$, filler1$, filler1$,      ~
                                       filler1$, eod goto cusCnt

EOF_FMT:       FMT CH(5), CH(6), CH(256), CH(256), CH(245)
            cusCnt
              convert ccnt%  to wcnt$, pic(000000)
              write #3, using EOF_FMT, eof$, wcnt$, filler1$, filler1$,       ~
                                       filler1$, eod goto crash
            crash
            return

            open_error
               comp% = 2%
               hdr$  = "******* (Error) (Error) (Error)  *******"
               msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
               msg$(2%) = "(Open Error) - File = " & filename$
               msg$(3%) = "Press Any Key To Continue."
REM CALL "ASKUSER" (COMP%, HDR$, MSG$(1%), MSG$(2%), MSG$(3%))
               goto exitProgram
            return

            exitProgram
              gosub write_eof
               end





