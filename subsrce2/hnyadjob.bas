        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA   DDDD   JJJJJ   OOO   BBBB    *~
            *  H   H  NN  N  Y   Y  A   A  D   D    J    O   O  B   B   *~
            *  HHHHH  N N N   YYY   AAAAA  D   D    J    O   O  BBBB    *~
            *  H   H  N  NN    Y    A   A  D   D  J J    O   O  B   B   *~
            *  H   H  N   N    Y    A   A  DDDD    J      OOO   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYADJOB - This sub looks to see if any inventory has     *~
            *            been moved 'from' a job back into inventory    *~
            *            by Inventory Additions. If inventory has been  *~
            *            moved from a job the HNYQUAN onhand, total $   *~
            *            materials (JOBMASTR) are adjusted, and a detail*~
            *            record is created in JOBMTLDR.                 *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/20/81 ! ORIGINAL                                 ! TOM *~
            * 11/25/85 ! Corrected Rounding Error                 ! LDJ *~
            *          !   Ignore remarks in program about posting!     *~
            *          !   G/L & Inventory - doesn't do it.       !     *~
            * 01/27/87 ! Renamed JOBHNYAD to HNYADJOB and turned  ! LDJ *~
            *          !   it into a subroutine called by         !     *~
            *          !   HNYADDJN - that way we don't have to   !     *~
            *          !   read the buffer twice.                 !     *~
            *          !   (Orig got in here just to change the   !     *~
            *          !   ADDBUFFR file layout).                 !     *~
            * 05/13/87 ! Std Cost Changes.                        ! ERN *~
            * 05/28/93 ! PRR 12927.  Corrected subscript in call  ! JDH *~
            *          !   to PIPFLAGS.                           !     *~
            *************************************************************

        sub "HNYADJOB" (date$,           /* Current System Date        */~
                        hnydate$,        /* Inventory Posting Date     */~
                        #5,              /* HNYADDTF record area       */~
                        #13,             /* PIPMASTR UFB               */~
                        #14)             /* SFCUM2   UFB               */

        dim                                                              ~
            costs(12),                   /* Inventory Costs            */~
            date$8,                      /* Date                       */~
            hnydate$6,                   /* User's inventory date      */~
            jobnr$8,                     /* Job number                 */~
            lot$16,                      /* Lot number                 */~
            part$25,                     /* Part number to do          */~
            record$100,                  /* PIPIN Record area          */~
            tagnr$19,                    /* PIP Tag                    */~
            store$3                      /* Store number               */~

        dim f2%(15),                     /* File status flags for      */~
            f1%(15),                     /* Record-on-file flags       */~
            rslt$(15)20,                 /* Return code message        */~
            fs%(15)                      /* File status flag           */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            *  #2 ! JOBMASTR ! Job master file                          *~
            *  #5 ! HNYADDTF ! Additions buffer for inventory           *~
            *  #6 ! JOBMTLDR ! Job material direct detail file          *~
            * #13 ! PIPMASTR ! Planned inv. position master             *~
            * #14 ! SFCUM2   ! Cumulative forcast for pipflags          *~
            * #15 ! PIPIN    ! Planned input                            *~
            *************************************************************

            select  #2, "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select #6, "JOBMTLDR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 200,                                    ~
                       keypos = 1, keylen = 16

            select #15, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 60,                                    ~
                        keypos = 30, keylen = 19,                        ~
                        alternate key 1, keypos = 1, keylen = 48

            if fs%(2) =  0% then                                         ~
               call "OPENCHCK" (#2,  fs%(2) , f2%( 2), 0%,  rslt$( 2))
            if fs%(6) =  0% then                                         ~
               call "OPENCHCK" (#6,  fs%(6) , f2%( 6), 1%,  rslt$( 6))
            if fs%(15)=  0% then                                         ~
               call "OPENCHCK" (#15, fs%(15), f2%(15), 0%,  rslt$(15))

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *-----------------------------------------------------------*~
            * Only posts material that has been xferred from Job to HNY.*~
            *************************************************************

            gosub dataload               /* Get buffer information     */
            gosub process_data           /* Process the data           */
            goto exit_routine            /* Get outta town             */

        process_data
*          Post inventory material to detail file
            unitcost  = 0
            for c% = 1% to 12%
                unitcost = unitcost + costs(c%)
            next c%
            totalcost = round(quantity*unitcost,4)
            call "JMTAPOST" (jobnr$, part$, store$, lot$, date$,         ~
                             quantity, unitcost, totalcost, #2, #6,      ~
                             f2%(2), f2%(6), u3%)

            tagnr$="JOB(PR):" & str(jobnr$,1,8) & hex(000000)

L10210:     call "PLOWNXT1" (#15, tagnr$, 16%, f1%(15))
            if f1%(15)=0 then return
            get #15, using L10240, record$
L10240:          FMT CH(60)
            if str(record$,1,25) <> str(part$,1,25) then L10210
            get #15, using L10270, indate%, pipquantity
L10270:          FMT XX(25), BI(4), XX(19), PD (14,4)
            remquantity=max(round(pipquantity-quantity,2%),0)
            if remquantity>0 then L10370

            delete #15
            call "PIPFLAGS" (part$,1%,indate%,-pipquantity,#13,#14)
            quantity = round(quantity - pipquantity ,2%)
            if quantity > 0 then L10210
            return

L10370:     put str(record$,49,8), using L10380, remquantity
L10380:          FMT PD(14,4)
            rewrite #15, using L10240, record$
            call "PIPFLAGS" (part$,1%,indate%,-quantity,#13,#14)
            return

        REM *************************************************************~
            *     G E T   I N F O R M A T I O N   F R O M   D I S K     *~
            *-----------------------------------------------------------*~
            * Gets information from disk and puts it into the various   *~
            * fields.  Then we return to process it.                    *~
            *************************************************************
        dataload
            get #5, using L30120,                                         ~
                    part$, store$, lot$, jobnr$, quantity, costs()
            if jobnr$ = " " then exit_routine
            return

L30120:     FMT POS(10),                                                 ~
                CH(25),                  /* Part number                */~
                CH(3),                   /* Store number               */~
                CH(16),                  /* Lot number                 */~
                CH(8),                   /* Job number                 */~
                PD(14,4),                /* Quantity                   */~
                POS(88),                                                 ~
                12*PD(14,4)              /* Inventory Costs            */

            u3% = u3%

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_routine
            end
