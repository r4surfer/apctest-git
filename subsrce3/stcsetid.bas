        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC    SSS   EEEEE  TTTTT  IIIII  DDDD    *~
            *  S        T    C   C  S      E        T      I    D   D   *~
            *   SSS     T    C       SSS   EEEE     T      I    D   D   *~
            *      S    T    C   C      S  E        T      I    D   D   *~
            *   SSS     T     CCC    SSS   EEEEE    T    IIIII  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCSETID - Finds and returns current set (or specific set)*~
            *            set id (internal), # of buckets assigned, and  *~
            *            optionally  1. Bucket Id's                     *~
            *                        2. Bucket & Set Descriptions       *~
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
            * 04/22/87 ! Original                                 ! KAB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDQ*

        sub "STCSETID"                                                   ~
                     (rqst%,   /* Function Request (Ret. # Buckets)    */~
                               /*    1. Set and Set Id only            */~
                               /*    2. Add Bucket Id's                */~
                               /*    3. Add Bucket & Set Descriptions  */~
                      #1,      /* SYSFILE2 Channel                     */~
                      set$,    /* Set Requested (' ' = Current)        */~
                      setid$,  /* Internal Set Id                      */~
                      bckid$(),/* Bucket Id    (Optional (2))          */~
                      bckd$(), /* Bucket Descr (Optional (3))          */~
                      setd$)   /* Set Descr    (Optional (3))          */~

        dim                                                              ~
            bckd$(12)20,       /* Bucket Descr (Optional (3))          */~
            bckid$(12)10,      /* Bucket Id    (Optional (2))          */~
            bucketid$(12)10,   /* Bucket Id    (Internal)              */~
            bucketdescr$(12)20,/* Bucket Descr (Internal)              */~
            set$8,             /* Set Requested (' ' = Current)        */~
            setd$30,           /* Set Descr    (Optional (3))          */~
            setdescr$30,       /* Set Descr    (Internal)              */~
            setid$4            /* System Internal Set Id               */~

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L02092
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12            "
L02092: REM *************************************************************
            request% = rqst%

            setid$ = " "
               if request% < 2% then L09020
            init (" ") bckid$()
               if request% < 3% then L09020
            init (" ") bckd$(), setd$

L09020:     if set$ <> " " then L09100

            call "READ100" (#1, "STC.CONTROL", rqst%)
                if rqst% = 0% then end                /* Utter Failure */
            get #1 using L09070, set$
L09070:         FMT POS(28), CH(8)

L09100:     setdescr$ = "STC.HDR." & set$
            call "READ100" (#1, setdescr$, rqst%)
                if rqst% = 0% then end           /* Utter Failure Again*/
            get #1 using L09150, setdescr$, setid$, rqst%, bucketid$(),    ~
                               bucketdescr$()
L09150:         FMT POS(21), CH(30), CH(4), XX(4), BI(1), 12*CH(10),     ~
                    12*CH(20)

            if request% < 2% then end
               mat bckid$ = bucketid$
            if request% < 3% then end
               mat bckd$  = bucketdescr$
               setd$      = setdescr$
            end

