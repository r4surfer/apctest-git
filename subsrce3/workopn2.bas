        REM *************************************************************~
            *                                                           *~
            *  W   W   OOO   RRRR   K   K   OOO   PPPP   N   N   2222   *~
            *  W   W  O   O  R   R  K  K   O   O  P   P  NN  N  2   2   *~
            *  W   W  O   O  RRRR   KKK    O   O  PPPP   N N N     2    *~
            *  W W W  O   O  R   R  K  K   O   O  P      N  NN    2     *~
            *   W W    OOO   R   R  K   K   OOO   P      N   N   22222  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WORKOPN2 - Opens a System Work File (## file name) in the *~
            *            desired mode. This routine differs from        *~
            *            WORKOPEN in that it does NOT create a new work *~
            *            file each time it is called.  Rather it is the *~
            *            calling program's responsibility to first call *~
            *            WORKOPN2 with mode = OUTPUT to create the work *~
            *            file.  This allows more flexibility in handling*~
            *            workfiles in that a progam may now open a work *~
            *            file in output mode, write to it in that mode, *~
            *            and then close the workfile and open it again  *~
            *            in INPUT or IO mode. (Note; it is much faster  *~
            *            to write to a file in OUTPUT mode than any     *~
            *            other mode).   Inputs are the UFB address of   *~
            *            the file, the desired Mode (same as OPENFILE), *~
            *            the initial number of records or size of the   *~
            *            work file (used when mode = OUTPT only) and a  *~
            *            status return code.                            *~
            *            Valid 'Modes' are;      OUTPT,                 *~
            *                                    INPUT,                 *~
            *                                    IO   ,                 *~
            *                                    EXTND,(consec files)   *~
            *                                    SHARE                  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/28/84 ! ORIGINAL  (cloned from WORKOPEN)         ! LDJ *~
            * 01/24/86 ! KILL GETUFBF1, TEST SPACE% (NO GETPARM)  ! KAB *~
            * 10/24/86 ! Added BLOCKS Parameter to OPEN modes     ! LDJ *~
            *          !   other than OUTPUT.                     !     *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            * 06/25/92 ! Changed NOGETPARM to NODISPLAY so that   ! JDH *~
            *          !   number of records can be overridden.   !     *~
            * 09/10/93 ! Support for poss. Unix flat ASCII files  ! KAB *~
            *************************************************************

            sub "WORKOPN2" (#1, mode$, space%, f2%)

        dim                                                              ~
            file$8,                      /* FILE NAME                  */~
            library$8,                   /* LIBRARY                    */~
            mode$5,                      /* MODE TO OPEN FILE IN       */~
            volume$6                     /* VOLUME                     */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 09/02/96 Support for Native Unix Files"
        REM *************************************************************
            f2% = 1

        REM *************************************************************~
            *     CHECK TO SEE IF FILE ALREADY OPEN ON THIS CHANNEL     *~
            *************************************************************
            call "GETUFBS1" addr(#1, ufbf1%)
                 if ufbf1% =  0% then L10090
                    close #1
                 /* IF UFBF1 FLAG LAST BIT = 1 THEN FILE ALREADY OPEN */

        REM *************************************************************~
            *                 NOW WE OPEN THE WORK FILE !               *~
            *************************************************************
L10090:     if mode$ <> "OUTPT" then L10120
               file$ = "##      " : library$, volume$ = " "
               nrecs% = max(10%, space%)
               open nodisplay #1, output,                                ~
                                space = nrecs%, dpack=100%, ipack=100%,  ~
                                file = file$,                            ~
                                library = library$, volume = volume$,    ~
                                blocks = 5%
               f2% = 0
               end
L10120:     if mode$ <> "INPUT" then L10170
               open nodisplay #1, input, blocks = 5%
               f2% = 0
               end
L10170:     if mode$ <> "EXTND" then L10220
               open nodisplay #1, extend, blocks = 5%
               f2% = 0
               end
L10220:     if mode$ <> "IO   " then L10270
               open nodisplay #1, io, blocks = 5%
               f2% = 0
               end
L10270:     if mode$ <> "SHARE" then L10320
               open nodisplay #1, shared
               f2% = 0
               end
L10320:     if mode$ <> "SPLIN" then L10360
               open nodisplay #1, spclinp, blocks = 5%
               f2% = 0
               end
L10360:     if mode$ <> "OUTSP" then L10500 /* OUTPUT, SPECIFIED */
*             CALL "GETNAMES" ADDR(#1, FILE$, LIBRARY$, VOLUME$)
*             GOTO 10092
               nrecs% = max(10%, space%)
               open nodisplay #1, output,                                ~
                                space = nrecs%, dpack=100%, ipack=100%,  ~
                                blocks = 5%
               f2% = 0
               end
        /* Note - Caller must close file, if open and call 'PUTNAMES'   */
        /* Example  CLOSE #?   (If necessary)                           */
        /*          CALL "PUTNAMES" ADDR(#?, FILE$, LIBRARY$, VOLUME$)  */
        /*          CALL "WORKOPN2" (#?, "OUTSP", SPACE%, F2%)          */
*       ** Special Stuff here to keep unix compatibility out of callers

L10500:     if mode$ <> "FAINP" then end   /* Flat, ASCII Files */
*             OPEN NODISPLAY #1, INPUT    /* Use this on WANG VS       */
               open nodisplay #1, fainp    /* Use this on UNIX          */
               f2% = 0
               end
