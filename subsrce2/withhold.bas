        REM *************************************************************~
            *                                                           *~
            *  W   W  IIIII  TTTTT  H   H  H   H   OOO   L      DDDD    *~
            *  W   W    I      T    H   H  H   H  O   O  L      D   D   *~
            *  W   W    I      T    HHHHH  HHHHH  O   O  L      D   D   *~
            *  W W W    I      T    H   H  H   H  O   O  L      D   D   *~
            *   W W   IIIII    T    H   H  H   H   OOO   LLLLL  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WITHHOLD - STANDARD DEDUCTION.     PERCENTAGE OF GROSS TO *~
            *            A GOAL.                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/16/81 ! ORIGINAL                                 ! TEM *~
            * 10/19/81 ! TAKE PERCENTAGE OF CURRENT AMOUNT        ! JAM *~
            *************************************************************

            sub "WITHHOLD" (gross(), amount(), dedamt(), amount)

        dim                                                              ~
            gross (4,2),                 /* GROSS PAY--UNITS/DOLLARS   */~
            amount(5),                   /* AMOUNTS FROM DEDXN RECORD  */~
            dedamt(4)                    /* CURR, MTD, QTD, YTD DEDXNS */~


            REM COMPUTE AMOUNT TO DEDUCT FROM FICA-TYPE SUBROUTINE.
                amount = 0

                REM HAVE WE ALREADY REACHED OUR GOAL
                    if amount(5) <= dedamt(4) then end
                    amount = (amount(1)*.01) * gross(1,2)
                    if amount + dedamt(4) <= amount(5) then L10210
                       amount = amount(5) - dedamt(4)

L10210:        REM ROUND AND GOOO...
                    amount = round (amount, 2%)
                    if amount < 0 then amount = 0

                end

        REM *************************************************************~
            *    N O T E   O N   C O M P U T A T I O N   M E T H O D    *~
            *                                                           *~
            * THE STANDARD METHOD FOR COMPUTING DEDUCTIONS ON THIS      *~
            * SYSTEM IS TO COMPUTE WHAT THE DEDUCTION FOR THE           *~
            * YEAR-TO-DATE *INCLUDING* THIS PAY PERIOD SHOULD BE AND    *~
            * THEN SUBTRACT THE AMOUNT ALREADY TAKEN THIS YEAR.  NOTE   *~
            * THAT YTD GROSS PAY AND DEDUCTIONS DO NOT GET ADDED IN     *~
            * UNTIL AFTER THE CHECKS HAVE BEEN PRINTED, SO WE CAN WIPE  *~
            * OUT WHATEVER'S THERE.                                     *~
            *        ON THIS FICA-TYPE DEDUCTION, THE METHOD IS THAT YOU*~
            * TAKE A PERCENTAGE OF THE GROSS PAY UP TO A GOAL.   THE    *~
            * PERCENTAGE IS ENCODED IN AMOUNT(1) AND THE GOAL IS ENCODED*~
            * IN AMOUNT(5).   THE ALGORITHM IS SET UP TO ENSURE THAT    *~
            * THE GOAL IS NOT EXCEEDED.   NOTE THAT AMOUNT(1) MUST BE   *~
            * DIVIDED BY 100 FOR USE IN THE COMPUTATION.                *~
            *************************************************************

