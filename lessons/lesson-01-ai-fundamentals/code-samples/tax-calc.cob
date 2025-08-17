       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAX-CALCULATOR.
      *********************************************************
      * FEDERAL TAX CALCULATION ENGINE                        *
      * PROCESSES INDIVIDUAL INCOME TAX CALCULATIONS          *
      * INCLUDES: STANDARD DEDUCTION, CHILD CREDIT, EIC      *
      * SYSTEM: MAINFRAME TAX PROCESSING                     *
      *********************************************************
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-TAXPAYER-INFO.
           05  WS-TAX-ID                PIC X(11).
           05  WS-FILING-STATUS         PIC X(20).
           05  WS-GROSS-INCOME          PIC 9(8)V99.
           05  WS-NUMBER-OF-CHILDREN    PIC 99.
           05  WS-AGE-65-OR-OLDER       PIC X VALUE 'N'.
           
       01  WS-TAX-CALCULATIONS.
           05  WS-ADJUSTED-GROSS-INCOME PIC 9(8)V99.
           05  WS-STANDARD-DEDUCTION    PIC 9(8)V99.
           05  WS-TAXABLE-INCOME        PIC 9(8)V99.
           05  WS-FEDERAL-TAX           PIC 9(8)V99.
           05  WS-CHILD-TAX-CREDIT      PIC 9(8)V99.
           05  WS-EARNED-INCOME-CREDIT  PIC 9(8)V99.
           05  WS-TOTAL-CREDITS         PIC 9(8)V99.
           05  WS-NET-TAX-DUE           PIC 9(8)V99.
           
       01  WS-TAX-CONSTANTS.
           05  WS-SINGLE-STD-DEDUCTION     PIC 9(8)V99 VALUE 12950.00.
           05  WS-MARRIED-STD-DEDUCTION    PIC 9(8)V99 VALUE 25900.00.
           05  WS-CHILD-CREDIT-AMOUNT      PIC 9(8)V99 VALUE 2000.00.
           05  WS-SENIOR-ADDITIONAL-STD    PIC 9(8)V99 VALUE 1400.00.
           
       PROCEDURE DIVISION.
       MAIN-TAX-CALCULATION.
           PERFORM CALCULATE-STANDARD-DEDUCTION
           PERFORM CALCULATE-TAXABLE-INCOME
           PERFORM CALCULATE-FEDERAL-TAX
           PERFORM CALCULATE-CHILD-CREDIT
           PERFORM CALCULATE-EARNED-INCOME-CREDIT
           PERFORM CALCULATE-NET-TAX
           STOP RUN.
           
       CALCULATE-STANDARD-DEDUCTION.
           IF WS-FILING-STATUS = "SINGLE"
               MOVE WS-SINGLE-STD-DEDUCTION TO WS-STANDARD-DEDUCTION
           ELSE IF WS-FILING-STATUS = "MARRIED_FILING_JOINT"
               MOVE WS-MARRIED-STD-DEDUCTION TO WS-STANDARD-DEDUCTION
           ELSE
               MOVE WS-SINGLE-STD-DEDUCTION TO WS-STANDARD-DEDUCTION
           END-IF
           
           IF WS-AGE-65-OR-OLDER = 'Y'
               ADD WS-SENIOR-ADDITIONAL-STD TO WS-STANDARD-DEDUCTION
           END-IF.
           
       CALCULATE-TAXABLE-INCOME.
           MOVE WS-GROSS-INCOME TO WS-ADJUSTED-GROSS-INCOME
           
           IF WS-ADJUSTED-GROSS-INCOME > WS-STANDARD-DEDUCTION
               SUBTRACT WS-STANDARD-DEDUCTION FROM WS-ADJUSTED-GROSS-INCOME
                   GIVING WS-TAXABLE-INCOME
           ELSE
               MOVE ZERO TO WS-TAXABLE-INCOME
           END-IF.
           
       CALCULATE-FEDERAL-TAX.
      *    SIMPLIFIED TAX BRACKET CALCULATION
      *    10% ON FIRST $10,275
      *    12% ON NEXT $31,500
      *    22% ON REMAINDER (FOR THIS EXAMPLE)
           
           IF WS-TAXABLE-INCOME <= 10275
               COMPUTE WS-FEDERAL-TAX = WS-TAXABLE-INCOME * 0.10
           ELSE IF WS-TAXABLE-INCOME <= 41775
               COMPUTE WS-FEDERAL-TAX = 
                   (10275 * 0.10) + 
                   ((WS-TAXABLE-INCOME - 10275) * 0.12)
           ELSE
               COMPUTE WS-FEDERAL-TAX = 
                   (10275 * 0.10) + 
                   (31500 * 0.12) + 
                   ((WS-TAXABLE-INCOME - 41775) * 0.22)
           END-IF.
           
       CALCULATE-CHILD-CREDIT.
           IF WS-NUMBER-OF-CHILDREN > 0
               COMPUTE WS-CHILD-TAX-CREDIT = 
                   WS-NUMBER-OF-CHILDREN * WS-CHILD-CREDIT-AMOUNT
           ELSE
               MOVE ZERO TO WS-CHILD-TAX-CREDIT
           END-IF.
           
       CALCULATE-EARNED-INCOME-CREDIT.
      *    SIMPLIFIED EIC CALCULATION
      *    ACTUAL CALCULATION IS MUCH MORE COMPLEX
           
           IF WS-NUMBER-OF-CHILDREN > 0 AND WS-GROSS-INCOME < 50000
               IF WS-NUMBER-OF-CHILDREN = 1
                   COMPUTE WS-EARNED-INCOME-CREDIT = 
                       WS-GROSS-INCOME * 0.34
               ELSE
                   COMPUTE WS-EARNED-INCOME-CREDIT = 
                       WS-GROSS-INCOME * 0.40
               END-IF
               
               IF WS-EARNED-INCOME-CREDIT > 6728
                   MOVE 6728 TO WS-EARNED-INCOME-CREDIT
               END-IF
           ELSE
               MOVE ZERO TO WS-EARNED-INCOME-CREDIT
           END-IF.
           
       CALCULATE-NET-TAX.
           ADD WS-CHILD-TAX-CREDIT TO WS-EARNED-INCOME-CREDIT
               GIVING WS-TOTAL-CREDITS
               
           IF WS-FEDERAL-TAX > WS-TOTAL-CREDITS
               SUBTRACT WS-TOTAL-CREDITS FROM WS-FEDERAL-TAX
                   GIVING WS-NET-TAX-DUE
           ELSE
               MOVE ZERO TO WS-NET-TAX-DUE
           END-IF.
