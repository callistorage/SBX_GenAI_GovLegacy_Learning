       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENEFIT-PROCESSOR.
      *********************************************************
      * GOVERNMENT BENEFIT PROCESSING SYSTEM                 *
      * PROCESSES MONTHLY BENEFIT CALCULATIONS               *
      * FOR FOOD ASSISTANCE PROGRAM                          *
      * LAST UPDATED: 1995-07-20                            *
      *********************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APPLICANT-FILE ASSIGN TO "APPLICANT.DAT"
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD  APPLICANT-FILE.
       01  APPLICANT-RECORD.
           05  APP-ID                  PIC X(8).
           05  APP-LAST-NAME           PIC X(25).
           05  APP-FIRST-NAME          PIC X(20).
           05  APP-HOUSEHOLD-SIZE      PIC 99.
           05  APP-MONTHLY-INCOME      PIC 9(6)V99.
           05  APP-HOUSING-COST        PIC 9(6)V99.
           05  APP-MEDICAL-EXPENSES    PIC 9(6)V99.
           05  APP-STATE-CODE          PIC X(2).
           05  FILLER                  PIC X(15).
           
       WORKING-STORAGE SECTION.
       01  WS-BENEFIT-CALCULATION.
           05  WS-GROSS-INCOME         PIC 9(6)V99.
           05  WS-NET-INCOME           PIC 9(6)V99.
           05  WS-ALLOWABLE-DEDUCTIONS PIC 9(6)V99.
           05  WS-MAXIMUM-BENEFIT      PIC 9(6)V99.
           05  WS-CALCULATED-BENEFIT   PIC 9(6)V99.
           05  WS-FINAL-BENEFIT        PIC 9(6)V99.
           
       01  WS-PROGRAM-CONSTANTS.
           05  WS-STANDARD-DEDUCTION   PIC 9(4) VALUE 167.
           05  WS-HOUSING-DEDUCTION-PCT PIC V99 VALUE .20.
           05  WS-MEDICAL-DEDUCTION-MIN PIC 9(3) VALUE 35.
           05  WS-BENEFIT-REDUCTION-PCT PIC V99 VALUE .30.
           
       01  WS-BENEFIT-TABLE.
           05  WS-BENEFIT-ENTRY OCCURS 10 TIMES.
               10  WS-HOUSEHOLD-SIZE   PIC 99.
               10  WS-MAX-BENEFIT-AMT  PIC 9(4).
               
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT         PIC 9(6) VALUE ZERO.
           05  WS-APPROVED-COUNT       PIC 9(6) VALUE ZERO.
           05  WS-DENIED-COUNT         PIC 9(6) VALUE ZERO.
           05  WS-TABLE-INDEX          PIC 99.
           
       01  WS-FLAGS.
           05  WS-EOF-FLAG             PIC X VALUE 'N'.
           05  WS-VALID-RECORD-FLAG    PIC X VALUE 'Y'.
           
       PROCEDURE DIVISION.
       MAIN-PROCESSING.
           PERFORM INITIALIZE-PROGRAM
           PERFORM LOAD-BENEFIT-TABLE
           PERFORM PROCESS-APPLICANT-FILE
           PERFORM DISPLAY-SUMMARY-TOTALS
           STOP RUN.
           
       INITIALIZE-PROGRAM.
           OPEN INPUT APPLICANT-FILE
           MOVE ZERO TO WS-RECORD-COUNT
           MOVE ZERO TO WS-APPROVED-COUNT  
           MOVE ZERO TO WS-DENIED-COUNT.
           
       LOAD-BENEFIT-TABLE.
           MOVE 1 TO WS-HOUSEHOLD-SIZE(1)
           MOVE 281 TO WS-MAX-BENEFIT-AMT(1)
           MOVE 2 TO WS-HOUSEHOLD-SIZE(2)
           MOVE 516 TO WS-MAX-BENEFIT-AMT(2)
           MOVE 3 TO WS-HOUSEHOLD-SIZE(3)
           MOVE 740 TO WS-MAX-BENEFIT-AMT(3)
           MOVE 4 TO WS-HOUSEHOLD-SIZE(4)
           MOVE 939 TO WS-MAX-BENEFIT-AMT(4)
           MOVE 5 TO WS-HOUSEHOLD-SIZE(5)
           MOVE 1116 TO WS-MAX-BENEFIT-AMT(5)
           MOVE 6 TO WS-HOUSEHOLD-SIZE(6)
           MOVE 1339 TO WS-MAX-BENEFIT-AMT(6)
           MOVE 7 TO WS-HOUSEHOLD-SIZE(7)
           MOVE 1480 TO WS-MAX-BENEFIT-AMT(7)
           MOVE 8 TO WS-HOUSEHOLD-SIZE(8)
           MOVE 1691 TO WS-MAX-BENEFIT-AMT(8).
           
       PROCESS-APPLICANT-FILE.
           READ APPLICANT-FILE
               AT END MOVE 'Y' TO WS-EOF-FLAG
           END-READ
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               ADD 1 TO WS-RECORD-COUNT
               PERFORM VALIDATE-APPLICANT-DATA
               IF WS-VALID-RECORD-FLAG = 'Y'
                   PERFORM CALCULATE-BENEFIT-AMOUNT
                   PERFORM DETERMINE-ELIGIBILITY
               END-IF
               READ APPLICANT-FILE
                   AT END MOVE 'Y' TO WS-EOF-FLAG
               END-READ
           END-PERFORM
           
           CLOSE APPLICANT-FILE.
           
       VALIDATE-APPLICANT-DATA.
           MOVE 'Y' TO WS-VALID-RECORD-FLAG
           
           IF APP-ID = SPACES OR APP-ID = LOW-VALUES
               MOVE 'N' TO WS-VALID-RECORD-FLAG
           END-IF
           
           IF APP-HOUSEHOLD-SIZE < 1 OR APP-HOUSEHOLD-SIZE > 8
               MOVE 'N' TO WS-VALID-RECORD-FLAG
           END-IF
           
           IF APP-MONTHLY-INCOME < 0
               MOVE 'N' TO WS-VALID-RECORD-FLAG
           END-IF.
           
       CALCULATE-BENEFIT-AMOUNT.
           MOVE APP-MONTHLY-INCOME TO WS-GROSS-INCOME
           
           PERFORM CALCULATE-DEDUCTIONS
           
           SUBTRACT WS-ALLOWABLE-DEDUCTIONS FROM WS-GROSS-INCOME
               GIVING WS-NET-INCOME
               
           IF WS-NET-INCOME < 0
               MOVE 0 TO WS-NET-INCOME
           END-IF
           
           PERFORM GET-MAXIMUM-BENEFIT
           
           COMPUTE WS-CALCULATED-BENEFIT = 
               WS-MAXIMUM-BENEFIT - (WS-NET-INCOME * WS-BENEFIT-REDUCTION-PCT)
               
           IF WS-CALCULATED-BENEFIT < 0
               MOVE 0 TO WS-CALCULATED-BENEFIT
           END-IF
           
           MOVE WS-CALCULATED-BENEFIT TO WS-FINAL-BENEFIT.
           
       CALCULATE-DEDUCTIONS.
           MOVE WS-STANDARD-DEDUCTION TO WS-ALLOWABLE-DEDUCTIONS
           
           IF APP-HOUSING-COST > 0
               COMPUTE WS-ALLOWABLE-DEDUCTIONS = 
                   WS-ALLOWABLE-DEDUCTIONS + 
                   (APP-HOUSING-COST * WS-HOUSING-DEDUCTION-PCT)
           END-IF
           
           IF APP-MEDICAL-EXPENSES > WS-MEDICAL-DEDUCTION-MIN
               ADD APP-MEDICAL-EXPENSES TO WS-ALLOWABLE-DEDUCTIONS
           END-IF.
           
       GET-MAXIMUM-BENEFIT.
           PERFORM VARYING WS-TABLE-INDEX FROM 1 BY 1
               UNTIL WS-TABLE-INDEX > 8
               IF WS-HOUSEHOLD-SIZE(WS-TABLE-INDEX) = APP-HOUSEHOLD-SIZE
                   MOVE WS-MAX-BENEFIT-AMT(WS-TABLE-INDEX) TO WS-MAXIMUM-BENEFIT
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           
       DETERMINE-ELIGIBILITY.
           IF WS-FINAL-BENEFIT > 0
               ADD 1 TO WS-APPROVED-COUNT
               DISPLAY "APPROVED: " APP-ID " BENEFIT: " WS-FINAL-BENEFIT
           ELSE
               ADD 1 TO WS-DENIED-COUNT
               DISPLAY "DENIED: " APP-ID " INSUFFICIENT BENEFIT"
           END-IF.
           
       DISPLAY-SUMMARY-TOTALS.
           DISPLAY "PROCESSING COMPLETE"
           DISPLAY "TOTAL RECORDS PROCESSED: " WS-RECORD-COUNT
           DISPLAY "APPLICATIONS APPROVED: " WS-APPROVED-COUNT
           DISPLAY "APPLICATIONS DENIED: " WS-DENIED-COUNT.
