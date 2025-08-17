       IDENTIFICATION DIVISION.
       PROGRAM-ID. RISK-ASSESSMENT.
      *********************************************************
      * GOVERNMENT RISK ASSESSMENT PROGRAM                   *
      * USED FOR BENEFIT ELIGIBILITY DETERMINATION          *
      * LAST MODIFIED: 1998-03-15                           *
      *********************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-MAINFRAME.
       OBJECT-COMPUTER. IBM-MAINFRAME.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-APPLICANT-RECORD.
           05  WS-APPLICANT-ID         PIC X(10).
           05  WS-ANNUAL-INCOME        PIC 9(8)V99.
           05  WS-FAMILY-SIZE          PIC 99.
           05  WS-EMPLOYMENT-STATUS    PIC X(10).
           05  WS-TOTAL-ASSETS         PIC 9(8)V99.
           
       01  WS-RISK-INDICATORS.
           05  WS-INCOME-RISK          PIC X(10).
           05  WS-ASSET-RISK           PIC X(10).
           05  WS-EMPLOYMENT-RISK      PIC X(10).
           05  WS-OVERALL-RISK         PIC X(10).
           
       01  WS-CONSTANTS.
           05  WS-HIGH-INCOME-LIMIT    PIC 9(8)V99 VALUE 75000.00.
           05  WS-HIGH-ASSET-LIMIT     PIC 9(8)V99 VALUE 50000.00.
           
       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INITIALIZE-VALUES
           PERFORM CALCULATE-INCOME-RISK
           PERFORM CALCULATE-ASSET-RISK  
           PERFORM CALCULATE-EMPLOYMENT-RISK
           PERFORM DETERMINE-OVERALL-RISK
           PERFORM DISPLAY-RESULTS
           STOP RUN.
           
       INITIALIZE-VALUES.
           MOVE SPACES TO WS-RISK-INDICATORS
           MOVE SPACES TO WS-OVERALL-RISK.
           
       CALCULATE-INCOME-RISK.
           IF WS-ANNUAL-INCOME > WS-HIGH-INCOME-LIMIT
               MOVE "HIGH" TO WS-INCOME-RISK
           ELSE
               MOVE "LOW" TO WS-INCOME-RISK
           END-IF.
           
       CALCULATE-ASSET-RISK.
           IF WS-TOTAL-ASSETS > WS-HIGH-ASSET-LIMIT
               MOVE "HIGH" TO WS-ASSET-RISK
           ELSE
               MOVE "LOW" TO WS-ASSET-RISK
           END-IF.
           
       CALCULATE-EMPLOYMENT-RISK.
           EVALUATE WS-EMPLOYMENT-STATUS
               WHEN "UNEMPLOYED"
                   MOVE "HIGH" TO WS-EMPLOYMENT-RISK
               WHEN "PART-TIME"
                   MOVE "MEDIUM" TO WS-EMPLOYMENT-RISK
               WHEN "FULL-TIME"
                   MOVE "LOW" TO WS-EMPLOYMENT-RISK
               WHEN OTHER
                   MOVE "UNKNOWN" TO WS-EMPLOYMENT-RISK
           END-EVALUATE.
           
       DETERMINE-OVERALL-RISK.
           IF WS-INCOME-RISK = "HIGH" OR WS-ASSET-RISK = "HIGH"
               MOVE "HIGH" TO WS-OVERALL-RISK
           ELSE
               IF WS-EMPLOYMENT-RISK = "HIGH"
                   MOVE "MEDIUM" TO WS-OVERALL-RISK
               ELSE
                   MOVE "LOW" TO WS-OVERALL-RISK
               END-IF
           END-IF.
           
       DISPLAY-RESULTS.
           DISPLAY "RISK ASSESSMENT COMPLETE"
           DISPLAY "APPLICANT ID: " WS-APPLICANT-ID
           DISPLAY "OVERALL RISK: " WS-OVERALL-RISK.
