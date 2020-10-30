       IDENTIFICATION DIVISION.
       PROGRAM-ID. ProgramTwo.
       AUTHOR. R C JENKINS. 
      ****************************************************************
      * This is a program that produces a detailed summary report of 
      * the sales made by ASHRALS, Ltd., a company that sells costumes,
      * shoes, and accessories to costume shops
      ****************************************************************
      * INPUT:
      *    The INVENTORY FILE contains the following
      *    data in each record:
      *         1. CUSTOMER ID
      *         2. CUSTOMER NAME
      *         3. PRODUCT ID
      *         4. PRODUCT NAME
      *         5. QUANTITY SOLD
      *         6. COST PER ITEM   
      ****************************************************************
      * OUTPUT:
      *    The INVENTORY REPORT contains the following information:
      *       DETAIL LINE:
      *         1. CUSTOMER ID
      *         2. PRODUCT ID
      *         3. PRODUCT NAME
      *         4. QUANTITY SOLD
      *         5. SALES VALUE
      ****************************************************************
      * CALCULATIONS:
      *    SALES VALUE = QTY SOLD * COST PER ITEM
      *    ACCUMULATE GROUP QUANTITY SOLD TOTAL
      *    ACCUMULATE GROUP SALES VALUE TOTAL
      *    ACCUMULATE GRAND TOTAL FOR ALL QUANTITY SOLD
      *    ACCUMULATE GRAND TOTAL FOR ALL SALES VALUES
      ****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. PC.
       OBJECT-COMPUTER. PC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-IN-FILE
               ASSIGN TO 'PR2FA20.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SALES-REPORT-OUT-FILE
               ASSIGN TO PRINTER 'SalesReportFile.txt'.

       DATA DIVISION.
       FILE SECTION.

       FD  SALES-IN-FILE
           RECORD CONTAINS 60 CHARACTERS.

       01  SALES-RECORD.
           05  SR-CUSTOMER-ID              PIC 9(5).
           05  SR-CUSTOMER-NAME            PIC X(25).
           05  SR-PRODUCT-ID               PIC X(3). 
           05  FILLER                      PIC X(5).
           05  SR-PRODUCT-NAME             PIC X(14).
           05  SR-QUANTITY-SOLD            PIC 9(3).
           05  SR-COST-PER-ITEM            PIC 999V99.

       FD  SALES-REPORT-OUT-FILE
           RECORD CONTAINS 80 CHARACTERS. 

       01  REPORT-RECORD                   PIC X(80).                 

       WORKING-STORAGE SECTION.

       01  WS-E0F-FLAGS.
           05 NO-MORE-DATA                 PIC X       VALUE "N".
           05 FIRST-RECORD                 PIC X(3)    VALUE "YES".
           05 CUSTOMER-ID-HOLD             PIC X(5).
           05 GROUP-FIRST-RECORD           PIC X(3)    VALUE "YES".

       01  WS-DATE.
           05 WS-YEAR                      PIC 99.
           05 WS-MONTH                     PIC 99.
           05 WS-DAY                       PIC 99. 
       
       01  WS-REPORT-FIELDS.
           05 PROPER-SPACING               PIC 9       VALUE 1.

       01  WS-DETAIL-FIELDS.
           05 WS-DF-SALES-VALUE            PIC 9(6)V99 VALUE 0.

       01  WS-GROUP-TOTAL-FIELDS.
           05 WS-GROUP-QTY-SOLD-TOTAL      PIC 9(6)    VALUE 0.
           05 WS-GROUP-SALES-VALUE-TOTAL   PIC 9(7)V99 VALUE 0.

       01  WS-GRAND-TOTAL-FIELDS.
           05 WS-GRAND-QTY-SOLD-TOTAL      PIC 9(7)    VALUE 0.
           05 WS-GRAND-SALES-VALUE-TOTAL   PIC 9(8)V99 VALUE 0. 
        
      ********************    OUTPUT AREA    *************************

       01  REPORT-HEADING-ONE.
           05                      PIC X(34)       VALUE SPACES.
           05                      PIC X(11)       VALUE "ASHRALS LTD".
           05                      PIC X(34)       VALUE SPACES.

       01  REPORT-HEADING-TWO.
           05                      PIC X(10)       VALUE SPACES.
           05  H2-DATE.
               10  H2-MONTH        PIC 99.
               10                  PIC X           VALUE "/".
               10  H2-DAY          PIC 99.
               10                  PIC X           VALUE "/".
               10  H2-YEAR         PIC 99.  
           05                      PIC X(7)        VALUE SPACES.
           05                      PIC X(24)       VALUE 
                                            "SALES SPECULATION REPORT".
           05                      PIC X(18)   VALUE SPACES.
           05                      PIC X(3)    VALUE "RCJ".
           05                      PIC X(10)    VALUE SPACES.

       01  COLUMN-HEADING-THREE.
           05                      PIC X(29)   VALUE SPACES.
           05                      PIC X(4)    VALUE "PROD".
           05                      PIC X(4)    VALUE SPACES.
           05                      PIC X(7)    VALUE "PRODUCT".
           05                      PIC X(9)    VALUE SPACES.
           05                      PIC X(3)    VALUE "QTY".
           05                      PIC X(9)    VALUE SPACES.
           05                      PIC X(5)    VALUE "SALES".
           05                      PIC X(10)   VALUE SPACES.       
       
       01  COLUMN-HEADING-FOUR.
           05                      PIC X(5)    VALUE SPACES.
           05                      PIC X(13)   VALUE "CUSTOMER NAME".
           05                      PIC X(12)   VALUE SPACES.
           05                      PIC X(2)    VALUE "ID".
           05                      PIC X(7)    VALUE SPACES.
           05                      PIC x(4)    VALUE "NAME".
           05                      PIC X(9)    VALUE SPACES.
           05                      PIC x(4)    VALUE "SOLD".
           05                      PIC X(9)    VALUE SPACES.
           05                      PIC X(5)    VALUE "VALUE".
           05                      PIC X(10)   VALUE SPACES.

       01  DETAIL-LINE.
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  DL-CUSTOMER-NAME    PIC X(25).
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  DL-PRODUCT-ID       PIC X(3).
           05  FILLER              PIC X(2)    VALUE SPACES.
           05  DL-PRODUCT-NAME     PIC X(14).
           05  FILLER              PIC X(4)    VALUE SPACES.
           05  DL-QUANTITY-SOLD    PIC ZZZ9.
           05  FILLER              PIC X(5)    VALUE SPACES.
           05  DL-SALES-VALUE      PIC $ZZZ,ZZ9.99.
           05  FILLER              PIC X(8)    VALUE SPACES.

       01  GROUP-TOTAL-LINE.
           05  FILLER              PIC X(41)       VALUE SPACES.
           05                      PIC X(6)        VALUE "TOTAL:".
           05  FILLER              PIC X(3)        VALUE SPACES.
           05  GTL-QTY-SOLD        PIC ZZZ,ZZ9.
           05  FILLER              PIC X(4)        VALUE SPACES.
           05  GTL-SALES-VALUE     PIC $Z,ZZZ,ZZ9.99.
           05  FILLER              PIC X(8)        VALUE SPACES.
       
       01  GRAND-TOTAL-QTY-SOLD-LINE.
           05  FILLER              PIC X(31)       VALUE SPACES.
           05                      PIC X(26)       VALUE 
                                          "GRAND TOTAL QUANTITY SOLD:".
           05  FILLER              PIC X(5)        VALUE SPACES.
           05  GRAND-TL-QTY-SOLD   PIC ZZ,ZZZ,ZZ9.
           05  FILLER              PIC X(10)       VALUE SPACES.

       01  GRAND-TOTAL-SALES-VALUE-LINE. 
           05  FILLER              PIC X(28)       VALUE SPACES.
           05                      PIC X(24)       VALUE 
                                           "GRAND TOTAL SALES VALUE:".
           05  FILLER              PIC X(7)        VALUE SPACES.
           05  GRAND-TL-SALES-VAL  PIC $ZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC X(10)       VALUE SPACES.

      ****************************************************************     

       PROCEDURE DIVISION.
       
       100-MAIN-MODULE.

           PERFORM 150-HOUSEKEEPING
           PERFORM 200-WRITE-REPORT-HEADERS-ROUTINE
           PERFORM 250-WRITE-COLUMN-HEADERS-ROUTINE
           PERFORM 350-READ-INPUT-FILE-ROUTINE
           PERFORM 550-END-OF-FILE-ROUTINE
           PERFORM 600-CLOSE-ROUTINE
        .

       150-HOUSEKEEPING.
           
           OPEN INPUT  SALES-IN-FILE
                OUTPUT SALES-REPORT-OUT-FILE
           ACCEPT WS-DATE FROM DATE 
           MOVE WS-MONTH TO H2-MONTH
           MOVE WS-DAY TO H2-DAY
           MOVE WS-YEAR TO H2-YEAR 
        .

       200-WRITE-REPORT-HEADERS-ROUTINE.
           
           WRITE REPORT-RECORD FROM REPORT-HEADING-ONE
               AFTER ADVANCING PAGE.
      
           MOVE REPORT-HEADING-TWO TO REPORT-RECORD
           PERFORM 300-WRITE-LINES-ROUTINE
        .

       250-WRITE-COLUMN-HEADERS-ROUTINE.
      
           MOVE 3 TO PROPER-SPACING
           MOVE COLUMN-HEADING-THREE TO REPORT-RECORD
           PERFORM 300-WRITE-LINES-ROUTINE
      
           MOVE 1 TO PROPER-SPACING
           MOVE COLUMN-HEADING-FOUR TO REPORT-RECORD
           PERFORM 300-WRITE-LINES-ROUTINE

           MOVE 1 TO PROPER-SPACING
           MOVE SPACES TO REPORT-RECORD
           PERFORM 300-WRITE-LINES-ROUTINE
        . 

       300-WRITE-LINES-ROUTINE.

           WRITE REPORT-RECORD
               AFTER ADVANCING PROPER-SPACING
        . 
       
       350-READ-INPUT-FILE-ROUTINE.

           PERFORM UNTIL NO-MORE-DATA = "Y"
               READ SALES-IN-FILE
                   AT END  
                       MOVE "Y" TO NO-MORE-DATA
                   NOT AT END  
                       PERFORM 400-PROCESS-SALES-FILE-ROUTINE
               END-READ
           END-PERFORM    
        .

       400-PROCESS-SALES-FILE-ROUTINE.
      ***********************
           EVALUATE TRUE
               WHEN FIRST-RECORD = "YES"
                   MOVE "NO" TO FIRST-RECORD
                   MOVE SR-CUSTOMER-ID TO CUSTOMER-ID-HOLD
           
               WHEN SR-CUSTOMER-ID NOT EQUAL CUSTOMER-ID-HOLD
                   PERFORM 500-CONTROL-BREAK
           END-EVALUATE
      *********************** 
      
           MOVE SR-PRODUCT-ID TO DL-PRODUCT-ID
           MOVE SR-PRODUCT-NAME TO DL-PRODUCT-NAME
           MOVE SR-QUANTITY-SOLD TO DL-QUANTITY-SOLD

      *    CALCULATE SALES VALUE FOR EACH DETAIL LINE
           MULTIPLY SR-QUANTITY-SOLD BY SR-COST-PER-ITEM 
               GIVING WS-DF-SALES-VALUE
           MOVE WS-DF-SALES-VALUE TO DL-SALES-VALUE   
     
      *    CALCULATE RUNNING GROUP TOTAL FOR QUANTITY SOLD
           ADD SR-QUANTITY-SOLD TO WS-GROUP-QTY-SOLD-TOTAL
       
      *    CALCULATE RUNNING GROUP TOTAL FOR SALES VALUE
           ADD WS-DF-SALES-VALUE TO WS-GROUP-SALES-VALUE-TOTAL
     
      *    CALCULATE RUNNING GRAND TOTAL FOR QTY SOLD
           ADD SR-QUANTITY-SOLD TO WS-GRAND-QTY-SOLD-TOTAL
     
      *    CALCULATE RUNNING GRAND TOTAL FOR SALES VALUE
           ADD WS-DF-SALES-VALUE TO WS-GRAND-SALES-VALUE-TOTAL
                  
           EVALUATE TRUE
               WHEN GROUP-FIRST-RECORD = "YES"
                   MOVE "NO" TO GROUP-FIRST-RECORD
                   MOVE SR-CUSTOMER-NAME TO DL-CUSTOMER-NAME

                   MOVE DETAIL-LINE TO REPORT-RECORD
                   MOVE 0 TO PROPER-SPACING
                   PERFORM 300-WRITE-LINES-ROUTINE

               WHEN OTHER
                   MOVE SPACES TO DL-CUSTOMER-NAME

                   MOVE DETAIL-LINE TO REPORT-RECORD
                   MOVE 0 TO PROPER-SPACING
                   PERFORM 300-WRITE-LINES-ROUTINE
           END-EVALUATE
        .

       500-CONTROL-BREAK.

           MOVE WS-GROUP-QTY-SOLD-TOTAL TO GTL-QTY-SOLD
           MOVE WS-GROUP-SALES-VALUE-TOTAL TO GTL-SALES-VALUE

           MOVE GROUP-TOTAL-LINE TO REPORT-RECORD

           MOVE 2 TO PROPER-SPACING
           PERFORM 300-WRITE-LINES-ROUTINE
           MOVE 2 TO PROPER-SPACING
           MOVE SPACES TO REPORT-RECORD
           PERFORM 300-WRITE-LINES-ROUTINE

           MOVE ZEROES TO WS-GROUP-QTY-SOLD-TOTAL
           MOVE ZEROES TO WS-GROUP-SALES-VALUE-TOTAL               
           MOVE ZEROES TO GTL-QTY-SOLD
           MOVE ZEROES TO GTL-SALES-VALUE

           MOVE SR-CUSTOMER-ID TO CUSTOMER-ID-HOLD
           MOVE "YES" TO GROUP-FIRST-RECORD
       
        .

       550-END-OF-FILE-ROUTINE.

           PERFORM 500-CONTROL-BREAK
           MOVE WS-GRAND-QTY-SOLD-TOTAL TO GRAND-TL-QTY-SOLD
           MOVE GRAND-TOTAL-QTY-SOLD-LINE TO REPORT-RECORD
           MOVE 1 TO PROPER-SPACING
           PERFORM 300-WRITE-LINES-ROUTINE

           MOVE WS-GRAND-SALES-VALUE-TOTAL TO GRAND-TL-SALES-VAL
           MOVE GRAND-TOTAL-SALES-VALUE-LINE TO REPORT-RECORD
           MOVE 2 TO PROPER-SPACING
           PERFORM 300-WRITE-LINES-ROUTINE
        .

       600-CLOSE-ROUTINE.
           
           CLOSE SALES-IN-FILE
                 SALES-REPORT-OUT-FILE
           STOP RUN
        .
