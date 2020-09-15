       IDENTIFICATION DIVISION.
       PROGRAM-ID.     BKBRK.
       AUTHOR.         GARRETT BURNS.
      *
      *                        LAB 6
      *    THIS PROGRAM READS A STUDENT FILE.  YOU WILL NEED TO
      *    INSERT A MULTI-LEVEL CONTROL BREAK ON DEPT AND ON CLASS
      *    PRINT THE DEPT-GROUP-LINE, CLASS-GROUP-LINE, AND
      *    PRINT THE ACCUMULATED NUMBER OF STUDENTS FOR EACH CLASS,
      *    DEPART, AND THE OVER NUMBER OF STUDENTS
      *    CREATE AN ARRAY TO HANDLE THE INCOMING TEST SCORES
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT STUDENT-FILE
      *      ASSIGN TO "STUDENT.DAT"
             ASSIGN TO "Lab6STUDENT.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
      *
           SELECT STUDENT-REPORT-FILE
               ASSIGN TO PRINTER "STUDENTCGB".
      *
       DATA DIVISION.
       FILE SECTION.
      *
       FD STUDENT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  STUDENT-RECORD.
           05  SR-DEPT-CODE                    PIC A(4).
           05  SR-CLASS-CODE                   PIC X(5).
           05  SR-NAME                         PIC X(20).
           
      *  YOU NEED TO CREATE AN ARRAY OF 4 TEST SCORES THAT ARE
      *   NUMERIC NOT SIGNED AND 3 POSITIONS HERE
       01  TEST-SCORES.
           05  TS-TEST1                        PIC 9(3).
           05  TS-TEST2                        PIC 9(3).
           05  TS-TEST3                        PIC 9(3).
           05  TS-TEST4                        PIC 9(3).

      *
       FD  STUDENT-REPORT-FILE
           RECORD CONTAINS 80 CHARACTERS.
      *
       01  REPORT-LINE                     PIC X(80).

      *
       WORKING-STORAGE SECTION.
      *
       01  FLAGS-N-SWITCHES.
           05  EOF-FLAG                    PIC X       VALUE ' '.
               88 NO-MORE-DATA                         VALUE 'N'.
               88 MORE-RECORDS                         VALUE 'Y'.
           05  FIRST-RECORD                PIC X(3)    VALUE 'YES'.
      *
       01  REPORT-FIELDS.
           05  PROPER-SPACING              PIC 9      VALUE 1.
           05  SUB                         PIC 9(2)   VALUE 0.
           05  PAGE-NO                     PIC 9(2)   VALUE 0.

      *
       01  WS-CURRENT-DATE.
           05  WS-YEAR                     PIC 99.
           05  WS-MONTH                    PIC 99.
           05  WS-DAY                      PIC 99.
      *
       01  DETAIL-FIELDS.
           05  DF-TEST-TOTAL                PIC S9(5)  VALUE +0.
           05  DF-TEST-GRADE                PIC S9(5)  VALUE +0.
           05  DF-TEST-AVERAGE              PIC S9(5)V99 VALUE +0.
           05  DF-GRADE                     PIC X.
           05  DF-TOTAL-STUDENTS            PIC S99 VALUE +0.
           05  DF-DEPT-TOTAL                PIC S99 VALUE +0.
           05  DF-DEPT-HOLD                 PIC A(4).
           05  DF-CLASS-HOLD                PIC X(5).
           05  DF-CLASS-TOTAL               PIC S9(5)  VALUE +0.
           
      ************ OUTPUT AREA ************
      
       01  HEADING-ONE.
           05                              PIC X(6) VALUE 'DATE:'.
           05  H1-DATE.
               10  H1-MONTH                PIC Z9.
               10                          PIC X    VALUE '/'.
               10  H1-DAY                  PIC 99.
               10                          PIC X    VALUE '/'.
               10  H1-YEAR                 PIC 99.
           05                              PIC X(7) VALUE SPACES.
           05                              PIC X(25) VALUE
                                           'STUDENT REPORT'.
           05                              PIC X(13) VALUE 'CGB'.
           05                              PIC X(5) VALUE 'PAGE'.
           05  H1-PAGE-NO                  PIC Z9.
      *
       01  HEADING-TWO.
           05                              PIC X(5) VALUE SPACES.
           05                              PIC X(20) VALUE
                                               'DEPARTMENT CODE  '.
           05                              PIC X(5) VALUE SPACES.
           05 H2-DEPT-CODE                 PIC A(4).
      *
       01  HEADING-THREE.
           05                              PIC X(10) VALUE SPACES.
           05                              PIC X(12) VALUE
                                              'CLASS CODE  '.
           05  H3-CLASS-CODE               PIC X(5).
      *
       01  HEADING-FOUR.
           05                              PIC X(19) VALUE SPACES.
           05                              PIC X(11) VALUE 'NAME'.
           05                              PIC X(3) VALUE SPACES.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(8)  VALUE 'SCORE   '.
           05                              PIC X(7) VALUE 'GRADE'.
      *
       01  DETAIL-LINE.
           05                              PIC X(7) VALUE SPACES.
           05 DL-NAME                      PIC X(20).
           05                              PIC X(7).
           
      *  CREATE AN ARRAY HERE TO HANDLE THE OUTGOING TEST
      *  SCORES NAME IT DL-TEST-ARRAY THERE ARE 4
      *  GIVE IT A PICTURE CLAUSE OF PICZ99BBBBB
           05  DL-TEST-ARRAY.
               10  DL-TEST1                PIC Z99BBBBB.
               10  DL-TEST2                PIC Z99BBBBB.
               10  DL-TEST3                PIC Z99BBBBB.
               10  DL-TEST4                PIC Z99BBBBB.

           05 DL-GRADE                     PIC X.

       01  DEPT-GROUP-LINE.
           05                              PIC X(45)   VALUE
                            'TOTAL NUMBER OF STUDENTS FOR DEPARTMENT '.
           05  DGL-DEPT-CODE               PIC X(4).
           05                              PIC X(5)    VALUE ' IS  '.
           05  DGL-DEPT-TOTAL              PIC ZZZ9.
      *
       01  CLASS-GROUP-LINE.
           05                              PIC X(45)   VALUE
                            'TOTAL MUMBER OF STUDENTS FOR CLASS '.
           05  CGL-CLASS-CODE              PIC X(5).
           05                              PIC X(5)    VALUE ' IS  '.
           05  CGL-CLASS-TOTAL             PIC ZZZ9.
      *

      *
       01  OVER-ALL-TOTAL.
           05                              PIC X(54)  VALUE
                           'TOTAL STUDENTS FOR ALL DEPARTMENTS IS '.
           05  OAT-TOTAL                   PIC ZZZZ9.

      *
       PROCEDURE DIVISION.
      *
       100-PRINT-STUDENT-REPORT.

           PERFORM 200-HSKPING-ROUTINE
           PERFORM 300-READ-STUDENT-FILE


       .
       200-HSKPING-ROUTINE.

           OPEN INPUT  STUDENT-FILE
                OUTPUT STUDENT-REPORT-FILE
           ACCEPT WS-CURRENT-DATE FROM DATE
           MOVE WS-MONTH TO H1-MONTH
           MOVE WS-DAY TO H1-DAY
           MOVE WS-YEAR TO H1-YEAR
           PERFORM 400-REPORT-HEADING
       .
       300-READ-STUDENT-FILE.

           PERFORM UNTIL NO-MORE-DATA
               READ STUDENT-FILE
                   AT END
                       MOVE 'N' TO EOF-FLAG
                   NOT AT END
                       PERFORM 500-PROCESS-STUDENT-RECORD
               END-READ
           END-PERFORM

       .

       400-REPORT-HEADING.
           ADD 1 TO PAGE-NO
           MOVE PAGE-NO TO H1-PAGE-NO
           WRITE REPORT-LINE FROM HEADING-ONE
               AFTER ADVANCING PAGE
           MOVE 2 TO PROPER-SPACING


       .

       450-PRINT-DEPT-HEADER.

           MOVE SR-DEPT-CODE TO H2-DEPT-CODE
           WRITE REPORT-LINE FROM HEADING-TWO
               AFTER ADVANCING 2 LINES
           .

       475-PRINT-CLASS-HEADER.

           MOVE SR-CLASS-CODE TO H3-CLASS-CODE
           WRITE REPORT-LINE FROM HEADING-THREE
               AFTER ADVANCING 2 LINES

           WRITE REPORT-LINE FROM HEADING-FOUR
               AFTER ADVANCING 2 LINES
           .

       500-PROCESS-STUDENT-RECORD.

      *  CHECK FOR CONTROL BREAKS HERE
      *  USE AN EVALUATE STATEMENT
           EVALUATE FIRST-RECORD
               WHEN 'YES'
                   MOVE 'NO' TO FIRST-RECORD
                   
                   MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   MOVE SR-DEPT-CODE TO DF-DEPT-HOLD
                   PERFORM 450-PRINT-DEPT-HEADER
                   PERFORM 475-PRINT-CLASS-HEADER
               WHEN SR-DEPT-CODE NOT EQUAL TO DF-DEPT-HOLD
                   PERFORM 700-DEPT-BREAK
                   PERFORM 450-PRINT-DEPT-HEADER
                   PERFORM 475-PRINT-CLASS-HEADER
               WHEN SR-CLASS-CODE NOT EQUAL TO DF-CLASS-HOLD
                   PERFORM 800-CLASS-BREAK
                   PERFORM 475-PRINT-CLASS-HEADER
               
               
               
           EVALUATE TRUE
               WHEN SR-DEPT-CODE = 'COMP'
                   MOVE SR-DEPT-CODE TO DF-DEPT-HOLD
                   
                   WHEN SR-CLASS-CODE = 'CS201'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   WHEN SR-CLASS-CODE = 'CS250'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   WHEN SR-CLASS-CODE = 'CS491'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
               WHEN SR-DEPT-CODE = 'ENGL'
                   MOVE SR-DEPT-CODE TO DF-DEPT-HOLD

                   WHEN SR-CLASS-CODE = 'EN102'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   WHEN SR-CLASS-CODE = 'EN103'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
               WHEN SR-DEPT-CODE = 'MATH'
                   MOVE SR-DEPT-CODE TO DF-DEPT-HOLD

                   WHEN SR-CLASS-CODE = 'MA101'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   WHEN SR-CLASS-CODE = 'MA102'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   WHEN SR-CLASS-CODE = 'MA104'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
                   WHEN SR-CLASS-CODE = 'MA250'
                       MOVE SR-CLASS-CODE TO DF-CLASS-HOLD
           END-EVALUATE



           MOVE SR-NAME TO DL-NAME

      * USE A PERFORM VARYING TO MOVE THE INCOMING ARRAY OF
      * GRADES TO THE DETAIL-LINE
      *  AND TO ADD THE INCOMING ARRAY OF GRADES TO THE
      *  DF-TEST-TOTAL





           DIVIDE DF-TEST-TOTAL BY 4
                  GIVING DF-TEST-AVERAGE ROUNDED

           MOVE DF-TEST-AVERAGE TO DF-TEST-GRADE

           EVALUATE TRUE
               WHEN DF-TEST-GRADE > 89
                   MOVE 'A' TO DF-GRADE
               WHEN DF-TEST-GRADE >= 80 AND DF-TEST-GRADE <= 89
                   MOVE 'B' TO DF-GRADE
               WHEN DF-TEST-GRADE >= 70 AND DF-TEST-GRADE <= 79
                   MOVE 'C' TO DF-GRADE
               WHEN DF-TEST-GRADE >= 60 AND DF-TEST-GRADE <= 69
                   MOVE 'D' TO DF-GRADE
               WHEN DF-TEST-GRADE < 60
                   MOVE 'F' TO DF-GRADE
           END-EVALUATE

           MOVE DF-GRADE TO DL-GRADE

           MOVE DETAIL-LINE TO REPORT-LINE
           PERFORM 600-WRITE-A-LINE

           MOVE 1 TO PROPER-SPACING

           ADD 1 TO DF-TOTAL-STUDENTS
           ADD 1 TO DF-CLASS-TOTAL
           ADD 1 TO DF-DEPT-TOTAL

           MOVE ZEROS TO DF-TEST-AVERAGE
           MOVE ZEROS TO DF-TEST-TOTAL
           MOVE ZEROS TO DF-TEST-GRADE
           .
       600-WRITE-A-LINE.
           WRITE REPORT-LINE
               AFTER ADVANCING PROPER-SPACING
           .

       700-DEPT-BREAK.

      * handle department break here

           MOVE DF-DEPT-HOLD TO DGL-DEPT-CODE
           
           MOVE DF-DEPT-TOTAL TO DGL-DEPT-TOTAL
           
           MOVE DEPT-GROUP-LINE TO REPORT-LINE
           
           MOVE 3 TO PROPER-SPACING
           PERFORM 600-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           
           MOVE ZEROS TO DF-DEPT-TOTAL
           MOVE ZEROS TO DGL-DEPT-TOTAL
           
           MOVE SR-DEPT-CODE TO DF-DEPT-HOLD

       .
      *
       800-CLASS-BREAK.
      *handle class break here
           MOVE DF-CLASS-HOLD TO CGL-CLASS-CODE
           
           MOVE DF-CLASS-TOTAL TO CGL-CLASS-TOTAL
           
           MOVE CLASS-GROUP-LINE TO REPORT-LINE
           
           MOVE 3 TO PROPER-SPACING
           PERFORM 600-WRITE-A-LINE
           MOVE 1 TO PROPER-SPACING
           
           MOVE ZEROS TO DF-CLASS-TOTAL
           MOVE ZEROS TO CGL-CLASS-TOTAL
           
           MOVE SR-CLASS-CODE TO DF-CLASS-HOLD

          .
      *

       900-END-OF-JOB-ROUTINE.
      *    CODE FOR LAST CONTROL LINE GOES HERE
           PERFORM 800-CLASS-BREAK

           .

       1000-PRINT-FINAL-TOTALS.

           MOVE DF-TOTAL-STUDENTS TO OAT-TOTAL
           MOVE OVER-ALL-TOTAL TO REPORT-LINE
           MOVE 3 TO PROPER-SPACING
           PERFORM 600-WRITE-A-LINE

            .


       1100-FINAL-ROUTINE.

           PERFORM 1000-PRINT-FINAL-TOTALS

           CLOSE STUDENT-FILE
                 STUDENT-REPORT-FILE
            STOP RUN
            .

