TITLE         COMMITTMENTS ANNUAL REPORT PROGRAM
SET BLANKS = 0
FILE HANDLE   DCOM90 / NAME='$USER.DATA.DCOM90' / LRECL = 80
DATA LIST     FILE = DCOM90 RECORDS=5
               / 1 SEQN 1-5 CINST 6 (A) SEX 7 (A)
                   ODCM 8-9 ODCD 10-11 ODCY 12-13
                    DCM 14-15 DCD 16-17 DCY 18-19 TCOM 20-21
                   ORIGSRC 22-23 ORGORG 24-25
               / 2 JCRD 6-10
                   OFF1 11-13 OFF2 14-16 OFF3 17-19 OFF4 20-22
                   OFFCNT 23-27
                   SENTYPE 28 CODEF 29-33 VICTIM 34-38
                   MINSENT 39-44 MAXSENT 45-50
                   MINY 39-40 MINM 41-42 MIND 43-44
                   MAXY 45-46 MAXM 47-48 MAXD 49-50
                   PEM 51-52 PED 53-54 PEY 55-56
                   EFFM 57-58 EFFD 59-60 EFFY 61-62
                   SENTM 63-64 SENTD 65-66 SENTY 67-68
               / 3 DOBM 6-7 DOBD 8-9 DOBY 10-11
                   POB 12-14 CIT 15-17 ADR 18-20
                   H1 21 H2 22 TMSP 23 TJLD 24 RACE 25 MAR 26 MIL 27
                   GED 28 LGC 29-30 DRUG 31
               / 4 CONVCTS 6-10 PROBNTS 11-15 DEFAULTS 16-20
                   D1M 21-22 D1D 23-24 D1Y 25-26 DKM 27-28 DKD 29-30
                   DKY 31-32 DGM 33-34 DGD 35-36 DGY 37-38
                   CAT 39-43 CAP 44-48 CAPR 49-53 CAS 54-58 CAN 59-63
                   CAD 64-68 CAE 69-73
               / 5 INJ 6-10 INH 11-15 INS 16-20
                   PJ 21-25 PVJ 26-30 PA 31-35 PVA 36-40 PROB 41
COMPUTE        UMINSENT=MINSENT
COMPUTE        UMAXSENT=MAXSENT
IF             (SENTYPE EQ 9)UMAXSENT=-9
COMPUTE        UOFF1=OFF1
IF             (CINST EQ 'F' AND MAXSENT GE 20600)FRAM=2
IF             (CINST EQ 'F' AND MAXSENT LT 20600)FRAM=3
IF             (CINST EQ 'F' AND MINSENT GT 0)FRAM=1
IF             (CINST EQ 'F' AND MINSENT LE 0 AND MAXSENT LE 0)FRAM=4
VALUE LABELS   FRAM
                  1 'PRISON'
                  2 'REFORMATORY'
                  3 'COUNTY'
                  4 'UNKNOWN'
VAR LABELS     DCM 'MONTH OF COMMITMENT'
VALUE LABELS   DCM 1 'JANUARY'
                   2 'FEBRUARY'
                   3 'MARCH'
                   4 'APRIL'
                   5 'MAY'
                   6 'JUNE'
                   7 'JULY'
                   8 'AUGUST'
                   9 'SEPTEMBER'
                  10 'OCTOBER'
                  11 'NOVEMBER'
                  12 'DECEMBER'
RECODE         SENTYPE (5=4)(6,7=5)(8=6)
VALUE LABELS   SENTYPE
                  0 'UNKNOWN'
                  1 'SIMPLE'
                  2 'CONCURRENT'
                  3 'AGGREGATE'
                  4 'FORTHWITH DOC-HOC'
                  5 'FROM AND AFTER DOC-HOC'
                  6 'SPLIT'
                  9 'FINE'
VAR LABELS     ORGORG 'COURT FROM WHICH COMMITTED'
COMPUTE        ORGCT=ORGORG
VAR LABELS     ORGCT 'COUNTY OF COURT FROM WHICH COMMITTED'
RECODE         ORGCT
               (15 THRU 22,35,88=13)
                  (23,68 THRU 71=11)
                           (24,25=1)
                (26 THRU 29,53,78=2)
                      (30 THRU 33=3)
                  (34,73 THRU 75=12)
               (36,47,48,51,77,94=7)
                              (37=4)
  (38 THRU 43,52,66,72,89 THRU 93=5)
         (44,54,79 THRU 87,97,98=14)
                           (45,46=6)
                           (49,50=8)
       (55 THRU 63,65,67,76,95,96=9)
                             (64=10)
RECODE         ORGORG
                     (15 THRU 23=15)
                     (24 THRU 98=16)
                             (99=17)
                              (0=18)
VALUE LABELS   ORGORG,ORGCT
               1 'BARNSTABLE'
               2 'BERKSHIRE'
               3 'BRISTOL'
               4 'DUKES'
               5 'ESSEX'
               6 'FRANKLIN'
               7 'HAMPDEN'
               8 'HAMPSHIRE'
               9 'MIDDLESEX'
              10 'NANTUCKET'
              11 'NORFOLK'
              12 'PLYMOUTH'
              13 'SUFFOLK'
              14 'WORCESTER'
              15 'MUNICIPAL COURTS'
              16 'DISTRICT COURTS '
              17 'OUT OF STATE' 18 'UNKNOWN'
COMPUTE    PE=(PEY-DCY)*12+PEM-DCM
RECODE     PE
             (0 THRU 3=2)
             (4 THRU 6=3)
             (7 THRU 9=4)
             (10 THRU 12=5)
             (13 THRU 18 =6)
             (19 THRU 24=7)
             (25 THRU 36=8)
             (37 THRU 60=9)
             (61 THRU 120=10)
             (121 THRU 180=11)
             (181 THRU HI=12)
IF           ((PEY GE 0 AND PEY LE 48)AND(PEM GT 0))PE=11
IF           (PEY LT 90 AND PEY GE 78)PE=1
IF           (PEY EQ 90 AND (PEM LT DCM))PE=1
IF           (PEM EQ 0 AND PEY EQ 0)PE=0
IF           (PEM LE 0 AND OFF1 EQ 110 AND MINSENT EQ 888888)
               PE = 13
IF           (PEM EQ 99)PE=14
IF (SEQN EQ 1952 AND PEY EQ 89)PE=11
VALUE LABELS   PE
                 0 'NO PE DATE'
                 1 'PAST PE DATE'
                 2 '1 TO 3 MONTHS'
                 3 '4 TO 6 MONTHS'
                 4 '7 TO 9 MONTHS'
                 5 '10 TO 12 MONTHS'
                 6 '13 TO 18 MONTHS'
                 7 '19 TO 24 MONTHS'
                 8 '2 TO 3 YEARS'
                 9 '3 TO 5 YEARS'
                10 '5 TO 10'
                11 '10 YEARS OR MORE'
                13 'LIFE'
                14 'DEATH'
                15 'UNKNOWN'
VALUE LABELS   SEX
                   'M' 'MALE'
                   'F' 'FEMALE'
IF             (ORIGSRC EQ 18 OR ORIGSRC EQ 18 OR ORIGSRC EQ 19)
                   CINST = 'O'
VAR LABELS     CINST 'COMMITTING INSTITUTION '
RECODE       CINST('W'='A')
VALUE LABELS   CINST
                  'A' 'WALPOLE'
                  'C' 'CONCORD'
                  'F' 'FRAMINGHAM'
                  'L' 'LONGWOOD'
                  'O' 'OTHER JURISDICTION '
VAR LABELS     JCRD 'JAIL CREDITS'
RECODE         JCRD
                           (0 = 0)
                     (1 THRU 10=1)
                    (11 THRU 50=2)
                   (51 THRU 100=3)
                  (101 THRU 150=4)
                  (151 THRU 200=5)
                   (200 THRU HI=6)
                            (-0=7)
VALUE LABELS   JCRD
                   0 'NONE'
                   1 '1 TO 10'
                   2 '11 TO 50'
                   3 '51 TO 100'
                   4 '101 TO 150'
                   5 '151 TO 200'
                   6 'OVER 200'
                   7 'UNKNOWN'
COMPUTE        HISP=RACE
RECODE         RACE
                   (6=1)
                   (7=2)
                   (4=4)
                   (0=6)
VALUE LABELS   RACE
                  1 'WHITE'
                  2 'BLACK'
                  3 'NATIVE AMERICAN'
                  4 'ASIATIC'
                  5 'HISPANIC'
                  6 'UNKNOWN'
RECODE         HISP
                 (1,2,3,4=1)
                       (6=2)
                       (7=3)
                       (5=4)
VALUE LABELS   HISP
                   1 'NON-HISPANIC'
                   2 'WHITE HISPANIC'
                   3 'BLACK HISPANIC'
                   4 'HISPANIC-UNSPECIFIED'
                   0 'UNKNOWN'
VAR LABELS      MAR 'MARITAL STATUS'
RECODE          MAR (-0,0,9=7)
VALUE LABELS    MAR
                   1 'MARRIED'
                   2 'SINGLE'
                   3 'DIVORCED'
                   4 'WIDOWED'
                   5 'COMMON LAW'
                   6 'SEPARATED'
                   7 'UNKNOWN'
VAR LABELS     MIL 'MILITARY DISCHARGE'
RECODE         MIL(-0,0,9=7)
VALUE LABELS   MIL
                  1 'NO SERVICE'
                  2 'HONORABLE'
                  3 'DISHONORABLE'
                  4 'BAD CONDUCT'
                  5 'MEDICAL'
                  6 'DISCHARGE UNK'
                  7 'UNKNOWN'
COMPUTE        COUNTY=ADR
COMPUTE        UADR=ADR
COMPUTE        SMSA=ADR
VAR LABELS      ADR    'PRIOR ADDRESS-SELECTED TOWNS'
               /COUNTY 'PRIOR ADDRESS-COUNTY'
               /SMSA   'PRIOR ADDRESS-SMSA '
RECODE         ADR
                   (51=1)
                  (111=2)
                   (61=3)
                  (151=4)
                  (265=5)
                  (221=6)
                  (231=7)
                  (241=8)
                  (251=9)
                 (301=10)
                 (401=11)
                 (101=12)
                 (431=13)
                 (501=14)
         (800 THRU HI=16)
                   (0=17)
                (ELSE=15)
   /SMSA
                      (51,52,61 THRU 63,71 THRU 74,
                         81,91 THRU 94,101,120,123,
                      125,127,130,132,141 THRU 143,
                  251 THRU 255,262,265,268,270,274,
            361 THRU 363,365 THRU 370,401 THRU 413,
                              416 THRU 420,423,424,
                 471,473,475 THRU 479,491 THRU 497=1)
            (111 THRU 114,117,118,122,133 THRU 135=2)
                                 (151,153 THRU 155=3)
                          (161,172,173,176,178,179=4)
                (211,213 THRU 215,231 THRU 234,327=5)
                        (241 THRU 244,247 THRU 249=6)
                          (301,302,304,306,310,312=7)
                             (371,375,380 THRU 383=8)
             (21,24,26,27,282 THRU 284,290,465,466=9)
              (221,223,341,347,349,350,431,432,435,
                       436,438,440,442,443,445,447,
                                     452 THRU 455=10)
     (177,263,269,292,293,485,488,501 THRU 509,512,
                                 514,516 THRU 519=11)
                                     (800 THRU HI=13)
                                               (0=14)
                                            (ELSE=12)
VALUE LABELS
  ADR
          1 'BOSTON'
          2 'BROCKTON'
          3 'CAMBRIDGE'
          4 'FALL RIVER'
          5 'FRAMINGHAM'
          6 'HOLYOKE'
          7 'LAWRENCE'
          8 'LOWELL'
          9 'LYNN'
         10 'NEW BEDFORD'
         11 'QUINCY'
         12 'SOMERVILLE'
         13 'SPRINGFIELD '
         14 'WORCESTER'
         15 'OTHER MASS'
         16 'OUT OF STATE'
         17 'UNKNOWN'
   /SMSA
          1 'BOSTON'
          2 'BROCKTON'
          3 'FALL RIVER'
          4 'FITCHBURG-LEOMINSTER'
          5 'LAWRENCE-HAVERHILL'
          6 'LOWELL'
          7 'NEW BEDFORD'
          8 'PITTSFIELD'
          9 'PROVIDENCE-PAW-WARWICK'
         10 'SPRINGFIELD'
         11 'WORCESTER'
         12 'OTHER MASS'
         13 'OUT OF STATE'
         14 'UNKNOWN'
RECODE   COUNTY
          (1 THRU 4,7 THRU 10,161,162,
           166,167,169 THRU 173,175,
           177,179,180,263,264,269,272,
           281,283,286,289,290,292,293,294,452,
                        481 THRU 488,501 THRU 520=1)
                         (5,6,12,181 THRU 204,335=2)
          (61 THRU 91,93,101,163 THRU 165,168,
           174,176,178,241 THRU 249,
           261,262,265 THRU 268,
           270,273,274,285,287,312,354,
                     471 THRU 479,491 THRU 497,626=3)
                                  (51,141 THRU 143=4)
          (22,27,52,92,94,113,123,
           130,133,282,284,288,
                 361 THRU 370,401 THRU 403,406,409=5)
                                               (26=5)
          (21,23 THRU 25,118,151 THRU 155,
                      301,302,304,306,461 THRU 466=6)
          (111,112,114 THRU 117,119,120,122,
           124 THRU 129,132,134,135,
           309,310,315,317,404,
                                       405,407,408=7)
          (211 THRU 215,231 THRU 234,251 THRU 255,
                         321 THRU 327,411 THRU 424=8)
                (223,341 THRU 343,345 THRU 360,450=9)
               (221,344,431 THRU 449,453 THRU 455=10)
          (331 THRU 334,336 THRU 338,371 THRU 397=11)
                     (303,305,307,308,314,316,318=12)
                                      (31 THRU 46=13)
                                             (313=14)
                                     (800 THRU HI=15)
                                               (0=16)
VALUE LABELS   COUNTY
                   1 'WORCESTER'
                   2 'FRANKLIN'
                   3 'MIDDLESEX'
                   4 'SUFFOLK'
                   5 'NORFOLK'
                   6 'BRISTOL'
                   7 'PLYMOUTH'
                   8 'ESSEX'
                   9 'HAMPSHIRE'
                  10 'HAMPDEN'
                  11 'BERKSHIRE'
                  12 'DUKES'
                  13 'BARNSTABLE'
                  14 'NANTUCKET'
                  15 'OUT OF STATE'
                  16 'UNKNOWN'
IF             ((H1 EQ 9)OR(H1 EQ 0 AND H2 LE 1))H2=12
IF             (H1 EQ 0 AND H2 EQ 2)H2=9
IF             (H1 EQ 0 AND H2 EQ 3)H2=11
IF             (H1 EQ 0 AND H2 EQ 4)H2=10
IF             (H1 EQ 8 AND H2 EQ 0)H2=8
VAR LABELS     H2 'OCCUPATION'
VALUE LABELS   H2
                   1 'PROFESSIONAL'
                   2 'SEMI-PROFESSIONAL'
                   3 'BUSINESS'
                   4 'SALES,CLERICAL'
                   5 'MANUAL'
                   6 'SERVICES'
                   7 'AGRICULTURE'
                   8 'ARMED SERVICES'
                   9 'HOUSEKEEPER'
                  10 'STUDENT'
                  11 'UNEMPLOYED'
                  12 'UNKNOWN'
VAR LABELS      TMSP 'TIME AT MOST SKILLED POSITION '
               /TJLD 'TIME ON JOB OF LONGEST DURATION '
IF             (H2 EQ 11)TMSP=1
IF             (H2 EQ 11)TJLD=1
RECODE          TMSP,TJLD (0,-0=10)
VALUE LABELS    TMSP, TJLD
                   1 'LESS THAN 1 MO'
                   2 '1-2MO'
                   3 '3-4MO'
                   4 '5-6MO'
                   5 '7-9MO'
                   6 '10-12MO'
                   7 '1-2 YEARS'
                   8 '2-5 YEARS'
                   9 '5+ YEARS'
                  10 'UNKNOWN'
VAR LABELS      LGC 'LAST GRADE COMPLETED '
IF             (GED EQ 2)LGC=13
RECODE         LGC
                  (1,2=3)
                  (13=13)
                  (14,15=14)
                  (0,99,-0=16)
                  (16 THRU HIGHEST=15)
VALUE LABELS   LGC
                   3 '3RD OR LESS'
                   4 '4TH'
                   5 '5TH'
                   6 '6TH'
                   7 '7TH'
                   8 '8TH'
                   9 '9TH'
                  10 '10TH'
                  11 '11TH'
                  12 'HIGH SCHOOL GRAD'
                  13 'GED'
                  14 'SOME COLLEGE'
                  15 'COLLEGE GRAD'
                  16 'UNKNOWN'
VAR LABELS     DRUG 'KNOWN DRUG USE'
RECODE         DRUG
                 (0=6)
                 (1=1)
                 (2=2)
                 (3=3)
                 (4=5)
                 (5=4)
                 (9=6)
VALUE LABELS   DRUG
                   1 'NONE'
                   2 'NON-SPECIFIC'
                   3 'HEROIN'
                   4 'MARIJUANA'
                   5 'OTHER'
                   6 'UNKNOWN '
VAR LABELS     CAT 'TOTAL NUMBER OF COURT APPEARANCES'
RECODE         CAT(6 THRU 8=6)
                  (9 THRU 11=7)
                  (12 THRU 15=8)
                  (16 THRU 20=9)
                  (21 THRU HIGHEST=10)
                  (0,-0=11)
IF             (D1Y LE 1)CAT=11
VALUE LABELS    CAT 1 '1ST OFFENSE'
                    2 'TWO'
                    3 'THREE'
                    4 'FOUR'
                    5 'FIVE'
                    6 '6 TO 8'
                    7 '9 TO 11'
                    8 '12 TO 15'
                    9 '16 TO 20'
                   10 'MORE THAN 20'
                   11 'UNKNOWN'
VAR LABELS      CAP  'NUMBER OF CHARGES FOR PERSON OFFENSES'
               /CAPR 'NUMBER OF CHARGES FOR PROPERTY OFFENSES'
               /CAS  'NUMBER OF CHARGES FOR SEX OFFENSES'
               /CAN  'NUMBER OF CHARGES FOR NARCOTICS OFFENSES'
               /CAD  'NUMBER OF CHARGES FOR DRUNKENNESS OFFENSES'
               /CAE  'NUMBER OF CHARGES FOR ESCAPE OFFENSES'
RECODE          CAP TO CAE
                  (6 THRU 8=6)
                  (9 THRU HIGHEST=7)
IF             (D1Y LE 1)CAP=8
IF             (D1Y LE 1)CAPR=8
IF             (D1Y LE 1)CAS=8
IF             (D1Y LE 1)CAN=8
VALUE LABELS    CAP TO CAE
                    0 'NONE '
                    1 'ONE '
                    2 'TWO '
                    3 'THREE '
                    4 'FOUR '
                    5 'FIVE '
                    6 '6 TO 8'
                    7 'OVER 8'
                    8 'UNKNOWN'
IF             (D1Y LE 1)CAD=8
IF             (D1Y LE 1)CAE=8
COMPUTE        TINC=INH+INJ+INS
COMPUTE        TAINC=INH+INS
VAR LABELS     INJ  'DEPARTMENT OF YOUTH SERVICE COMMITMENT INDICATOR '
               /INH 'NUMBER PRIOR COUNTY INCARCERATIONS'
               /INS 'NUMBER PRIOR STATE OR FEDERAL INCARCERATIONS'
               /TINC 'TOTAL NUMBER ANY PRIOR INCARCERATIONS'
               /TAINC 'TOTAL NUMBER PRIOR ADULT INCARCERATIONS '
RECODE         INH TO INS,TINC,TAINC
                  (6 THRU HI=6)
IF             (D1Y LE 1)INH=7
IF             (D1Y LE 1)INS=7
IF             (D1Y LE 1)TAINC=7
VALUE LABELS   INH TO INS,TINC,TAINC
                     0 'NONE'
                     1 'ONE'
                     2 'TWO'
                     3 'THREE'
                     4 'FOUR'
                     5 'FIVE'
                     6 'SIX OR MORE'
                     7 'UNKNOWN'
RECODE         INJ(1 THRU HI=1)(0=0)
IF             (D1Y LE 1)INJ=-1
VALUE LABELS   INJ
                  -1 'UNKNOWN'
                   0 'NO'
                   1 'YES'
COMPUTE        PT=PJ+PA
COMPUTE        PVT=PVJ+PVA
IF             (PJ LT 0 )PJ=-2
IF             (PA LT 0 )PA=-2
IF             (PT EQ 0)PVT=-1
IF             (PJ EQ 0)PVJ=-1
IF             (PA EQ 0)PVA=-1
VAR LABELS     PJ   'NUMBER OF JUVENILE PAROLES'
               /PVJ 'NUMBER OF JUVENILE PAROLE VIOLATIONS'
               /PA  'NUMBER OF ADULT PAROLES'
               /PVA 'NUMBER OF ADULT PAROLE VIOLATIONS '
               /PT  'TOTAL NUMBER OF PAROLES'
               /PVT 'TOTAL NUMBER OF PAROLE VIOLATIONS '
RECODE         PJ TO PVA,PT,PVT
                     (4 THRU HI=4)
IF             (D1Y LE 1)PJ=5
IF             (D1Y LE 1)PVJ=5
IF             (D1Y LE 1)PA=5
IF             (D1Y LE 1)PVA=5
VALUE LABELS   PJ TO PVA,PT,PVT
                    -1 'NEVER PAROLED'
                     0 'NONE'
                     1 'ONE'
                     2 'TWO'
                     3 'THREE'
                     4 'FOUR OR MORE'
                     5 'UNKNOWN'
IF             (D1Y LE 1)PT=5
IF             (D1Y LE 1)PVT=5
COMPUTE        AGEINC=TRUNC((DCM-DOBM+(DCY-DOBY)*12)/12)
COMPUTE        UAGEINC=AGEINC
COMPUTE        AGEAR1=TRUNC((D1M-DOBM+(D1Y-DOBY)*12)/12)
COMPUTE        AGEADK=TRUNC((DKM-DOBM+(DKY-DOBY)*12)/12)
COMPUTE        AGEADG=TRUNC((DGM-DOBM+(DGY-DOBY)*12)/12)
IF             (DCY EQ 0 OR DOBY EQ 0)AGEINC=-1
IF             (D1Y EQ 0 OR DOBY EQ 0)AGEAR1=-1
IF             (D1Y LE 1)AGEAR1=-1
COMPUTE        UAGEAR1=AGEAR1
MISSING VALUES UAGEAR1(-1)
IF             (DOBY EQ 0)AGEADK=-0
IF             (DKY LE 0)AGEADK=0
IF             (DOBY EQ 0)AGEADG=-0
IF             (DGY LE 0)AGEADG=0
VAR LABELS      AGEINC 'AGE AT INCARCERATION'
               /AGEAR1 'AGE AT 1ST COURT APPEARANCE'
               /AGEADK 'AGE AT 1ST COURT APPEARANCE FOR DRUNK OFFENSE'
               /AGEADG 'AGE AT 1ST COURT APPEARANCE FOR DRUG OFFENSE '
RECODE       AGEINC,AGEAR1
                 (26 THRU 29=26)
                 (30 THRU 39=27)
                 (40 THRU 49=28)
                 (50 THRU HI=29)
                 (-1=30)
                 (5 THRU 10=10)
VALUE LABELS  AGEINC,AGEAR1
                     10 '10 OR LESS'
                     11 'ELEVEN'
                     12 'TWELVE'
                     13 'THIRTEEN'
                     14 'FOURTEEN'
                     15 'FIFTEEN'
                     16 'SIXTEEN'
                     17 'SEVENTEEN'
                     18 'EIGHTEEN'
                     19 'NINETEEN'
                     20 'TWENTY'
                     21 'TWENTY-ONE'
                     22 'TWENTY-TWO'
                     23 'TWENTY THREE'
                     24 'TWENTY-FOUR'
                     25 'TWENTY-FIVE'
                     26 '26 TO 29'
                     27 '30 TO 39'
                     28 '40 TO 49'
                     29 '50 AND OVER'
                     30 'UNKNOWN'
                     -2 'NOT SEN-A'
RECODE         AGEADK,AGEADG
                   (6 THRU 14=1)
                   (15 THRU 17=2)
                   (18,19=3)
                   (20,21=4)
                   (22 THRU 24=5)
                   (25 THRU 29=6)
                   (30 THRU 34=7)
                   (35 THRU 39=8)
                   (40 THRU HIGHEST=9)
                   (LO THRU -1=10)
                   (0=0)
IF             (D1Y LE 1)AGEADK=10
IF             (D1Y LE 1)AGEADG=10
VALUE LABELS   AGEADK,AGEADG
                   0 'NOT APPLICABLE'
                   1 '14 OR LESS'
                   2 '15 TO 17'
                   3 '18 TO 19'
                   4 '20 TO 21'
                   5 '22 TO 24'
                   6 '25 TO 29'
                   7 '30 TO 34'
                   8 '35 TO 39'
                   9 '40 AND OVER'
                  10 'UNKNOWN'
IF             (MINSENT EQ 888888)MAXSENT=888888
RECODE         MINSENT,MAXSENT
                (000000=18)
                (000001 THRU 000029=-1)
                (888888=16)
                (999999=17)
                (000030 THRU 001199=0)
                (001200 THRU 002399 = 1)
                (10000 THRU 19999=1)
                (20000 THRU 29999=2)
                (002400 THRU 003599=2)
                (30000 THRU 39999=3)
                 (003600 THRU 004799=3)
                (40000 THRU 49999=4)
                (004800 THRU 005999 = 4)
                (50000 THRU 59999=5)
                (006000 THRU 007199 = 5)
                (60000 THRU 69999=5)
                (007200 THRU 008399 = 6)
                (70000 THRU 79999=7)
                (008400 THRU 009599 = 7)
                (80000 THRU 89999=8)
                (009600 THRU 009999 = 8)
                (90000 THRU 99999=9)
                (100000 THRU 109999=10)
                (110000 THRU 129999=11)
                (130000 THRU 159999=12)
                (160000 THRU 199999=13)
                (200000 THRU 249999=14)
                (250000 THRU HI=15)
IF             (SENTYPE EQ 9)MINSENT=18
IF             (SENTYPE EQ 9)MAXSENT=-2
VALUE LABELS   MINSENT,MAXSENT
                   1 '1 YEAR'
                   2 '2 YEARS'
                   3 '3 YEARS'
                  -2 'FINE'
                   4 '4 YEARS'
                   5 '5 YEARS'
                   6 '6 YEARS'
                   7 '7 YEARS'
                   8 '8 YEARS'
                   9 '9 YEARS'
                  10 '10 YEARS'
                  11 '11 TO 12 YEARS'
                  12 '13 TO 15 YEARS'
                  13 '16 TO 19 YEARS'
                  14 '20 TO 24 YEARS'
                  15 '25 OR MORE YEARS'
                  16 'LIFE'
                  17 'DEATH'
                  18 'INDETERMINATE'
                  19 'UNKNOWN'
                   0 'LESS THAN 1 YR'
                  -1 'LESS THAN 1 MO'
COMMENT ***********************************************
               PRIMARY OFFENSE SECTION
        ***********************************************
COMPUTE  OFFP = OFF1
COMPUTE OFFS = OFF1
COMPUTE  OFFPR = OFF1
COMPUTE OFFD = OFF1
COMPUTE OFFO = OFF1
VAR LABELS     OFFP  'PERSON OFFENSES'
              /OFFS  'SEX OFFENSES'
              /OFFPR 'PROPERTY OFFENSES'
              /OFFD  'DRUG OFFENSES'
              /OFFO  'OTHER OFFENSES'
RECODE         OFFP
                  (155 = 150)
                  (160,163,171,172=160)
                  (159= 160)
                  (161,170,174=161)
                  (131=130)
                  (190=179)
                  (300 THRU HI = 0)
               /OFFS
                  (320,321=320)
                  (331=330)
                  (410=400)
                  (332,333=310)
                  (110 THRU 299, 500 THRU HI=0)
               /OFFPR
                  (561=560)
                  (110 THRU 499, 600 THRU HI=0)
               /OFFO
                  (615=610)
                  (725=720)
                  (625 = 720)
                  (950=910)
                  (110 THRU 599, 800 THRU 899=0)
               /OFFD
                  (801=800)
                  (805 = 815)
                  (830,831=821)
                  (835,836,837,838,854=822)
                  (839 = 822)
                  (840=823)
                  (850 THRU 853=824)
                  (855 THRU 857=822)
                  (860 THRU 862=821)
                  (110 THRU 799, 900 THRU HI = 0)
VALUE LABELS   OFFP
                     0 'NOT APPLICABLE'
                   110 'MURDER-1'
                   115 'MURDER-2'
                   120 'MANSLAUGHTER'
                   121 'MV HOMICIDE'
                   135 'ACC-MURDER'
                   195 'CIVIL RIGHTS'
                   130 'ASSLT-INTENT - MURDER'
                   140 'ARMED ROBBERY'
                   150 'UNARMED ROBBERY'
                   160 'ARMED ASSAULT'
                   161 'UNARMED ASSAULT'
                   180 'KIDNAPPING'
                   200 'CONSPIRACY'
                   201 'OTHER PERSON'
                   179 'EXTORTION'
                   173 'MAYHEM'
                   162 'PUTTING IN FEAR'
                   999 'UNKNOWN'
          /OFFS      0 'NOT APPLICABLE'
                   300 'RAPE'
                   310 'ASSAULT-RAPE'
                   320 'RAPE-MINOR'
                   305 'AGG RAPE'
                   332 'INDECENT A & B'
                   450 'LEWDNESS'
                   330 'ASSLT-RAPE-MINOR'
                   400 'UNNATURAL ACTS'
                   420 'SODOMY-BUGGARY'
                   430 'INCEST'
                   440 'GROSS SEXUAL MISCONDUCT'
                   999 'UNKNOWN'
        /OFFPR       0 'NOT APPLICABLE'
                   500 'ARSON'
                   510 'BURGLARY-ARMED'
                   511 'BURGLARY'
                   512 'TOOLS'
                   520 'STEALING'
                   521 'LARCENY-PERSON'
                   522 'LARCENY'
                   523 'VEHICLE THEFT'
                   530 'FORGERY-UTTERING'
                   524 'UNAUTH USE'
                   540 'COMMON THEFT'
                   560 'STOLEN GOODS'
                   570 'PROPERTY INJURIES'
                   550 'FRAUD'
                   999 'UNKNOWN'
         /OFFO       0 'NOT APPLICABLE'
                   600 'ESCAPES'
                   610 'WEAPONS OFFENSES'
                   640 'STUBBORN CHILD'
                   641 'NIGHTWALKER'
                   660 'DISTURBING THE PEACE'
                   670 'PROSTITUTION'
                   690 'ABORTION'
                   700 'GAMING'
                   710 'VEHICLE OFFENSE'
                   741 'OUI'
                   743 'UNLAW POSS ALCOHOL'
                   715 'LEAVE SCENE'
                   742 'MINOR-ALC'
                   910 'TRESPASS'
                   620 'NONSUPPORT'
                   650 'PIMPING'
                   900 'OTHER'
                   720 'CONTEMPT OF COURT'
                   730 'BRIBERY'
                   740 'DRUNKENESS'
                   750 'USURY'
                   790 'HABITUAL OFFENDER'
                   999 'UNKNOWN'
       /OFFD         0 'NOT APPLICABLE'
                   800 'POSSESSION-HEROIN'
                   802 'STEALING NARCOTICS'
                   803 'BEING NEAR NARCOTICS'
                   804 'SYRINGE'
                   810 'SALE OF HEROIN'
                   811 'SALE OF NARCOTIC'
                   812 'INTENT TO SELL'
                   813 'OUI-NARCOTICS'
                   815 'CONSPIRACY TO VIOLATE CS ACT'
                   820 'CONTROLLED SUBSTANCE-CLASS NOT SPECIFIED'
                   821 'CLASS-A'
                   822 'CLASS-B'
                   823 'CLASS-C'
                   824 'CLASS-D'
                   825 'CLASS-E'
                   872 'DRUGS WI SCHOOL ZONE'
                   999 'UNKNOWN'
VAR LABELS     OFF1 'PRESENT OFFENSE'
RECODE         OFF1
                    (110 THRU 200=1)
                    (300 THRU 499=2)
                    (500 THRU 570=3)
                    (600 THRU 799=5)
                    (900 THRU HI=5)
                    (800 THRU 899=4)
                    (0,-0=6)
VALUE LABELS   OFF1
                     1 'PERSON'
                     2 'SEX'
                     3 'PROPERTY'
                     4 'DRUG'
                     5 'OTHER'
                     6 'UNKNOWN'
COMMENT     THE FOLLOWING LINES ARE USED TO MAKE THE UNKNOWN
                CATEGORIES FOR THE OFFENSES
IF             (OFF1 EQ 6)OFFP=999
IF             (OFF1 EQ 6)OFFS=999
IF             (OFF1 EQ 6)OFFPR=999
IF             (OFF1 EQ 6)OFFO=999
IF             (OFF1 EQ 6)OFFD=999
COMPUTE        CITIZEN=0
IF             (POB LE 520 AND CIT EQ 800)
                     CITIZEN=1
IF             (POB GE 900 AND POB LE 997 AND CIT EQ 800)
                     CITIZEN=2
IF             (POB GT 800 AND POB LT 900 AND CIT EQ 800)
                     CITIZEN=4
IF             (POB GT 985 AND CIT EQ 800)
                     CITIZEN=4
IF             ((POB EQ 801 OR POB EQ 824 OR POB EQ 874)AND CIT EQ 800)
                     CITIZEN=3
IF             (POB LE 0 AND CIT EQ 800)
                     CITIZEN=5
IF             (CIT GT 800)
                     CITIZEN=6
IF             (POB LE 0 AND CIT LE 0)
                     CITIZEN=7
VALUE LABELS   CITIZEN
                     1 'US CITIZEN-MASS BORN'
                     2 'US CIT-OTHER STATE'
                     3 'US CIT - US TERR '
                     4 'US CIT - FOREIGN BORN '
                     5 'US CIT - UNKNOWN POB '
                     6 ' NON CITIZEN'
                     7 'UNKNOWN'
CROSSTABS      TABLES=ORGORG,JCRD,MINSENT,MAXSENT,OFF1,OFFP,
                      OFFS,OFFPR,OFFD,OFFO,PE,AGEINC BY CINST
OPTIONS        4
CROSSTABS      TABLES=SEX,RACE,MAR,MIL,ADR,COUNTY,SMSA,
               H2,TMSP,TJLD,LGC,DRUG BY CINST
OPTIONS        4
CROSSTABS      TABLES=CAT,CAP,CAPR,CAS,CAN,CAD,CAE,
                      INJ,INH,INS,TAINC,HISP,
                      PJ,PVJ,PA,PVA,PT,PVT,
                      AGEAR1,AGEADK,AGEADG,
                      SENTYPE BY CINST
OPTIONS        4
CROSSTABS      TABLES=DCM,CODEF,VICTIM,CITIZEN,ORGCT BY CINST
OPTIONS        4
FREQUENCIES    GENERAL=UAGEINC
OPTIONS        5
TEMPORARY
SELECT IF      (CINST EQ 'A')
FREQUENCIES    GENERAL=UAGEINC
OPTIONS        5
TEMPORARY
SELECT IF     (CINST EQ 'C')
FREQUENCIES    GENERAL=UAGEINC
OPTIONS        5
TEMPORARY
SELECT IF     (CINST EQ 'F')
FREQUENCIES    GENERAL=UAGEINC
OPTIONS        5
TEMPORARY
SELECT IF     (FRAM EQ 1)
FREQUENCIES   GENERAL=UMINSENT
TEMPORARY
SELECT IF     (MINSENT EQ 16)
FREQUENCIES   GENERAL = OFFP, OFFS, OFFPR, OFFD, OFFO
OPTIONS       5
CROSSTABS     TABLES=UMINSENT,UMAXSENT BY CINST
OPTIONS       4
FREQUENCIES   GENERAL=FRAM
OPTIONS       5
FINISH
