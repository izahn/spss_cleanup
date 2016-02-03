TITLE  COMMITTMENTS ANNUAL REPORT PROGRAM  - TABLES
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
                  1 'Prison'
                  2 'Reformatory'
                  3 'County'
                  4 'Unknown'
VAR LABELS     FRAM 'TYPE OF SENTENCE'
VAR LABELS     DCM 'MONTH OF COMMITMENT'
VALUE LABELS   DCM 1 'January'
                   2 'February'
                   3 'March'
                   4 'April'
                   5 'May'
                   6 'June'
                   7 'July'
                   8 'August'
                   9 'September'
                  10 'October'
                  11 'November'
                  12 'December'
RECODE            SENTYPE (5=4)(6,7=5)(8=6)
VARIABLE LABELS SENTYPE 'SENTENCE TYPE'
VALUE LABELS   SENTYPE
    0 'Unknown'
    1 'Simple Sentence'
    2 'Concurrent Sentence'
    3 'Aggregate Sentence'
    4 'Forthwith Sentence'
    5 'From & After Sentence'
    6 'Split Sentence'
    9 'Fine'
VAR LABELS OFFCNT 'NUMBER OF CHARGES: CURRENT OFFENSE'
RECODE OFFCNT (5 THRU 9 = 5)
              (10 THRU 19 = 10)
              (20 THRU HI = 20)
VALUE LABELS OFFCNT 1 'One'
                    2 'Two'
                    3 'Three'
                          4 'Four'
                    5 'Five to Nine'
                   10 'Ten to Nineteen'
                   20 'Twenty or More'
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
               1 'Barnstable Superior'
               2 'Berkshire Superior'
               3 'Bristol Superior'
               4 'Dukes Superior'
               5 'Essex Superior'
               6 'Franklin Superior'
               7 'Hampden Superior'
               8 'Hampshire Superior'
               9 'Middlesex Superior'
              10 'Nantucket Superior'
              11 'Norfolk Superior'
              12 'Plymouth Superior'
              13 'Suffolk Superior'
              14 'Worcester Superior'
              15 'Municipal Courts'
              16 'District Courts '
              17 'Out of State' 18 'Unknown'
VALUE LABELS          ORGCT
               1 'Barnstable County'
               2 'Berkshire County'
               3 'Bristol County'
               4 'Dukes County'
               5 'Essex County'
               6 'Franklin County'
               7 'Hampden County'
               8 'Hampshire County'
               9 'Middlesex County'
              10 'Nantucket County'
              11 'Norfolk County'
              12 'Plymouth County'
              13 'Suffolk County'
              14 'Worcester County'
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
VAR LABELS PE 'TIME UNTIL ORIGINAL PAROLE ELIGIBILITY'
VALUE LABELS   PE
                 0 'No PE Date'
                 1 'Past PE Date'
                 2 '1 - 3 Months'
                 3 '4 - 6 Months'
                 4 '7 - 9 Months'
                 5 '10 - 12 Months'
                 6 '13 - 18 Months'
                 7 '19 - 24 Months'
                 8 '2 - 3 Years'
                 9 '3 - 5 Years'
                10 '5 - 10 Years'
                11 '10 Years or More'
                13 'Life'
                14 'Death'
                15 'Unknown'
VALUE LABELS   SEX
                   'M' 'Male'
                   'F' 'Female'
IF             (ORIGSRC EQ 18 OR ORIGSRC EQ 18 OR ORIGSRC EQ 19)
                   CINST = 'O'
VAR LABELS     CINST 'COMMITTING INSTITUTION '
RECODE         CINST('W'='A')
VALUE LABELS   CINST
                  'A' 'Cedar Junction'
                  'C' 'Concord'
                  'F' 'Framingham'
                  'L' 'Longwood'
                  'O' 'Other Jurisdiction '
VAR LABELS     JCRD 'JAIL CREDIT DAYS'
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
                   0 'None'
                   1 '1 - 10'
                   2 '11 - 50'
                   3 '51 - 100'
                   4 '101 - 150'
                   5 '151 - 200'
                   6 'Over 200'
                   7 'Unknown'
VAR LABELS  RACE 'RACE/ETHNICITY'
RECODE         RACE
                   (5,6,7=5)
                   (0=6)
VALUE LABELS   RACE
                  1 'White'
                  2 'African American'
                  3 'Native American'
                  4 'Asiatic'
                  5 'Hispanic'
                  6 'Unknown'
VAR LABELS      MAR 'MARITAL STATUS'
RECODE          MAR (-0,0,9=7)
VALUE LABELS    MAR
                   1 'Married'
                   2 'Single'
                   3 'Divorced'
                   4 'Widowed'
                   5 'Common law'
                   6 'Separated'
                   7 'Unknown'
VAR LABELS     MIL 'MILITARY DISCHARGE'
RECODE         MIL(-0,0,9=7)
VALUE LABELS   MIL
                  1 'No Military Service'
                  2 'Honorable Discharge'
                  3 'Dishonorable Discharge'
                  4 'Bad Conduct Discharge'
                  5 'Medical Discharge'
                  6 'Discharge Unknown'
                  7 'Unknown'
COMPUTE        COUNTY=ADR
COMPUTE        UADR=ADR
COMPUTE        SMSA=ADR
VAR LABELS      ADR    'PRIOR ADDRESS: SELECTED CITIES/TOWNS'
               /COUNTY 'PRIOR ADDRESS: COUNTY'
               /SMSA   'PRIOR ADDRESS: MSA '
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
          1 'Boston'
          2 'Brockton'
          3 'Cambridge'
          4 'Fall River'
          5 'Framingham'
          6 'Holyoke'
          7 'Lawrence'
          8 'Lowell'
          9 'Lynn'
         10 'New Bedford'
         11 'Quincy'
         12 'Somerville'
         13 'Springfield '
         14 'Worcester'
         15 'Other Mass'
         16 'Out of State'
         17 'Unknown'
   /SMSA
          1 'Boston'
          2 'Brockton'
          3 'Fall River'
          4 'Fitchburg-Leominster'
          5 'Lawrence-Haverhill'
          6 'Lowell'
          7 'New Bedford'
          8 'Pittsfield'
          9 'Providence-Pawtucket-Warwick'
         10 'Springfield'
         11 'Worcester'
         12 'Other Mass'
         13 'Out of State'
         14 'Unknown'
RECODE   COUNTY
          (1 THRU 4,7 THRU 10,161,162,
           166,167,169 THRU 173,175,
           177,179,180,263,264,269,272,
           281,283,286,289,290,292,293,294,452,
                        481 THRU 488,501 THRU 520=14)
                         (5,6,12,181 THRU 204,335=6)
          (61 THRU 91,93,101,163 THRU 165,168,
           174,176,178,241 THRU 249,
           261,262,265 THRU 268,
           270,273,274,285,287,312,354,
                     471 THRU 479,491 THRU 497,626=9)
                                  (51,141 THRU 143=13)
          (22,27,52,92,94,113,123,
           130,133,282,284,288,
                 361 THRU 370,401 THRU 403,406,409=11)
                                               (26=11)
          (21,23 THRU 25,118,151 THRU 155,
                      301,302,304,306,461 THRU 466=3)
          (111,112,114 THRU 117,119,120,122,
           124 THRU 129,132,134,135,
           309,310,315,317,404,
                                       405,407,408=12)
          (211 THRU 215,231 THRU 234,251 THRU 255,
                         321 THRU 327,411 THRU 424=5)
                (223,341 THRU 343,345 THRU 360,450=8)
               (221,344,431 THRU 449,453 THRU 455=7)
          (331 THRU 334,336 THRU 338,371 THRU 397=2)
                     (303,305,307,308,314,316,318=4)
                                      (31 THRU 46=1)
                                             (313=10)
                                     (800 THRU HI=15)
                                               (0=16)
VALUE LABELS   COUNTY
                  14 'Worcester'
                   6 'Franklin'
                   9 'Middlesex'
                  13 'Suffolk'
                  11 'Norfolk'
                   3 'Bristol'
                  12 'Plymouth'
                   5 'Essex'
                   8 'Hampshire'
                   7 'Hampden'
                   2 'Berkshire'
                   4 'Dukes'
                   1 'Barnstable'
                  10 'Nantucket'
                  15 'Out of State'
                  16 'Unknown'
IF             ((H1 EQ 9)OR(H1 EQ 0 AND H2 LE 1))H2=12
IF             (H1 EQ 0 AND H2 EQ 2)H2=9
IF             (H1 EQ 0 AND H2 EQ 3)H2=11
IF             (H1 EQ 0 AND H2 EQ 4)H2=10
IF             (H1 EQ 8 AND H2 EQ 0)H2=8
VAR LABELS     H2 'OCCUPATION'
VALUE LABELS   H2
                   1 'Professional'
                   2 'Semi-Professional'
                   3 'Business'
                   4 'Sales, Clerical'
                   5 'Manual'
                   6 'Services'
                   7 'Agriculture'
                   8 'Armed Services'
                   9 'Housekeeper'
                  10 'Student'
                  11 'Unemployed'
                  12 'Unknown'
VAR LABELS      TMSP 'TIME AT MOST SKILLED POSITION '
               /TJLD 'TIME ON JOB OF LONGEST DURATION '
IF             (H2 EQ 11)TMSP=1
IF             (H2 EQ 11)TJLD=1
RECODE          TMSP,TJLD (0,-0=10)
VALUE LABELS    TMSP, TJLD
                   1 'Less Than 1 Month'
                   2 '1 - 2 Months'
                   3 '3 - 4 Months'
                   4 '5 - 6 Months'
                   5 '7 - 9 Months'
                   6 '10 - 12 Months'
                   7 '1 - 2 Years'
                   8 '2 - 5 Years'
                   9 '5 Years or More'
                  10 'Unknown'
VAR LABELS      LGC 'LAST GRADE COMPLETED '
IF             (GED EQ 2)LGC=13
RECODE         LGC
                  (1,2=3)
                  (13=13)
                  (14,15=14)
                  (0,99,-0=16)
                  (16 THRU HIGHEST=15)
VALUE LABELS   LGC
                   3 '3rd or Less'
                   4 '4th'
                   5 '5th'
                   6 '6th'
                   7 '7th'
                   8 '8th'
                   9 '9th'
                  10 '10th'
                  11 '11th'
                  12 'High School Graduate'
                  13 'GED'
                  14 'Some College'
                  15 'College Graduate'
                  16 'Unknown'
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
                   1 'None'
                   2 'Non-Specific'
                   3 'Heroin'
                   4 'Marijuana'
                   5 'Other'
                   6 'Unknown '
VAR LABELS     CAT 'TOTAL NUMBER OF COURT APPEARANCES'
RECODE         CAT(6 THRU 8=6)
                  (9 THRU 11=7)
                  (12 THRU 15=8)
                  (16 THRU 20=9)
                  (21 THRU HIGHEST=10)
                  (0,-0=11)
IF             (D1Y LE 1)CAT=11
VALUE LABELS    CAT 1 '1'
                    2 '2'
                    3 '3'
                    4 '4'
                    5 '5'
                    6 '6 - 8'
                    7 '9 - 11'
                    8 '12 - 15'
                    9 '16 - 20'
                   10 '21 Or More'
                   11 'Unknown'
VAR LABELS      CAP  'NUMBER OF CHARGES FOR PERSON OFFENSES'
               /CAPR 'NUMBER OF CHARGES FOR PROPERTY OFFENSES'
               /CAS  'NUMBER OF CHARGES FOR SEX OFFENSES'
               /CAN  'NUMBER OF CHARGES FOR DRUG OFFENSES'
               /CAD  'NUMBER OF CHARGES FOR ALCOHOL OFFENSES'
               /CAE  'NUMBER OF CHARGES FOR ESCAPE OFFENSES'
               /PROBNTS, 'NUMBER OF PRIOR PROBATIONS'
               /DEFAULTS, 'NUMBER OF PRIOR DEFAULTS'
RECODE          CAP TO CAE,PROBNTS, DEFAULTS,
                  (6 THRU 8=6)
                  (9 THRU HIGHEST=7)
IF             (D1Y LE 1)CAP=8
IF             (D1Y LE 1)CAPR=8
IF             (D1Y LE 1)CAS=8
IF             (D1Y LE 1)CAN=8
IF             (D1Y LE 1) PROBNTS = 8
IF             (D1Y LE 1) DEFAULTS = 8
VALUE LABELS    CAP TO CAE,PROBNTS, DEFAULTS
                    0 'None '
                    1 '1'
                    2 '2'
                    3 '3'
                    4 '4'
                    5 '5'
                    6 '6 - 8'
                    7 '9 or More'
                    8 'Unknown'
IF             (D1Y LE 1)CAD=8
IF             (D1Y LE 1)CAE=8
COMPUTE        TINC=INH+INJ+INS
COMPUTE        TAINC=INH+INS
VAR LABELS     INJ  'DEPARTMENT OF YOUTH SERVICE COMMITMENT INDICATOR '
               /INH 'PRIOR HOUSE OF CORRECTION INCARCERATIONS'
               /INS 'PRIOR STATE OR FEDERAL INCARCERATIONS'
               /TINC 'TOTAL NUMBER ANY PRIOR INCARCERATIONS'
               /TAINC 'PRIOR ADULT INCARCERATIONS '
RECODE         INH TO INS,TINC,TAINC
                  (6 THRU HI=6)
IF             (D1Y LE 1)INH=7
IF             (D1Y LE 1)INS=7
IF             (D1Y LE 1)TAINC=7
VALUE LABELS   INH TO INS,TINC,TAINC
                     0 'None'
                     1 '1'
                     2 '2'
                     3 '3'
                     4 '4'
                     5 '5'
                     6 '6 or More'
                     7 'Unknown'
RECODE         INJ(1 THRU HI=1)(0=0)
IF             (D1Y LE 1)INJ=-1
VALUE LABELS   INJ
                  -1 'Unknown'
                   0 'No'
                   1 'Yes'
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
                    -1 'Never Paroled'
                     0 'None'
                     1 '1'
                     2 '2'
                     3 '3'
                     4 '4 or More'
                     5 'Unknown'
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
      /AGEAR1 'AGE AT FIRST COURT APPEARANCE'
       /AGEADK 'AGE AT FIRST COURT APPEARANCE FOR ALCOHOL OFFENSE'
       /AGEADG 'AGE AT FIRST COURT APPEARANCE FOR DRUG OFFENSE '
RECODE       AGEINC
                 (25 THRU 29=25)
                 (30 THRU 34=30)
                  (35 THRU 39=35)
                 (40 THRU 44=40)
                 (45 THRU 49=45)
                 (50 THRU 54=50)
                 (55 THRU 59=55)
                 (60 THRU HI=60)
                 (-1=30)
                 (5 THRU 10=10)
VALUE LABELS  AGEINC
                     15 '15'
                     16 '16'
                     17 '17'
                     18 '18'
                     19 '19'
                     20 '20'
                     21 '21'
                     22 '22'
                     23 '23'
                     24 '24'
                     25 '25 to 29'
                     30 '30 to 34'
                     35 '35 to 39'
                     40 '40 to 44'
                     45 '45 to 49'
                     50 '50 to 54'
                     55 '55 to 59'
                     60 '60 and Older'
                     -2 'not Sen-A'
RECODE         AGEADK,AGEADG,agear1
                   (6 thru 19 = 19)
                   (20 thru 24 = 20)
                   (25 THRU 29= 25)
                   (30 THRU 34= 30)
                   (35 THRU 39= 35)
                   (40 THRU 44 = 40)
                   (45 thru 49 = 45)
                   (50 thru 54 = 50)
                   (55 thru 59 = 55)
                   (60 thru hi = 60)
                   (LO THRU -1=99)
                   (0=0)
IF             (D1Y LE 1)AGEADK=99
IF             (D1Y LE 1)AGEADG=99
VALUE LABELS   AGEADK,AGEADG,agear1
                   0 'Not Applicable'
                  19 '19 or Younger'
                  20 '20 to 24'
                  25 '25 to 29'
                  30 '30 to 34'
                  35 '35 to 39'
                  40 '40 to 44'
                  45 '45 to 49'
                  50 '50 to 54'
                  55 '55 to 59'
                  60 '60 and Older'
                  99 'Unknown'
IF             (MINSENT EQ 888888)MAXSENT=888888
RECODE         MINSENT,MAXSENT
                (000000=18)
                (000001 THRU 000029=-1)
                (888888=16)
                (999999=17)
                (000030 THRU 001199=0)
                (10000 THRU 19999=1)
                (1200 thru 2399 =1)
                (20000 THRU 29999=2)
                (2400 thru 3599 = 2)
                (30000 THRU 39999=3)
                (3600 thru 4799 = 3)
                (40000 THRU 49999=4)
                (4800 thru 5999 = 4)
                (50000 THRU 59999=5)
                (6000 thru 7199 = 5)
                (60000 THRU 69999=6)
                (7200 thru 8399 = 6)
                (70000 THRU 79999=7)
                (8400 thru 9599 = 7)
                (80000 THRU 89999=8)
                (9600 thru 9999 = 8)
                (90000 THRU 99999=9)
                (100000 THRU 109999=10)
                (110000 THRU 129999=11)
                (130000 THRU 159999=12)
                (160000 THRU 199999=13)
                (200000 THRU 249999=14)
                (250000 THRU HI=15)
IF             (SENTYPE EQ 9)MINSENT=18
IF             (SENTYPE EQ 9)MAXSENT=-2
VARIABLE LABELS MINSENT 'MINIMUM SENTENCE'
                MAXSENT 'MAXIMUM SENTENCE'
VALUE LABELS   MINSENT, MAXSENT
                   1 '1 Year'
                   2 '2 Years'
                   3 '3 Years'
                  -2 'Fine'
                   4 '4 Years'
                   5 '5 Years'
                   6 '6 Years'
                   7 '7 Years'
                   8 '8 Years'
                   9 '9 Years'
                  10 '10 Years'
                  11 '11 - 12 Years'
                  12 '13 - 15 Years'
                  13 '16 - 19 Years'
                  14 '20 - 24 Years'
                  15 '25 Years or More'
                  16 'Life'
                  17 'Death'
                  18 'Indeterminate'
                  19 'Unknown'
                   0 'Less Than 1 Year'
                  -1 'Less Than 1 Month'
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
COMPUTE UOFFD = OFFD
RECODE UOFFD (LO THRU 799,900 THRU HI = 0)
RECODE         OFFP
                  (155=150)
                  (160,163,171,172=160)
                  (159=160)
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
                  (950=910)
                  (110 THRU 599, 800 THRU 899=0)
               /OFFD
                  (801=800)
                  (830,831=821)
                  (835,836,837,838,854=822)
                   (839=822)
                  (840=823)
                  (850 THRU 853=824)
                  (855 THRU 858=822)
                  (860 THRU 862=821)
                  (110 THRU 799, 900 THRU HI = 0)
var labels   uoffd 'DRUG OFFENSES'
recode       uoffd (830=828)(835=829)(837=873)(858=874)
VALUE LABELS UOFFD
                     0 'Not Applicable'
                   800 'Poss of Narcotic Drug'
                   801 'Poss of Heroin'
                   802 'Stealing Narcotic Drug'
                   803 'Presence of Narcotic'
                   804 'Poss of Syringe'
                   813 'OUI Narcotics'
                   815 'Conspiracy to Violate CSA'
                   820 'Poss wi Dist No Class'
                   821 'Poss wi Dist Class A'
                   822 'Poss wi Dist Class B'
                   823 'Poss wi Dist Class C'
                   824 'Poss wi Dist Class D'
                   828 'Class A First Offense'
                   829 'Class B First Offense'
                   831 '* Class A Repeat'
                   836 '* Class B Repeat'
                   838 '* Class B PCP/Cocaine'
                   839 '* Class B PCP/Cocaine Repeat'
                   851 '* Marijuana 100-1,999 lbs.'
                   854 '* Cocaine 14-27 grams'
                   855 '* Cocaine 28-99 grams'
                   856 '* Cocaine 100-199 grams'
                   857 '* Cocaine 200 grams or More'
                   860 '* Heroin 28-99 grams'
                   861 '* Heroin 100-199 grams'
                   872 '* Poss wi School Zone'
                   873 '* Class B to Minor'
                   874 '* Cocaine to Minor'
VALUE LABELS   OFFP
                     0 'Not Applicable'
                   110 'Murder-1'
                   115 'Murder-2'
                   120 'Manslaughter'
                   121 'Vehicular Homicide'
                   135 'Accessory to Murder'
                   195 'Civil Rights'
                   130 'Assault w.i. Murder'
                   140 'Armed Robbery'
                   150 'Unarmed Robbery'
                   160 'Armed Assault'
                   161 'Unarmed Assault'
                   180 'Kidnapping'
                   200 'Conspiracy'
                   201 'Other Person'
                   179 'Extortion'
                   173 'Mayhem'
                   162 'Putting in Fear'
                   999 'Unknown'
          /OFFS      0 'Not Applicable'
                   300 'Rape'
                   310 'Assault w.i. Rape'
                   320 'Rape of a Minor'
                   305 'Aggravated Rape'
                   332 'Indecent A & B'
                   450 'Pornography'
                   330 'Assault w.i. Rape Minor'
                   400 'Unnatural Acts'
                   420 'Sodomy-Buggary'
                   430 'Incest'
                   440 'Other Sex Offenses'
                   999 'Unknown'
        /OFFPR       0 'Not Applicable'
                   500 'Arson'
                   510 'Burglary-Armed'
                   511 'Burglary'
                   512 'Burglary Tools'
                   520 'Stealing'
                   521 'Larceny from Person'
                   522 'Larceny'
                   523 'Motor Vehicle Theft'
                   530 'Forgery-Uttering'
                   524 'Unauthorized Use M.V.'
                   540 'Common Theft'
                   560 'Recv. Stolen Goods'
                   570 'Property Injuries'
                   550 'Fraud'
                   999 'Unknown'
         /OFFO       0 'Not Applicable'
                   600 'Escape'
                   610 'Weapons Offense'
                   640 'Stubborn Child'
                   641 'Common Nightwalker'
                   660 'Disturbing the Peace'
                   670 'Prostitution'
                   690 'Abortion'
                   700 'Gaming'
                   710 'Motor Vehicle'
                   741 'OUI'
                   743 'Unlawful Poss of Alcohol'
                   715 'Leaving the Scene'
                   742 'Minor in Possession of Alcohol'
                   790 'Habitual Offender'
                   910 'Trespassing'
                   620 'Nonsupport'
                   650 'Pimping'
                   900 'Other'
                   720 'Contempt of Court'
                   730 'Bribery'
                   740 'Drunkeness'
                   750 'Usury'
                   999 'Unknown'
       /OFFD         0 'Not Applicable'
                   800 'Possession of Heroin'
                   802 'Stealing Narcotics'
                   803 'Being Near Narcotics'
                   804 'Poss. of Syringe'
                   810 'Sale of Heroin'
                   811 'Sale of Narcotics'
                   812 'Intent to Sell'
                   813 'OUI-Narcotics'
                   815 'Conspiracy to Violate C.S.A.'
                   820 'Violate C.S.A.'
                   821 'Class-A'
                   822 'Class-B'
                   823 'Class-C'
                   824 'Class-D'
                   825 'Class-E'
                   872 'C.S.A. w.i. School Zone'
                   999 'Unknown'
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
                     1 'Person'
                     2 'Sex'
                     3 'Property'
                     4 'Drug'
                     5 'Other'
                     6 'Unknown'
COMMENT     THE FOLLOWING LINES ARE USED TO MAKE THE Unknown
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
VAR LABELS CITIZEN 'CITIZENSHIP'
VALUE LABELS   CITIZEN
                     1 'US Citizen-Born in Mass.'
                     2 'US Citizen- Born in Other State'
                     3 'US Citizen- Born in US Territory'
                     4 'US Citizen- Foreign Born'
                     5 'US Citizen- Unknown Place of Birth'
                     6 'Not a US Citizen'
                     7 'Unknown'

TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=DCM + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='MONTH OF COMMITMENT'
  /TABLE=ORGORG + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='COURT FROM WHICH COMMITTED'
  /TABLE=ORGCT + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='COUNTY OF COURT FROM WHICH COMMITTED'
  /TABLE=JCRD + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='JAIL CREDIT DAYS'
  /TABLE=MINSENT + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='MINIMUM SENTENCE'
  /TABLE=MAXSENT + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='MAXIMUM SENTENCE'
  /TABLE=SENTYPE + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='SENTENCE TYPE'
  /TABLE=OFF1 + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRESENT OFFENSE: GENERAL CATEGORIES'
  /TABLE=OFFP + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRESENT OFFENSE: PERSON OFFENSES'
  /TABLE=OFFS + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRESENT OFFENSE: SEX OFFENSES'
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=OFFPR + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRESENT OFFENSE: PROPERTY OFFENSES'
TABLES FORMAT=ZERO CWIDTH(30,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=UOFFD  + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRESENT OFFENSE: DRUG OFFENSES'
  /TFOOTNOTE='* Mandatory Term of Incarceration'
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=OFFO + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRESENT OFFENSE: OTHER OFFENSES'
  /TABLE=PE + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='TIME UNTIL ORIGINAL PAROLE ELIGIBILITY'
  /TABLE=AGEINC + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='AGE AT INCARCERATION'
  /TABLE=SEX + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='SEX'
  /TABLE=RACE + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='RACE/ETHNICITY'
  /TABLE=CITIZEN + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='CITIZENSHIP'
  /TABLE=MAR + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='MARITAL STATUS'
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=MIL + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='MILITARY DISCHARGE'
  /TABLE=ADR + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRIOR ADDRESS: SELECTED CITIES/TOWNS'
  /TABLE=COUNTY + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRIOR ADDRESS: COUNTY'
  /TABLE=SMSA + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRIOR ADDRESS: MSA'
  /TABLE=H2 + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='OCCUPATION'
  /TABLE=LGC + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='LAST GRADE COMPLETED'
  /TABLE=DRUG + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='KNOWN DRUG USE'
  /TABLE=CAT + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='TOTAL NUMBER OF COURT APPEARANCES'
  /TABLE=CAP + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF CHARGES FOR PERSON OFFENSES'
  /TABLE=CAPR + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF CHARGES FOR PROPERTY OFFENSES'
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=CAS + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF CHARGES FOR SEX OFFENSES'
  /TABLE=CAN + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF CHARGES FOR DRUG OFFENSES'
  /TABLE=CAD + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF CHARGES FOR ALCOHOL OFFENSES'
  /TABLE=CAE + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF CHARGES FOR ESCAPE OFFENSES'
  /TABLE=INH + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRIOR HOUSE OF CORRECTION INCARCERATIONS'
  /TABLE=INS + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRIOR STATE OR FEDERAL INCARCERATIONS'
  /TABLE=TAINC + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='PRIOR ADULT INCARCERATIONS'
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=AGEAR1 + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='AGE AT FIRST COURT APPEARANCE'
  /TABLE=AGEADK + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='AGE AT FIRST COURT APPEARANCE FOR ALCOHOL OFFENSE'
  /TABLE=AGEADG + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='AGE AT FIRST COURT APPEARANCE FOR DRUG OFFENSE'
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=PROBNTS + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF PRIOR PROBATIONS'
  /TABLE=DEFAULTS + T1 BY CINST + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':CINST)
  /TTITLE='NUMBER OF PRIOR DEFAULTS'
  /table = offcnt + t1 by cinst + t1
  /statistics county (' N') cpct ((paren5.0)' %':cinst)
  /ttitle ='NUMBER OF CHARGES: CURRENT OFFENSE'
temporary
select if (cinst eq 'A')
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=MINSENT + T1 BY OFF1 + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':OFF1)
  /TTITLE='MINIMUM SENTENCE BY PRESENT OFFENSE: CEDAR JUNCTION'
temporary
select if (cinst eq 'C')
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=MAXSENT + T1 BY OFF1 + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':OFF1)
  /TTITLE='MAXIMUM SENTENCE BY PRESENT OFFENSE: CONCORD'
temporary
select if (cinst eq 'F')
TABLES FORMAT=ZERO CWIDTH(20,5) NBOX NFRAME LENGTH(10)
  /FTOTAL=T1 'Total'
  /TABLE=FRAM   + T1 BY OFF1  + T1
  /STATISTICS COUNT(' N') CPCT((PAREN5.0)' %':OFF1)
  /TTITLE='TYPE OF SENTENCE BY PRESENT OFFENSE: FRAMINGHAM'
FINISH
