FILE HANDLE   DCOM75 / NAME='$USER.KLAUSNER.DCOM75' / LRECL = 80
DATA LIST     FILE = DCOM75 RECORDS=4
               / 1 SEQN 1-7 SEX 26
               / 2
                   OFF1 11-13
                / 3
                /4
SELECT IF (SEX EQ 2 AND
          (OFF1 EQ 110 OR OFF1 EQ 115 OR OFF1 EQ 120 OR
           OFF1 EQ 121 OR OFF1 EQ 130 OR OFF1 EQ 131 OR
           OFF1 EQ 135))
CROSSTABS TABLES = SEQN BY OFFF1
FINISH
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
FREQUENCIES      GENERAL=CINST TO PROB
FINISH
