ITLE         COMMITTMENTS ANNUAL REPORT PROGRAM
ET BLANKS = 0
ILE HANDLE   DCOM90O / NAME='$USER.DATA.DCOM90_218' / LRECL = 218
ATA LIST     FILE = DCOM90O /
  SEQN 1-5 CINST 6 (A) SEX 7 (A)
   ODCM 8-9 ODCD 10-11 ODCY 12-13 DCM 14-15 DCD 16-17
   DCY 18-19
   TCOM 20-21 CODEF 22-26 VICTIM 27-31
   JCRD 32-36 OFF1 37-39 OFF2 40-42 OFF3 43-45 OFF4 46-48
   OFFCNT 49-53 SENTYPE 54 MINSENT 55-60 MAXSENT 61-66
   ORIGSRC 67-68 ORGORG 69-70 EFFM 71-72
   EFFD 73-74  EFFY 75-76
   SENTM 77-78 SENTD 79-80 SENTY 81-82 PEM 83-84 PED 85-86
   PEY 87-88 DOBM 89-90 DOBD 91-92 DOBY 93-94
   POB 95-97 CIT     98-100 ADR 101-103
   H1 104 H2 105 TMSP 106 TJLD 107
    RACE 108 MAR 109 MIL 110 GED 111 LGC 112-113 DRUG 114
   CONVCTS 115-119 PROBNTS 120-124 DEFAULTS 125-129
    D1M 130-131 D1D 132-133 D1Y 134-135
   DKM 136-137 DKD 138-139 DKY 140-141
   DGM 142-143 DGD 144-145 DGY 146-147
   CAT 148-152 CAP 153-157 CAPR 158-162
   CAS 163-167 CAN 168-172 CAD 173-177 CAE 178-182
   INJ 183-187 INH 188-192 INS 193-197
   PJ 198-202 PVJ 203-207 PA  208-212 PVA 213-217
   PROB 218
ILE HANDLE    DCOM90/NAME='$USER.DATA.DCOM90'/LRECL=80
RITE OUTFILE=DCOM90
   / 1 SEQN 1-5 CINST 6 (A) SEX 7 (A)
       ODCM 8-9 ODCD 10-11 ODCY 12-13
        DCM 14-15 DCD 16-17 DCY 18-19 TCOM 20-21
       ORIGSRC 22-23 ORGORG 24-25
   / 2 SEQN 1-5 JCRD 6-10
       OFF1 11-13 OFF2 14-16 OFF3 17-19 OFF4 20-22
       OFFCNT 23-27 SENTYPE 28 CODEF 29-33 VICTIM 34-38
      MINSENT 39-44 MAXSENT 45-50
   PEM 51-52 PED 53-54 PEY 55-56
   EFFM 57-58 EFFD 59-60 EFFY 61-62
   SENTM 63-64 SENTD 65-66 SENTY 67-68
      / 3 SEQN 1-5 DOBM 6-7 DOBD 8-9 DOBY 10-11
   POB 12-14 CIT 15-17 ADR 18-20
   H1 21 H2 22 TMSP 23 TJLD 24 RACE 25 MAR 26 MIL 27
   GED 28 LGC 29-30 DRUG 31
      / 4 SEQN 1-5 CONVCTS 6-10 PROBNTS 11-15 DEFAULTS 16-20
   D1M 21-22 D1D 23-24 D1Y 25-26 DKM 27-28 DKD 29-30
    DKY 31-32 DGM 33-34 DGD 35-36 DGY 37-38
  CAT 39-43 CAP 44-48 CAPR 49-53 CAS 54-58 CAN 59-63
   CAD 64-68 CAE 69-73
/ 5 SEQN 1-5 INJ 6-10 INH 11-15 INS 16-20
    PJ 21-25 PVJ 26-30 PA 31-35 PVA 36-40 PROB 41
XECUTE
INISH
