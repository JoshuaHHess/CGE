$TITLE ft collins ANALYSIS MODEL - ftCol3L(WRS:2-05)

*The standard model with Employment in actual workers & 3 Labor groups with SS fixed

*-------------------------------------------------------------------------------------------------------------
* 1.1 CONTROLS PLACED ON OUTPUT GENERATION
*-------------------------------------------------------------------------------------------------------------

$OFFSYMLIST OFFSYMXREF

*OPTIONS SYSOUT=OFF, SOLPRINT=OFF, LIMROW=0, LIMCOL=0;

*-------------------------------------------------------------------------------------------------------------
* 1.2 SET UP FILE FOR SOLUTION VALUES
*-------------------------------------------------------------------------------------------------------------

FILE RES /c:\cedar rapids\cofix1.RES/; RES.PW=250; RES.ND = 6; RES.LW = 20;
RES.NW=20;RES.LJ = 1; PUT RES;



*-------------------------------------------------------------------------------------------------------------
* 2. SET DEFINITION
*-------------------------------------------------------------------------------------------------------------
* 2.1 EXPLICIT SET DECLARATION
*-------------------------------------------------------------------------------------------------------------

SETS  Z  ALL ACCOUNTS IN SOCIAL ACCOUNTING MATRIX /
Utilities
Const
Manuf
WT
Retail
Other
HS1
L1
L2
L3
L4
L5
L6
L7
L8
L9
KAP
HH1
HH2
HH3
HH4
HH5
HH6
HH7
HH8
HH9
INVES
USSOCL1
USSOCL2
USSOCL3
USSOCL4
USSOCL5
USSOCL6
USSOCL7
USSOCL8
USSOCL9
USPIT
IAPIT
CNPRP
FEES
IASTX
LOCSTX
CYGF
FED
state
local
ROW   /


F(Z)      FACTORS                  / L1,L2,L3,L4,L5,L6,L7,L8,L9,KAP/


L(F)        LABOR                  /L1,L2,L3,L4,L5,L6,L7,L8,L9/

K(F)        CAPITAL                /KAP/

G(Z)      GOVERNMENTS              / USSOCL1,USSOCL2,USSOCL3,USSOCL4,USSOCL5,USSOCL6,USSOCL7
                                     USSOCL8,USSOCL9,USPIT,IAPIT,CNPRP,FEES,IASTX,LOCSTX,CYGF,FED,state,local/


GN(G)     ENDOGENOUS GOVERNMENTS   / FED,state,local /


GNL(G)   lOCAL  ENDOGENOUS GOVERNMENTS   / local/



GX(G)     EXOGENOUS GOVERMENTS     / USSOCL1,USSOCL2,USSOCL3,USSOCL4,USSOCL5,USSOCL6,USSOCL7
                                     USSOCL8,USSOCL9,USPIT,IAPIT,CNPRP,FEES,IASTX,LOCSTX/

GC(G)    LOCAL GOVNERMENT          /CNPRP,FEES,LOCSTX /


GS(G)     SALES OR EXCISE TAXES    / FEES,IASTX,LOCSTX /

GL(G)      LAND TAXES             / CNPRP/

GF(G)     FACTOR TAXES             / USSOCL1,USSOCL2,USSOCL3,USSOCL4,USSOCL5,USSOCL6,USSOCL7
                                     USSOCL8,USSOCL9, CNPRP/

GI(G)     INCOME TAX UNITS         / USPIT,IAPIT /

GH(G)     HOUSEHOLD TAX UNITS      / CNPRP,FEES /

GY(G)    EXOGNOUS TRANSFER PMT   / USSOCL1,USSOCL2,USSOCL3,USSOCL4,USSOCL5,USSOCL6,USSOCL7
                                     USSOCL8,USSOCL9,USPIT,IAPIT,CNPRP,FEES,IASTX,LOCSTX,FED,state /

GTA(G)    EXOGNOUS TRANSFER PMT   / USSOCL1,USSOCL2,USSOCL3,USSOCL4,USSOCL5,USSOCL6,USSOCL7
                                     USSOCL8,USSOCL9,USPIT,IAPIT,CNPRP,FEES,IASTX,LOCSTX,CYGF,FED,state/

GT(G)    ENDOGENOUS TRANSFER PMT   / CYGF, FED,state /

H(Z)      HOUSEHOLDS               / HH1,HH2,HH3,HH4,HH5,HH6,HH7,HH8,HH9/



IG(Z)      I+G SECTORS   / Utilities,Const,Manuf, WT,Retail, Other,hs1,FED,state,local/


I(IG)      INDUSTRY SECTORS         / Utilities,Const,Manuf, WT,Retail, Other,hs1/


IG2(IG)   ENDOGENOUS GOVERNMENTS   / FED,state,local /


IP(I)      PRODUCTION SECTORS      /Utilities,Const,Manuf, WT,Retail, Other,hs1/


FG(IG)    PRODUCTION GOV.         /FED,state,local/

SM         SIMMLOOP                 /BASE, TODAY,simm/

R1H       REPORT 1 FOR SCALARS     / GFREV, SFREV, PIT,
                                    DGF, DSF, DDRE, PDRE, SPI,COMM,COMMO,
                                    GN, NKI, HH, W, W1, W2, W3, R,RL, L, K, HN,HW, GFSAV, LD,
                                     HC,SSC, LAND, LAS /

R2H       REPORT 2 FOR STATUS      / M-STAT, S-STAT /

MS        LABELS FOR MODEL STATUS  / OPTIMAL, LOCALOP, UNBOUND,
                                        INFSBLE, INFSLOC, INFSINT,
                                        NOOPTML, MIPSOLN, NOINTGR,
                                        INFSMIP, UNUSED,  UNKNOWN,
                                        NOSOLUT /

SS        LABELS FOR SOLVER STATUS / OK, ITERATE, RESRCE,
                                        SOLVER,  EVALUATE,NOTKNWN,
                                        NOTUSED, PRE-PROC,SETUP,
                                        SLVFAIL, SLVINTER,POST-PROC,
                                        METSYS /

*-------------------------------------------------------------------------------------------------------------
* 2.2 ALIASES
*-------------------------------------------------------------------------------------------------------------

ALIAS (I,J), (I,I1), (Z,Z1), (F,F1), (G,G1), (G,G2), (GI,GI1), (GC, GC1), (GS,GS1),(GX,GX1), (GN,GN1),
 (GF,GF1), (H,H1), (IP,JP), (IG,JG),(GY,GY1), (GT,GT1), (GY, GY2),(GNL, GNL1) ;

*-------------------------------------------------------------------------------------------------------------
* 3. PARAMETERS AND EXOGENOUS VARIABLES
*-------------------------------------------------------------------------------------------------------------
* 3.1 SOCIAL ACCOUNTING MATRIX, CAPITAL COEFFICIENT MATRIX AND PARAMETERS
*----------------------------------------------------------------
TABLE SAM(Z,Z1) SOCIAL ACCOUNTING MATRIX
$ONDELIM
$INCLUDE C:\cedar rapids\samCR1.CSV
$OFFDELIM
$INCLUDE C:\cedar rapids\miscCR.prn
TABLE BB(I,IG) CAPITAL COMP
$ONDELIM
$INCLUDE C:\cedar rapids\CAPCOMCR.CSV
$OFFDELIM
$OFFDELIM
*was originaly capcomtest
*-------------------------------------------------------------------------------------------------------------
* 3.2 PARAMETER DECLARATION
*-------------------------------------------------------------------------------------------------------------


SCALARS
  DEPR          CALC    DEPRECIATION RATE FOR K
* ETAI                  INVESTMENT SUPPLY ELASTICITY        / 3.50 /
  ETAL2         CRCE    LAND SUPPLY ELASTICITY              / 3.0 /
* KSTOK
* COSTCHILD     PSD     EDU COST PER CHILD                 /.002150/;

PARAMETERS
* PARAMETERS CALCULATED FROM SOCIAL ACCOUNTING MATRIX AND TABLE DATA

A(Z,Z1)       IMPLAN  INPUT OUTPUT COEFFICIENTS
A3(H,L)
AD(Z,Z1)      IMPLAN  DOMESTIC INPUT OUTPUT COEFFICIENTS
AD1(z,z1)
AG(Z,G)       IMPLAN  GOVERNMENT SPENDING SHARES OF NET INCOME
AGFS(Z,G)
ALPHA(F,I)    IMPLAN  FACTOR SHARE EXPONENTS IN PRODUCTION FUNCTION
ALPHA1(F,I)
B(I,IG)
B1(I,J)
*CMOWAGE(L)
*CMIWAGE(L)
FCONST(F,I)
GAMMA(I)      CALC    PRODUCTION FUNCTION SCALE
DELTA(I)
PIT(G,H)
PIT0(G,H)     CALC    PERSONAL INCOME TAX SAM VALUE
PRIVRET1(H)
PRIVRET(H)
*LFOR(LA)               PROPORTION OF LAND INCOME    OUTFLOW
KFOR(K)                PROPORTION OF CAPITAL INCOME  OUTFLOW
GFOR(G)                PROPORTION OF GOVT INCOME OUTFLOW
out(G1,G1)
TAUF(G,F,Z)   CITY     FACTOR TAXES
TAUFH(G,F)    CITY     AGG FACTOR TAXES
TAUFL(G,L)    CITY    EMPLOYEE PORTION OF FACTOR TAXES
*TAUFLA(G,LA)  CITY    LAND FACTOR TAXES
TAUFK(G,K)    CITY    CAPITAL FACTOR TAXES
TAUFX(G,F,Z)
TAUH(G,H)     CITY    HOUSEHOLD TAXES OTHER THAN PIT
TAUH0(G,H)    CITY    HOUSEHOLD TAXES OTHER THAN PIT SAM VALUE
*TAUM(G,IG)    CITY    IMPORT DUTY RATES
TAUQ(G,IG)    CITY    AVERAGE SALES TAX RATES
TAUC(G,I)     CITY    EXPERIMENTAL CONSUMPTION SALES TAX RATES
*TAUCH(G,HD)   CITY    HOUSING CONSUMPTION SALES TAX RATES
TAUV(G,I)     CITY    EXPERIMENTAL CONSUMPTION SALES TAX RATES
TAUN(G,IG)    CITY    EXPERIMENTAL CONSUMPTION SALES TAX RATES
TAUX(G,IG)    CITY    EXPERIMENTAL CONSUMPTION SALES TAX RATES
TAUG(G,I)     CITY    EXPERIMENTAL CONSUMPTION SALES TAX RATES
TAXS(G,GX)    CITY    TAX DESTINATION SHARES
TAXS1(GNL)
TEST10(Z,Z)
TEST20(Z,Z)
TEST30(Z)

* ELASTICITIES AND TAX DATA IMPOSED

BETA(I,H)     CRCE    INCOME ELASTICITY OF DEMAND
*BETAH(HD,H)   CALC    INCOME ELASTICITY OF HOUSING DEMAND
ETAD(I)       CALC    DOMESTIC SHARE PRICE ELASTICITIES
ETAE(I)       CRCE    EXPORT ELASTICITIES WITH RESPECT TO DOMESTIC PRICE
ETAI(IG)      CRCE      LAND ELASTICITY
ETAIX(K,IG)   CRCE      LAND ELASTICITY
*ETAL(LA,IG)   CRCE      LAND ELASTICITY
ETAL1(IG)     CRCE      LABOR ELASTICITY
ETALB1(IG)    CRCE      LABOR ELASTICITY
ETALB(L,IG)   CRCE      LABOR ELASTICITY
ETAM(I)       CRCE    IMPORT ELASTICITIES WITH RESPECT TO DOMESTIC PRICE
ETARA(H)      CRCE    L SUPPLY ELASTICITY WITH RESPECT TO AVERAGE WAGE
ETAYD(H)      CRCE    RESPONSIVENESS OF INMIGRATION TO AFTER TAX EARNINGS
ETAU(H)       CRCE    RESPONSIVENESS OF INMIGRATION TO UNEMPLOYMENT
ETAPT(H)      CRCE    HOUSEHOLD RESPONSE TO TRANSFER PAYMENTS
ETAPIT(H)     STCO    L SUPPLY ELASTICITY WITH RESPECT TO TAXES
EXWGE1(L)     CALC    EXTERNAL WAGE commuting out
EXWGE2(L)     CALC    EXTERNAL WAGE commuting in
ECOMI(L)      CALC    ELASTICITY OF LABOR SUPPLY FOR IN COMMUTTERS
ECOMO(L)      CALC    ELASTICITY OF LABOR SUPPLY FOR OUT COMMUTTERS
*HOUSECOR(H,HD)        HOUSEHOLD HOUSING RELATIONSHIP
JOBCOR(H,L)   CALC    CORRECTION FACTOR BETWEEN HOUSEHOLDS AND JOBS
LAMBDA(I,J)   CRCE    CROSS PRICE ELASTICITIES
*LAMBDAH(HD,HD1)       HOUSING CROSS PRICE ELASTCITIES
NRPG(H)       CRCE    NATURAL RATE OF POPULATION GROWTH
RHO(I)        CRCE    EXPONENT IN PRODUCTION FUNCTION
TT(I)

* ARRAYS BUILT TO EXPORT RESULTS TO SEPARATE FILE

R1(R1H,SM)     REPORT  SCALAR VARIABLES
R2(R2H,SM)     REPORT  SOLVER AND MODEL STATUS VALUES

* INITIAL VALUES OF ENDOGENOUS VARIABLES

CG0(I,G)      CITY     REAL    GOVERNMENT CONSUMPTION
CG0T(I,G)
CH0(I,H)      IMPLAN  REAL    PRIVATE CONSUMPTION
CH0T(I,H)
* CHH0(HD,H)  IMPLAN  REAL    PRIVATE HOUSING CONSUMPTION
*CMI0(L)       CALC    REAL    NUMBER COMMUTING IN
*CMO0(L)       CALC    REAL    NUMBER COMMUTING OUT
CN0(I)        IMPLAN  REAL    INVESTMENT BY SECTOR OF SOURCE
CN0T(I)
CPI0(H)       CALC    PRICE   CONSUMER PRICE INDICES
CPIN0(H)      CALC    PRICE   NONHOUSING PRICE INDEX
CPIH0(H)      CALC    PRICE   HOUSING PRICE INDEX
CX0(I)        CALC    REAL    EXPORT CONSUMPTION
D0(I)         CALC    RATIO   DOMESTIC SUPPLY SHARE OF DOMESTIC DEMAND
DD0(Z)        CALC    REAL    DOMESTIC DEMAND
DS0(Z)        CALC    REAL    DOMESTIC SUPPLY QUANTITIES
DQ0(Z)
FD0(F,Z)      ES202   REAL    FACTOR DEMAND
IGT0(G,GX)    CITY    NOMINAL INTER GOVERNMENTAL TRANSFERS
KS0(K,IG)     CALC    REAL    CAPITAL STOCK
*LAS0(LA,IG)   ASSESSOR        LAND STOCK
HH0(H)        DOF     HHDS    NUMBER OF HOUSEHOLDS
HN0(H)        DOF     HHDS    NUMBER OF NONWORKING HOUSEHOLDS
HW0(H)        DOF     HHDS    NUMBER OF WORKING HOUSEHOLDS
LL(L)
M0(I)         IMPLAN  REAL    IMPORTS
M01(Z)        IMPLAN  REAL    IMPORTS
MI0(H)        CRCE    REAL    IN MIGRATION
MO0(H)        CRCE    REAL    OUT MIGRATION
N0(K,IG)      CALC    REAL    GROSS INVESTMENT BY SECTOR OF DESTINATION
NKI0          CALC    NOMINAL NET CAPITAL INFLOW
KPFOR01(K)
KPFOR0(K)     CALC    NOMINAL   CAPITAL OUTFLOW
*LNFOR0(LA)    CALC    NOMINAL   LAND OUTFLOW
*LNFOR01(LA)
GVFOR0(G)     CALC    NOMINAL   GOVT OUTFLOW
P0(IG)        CALC    PRICE   AGGREGATE PRICES
*PH0(HD)       CALC    PRICE   AGGREGATE HOUSING PRICES
PD0(I)        CALC    PRICE   DOMESTIC PRICES
PVA0(I)       CALC    PRICE   VALUE ADDED PRICES
PW0(I)        CALC    PRICE   EXOGENOUS PRICES IN EXTERNAL MARKETS
PWM0(I)       CALC    PRICE   IMPORT PRICE
Q0(Z)         CRCE    REAL    SOCIAL ACCOUNTING MATRIX COLUMN TOTALS
Q10(Z)        CRCE    REAL    SOCIAL ACCOUNTING MATRIX ROW TOTALS
R0(F,Z)       ES202   PRICE   INITIAL SECTORAL RENTAL RATE FOR FACTOR
RA0(F)        CALC            AVERAGE RENTAL RATES FOR FACTORS
S0(Z)         CRCE    NOMINAL SAVINGS
SPI0          CALC            PERSONAL INCOME (OBJ FUNC)
V0(I)         IMPLAN  REAL    INTERMEDIATE DEMAND
V0T(I)
TP(H,G)       SSAD    NOMINAL GOVERNMENT SOCIAL SECURITY PAYMENTS
TAUF0(G,F,Z)  CALC            SOCIAL SECURITY TAX
YD0(H)        CALC    NOMINAL AFTER TAX TOTAL HOUSEHOLD INCOMES
Y0(Z)         CALC    NOMINAL GROSS HOUSEHOLD INCOME
Y01(H)
YT0(G)        CALC            GOV INCOMES
GCP10(I)      CALC    REAL    GROSS CITY PRODUCT
GCP0
SD3(Z)
SD4(GX)
SD5(GX)
SD6(GX)
SD7(GX)       >
*SD7(L)          >
SD8(I)              >    TESTS
SD9             >
SD10
sd11          >
DDCX(I)
DYY(H,L)
dyy1(L)
dyy2(L)
dyy3(H,L)
DYK(H,K)
DYK1(K)
DYK2(K)
Rev(G,GX)
TR(GNL)
FD2(F,IG)
SSP(F,IG)
Tottax(GX,I)
*HHTAX(GH,H)
InterD(I,J) ;
*-------------------------------------------------------------------------------------------------------------
* 3.3 CALCULATIONS OF PARAMETERS AND INITIAL VALUES
*-------------------------------------------------------------------------------------------------------------

* ETAIX=ETAI;

* CALCULATE COLUMN AND ROW TOTALS OF SAM TO COMPARE FOR BALANCE

Q10(Z)=SUM(Z1,SAM(Z,Z1) );

Q0(Z)=SUM(Z1,SAM(Z1,Z) );

*SD3('coedu')=1/Q0('coedu');     display sd3;

DQ0(Z) = Q10(Z)-Q0(Z);

*SAM(Z,'ROW') = SAM(Z,'ROW')- DQ0(Z);


* SAM(Z,'ROW') = SAM(Z,'ROW')- DQ0(Z);

*Q10(Z)=SUM(Z1,SAM(Z,Z1) );

*Q0(Z)=SUM(Z1,SAM(Z1,Z) );

DQ0(Z) = Q10(Z)-Q0(Z);

B1(I,J)=  SAM(I,J);

*DISPLAY Q0, Q10, DQ0;

*TEST0(Z) = Q10(Z)-Q0(Z); DISPLAY TEST0;

* READ IN ELASTICITY PARAMETERS FROM MISC.PRN

out(G1,G1) = IOUT(G1,G1);

BETA(I,H)=MISC(I,'ETAY');

*      BETAH(HD,H)=MISC(HD,'ETAY');

DISPLAY BETA ;

LAMBDA(I,I)=MISC(I,'ETAOP');

*LAMBDAH(HD,HD)=MISC(HD,'ETAOP');

ETAE(I)=MISC(I,'ETAE');

ETAM(I)=MISC(I,'ETAM');

RHO(I)=( 1 - MISC(I,'SIGMA') ) / MISC(I,'SIGMA');

ETARA(H)=MISCH(H,'ETARA');

ETAPIT(H)=MISCH(H,'ETAPIT');

ETAPT(H)=MISCH(H,'ETAPT');

ETAYD(H)=MISCH(H,'ETAYD');

NRPG(H)=MISCH(H,'NRPG');

ETAU(H)=MISCH(H,'ETAU');

*ECOMI(F)=MISCL(F,'ECOMI');



* TAXBASE(G,H)=MISCG(G,H,'TAXBASE');

* TAXBM(G,H)=MISCG(G,H,'TAXBM');

* TAXSD(G,H)=MISCG(G,H,'TAXSD');

* TAXOD(G,H)=MISCG(G,H,'TAXOD');

* TAXPI(G,H)=MISCG(G,H,'TAXPI');

* TAXCVC(G,H)=MISCG(G,H,'TAXCVC');

* MTR(H)= MTR(H,'TRATE');

ETAI(IG)= LANDCAP(IG,'ETAI1');

ETAL1(IG)= LANDCAP(IG,'ETAL1');

ETALB1(IG)= LANDCAP(IG,'ETALB1');

ETAIX('KAP',IG)=ETAI(IG);

*ETAL('LAND',IG)=ETAL1(IG);

ETALB(L,IG)=ETALB1(IG);

* CALCULATE TAX RATES FROM SAM INFORMATION


* DS0(I)=DD0(I) + CX0(I) - M0(I);


*TAUQ(GS,I)=SAM(GS,I) / (  SUM(J, SAM(I,J) )  + SUM(H, SAM(I,H))+ SAM(I, 'INVES')  + SUM(G,SAM(I,G))
*                                - SUM(GS1, SAM(GS1,I) ));

TAUQ(GS,I)=SAM(GS,I) /(  SUM(J, SAM(I,J) )  + SUM(H, SAM(I,H))+ SAM(I, 'INVES')  + SUM(G,SAM(I,G))
             + SAM(I,'ROW') - SUM(GS1, SAM(GS1,I) ));

* + SAM(I,'ROW')

TAUC(GS,I)=TAUQ(GS,I);
TAUV(GS,I)=TAUQ(GS,I);
TAUN(GS,I)=TAUQ(GS,I);
TAUG(GS,I)=TAUQ(GS,I);
TAUX(GS,I)=TAUQ(GS,I);

*TAUG(GS,I)=0;

*TAUM('CYUSE',I)$(SAM('ROW', I))=SAM('CYUSE',I) / SAM('ROW',I);

*TAUM('CYUSE',I)$(SAM('ROW', I))=SAM('CYUSE',I) / (SUM(Z,SAM(I,Z) )-(SUM(J, B1(J,I))+SUM(F,SAM(F,I))+SUM(G, SAM(G,I))));

TAUF0(G,F,Z)=0;

*TAUF(GF,F,I)$(SAM(F,I) AND TAUFF(GF,F))=SSAMT(F,I) / SAM(F,I);

*TAUF(GF,F,G)$(SAM(F,G) AND TAUFF(GF,F))=SSAMT(F,G) / SAM(F,G);

TAUF(GF,F,I)$(SAM(F,I) AND TAUFF(GF,F))=SAM(GF,I) / SAM(F,I);    display tauff, tauf;


TAUF(GF,F,G)$(SAM(F,G) AND TAUFF(GF,F))=SAM(GF,G) / SAM(F,G);


*TAUF(GF,L,I)=.062;
*TAUF('ussocl1na','L5na',I)=0;

*TAUF(GF,'L1na','forsfish')=0;
*TAUF(GF,'L3na','forsfish')=0;
*TAUF(GF,'L4na','forsfish')=0;
*TAUF(GF,'L5na','forsfish')=0;
*TAUF(GF,'L1o','water')=0;
*TAUF(GF,'L5o','water')=0;
*TAUF(GF,'L1na','water')=0;
*TAUF(GF,'L2na','water')=0;
*TAUF(GF,'L3na','water')=0;
*TAUF(GF,'L5na','water')=0;
*TAUF(GF,'L4o','nursing')=0;
*TAUF(GF,'L5o','nursing')=0;
*TAUF(GF,'L3o','social')=0;
*TAUF(GF,'L1na','mining')=0;
*TAUF(GF,'L2na','mining')=0;





TAUFX(GF,F,Z)=TAUF(GF,F,Z);

TAUFH(GF,F)$(TAUFF(GF,F)) =SAM(GF,F) / SUM(Z, SAM(Z,F));

TAUFL(GF,L)=SAM(GF,L) / SUM(Z, SAM(Z,L));


TAUFK(GF,K)=SAM(GF,K) / SUM(Z, SAM(Z,K));    display taufk;

TAXS(G,GX)$(IGTD(G,GX) EQ 1)=SAM(G,GX) / SUM(G1$(IGTD(G1,GX) EQ 1), SAM(G1,GX) );


 TAXS1(GNL)=SAM(GNL,'CYGF') / SUM(GNL1, SAM(GNL1,'CYGF'));




* SET INITIAL INTER GOVERNMENTAL TRANSFERS

IGT0(G,GX)=SAM(G,GX);

 DISPLAY TAXS, TAXS1, IGT0;


* SET INITIAL PRICES TO UNITY LESS SALES AND EXCISE TAXES

PW0(I)=1;

PWM0(I)= 1;

P0(I)=1;

*PH0(HD)=1;

PD0(I)=1;

CPI0(H)=1;

CPIN0(H)=1;

CPIH0(H)=1;

TT(I) = 1;

* CPIH0(H)=1;

* HOUSEHOLD TRANSFER PAYMENTS AND PERSONAL INCOME TAXES

HH0(H)=MISCH(H,'HH0');

HW0(H)=MISCH(H,'HW0');

HN0(H)= HH0(H) - HW0(H);  display hh0, hw0, hn0;


TP(H,G) = 0;

TP(H,G)$(HH0(H))= SAM(H,G) / ( HH0(H) );   display TP;

* FACTOR RENTALS

JOBCOR(H,L)= JOBCR(H,L);
LL(L)= SUM(H, HW0(H)* JOBCOR(H,L))  ; display LL;
*HOUSECOR(H,HD) = HOUSCR(H,HD);

R0(F, Z)=1 ;

R0(F ,IG)$EMPLOY(IG,F)=SAM(F ,IG) / (EMPLOY(IG,F)) ;

* R0(L ,G)$MISC(G,L)=SAM(L ,G) / EMPLOY(G,L) * 1000000;

FD0(F,Z)=EMPLOY(Z,F); DISPLAY FD0;

KS0(K,IG)=FD0(K ,IG);

*LAS0(LA)=SUM(IG,FD0(LA ,IG));

*LAS0(LA,IG)=FD0(LA ,IG);

* FCONST(F,I)=FD0(F,I);

* SHARES FOUND IN THE SOCIAL ACCOUNTING MATRIX DATA

 A(Z,Z1)=SAM(Z,Z1) / Q0(Z1);

DISPLAY Q0, Q10, DQ0;
AG(I,G)$(SUM(J, SAM(J,G) ) + SUM(F, SAM(F,G) ) + SUM(GF, SAM(GF,G) ) )
        =SAM(I,G) / ( SUM(J, SAM(J,G) ) + SUM(F, SAM(F,G) )
        + SUM(GF, SAM(GF,G) ));

* AGFS(L,G)=SAM(L,G)+SAM('USSOCL1',G)+SAM('USSOCL2',G)+SAM('USSOCL3',G);

AGFS('L1',G)=SAM('L1',G)+SAM('USSOCL1',G);

AGFS('L2',G)=SAM('L2',G)+SAM('USSOCL2',G);

AGFS('L3',G)=SAM('L3',G)+SAM('USSOCL3',G);

 AGFS('L4',G)=SAM('L4',G)+SAM('USSOCL4',G);

 AGFS('L5',G)=SAM('L5',G)+SAM('USSOCL5',G);

 AGFS('L6',G)=SAM('L6',G)+SAM('USSOCL6',G);

 AGFS('L7',G)=SAM('L7',G)+SAM('USSOCL7',G);
 AGFS('L8',G)=SAM('L8',G)+SAM('USSOCL8',G);
 AGFS('L9',G)=SAM('L9',G)+SAM('USSOCL9',G);

AG(F,G)$(SUM(I, SAM(I,G) ) + SUM(F1, SAM(F1,G) ) + SUM(GF, SAM(GF,G) ) )
        =SAM(F,G) / ( SUM(I, SAM(I,G) ) + SUM(F1, SAM(F1,G) )
        + SUM(GF, SAM(GF,G) ) );

AG(L,G)$(SUM(I, SAM(I,G) ) + SUM(F1, SAM(F1,G) ) + SUM(GF, SAM(GF,G) ) )
        =AGFS(L,G) / ( SUM(I, SAM(I,G) ) + SUM(F1, SAM(F1,G) )
        + SUM(GF, SAM(GF,G) ) );




* TRADE INTERMEDIATES CONSUMPTION INVESTMENT INITIAL LEVELS

* CX0(I)=SAM(I,'ROW');

CX0(I)=SAM(I,'ROW')/P0(I) /( 1 + SUM(GS, TAUQ(GS,I) ) );


*M0(I)=SAM('ROW',I) / PWM0(I);

M01(I)= SAM('ROW',I) / PWM0(I);

M0(IP)= SUM(Z,SAM(IP,Z) )-(SUM(J, B1(J,IP))+SUM(F,SAM(F,IP))+SUM(G, SAM(G,IP))) ;

M0(I)= M0(I) / PWM0(I);

V0(I)=SUM(J, SAM(I,J) ) / P0(I) /( 1 + SUM(GS, TAUQ(GS,I) ) );

V0T(I)=SUM(J, SAM(I,J) ) / P0(I) ;

CH0(I,H)=SAM(I,H) / P0(I)/ ( 1 + SUM(GS, TAUQ(GS,I) ) );

CH0T(I,H)=SAM(I,H) / P0(I);

CG0(I,GN)=SAM(I,GN) / P0(I)/ ( 1 + SUM(GS, TAUQ(GS,I) ) );

CG0T(I,GN)=SAM(I,GN) / P0(I);

 DEPR= SUM(IG, SAM(IG,'INVES') ) / (SUM((K,IG), KS0(K,IG)));

*SD8= SUM(IG, SAM(IG,'INVES'));
SD9= SUM((K,IG), KS0(K,IG));

*DISPLAY SD8, SD9;



N0(K,IG)=(KS0(K,IG)) * (DEPR);

CN0(I)=0;

B(I,IG) = BB(I,IG);

CN0(I)=SUM(IG, B(I,IG) * SUM(K, N0(K,IG)) )  / P0(I)/ ( 1 + SUM(GS, TAUN(GS,I) ) );

CN0T(I)=SUM(IG, B(I,IG) * SUM(K, N0(K,IG)) )/P0(I)   ;

DD0(I)= SUM(H, CH0(I,H) ) + SUM(G, CG0(I,G) ) + CN0(I) + V0(I);

D0(I)= 1 - (M0(I) / DD0(I));

* CORRECT IMPORT ELASTICITY TO DOMESTIC SHARE ELASTICITY

* ETAD(I)= - 3.5;
  ETAD(I)= - ETAM(I) * M0(I) / ( DD0(I) * D0(I) );
*  etad('fire')= -3;
*  PRODUCTION DATA

DS0(I)=DD0(I) + CX0(I) - M0(I);

* ETAD(I)= 5*(-DS0(I)/SUM(K, KS0(K,I)));

* ETAD(I)= SUM(K, KS0(K,I))/DS0(I);

 AD(I,J)= SAM(I,J) / P0(I) /(1 + SUM(GS, TAUQ(GS, I)))/ DS0(J)   ;

 AD1(I,J)= SAM(I,J) / P0(I) / DS0(J)   ;
display AD1;

PVA0(I)= PD0(I) - SUM(J, AD(J,I) * P0(J)*(1 + SUM(GS, TAUQ(GS, J))));

RA0(F)=1;

TEST20(F,I) =  SUM(GF$TAUFF(GF,F), SAM(GF,I) ); DISPLAY TEST20;

 ALPHA1(F,I)  =   ( SAM(F,I)            + SUM(GF$TAUFF(GF,F), SAM(GF,I) ) )
                / ( SUM(F1, SAM(F1,I) ) + SUM(GF, SAM(GF,I)) );

*  ALPHA1(F,I)  =    SAM(F,I )
*                / ( SUM(F1, SAM(F1,I)));

 ALPHA(F,I) =  ALPHA1(F,I)/(SUM(F1, ALPHA1(F1,I)));

* ALPHA(F,I)  =   ( SAM(F,I)            + SUM(GF$TAUFF(GF,F), SAM(GF,I) ) )
*                / ( SUM(F1, SAM(F1,I) ) + SUM(GF, SAM(GF,I)             ) );


 GAMMA(I)    = DS0(I) / ( SUM(F, ALPHA(F,I) *  FD0(F,I)) ** ( - RHO(I) ) ) ** ( -1 / RHO(I) );

 DELTA(I) = DS0(I)/ (PROD(F$ALPHA(F,I),FD0(F,I)**ALPHA(F,I)));

 sd8(I)=           (PROD(F$ALPHA(F,I),FD0(F,I)**ALPHA(F,I)));  display sd8;

* DELTA('serv') = DS0('serv')/ (PROD(F$ALPHA(F,'serv'),FD0(F,'serv')**ALPHA(F,'serv')));

* DELTA('accom') = DS0('accom')/ (PROD(F$ALPHA('l8O','accom'),FD0('l8O','accom')**ALPHA('l8O','accom')));

*DELTA('wind') = DS0('wind')/ (PROD(F$ALPHA('l7O','wind'),FD0('l7O','wind')**ALPHA('l7O','wind')));
* OTHER DATA

*CMI0(F)=MISCL(F,'COMIN');

*CMO0(F)=MISCL(F,'COMOT');

*SCALEI(F)$EXWGE(F)=CMI0(F)/(((SUM(I, R0(F,I)) / EXWGE(F)))** ECOMI(F));

*SCALEO(F)=CMO0(F)/((EXWGE(F) / (SUM(I, R0(F,I))))** ECOMO(F));

PRIVRET(H) = SUM(Z,SAM(Z,H))-(SUM(F, SAM(H,F))+ SUM(GX,SAM(H,GX)) );

PRIVRET(H) = PRIVRET(H)/HH0(H);

Y0(F)= SUM(IG, SAM(F,IG) );

KPFOR01(K)=SAM('KAP', 'ROW');

KPFOR0(K) = SUM(Z,SAM(Z,K))-(SUM(IG, SAM(K,IG)));

* LNFOR01(LA)=SAM('LAND', 'ROW');

*LNFOR0(LA) = SUM(Z,SAM(Z,LA))-(SUM(IG, SAM(LA,IG)));

GVFOR0(G) = SAM(G, 'ROW');

GVFOR0(GT) = SUM(Z,SAM(Z,GT))-(SUM(I, SAM(GT,I))+SUM(F, SAM(GT,F))+SUM(H,SAM(GT,H))+SUM(G1,SAM(GT,G1)));

*A(H,L)=SAM(H,L) / HW0(H) /
*          (Y0(L)+ SAM(L,'ROW'))*( 1 - SUM(G, TAUFL(G,L) ) );

*A(H,'KAP')=SAM(H,'KAP') / HW0(H) / (Y0('KAP') + SAM('KAP','ROW'));

*A(H,'LAND')=SAM(H,'LAND') / HW0(H) / (Y0('LAND') + SAM('LAND', 'ROW'))* ( 1 - SUM(G, TAUFLA(G,'LAND')) );

A(H,L)=SAM(H,L) / HW0(H) /
          (Y0(L)+ SUM(Z,SAM(Z,L))-(SUM(IG, SAM(L,IG))))*( 1 - SUM(G, TAUFL(G,L) ) );

A3(H,L)=SAM(H,L) / HW0(H) /
          (Y0(L)+ SUM(Z,SAM(Z,L))-(SUM(IG, SAM(L,IG))))*( 1 - SUM(G, TAUFL(G,L) ) );  display A3;

A(H,'KAP')=SAM(H,'KAP') / HW0(H) / (Y0('KAP') + SUM(Z,SAM(Z,'KAP'))-(SUM(IG, SAM('KAP',IG))));

*A(H,'LAND')=SAM(H,'LAND') / HW0(H) / (Y0('LAND') + SUM(Z,SAM(Z,'LAND'))-(SUM(IG, SAM('LAND',IG))))* ( 1 - SUM(G, TAUFLA(G,'LAND')) );




TAUH(GH,H)=SAM(GH,H) / HH0(H);       display tauh;
TAUH0(GH,H)=SAM(GH,H) / HH0(H);      display tauh0;

S0(H)=SAM('INVES',H);

YD0(H)=SUM(I, SAM(I,H) ) + S0(H)  ;  display yd0;

Y0(G) = SUM(Z, SAM(G,Z)) - SAM(G,'ROW' );

S0(G)=SAM('INVES',G);


* CNFOR0 = SAM('CNPRP', 'ROW');

*LFOR(LA)=LNFOR0(LA)/(SUM(IG, SAM('LAND', IG)));

KFOR(K)=KPFOR0(K)/(SUM(IG, SAM('KAP', IG)));

GFOR(G)$(Y0(G))=GVFOR0(G)/(Y0(G));



DISPLAY GFOR ;

*   CFOR=CNFOR0/(Y0('CNPRP'));



NKI0 = SUM(I, M0(I) * PWM0(I) ) - SUM(I, CX0(I) * PD0(I) )
      - SUM(H, PRIVRET(H)*HH0(H)) - SUM(K, KPFOR0(K)) - SUM(G, GVFOR0(G));

Y0(H)= SUM(L,  A(H,L) * HW0(H) / SUM(H1, A(H1,L) * HW0(H1) )
      * (Y0(L))* ( 1 - SUM(G, TAUFL(G,L) ) ))
      + SUM(K,  A(H,K) * HW0(H) / SUM(H1, A(H1,K) * HW0(H1)) * (Y0(K)
      * ( 1 - SUM(G, TAUFK(G,K)))+ KPFOR0(K)) ) ;



SPI0= SUM(H, Y0(H) ) + SUM((H,G), TP(H,G) * HH0(H) )+ SUM(H, PRIVRET(H)*HH0(H));
*TEST(H) = PRIVRET(H)*HN0(H);

*PIT(GI,H) = SAM(GI,H) / (HH0(H));
*PIT0(GI,H) = SAM(GI,H) / (HH0(H));

PIT(GI,H) = SAM(GI,H) / (Y0(H));

PIT0(GI,H) = SAM(GI,H) / (Y0(H));

MI0(H)=HH0(H) * 0.04;

MO0(H)=HH0(H) * 0.04;

GCP0 =SUM((I,H), (CH0(I,H)))+ SUM(I, CN0(I))+ SUM((I,GN), (CG0(I,GN)))+ SUM(I, CX0(I))-SUM(I, M0(I));

GCP10(I) = SUM(H, CH0(I,H))+ CN0(I)+ SUM(GN, CG0(I,GN))+ CX0(I)-M0(I);


*SD7(L) =  SUM(H, HW0(H)* JOBCOR(H,L));
*SD7(H) = SUM(L,  A(H,L) * HW0(H) / SUM(H1, A(H1,L) * HW0(H1) )
*      * (Y0(L)+ CMIWAGE(L)*CMI0(L))* ( 1 - SUM(G, TAUFL(G,L) ) ));

OPTION DECIMALS=8 ;

*DISPLAY SD7;

*-------------------------------------------------------------------------------------------------------------
* 4. VARIABLES
*-------------------------------------------------------------------------------------------------------------
* 4.1 VARIABLE DECLARATION
*-------------------------------------------------------------------------------------------------------------
VARIABLES
CG(I,G)   PUBLIC CONSUMPTION
CH(I,H)   PRIVATE CONSUMPTION

CN(I)     GROSS INVESTMENT BY SECTOR OF SOURCE
CPI(H)    CONSUMER PRICE INDEX
CPIN(H)   NONHOUSING CONSUMER PRICE INDEX
CPIH(H)   HOUSING CONSUMER PRICE INDEX
CX(I)     EXPORT DEMAND
D(I)      DOMESTIC SHARE OF DOMESTIC DEMAND
DD(I)     DOMESTIC DEMAND
DS(I)     DOMESTIC SUPPLY
FD(F,Z)   SECTORAL FACTOR DEMAND
GCP       GROSS AGGREGATE CITY PRODUCT
GCP1(I)   GROSS CITY PRODUCT BY SECTOR
HH(H)     NUMBER OF HOUSEHOLDS
HN(H)     NUMBER OF NONWORKING HOUSEHOLDS
HW(H)     NUMBER OF WORKING HOUSEHOLDS
IGT(G,G1) INTER GOVERNMENTAL TRANSFERS
KS(K,IG)   CAPITAL FLOW
*LAS(LA,IG) LAND FLOW
M(I)      IMPORTS
N(K,IG)   GROSS INVESTMENT BY SECTOR OF DESTINATION
NKI       NET CAPITAL INFLOW
*LNFOR(LA) LAND OUTFLOW
KPFOR(K)  CAPITAL OUTFLOW
GVFOR(G)  GOVT OUTFLOW
P(I)      AGGREGATE DOMESTIC PRICE PAID BY PURCHASERS
PD(I)     DOMESTIC PRICE RECEIVED BY SUPPLIERS
PVA(I)    VALUE ADDED PRICE
RA(F)     ECONOMY WIDE SCALAR RENTAL RATES OF FACTORS
R(F,Z)    SECTORAL RENTAL RATES
S(Z)      SAVINGS
SPI       PERSONAL INCOME (OBJECTIVE FUNCTION)
V(I)      INTERMEDIATE GOODS
Y(Z)      GROSS INCOMES
YD(H)     AFTER TAX TOTAL HOUSEHOLD INCOMES
YT(G,G1)  GOV INCOME
*DIFFERENCES FOR RESULTS INITIALIZED BELOW
DFCG(I,G)
DFCH(I,H)
DFCN(IG)
DFCPI(H)
DCX(I)
DFD(I)
DFDD(I)
DFS(I)
DGCP
DGCP1(I)
DFHH(H)
DFHN(H)
DFHW(H)
DFFD(F,Z)
DV(I)
DK(K,Z)
DRR(F,Z)
DM(I)
DY(Z)
DDS(I)
*DSS(G)
DDD(I)
DCH(I,H)
*DYD1(H)
*TAXB(I)
*TAXSIM(I)
*TAXBC(H)
*TAXSIMC(H)
*HHPIT(H)
*HHPITB(H)
*TAXBV(I)
*TAXSV(I)
*TAXBN(I)
*TAXSN(I)
*TAXBX(I)
*TAXSX(I)
*TAXBG(I)
*TAXSG(I)
*DLAS(LA,IG)
*TAUTST(F,GN)
SD1(GN)
SD2(F,GN);
*SD4(H)
*SD5(H)
*SD6(H);

*-------------------------------------------------------------------------------------------------------------
* 4.2 INITIALIZATION OF VARIABLES AND REMOVING TRACE NUMBERS
*-------------------------------------------------------------------------------------------------------------

P.L(I)=P0(I);          PD.L(I)     = PD0(I);
PVA.L(I)=PVA0(I);        RA.L(F)     = RA0(F);
R.L(F,Z)=R0(F,Z);        CPI.L(H)    = CPI0(H);
*CMI.L(L)=CMI0(L);        CMO.L(L)=CMO0(L);
DS.L(I)=DS0(I);         DD.L(I)     = DD0(I);
V.L(I)=V0(I);          FD.L(F,Z)   = FD0(F,Z);
HH.L(H)=HH0(H);         HN.L(H)     = HN0(H);
HW.L(H)=HW0(H);         KS.L(K,IG)   = KS0(K,IG);
CN.L(I)=CN0(I);         N.L(K,IG)    =N0(K,IG);
D.L(I)=D0(I);          CX.L(I)     = CX0(I);
M.L(I)=M0(I);          NKI.L       = NKI0;
KPFOR.L(K)   = KPFOR0(K);
*CNFOR.L=CNFOR0;
GVFOR.L(G)=GVFOR0(G);
*TP.L(H,G)=TP(H,G);
Y.L(Z) = Y0(Z);
YD.L(H)=YD0(H);
    IGT.L(G,GX)=IGT0(G,GX);     CH.L(I,H)   = CH0(I,H);
CG.L(I,G)=CG0(I,G);       S.L(Z)      = S0(Z);
SPI.L=SPI0;
* LAS.L(LA) = LAS0(LA);
* LAS.L(LA,IG) = LAS0(LA,IG);
* CPIH.L(H)=CPIH0(H);

* REMOVE TRACE NUMBERS FOR COMPUTATIONAL PURPOSES

P.L(I)$(ABS(P.L(I))           LT 1)=0;
PD.L(I)$(ABS(PD.L(I))         LT 0.00000001)=0;
PVA.L(I)$(ABS(PVA.L(I))       LT 0.00000001)=0;
RA.L(F)$(ABS(RA.L(F))         LT 1)=0;
R.L(F,Z)$(ABS(R.L(F,Z))       LT 0.00000001)=0;
CPI.L(H)$(ABS(CPI.L(H))       LT 0.00000001)=0;
*CMI.L(L)$(ABS(CMI.L(L))       LT 0.00000001)=0;
*CMO.L(L)$(ABS(CMO.L(L))       LT 0.00000001)=0;
DS.L(I)$(ABS(DS.L(I))         LT 0.00000001)=0;
DD.L(I)$(ABS(DD.L(I))         LT 0.00000001)=0;
V.L(I)$(ABS(V.L(I))           LT 0.00000001)=0;
FD.L(F,Z)$(ABS(FD.L(F,Z))     LT 0.00000001)=0;
HH.L(H)$(ABS(HH.L(H))         LT 1)=0;
HN.L(H)$(ABS(HN.L(H))         LT 1)=0;
HW.L(H)$(ABS(HW.L(H))         LT 1)=0;
KS.L(K,IG)$(ABS(KS.L(K,IG))   LT 0.0000001)=0;
*LAS.L(LA)$(ABS(LAS.L(LA))      LT 0.0000001)=0;
*LAS.L(LA,IG)$(ABS(LAS.L(LA,IG)) LT 0.00000001)=0;
CN.L(I)$(ABS(CN.L(I))         LT 0.00000001)=0;
N.L(K,IG)$(ABS(N.L(K,IG))     LT 0.00000001)=0;
D.L(I)$(ABS(D.L(I))           LT 0.00000001)=0;
CX.L(I)$(ABS(CX.L(I))         LT 0.00000001)=0;
M.L(I)$(ABS(M.L(I))           LT 0.00000001)=0;
NKI.L$(ABS(NKI.L)             LT 0.00000001)=0;
*LNFOR.L(LA)$(ABS(LNFOR.L(LA)) LT 0.00000001)=0;
KPFOR.L(K)$(ABS(KPFOR.L(K))   LT 0.00000001)=0;
GVFOR.L(G)$(ABS(GVFOR.L(G))   LT 0.00000001)=0;
*CNFOR.L$(ABS(CNFOR.L)        LT 0.0000001)=0;
*TP.L(H,G)$(ABS(TP.L(H,G))    LT 0.0000001)=0;
Y.L(Z)$(ABS(Y.L(Z))           LT 0.00000001)=0;
YD.L(H)$(ABS(YD.L(H))         LT 0.00000001)=0;
IGT.L(G,G1)$(ABS(IGT.L(G,G1)) LT 0.00000001)=0;
CH.L(I,H)$(ABS(CH.L(I,H))     LT 0.00000001)=0;
CG.L(I,G)$(ABS(CG.L(I,G))     LT 0.00000001)=0;
S.L(Z)$(ABS(S.L(Z))           LT 0.00000001)=0;
SPI.L$(ABS(SPI.L)             LT 0.00000001)=0;
*CPIH.L(H)$(ABS(CPIH.L(H))     LT 0.00000001)=0;

*-------------------------------------------------------------------------------------------------------------
* 4.2 INITIALIZATION OF VARIABLES AND REMOVING TRACE NUMBERS
*-------------------------------------------------------------------------------------------------------------

P.LO(I)=P.L(I)     / 1000;   P.UP(I)     = P.L(I)     * 1000;
PD.LO(I)=PD.L(I)    / 1000;   PD.UP(I)    = PD.L(I)    * 1000;
PVA.LO(I)=PVA.L(I)   / 1000;   PVA.UP(I)   = PVA.L(I)   * 1000;
RA.LO(F)=RA.L(F)    / 1000;   RA.UP(F)    = RA.L(F)    * 1000;
CPI.LO(H)=CPI.L(H)   / 1000;   CPI.UP(H)   = CPI.L(H)   * 1000;
*CMI.LO(L)=CMI.L(L)   /1000;     CMI.UP(L)   = CMI.L(L)   *1000;
* CMO.LO(L)=CMO.L(L)   /1000;    CMO.UP(L)   = CMO.L(L)   *1000;
*CPIH.LO(H)=CPIH.L(H) / 1000;   CPIH.UP(H)   = CPIH.L(H) * 1000;
DS.LO(I)=DS.L(I)    / 1000;   DS.UP(I)    = DS.L(I)    * 1000;
DD.LO(I)=DD.L(I)    / 1000;   DD.UP(I)    = DD.L(I)    * 1000;
D.LO(I)=D.L(I)     / 1000;   D.UP(I)     = D.L(I)     * 1000;
V.LO(I)=V.L(I)     / 1000;   V.UP(I)     = V.L(I)     * 1000;
FD.LO(F,Z)=FD.L(F,Z)  / 1000;   FD.UP(F,Z)  = FD.L(F,Z)  * 1000;
HH.LO(H)=HH.L(H)    / 1000;   HH.UP(H)    = HH.L(H)    * 1000;
HW.LO(H)=HW.L(H)    / 1000;   HW.UP(H)    = HW.L(H)    * 1000;
HN.LO(H)=HN.L(H)    / 1000;   HN.UP(H)    = HN.L(H)    * 1000;
KS.LO(K,IG)=KS.L(K,IG)    / 1000;   KS.UP(K,IG)  = KS.L(K,IG)    * 1000;
*LAS.LO(LA,IG)=LAS.L(LA,IG)  / 1000;  LAS.UP(LA,IG) = LAS.L(LA,IG) *1000;
M.LO(I)=M.L(I)     / 1000;   M.UP(I)     = M.L(I)     * 1000;
Y.LO(Z)=Y.L(Z)     / 1000;   Y.UP(Z)     = Y.L(Z)     * 1000;
YD.LO(H)=YD.L(H)    / 1000;   YD.UP(H)    = YD.L(H)    * 1000;
CH.LO(I,H)=CH.L(I,H)  / 1000;   CH.UP(I,H) = CH.L(I,H) * 1000;
CG.LO(I,G)=CG.L(I,G)  / 1000;   CG.UP(I,G)  = CG.L(I,G)  * 1000;
CN.LO(I)=0;
CX.LO(I)=CX.L(I)    / 1000;   CX.UP(I)    = CX.L(I)    * 1000;
N.LO(K,IG)=0;
R.LO(F,IG)=R.L(F,IG)   / 1000;   R.UP(F,IG)   = R.L(F,IG)   * 1000;

*-------------------------------------------------------------------------------------------------------------
* 5. PRE-MODEL CHECK OF PARAMETERS AND INITIAL VALUES OF VARIABLES
*-------------------------------------------------------------------------------------------------------------
* 5.1 PRINTING OF CALCULATED PARAMETERS AND EXOGENOUS VARIABLES
*-------------------------------------------------------------------------------------------------------------
OPTION DECIMALS=6;
DISPLAY  GAMMA, DELTA, JOBCOR, TAUF, TAUFH,TAUFL, TAUQ, tauv,taux,tauc, taun,taug,taufx, TAXS, ALPHA, AG, AD, A, D0, CX0, M0, DD0,DS0,V0, V0T, FD0,RA0, CN0T, CN0,
DEPR, CG0, CG0T, KS0,N0, RHO, N0,R0, ALPHA, IGT0, TP, PIT, KFOR, NKI0, CH0,CH0T, Y0, TAUV, TAUC, TAUN, TAUFX, GVFOR0
Y0,A;


*-------------------------------------------------------------------------------------------------------------
* 5.2 SAVING OF INITIAL VALUES FOR VARIABLES
*-------------------------------------------------------------------------------------------------------------
*  R1('GFREV',SM)=Y.L('CALGF') + SUM(G, IGT.L('CALGF',G) );
*  R1('SFREV',SM)=SUM(GC, Y.L(GC) - IGT.L('CALGF',GC) );
*  R1('STATIC',SM)=0;
*R1('PIT',SM)=SUM((GI,H), PIT.L(GI,H)*HH.L(H));
*R1('SSC',SM)= SUM(IG, R.L('LAND',IG) * RA.L('LAND') * FD.L('LAND',IG));
*R1('COMM',SM)=SUM(F,CMI.L(F));
*R1('COMMO ',SM)=SUM(F,CMO.L(F));
*R1('CMO',SM)=CMO.L('L1');
*R1('CMI',SM)=CMI.L('L1');
* R1('HC',SM)=SUM(I, CH.L(I, 'HH5'))+ S.L('HH5')+ SUM(GI,PIT.L(GI,'HH5'))*HH.L('HH5')
*             +SUM(GH, TAUH(GH,'HH5') * HH.L('HH5') ) ;
*R1('HC',SM) = D.L('COMMU');
*+ SUM((H,G), TP.L(H,G) * HN.L(H)  ) + SUM(H, PRIVRET(H)*HN.L(H));
*R1('HC',SM)=M.L('MANUF') ;
R1('SPI',SM)=SPI.L;
R1('HH',SM)=SUM(H, HH.L(H) );
R1('HN',SM)=SUM(H, HN.L(H) );
R1('HW',SM)=SUM(H, HW.L(H) );
*R1('W1',SM)= RA.L('L1');
*R1('W2',SM)= RA.L('L2');
*R1('W3',SM)= RA.L('L3');
R1('R',SM)=SUM(Z, R.L('KAP',Z));
*R1('RL',SM)= RA.L('LAND');
R1('L',SM)=SUM((L,Z), FD.L(L,Z) );
R1('K',SM)=SUM(Z, FD.L('KAP',Z) );
*R1('LAND',SM)=SUM(IG, FD0('LAND',IG) );
*R1('LAS',SM)=LAS.L('LAND');
*R1('LAND2',SM)=SUM(Z, FD.L('LAND2',Z) );
  R1('GFSAV',SM)=S.L('CYGF');
*-------------------------------------------------------------------------------------------------------------
* 6. EQUATIONS
*-------------------------------------------------------------------------------------------------------------
* 6.1 EQUATION DECLARATION
*-------------------------------------------------------------------------------------------------------------
EQUATIONS

* HOUSEHOLDS
CPIEQ(H)    CONSUMER PRICE INDICES
* CPIHEQ(H)   HOUSING PRICE INDICES
YEQ(H)      HOUSEHOLD GROSS INCOMES
YDEQ(H)     HOUSEHOLD DISPOSABLE INCOMES
CHEQ(I,H)   PRIVATE CONSUMPTION
* CHHEQ(HD,H) PRIVATE HOUSING CONSUMPTION
SHEQ(H)     HOUSEHOLD SAVINGS

* PRODUCERS
PVAEQ(I)    VALUE ADDED
PFEQ(I)     PRODUCTION FUNCTION
FDEQ(F,I)   FACTOR DEMAND
VEQ(I)      INTERMEDIATE DEMAND
YFEQL(L)    LABOR FACTOR INCOME
YFEQK(K)    CAPITAL FACTOR INCOME
*YFEQLA(LA)  LAND FACTOR INCOME
*LANFOR(LA)      LAND INCOME OUTFLOW
KAPFOR(K)      CAPITAL INCOME OUTFLOW
GOVFOR(G)         GOVT OUTFLOW
*CONFOR          COUNTY OUTFLOW

* TRADE
XEQ(I)      EXPORT DEMAND
DEQ(I)      DOMESTIC SHARES
MEQ(I)      IMPORT DEMAND
PEQ(I)      AGGREGATED PRICES
NKIEQ       NET CAPITAL INFLOW

* INVESTMENT
NEQ(K,I)   GROSS INVESTMENT BY SECTOR OF DESTINATION
CNEQ(I)     GROSS INVESTMENT BY SECTOR OF SOURCE
KSEQ(K,IG)  CAPITAL STOCK
* ITCEQ(I)  INVESTMENT TAX CREDIT

* FACTOR SUPPLY
LSEQ1(H)     LABOR SUPPLY
*LASEQ1(LA,I)     LAND SUPPLY1
*LASEQ2(LA,IG2)     LAND SUPPLY1

* LASEQ2(Z)     LAND SUPPLY2
* LASEQ3(Z)     LAND SUPPLY3

* MIGRATION
 POPEQ(H)    POPULATION
ANEQ(H)     NUMBER OF NON WORKING HOUSEHOLDS


* GOVERNMENT
YGEQ(GX)     GOVERNMENT INCOME
CGEQ(I,GN)  GOVERNMENT ENDOGENOUS PURCHASES OF GOODS AND SERVICES
GFEQ(F,GN)  GOVERNMENT ENDOGENOUS RENTAL OF FACTORS
GSEQL(G)     GOVERNMENT SAVINGS
GSEQJ1(G)     GOVERNMENT SAVINGS
*GSEQJ2(G)     GOVERNMENT SAVINGS
GSEQ(G)     GOVERNMENT SAVINGS
TDEQ(G,G1)  DISTRIBUTION OF TAXES
YGEQ1(GNL)
YGEQ2 (GT)

* MODEL CLOSURE
SPIEQ       STATE PERSONAL INCOME
LMEQ(L)     LABOR MARKET CLEARING
*HHEQ(H,HD)     HOUSEHOLD L CLEARING
KMEQ(K,IG)   CAPITAL MARKET CLEARING
*LAMEQ(LA,IG) LAND MARKET CLEARING
*LAMEQ(LA,IG)
GMEQ(I)     GOODS MARKET CLEARING
DDEQ(I)     DEFINITION OF DOMESTIC DEMAND ;

*-------------------------------------------------------------------------------------------------------------
* 6.2 EQUATION ASSIGNMENT
*-------------------------------------------------------------------------------------------------------------

* HOUSEHOLDS

CPIEQ(H).. CPI(H)=E= SUM(I, P(I)*TT(I)  * ( 1 + SUM(GS, TAUC(GS,I) ) ) * CH(I,H) )
                       / SUM(I, P0(I) * ( 1 + SUM(GS, TAUQ(GS,I) ) ) * CH(I,H) );


YEQ(H).. Y(H)=E= SUM(L,  A(H,L) * HW(H) / SUM(H1, A(H1,L) * HW(H1) )
                 * (Y(L)) * ( 1 - SUM(G, TAUFL(G,L))))
                   + SUM(K,  A(H,K) * HW(H) / SUM(H1, A(H1,K) * HW(H1))
                 * (Y(K) + KPFOR(K)) * ( 1 - SUM(G, TAUFK(G,K) ) ) );


YDEQ(H).. YD(H)=E=   Y(H) + (PRIVRET(H) * HH(H))+ SUM(G, TP(H,G) * HH(H) )
                     - SUM(GI, PIT0(GI,H)  * Y(H))
                     - SUM(G, TAUH(G,H)  * HH(H));


CHEQ(I,H).. CH(I,H)=E= CH0(I,H)
                       * ( ( YD(H)  / YD0(H)  ) / ( CPI(H) / CPI0(H) ) ) ** (BETA(I,H)*1.0)
                       * PROD(J, ( ( P(J)*TT(I)  * ( 1 + SUM(GS, TAUC(GS,J) ) ) )
                                 / ( P0(J) * ( 1 + SUM(GS, TAUQ(GS,J) ) ) ) )** (LAMBDA(J,I)*1) ) ;



SHEQ(H).. S(H)=E= YD(H) - SUM(I, P(I)*TT(I) * CH(I,H) * ( 1 + SUM(GS, TAUC(GS,I) ) ));

* PRODUCERS

PVAEQ(I).. PVA(I) =E= PD(I) - SUM(J, AD(J,I) * P(J)*TT(I) * (1 + SUM(GS, TAUV(GS, J) )) );


* PFEQ(I).. DS(I) =E= GAMMA(I) * SUM(F, ALPHA(F,I) * FD(F,I)) ** ( - RHO(I)) ** ( -1 / RHO(I) );

     PFEQ(I)..DS(I) =E= DELTA(I)*PROD(F$ALPHA(F,I),(FD(F,I))**ALPHA(F,I));
*TT(F,I)*

   FDEQ(F,I).. R(F,I) * RA(F) * ( 1 + SUM(GF,TAUFX(GF,F,I) ) )*  FD(F,I)
           =E= PVA(I) * DS(I) * ALPHA(F,I);


*  VEQ(I).. V(I) =E= SUM(J, AD(I,J) * DS(J) );

VEQ(I).. V(I) =E= V0(I)* (PROD(J, ( ( P(J) *TT(I) * ( 1 + SUM(GS, TAUC(GS,J) ) ) )
                                 / ( P0(J) * ( 1 + SUM(GS, TAUQ(GS,J) ) ) ) )** (LAMBDA(J,I)*1) ));


YFEQL(L).. Y(L) =E= SUM(IG, R(L,IG)* RA(L)*FD(L,IG));

YFEQK(K).. Y('KAP') =E= SUM(IG, R('KAP',IG) * RA('KAP') * FD('KAP',IG));

*YFEQLA(LA).. Y('LAND') =E= SUM(IG, R('LAND',IG) * RA('LAND') * FD('LAND',IG));



*LANFOR(LA).. LNFOR(LA) =E= LFOR(LA)*Y(LA);

KAPFOR(K).. KPFOR(K) =E= KFOR(K)*Y(K);



* TRADE

XEQ(I).. CX(I) =E= CX0(I)*( (PD(I)*(1+SUM(GS,TAUX(GS,I))))
                  /(PW0(I)*(1+SUM(GS,TAUQ(GS,I))))) **(ETAE(I)*1.0);

DEQ(I)$PWM0(I).. D(I) =E= D0(I) * ( PD(I) / PWM0(I))** (ETAD(I)*1.0) ;

MEQ(I).. M(I)=E= ( 1 - D(I) ) * DD(I);

PEQ(I).. P(I)=E= D(I) * PD(I) + ( 1 - D(I) ) * PWM0(I) ;

NKIEQ.. NKI =E= SUM(I, M(I) * PWM0(I) ) - SUM(I, CX(I) * PD(I) )- SUM(H, PRIVRET(H)*HH(H))
        - SUM(K, KPFOR(K)) - SUM(G, GVFOR(G)) ;

* INVESTMENT

NEQ(K,I).. N(K,I)=E= N0(K,I)*( R(K,I)/R0(K,I))** ETAIX(K,I)* ( DS(I) / DS0(I))**(ETAIX(K,I)*1);

CNEQ(I).. P(I)* ( 1 + SUM(GS, TAUN(GS,I) ) )  * CN(I) =E= SUM(IG, B(I,IG) * (SUM(K, N(K,IG))) );

KSEQ(K,IG).. KS(K,IG)=E= KS0(K,IG) * ( 1 - DEPR) + N(K,IG) ;


* FACTOR SUPPLY

LSEQ1(H).. HW(H)/HH(H) =E= HW0(H)/HH0(H)   * ((SUM(L, RA(L) / RA0(L))/9)/ (CPI(H) / CPI0(H)))** (ETARA(H)*1.5)
                   * ( SUM(G, TP(H,G) / CPI(H) )
                  / SUM(G, TP(H,G) / CPI0(H) )) ** ETAPT(H)
                  *  ((SUM(GI, PIT0(GI,H)* HH(H))+ SUM(G, TAUH0(G,H)*HH(H)))
                  /(SUM(GI, PIT(GI,H)* HH(H))+ SUM(G, TAUH(G,H)*HH(H))))**(ETAPIT(H)*1) ;




* MIGRATION

 POPEQ(H).. HH(H)=E= HH0(H) * NRPG(H)
                              + MI0(H) * ( ( YD(H)   / HH(H)   )
                                        / ( YD0(H)  / HH0(H)  )
                                         / ( CPI(H)  / CPI0(H) ) ) ** (ETAYD(H)*2.5)
                                        * ( ( HN(H)   / HH(H)   )
                                         / ( HN0(H)  / HH0(H)  ) ) ** (ETAU(H)*2.5)
                              - MO0(H) * ( ( YD0(H)  / HH0(H)  )
                                         / ( YD(H)   / HH(H)   )
                                         / ( CPI0(H) / CPI(H)  ) ) ** (ETAYD(H)*(2)
                                         * ( ( HN0(H)  / HH0(H)  )
                                        / ( HN(H)   / HH(H)   ) ) ** (ETAU(H)*2));

ANEQ(H).. HN(H)=E= HH(H) - HW(H);

* GOVERNMENT


YGEQ(GX).. Y(GX)=E=   SUM(I, TAUV(GX,I) * V(I) * P(I)*TT(I) )
                        + SUM(I, TAUX(GX,I)* CX(I) * PD(I)*TT(I))
                        + SUM((H,I), TAUC(GX,I) * CH(I,H) * P(I)*TT(I) )
                        + SUM(I, TAUN(GX,I) * CN(I) * P(I)*TT(I) )
                        + SUM((GN,I), TAUG(GX,I) * CG(I,GN) * P(I)*TT(I) )
                        + SUM((F,I), TAUFX(GX,F,I) * RA(F) * R(F,I) * FD(F,I) )
                        + SUM((F,GN), TAUFX(GX,F,GN) * RA(F) * R(F,GN) * FD(F,GN) )
                        + SUM(L, TAUFH(GX,L)*Y(L))
                        + SUM(K, TAUFH(GX,K) * (Y(K)))
                        + SUM(H, TAUH(GX,H) * HH(H) )
                        + SUM(H, PIT0(GX,H) * Y(H) )
                        + SUM(GX1, IGT(GX,GX1));


YGEQ2(GT).. Y(GT)=E= SUM(GX, IGT(GT,GX));

YGEQ1(GNL)..  Y(GNL)=E=TAXS1(GNL)*Y('CYGF');

GOVFOR(G).. GVFOR(G) =E= GFOR(G)*Y(G);

CGEQ(I,GN).. P(I) * ( 1 + SUM(GS, TAUG(GS,I) ) ) * CG(I,GN)
                =E= AG(I,GN) * (Y(GN)+ GVFOR(GN));

GFEQ(F,GN)..  FD(F,GN) * R(F,GN) * RA(F)*( 1 + SUM(GF, TAUFX(GF,F,GN)))
                =E= AG(F,GN) * (Y(GN)+ GVFOR(GN));


GSEQL(GN).. S(GN)=E= (Y(GN)+ GVFOR(GN)) - SUM(I, CG(I,GN) * P(I) * ( 1 + SUM(GS, TAUG(GS,I) ) ) )
                 - SUM(F, FD(F,GN) * R(F,GN) * RA(F) * ( 1 + SUM(GF, TAUFX(GF,F,GN))));

GSEQ(GX).. S(GX)=E= (Y(GX)+GVFOR(GX)) - SUM(H, (TP(H,GX) * HH(H)) ) - SUM(G,IGT(G,GX) );

GSEQJ1('CYGF').. S('CYGF')=E= Y('CYGF') -  Y('CYGF');

TDEQ(G,GX)$(IGTD(G,GX) EQ 1).. IGT(G,GX) =E= TAXS(G,GX) * ( Y(GX) + GVFOR(GX)- SUM(H, (TP(H,GX) * HH(H)) ));


* MODEL CLOSURE

SPIEQ.. SPI =E=  SUM(H, Y(H) )+ SUM((H,G), TP(H,G) * HH(H)  )+ SUM(H, PRIVRET(H)*HH(H));

LMEQ(L).. SUM(H, HW(H)* JOBCOR(H,L)) =E= SUM(Z, FD(L ,Z) );

 KMEQ(K,IG).. KS(K,IG) =E= FD(K,IG);

*LAMEQ(LA,IG).. LAS(LA,IG) =E= FD(LA,IG) ;

*LAMEQ(LA,IG).. LAS(LA,IG) =E= SUM(IG, FD(LA,IG)) ;

GMEQ(I).. DS(I)=E= DD(I) + CX(I) - M(I);

DDEQ(I).. DD(I)=E= V(I) + SUM(H, CH(I,H) ) + SUM(G, CG(I,G) ) + CN(I);

*-------------------------------------------------------------------------------------------------------------
* 6.3 MODEL CLOSURE
*-------------------------------------------------------------------------------------------------------------
*  P.FX(FG)= P0(FG);

* FIX INTER GOVERNMENTAL TRANSFERS TO ZERO IF NOT IN ORIGINAL SAM
  IGT.FX(G,GX)$(NOT IGT0(G,GX))=0;

* FIX EXOGENOUS INTERGOVERNMENTAL TRANSFERS
IGT.FX(G,GX)$(IGTD(G,GX) EQ 2)=IGT0(G,GX);

* FIX INTER SECTORAL WAGE DIFFERENTIALS
  R.FX(L,Z)=R0(L,Z);
*   R.FX(LA,Z)=R0(LA,Z);
*   R.FX(K,Z)=R0(K,Z);

* FIX ECONOMY WIDE SCALAR
*  RA.FX(L)=RA0(L);
*  RA.FX(LA)=RA0(LA);
  RA.FX(K)=RA0(K);

*FIXING THE AMOUNT OF LAND
*LAS.FX(LA,IG) = LAS0(LA,IG);


*-------------------------------------------------------------------------------------------------------------
* 7. SOLVE AND OUTPUT PREPARATION
*-------------------------------------------------------------------------------------------------------------

MODEL FTC0 /ALL/;

* EXPERIMENT LOOP


 LOOP(SM$(ORD(SM) GT 1),
  IF (
(ORD(SM)) >2,

pw0(I)= pw0(I)*1.02;
*  ks0(k,'ngs') = ks0(k,'ngs')*.75;
*   GFOR('state')= GFOR('state')*1.005;
*   GFOR('fed')= GFOR('fed')*1.05;

*    TAXS1(GNL)=TAXS1(GNL)*.989;

*     pw0('retail') = pw0('retail')*.99869;
*     pw0('accom') = pw0('accom')*.9977;




*   GFOR(GN)= GFOR(GN)*1.009;



*   GFOR('cothers')= GFOR('cothers')*1.05;

*NRPG(H) = NRPG(H)*1.01;

*   pw0('manu')=pw0('manu')*1.02;

* pw0('hosp')= pw0('hosp')*1.02;

* New state plan

*PIT0('copit','HH1')= PIT0('copit','HH1')*1.079;
*PIT0('copit','HH2')= PIT0('copit','HH2')*1.079;
*PIT0('copit','HH3')= PIT0('copit','HH3')*1.079;
*PIT0('copit','HH4')= PIT0('copit','HH4')*1.079;
*PIT0('copit','HH5')= PIT0('copit','HH5')*1.079;
*     PIT0('copit','HH6')= PIT0('copit','HH6')*1.286;
*       PIT0('copit','HH7')= PIT0('copit','HH7')*1.286;

*PIT0('copit',H)= PIT0('copit',H)*1.2651;


*       tauc('costx',I)= tauc('costx',I)*0.9;
*       tauv('costx',I)= tauv('costx',I)*0.9;
*       taun('costx',I)= taun('costx',I)*0.9;
*       taux('costx',I)= taux('costx',I)*0.9;
*        taug('costx',I)= taug('costx',I)*0.9;
*   pw0('manu')=pw0('manu')*1.02;


*taxs1(gnl)=taxs1(gnl)*.9;

* Heath Plan
*    PIT0('copit',H)= PIT0('copit',H)*1.08;

*   tauc('costx',I)= tauc('costx',I)*1.034482759;
*       tauv('costx',I)= tauv('costx',I)*1.034482759;
*  taun('costx',I)= taun('costx',I)*1.034482759;
*   taux('costx',I)= taux('costx',I)*1.034482759;
*    taug('costx',I)= taug('costx',I)*1.034482759;

*  P0(I)= P0(I)*1.015;

* THE BOYS



*   PIT0('copit',H)= PIT0('copit',H)*1.26478;
*   tauc('costx',I)= tauc('costx',I)*.6;
*       tauv('costx',I)= tauv('costx',I)*.6;
*   taun('costx',I)= taun('costx',I)*.6;
* taux('costx',I)= taux('costx',I)*.6;
*   taug('costx',I)= taug('costx',I)*.6;

*    TransWare

*    pw0(I)= pw0(I)*1.01;

*AVIATION

*CDAG
*    ks0(k,'TransWare')=  ks0(k,'TransWare')*1.00185;
*      pw0('const')=pw0('const')*1.00007;
*      pw0('TransWare')=pw0('TransWare')*1.0002;
*    DELTA(I)= DELTA(I)*1.000003;

* Federal and Local

*    ks0(k,'TransWare')=  ks0(k,'TransWare')*1.0246;
*      pw0('const')=pw0('const')*1.0008;
*      pw0('TransWare')=pw0('TransWare')*1.0009;

*     DELTA(I)= DELTA(I)*1.000003;
*    V0('transware')=V0('transware')*1.00525;

*    DELTA(I)= DELTA(I)*1.01;
*    NRPG(H) = NRPG(H)*1.01;
*      NRPG('HH1') = NRPG('HH1')*1.01
* Income Tax Plan

*  PIT0('copit',H)= PIT0('copit',H)*1.10905;

* Sales tax Plan

*  tauc('costx',I)= tauc('costx',I)*1.345;
*       tauv('costx',I)= tauv('costx',I)*1.345;
*  taun('costx',I)= taun('costx',I)*1.345;
*   taux('costx',I)= taux('costx',I)*1.345;
*   taug('costx',I)= taug('costx',I)*1.345;

*   PIT0('copit',H)= PIT0('copit',H)*.7001;

*   AD('manuf','hgser')=AD('manuf','hgser')*.9;
*         tauc('costx',I)= tauc('costx',I)*.655;
*        tauv('costx',I)=tauv('costx',I)*.655;
*        taun('costx',I)= taun('costx',I)*.655;
*        taux('costx',I)= taux('costx',I)*.655;
*        taug('costx',I)= taug('costx',I)*.655;
*        PIT0('copit',H)= PIT0('copit',H)*1.2189;

* Normal growth
*         ks0(k,I)=ks0(k,I)*1.01;
*               DELTA(I)= DELTA(I)*1.01;
*                NRPG(H)= NRPG(H)*1.01;
*               pw0(I)=pw0(I)*1.01;






*
*

* Optimal Policy

*PIT0('copit',H)= PIT0('copit',H)*1.11687;

*tauc('COSTX',I)= tauc('COSTX',I)*0.7;
*tauv('COSTX',I)= tauv('COSTX',I)*0.7;
*taun('COSTX',I)= taun('COSTX',I)*0.7;
*taux('COSTX',I)= taux('COSTX',I)*0.7;
*taug('COSTX',I)= taug('COSTX',I)*0.7;

*pw0('unijc')= pw0('unijc')*1.02;

*  ks0(k,'unijc')=  ks0(k,'unijc')*1.02;
*  DELTA(I)= DELTA(I)*1.02;

*     NRPG(H) = NRPG(H)*1.03;



*      JOBCOR('HH1','L1')= JOBCOR('HH1','L1')*.985;

*      JOBCOR('HH2','L1')= JOBCOR('HH2','L1')*.985;
*      JOBCOR('HH2','L2')= JOBCOR('HH2','L2')*.985;

*    JOBCOR('HH3','L1')= JOBCOR('HH3','L1')*.985;
*      JOBCOR('HH3','L2')= JOBCOR('HH3','L2')*.985;

*      JOBCOR('HH4','L1')= JOBCOR('HH4','L1')*.985;
*     JOBCOR('HH4','L2')= JOBCOR('HH4','L2')*.985;

*      JOBCOR('HH5','L1')= JOBCOR('HH5','L1')*.985;
*      JOBCOR('HH5','L2')= JOBCOR('HH5','L2')*.985;
*     JOBCOR('HH5','L3')= JOBCOR('HH5','L3')*1.09;

*       JOBCOR('HH6','L1')= JOBCOR('HH6','L1')*.9999;
*      JOBCOR('HH6','L2')= JOBCOR('HH6','L2')*.985;
*      JOBCOR('HH6','L3')= JOBCOR('HH6','L3')*1.09;
*      JOBCOR('HH6','L4')= JOBCOR('HH6','L4')*1.102;

*      JOBCOR('HH7','L1')= JOBCOR('HH7','L1')*.999;
*      JOBCOR('HH7','L2')= JOBCOR('HH7','L2')*.985;
*      JOBCOR('HH7','L3')= JOBCOR('HH7','L3')*1.09;
*      JOBCOR('HH7','L4')= JOBCOR('HH7','L4')*1.102;
*      JOBCOR('HH7','L5')= JOBCOR('HH7','L5')*1.18;


)



OPTION NLP=MINOS5;
*file opt Minos option file / minos.opt /;
*put opt;
*put ’Iteration limit 500’/
*’Feasibility tolerance 1.0E-5’/ ;
*option iterlim = 650 ;
*Option  Optcr=0.1;
*Option profiletol=.2;
FTC0.scaleopt = 1;
FTC0.OPTFILE = 1;
OPTION SYSOUT = ON;
*FTC0.optfile = 1;
SOLVE FTC0 MAXIMIZING SPI USING NLP;

*  R1('GFREV',SM)=Y.L('CALGF') + SUM(G, IGT.L('CALGF',G) );
*  R1('SFREV',SM)=SUM(GC, Y.L(GC) - IGT.L('CALGF',GC) );
*  R1('STATIC',SM)=TXE(SM,'CABAC') + TXE(SM,'CAPIT') + TXE(SM,'CALSU');
*R1('PIT',SM)=SUM((GI,H), PIT.L(GI,H)*HH.L(H));
*  R1('DSF',SM)=R1('SFREV',SM) - R1('SFREV','TODAY');
*  R1('DDRE',SM)=R1('DGF',SM) + R1('DSF',SM) - R1('STATIC',SM);
*  R1('PDRE',SM)$R1('STATIC',SM)=R1('DDRE',SM) / R1('STATIC',SM) * 100;
*R1('COMM',SM)=SUM(F, CMI.L(F));
R1 ('SPI',SM) = SPI.L;
*R1('COMMO',SM)=SUM(F,CMO.L(F));
* R1('CMO',SM)=CMO.L('L1');
* R1('CMI',SM)=CMI.L('L1');
*  R1('GN',SM)=SUM(I, N.L(I) )   + SUM(L, CMI.L(L))*JOBCOR('HH1')- SUM(L, CMO.L(L))*JOBCOR('HH1');
*R1('SSC',SM)= SUM(IG, R.L('LAND',IG) * RA.L('LAND') * FD.L('LAND',IG));
* R1('HC',SM) = D.L('COMMU');
*R1('HC',SM) = SUM(IG,LAS.L('LAND',IG));
* R1('HC',SM)=M.L('MANUF') ;
* R1('HC',SM)=SUM(I, CH.L(I, 'HH5'))+ S.L('HH5')+ SUM(GI,PIT.L(GI,'HH5'))*HH.L('HH5')
*             +SUM(GH, TAUH(GH,'HH5') * HH.L('HH5') ) ;
R1('HH',SM)=SUM(H, HH.L(H) );
R1('HN',SM)=SUM(H, HN.L(H) );
R1('HW',SM)=SUM(H, HW.L(H) );
*R1('W1',SM)= RA.L('L1');
*R1('W2',SM)= RA.L('L2');
*R1('W3',SM)= RA.L('L3');
R1('R',SM)=SUM(Z, R.L('KAP',Z));
*R1('RL',SM)= RA.L('LAND');
R1('L',SM)=SUM((Z,L), FD.L(L,Z) );
R1('K',SM)=SUM(Z, FD.L('KAP',Z) );
*R1('LAND',SM)=SUM(Z, FD.L('LAND',Z) );
R1('GFSAV',SM)=S.L('CYGF');
R2('M-STAT',SM)=FTC0.MODELSTAT;
R2('S-STAT',SM)=FTC0.SOLVESTAT);

CPIN.L(H)= SUM(IP, P.L(IP)  * ( 1 + SUM(GS, TAUC(GS,IP) ) ) * CH.L(IP,H) )
                       / SUM(IP, P0(IP) * ( 1 + SUM(GS, TAUQ(GS,IP) ) ) * CH.L(IP,H) );

*CPIH.L(H)= SUM(HD, P.L(HD)  * ( 1 + SUM(GS, TAUC(GS,HD) ) ) * CH.L(HD,H) )
*                       / SUM(HD, P0(HD) * ( 1 + SUM(GS, TAUQ(GS,HD) ) ) * CH.L(HD,H) );

DFCG.L(I,G)=CG.L(I,G)-CG0(I,G);
DFFD.L(F,Z) = FD.L(F,Z)-FD0(F,Z);
DV.L(I) = V.L(I)-V0(I);
DK.L(K,Z) = FD.L(K,Z)-FD0(K,Z);
DY.L(Z) = Y.L(Z)-Y0(Z);
DM.L(I) =M.L(I)-M0(I);
DDS.L(I) = DS.L(I)-DS0(I);
DDD.L(I) =DD.L(I) - DD0(I);
DCX.L(I) =CX.L(I) -CX0(I);
GCP1.L(I) =SUM(H, CH.L(I,H))+ CN.L(I)+ SUM(GN, CG.L(I,GN))+ CX.L(I)-M.L(I);
DGCP1.L(I) = GCP1.L(I) - GCP10(I);
DCH.L(I,H) = CH.L(I,H) - CH0(I,H);
* DYD1.L(H) = (YD.L(H) -(SUM((I,GX), TAUC(GX,I)*.6 * CH.L(I,H) * P.L(I)))) - (YD0(H)-SUM((I,GX), TAUC(GX,I) * CH0(I,H) * P0(I)));

*DYD1.L(H) = YD.L(H) - YD0(H);
*TAXB.L(I)= SUM(H, TAUC('costx',I) * CH0(I,H) * P0(I));

*TAXSIM.L(I)= SUM(H, TAUC('costx',I)* CH.L(I,H) * P.L(I));

*TAXBC.L(H)= SUM(I, TAUC('costx',I) * CH0(I,H) * P0(I));

*TAXSIMC.L(H)= SUM(I, TAUC('costx',I)* CH.L(I,H) * P.L(I));

*HHPIT.L(H) = PIT0('copit', H)* Y.L(H);

*HHPITB.L(H) = PIT0('copit', H)* Y0(H);

*TAXBV.L(I)=  TAUV('costx',I)* V0(I) * P0(I);

*TAXSV.L(I) =  TAUV('costx',I) * V.L(I) * P.L(I);

*TAXBX.L(I) = TAUX('costx',I)* CX0(I) * PD0(I);

*TAXSX.L(I) = TAUX('costx',I)* CX.L(I) * PD.L(I);

*TAXBN.L(I) = TAUN('costx',I)* CN0(I) * P0(I);

*TAXSN.L(I) = TAUN('costx',I) * CN.L(I) * P.L(I);

*TAXBG.L(I) = SUM(GN, TAUG('costx',I)* CG0(I,GN) * P0(I));

*TAXSG.L(I) = SUM(GN, TAUG('costx',I) * CG.L(I,GN) * P.L(I));

GCP.L =SUM((I,H), (CH.L(I,H)))+ SUM(I, CN.L(I))+ SUM((I,GN), (CG.L(I,GN)))+ SUM(I, CX.L(I))-SUM(I, M.L(I));
DGCP.L=GCP.L-GCP0;
DRR.L(F,Z) = R.L(F,Z)-R0(F,Z);
*SD3(GX)=SUM((H,I), TAUC(GX,I) * CH.l(I,H) * P.l(I)*TT(I) )  ;
sd4(GX) = SUM(I, TAUX(GX,I)* CX.l(I) * PD.l(I));
sd5(GX)=        SUM(I, TAUV(GX,I) * V.l(I) * P.l(I)*TT(I) );
sd6(GX)=         SUM(I, TAUN(GX,I) * CN.l(I) * P.l(I)*TT(I) );
sd7(GX)=         SUM((GN,I), TAUG(GX,I) * CG.l(I,GN) * P.l(I)*TT(I) );
DYY(H,L)=   A(H,L) * HW.l(H);
dyy2(L) = SUM(H1, A(H1,L) * HW.l(H1));

*dyy1(L) = (Y.l(L)) * ( 1 - SUM(G, TAUFL(G,L)));
*dyy3(h,l)= dyy(H,L)/ dyy2(L);


*dyla(H,LA) = A(H,LA) * HW.l(H);

*dyla1(LA)=  SUM(H1, A(H1,LA) * HW.l(H1));

*dyla2(LA)= (Y.l(LA)+ LNFOR.l(LA) )* ( 1 - SUM(G, TAUFLA(G,LA)));

*DYK(H,K) = A(H,K) * HW.l(H);

*DYK1(K)= SUM(H1, A(H1,K) * HW.l(H1));


*DYK2(K)= (Y.l(K) + KPFOR.l(K)) * ( 1 - SUM(G, TAUFK(G,K)) ) ;

*Rev(G,GX)=    TAXS(G,GX) * ( Y.l(GX) + GVFOR.l(GX)- SUM(H, (TP(H,GX) * HH.l(H)) ));

*TR(GNL)=  TAXS1(GNL)*Y.l('CYGF');

*FD2(F,IG)= R.l(F,IG) * RA.l(F) * FD.l(F,IG);

*SSP(F,IG)= FD2(F,IG)* (SUM(GF,TAUFX(GF,F,IG) ));

*Tottax(GX,I)= TAUV(GX,I) * V.l(I) * P.l(I)+TAUX(GX,I)* CX.l(I) * PD.l(I) + TAUC(GX,I) * (SUM(H,CH.l(I,H))) * P.l(I)
*                  + TAUN(GX,I) * CN.l(I) * P.l(I)+ TAUG(GX,I) * CG.l(I,GX) * P.l(I);

*HHTAX(GH,H)= HH.l(H)*tauh(GH,H);

*InterD(I,J)= AD(I,J) * DS.l(J) ;





*sd11= SUM((I,H), (CH.L(I,H)));
*DSS.L(G) = Y.L(GX)-Y.L(GT)-Y.L(GT);

OPTION DECIMALS=6;
DISPLAY   CG0,CG.L,CH0,CH.L,CN0,CN.L,CPI0,CPI.L,CX0,CX.L,D0,D.L,DD0,DD.L,DS0,DS.L,FD0,FD.L,HH0,HH.L,HN0,HN.L,HW0,HW.L
,IGT0,IGT.L,KS0,KS.L,M0,M.L,N0,N.L,NKI0,NKI.L,KPFOR0,KPFOR.L,GVFOR0,GVFOR.L,
P0,P.L,PD0,PD.L,PVA0,PVA.L, PWM0, RA0,RA.L,R0,R.L,S0,S.L,SPI0,SPI.L,TP,V0,V.L,Y0,Y.L,YD0,YD.L,DFCG.L, DFFD.L,
DY.L, CPI.L, CPIN.L,HH.L,HN.L,HW.L,DDS.L,DM.L,  DCX.L,
 DM.L, DDD.L,  DGCP1.L, GCP1.L, dgcp.L, GCP.L, GCP0, HH.L,HN.L,HW.L, DRR.L, gvfor0,
gvfor.l, pit, etad, privret, alpha, alpha1, DK.L, dch.l;

* PUT RESULTS INTO OUTPUT FILE
PUT 'FTCOL              ';
LOOP(SM, PUT '   ',SM.TL);
PUT /;

PUT '                          ','MODEL    ','              ';
        LOOP(SM$(ORD(SM) GT 1), LOOP(MS$(R2('M-STAT',SM) EQ ORD(MS) ), PUT MS.TL ) );
        PUT /;

PUT '                          ','SOLVER   ', PUT '          ';
        LOOP(SM$(ORD(SM) GT 1), LOOP(SS$(R2('S-STAT',SM) EQ ORD(SS) ), PUT SS.TL ) );
        PUT /;

LOOP(R1H, PUT '          ';
        PUT R1H.TL,
        LOOP(SM, PUT R1(R1H,SM) );
        PUT / );


execute_unload 'outdata2.gdx';
execute 'gdxxrw outdata2.gdx dffd.2';