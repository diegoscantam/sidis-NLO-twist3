!23456  Program to calculate observables in the process e + p ---> h + X
!       Start Program with gfortran -O4 ep2hXNum.f Ctq6Pdf.f QcdPac66.for alf.f DSSV-2008.f fDSS.f -o ep2hXNum1.o; time ./ep2hXNum1.o

      Program ep2hXNum
      implicit none
      integer i,IH,IC,nplot,IT,ICpl,IHpl,ITpl,FINI
      parameter(nplot=40)

      real*8 Test,muFF,D1,z(0:nplot),D1plotu(0:nplot),D1plotd(0:nplot),
     &       D1plotub(0:nplot),D1plotdb(0:nplot),D1plots(0:nplot),
     &       D1plotsb(0:nplot),D1plotc(0:nplot),D1plotb(0:nplot),
     &       D1plotgl(0:nplot)

      real*8 S,UULOFTM1PT,PTmin,PTmax,xF(0:nplot),UULOHermesxF(0:nplot),
     &       UULOFTM1xF,xFmin,xFmax,PT(0:nplot),UULOHermesPT(0:nplot),
     &       UUWWFTM1PT,UUWWHermesxF(0:nplot),UUWWHermesxF1(0:nplot),
     &       UUWWFTM1xF,UUWWHermesPT(0:nplot),UUWWHermesPT1(0:nplot),
     &       UUNLOvirFTM1PT,UUNLOvirHermesxF(0:nplot),
     &       UUNLOvirFTM1xF,UUNLOvirHermesPT(0:nplot),
     &       UUNLOrealFTM1PT,UUNLOrealHermesxF(0:nplot),
     &       UUNLOrealFTM1xF,UUNLOrealHermesPT(0:nplot)

      real*8 LLLOFTM1PT,LLLOHermesxF(0:nplot),
     &       LLLOFTM1xF,LLLOHermesPT(0:nplot),
     &       LLWWFTM1PT,LLWWHermesxF(0:nplot),LLWWHermesxF1(0:nplot),
     &       LLWWFTM1xF,LLWWHermesPT(0:nplot),LLWWHermesPT1(0:nplot),
     &       LLNLOvirFTM1PT,LLNLOvirHermesxF(0:nplot),
     &       LLNLOvirFTM1xF,LLNLOvirHermesPT(0:nplot),
     &       LLNLOrealFTM1PT,LLNLOrealHermesxF(0:nplot),
     &       LLNLOrealFTM1xF,LLNLOrealHermesPT(0:nplot)

      real*8 ALLLOHermesxF(0:nplot),ALLLOHermesPT(0:nplot),
     &       ALLWWHermesxF(0:nplot),ALLWWHermesxF1(0:nplot),
     &       ALLWWHermesPT(0:nplot),ALLWWHermesPT1(0:nplot),
     &       ALLNLOHermesxF(0:nplot),ALLNLOHermesPT(0:nplot)

      real*8 UULOFTM1,PTfix,UULOJLabxF(0:nplot),UULOJLabPT(0:nplot),
     &       UUWWFTM1,UUWWJLabxF(0:nplot),UUWWJLabxF1(0:nplot),
     &       UUWWJLabPT(0:nplot),UUWWJLabPT1(0:nplot),
     &     UUNLOvirFTM1,UUNLOvirJLabxF(0:nplot),UUNLOvirJLabPT(0:nplot),
     &   UUNLOrealFTM1,UUNLOrealJLabxF(0:nplot),UUNLOrealJLabPT(0:nplot)

      real*8 LLLOFTM1,LLLOJLabxF(0:nplot),LLLOJLabPT(0:nplot),
     &       LLWWFTM1,LLWWJLabxF(0:nplot),LLWWJLabxF1(0:nplot),
     &       LLWWJLabPT(0:nplot),LLWWJLabPT1(0:nplot),
     &     LLNLOvirFTM1,LLNLOvirJLabxF(0:nplot),LLNLOvirJLabPT(0:nplot),
     &   LLNLOrealFTM1,LLNLOrealJLabxF(0:nplot),LLNLOrealJLabPT(0:nplot)

      real*8 ALLLOJLabxF(0:nplot),ALLLOJLabPT(0:nplot),
     &       ALLWWJLabxF(0:nplot),ALLWWJLabxF1(0:nplot),
     &       ALLWWJLabPT(0:nplot),ALLWWJLabPT1(0:nplot),
     &       ALLNLOJLabxF(0:nplot),ALLNLOJLabPT(0:nplot)

      real*8 UULOCM,eta(0:nplot),UULOCOMPASSeta(0:nplot),etamin,etamax,
     &       UULOCMeta,UULOCOMPASSPT1(0:nplot),UULOCOMPASSPT2(0:nplot),
     &       mu1,mu2,
     &       UUWWCM,UUWWCOMPASSeta(0:nplot),UUWWCOMPASSeta1(0:nplot),
     &       UUWWCOMPASSPT1(0:nplot),UUWWCOMPASSPT2(0:nplot),UUWWCMeta,
     &       UUNLOvirCM,UUNLOvirCOMPASSeta(0:nplot),UUNLOvirCMeta,
     &       UUNLOvirCOMPASSPT1(0:nplot),UUNLOvirCOMPASSPT2(0:nplot),
     &       UUNLOrealCM,UUNLOrealCOMPASSeta(0:nplot),UUNLOrealCMeta,
     &       UUNLOrealCOMPASSPT1(0:nplot),UUNLOrealCOMPASSPT2(0:nplot)

      real*8 LLLOCM,LLLOCOMPASSeta(0:nplot),
     &       LLLOCMeta,LLLOCOMPASSPT1(0:nplot),LLLOCOMPASSPT2(0:nplot),
     &       LLWWCM,LLWWCOMPASSeta(0:nplot),LLWWCOMPASSeta1(0:nplot),
     &       LLWWCOMPASSPT1(0:nplot),LLWWCOMPASSPT2(0:nplot),LLWWCMeta,
     &       LLNLOvirCM,LLNLOvirCOMPASSeta(0:nplot),LLNLOvirCMeta,
     &       LLNLOvirCOMPASSPT1(0:nplot),LLNLOvirCOMPASSPT2(0:nplot),
     &       LLNLOrealCM,LLNLOrealCOMPASSeta(0:nplot),LLNLOrealCMeta,
     &       LLNLOrealCOMPASSPT1(0:nplot),LLNLOrealCOMPASSPT2(0:nplot)

      real*8 ALLLOCOMPASSeta(0:nplot),ALLLOCOMPASSPT1(0:nplot),
     &       ALLLOCOMPASSPT2(0:nplot),
     &       ALLWWCOMPASSeta(0:nplot),ALLWWCOMPASSeta1(0:nplot),
     &       ALLWWCOMPASSPT1(0:nplot),ALLWWCOMPASSPT2(0:nplot),
     &       ALLNLOCOMPASSeta(0:nplot),ALLNLOCOMPASSPT1(0:nplot),
     &       ALLNLOCOMPASSPT2(0:nplot)

      real*8 UULOEICeta(0:nplot),UULOEICPT(0:nplot),UUWWEICeta(0:nplot),
     &       UUWWEICeta1(0:nplot),UUWWEICPT(0:nplot),UUWWEICPT1(0:nplot)
     &       ,UUNLOvirEICeta(0:nplot),UUNLOvirEICPT(0:nplot)
     &       ,UUNLOrealEICeta(0:nplot),UUNLOrealEICPT(0:nplot)

      real*8 LLLOEICeta(0:nplot),LLLOEICPT(0:nplot),LLWWEICeta(0:nplot),
     &       LLWWEICeta1(0:nplot),LLWWEICPT(0:nplot),LLWWEICPT1(0:nplot)
     &       ,LLNLOvirEICeta(0:nplot),LLNLOvirEICPT(0:nplot)
     &       ,LLNLOrealEICeta(0:nplot),LLNLOrealEICPT(0:nplot)

      real*8 ALLLOEICeta(0:nplot),ALLLOEICPT(0:nplot),
     &       ALLWWEICeta(0:nplot),ALLWWEICeta1(0:nplot),
     &       ALLWWEICPT(0:nplot),ALLWWEICPT1(0:nplot)
     &       ,ALLNLOEICeta(0:nplot),ALLNLOEICPT(0:nplot)

      real*8 UULOJetCM,UULOJetCMeta,UULOEICJeteta(0:nplot),
     &       UULOEICJetPT(0:nplot),UUWWCMJet,UUWWCMJeteta,
     &       UUWWEICJeteta(0:nplot),UUWWEICJetPT(0:nplot),
     &       UUWWEICJeteta1(0:nplot),UUWWEICJetPT1(0:nplot),
     &       UUNLOvirJetCM,UUNLOvirJetCMeta,R1,R2,
     &       UUNLOvirEICJeteta1(0:nplot),UUNLOvirEICJeteta2(0:nplot),
     &       UUNLOvirEICJetPT1(0:nplot),UUNLOvirEICJetPT2(0:nplot),
     &       UUNLOrealCMJet,UUNLOrealCMJeteta,
     &       UUNLOrealEICJeteta1(0:nplot),UUNLOrealEICJetPT1(0:nplot),
     &       UUNLOrealEICJeteta2(0:nplot),UUNLOrealEICJetPT2(0:nplot)

      real*8 LLLOJetCM,LLLOJetCMeta,LLLOEICJeteta(0:nplot),
     &       LLLOEICJetPT(0:nplot),LLWWCMJet,LLWWCMJeteta,
     &       LLWWEICJeteta(0:nplot),LLWWEICJetPT(0:nplot),
     &       LLWWEICJeteta1(0:nplot),LLWWEICJetPT1(0:nplot),
     &       LLNLOvirJetCM,LLNLOvirJetCMeta,
     &       LLNLOvirEICJeteta1(0:nplot),LLNLOvirEICJeteta2(0:nplot),
     &       LLNLOvirEICJetPT1(0:nplot),LLNLOvirEICJetPT2(0:nplot),
     &       LLNLOrealCMJet,LLNLOrealCMJeteta,
     &       LLNLOrealEICJeteta1(0:nplot),LLNLOrealEICJetPT1(0:nplot),
     &       LLNLOrealEICJeteta2(0:nplot),LLNLOrealEICJetPT2(0:nplot)

      real*8 ALLLOEICJeteta(0:nplot),ALLLOEICJetPT(0:nplot),
     &       ALLWWEICJeteta(0:nplot),ALLWWEICJetPT(0:nplot),
     &       ALLWWEICJeteta1(0:nplot),ALLWWEICJetPT1(0:nplot),
     &       ALLNLOEICJeteta1(0:nplot),ALLNLOEICJeteta2(0:nplot),
     &       ALLNLOEICJetPT1(0:nplot),ALLNLOEICJetPT2(0:nplot)

      real*8 E0,M,Pi,thetafixed,Pmin,Pmax,mu(0:nplot),mumin,mumax,
     &       P(0:nplot),
     &       UULOE155p2pip275(0:nplot),UULOE155p2pip275min(0:nplot),
     &       UULOE155p2pip275max(0:nplot),UULOFTM2,
     &       UUWWFTM2,UUWWE155p2pip275(0:nplot),
     &       UUWWE155p2pip275min(0:nplot),UUWWE155p2pip275max(0:nplot),
     &       UUNLOvirFTM2,UUNLOvirE155p2pip275(0:nplot),
     &       UUNLOvirE155p2pip275min(0:nplot),
     &       UUNLOvirE155p2pip275max(0:nplot),
     &       UUNLOrealFTM2,UUNLOrealE155p2pip275(0:nplot),
     &       UUNLOrealE155p2pip275min(0:nplot),
     &       UUNLOrealE155p2pip275max(0:nplot)

      real*8 LLLOE155p2pip275(0:nplot),LLLOE155p2pip275min(0:nplot),
     &       LLLOE155p2pip275max(0:nplot),LLLOFTM2,
     &       LLWWFTM2,LLWWE155p2pip275(0:nplot),
     &       LLWWE155p2pip275min(0:nplot),LLWWE155p2pip275max(0:nplot),
     &       LLNLOvirFTM2,LLNLOvirE155p2pip275(0:nplot),
     &       LLNLOvirE155p2pip275min(0:nplot),
     &       LLNLOvirE155p2pip275max(0:nplot),
     &       LLNLOrealFTM2,LLNLOrealE155p2pip275(0:nplot),
     &       LLNLOrealE155p2pip275min(0:nplot),
     &       LLNLOrealE155p2pip275max(0:nplot)

      real*8 ALLLOE155p2pip275(0:nplot),ALLLOE155p2pip275min(0:nplot),
     &       ALLLOE155p2pip275max(0:nplot),
     &       ALLWWE155p2pip275(0:nplot),
     &       ALLNLOE155p2pip275(0:nplot),
     &       ALLNLOE155p2pip275min(0:nplot),
     &       ALLNLOE155p2pip275max(0:nplot)
      
      real*8 UULOE155p2pip55(0:nplot),UULOE155p2pip55min(0:nplot),
     &       UULOE155p2pip55max(0:nplot),
     &       UUWWE155p2pip55(0:nplot),
     &       UUWWE155p2pip55min(0:nplot),UUWWE155p2pip55max(0:nplot),
     &       UUNLOvirE155p2pip55(0:nplot),
     &       UUNLOvirE155p2pip55min(0:nplot),
     &       UUNLOvirE155p2pip55max(0:nplot),
     &       UUNLOrealE155p2pip55(0:nplot),
     &       UUNLOrealE155p2pip55min(0:nplot),
     &       UUNLOrealE155p2pip55max(0:nplot)

      real*8 LLLOE155p2pip55(0:nplot),LLLOE155p2pip55min(0:nplot),
     &       LLLOE155p2pip55max(0:nplot),
     &       LLWWE155p2pip55(0:nplot),
     &       LLWWE155p2pip55min(0:nplot),LLWWE155p2pip55max(0:nplot),
     &       LLNLOvirE155p2pip55(0:nplot),
     &       LLNLOvirE155p2pip55min(0:nplot),
     &       LLNLOvirE155p2pip55max(0:nplot),
     &       LLNLOrealE155p2pip55(0:nplot),
     &       LLNLOrealE155p2pip55min(0:nplot),
     &       LLNLOrealE155p2pip55max(0:nplot)

      real*8 ALLLOE155p2pip55(0:nplot),ALLLOE155p2pip55min(0:nplot),
     &       ALLLOE155p2pip55max(0:nplot),
     &       ALLWWE155p2pip55(0:nplot),
     &       ALLNLOE155p2pip55(0:nplot),
     &       ALLNLOE155p2pip55min(0:nplot),
     &       ALLNLOE155p2pip55max(0:nplot)

      real*8 UULOE155p2pim275(0:nplot),UULOE155p2pim275min(0:nplot),
     &       UULOE155p2pim275max(0:nplot),
     &       UUWWE155p2pim275(0:nplot),
     &       UUWWE155p2pim275min(0:nplot),UUWWE155p2pim275max(0:nplot),
     &       UUNLOvirE155p2pim275(0:nplot),
     &       UUNLOvirE155p2pim275min(0:nplot),
     &       UUNLOvirE155p2pim275max(0:nplot),
     &       UUNLOrealE155p2pim275(0:nplot),
     &       UUNLOrealE155p2pim275min(0:nplot),
     &       UUNLOrealE155p2pim275max(0:nplot)

      real*8 LLLOE155p2pim275(0:nplot),LLLOE155p2pim275min(0:nplot),
     &       LLLOE155p2pim275max(0:nplot),
     &       LLWWE155p2pim275(0:nplot),
     &       LLWWE155p2pim275min(0:nplot),LLWWE155p2pim275max(0:nplot),
     &       LLNLOvirE155p2pim275(0:nplot),
     &       LLNLOvirE155p2pim275min(0:nplot),
     &       LLNLOvirE155p2pim275max(0:nplot),
     &       LLNLOrealE155p2pim275(0:nplot),
     &       LLNLOrealE155p2pim275min(0:nplot),
     &       LLNLOrealE155p2pim275max(0:nplot)

      real*8 ALLLOE155p2pim275(0:nplot),ALLLOE155p2pim275min(0:nplot),
     &       ALLLOE155p2pim275max(0:nplot),
     &       ALLWWE155p2pim275(0:nplot),
     &       ALLNLOE155p2pim275(0:nplot),
     &       ALLNLOE155p2pim275min(0:nplot),
     &       ALLNLOE155p2pim275max(0:nplot)

      real*8 UULOE155p2pim55(0:nplot),UULOE155p2pim55min(0:nplot),
     &       UULOE155p2pim55max(0:nplot),
     &       UUWWE155p2pim55(0:nplot),
     &       UUWWE155p2pim55min(0:nplot),UUWWE155p2pim55max(0:nplot),
     &       UUNLOvirE155p2pim55(0:nplot),
     &       UUNLOvirE155p2pim55min(0:nplot),
     &       UUNLOvirE155p2pim55max(0:nplot),
     &       UUNLOrealE155p2pim55(0:nplot),
     &       UUNLOrealE155p2pim55min(0:nplot),
     &       UUNLOrealE155p2pim55max(0:nplot)

      real*8 LLLOE155p2pim55(0:nplot),LLLOE155p2pim55min(0:nplot),
     &       LLLOE155p2pim55max(0:nplot),
     &       LLWWE155p2pim55(0:nplot),
     &       LLWWE155p2pim55min(0:nplot),LLWWE155p2pim55max(0:nplot),
     &       LLNLOvirE155p2pim55(0:nplot),
     &       LLNLOvirE155p2pim55min(0:nplot),
     &       LLNLOvirE155p2pim55max(0:nplot),
     &       LLNLOrealE155p2pim55(0:nplot),
     &       LLNLOrealE155p2pim55min(0:nplot),
     &       LLNLOrealE155p2pim55max(0:nplot)

      real*8 ALLLOE155p2pim55(0:nplot),ALLLOE155p2pim55min(0:nplot),
     &       ALLLOE155p2pim55max(0:nplot),
     &       ALLWWE155p2pim55(0:nplot),
     &       ALLNLOE155p2pim55(0:nplot),
     &       ALLNLOE155p2pim55min(0:nplot),
     &       ALLNLOE155p2pim55max(0:nplot)

      real*8 UULOE155p2hp275(0:nplot),UULOE155p2hp275min(0:nplot),
     &       UULOE155p2hp275max(0:nplot),
     &       UUWWE155p2hp275(0:nplot),
     &       UUWWE155p2hp275min(0:nplot),UUWWE155p2hp275max(0:nplot),
     &       UUNLOvirE155p2hp275(0:nplot),
     &       UUNLOvirE155p2hp275min(0:nplot),
     &       UUNLOvirE155p2hp275max(0:nplot),
     &       UUNLOrealE155p2hp275(0:nplot),
     &       UUNLOrealE155p2hp275min(0:nplot),
     &       UUNLOrealE155p2hp275max(0:nplot)

      real*8 LLLOE155p2hp275(0:nplot),LLLOE155p2hp275min(0:nplot),
     &       LLLOE155p2hp275max(0:nplot),
     &       LLWWE155p2hp275(0:nplot),
     &       LLWWE155p2hp275min(0:nplot),LLWWE155p2hp275max(0:nplot),
     &       LLNLOvirE155p2hp275(0:nplot),
     &       LLNLOvirE155p2hp275min(0:nplot),
     &       LLNLOvirE155p2hp275max(0:nplot),
     &       LLNLOrealE155p2hp275(0:nplot),
     &       LLNLOrealE155p2hp275min(0:nplot),
     &       LLNLOrealE155p2hp275max(0:nplot)

      real*8 ALLLOE155p2hp275(0:nplot),ALLLOE155p2hp275min(0:nplot),
     &       ALLLOE155p2hp275max(0:nplot),
     &       ALLWWE155p2hp275(0:nplot),
     &       ALLNLOE155p2hp275(0:nplot),
     &       ALLNLOE155p2hp275min(0:nplot),
     &       ALLNLOE155p2hp275max(0:nplot)

      real*8 UULOE155p2hp55(0:nplot),UULOE155p2hp55min(0:nplot),
     &       UULOE155p2hp55max(0:nplot),
     &       UUWWE155p2hp55(0:nplot),
     &       UUWWE155p2hp55min(0:nplot),UUWWE155p2hp55max(0:nplot),
     &       UUNLOvirE155p2hp55(0:nplot),
     &       UUNLOvirE155p2hp55min(0:nplot),
     &       UUNLOvirE155p2hp55max(0:nplot),
     &       UUNLOrealE155p2hp55(0:nplot),
     &       UUNLOrealE155p2hp55min(0:nplot),
     &       UUNLOrealE155p2hp55max(0:nplot)

      real*8 LLLOE155p2hp55(0:nplot),LLLOE155p2hp55min(0:nplot),
     &       LLLOE155p2hp55max(0:nplot),
     &       LLWWE155p2hp55(0:nplot),
     &       LLWWE155p2hp55min(0:nplot),LLWWE155p2hp55max(0:nplot),
     &       LLNLOvirE155p2hp55(0:nplot),
     &       LLNLOvirE155p2hp55min(0:nplot),
     &       LLNLOvirE155p2hp55max(0:nplot),
     &       LLNLOrealE155p2hp55(0:nplot),
     &       LLNLOrealE155p2hp55min(0:nplot),
     &       LLNLOrealE155p2hp55max(0:nplot)

      real*8 ALLLOE155p2hp55(0:nplot),ALLLOE155p2hp55min(0:nplot),
     &       ALLLOE155p2hp55max(0:nplot),
     &       ALLWWE155p2hp55(0:nplot),
     &       ALLNLOE155p2hp55(0:nplot),
     &       ALLNLOE155p2hp55min(0:nplot),
     &       ALLNLOE155p2hp55max(0:nplot)

      real*8 UULOE155p2hm275(0:nplot),UULOE155p2hm275min(0:nplot),
     &       UULOE155p2hm275max(0:nplot),
     &       UUWWE155p2hm275(0:nplot),
     &       UUWWE155p2hm275min(0:nplot),UUWWE155p2hm275max(0:nplot),
     &       UUNLOvirE155p2hm275(0:nplot),
     &       UUNLOvirE155p2hm275min(0:nplot),
     &       UUNLOvirE155p2hm275max(0:nplot),
     &       UUNLOrealE155p2hm275(0:nplot),
     &       UUNLOrealE155p2hm275min(0:nplot),
     &       UUNLOrealE155p2hm275max(0:nplot)

      real*8 LLLOE155p2hm275(0:nplot),LLLOE155p2hm275min(0:nplot),
     &       LLLOE155p2hm275max(0:nplot),
     &       LLWWE155p2hm275(0:nplot),
     &       LLWWE155p2hm275min(0:nplot),LLWWE155p2hm275max(0:nplot),
     &       LLNLOvirE155p2hm275(0:nplot),
     &       LLNLOvirE155p2hm275min(0:nplot),
     &       LLNLOvirE155p2hm275max(0:nplot),
     &       LLNLOrealE155p2hm275(0:nplot),
     &       LLNLOrealE155p2hm275min(0:nplot),
     &       LLNLOrealE155p2hm275max(0:nplot)

      real*8 ALLLOE155p2hm275(0:nplot),ALLLOE155p2hm275min(0:nplot),
     &       ALLLOE155p2hm275max(0:nplot),
     &       ALLWWE155p2hm275(0:nplot),
     &       ALLNLOE155p2hm275(0:nplot),
     &       ALLNLOE155p2hm275min(0:nplot),
     &       ALLNLOE155p2hm275max(0:nplot)

      real*8 UULOE155p2hm55(0:nplot),UULOE155p2hm55min(0:nplot),
     &       UULOE155p2hm55max(0:nplot),
     &       UUWWE155p2hm55(0:nplot),
     &       UUWWE155p2hm55min(0:nplot),UUWWE155p2hm55max(0:nplot),
     &       UUNLOvirE155p2hm55(0:nplot),
     &       UUNLOvirE155p2hm55min(0:nplot),
     &       UUNLOvirE155p2hm55max(0:nplot),
     &       UUNLOrealE155p2hm55(0:nplot),
     &       UUNLOrealE155p2hm55min(0:nplot),
     &       UUNLOrealE155p2hm55max(0:nplot)

      real*8 LLLOE155p2hm55(0:nplot),LLLOE155p2hm55min(0:nplot),
     &       LLLOE155p2hm55max(0:nplot),
     &       LLWWE155p2hm55(0:nplot),
     &       LLWWE155p2hm55min(0:nplot),LLWWE155p2hm55max(0:nplot),
     &       LLNLOvirE155p2hm55(0:nplot),
     &       LLNLOvirE155p2hm55min(0:nplot),
     &       LLNLOvirE155p2hm55max(0:nplot),
     &       LLNLOrealE155p2hm55(0:nplot),
     &       LLNLOrealE155p2hm55min(0:nplot),
     &       LLNLOrealE155p2hm55max(0:nplot)

      real*8 ALLLOE155p2hm55(0:nplot),ALLLOE155p2hm55min(0:nplot),
     &       ALLLOE155p2hm55max(0:nplot),
     &       ALLWWE155p2hm55(0:nplot),
     &       ALLNLOE155p2hm55(0:nplot),
     &       ALLNLOE155p2hm55min(0:nplot),
     &       ALLNLOE155p2hm55max(0:nplot)

      real*8 UULOE155D2pip275(0:nplot),UULOE155D2pip275min(0:nplot),
     &       UULOE155D2pip275max(0:nplot),UUWWE155D2pip275(0:nplot),
     &       UUWWE155D2pip275min(0:nplot),UUWWE155D2pip275max(0:nplot),
     &       UUNLOvirE155D2pip275(0:nplot),
     &       UUNLOvirE155D2pip275min(0:nplot),
     &       UUNLOvirE155D2pip275max(0:nplot),
     &       UUNLOrealE155D2pip275(0:nplot),
     &       UUNLOrealE155D2pip275min(0:nplot),
     &       UUNLOrealE155D2pip275max(0:nplot)

      real*8 LLLOE155D2pip275(0:nplot),LLLOE155D2pip275min(0:nplot),
     &       LLLOE155D2pip275max(0:nplot),LLWWE155D2pip275(0:nplot),
     &       LLWWE155D2pip275min(0:nplot),LLWWE155D2pip275max(0:nplot),
     &       LLNLOvirE155D2pip275(0:nplot),
     &       LLNLOvirE155D2pip275min(0:nplot),
     &       LLNLOvirE155D2pip275max(0:nplot),
     &       LLNLOrealE155D2pip275(0:nplot),
     &       LLNLOrealE155D2pip275min(0:nplot),
     &       LLNLOrealE155D2pip275max(0:nplot)

      real*8 ALLLOE155D2pip275(0:nplot),ALLLOE155D2pip275min(0:nplot),
     &       ALLLOE155D2pip275max(0:nplot),
     &       ALLWWE155D2pip275(0:nplot),
     &       ALLNLOE155D2pip275(0:nplot),
     &       ALLNLOE155D2pip275min(0:nplot),
     &       ALLNLOE155D2pip275max(0:nplot)
      
      real*8 UULOE155D2pip55(0:nplot),UULOE155D2pip55min(0:nplot),
     &       UULOE155D2pip55max(0:nplot),
     &       UUWWE155D2pip55(0:nplot),
     &       UUWWE155D2pip55min(0:nplot),UUWWE155D2pip55max(0:nplot),
     &       UUNLOvirE155D2pip55(0:nplot),
     &       UUNLOvirE155D2pip55min(0:nplot),
     &       UUNLOvirE155D2pip55max(0:nplot),
     &       UUNLOrealE155D2pip55(0:nplot),
     &       UUNLOrealE155D2pip55min(0:nplot),
     &       UUNLOrealE155D2pip55max(0:nplot)

      real*8 LLLOE155D2pip55(0:nplot),LLLOE155D2pip55min(0:nplot),
     &       LLLOE155D2pip55max(0:nplot),
     &       LLWWE155D2pip55(0:nplot),
     &       LLWWE155D2pip55min(0:nplot),LLWWE155D2pip55max(0:nplot),
     &       LLNLOvirE155D2pip55(0:nplot),
     &       LLNLOvirE155D2pip55min(0:nplot),
     &       LLNLOvirE155D2pip55max(0:nplot),
     &       LLNLOrealE155D2pip55(0:nplot),
     &       LLNLOrealE155D2pip55min(0:nplot),
     &       LLNLOrealE155D2pip55max(0:nplot)

      real*8 ALLLOE155D2pip55(0:nplot),ALLLOE155D2pip55min(0:nplot),
     &       ALLLOE155D2pip55max(0:nplot),
     &       ALLWWE155D2pip55(0:nplot),
     &       ALLNLOE155D2pip55(0:nplot),
     &       ALLNLOE155D2pip55min(0:nplot),
     &       ALLNLOE155D2pip55max(0:nplot)

      real*8 UULOE155D2pim275(0:nplot),UULOE155D2pim275min(0:nplot),
     &       UULOE155D2pim275max(0:nplot),UUWWE155D2pim275(0:nplot),
     &       UUWWE155D2pim275min(0:nplot),UUWWE155D2pim275max(0:nplot),
     &       UUNLOvirE155D2pim275(0:nplot),
     &       UUNLOvirE155D2pim275min(0:nplot),
     &       UUNLOvirE155D2pim275max(0:nplot),
     &       UUNLOrealE155D2pim275(0:nplot),
     &       UUNLOrealE155D2pim275min(0:nplot),
     &       UUNLOrealE155D2pim275max(0:nplot)

      real*8 LLLOE155D2pim275(0:nplot),LLLOE155D2pim275min(0:nplot),
     &       LLLOE155D2pim275max(0:nplot),LLWWE155D2pim275(0:nplot),
     &       LLWWE155D2pim275min(0:nplot),LLWWE155D2pim275max(0:nplot),
     &       LLNLOvirE155D2pim275(0:nplot),
     &       LLNLOvirE155D2pim275min(0:nplot),
     &       LLNLOvirE155D2pim275max(0:nplot),
     &       LLNLOrealE155D2pim275(0:nplot),
     &       LLNLOrealE155D2pim275min(0:nplot),
     &       LLNLOrealE155D2pim275max(0:nplot)

      real*8 ALLLOE155D2pim275(0:nplot),ALLLOE155D2pim275min(0:nplot),
     &       ALLLOE155D2pim275max(0:nplot),
     &       ALLWWE155D2pim275(0:nplot),
     &       ALLNLOE155D2pim275(0:nplot),
     &       ALLNLOE155D2pim275min(0:nplot),
     &       ALLNLOE155D2pim275max(0:nplot)
      
      real*8 UULOE155D2pim55(0:nplot),UULOE155D2pim55min(0:nplot),
     &       UULOE155D2pim55max(0:nplot),
     &       UUWWE155D2pim55(0:nplot),
     &       UUWWE155D2pim55min(0:nplot),UUWWE155D2pim55max(0:nplot),
     &       UUNLOvirE155D2pim55(0:nplot),
     &       UUNLOvirE155D2pim55min(0:nplot),
     &       UUNLOvirE155D2pim55max(0:nplot),
     &       UUNLOrealE155D2pim55(0:nplot),
     &       UUNLOrealE155D2pim55min(0:nplot),
     &       UUNLOrealE155D2pim55max(0:nplot)

      real*8 LLLOE155D2pim55(0:nplot),LLLOE155D2pim55min(0:nplot),
     &       LLLOE155D2pim55max(0:nplot),
     &       LLWWE155D2pim55(0:nplot),
     &       LLWWE155D2pim55min(0:nplot),LLWWE155D2pim55max(0:nplot),
     &       LLNLOvirE155D2pim55(0:nplot),
     &       LLNLOvirE155D2pim55min(0:nplot),
     &       LLNLOvirE155D2pim55max(0:nplot),
     &       LLNLOrealE155D2pim55(0:nplot),
     &       LLNLOrealE155D2pim55min(0:nplot),
     &       LLNLOrealE155D2pim55max(0:nplot)

      real*8 ALLLOE155D2pim55(0:nplot),ALLLOE155D2pim55min(0:nplot),
     &       ALLLOE155D2pim55max(0:nplot),
     &       ALLWWE155D2pim55(0:nplot),
     &       ALLNLOE155D2pim55(0:nplot),
     &       ALLNLOE155D2pim55min(0:nplot),
     &       ALLNLOE155D2pim55max(0:nplot)

      real*8 UULOE155D2hp275(0:nplot),UULOE155D2hp275min(0:nplot),
     &       UULOE155D2hp275max(0:nplot),UUWWE155D2hp275(0:nplot),
     &       UUWWE155D2hp275min(0:nplot),UUWWE155D2hp275max(0:nplot),
     &       UUNLOvirE155D2hp275(0:nplot),
     &       UUNLOvirE155D2hp275min(0:nplot),
     &       UUNLOvirE155D2hp275max(0:nplot),
     &       UUNLOrealE155D2hp275(0:nplot),
     &       UUNLOrealE155D2hp275min(0:nplot),
     &       UUNLOrealE155D2hp275max(0:nplot)

      real*8 LLLOE155D2hp275(0:nplot),LLLOE155D2hp275min(0:nplot),
     &       LLLOE155D2hp275max(0:nplot),LLWWE155D2hp275(0:nplot),
     &       LLWWE155D2hp275min(0:nplot),LLWWE155D2hp275max(0:nplot),
     &       LLNLOvirE155D2hp275(0:nplot),
     &       LLNLOvirE155D2hp275min(0:nplot),
     &       LLNLOvirE155D2hp275max(0:nplot),
     &       LLNLOrealE155D2hp275(0:nplot),
     &       LLNLOrealE155D2hp275min(0:nplot),
     &       LLNLOrealE155D2hp275max(0:nplot)

      real*8 ALLLOE155D2hp275(0:nplot),ALLLOE155D2hp275min(0:nplot),
     &       ALLLOE155D2hp275max(0:nplot),
     &       ALLWWE155D2hp275(0:nplot),
     &       ALLNLOE155D2hp275(0:nplot),
     &       ALLNLOE155D2hp275min(0:nplot),
     &       ALLNLOE155D2hp275max(0:nplot)
      
      real*8 UULOE155D2hp55(0:nplot),UULOE155D2hp55min(0:nplot),
     &       UULOE155D2hp55max(0:nplot),
     &       UUWWE155D2hp55(0:nplot),
     &       UUWWE155D2hp55min(0:nplot),UUWWE155D2hp55max(0:nplot),
     &       UUNLOvirE155D2hp55(0:nplot),
     &       UUNLOvirE155D2hp55min(0:nplot),
     &       UUNLOvirE155D2hp55max(0:nplot),
     &       UUNLOrealE155D2hp55(0:nplot),
     &       UUNLOrealE155D2hp55min(0:nplot),
     &       UUNLOrealE155D2hp55max(0:nplot)

      real*8 LLLOE155D2hp55(0:nplot),LLLOE155D2hp55min(0:nplot),
     &       LLLOE155D2hp55max(0:nplot),
     &       LLWWE155D2hp55(0:nplot),
     &       LLWWE155D2hp55min(0:nplot),LLWWE155D2hp55max(0:nplot),
     &       LLNLOvirE155D2hp55(0:nplot),
     &       LLNLOvirE155D2hp55min(0:nplot),
     &       LLNLOvirE155D2hp55max(0:nplot),
     &       LLNLOrealE155D2hp55(0:nplot),
     &       LLNLOrealE155D2hp55min(0:nplot),
     &       LLNLOrealE155D2hp55max(0:nplot)

      real*8 ALLLOE155D2hp55(0:nplot),ALLLOE155D2hp55min(0:nplot),
     &       ALLLOE155D2hp55max(0:nplot),
     &       ALLWWE155D2hp55(0:nplot),
     &       ALLNLOE155D2hp55(0:nplot),
     &       ALLNLOE155D2hp55min(0:nplot),
     &       ALLNLOE155D2hp55max(0:nplot)

      real*8 UULOE155D2hm275(0:nplot),UULOE155D2hm275min(0:nplot),
     &       UULOE155D2hm275max(0:nplot),UUWWE155D2hm275(0:nplot),
     &       UUWWE155D2hm275min(0:nplot),UUWWE155D2hm275max(0:nplot),
     &       UUNLOvirE155D2hm275(0:nplot),
     &       UUNLOvirE155D2hm275min(0:nplot),
     &       UUNLOvirE155D2hm275max(0:nplot),
     &       UUNLOrealE155D2hm275(0:nplot),
     &       UUNLOrealE155D2hm275min(0:nplot),
     &       UUNLOrealE155D2hm275max(0:nplot)

      real*8 LLLOE155D2hm275(0:nplot),LLLOE155D2hm275min(0:nplot),
     &       LLLOE155D2hm275max(0:nplot),LLWWE155D2hm275(0:nplot),
     &       LLWWE155D2hm275min(0:nplot),LLWWE155D2hm275max(0:nplot),
     &       LLNLOvirE155D2hm275(0:nplot),
     &       LLNLOvirE155D2hm275min(0:nplot),
     &       LLNLOvirE155D2hm275max(0:nplot),
     &       LLNLOrealE155D2hm275(0:nplot),
     &       LLNLOrealE155D2hm275min(0:nplot),
     &       LLNLOrealE155D2hm275max(0:nplot)

      real*8 ALLLOE155D2hm275(0:nplot),ALLLOE155D2hm275min(0:nplot),
     &       ALLLOE155D2hm275max(0:nplot),
     &       ALLWWE155D2hm275(0:nplot),
     &       ALLNLOE155D2hm275(0:nplot),
     &       ALLNLOE155D2hm275min(0:nplot),
     &       ALLNLOE155D2hm275max(0:nplot)
      
      real*8 UULOE155D2hm55(0:nplot),UULOE155D2hm55min(0:nplot),
     &       UULOE155D2hm55max(0:nplot),
     &       UUWWE155D2hm55(0:nplot),
     &       UUWWE155D2hm55min(0:nplot),UUWWE155D2hm55max(0:nplot),
     &       UUNLOvirE155D2hm55(0:nplot),
     &       UUNLOvirE155D2hm55min(0:nplot),
     &       UUNLOvirE155D2hm55max(0:nplot),
     &       UUNLOrealE155D2hm55(0:nplot),
     &       UUNLOrealE155D2hm55min(0:nplot),
     &       UUNLOrealE155D2hm55max(0:nplot)

      real*8 LLLOE155D2hm55(0:nplot),LLLOE155D2hm55min(0:nplot),
     &       LLLOE155D2hm55max(0:nplot),
     &       LLWWE155D2hm55(0:nplot),
     &       LLWWE155D2hm55min(0:nplot),LLWWE155D2hm55max(0:nplot),
     &       LLNLOvirE155D2hm55(0:nplot),
     &       LLNLOvirE155D2hm55min(0:nplot),
     &       LLNLOvirE155D2hm55max(0:nplot),
     &       LLNLOrealE155D2hm55(0:nplot),
     &       LLNLOrealE155D2hm55min(0:nplot),
     &       LLNLOrealE155D2hm55max(0:nplot)

      real*8 ALLLOE155D2hm55(0:nplot),ALLLOE155D2hm55min(0:nplot),
     &       ALLLOE155D2hm55max(0:nplot),
     &       ALLWWE155D2hm55(0:nplot),
     &       ALLNLOE155D2hm55(0:nplot),
     &       ALLNLOE155D2hm55min(0:nplot),
     &       ALLNLOE155D2hm55max(0:nplot)

      common/ FRAGINID / FINI

!      call SetCtq6(400)
      call DSSVINI(0)

      Pi = 2d0*dasin(1d0)

! Test Fragmentation Function

      open(19,file='D1DSSold.dat',status='unknown')
      muFF = 2d0
      FINI = 0

      do i=0,nplot

      z(i) = 0.001d0 + dble(i)/dble(nplot)*(0.95d0 - 0.001d0 )

      D1plotu(i) = D1(z(i),muFF,1,4,1)
      D1plotd(i) = D1(z(i),muFF,2,4,1)
      D1plotub(i) = D1(z(i),muFF,-1,4,1)
      D1plotdb(i) = D1(z(i),muFF,-2,4,1)
      D1plots(i) = D1(z(i),muFF,3,4,1)
      D1plotsb(i) = D1(z(i),muFF,-3,4,1)
      D1plotc(i) = D1(z(i),muFF,4,4,1)
      D1plotb(i) = D1(z(i),muFF,-4,4,1)
      D1plotgl(i) = D1(z(i),muFF,0,4,1)

      write(19,'(10(F16.9))') z(i),D1plotu(i),D1plotd(i),D1plotub(i),
     &       D1plotdb(i),D1plots(i),D1plotsb(i),D1plotc(i),D1plotb(i),
     &       D1plotgl(i)

      end do
      close(19)
    
     
! Charge of fragmenting particle:  IC = 1(+), -1(-), 0 (neutral)
! Species of fragmenting particle: IH = 1(pion), 2(Kaon), 3(Proton), 4(charged hadrons)
! Target Particle: IT = 1(Proton), 2(Deutron), 3(Helium3)

!! ****** HERMES Unpolarized CS binned in PT ****************************

!      IH = 1
!      IC = 1
!      IT = 1

!      S = (7.25d0)**2d0
!      PTmin = 1d0
!      PTmax = 2.2d0

!      open(20,file='UUHermes_xF_PTint.dat',status='unknown')

!      do i=0,nplot
!      xF(i) = -0.6d0 + dble(i)/dble(nplot)*(0.6d0 - (-0.6d0))
!      UULOHermesxF(i) = UULOFTM1PT(S,xF(i),PTmin,PTmax,IH,IC,IT)
!      UUWWHermesxF(i) = 
!     &             UUWWFTM1PT(4,1,S,xF(i),PTmin,PTmax,IH,IC,IT,1)
!      UUWWHermesxF1(i) = 
!     &             UUWWFTM1PT(4,1,S,xF(i),PTmin,PTmax,IH,IC,IT,2)
!      UUNLOvirHermesxF(i) = 
!     &                UUNLOvirFTM1PT(S,xF(i),PTmin,PTmax,IH,IC,IT)
!      UUNLOrealHermesxF(i) = 
!     &             UUNLOrealFTM1PT(4,S,xF(i),PTmin,PTmax,IH,IC,IT)
!      write(20,'(5(F12.6))') 
!     &        xF(i),UULOHermesxF(i),UULOHermesxF(i)+UUWWHermesxF(i),
!     &                  UULOHermesxF(i) + UUWWHermesxF1(i),
!     &              UULOHermesxF(i)+UUWWHermesxF(i)+
!     &                          UUNLOvirHermesxF(i)+UUNLOrealHermesxF(i)
!      end do
!      close(20)

!! ****** HERMES Polarized CS binned in PT ****************************

!      IH = 1
!      IC = 1
!      IT = 1

!      S = (7.25d0)**2d0
!      PTmin = 1d0
!      PTmax = 2.2d0

!      open(21,file='LLHermes_xF_PTint.dat',status='unknown')

!      do i=0,nplot
!      xF(i) = -0.6d0 + dble(i)/dble(nplot)*(0.6d0 - (-0.6d0))
!      LLLOHermesxF(i) = LLLOFTM1PT(S,xF(i),PTmin,PTmax,IH,IC,IT)
!      LLWWHermesxF(i) = 
!     &             LLWWFTM1PT(4,1,S,xF(i),PTmin,PTmax,IH,IC,IT,1)
!      LLWWHermesxF1(i) = 
!     &             LLWWFTM1PT(4,1,S,xF(i),PTmin,PTmax,IH,IC,IT,2)
!      LLNLOvirHermesxF(i) = 
!     &                LLNLOvirFTM1PT(S,xF(i),PTmin,PTmax,IH,IC,IT)
!      LLNLOrealHermesxF(i) = 
!     &             LLNLOrealFTM1PT(4,S,xF(i),PTmin,PTmax,IH,IC,IT)
!      write(21,'(5(F12.6))') 
!     &        xF(i),LLLOHermesxF(i),LLLOHermesxF(i)+LLWWHermesxF(i),
!     &                  LLLOHermesxF(i) + LLWWHermesxF1(i),
!     &              LLLOHermesxF(i)+LLWWHermesxF(i)+
!     &                          LLNLOvirHermesxF(i)+LLNLOrealHermesxF(i)
!      end do
!      close(21)

!      open(22,file='ALLHermes_xF_PTint.dat',status='unknown')

!      do i=0,nplot
!      xF(i) = -0.6d0 + dble(i)/dble(nplot)*(0.6d0 - (-0.6d0))
!      ALLLOHermesxF(i) = LLLOHermesxF(i)/UULOHermesxF(i)
!      ALLWWHermesxF(i) = (LLLOHermesxF(i) + LLWWHermesxF(i))/
!     &                          (UULOHermesxF(i) + UUWWHermesxF(i))
!      ALLWWHermesxF1(i) = (LLLOHermesxF(i) + LLWWHermesxF1(i))/
!     &                          (UULOHermesxF(i) + UUWWHermesxF1(i))
!      ALLNLOHermesxF(i) = (LLLOHermesxF(i) + LLWWHermesxF(i) + 
!     &                     LLNLOvirHermesxF(i) + LLNLOrealHermesxF(i))/
!     &                    (UULOHermesxF(i) + UUWWHermesxF(i) + 
!     &                     UUNLOvirHermesxF(i) + UUNLOrealHermesxF(i))
!      write(22,'(5(F12.6))') 
!     &        xF(i),ALLLOHermesxF(i),ALLWWHermesxF(i),ALLWWHermesxF1(i),
!     &              ALLNLOHermesxF(i)
!      end do
!      close(22)


!! ****** HERMES Unpolarized CS binned in xF ****************************

!      IH = 1
!      IC = 1
!      IT = 1

!      S = (7.25d0)**2d0
!      xFmin = 0.3d0
!      xFmax = 0.55d0

!      open(25,file='UUHermes_PT_xFint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 1d0 + dble(i)/dble(nplot)*(2d0 - (1d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      UULOHermesPT(i) = UULOFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      UUWWHermesPT(i) = 
!     &             UUWWFTM1xF(4,1,S,PT(i),mu1,mu1,xFmin,xFmax,IH,IC,IT)
!      UUWWHermesPT1(i) = 
!     &             UUWWFTM1xF(4,1,S,PT(i),mu1,mu2,xFmin,xFmax,IH,IC,IT)
!      UUNLOvirHermesPT(i) = 
!     &                UUNLOvirFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      UUNLOrealHermesPT(i) = 
!     &             UUNLOrealFTM1xF(4,S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      write(25,'(5(F12.6))') PT(i),UULOHermesPT(i),
!     &                             UULOHermesPT(i) + UUWWHermesPT(i),
!     &                             UULOHermesPT(i) + UUWWHermesPT1(i),
!     &  UULOHermesPT(i) + UUWWHermesPT(i) + 
!     &                       UUNLOvirHermesPT(i) + UUNLOrealHermesPT(i)
!      end do
!      close(25)

!! ****** HERMES Polarized CS binned in xF ****************************

!      IH = 1
!      IC = 1
!      IT = 1

!      S = (7.25d0)**2d0
!      xFmin = 0.3d0
!      xFmax = 0.55d0

!      open(26,file='LLHermes_PT_xFint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 1d0 + dble(i)/dble(nplot)*(2d0 - (1d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      LLLOHermesPT(i) = LLLOFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      LLWWHermesPT(i) = 
!     &             LLWWFTM1xF(4,1,S,PT(i),mu1,mu1,xFmin,xFmax,IH,IC,IT)
!      LLWWHermesPT1(i) = 
!     &             LLWWFTM1xF(4,1,S,PT(i),mu1,mu2,xFmin,xFmax,IH,IC,IT)
!      LLNLOvirHermesPT(i) = 
!     &                LLNLOvirFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      LLNLOrealHermesPT(i) = 
!     &             LLNLOrealFTM1xF(4,S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      write(26,'(5(F12.6))') PT(i),LLLOHermesPT(i),
!     &                             LLLOHermesPT(i) + LLWWHermesPT(i),
!     &                             LLLOHermesPT(i) + LLWWHermesPT1(i),
!     &  LLLOHermesPT(i) + LLWWHermesPT(i) + 
!     &                       LLNLOvirHermesPT(i) + LLNLOrealHermesPT(i)
!      end do
!      close(26)

!      open(27,file='ALLHermes_PT_xFint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 1d0 + dble(i)/dble(nplot)*(2d0 - (1d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      ALLLOHermesPT(i) = LLLOHermesPT(i)/UULOHermesPT(i)
!      ALLWWHermesPT(i) = (LLLOHermesPT(i) + LLWWHermesPT(i))/
!     &                            (UULOHermesPT(i) + UUWWHermesPT(i))
!      ALLWWHermesPT1(i) = (LLLOHermesPT(i) + LLWWHermesPT1(i))/
!     &                            (UULOHermesPT(i) + UUWWHermesPT1(i))
!      ALLNLOHermesPT(i) = (LLLOHermesPT(i) + LLWWHermesPT(i) + 
!     &                  LLNLOvirHermesPT(i) + LLNLOrealHermesPT(i))/
!     &                    (UULOHermesPT(i) + UUWWHermesPT(i) +
!     &                  UUNLOvirHermesPT(i) + UUNLOrealHermesPT(i))
!      write(27,'(5(F12.6))') PT(i),ALLLOHermesPT(i),ALLWWHermesPT(i),
!     &                  ALLWWHermesPT1(i),ALLNLOHermesPT(i)
!      end do
!      close(27)


!! ****** JLab12 Unpolarized CS at fixed PT *****************************

!      IH = 1
!      IC = 1
!      IT = 3

!      S = (4.84d0)**2d0

!      open(30,file='UUJLab_xF_PTfix.dat',status='unknown')
!      PTfix = 1.5d0
!      mu1 = PTfix
!      mu2 = dsqrt(S)/2d0

!      do i=0,nplot
!      xF(i) = -0.4d0 + dble(i)/dble(nplot)*(0.4d0 - (-0.4d0))
!      UULOJLabxF(i) = UULOFTM1(S,xF(i),PTfix,mu1,IH,IC,IT)
!      UUWWJLabxF(i) = UUWWFTM1(4,1,S,xF(i),PTfix,mu1,mu1,IH,IC,IT)
!      UUWWJLabxF1(i) = UUWWFTM1(4,1,S,xF(i),PTfix,mu1,mu2,IH,IC,IT)
!      UUNLOvirJLabxF(i) = UUNLOvirFTM1(S,xF(i),PTfix,mu1,IH,IC,IT)
!      UUNLOrealJLabxF(i) = UUNLOrealFTM1(4,S,xF(i),PTfix,mu1,IH,IC,IT)
!      write(30,'(5(F12.6))') xF(i),UULOJLabxF(i),
!     &                  UULOJLabxF(i) + UUWWJLabxF(i),
!     &                  UULOJLabxF(i) + UUWWJLabxF1(i),
!     &   UULOJLabxF(i) + UUWWJLabxF(i) + 
!     &                            UUNLOvirJLabxF(i) + UUNLOrealJLabxF(i)
!      end do
!      close(30)

!! ****** JLab12 Polarized CS at fixed PT *****************************

!      IH = 1
!      IC = 1
!      IT = 3

!      S = (4.84d0)**2d0

!      open(31,file='LLJLab_xF_PTfix.dat',status='unknown')
!      PTfix = 1.5d0
!      mu1 = PTfix
!      mu2 = dsqrt(S)/2d0

!      do i=0,nplot
!      xF(i) = -0.4d0 + dble(i)/dble(nplot)*(0.4d0 - (-0.4d0))
!      LLLOJLabxF(i) = LLLOFTM1(S,xF(i),PTfix,mu1,IH,IC,IT)
!      LLWWJLabxF(i) = LLWWFTM1(4,1,S,xF(i),PTfix,mu1,mu1,IH,IC,IT)
!      LLWWJLabxF1(i) = LLWWFTM1(4,1,S,xF(i),PTfix,mu1,mu2,IH,IC,IT)
!      LLNLOvirJLabxF(i) = LLNLOvirFTM1(S,xF(i),PTfix,mu1,IH,IC,IT)
!      LLNLOrealJLabxF(i) = LLNLOrealFTM1(4,S,xF(i),PTfix,mu1,IH,IC,IT)
!      write(31,'(5(F12.6))') xF(i),LLLOJLabxF(i),
!     &                  LLLOJLabxF(i) + LLWWJLabxF(i),
!     &                  LLLOJLabxF(i) + LLWWJLabxF1(i),
!     &   LLLOJLabxF(i) + LLWWJLabxF(i) + 
!     &                            LLNLOvirJLabxF(i) + LLNLOrealJLabxF(i)
!      end do
!      close(31)

!      open(32,file='ALLJLab_xF_PTfix.dat',status='unknown')
!      PTfix = 1.5d0
!      mu1 = PTfix
!      mu2 = dsqrt(S)/2d0

!      do i=0,nplot
!      xF(i) = -0.4d0 + dble(i)/dble(nplot)*(0.4d0 - (-0.4d0))
!      ALLLOJLabxF(i) = LLLOJLabxF(i)/UULOJLabxF(i)
!      ALLWWJLabxF(i) = (LLLOJLabxF(i) + LLWWJLabxF(i))/
!     &                                 (UULOJLabxF(i) + UUWWJLabxF(i))
!      ALLWWJLabxF1(i) = (LLLOJLabxF(i) + LLWWJLabxF1(i))/
!     &                                 (UULOJLabxF(i) + UUWWJLabxF1(i))
!      ALLNLOJLabxF(i) = (LLLOJLabxF(i) + LLWWJLabxF(i) + 
!     &                        LLNLOvirJLabxF(i) + LLNLOrealJLabxF(i))/
!     &                     (UULOJLabxF(i) + UUWWJLabxF(i) + 
!     &                        UUNLOvirJLabxF(i) + UUNLOrealJLabxF(i))
!      write(32,'(5(F12.6))') xF(i),ALLLOJLabxF(i),ALLWWJLabxF(i),
!     &                              ALLWWJLabxF1(i),ALLNLOJLabxF(i)
!      end do
!      close(32)

!!****** JLab12 Unpolarized CS binned in xF *****************************

!      IH = 1
!      IC = 1
!      IT = 3

!      S = (4.84d0)**2d0
!      xFmin = -0.4d0
!      xFmax =  0.4d0

!      open(35,file='UUJLab_PT_xFint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 1d0 + dble(i)/dble(nplot)*(2d0 - (1d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      UULOJLabPT(i) = UULOFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      UUWWJLabPT(i) = 
!     &             UUWWFTM1xF(4,1,S,PT(i),mu1,mu1,xFmin,xFmax,IH,IC,IT)
!      UUWWJLabPT1(i) = 
!     &             UUWWFTM1xF(4,1,S,PT(i),mu1,mu2,xFmin,xFmax,IH,IC,IT)
!      UUNLOvirJLabPT(i) = 
!     &               UUNLOvirFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      UUNLOrealJLabPT(i) = 
!     &            UUNLOrealFTM1xF(4,S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      write(35,'(5(F12.6))') PT(i),UULOJLabPT(i),
!     &                             UULOJLabPT(i) + UUWWJLabPT(i),
!     &                             UULOJLabPT(i) + UUWWJLabPT1(i),
!     &    UULOJLabPT(i) + UUWWJLabPT(i) + 
!     &                           UUNLOvirJLabPT(i) + UUNLOrealJLabPT(i)
!      end do
!      close(35)

!!****** JLab12 Polarized CS binned in xF *****************************

!      IH = 1
!      IC = 1
!      IT = 3

!      S = (4.84d0)**2d0
!      xFmin = -0.4d0
!      xFmax =  0.4d0

!      open(36,file='LLJLab_PT_xFint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 1d0 + dble(i)/dble(nplot)*(2d0 - (1d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      LLLOJLabPT(i) = LLLOFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      LLWWJLabPT(i) = 
!     &             LLWWFTM1xF(4,1,S,PT(i),mu1,mu1,xFmin,xFmax,IH,IC,IT)
!      LLWWJLabPT1(i) = 
!     &             LLWWFTM1xF(4,1,S,PT(i),mu1,mu2,xFmin,xFmax,IH,IC,IT)
!      LLNLOvirJLabPT(i) = 
!     &               LLNLOvirFTM1xF(S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      LLNLOrealJLabPT(i) = 
!     &            LLNLOrealFTM1xF(4,S,PT(i),PT(i),xFmin,xFmax,IH,IC,IT)
!      write(36,'(5(F12.6))') PT(i),LLLOJLabPT(i),
!     &                             LLLOJLabPT(i) + LLWWJLabPT(i),
!     &                             LLLOJLabPT(i) + LLWWJLabPT1(i),
!     &    LLLOJLabPT(i) + LLWWJLabPT(i) + 
!     &                           LLNLOvirJLabPT(i) + LLNLOrealJLabPT(i)
!      end do
!      close(36)

!      open(37,file='ALLJLab_PT_xFint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 1d0 + dble(i)/dble(nplot)*(2d0 - (1d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      ALLLOJLabPT(i) = LLLOJLabPT(i)/UULOJLabPT(i)
!      ALLWWJLabPT(i) = (LLLOJLabPT(i) + LLWWJLabPT(i))/
!     &                           (UULOJLabPT(i) + UUWWJLabPT(i))
!      ALLWWJLabPT1(i) = (LLLOJLabPT(i) + LLWWJLabPT1(i))/
!     &                           (UULOJLabPT(i) + UUWWJLabPT1(i))
!      ALLNLOJLabPT(i) = (LLLOJLabPT(i) + LLWWJLabPT(i) + 
!     &                      LLNLOvirJLabPT(i) + LLNLOrealJLabPT(i))/
!     &                     (UULOJLabPT(i) + UUWWJLabPT(i) + 
!     &                      UUNLOvirJLabPT(i) + UUNLOrealJLabPT(i))
!      write(37,'(5(F12.6))') PT(i),ALLLOJLabPT(i),ALLWWJLabPT(i),
!     &                           ALLWWJLabPT1(i),ALLNLOJLabPT(i)
!      end do
!      close(37)

!!****** COMPASS Unpolarized CS at fixed PT *****************************

!      IH = 1
!      IC = 0
!      IT = 1

!      S = (17.4d0)**2d0

!      open(40,file='UUCOMPASS_eta_PTfix.dat',status='unknown')
!      PTfix = 2d0
!      mu1 = PTfix
!      mu2 = dsqrt(S)/2d0

!      do i=0,nplot
!      eta(i) = -0.1d0 + dble(i)/dble(nplot)*(2.1d0 - (-0.1d0))
!      UULOCOMPASSeta(i) = UULOCM(S,eta(i),PTfix,mu1,IH,IC,IT)
!      UUWWCOMPASSeta(i) = UUWWCM(4,2,S,eta(i),PTfix,mu1,mu1,IH,IC,IT)
!      UUWWCOMPASSeta1(i) = UUWWCM(4,2,S,eta(i),PTfix,mu1,mu2,IH,IC,IT)
!      UUNLOvirCOMPASSeta(i) = UUNLOvirCM(S,eta(i),PTfix,mu1,IH,IC,IT)
!      UUNLOrealCOMPASSeta(i) = 
!     &                    UUNLOrealCM(4,S,eta(i),PTfix,mu1,IH,IC,IT)
!      write(40,'(5(F12.6))') eta(i),UULOCOMPASSeta(i),
!     &                   UULOCOMPASSeta(i) + UUWWCOMPASSeta(i),
!     &                   UULOCOMPASSeta(i) + UUWWCOMPASSeta1(i),
!     &   UULOCOMPASSeta(i) + UUWWCOMPASSeta(i) + 
!     &                  UUNLOvirCOMPASSeta(i) + UUNLOrealCOMPASSeta(i)
!      end do
!      close(40)

!!****** COMPASS Polarized CS at fixed PT *****************************

!      IH = 1
!      IC = 0
!      IT = 1

!      S = (17.4d0)**2d0

!      open(41,file='LLCOMPASS_eta_PTfix.dat',status='unknown')
!      PTfix = 2d0
!      mu1 = PTfix
!      mu2 = dsqrt(S)/2d0

!      do i=0,nplot
!      eta(i) = -0.1d0 + dble(i)/dble(nplot)*(2.1d0 - (-0.1d0))
!      LLLOCOMPASSeta(i) = LLLOCM(S,eta(i),PTfix,mu1,IH,IC,IT)
!      LLWWCOMPASSeta(i) = LLWWCM(4,2,S,eta(i),PTfix,mu1,mu1,IH,IC,IT)
!      LLWWCOMPASSeta1(i) = LLWWCM(4,2,S,eta(i),PTfix,mu1,mu2,IH,IC,IT)
!      LLNLOvirCOMPASSeta(i) = LLNLOvirCM(S,eta(i),PTfix,mu1,IH,IC,IT)
!      LLNLOrealCOMPASSeta(i) = 
!     &                    LLNLOrealCM(4,S,eta(i),PTfix,mu1,IH,IC,IT)
!      write(41,'(5(F12.6))') eta(i),LLLOCOMPASSeta(i),
!     &                   LLLOCOMPASSeta(i) + LLWWCOMPASSeta(i),
!     &                   LLLOCOMPASSeta(i) + LLWWCOMPASSeta1(i),
!     &   LLLOCOMPASSeta(i) + LLWWCOMPASSeta(i) + 
!     &                  LLNLOvirCOMPASSeta(i) + LLNLOrealCOMPASSeta(i)
!      end do
!      close(41)

!      open(42,file='ALLCOMPASS_eta_PTfix.dat',status='unknown')
!      PTfix = 2d0
!      mu1 = PTfix
!      mu2 = dsqrt(S)/2d0

!      do i=0,nplot
!      eta(i) = -0.1d0 + dble(i)/dble(nplot)*(2.1d0 - (-0.1d0))
!      ALLLOCOMPASSeta(i) = LLLOCOMPASSeta(i)/UULOCOMPASSeta(i)
!      ALLWWCOMPASSeta(i) = (LLLOCOMPASSeta(i) + LLWWCOMPASSeta(i))/
!     &                          (UULOCOMPASSeta(i) + UUWWCOMPASSeta(i))
!      ALLWWCOMPASSeta1(i) = (LLLOCOMPASSeta(i) + LLWWCOMPASSeta1(i))/
!     &                          (UULOCOMPASSeta(i) + UUWWCOMPASSeta1(i))
!      ALLNLOCOMPASSeta(i) = (LLLOCOMPASSeta(i) + LLWWCOMPASSeta(i) + 
!     &            LLNLOvirCOMPASSeta(i) + LLNLOrealCOMPASSeta(i))/
!     &                          (UULOCOMPASSeta(i) + UUWWCOMPASSeta(i) + 
!     &            UUNLOvirCOMPASSeta(i) + UUNLOrealCOMPASSeta(i))

!      write(42,'(5(F12.6))') eta(i),ALLLOCOMPASSeta(i),
!     &   ALLWWCOMPASSeta(i),ALLWWCOMPASSeta1(i),ALLNLOCOMPASSeta(i)
!      end do
!      close(42)


!! ****** COMPASS Unpolarized CS binned in PT *****************************

!      IH = 1
!      IC = 0
!      IT = 1

!      S = (17.4d0)**2d0
!      etamin = -0.1d0
!      etamax =  2.38d0

!      open(45,file='UUCOMPASS_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 2d0 + dble(i)/dble(nplot)*(3.5d0 - (2d0))
!      mu1 = PT(i)
!      mu2 = 2d0*PT(i)
!      UULOCOMPASSPT1(i) = 
!     &            UULOCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      UULOCOMPASSPT2(i) = 
!     &              UULOCMeta(S,PT(i),mu2,etamin,etamax,IH,IC,IT)
!      UUWWCOMPASSPT1(i) = 
!     &      UUWWCMeta(4,2,S,PT(i),mu1,mu1,etamin,etamax,IH,IC,IT)
!      UUWWCOMPASSPT2(i) = 
!     &      UUWWCMeta(4,2,S,PT(i),mu2,mu2,etamin,etamax,IH,IC,IT)
!      UUNLOvirCOMPASSPT1(i) = 
!     &         UUNLOvirCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      UUNLOvirCOMPASSPT2(i) = 
!     &          UUNLOvirCMeta(S,PT(i),mu2,etamin,etamax,IH,IC,IT)
!      UUNLOrealCOMPASSPT1(i) = 
!     &       UUNLOrealCMeta(4,S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      UUNLOrealCOMPASSPT2(i) = 
!     &       UUNLOrealCMeta(4,S,PT(i),mu2,etamin,etamax,IH,IC,IT)
!      write(45,'(5(F12.6))') PT(i),UULOCOMPASSPT1(i),UULOCOMPASSPT2(i),
!     &  UULOCOMPASSPT1(i) + UUWWCOMPASSPT1(i) + 
!     &  UUNLOvirCOMPASSPT1(i) + UUNLOrealCOMPASSPT1(i),
!     &  UULOCOMPASSPT2(i) + UUWWCOMPASSPT2(i) + 
!     &  UUNLOvirCOMPASSPT2(i) + UUNLOrealCOMPASSPT2(i)
!      end do
!      close(45)

!! ****** COMPASS Polarized CS binned in PT *****************************

!      IH = 1
!      IC = 0
!      IT = 1

!      S = (17.4d0)**2d0
!      etamin = -0.1d0
!      etamax =  2.38d0

!      open(46,file='LLCOMPASS_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 2d0 + dble(i)/dble(nplot)*(3.5d0 - (2d0))
!      mu1 = PT(i)
!      mu2 = 2d0*PT(i)
!      LLLOCOMPASSPT1(i) = 
!     &            LLLOCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      LLLOCOMPASSPT2(i) = 
!     &              LLLOCMeta(S,PT(i),mu2,etamin,etamax,IH,IC,IT)
!      LLWWCOMPASSPT1(i) = 
!     &      LLWWCMeta(4,2,S,PT(i),mu1,mu1,etamin,etamax,IH,IC,IT)
!      LLWWCOMPASSPT2(i) = 
!     &      LLWWCMeta(4,2,S,PT(i),mu2,mu2,etamin,etamax,IH,IC,IT)
!      LLNLOvirCOMPASSPT1(i) = 
!     &         LLNLOvirCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      LLNLOvirCOMPASSPT2(i) = 
!     &          LLNLOvirCMeta(S,PT(i),mu2,etamin,etamax,IH,IC,IT)
!      LLNLOrealCOMPASSPT1(i) = 
!     &       LLNLOrealCMeta(4,S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      LLNLOrealCOMPASSPT2(i) = 
!     &       LLNLOrealCMeta(4,S,PT(i),mu2,etamin,etamax,IH,IC,IT)
!      write(46,'(5(F12.6))') PT(i),LLLOCOMPASSPT1(i),LLLOCOMPASSPT2(i),
!     &  LLLOCOMPASSPT1(i) + LLWWCOMPASSPT1(i) + 
!     &  LLNLOvirCOMPASSPT1(i) + LLNLOrealCOMPASSPT1(i),
!     &  LLLOCOMPASSPT2(i) + LLWWCOMPASSPT2(i) + 
!     &  LLNLOvirCOMPASSPT2(i) + LLNLOrealCOMPASSPT2(i)
!      end do
!      close(46)

!      open(47,file='ALLCOMPASS_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 2d0 + dble(i)/dble(nplot)*(3.5d0 - (2d0))
!      mu1 = PT(i)
!      mu2 = 2d0*PT(i)
!      ALLLOCOMPASSPT1(i) = LLLOCOMPASSPT1(i)/UULOCOMPASSPT1(i)
!      ALLLOCOMPASSPT2(i) = LLLOCOMPASSPT2(i)/UULOCOMPASSPT2(i)
!      ALLNLOCOMPASSPT1(i) = (LLLOCOMPASSPT1(i) + LLWWCOMPASSPT1(i) + 
!     &                LLNLOvirCOMPASSPT1(i) + LLNLOrealCOMPASSPT1(i))/
!     &                      (UULOCOMPASSPT1(i) + UUWWCOMPASSPT1(i) + 
!     &                UUNLOvirCOMPASSPT1(i) + UUNLOrealCOMPASSPT1(i))
!      ALLNLOCOMPASSPT2(i) = (LLLOCOMPASSPT2(i) + LLWWCOMPASSPT2(i) + 
!     &                LLNLOvirCOMPASSPT2(i) + LLNLOrealCOMPASSPT2(i))/
!     &                      (UULOCOMPASSPT2(i) + UUWWCOMPASSPT2(i) + 
!     &                UUNLOvirCOMPASSPT2(i) + UUNLOrealCOMPASSPT2(i))
!      write(47,'(5(F12.6))') PT(i),ALLLOCOMPASSPT1(i),
!     &        ALLLOCOMPASSPT2(i),ALLNLOCOMPASSPT1(i),ALLNLOCOMPASSPT2(i)
!      end do
!      close(47)

! ****** EIC Unpolarized CS at fixed PT *****************************

      IH = 1
      IC = 1
      IT = 1

      S = (100d0)**2d0

      open(50,file='UUEIC_eta_PTfix.dat',status='unknown')
      PTfix = 1d0
      mu1 = PTfix
      mu2 = dsqrt(S)/2d0

      do i=0,nplot
      eta(i) = -2d0 + dble(i)/dble(nplot)*(2d0 - (-2d0))
      UULOEICeta(i) = UULOCM(S,eta(i),PTfix,mu1,IH,IC,IT)
      UUWWEICeta(i) = UUWWCM(4,1,S,eta(i),PTfix,mu1,mu1,IH,IC,IT)
      UUWWEICeta1(i) = UUWWCM(4,1,S,eta(i),PTfix,mu1,mu2,IH,IC,IT)
      UUNLOvirEICeta(i) = UUNLOvirCM(S,eta(i),PTfix,mu1,IH,IC,IT)
      UUNLOrealEICeta(i) = UUNLOrealCM(4,S,eta(i),PTfix,mu1,IH,IC,IT)
      write(50,'(5(F12.6))') 
     &       eta(i),UULOEICeta(i), UULOEICeta(i) + UUWWEICeta(i),
     &                  UULOEICeta(i) + UUWWEICeta1(i),
     &  UULOEICeta(i) + UUWWEICeta(i) + UUNLOvirEICeta(i) + 
     &                                               UUNLOrealEICeta(i)
      end do
      close(50)

! ****** EIC Polarized CS at fixed PT *****************************

      IH = 1
      IC = 1
      IT = 1

      S = (100d0)**2d0

      open(51,file='LLEIC_eta_PTfix.dat',status='unknown')
      PTfix = 1d0
      mu1 = PTfix
      mu2 = dsqrt(S)/2d0

      do i=0,nplot
      eta(i) = -2d0 + dble(i)/dble(nplot)*(2d0 - (-2d0))
      LLLOEICeta(i) = LLLOCM(S,eta(i),PTfix,mu1,IH,IC,IT)
      LLWWEICeta(i) = LLWWCM(4,1,S,eta(i),PTfix,mu1,mu1,IH,IC,IT)
      LLWWEICeta1(i) = LLWWCM(4,1,S,eta(i),PTfix,mu1,mu2,IH,IC,IT)
      LLNLOvirEICeta(i) = LLNLOvirCM(S,eta(i),PTfix,mu1,IH,IC,IT)
      LLNLOrealEICeta(i) = LLNLOrealCM(4,S,eta(i),PTfix,mu1,IH,IC,IT)
      write(51,'(5(F12.6))') 
     &       eta(i),LLLOEICeta(i),LLLOEICeta(i) + LLWWEICeta(i),
     &                  LLLOEICeta(i) + LLWWEICeta1(i),
     &  LLLOEICeta(i) +  LLWWEICeta(i) + 
     &                          LLNLOvirEICeta(i) + LLNLOrealEICeta(i)
      end do
      close(51)

      open(52,file='ALLEIC_eta_PTfix.dat',status='unknown')
      PTfix = 1d0
      mu1 = PTfix
      mu2 = dsqrt(S)/2d0

      do i=0,nplot
      eta(i) = -2d0 + dble(i)/dble(nplot)*(2d0 - (-2d0))
      ALLLOEICeta(i) = LLLOEICeta(i)/UULOEICeta(i)
      ALLWWEICeta(i) = (LLLOEICeta(i) + LLWWEICeta(i))/
     &                            (UULOEICeta(i) + UUWWEICeta(i))
      ALLWWEICeta1(i) = (LLLOEICeta(i) + LLWWEICeta1(i))/
     &                            (UULOEICeta(i) + UUWWEICeta1(i))
      ALLNLOEICeta(i) = (LLLOEICeta(i) + LLWWEICeta(i) + 
     &                      LLNLOvirEICeta(i) + LLNLOrealEICeta(i))/
     &                  (UULOEICeta(i) + UUWWEICeta(i) + 
     &                      UUNLOvirEICeta(i) + UUNLOrealEICeta(i))
      
      write(52,'(5(F12.6))') 
     &       eta(i),ALLLOEICeta(i),ALLWWEICeta(i),ALLWWEICeta1(i),
     &       ALLNLOEICeta(i) 
      end do
      close(52)

!! ****** EIC Unpolarized CS binned in eta  *****************************

!      IH = 1
!      IC = 1
!      IT = 1

!      S = (100d0)**2d0
!      etamin = -2d0
!      etamax =  2d0

!      open(55,file='UUEIC_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 3d0 + dble(i)/dble(nplot)*(10d0 - (3d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      UULOEICPT(i) = UULOCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      UUWWEICPT(i) = 
!     &         UUWWCMeta(4,1,S,PT(i),mu1,mu1,etamin,etamax,IH,IC,IT)
!      UUWWEICPT1(i) = 
!     &         UUWWCMeta(4,1,S,PT(i),mu1,mu2,etamin,etamax,IH,IC,IT)
!      UUNLOvirEICPT(i) = 
!     &             UUNLOvirCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      UUNLOrealEICPT(i) = 
!     &          UUNLOrealCMeta(4,S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      write(55,'(5(F12.6))') PT(i),UULOEICPT(i),
!     &       UULOEICPT(i) + UUWWEICPT(i),UULOEICPT(i) + UUWWEICPT1(i),
!     &   UULOEICPT(i) + UUWWEICPT(i) + 
!     &                             UUNLOvirEICPT(i) + UUNLOrealEICPT(i)
!      end do
!      close(55)

!! ****** EIC Polarized CS binned in eta  *****************************

!      IH = 1
!      IC = 1
!      IT = 1

!      S = (100d0)**2d0
!      etamin = -2d0
!      etamax =  2d0

!      open(56,file='LLEIC_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 3d0 + dble(i)/dble(nplot)*(10d0 - (3d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      LLLOEICPT(i) = LLLOCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      LLWWEICPT(i) = 
!     &         LLWWCMeta(4,1,S,PT(i),mu1,mu1,etamin,etamax,IH,IC,IT)
!      LLWWEICPT1(i) = 
!     &         LLWWCMeta(4,1,S,PT(i),mu1,mu2,etamin,etamax,IH,IC,IT)
!      LLNLOvirEICPT(i) = 
!     &             LLNLOvirCMeta(S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      LLNLOrealEICPT(i) = 
!     &          LLNLOrealCMeta(4,S,PT(i),mu1,etamin,etamax,IH,IC,IT)
!      write(56,'(5(F12.6))') PT(i),LLLOEICPT(i),
!     &       LLLOEICPT(i) + LLWWEICPT(i),LLLOEICPT(i) + LLWWEICPT1(i),
!     &   LLLOEICPT(i) + LLWWEICPT(i) + 
!     &                             LLNLOvirEICPT(i) + LLNLOrealEICPT(i)
!      end do
!      close(56)

!      open(57,file='ALLEIC_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 3d0 + dble(i)/dble(nplot)*(10d0 - (3d0))
!      mu1 = PT(i)
!      mu2 = dsqrt(S)/2d0
!      ALLLOEICPT(i) = LLLOEICPT(i)/UULOEICPT(i)
!      ALLWWEICPT(i) = (LLLOEICPT(i) + LLWWEICPT(i))/
!     &                     (UULOEICPT(i) + UUWWEICPT(i))
!      ALLWWEICPT1(i) = (LLLOEICPT(i) + LLWWEICPT1(i))/
!     &                     (UULOEICPT(i) + UUWWEICPT1(i))
!      ALLNLOEICPT(i) = (LLLOEICPT(i) + LLWWEICPT(i) + 
!     &                      LLNLOvirEICPT(i) + LLNLOrealEICPT(i))/
!     &                 (UULOEICPT(i) + UUWWEICPT(i) + 
!     &                      UUNLOvirEICPT(i) + UUNLOrealEICPT(i))
!      write(57,'(5(F12.6))') PT(i),ALLLOEICPT(i),
!     &       ALLWWEICPT(i),ALLWWEICPT1(i),ALLNLOEICPT(i)
!      end do
!      close(57)

! ****** EIC(Jets) Unpolarized CS at fixed PT **************************

      IT = 1

      S = (100d0)**2d0

      open(60,file='UUEICJet_eta_PTfix.dat',status='unknown')
      PTfix = 1d0
      mu1 = PTfix
      mu2 = dsqrt(S)/2d0
      R1  = 0.2d0
      R2  = 0.7d0

      do i=0,nplot
      eta(i) = -2d0 + dble(i)/dble(nplot)*(2d0 - (-2d0))
      UULOEICJeteta(i) = UULOJetCM(S,eta(i),PTfix,mu1,IT)
      UUWWEICJeteta(i) = UUWWCMJet(4,1,S,eta(i),PTfix,mu1,mu1,IT)
      UUWWEICJeteta1(i) = UUWWCMJet(4,1,S,eta(i),PTfix,mu1,mu2,IT)
      UUNLOvirEICJeteta1(i) = UUNLOvirJetCM(S,eta(i),PTfix,mu1,R1,IT)
      UUNLOvirEICJeteta2(i) = UUNLOvirJetCM(S,eta(i),PTfix,mu1,R2,IT)
      UUNLOrealEICJeteta1(i) =UUNLOrealCMJet(4,S,eta(i),PTfix,mu1,R1,IT)
      UUNLOrealEICJeteta2(i) =UUNLOrealCMJet(4,S,eta(i),PTfix,mu1,R2,IT)
      write(60,'(6(F18.6))') 
     &       eta(i),UULOEICJeteta(i)
     &               , UULOEICJeteta(i) + UUWWEICJeteta(i),
     &                  UULOEICJeteta(i) + UUWWEICJeteta1(i)
     &  ,UULOEICJeteta(i) + UUWWEICJeteta(i)  + 
     &                 UUNLOvirEICJeteta1(i) + UUNLOrealEICJeteta1(i),
     &  UULOEICJeteta(i) + UUWWEICJeteta(i) + 
     &                   UUNLOvirEICJeteta2(i) +  UUNLOrealEICJeteta2(i)
      end do
      close(60)

! ****** EIC(Jets) Polarized CS at fixed PT **************************

      IT = 1

      S = (100d0)**2d0

      open(61,file='LLEICJet_eta_PTfix.dat',status='unknown')
      PTfix = 1d0
      mu1 = PTfix
      mu2 = dsqrt(S)/2d0
      R1  = 0.2d0
      R2  = 0.7d0

      do i=0,nplot
      eta(i) = -2d0 + dble(i)/dble(nplot)*(2d0 - (-2d0))
      LLLOEICJeteta(i) = LLLOJetCM(S,eta(i),PTfix,mu1,IT)
      LLWWEICJeteta(i) = LLWWCMJet(4,1,S,eta(i),PTfix,mu1,mu1,IT)
      LLWWEICJeteta1(i) = LLWWCMJet(4,1,S,eta(i),PTfix,mu1,mu2,IT)
      LLNLOvirEICJeteta1(i) = LLNLOvirJetCM(S,eta(i),PTfix,mu1,R1,IT)
      LLNLOvirEICJeteta2(i) = LLNLOvirJetCM(S,eta(i),PTfix,mu1,R2,IT)
      LLNLOrealEICJeteta1(i) =LLNLOrealCMJet(4,S,eta(i),PTfix,mu1,R1,IT)
      LLNLOrealEICJeteta2(i) =LLNLOrealCMJet(4,S,eta(i),PTfix,mu1,R2,IT)
      write(61,'(6(F18.6))') 
     &       eta(i),LLLOEICJeteta(i)
     &               , LLLOEICJeteta(i) + LLWWEICJeteta(i),
     &                  LLLOEICJeteta(i) + LLWWEICJeteta1(i)
     &  ,LLLOEICJeteta(i) + LLWWEICJeteta(i)  + 
     &                 LLNLOvirEICJeteta1(i) + LLNLOrealEICJeteta1(i),
     &  LLLOEICJeteta(i) + LLWWEICJeteta(i) + 
     &                   LLNLOvirEICJeteta2(i) +  LLNLOrealEICJeteta2(i)
      end do
      close(61)

      open(62,file='ALLEICJet_eta_PTfix.dat',status='unknown')
      PTfix = 1d0
      mu1 = PTfix
      mu2 = dsqrt(S)/2d0
      R1  = 0.2d0
      R2  = 0.7d0

      do i=0,nplot
      eta(i) = -2d0 + dble(i)/dble(nplot)*(2d0 - (-2d0))
      ALLLOEICJeteta(i) = LLLOEICJeteta(i)/UULOEICJeteta(i)
      ALLWWEICJeteta(i) = (LLLOEICJeteta(i) + LLWWEICJeteta(i))/
     &                        (UULOEICJeteta(i) + UUWWEICJeteta(i))
      ALLWWEICJeteta1(i) = (LLLOEICJeteta(i) + LLWWEICJeteta1(i))/
     &                        (UULOEICJeteta(i) + UUWWEICJeteta1(i))
      ALLNLOEICJeteta1(i) = (LLLOEICJeteta(i) + LLWWEICJeteta(i) + 
     &               LLNLOvirEICJeteta1(i) + LLNLOrealEICJeteta1(i))/
     &                      (UULOEICJeteta(i) + UUWWEICJeteta(i) + 
     &               UUNLOvirEICJeteta1(i) + UUNLOrealEICJeteta1(i))
      ALLNLOEICJeteta2(i) = (LLLOEICJeteta(i) + LLWWEICJeteta(i) + 
     &               LLNLOvirEICJeteta2(i) + LLNLOrealEICJeteta2(i))/
     &                      (UULOEICJeteta(i) + UUWWEICJeteta(i) + 
     &               UUNLOvirEICJeteta2(i) + UUNLOrealEICJeteta2(i))
      write(62,'(6(F18.6))') 
     &    eta(i),ALLLOEICJeteta(i),ALLWWEICJeteta(i),ALLWWEICJeteta1(i),
     &  ALLNLOEICJeteta1(i),ALLNLOEICJeteta2(i)
      end do
      close(62)

!! ****** EIC(Jet) Unpolarized CS binned in eta  ************************

!      IT = 1

!      S = (100d0)**2d0
!      etamin = -2d0
!      etamax =  2d0
!      R1 = 0.2d0
!      R2 = 0.7d0

!      open(65,file='UUEICJet_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 10d0 + dble(i)/dble(nplot)*(40d0 - (10d0))
!      mu1 = PT(i)
!      UULOEICJetPT(i) = UULOJetCMeta(S,PT(i),mu1,etamin,etamax,IT)
!      UUWWEICJetPT(i) = 
!     &         UUWWCMJeteta(4,1,S,PT(i),mu1,mu1,etamin,etamax,IT)
!      UUNLOvirEICJetPT1(i) = 
!     &             UUNLOvirJetCMeta(S,PT(i),mu1,R1,etamin,etamax,IT)
!      UUNLOvirEICJetPT2(i) = 
!     &             UUNLOvirJetCMeta(S,PT(i),mu1,R2,etamin,etamax,IT)
!      UUNLOrealEICJetPT1(i) = 
!     &          UUNLOrealCMJeteta(4,S,PT(i),mu1,R1,etamin,etamax,IT)
!      UUNLOrealEICJetPT2(i) = 
!     &          UUNLOrealCMJeteta(4,S,PT(i),mu1,R2,etamin,etamax,IT)
!      write(65,'(5(F18.6))') PT(i),UULOEICJetPT(i)
!     &      , UULOEICJetPT(i) + UUWWEICJetPT(i),
!     &   UULOEICJetPT(i) + UUWWEICJetPT(i) + 
!     &                     UUNLOvirEICJetPT1(i) + UUNLOrealEICJetPT1(i),
!     &  UULOEICJetPT(i) + UUWWEICJetPT(i) + 
!     &                     UUNLOvirEICJetPT2(i) + UUNLOrealEICJetPT2(i)
!      end do
!      close(65)

!! ****** EIC(Jet) Polarized CS binned in eta  ************************

!      IT = 1

!      S = (100d0)**2d0
!      etamin = -2d0
!      etamax =  2d0
!      R1 = 0.2d0
!      R2 = 0.7d0

!      open(66,file='LLEICJet_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 10d0 + dble(i)/dble(nplot)*(40d0 - (10d0))
!      mu1 = PT(i)
!      LLLOEICJetPT(i) = LLLOJetCMeta(S,PT(i),mu1,etamin,etamax,IT)
!      LLWWEICJetPT(i) = 
!     &         LLWWCMJeteta(4,1,S,PT(i),mu1,mu1,etamin,etamax,IT)
!      LLNLOvirEICJetPT1(i) = 
!     &             LLNLOvirJetCMeta(S,PT(i),mu1,R1,etamin,etamax,IT)
!      LLNLOvirEICJetPT2(i) = 
!     &             LLNLOvirJetCMeta(S,PT(i),mu1,R2,etamin,etamax,IT)
!      LLNLOrealEICJetPT1(i) = 
!     &          LLNLOrealCMJeteta(4,S,PT(i),mu1,R1,etamin,etamax,IT)
!      LLNLOrealEICJetPT2(i) = 
!     &          LLNLOrealCMJeteta(4,S,PT(i),mu1,R2,etamin,etamax,IT)
!      write(66,'(5(F18.6))') PT(i),LLLOEICJetPT(i)
!     &      , LLLOEICJetPT(i) + LLWWEICJetPT(i),
!     &   LLLOEICJetPT(i) + LLWWEICJetPT(i) + 
!     &                     LLNLOvirEICJetPT1(i) + LLNLOrealEICJetPT1(i),
!     &  LLLOEICJetPT(i) + LLWWEICJetPT(i) + 
!     &                     LLNLOvirEICJetPT2(i) + LLNLOrealEICJetPT2(i)
!      end do
!      close(66)

!      open(67,file='ALLEICJet_PT_etaint.dat',status='unknown')

!      do i=0,nplot
!      PT(i) = 10d0 + dble(i)/dble(nplot)*(40d0 - (10d0))
!      mu1 = PT(i)
!      ALLLOEICJetPT(i) = LLLOEICJetPT(i)/UULOEICJetPT(i)
!      ALLWWEICJetPT(i) = (LLLOEICJetPT(i) + LLWWEICJetPT(i))/
!     &                            (UULOEICJetPT(i) + UUWWEICJetPT(i))
!      ALLNLOEICJetPT1(i) = (LLLOEICJetPT(i) + LLWWEICJetPT(i) + 
!     &              LLNLOvirEICJetPT1(i) + LLNLOrealEICJetPT1(i))/
!     &                     (UULOEICJetPT(i) + UUWWEICJetPT(i) + 
!     &              UUNLOvirEICJetPT1(i) + UUNLOrealEICJetPT1(i))
!      ALLNLOEICJetPT2(i) = (LLLOEICJetPT(i) + LLWWEICJetPT(i) + 
!     &              LLNLOvirEICJetPT2(i) + LLNLOrealEICJetPT2(i))/
!     &                     (UULOEICJetPT(i) + UUWWEICJetPT(i) + 
!     &              UUNLOvirEICJetPT2(i) + UUNLOrealEICJetPT2(i))
!      write(67,'(5(F18.6))') PT(i),ALLLOEICJetPT(i),ALLWWEICJetPT(i),
!     &   ALLNLOEICJetPT1(i),ALLNLOEICJetPT2(i)
!      end do
!      close(67)

!! ****** E155 Unpolarized & Polarized CS and asymmetry at fixed theta  **********************

!! ****** ep -> pip X ( theta = 2.75° ) ***************************************************

!      IH = 1
!      IC = 1
!      IT = 1

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(70,file='UUE155_275_p2pip.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2pip275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2pip275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2pip275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2pip275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2pip275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2pip275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2pip275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2pip275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2pip275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2pip275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2pip275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2pip275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(70,'(8(F12.6))') P(i),UULOE155p2pip275(i),
!     &                  UULOE155p2pip275min(i),UULOE155p2pip275max(i), 
!     &                       UUWWE155p2pip275(i),
!     & UULOE155p2pip275(i) + UUWWE155p2pip275(i) + 
!     & UUNLOvirE155p2pip275(i) + UUNLOrealE155p2pip275(i),
!     & UULOE155p2pip275min(i) + UUWWE155p2pip275min(i) + 
!     & UUNLOvirE155p2pip275min(i) + UUNLOrealE155p2pip275min(i),
!     & UULOE155p2pip275max(i) + UUWWE155p2pip275max(i) + 
!     & UUNLOvirE155p2pip275max(i) + UUNLOrealE155p2pip275max(i)
!      end do
!      close(70)

!      open(71,file='LLE155_275_p2pip.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2pip275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2pip275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2pip275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2pip275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2pip275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2pip275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2pip275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2pip275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2pip275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2pip275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2pip275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2pip275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(71,'(8(F12.6))') P(i),LLLOE155p2pip275(i),
!     &                  LLLOE155p2pip275min(i),LLLOE155p2pip275max(i), 
!     &                       LLWWE155p2pip275(i),
!     & LLLOE155p2pip275(i) + LLWWE155p2pip275(i) + 
!     & LLNLOvirE155p2pip275(i) + LLNLOrealE155p2pip275(i),
!     & LLLOE155p2pip275min(i) + LLWWE155p2pip275min(i) + 
!     & LLNLOvirE155p2pip275min(i) + LLNLOrealE155p2pip275min(i),
!     & LLLOE155p2pip275max(i) + LLWWE155p2pip275max(i) + 
!     & LLNLOvirE155p2pip275max(i) + LLNLOrealE155p2pip275max(i)
!      end do
!      close(71)

!      open(72,file='ALLE155_275_p2pip.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2pip275(i) = LLLOE155p2pip275(i)/UULOE155p2pip275(i)
!      ALLLOE155p2pip275min(i) = LLLOE155p2pip275min(i)/
!     &                                   UULOE155p2pip275min(i)
!      ALLLOE155p2pip275max(i) = LLLOE155p2pip275max(i)/
!     &                                   UULOE155p2pip275max(i)
!      ALLWWE155p2pip275(i) = LLWWE155p2pip275(i)/UUWWE155p2pip275(i)
!      
!      ALLNLOE155p2pip275(i) = (LLLOE155p2pip275(i) + LLWWE155p2pip275(i)
!     &            + LLNLOvirE155p2pip275(i) + LLNLOrealE155p2pip275(i))/
!     &           (UULOE155p2pip275(i) + UUWWE155p2pip275(i) + 
!     &            UUNLOvirE155p2pip275(i) + UUNLOrealE155p2pip275(i))
!      ALLNLOE155p2pip275min(i) = 
!     &          (LLLOE155p2pip275min(i) + LLWWE155p2pip275min(i)
!     &      + LLNLOvirE155p2pip275min(i) + LLNLOrealE155p2pip275min(i))/
!     &           (UULOE155p2pip275min(i) + UUWWE155p2pip275min(i) + 
!     &         UUNLOvirE155p2pip275min(i) + UUNLOrealE155p2pip275min(i))
!      ALLNLOE155p2pip275max(i) = 
!     &          (LLLOE155p2pip275max(i) + LLWWE155p2pip275max(i)
!     &      + LLNLOvirE155p2pip275max(i) + LLNLOrealE155p2pip275max(i))/
!     &           (UULOE155p2pip275max(i) + UUWWE155p2pip275max(i) + 
!     &         UUNLOvirE155p2pip275max(i) + UUNLOrealE155p2pip275max(i))
!      
!      write(72,'(8(F12.6))') P(i),ALLLOE155p2pip275(i),
!     &                  ALLLOE155p2pip275min(i),ALLLOE155p2pip275max(i), 
!     &                       ALLWWE155p2pip275(i),
!     & ALLNLOE155p2pip275(i),ALLNLOE155p2pip275min(i),
!     &   ALLNLOE155p2pip275max(i)
!      end do
!      close(72)

!! ****** ep -> pip X ( theta = 5.5° ) ***************************************************

!      IH = 1
!      IC = 1
!      IT = 1

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(80,file='UUE155_55_p2pip.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2pip55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2pip55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2pip55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2pip55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2pip55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2pip55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2pip55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2pip55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2pip55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2pip55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2pip55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2pip55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(80,'(8(F12.6))') P(i),UULOE155p2pip55(i),
!     &                  UULOE155p2pip55min(i),UULOE155p2pip55max(i), 
!     &                       UUWWE155p2pip55(i),
!     & UULOE155p2pip55(i) + UUWWE155p2pip55(i) + 
!     & UUNLOvirE155p2pip55(i) + UUNLOrealE155p2pip55(i),
!     & UULOE155p2pip55min(i) + UUWWE155p2pip55min(i) + 
!     & UUNLOvirE155p2pip55min(i) + UUNLOrealE155p2pip55min(i),
!     & UULOE155p2pip55max(i) + UUWWE155p2pip55max(i) + 
!     & UUNLOvirE155p2pip55max(i) + UUNLOrealE155p2pip55max(i)
!      end do
!      close(80)

!      open(81,file='LLE155_55_p2pip.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2pip55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2pip55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2pip55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2pip55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2pip55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2pip55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2pip55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2pip55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2pip55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2pip55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2pip55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2pip55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(81,'(8(F12.6))') P(i),LLLOE155p2pip55(i),
!     &                  LLLOE155p2pip55min(i),LLLOE155p2pip55max(i), 
!     &                       LLWWE155p2pip55(i),
!     & LLLOE155p2pip55(i) + LLWWE155p2pip55(i) + 
!     & LLNLOvirE155p2pip55(i) + LLNLOrealE155p2pip55(i),
!     & LLLOE155p2pip55min(i) + LLWWE155p2pip55min(i) + 
!     & LLNLOvirE155p2pip55min(i) + LLNLOrealE155p2pip55min(i),
!     & LLLOE155p2pip55max(i) + LLWWE155p2pip55max(i) + 
!     & LLNLOvirE155p2pip55max(i) + LLNLOrealE155p2pip55max(i)
!      end do
!      close(81)

!      open(82,file='ALLE155_55_p2pip.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2pip55(i) = LLLOE155p2pip55(i)/UULOE155p2pip55(i)
!      ALLLOE155p2pip55min(i) = LLLOE155p2pip55min(i)/
!     &                                   UULOE155p2pip55min(i)
!      ALLLOE155p2pip55max(i) = LLLOE155p2pip55max(i)/
!     &                                   UULOE155p2pip55max(i)
!      ALLWWE155p2pip55(i) = LLWWE155p2pip55(i)/UUWWE155p2pip55(i)
!      
!      ALLNLOE155p2pip55(i) = (LLLOE155p2pip55(i) + LLWWE155p2pip55(i)
!     &            + LLNLOvirE155p2pip55(i) + LLNLOrealE155p2pip55(i))/
!     &           (UULOE155p2pip55(i) + UUWWE155p2pip55(i) + 
!     &            UUNLOvirE155p2pip55(i) + UUNLOrealE155p2pip55(i))
!      ALLNLOE155p2pip55min(i) = 
!     &          (LLLOE155p2pip55min(i) + LLWWE155p2pip55min(i)
!     &      + LLNLOvirE155p2pip55min(i) + LLNLOrealE155p2pip55min(i))/
!     &           (UULOE155p2pip55min(i) + UUWWE155p2pip55min(i) + 
!     &         UUNLOvirE155p2pip55min(i) + UUNLOrealE155p2pip55min(i))
!      ALLNLOE155p2pip55max(i) = 
!     &          (LLLOE155p2pip55max(i) + LLWWE155p2pip55max(i)
!     &      + LLNLOvirE155p2pip55max(i) + LLNLOrealE155p2pip55max(i))/
!     &           (UULOE155p2pip55max(i) + UUWWE155p2pip55max(i) + 
!     &         UUNLOvirE155p2pip55max(i) + UUNLOrealE155p2pip55max(i))
!      
!      write(82,'(8(F12.6))') P(i),ALLLOE155p2pip55(i),
!     &                  ALLLOE155p2pip55min(i),ALLLOE155p2pip55max(i), 
!     &                       ALLWWE155p2pip55(i),
!     & ALLNLOE155p2pip55(i),ALLNLOE155p2pip55min(i),
!     &   ALLNLOE155p2pip55max(i)
!      end do
!      close(82)

!! ****** ep -> pim X ( theta = 2.75° ) ***************************************************

!      IH = 1
!      IC = -1
!      IT = 1

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(90,file='UUE155_275_p2pim.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2pim275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2pim275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2pim275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2pim275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2pim275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2pim275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2pim275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2pim275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2pim275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2pim275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2pim275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2pim275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(90,'(8(F12.6))') P(i),UULOE155p2pim275(i),
!     &                  UULOE155p2pim275min(i),UULOE155p2pim275max(i), 
!     &                       UUWWE155p2pim275(i),
!     & UULOE155p2pim275(i) + UUWWE155p2pim275(i) + 
!     & UUNLOvirE155p2pim275(i) + UUNLOrealE155p2pim275(i),
!     & UULOE155p2pim275min(i) + UUWWE155p2pim275min(i) + 
!     & UUNLOvirE155p2pim275min(i) + UUNLOrealE155p2pim275min(i),
!     & UULOE155p2pim275max(i) + UUWWE155p2pim275max(i) + 
!     & UUNLOvirE155p2pim275max(i) + UUNLOrealE155p2pim275max(i)
!      end do
!      close(90)

!      open(91,file='LLE155_275_p2pim.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2pim275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2pim275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2pim275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2pim275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2pim275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2pim275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2pim275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2pim275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2pim275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2pim275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2pim275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2pim275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(91,'(8(F12.6))') P(i),LLLOE155p2pim275(i),
!     &                  LLLOE155p2pim275min(i),LLLOE155p2pim275max(i), 
!     &                       LLWWE155p2pim275(i),
!     & LLLOE155p2pim275(i) + LLWWE155p2pim275(i) + 
!     & LLNLOvirE155p2pim275(i) + LLNLOrealE155p2pim275(i),
!     & LLLOE155p2pim275min(i) + LLWWE155p2pim275min(i) + 
!     & LLNLOvirE155p2pim275min(i) + LLNLOrealE155p2pim275min(i),
!     & LLLOE155p2pim275max(i) + LLWWE155p2pim275max(i) + 
!     & LLNLOvirE155p2pim275max(i) + LLNLOrealE155p2pim275max(i)
!      end do
!      close(91)

!      open(92,file='ALLE155_275_p2pim.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2pim275(i) = LLLOE155p2pim275(i)/UULOE155p2pim275(i)
!      ALLLOE155p2pim275min(i) = LLLOE155p2pim275min(i)/
!     &                                   UULOE155p2pim275min(i)
!      ALLLOE155p2pim275max(i) = LLLOE155p2pim275max(i)/
!     &                                   UULOE155p2pim275max(i)
!      ALLWWE155p2pim275(i) = LLWWE155p2pim275(i)/UUWWE155p2pim275(i)
!      
!      ALLNLOE155p2pim275(i) = (LLLOE155p2pim275(i) + LLWWE155p2pim275(i)
!     &            + LLNLOvirE155p2pim275(i) + LLNLOrealE155p2pim275(i))/
!     &           (UULOE155p2pim275(i) + UUWWE155p2pim275(i) + 
!     &            UUNLOvirE155p2pim275(i) + UUNLOrealE155p2pim275(i))
!      ALLNLOE155p2pim275min(i) = 
!     &          (LLLOE155p2pim275min(i) + LLWWE155p2pim275min(i)
!     &      + LLNLOvirE155p2pim275min(i) + LLNLOrealE155p2pim275min(i))/
!     &           (UULOE155p2pim275min(i) + UUWWE155p2pim275min(i) + 
!     &         UUNLOvirE155p2pim275min(i) + UUNLOrealE155p2pim275min(i))
!      ALLNLOE155p2pim275max(i) = 
!     &          (LLLOE155p2pim275max(i) + LLWWE155p2pim275max(i)
!     &      + LLNLOvirE155p2pim275max(i) + LLNLOrealE155p2pim275max(i))/
!     &           (UULOE155p2pim275max(i) + UUWWE155p2pim275max(i) + 
!     &         UUNLOvirE155p2pim275max(i) + UUNLOrealE155p2pim275max(i))
!      
!      write(92,'(8(F12.6))') P(i),ALLLOE155p2pim275(i),
!     &                  ALLLOE155p2pim275min(i),ALLLOE155p2pim275max(i), 
!     &                       ALLWWE155p2pim275(i),
!     & ALLNLOE155p2pim275(i),ALLNLOE155p2pim275min(i),
!     &   ALLNLOE155p2pim275max(i)
!      end do
!      close(92)

!! ****** ep -> pim X ( theta = 5.5° ) ***************************************************

!      IH = 1
!      IC = -1
!      IT = 1

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(100,file='UUE155_55_p2pim.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2pim55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2pim55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2pim55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2pim55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2pim55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2pim55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2pim55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2pim55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2pim55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2pim55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2pim55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2pim55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(100,'(8(F12.6))') P(i),UULOE155p2pim55(i),
!     &                  UULOE155p2pim55min(i),UULOE155p2pim55max(i), 
!     &                       UUWWE155p2pim55(i),
!     & UULOE155p2pim55(i) + UUWWE155p2pim55(i) + 
!     & UUNLOvirE155p2pim55(i) + UUNLOrealE155p2pim55(i),
!     & UULOE155p2pim55min(i) + UUWWE155p2pim55min(i) + 
!     & UUNLOvirE155p2pim55min(i) + UUNLOrealE155p2pim55min(i),
!     & UULOE155p2pim55max(i) + UUWWE155p2pim55max(i) + 
!     & UUNLOvirE155p2pim55max(i) + UUNLOrealE155p2pim55max(i)
!      end do
!      close(100)

!      open(101,file='LLE155_55_p2pim.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2pim55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2pim55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2pim55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2pim55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2pim55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2pim55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2pim55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2pim55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2pim55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2pim55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2pim55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2pim55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(101,'(8(F12.6))') P(i),LLLOE155p2pim55(i),
!     &                  LLLOE155p2pim55min(i),LLLOE155p2pim55max(i), 
!     &                       LLWWE155p2pim55(i),
!     & LLLOE155p2pim55(i) + LLWWE155p2pim55(i) + 
!     & LLNLOvirE155p2pim55(i) + LLNLOrealE155p2pim55(i),
!     & LLLOE155p2pim55min(i) + LLWWE155p2pim55min(i) + 
!     & LLNLOvirE155p2pim55min(i) + LLNLOrealE155p2pim55min(i),
!     & LLLOE155p2pim55max(i) + LLWWE155p2pim55max(i) + 
!     & LLNLOvirE155p2pim55max(i) + LLNLOrealE155p2pim55max(i)
!      end do
!      close(101)

!      open(102,file='ALLE155_55_p2pim.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2pim55(i) = LLLOE155p2pim55(i)/UULOE155p2pim55(i)
!      ALLLOE155p2pim55min(i) = LLLOE155p2pim55min(i)/
!     &                                   UULOE155p2pim55min(i)
!      ALLLOE155p2pim55max(i) = LLLOE155p2pim55max(i)/
!     &                                   UULOE155p2pim55max(i)
!      ALLWWE155p2pim55(i) = LLWWE155p2pim55(i)/UUWWE155p2pim55(i)
!      
!      ALLNLOE155p2pim55(i) = (LLLOE155p2pim55(i) + LLWWE155p2pim55(i)
!     &            + LLNLOvirE155p2pim55(i) + LLNLOrealE155p2pim55(i))/
!     &           (UULOE155p2pim55(i) + UUWWE155p2pim55(i) + 
!     &            UUNLOvirE155p2pim55(i) + UUNLOrealE155p2pim55(i))
!      ALLNLOE155p2pim55min(i) = 
!     &          (LLLOE155p2pim55min(i) + LLWWE155p2pim55min(i)
!     &      + LLNLOvirE155p2pim55min(i) + LLNLOrealE155p2pim55min(i))/
!     &           (UULOE155p2pim55min(i) + UUWWE155p2pim55min(i) + 
!     &         UUNLOvirE155p2pim55min(i) + UUNLOrealE155p2pim55min(i))
!      ALLNLOE155p2pim55max(i) = 
!     &          (LLLOE155p2pim55max(i) + LLWWE155p2pim55max(i)
!     &      + LLNLOvirE155p2pim55max(i) + LLNLOrealE155p2pim55max(i))/
!     &           (UULOE155p2pim55max(i) + UUWWE155p2pim55max(i) + 
!     &         UUNLOvirE155p2pim55max(i) + UUNLOrealE155p2pim55max(i))
!      
!      write(102,'(8(F12.6))') P(i),ALLLOE155p2pim55(i),
!     &                  ALLLOE155p2pim55min(i),ALLLOE155p2pim55max(i), 
!     &                       ALLWWE155p2pim55(i),
!     & ALLNLOE155p2pim55(i),ALLNLOE155p2pim55min(i),
!     &   ALLNLOE155p2pim55max(i)
!      end do
!      close(102)


!! ****** ep -> hp X ( theta = 2.75° ) ***************************************************

!      IH = 4
!      IC = 1
!      IT = 1

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(110,file='UUE155_275_p2hp.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2hp275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2hp275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2hp275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2hp275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2hp275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2hp275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2hp275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2hp275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2hp275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2hp275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2hp275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2hp275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(110,'(8(F12.6))') P(i),UULOE155p2hp275(i),
!     &                  UULOE155p2hp275min(i),UULOE155p2hp275max(i), 
!     &                       UUWWE155p2hp275(i),
!     & UULOE155p2hp275(i) + UUWWE155p2hp275(i) + 
!     & UUNLOvirE155p2hp275(i) + UUNLOrealE155p2hp275(i),
!     & UULOE155p2hp275min(i) + UUWWE155p2hp275min(i) + 
!     & UUNLOvirE155p2hp275min(i) + UUNLOrealE155p2hp275min(i),
!     & UULOE155p2hp275max(i) + UUWWE155p2hp275max(i) + 
!     & UUNLOvirE155p2hp275max(i) + UUNLOrealE155p2hp275max(i)
!      end do
!      close(110)

!      open(111,file='LLE155_275_p2hp.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2hp275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2hp275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2hp275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2hp275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2hp275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2hp275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2hp275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2hp275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2hp275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2hp275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2hp275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2hp275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(111,'(8(F12.6))') P(i),LLLOE155p2hp275(i),
!     &                  LLLOE155p2hp275min(i),LLLOE155p2hp275max(i), 
!     &                       LLWWE155p2hp275(i),
!     & LLLOE155p2hp275(i) + LLWWE155p2hp275(i) + 
!     & LLNLOvirE155p2hp275(i) + LLNLOrealE155p2hp275(i),
!     & LLLOE155p2hp275min(i) + LLWWE155p2hp275min(i) + 
!     & LLNLOvirE155p2hp275min(i) + LLNLOrealE155p2hp275min(i),
!     & LLLOE155p2hp275max(i) + LLWWE155p2hp275max(i) + 
!     & LLNLOvirE155p2hp275max(i) + LLNLOrealE155p2hp275max(i)
!      end do
!      close(111)

!      open(112,file='ALLE155_275_p2hp.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2hp275(i) = LLLOE155p2hp275(i)/UULOE155p2hp275(i)
!      ALLLOE155p2hp275min(i) = LLLOE155p2hp275min(i)/
!     &                                   UULOE155p2hp275min(i)
!      ALLLOE155p2hp275max(i) = LLLOE155p2hp275max(i)/
!     &                                   UULOE155p2hp275max(i)
!      ALLWWE155p2hp275(i) = LLWWE155p2hp275(i)/UUWWE155p2hp275(i)
!      
!      ALLNLOE155p2hp275(i) = (LLLOE155p2hp275(i) + LLWWE155p2hp275(i)
!     &            + LLNLOvirE155p2hp275(i) + LLNLOrealE155p2hp275(i))/
!     &           (UULOE155p2hp275(i) + UUWWE155p2hp275(i) + 
!     &            UUNLOvirE155p2hp275(i) + UUNLOrealE155p2hp275(i))
!      ALLNLOE155p2hp275min(i) = 
!     &          (LLLOE155p2hp275min(i) + LLWWE155p2hp275min(i)
!     &      + LLNLOvirE155p2hp275min(i) + LLNLOrealE155p2hp275min(i))/
!     &           (UULOE155p2hp275min(i) + UUWWE155p2hp275min(i) + 
!     &         UUNLOvirE155p2hp275min(i) + UUNLOrealE155p2hp275min(i))
!      ALLNLOE155p2hp275max(i) = 
!     &          (LLLOE155p2hp275max(i) + LLWWE155p2hp275max(i)
!     &      + LLNLOvirE155p2hp275max(i) + LLNLOrealE155p2hp275max(i))/
!     &           (UULOE155p2hp275max(i) + UUWWE155p2hp275max(i) + 
!     &         UUNLOvirE155p2hp275max(i) + UUNLOrealE155p2hp275max(i))
!      
!      write(112,'(8(F12.6))') P(i),ALLLOE155p2hp275(i),
!     &                  ALLLOE155p2hp275min(i),ALLLOE155p2hp275max(i), 
!     &                       ALLWWE155p2hp275(i),
!     & ALLNLOE155p2hp275(i),ALLNLOE155p2hp275min(i),
!     &   ALLNLOE155p2hp275max(i)
!      end do
!      close(112)

! ****** ep -> hp X ( theta = 5.5° ) ***************************************************

!      IH = 4
!      IC = 1
!      IT = 1
!      FINI = 0

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(120,file='UUE155_55_p2hpAux.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2hp55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2hp55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2hp55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2hp55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2hp55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2hp55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2hp55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2hp55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2hp55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2hp55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2hp55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2hp55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(120,'(8(F12.6))') P(i),UULOE155p2hp55(i),
!     &                  UULOE155p2hp55min(i),UULOE155p2hp55max(i), 
!     &                       UUWWE155p2hp55(i),
!     & UULOE155p2hp55(i) + UUWWE155p2hp55(i) + 
!     & UUNLOvirE155p2hp55(i) + UUNLOrealE155p2hp55(i),
!     & UULOE155p2hp55min(i) + UUWWE155p2hp55min(i) + 
!     & UUNLOvirE155p2hp55min(i) + UUNLOrealE155p2hp55min(i),
!     & UULOE155p2hp55max(i) + UUWWE155p2hp55max(i) + 
!     & UUNLOvirE155p2hp55max(i) + UUNLOrealE155p2hp55max(i)
!      end do
!      close(120)

!      open(121,file='LLE155_55_p2hp.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2hp55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2hp55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2hp55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2hp55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2hp55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2hp55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2hp55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2hp55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2hp55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2hp55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2hp55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2hp55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(121,'(8(F12.6))') P(i),LLLOE155p2hp55(i),
!     &                  LLLOE155p2hp55min(i),LLLOE155p2hp55max(i), 
!     &                       LLWWE155p2hp55(i),
!     & LLLOE155p2hp55(i) + LLWWE155p2hp55(i) + 
!     & LLNLOvirE155p2hp55(i) + LLNLOrealE155p2hp55(i),
!     & LLLOE155p2hp55min(i) + LLWWE155p2hp55min(i) + 
!     & LLNLOvirE155p2hp55min(i) + LLNLOrealE155p2hp55min(i),
!     & LLLOE155p2hp55max(i) + LLWWE155p2hp55max(i) + 
!     & LLNLOvirE155p2hp55max(i) + LLNLOrealE155p2hp55max(i)
!      end do
!      close(121)

!      open(122,file='ALLE155_55_p2hp.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2hp55(i) = LLLOE155p2hp55(i)/UULOE155p2hp55(i)
!      ALLLOE155p2hp55min(i) = LLLOE155p2hp55min(i)/
!     &                                   UULOE155p2hp55min(i)
!      ALLLOE155p2hp55max(i) = LLLOE155p2hp55max(i)/
!     &                                   UULOE155p2hp55max(i)
!      ALLWWE155p2hp55(i) = LLWWE155p2hp55(i)/UUWWE155p2hp55(i)
!      
!      ALLNLOE155p2hp55(i) = (LLLOE155p2hp55(i) + LLWWE155p2hp55(i)
!     &            + LLNLOvirE155p2hp55(i) + LLNLOrealE155p2hp55(i))/
!     &           (UULOE155p2hp55(i) + UUWWE155p2hp55(i) + 
!     &            UUNLOvirE155p2hp55(i) + UUNLOrealE155p2hp55(i))
!      ALLNLOE155p2hp55min(i) = 
!     &          (LLLOE155p2hp55min(i) + LLWWE155p2hp55min(i)
!     &      + LLNLOvirE155p2hp55min(i) + LLNLOrealE155p2hp55min(i))/
!     &           (UULOE155p2hp55min(i) + UUWWE155p2hp55min(i) + 
!     &         UUNLOvirE155p2hp55min(i) + UUNLOrealE155p2hp55min(i))
!      ALLNLOE155p2hp55max(i) = 
!     &          (LLLOE155p2hp55max(i) + LLWWE155p2hp55max(i)
!     &      + LLNLOvirE155p2hp55max(i) + LLNLOrealE155p2hp55max(i))/
!     &           (UULOE155p2hp55max(i) + UUWWE155p2hp55max(i) + 
!     &         UUNLOvirE155p2hp55max(i) + UUNLOrealE155p2hp55max(i))
!      
!      write(122,'(8(F12.6))') P(i),ALLLOE155p2hp55(i),
!     &                  ALLLOE155p2hp55min(i),ALLLOE155p2hp55max(i), 
!     &                       ALLWWE155p2hp55(i),
!     & ALLNLOE155p2hp55(i),ALLNLOE155p2hp55min(i),
!     &   ALLNLOE155p2hp55max(i)
!      end do
!      close(122)

!! ****** ep -> hm X ( theta = 2.75° ) ***************************************************

!      IH = 4
!      IC = -1
!      IT = 1

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(130,file='UUE155_275_p2hm.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2hm275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2hm275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2hm275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2hm275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2hm275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2hm275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2hm275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2hm275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2hm275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2hm275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2hm275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2hm275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(130,'(8(F12.6))') P(i),UULOE155p2hm275(i),
!     &                  UULOE155p2hm275min(i),UULOE155p2hm275max(i), 
!     &                       UUWWE155p2hm275(i),
!     & UULOE155p2hm275(i) + UUWWE155p2hm275(i) + 
!     & UUNLOvirE155p2hm275(i) + UUNLOrealE155p2hm275(i),
!     & UULOE155p2hm275min(i) + UUWWE155p2hm275min(i) + 
!     & UUNLOvirE155p2hm275min(i) + UUNLOrealE155p2hm275min(i),
!     & UULOE155p2hm275max(i) + UUWWE155p2hm275max(i) + 
!     & UUNLOvirE155p2hm275max(i) + UUNLOrealE155p2hm275max(i)
!      end do
!      close(130)

!      open(131,file='LLE155_275_p2hm.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2hm275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2hm275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2hm275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2hm275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2hm275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2hm275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2hm275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2hm275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2hm275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2hm275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2hm275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2hm275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(131,'(8(F12.6))') P(i),LLLOE155p2hm275(i),
!     &                  LLLOE155p2hm275min(i),LLLOE155p2hm275max(i), 
!     &                       LLWWE155p2hm275(i),
!     & LLLOE155p2hm275(i) + LLWWE155p2hm275(i) + 
!     & LLNLOvirE155p2hm275(i) + LLNLOrealE155p2hm275(i),
!     & LLLOE155p2hm275min(i) + LLWWE155p2hm275min(i) + 
!     & LLNLOvirE155p2hm275min(i) + LLNLOrealE155p2hm275min(i),
!     & LLLOE155p2hm275max(i) + LLWWE155p2hm275max(i) + 
!     & LLNLOvirE155p2hm275max(i) + LLNLOrealE155p2hm275max(i)
!      end do
!      close(131)

!      open(132,file='ALLE155_275_p2hm.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2hm275(i) = LLLOE155p2hm275(i)/UULOE155p2hm275(i)
!      ALLLOE155p2hm275min(i) = LLLOE155p2hm275min(i)/
!     &                                   UULOE155p2hm275min(i)
!      ALLLOE155p2hm275max(i) = LLLOE155p2hm275max(i)/
!     &                                   UULOE155p2hm275max(i)
!      ALLWWE155p2hm275(i) = LLWWE155p2hm275(i)/UUWWE155p2hm275(i)
!      
!      ALLNLOE155p2hm275(i) = (LLLOE155p2hm275(i) + LLWWE155p2hm275(i)
!     &            + LLNLOvirE155p2hm275(i) + LLNLOrealE155p2hm275(i))/
!     &           (UULOE155p2hm275(i) + UUWWE155p2hm275(i) + 
!     &            UUNLOvirE155p2hm275(i) + UUNLOrealE155p2hm275(i))
!      ALLNLOE155p2hm275min(i) = 
!     &          (LLLOE155p2hm275min(i) + LLWWE155p2hm275min(i)
!     &      + LLNLOvirE155p2hm275min(i) + LLNLOrealE155p2hm275min(i))/
!     &           (UULOE155p2hm275min(i) + UUWWE155p2hm275min(i) + 
!     &         UUNLOvirE155p2hm275min(i) + UUNLOrealE155p2hm275min(i))
!      ALLNLOE155p2hm275max(i) = 
!     &          (LLLOE155p2hm275max(i) + LLWWE155p2hm275max(i)
!     &      + LLNLOvirE155p2hm275max(i) + LLNLOrealE155p2hm275max(i))/
!     &           (UULOE155p2hm275max(i) + UUWWE155p2hm275max(i) + 
!     &         UUNLOvirE155p2hm275max(i) + UUNLOrealE155p2hm275max(i))
!      
!      write(132,'(8(F12.6))') P(i),ALLLOE155p2hm275(i),
!     &                  ALLLOE155p2hm275min(i),ALLLOE155p2hm275max(i), 
!     &                       ALLWWE155p2hm275(i),
!     & ALLNLOE155p2hm275(i),ALLNLOE155p2hm275min(i),
!     &   ALLNLOE155p2hm275max(i)
!      end do
!      close(132)

!! ****** ep -> hm X ( theta = 5.5° ) ***************************************************

!      IH = 4
!      IC = -1
!      IT = 1

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(140,file='UUE155_55_p2hm.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155p2hm55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155p2hm55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155p2hm55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155p2hm55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155p2hm55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155p2hm55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155p2hm55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155p2hm55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155p2hm55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155p2hm55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155p2hm55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155p2hm55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(140,'(8(F12.6))') P(i),UULOE155p2hm55(i),
!     &                  UULOE155p2hm55min(i),UULOE155p2hm55max(i), 
!     &                       UUWWE155p2hm55(i),
!     & UULOE155p2hm55(i) + UUWWE155p2hm55(i) + 
!     & UUNLOvirE155p2hm55(i) + UUNLOrealE155p2hm55(i),
!     & UULOE155p2hm55min(i) + UUWWE155p2hm55min(i) + 
!     & UUNLOvirE155p2hm55min(i) + UUNLOrealE155p2hm55min(i),
!     & UULOE155p2hm55max(i) + UUWWE155p2hm55max(i) + 
!     & UUNLOvirE155p2hm55max(i) + UUNLOrealE155p2hm55max(i)
!      end do
!      close(140)

!      open(141,file='LLE155_55_p2hm.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155p2hm55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155p2hm55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155p2hm55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155p2hm55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155p2hm55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155p2hm55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155p2hm55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155p2hm55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155p2hm55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155p2hm55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155p2hm55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155p2hm55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(141,'(8(F12.6))') P(i),LLLOE155p2hm55(i),
!     &                  LLLOE155p2hm55min(i),LLLOE155p2hm55max(i), 
!     &                       LLWWE155p2hm55(i),
!     & LLLOE155p2hm55(i) + LLWWE155p2hm55(i) + 
!     & LLNLOvirE155p2hm55(i) + LLNLOrealE155p2hm55(i),
!     & LLLOE155p2hm55min(i) + LLWWE155p2hm55min(i) + 
!     & LLNLOvirE155p2hm55min(i) + LLNLOrealE155p2hm55min(i),
!     & LLLOE155p2hm55max(i) + LLWWE155p2hm55max(i) + 
!     & LLNLOvirE155p2hm55max(i) + LLNLOrealE155p2hm55max(i)
!      end do
!      close(141)

!      open(142,file='ALLE155_55_p2hm.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155p2hm55(i) = LLLOE155p2hm55(i)/UULOE155p2hm55(i)
!      ALLLOE155p2hm55min(i) = LLLOE155p2hm55min(i)/
!     &                                   UULOE155p2hm55min(i)
!      ALLLOE155p2hm55max(i) = LLLOE155p2hm55max(i)/
!     &                                   UULOE155p2hm55max(i)
!      ALLWWE155p2hm55(i) = LLWWE155p2hm55(i)/UUWWE155p2hm55(i)
!      
!      ALLNLOE155p2hm55(i) = (LLLOE155p2hm55(i) + LLWWE155p2hm55(i)
!     &            + LLNLOvirE155p2hm55(i) + LLNLOrealE155p2hm55(i))/
!     &           (UULOE155p2hm55(i) + UUWWE155p2hm55(i) + 
!     &            UUNLOvirE155p2hm55(i) + UUNLOrealE155p2hm55(i))
!      ALLNLOE155p2hm55min(i) = 
!     &          (LLLOE155p2hm55min(i) + LLWWE155p2hm55min(i)
!     &      + LLNLOvirE155p2hm55min(i) + LLNLOrealE155p2hm55min(i))/
!     &           (UULOE155p2hm55min(i) + UUWWE155p2hm55min(i) + 
!     &         UUNLOvirE155p2hm55min(i) + UUNLOrealE155p2hm55min(i))
!      ALLNLOE155p2hm55max(i) = 
!     &          (LLLOE155p2hm55max(i) + LLWWE155p2hm55max(i)
!     &      + LLNLOvirE155p2hm55max(i) + LLNLOrealE155p2hm55max(i))/
!     &           (UULOE155p2hm55max(i) + UUWWE155p2hm55max(i) + 
!     &         UUNLOvirE155p2hm55max(i) + UUNLOrealE155p2hm55max(i))
!      
!      write(142,'(8(F12.6))') P(i),ALLLOE155p2hm55(i),
!     &                  ALLLOE155p2hm55min(i),ALLLOE155p2hm55max(i), 
!     &                       ALLWWE155p2hm55(i),
!     & ALLNLOE155p2hm55(i),ALLNLOE155p2hm55min(i),
!     &   ALLNLOE155p2hm55max(i)
!      end do
!      close(142)

!! ****** eD -> pip X ( theta = 2.75° ) ***************************************************

!      IH = 1
!      IC = 1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(150,file='UUE155_275_D2pip.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2pip275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2pip275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2pip275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2pip275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2pip275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2pip275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2pip275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2pip275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2pip275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2pip275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2pip275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2pip275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(150,'(8(F12.6))') P(i),UULOE155D2pip275(i),
!     &                  UULOE155D2pip275min(i),UULOE155D2pip275max(i), 
!     &                       UUWWE155D2pip275(i),
!     & UULOE155D2pip275(i) + UUWWE155D2pip275(i) + 
!     & UUNLOvirE155D2pip275(i) + UUNLOrealE155D2pip275(i),
!     & UULOE155D2pip275min(i) + UUWWE155D2pip275min(i) + 
!     & UUNLOvirE155D2pip275min(i) + UUNLOrealE155D2pip275min(i),
!     & UULOE155D2pip275max(i) + UUWWE155D2pip275max(i) + 
!     & UUNLOvirE155D2pip275max(i) + UUNLOrealE155D2pip275max(i)
!      end do
!      close(150)

!      open(151,file='LLE155_275_D2pip.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2pip275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2pip275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2pip275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2pip275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2pip275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2pip275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2pip275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2pip275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2pip275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2pip275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2pip275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2pip275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(151,'(8(F12.6))') P(i),LLLOE155D2pip275(i),
!     &                  LLLOE155D2pip275min(i),LLLOE155D2pip275max(i), 
!     &                       LLWWE155D2pip275(i),
!     & LLLOE155D2pip275(i) + LLWWE155D2pip275(i) + 
!     & LLNLOvirE155D2pip275(i) + LLNLOrealE155D2pip275(i),
!     & LLLOE155D2pip275min(i) + LLWWE155D2pip275min(i) + 
!     & LLNLOvirE155D2pip275min(i) + LLNLOrealE155D2pip275min(i),
!     & LLLOE155D2pip275max(i) + LLWWE155D2pip275max(i) + 
!     & LLNLOvirE155D2pip275max(i) + LLNLOrealE155D2pip275max(i)
!      end do
!      close(151)

!      open(152,file='ALLE155_275_D2pip.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2pip275(i) = LLLOE155D2pip275(i)/UULOE155D2pip275(i)
!      ALLLOE155D2pip275min(i) = LLLOE155D2pip275min(i)/
!     &                                   UULOE155D2pip275min(i)
!      ALLLOE155D2pip275max(i) = LLLOE155D2pip275max(i)/
!     &                                   UULOE155D2pip275max(i)
!      ALLWWE155D2pip275(i) = LLWWE155D2pip275(i)/UUWWE155D2pip275(i)
!      
!      ALLNLOE155D2pip275(i) = (LLLOE155D2pip275(i) + LLWWE155D2pip275(i)
!     &            + LLNLOvirE155D2pip275(i) + LLNLOrealE155D2pip275(i))/
!     &           (UULOE155D2pip275(i) + UUWWE155D2pip275(i) + 
!     &            UUNLOvirE155D2pip275(i) + UUNLOrealE155D2pip275(i))
!      ALLNLOE155D2pip275min(i) = 
!     &          (LLLOE155D2pip275min(i) + LLWWE155D2pip275min(i)
!     &      + LLNLOvirE155D2pip275min(i) + LLNLOrealE155D2pip275min(i))/
!     &           (UULOE155D2pip275min(i) + UUWWE155D2pip275min(i) + 
!     &         UUNLOvirE155D2pip275min(i) + UUNLOrealE155D2pip275min(i))
!      ALLNLOE155D2pip275max(i) = 
!     &          (LLLOE155D2pip275max(i) + LLWWE155D2pip275max(i)
!     &      + LLNLOvirE155D2pip275max(i) + LLNLOrealE155D2pip275max(i))/
!     &           (UULOE155D2pip275max(i) + UUWWE155D2pip275max(i) + 
!     &         UUNLOvirE155D2pip275max(i) + UUNLOrealE155D2pip275max(i))
!      
!      write(152,'(8(F12.6))') P(i),ALLLOE155D2pip275(i),
!     &                  ALLLOE155D2pip275min(i),ALLLOE155D2pip275max(i), 
!     &                       ALLWWE155D2pip275(i),
!     & ALLNLOE155D2pip275(i),ALLNLOE155D2pip275min(i),
!     &   ALLNLOE155D2pip275max(i)
!      end do
!      close(152)

!! ****** eD -> pip X ( theta = 5.5° ) ***************************************************

!      IH = 1
!      IC = 1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(160,file='UUE155_55_D2pip.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2pip55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2pip55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2pip55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2pip55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2pip55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2pip55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2pip55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2pip55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2pip55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2pip55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2pip55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2pip55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(160,'(8(F12.6))') P(i),UULOE155D2pip55(i),
!     &                  UULOE155D2pip55min(i),UULOE155D2pip55max(i), 
!     &                       UUWWE155D2pip55(i),
!     & UULOE155D2pip55(i) + UUWWE155D2pip55(i) + 
!     & UUNLOvirE155D2pip55(i) + UUNLOrealE155D2pip55(i),
!     & UULOE155D2pip55min(i) + UUWWE155D2pip55min(i) + 
!     & UUNLOvirE155D2pip55min(i) + UUNLOrealE155D2pip55min(i),
!     & UULOE155D2pip55max(i) + UUWWE155D2pip55max(i) + 
!     & UUNLOvirE155D2pip55max(i) + UUNLOrealE155D2pip55max(i)
!      end do
!      close(160)

!      open(161,file='LLE155_55_D2pip.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2pip55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2pip55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2pip55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2pip55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2pip55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2pip55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2pip55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2pip55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2pip55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2pip55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2pip55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2pip55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(161,'(8(F12.6))') P(i),LLLOE155D2pip55(i),
!     &                  LLLOE155D2pip55min(i),LLLOE155D2pip55max(i), 
!     &                       LLWWE155D2pip55(i),
!     & LLLOE155D2pip55(i) + LLWWE155D2pip55(i) + 
!     & LLNLOvirE155D2pip55(i) + LLNLOrealE155D2pip55(i),
!     & LLLOE155D2pip55min(i) + LLWWE155D2pip55min(i) + 
!     & LLNLOvirE155D2pip55min(i) + LLNLOrealE155D2pip55min(i),
!     & LLLOE155D2pip55max(i) + LLWWE155D2pip55max(i) + 
!     & LLNLOvirE155D2pip55max(i) + LLNLOrealE155D2pip55max(i)
!      end do
!      close(161)

!      open(162,file='ALLE155_55_D2pip.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2pip55(i) = LLLOE155D2pip55(i)/UULOE155D2pip55(i)
!      ALLLOE155D2pip55min(i) = LLLOE155D2pip55min(i)/
!     &                                   UULOE155D2pip55min(i)
!      ALLLOE155D2pip55max(i) = LLLOE155D2pip55max(i)/
!     &                                   UULOE155D2pip55max(i)
!      ALLWWE155D2pip55(i) = LLWWE155D2pip55(i)/UUWWE155D2pip55(i)
!      
!      ALLNLOE155D2pip55(i) = (LLLOE155D2pip55(i) + LLWWE155D2pip55(i)
!     &            + LLNLOvirE155D2pip55(i) + LLNLOrealE155D2pip55(i))/
!     &           (UULOE155D2pip55(i) + UUWWE155D2pip55(i) + 
!     &            UUNLOvirE155D2pip55(i) + UUNLOrealE155D2pip55(i))
!      ALLNLOE155D2pip55min(i) = 
!     &          (LLLOE155D2pip55min(i) + LLWWE155D2pip55min(i)
!     &      + LLNLOvirE155D2pip55min(i) + LLNLOrealE155D2pip55min(i))/
!     &           (UULOE155D2pip55min(i) + UUWWE155D2pip55min(i) + 
!     &         UUNLOvirE155D2pip55min(i) + UUNLOrealE155D2pip55min(i))
!      ALLNLOE155D2pip55max(i) = 
!     &          (LLLOE155D2pip55max(i) + LLWWE155D2pip55max(i)
!     &      + LLNLOvirE155D2pip55max(i) + LLNLOrealE155D2pip55max(i))/
!     &           (UULOE155D2pip55max(i) + UUWWE155D2pip55max(i) + 
!     &         UUNLOvirE155D2pip55max(i) + UUNLOrealE155D2pip55max(i))
!      
!      write(162,'(8(F12.6))') P(i),ALLLOE155D2pip55(i),
!     &                  ALLLOE155D2pip55min(i),ALLLOE155D2pip55max(i), 
!     &                       ALLWWE155D2pip55(i),
!     & ALLNLOE155D2pip55(i),ALLNLOE155D2pip55min(i),
!     &   ALLNLOE155D2pip55max(i)
!      end do
!      close(162)

!! ****** eD -> pim X ( theta = 2.75° ) ***************************************************

!      IH = 1
!      IC = -1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(170,file='UUE155_275_D2pim.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2pim275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2pim275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2pim275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2pim275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2pim275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2pim275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2pim275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2pim275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2pim275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2pim275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2pim275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2pim275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(170,'(8(F12.6))') P(i),UULOE155D2pim275(i),
!     &                  UULOE155D2pim275min(i),UULOE155D2pim275max(i), 
!     &                       UUWWE155D2pim275(i),
!     & UULOE155D2pim275(i) + UUWWE155D2pim275(i) + 
!     & UUNLOvirE155D2pim275(i) + UUNLOrealE155D2pim275(i),
!     & UULOE155D2pim275min(i) + UUWWE155D2pim275min(i) + 
!     & UUNLOvirE155D2pim275min(i) + UUNLOrealE155D2pim275min(i),
!     & UULOE155D2pim275max(i) + UUWWE155D2pim275max(i) + 
!     & UUNLOvirE155D2pim275max(i) + UUNLOrealE155D2pim275max(i)
!      end do
!      close(170)

!      open(171,file='LLE155_275_D2pim.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2pim275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2pim275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2pim275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2pim275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2pim275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2pim275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2pim275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2pim275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2pim275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2pim275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2pim275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2pim275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(171,'(8(F12.6))') P(i),LLLOE155D2pim275(i),
!     &                  LLLOE155D2pim275min(i),LLLOE155D2pim275max(i), 
!     &                       LLWWE155D2pim275(i),
!     & LLLOE155D2pim275(i) + LLWWE155D2pim275(i) + 
!     & LLNLOvirE155D2pim275(i) + LLNLOrealE155D2pim275(i),
!     & LLLOE155D2pim275min(i) + LLWWE155D2pim275min(i) + 
!     & LLNLOvirE155D2pim275min(i) + LLNLOrealE155D2pim275min(i),
!     & LLLOE155D2pim275max(i) + LLWWE155D2pim275max(i) + 
!     & LLNLOvirE155D2pim275max(i) + LLNLOrealE155D2pim275max(i)
!      end do
!      close(171)

!      open(172,file='ALLE155_275_D2pim.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2pim275(i) = LLLOE155D2pim275(i)/UULOE155D2pim275(i)
!      ALLLOE155D2pim275min(i) = LLLOE155D2pim275min(i)/
!     &                                   UULOE155D2pim275min(i)
!      ALLLOE155D2pim275max(i) = LLLOE155D2pim275max(i)/
!     &                                   UULOE155D2pim275max(i)
!      ALLWWE155D2pim275(i) = LLWWE155D2pim275(i)/UUWWE155D2pim275(i)
!      
!      ALLNLOE155D2pim275(i) = (LLLOE155D2pim275(i) + LLWWE155D2pim275(i)
!     &            + LLNLOvirE155D2pim275(i) + LLNLOrealE155D2pim275(i))/
!     &           (UULOE155D2pim275(i) + UUWWE155D2pim275(i) + 
!     &            UUNLOvirE155D2pim275(i) + UUNLOrealE155D2pim275(i))
!      ALLNLOE155D2pim275min(i) = 
!     &          (LLLOE155D2pim275min(i) + LLWWE155D2pim275min(i)
!     &      + LLNLOvirE155D2pim275min(i) + LLNLOrealE155D2pim275min(i))/
!     &           (UULOE155D2pim275min(i) + UUWWE155D2pim275min(i) + 
!     &         UUNLOvirE155D2pim275min(i) + UUNLOrealE155D2pim275min(i))
!      ALLNLOE155D2pim275max(i) = 
!     &          (LLLOE155D2pim275max(i) + LLWWE155D2pim275max(i)
!     &      + LLNLOvirE155D2pim275max(i) + LLNLOrealE155D2pim275max(i))/
!     &           (UULOE155D2pim275max(i) + UUWWE155D2pim275max(i) + 
!     &         UUNLOvirE155D2pim275max(i) + UUNLOrealE155D2pim275max(i))
!      
!      write(172,'(8(F12.6))') P(i),ALLLOE155D2pim275(i),
!     &                  ALLLOE155D2pim275min(i),ALLLOE155D2pim275max(i), 
!     &                       ALLWWE155D2pim275(i),
!     & ALLNLOE155D2pim275(i),ALLNLOE155D2pim275min(i),
!     &   ALLNLOE155D2pim275max(i)
!      end do
!      close(172)

!! ****** eD -> pim X ( theta = 5.5° ) ***************************************************

!      IH = 1
!      IC = -1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(180,file='UUE155_55_D2pim.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2pim55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2pim55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2pim55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2pim55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2pim55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2pim55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2pim55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2pim55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2pim55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2pim55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2pim55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2pim55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(180,'(8(F12.6))') P(i),UULOE155D2pim55(i),
!     &                  UULOE155D2pim55min(i),UULOE155D2pim55max(i), 
!     &                       UUWWE155D2pim55(i),
!     & UULOE155D2pim55(i) + UUWWE155D2pim55(i) + 
!     & UUNLOvirE155D2pim55(i) + UUNLOrealE155D2pim55(i),
!     & UULOE155D2pim55min(i) + UUWWE155D2pim55min(i) + 
!     & UUNLOvirE155D2pim55min(i) + UUNLOrealE155D2pim55min(i),
!     & UULOE155D2pim55max(i) + UUWWE155D2pim55max(i) + 
!     & UUNLOvirE155D2pim55max(i) + UUNLOrealE155D2pim55max(i)
!      end do
!      close(180)

!      open(181,file='LLE155_55_D2pim.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2pim55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2pim55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2pim55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2pim55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2pim55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2pim55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2pim55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2pim55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2pim55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2pim55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2pim55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2pim55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(181,'(8(F12.6))') P(i),LLLOE155D2pim55(i),
!     &                  LLLOE155D2pim55min(i),LLLOE155D2pim55max(i), 
!     &                       LLWWE155D2pim55(i),
!     & LLLOE155D2pim55(i) + LLWWE155D2pim55(i) + 
!     & LLNLOvirE155D2pim55(i) + LLNLOrealE155D2pim55(i),
!     & LLLOE155D2pim55min(i) + LLWWE155D2pim55min(i) + 
!     & LLNLOvirE155D2pim55min(i) + LLNLOrealE155D2pim55min(i),
!     & LLLOE155D2pim55max(i) + LLWWE155D2pim55max(i) + 
!     & LLNLOvirE155D2pim55max(i) + LLNLOrealE155D2pim55max(i)
!      end do
!      close(181)

!      open(182,file='ALLE155_55_D2pim.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2pim55(i) = LLLOE155D2pim55(i)/UULOE155D2pim55(i)
!      ALLLOE155D2pim55min(i) = LLLOE155D2pim55min(i)/
!     &                                   UULOE155D2pim55min(i)
!      ALLLOE155D2pim55max(i) = LLLOE155D2pim55max(i)/
!     &                                   UULOE155D2pim55max(i)
!      ALLWWE155D2pim55(i) = LLWWE155D2pim55(i)/UUWWE155D2pim55(i)
!      
!      ALLNLOE155D2pim55(i) = (LLLOE155D2pim55(i) + LLWWE155D2pim55(i)
!     &            + LLNLOvirE155D2pim55(i) + LLNLOrealE155D2pim55(i))/
!     &           (UULOE155D2pim55(i) + UUWWE155D2pim55(i) + 
!     &            UUNLOvirE155D2pim55(i) + UUNLOrealE155D2pim55(i))
!      ALLNLOE155D2pim55min(i) = 
!     &          (LLLOE155D2pim55min(i) + LLWWE155D2pim55min(i)
!     &      + LLNLOvirE155D2pim55min(i) + LLNLOrealE155D2pim55min(i))/
!     &           (UULOE155D2pim55min(i) + UUWWE155D2pim55min(i) + 
!     &         UUNLOvirE155D2pim55min(i) + UUNLOrealE155D2pim55min(i))
!      ALLNLOE155D2pim55max(i) = 
!     &          (LLLOE155D2pim55max(i) + LLWWE155D2pim55max(i)
!     &      + LLNLOvirE155D2pim55max(i) + LLNLOrealE155D2pim55max(i))/
!     &           (UULOE155D2pim55max(i) + UUWWE155D2pim55max(i) + 
!     &         UUNLOvirE155D2pim55max(i) + UUNLOrealE155D2pim55max(i))
!      
!      write(182,'(8(F12.6))') P(i),ALLLOE155D2pim55(i),
!     &                  ALLLOE155D2pim55min(i),ALLLOE155D2pim55max(i), 
!     &                       ALLWWE155D2pim55(i),
!     & ALLNLOE155D2pim55(i),ALLNLOE155D2pim55min(i),
!     &   ALLNLOE155D2pim55max(i)
!      end do
!      close(182)

!! ****** eD -> hp X ( theta = 2.75° ) ***************************************************

!      IH = 4
!      IC = 1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(190,file='UUE155_275_D2hp.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2hp275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2hp275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2hp275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2hp275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2hp275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2hp275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2hp275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2hp275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2hp275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2hp275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2hp275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2hp275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(190,'(8(F12.6))') P(i),UULOE155D2hp275(i),
!     &                  UULOE155D2hp275min(i),UULOE155D2hp275max(i), 
!     &                       UUWWE155D2hp275(i),
!     & UULOE155D2hp275(i) + UUWWE155D2hp275(i) + 
!     & UUNLOvirE155D2hp275(i) + UUNLOrealE155D2hp275(i),
!     & UULOE155D2hp275min(i) + UUWWE155D2hp275min(i) + 
!     & UUNLOvirE155D2hp275min(i) + UUNLOrealE155D2hp275min(i),
!     & UULOE155D2hp275max(i) + UUWWE155D2hp275max(i) + 
!     & UUNLOvirE155D2hp275max(i) + UUNLOrealE155D2hp275max(i)
!      end do
!      close(190)

!      open(191,file='LLE155_275_D2hp.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2hp275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2hp275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2hp275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2hp275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2hp275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2hp275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2hp275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2hp275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2hp275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2hp275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2hp275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2hp275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(191,'(8(F12.6))') P(i),LLLOE155D2hp275(i),
!     &                  LLLOE155D2hp275min(i),LLLOE155D2hp275max(i), 
!     &                       LLWWE155D2hp275(i),
!     & LLLOE155D2hp275(i) + LLWWE155D2hp275(i) + 
!     & LLNLOvirE155D2hp275(i) + LLNLOrealE155D2hp275(i),
!     & LLLOE155D2hp275min(i) + LLWWE155D2hp275min(i) + 
!     & LLNLOvirE155D2hp275min(i) + LLNLOrealE155D2hp275min(i),
!     & LLLOE155D2hp275max(i) + LLWWE155D2hp275max(i) + 
!     & LLNLOvirE155D2hp275max(i) + LLNLOrealE155D2hp275max(i)
!      end do
!      close(191)

!      open(192,file='ALLE155_275_D2hp.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2hp275(i) = LLLOE155D2hp275(i)/UULOE155D2hp275(i)
!      ALLLOE155D2hp275min(i) = LLLOE155D2hp275min(i)/
!     &                                   UULOE155D2hp275min(i)
!      ALLLOE155D2hp275max(i) = LLLOE155D2hp275max(i)/
!     &                                   UULOE155D2hp275max(i)
!      ALLWWE155D2hp275(i) = LLWWE155D2hp275(i)/UUWWE155D2hp275(i)
!      
!      ALLNLOE155D2hp275(i) = (LLLOE155D2hp275(i) + LLWWE155D2hp275(i)
!     &            + LLNLOvirE155D2hp275(i) + LLNLOrealE155D2hp275(i))/
!     &           (UULOE155D2hp275(i) + UUWWE155D2hp275(i) + 
!     &            UUNLOvirE155D2hp275(i) + UUNLOrealE155D2hp275(i))
!      ALLNLOE155D2hp275min(i) = 
!     &          (LLLOE155D2hp275min(i) + LLWWE155D2hp275min(i)
!     &      + LLNLOvirE155D2hp275min(i) + LLNLOrealE155D2hp275min(i))/
!     &           (UULOE155D2hp275min(i) + UUWWE155D2hp275min(i) + 
!     &         UUNLOvirE155D2hp275min(i) + UUNLOrealE155D2hp275min(i))
!      ALLNLOE155D2hp275max(i) = 
!     &          (LLLOE155D2hp275max(i) + LLWWE155D2hp275max(i)
!     &      + LLNLOvirE155D2hp275max(i) + LLNLOrealE155D2hp275max(i))/
!     &           (UULOE155D2hp275max(i) + UUWWE155D2hp275max(i) + 
!     &         UUNLOvirE155D2hp275max(i) + UUNLOrealE155D2hp275max(i))
!      
!      write(192,'(8(F12.6))') P(i),ALLLOE155D2hp275(i),
!     &                  ALLLOE155D2hp275min(i),ALLLOE155D2hp275max(i), 
!     &                       ALLWWE155D2hp275(i),
!     & ALLNLOE155D2hp275(i),ALLNLOE155D2hp275min(i),
!     &   ALLNLOE155D2hp275max(i)
!      end do
!      close(192)

!! ****** eD -> hp X ( theta = 5.5° ) ***************************************************

!      IH = 4
!      IC = 1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(200,file='UUE155_55_D2hp.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2hp55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2hp55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2hp55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2hp55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2hp55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2hp55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2hp55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2hp55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2hp55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2hp55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2hp55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2hp55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(200,'(8(F12.6))') P(i),UULOE155D2hp55(i),
!     &                  UULOE155D2hp55min(i),UULOE155D2hp55max(i), 
!     &                       UUWWE155D2hp55(i),
!     & UULOE155D2hp55(i) + UUWWE155D2hp55(i) + 
!     & UUNLOvirE155D2hp55(i) + UUNLOrealE155D2hp55(i),
!     & UULOE155D2hp55min(i) + UUWWE155D2hp55min(i) + 
!     & UUNLOvirE155D2hp55min(i) + UUNLOrealE155D2hp55min(i),
!     & UULOE155D2hp55max(i) + UUWWE155D2hp55max(i) + 
!     & UUNLOvirE155D2hp55max(i) + UUNLOrealE155D2hp55max(i)
!      end do
!      close(200)

!      open(201,file='LLE155_55_D2hp.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2hp55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2hp55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2hp55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2hp55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2hp55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2hp55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2hp55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2hp55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2hp55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2hp55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2hp55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2hp55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(201,'(8(F12.6))') P(i),LLLOE155D2hp55(i),
!     &                  LLLOE155D2hp55min(i),LLLOE155D2hp55max(i), 
!     &                       LLWWE155D2hp55(i),
!     & LLLOE155D2hp55(i) + LLWWE155D2hp55(i) + 
!     & LLNLOvirE155D2hp55(i) + LLNLOrealE155D2hp55(i),
!     & LLLOE155D2hp55min(i) + LLWWE155D2hp55min(i) + 
!     & LLNLOvirE155D2hp55min(i) + LLNLOrealE155D2hp55min(i),
!     & LLLOE155D2hp55max(i) + LLWWE155D2hp55max(i) + 
!     & LLNLOvirE155D2hp55max(i) + LLNLOrealE155D2hp55max(i)
!      end do
!      close(201)

!      open(202,file='ALLE155_55_D2hp.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2hp55(i) = LLLOE155D2hp55(i)/UULOE155D2hp55(i)
!      ALLLOE155D2hp55min(i) = LLLOE155D2hp55min(i)/
!     &                                   UULOE155D2hp55min(i)
!      ALLLOE155D2hp55max(i) = LLLOE155D2hp55max(i)/
!     &                                   UULOE155D2hp55max(i)
!      ALLWWE155D2hp55(i) = LLWWE155D2hp55(i)/UUWWE155D2hp55(i)
!      
!      ALLNLOE155D2hp55(i) = (LLLOE155D2hp55(i) + LLWWE155D2hp55(i)
!     &            + LLNLOvirE155D2hp55(i) + LLNLOrealE155D2hp55(i))/
!     &           (UULOE155D2hp55(i) + UUWWE155D2hp55(i) + 
!     &            UUNLOvirE155D2hp55(i) + UUNLOrealE155D2hp55(i))
!      ALLNLOE155D2hp55min(i) = 
!     &          (LLLOE155D2hp55min(i) + LLWWE155D2hp55min(i)
!     &      + LLNLOvirE155D2hp55min(i) + LLNLOrealE155D2hp55min(i))/
!     &           (UULOE155D2hp55min(i) + UUWWE155D2hp55min(i) + 
!     &         UUNLOvirE155D2hp55min(i) + UUNLOrealE155D2hp55min(i))
!      ALLNLOE155D2hp55max(i) = 
!     &          (LLLOE155D2hp55max(i) + LLWWE155D2hp55max(i)
!     &      + LLNLOvirE155D2hp55max(i) + LLNLOrealE155D2hp55max(i))/
!     &           (UULOE155D2hp55max(i) + UUWWE155D2hp55max(i) + 
!     &         UUNLOvirE155D2hp55max(i) + UUNLOrealE155D2hp55max(i))
!      
!      write(202,'(8(F12.6))') P(i),ALLLOE155D2hp55(i),
!     &                  ALLLOE155D2hp55min(i),ALLLOE155D2hp55max(i), 
!     &                       ALLWWE155D2hp55(i),
!     & ALLNLOE155D2hp55(i),ALLNLOE155D2hp55min(i),
!     &   ALLNLOE155D2hp55max(i)
!      end do
!      close(202)

!! ****** eD -> hm X ( theta = 2.75° ) ***************************************************

!      IH = 4
!      IC = -1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(210,file='UUE155_275_D2hm.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2hm275(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2hm275min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2hm275max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2hm275(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2hm275min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2hm275max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2hm275(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2hm275min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2hm275max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2hm275(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2hm275min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2hm275max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(210,'(8(F12.6))') P(i),UULOE155D2hm275(i),
!     &                  UULOE155D2hm275min(i),UULOE155D2hm275max(i), 
!     &                       UUWWE155D2hm275(i),
!     & UULOE155D2hm275(i) + UUWWE155D2hm275(i) + 
!     & UUNLOvirE155D2hm275(i) + UUNLOrealE155D2hm275(i),
!     & UULOE155D2hm275min(i) + UUWWE155D2hm275min(i) + 
!     & UUNLOvirE155D2hm275min(i) + UUNLOrealE155D2hm275min(i),
!     & UULOE155D2hm275max(i) + UUWWE155D2hm275max(i) + 
!     & UUNLOvirE155D2hm275max(i) + UUNLOrealE155D2hm275max(i)
!      end do
!      close(210)

!      open(211,file='LLE155_275_D2hm.dat',status='unknown')

!      thetafixed = 2.75d0/180d0*Pi
!      Pmin = 21d0 !PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 43d0 !M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2hm275(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2hm275min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2hm275max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2hm275(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2hm275min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2hm275max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2hm275(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2hm275min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2hm275max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2hm275(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2hm275min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2hm275max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(211,'(8(F12.6))') P(i),LLLOE155D2hm275(i),
!     &                  LLLOE155D2hm275min(i),LLLOE155D2hm275max(i), 
!     &                       LLWWE155D2hm275(i),
!     & LLLOE155D2hm275(i) + LLWWE155D2hm275(i) + 
!     & LLNLOvirE155D2hm275(i) + LLNLOrealE155D2hm275(i),
!     & LLLOE155D2hm275min(i) + LLWWE155D2hm275min(i) + 
!     & LLNLOvirE155D2hm275min(i) + LLNLOrealE155D2hm275min(i),
!     & LLLOE155D2hm275max(i) + LLWWE155D2hm275max(i) + 
!     & LLNLOvirE155D2hm275max(i) + LLNLOrealE155D2hm275max(i)
!      end do
!      close(211)

!      open(212,file='ALLE155_275_D2hm.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2hm275(i) = LLLOE155D2hm275(i)/UULOE155D2hm275(i)
!      ALLLOE155D2hm275min(i) = LLLOE155D2hm275min(i)/
!     &                                   UULOE155D2hm275min(i)
!      ALLLOE155D2hm275max(i) = LLLOE155D2hm275max(i)/
!     &                                   UULOE155D2hm275max(i)
!      ALLWWE155D2hm275(i) = LLWWE155D2hm275(i)/UUWWE155D2hm275(i)
!      
!      ALLNLOE155D2hm275(i) = (LLLOE155D2hm275(i) + LLWWE155D2hm275(i)
!     &            + LLNLOvirE155D2hm275(i) + LLNLOrealE155D2hm275(i))/
!     &           (UULOE155D2hm275(i) + UUWWE155D2hm275(i) + 
!     &            UUNLOvirE155D2hm275(i) + UUNLOrealE155D2hm275(i))
!      ALLNLOE155D2hm275min(i) = 
!     &          (LLLOE155D2hm275min(i) + LLWWE155D2hm275min(i)
!     &      + LLNLOvirE155D2hm275min(i) + LLNLOrealE155D2hm275min(i))/
!     &           (UULOE155D2hm275min(i) + UUWWE155D2hm275min(i) + 
!     &         UUNLOvirE155D2hm275min(i) + UUNLOrealE155D2hm275min(i))
!      ALLNLOE155D2hm275max(i) = 
!     &          (LLLOE155D2hm275max(i) + LLWWE155D2hm275max(i)
!     &      + LLNLOvirE155D2hm275max(i) + LLNLOrealE155D2hm275max(i))/
!     &           (UULOE155D2hm275max(i) + UUWWE155D2hm275max(i) + 
!     &         UUNLOvirE155D2hm275max(i) + UUNLOrealE155D2hm275max(i))
!      
!      write(212,'(8(F12.6))') P(i),ALLLOE155D2hm275(i),
!     &                  ALLLOE155D2hm275min(i),ALLLOE155D2hm275max(i), 
!     &                       ALLWWE155D2hm275(i),
!     & ALLNLOE155D2hm275(i),ALLNLOE155D2hm275min(i),
!     &   ALLNLOE155D2hm275max(i)
!      end do
!      close(212)

!! ****** eD -> hm X ( theta = 5.5° ) ***************************************************

!      IH = 4
!      IC = -1
!      IT = 2

!      M  = 0.94d0
!      E0 = 48.35d0
!      S  = M**2d0+2d0*M*E0
!      PTmin = 1d0

!      open(220,file='UUE155_55_D2hm.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      UULOE155D2hm55(i) = UULOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UULOE155D2hm55min(i) =UULOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UULOE155D2hm55max(i) =UULOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUWWE155D2hm55(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      UUWWE155D2hm55min(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      UUWWE155D2hm55max(i) = 
!     &             UUWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      UUNLOvirE155D2hm55(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOvirE155D2hm55min(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOvirE155D2hm55max(i) = 
!     &                   UUNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      UUNLOrealE155D2hm55(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      UUNLOrealE155D2hm55min(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      UUNLOrealE155D2hm55max(i) = 
!     &                UUNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(220,'(8(F12.6))') P(i),UULOE155D2hm55(i),
!     &                  UULOE155D2hm55min(i),UULOE155D2hm55max(i), 
!     &                       UUWWE155D2hm55(i),
!     & UULOE155D2hm55(i) + UUWWE155D2hm55(i) + 
!     & UUNLOvirE155D2hm55(i) + UUNLOrealE155D2hm55(i),
!     & UULOE155D2hm55min(i) + UUWWE155D2hm55min(i) + 
!     & UUNLOvirE155D2hm55min(i) + UUNLOrealE155D2hm55min(i),
!     & UULOE155D2hm55max(i) + UUWWE155D2hm55max(i) + 
!     & UUNLOvirE155D2hm55max(i) + UUNLOrealE155D2hm55max(i)
!      end do
!      close(220)

!      open(221,file='LLE155_55_D2hm.dat',status='unknown')

!      thetafixed = 5.5d0/180d0*Pi
!      Pmin = 11d0 ! PTmin/dsin(thetafixed) ! from the condition that Phperp > 1
!      Pmax = 35d0 ! M*(E0 + M )/(E0*(1d0 - dcos(thetafixed)) + M) ! from the condition that xmin < 1

!      do i = 0,nplot
!      P(i) = Pmin +dble(i)/dble(nplot)*(Pmax - Pmin)
!      mu(i) = P(i)*dsin(thetafixed)
!      mumin = 1d0
!      mumax = dsqrt(S)/2d0
!      LLLOE155D2hm55(i) = LLLOFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLLOE155D2hm55min(i) =LLLOFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLLOE155D2hm55max(i) =LLLOFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLWWE155D2hm55(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mu(i),mu(i),IH,IC,IT)
!      LLWWE155D2hm55min(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumin,mumin,IH,IC,IT)
!      LLWWE155D2hm55max(i) = 
!     &             LLWWFTM2(4,1,S,P(i),thetafixed,mumax,mumax,IH,IC,IT)
!      LLNLOvirE155D2hm55(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOvirE155D2hm55min(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOvirE155D2hm55max(i) = 
!     &                   LLNLOvirFTM2(S,P(i),thetafixed,mumax,IH,IC,IT)
!      LLNLOrealE155D2hm55(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mu(i),IH,IC,IT)
!      LLNLOrealE155D2hm55min(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumin,IH,IC,IT)
!      LLNLOrealE155D2hm55max(i) = 
!     &                LLNLOrealFTM2(4,S,P(i),thetafixed,mumax,IH,IC,IT)
!      write(221,'(8(F12.6))') P(i),LLLOE155D2hm55(i),
!     &                  LLLOE155D2hm55min(i),LLLOE155D2hm55max(i), 
!     &                       LLWWE155D2hm55(i),
!     & LLLOE155D2hm55(i) + LLWWE155D2hm55(i) + 
!     & LLNLOvirE155D2hm55(i) + LLNLOrealE155D2hm55(i),
!     & LLLOE155D2hm55min(i) + LLWWE155D2hm55min(i) + 
!     & LLNLOvirE155D2hm55min(i) + LLNLOrealE155D2hm55min(i),
!     & LLLOE155D2hm55max(i) + LLWWE155D2hm55max(i) + 
!     & LLNLOvirE155D2hm55max(i) + LLNLOrealE155D2hm55max(i)
!      end do
!      close(221)

!      open(222,file='ALLE155_55_D2hm.dat',status='unknown')

!      do i = 0,nplot
!      
!      ALLLOE155D2hm55(i) = LLLOE155D2hm55(i)/UULOE155D2hm55(i)
!      ALLLOE155D2hm55min(i) = LLLOE155D2hm55min(i)/
!     &                                   UULOE155D2hm55min(i)
!      ALLLOE155D2hm55max(i) = LLLOE155D2hm55max(i)/
!     &                                   UULOE155D2hm55max(i)
!      ALLWWE155D2hm55(i) = LLWWE155D2hm55(i)/UUWWE155D2hm55(i)
!      
!      ALLNLOE155D2hm55(i) = (LLLOE155D2hm55(i) + LLWWE155D2hm55(i)
!     &            + LLNLOvirE155D2hm55(i) + LLNLOrealE155D2hm55(i))/
!     &           (UULOE155D2hm55(i) + UUWWE155D2hm55(i) + 
!     &            UUNLOvirE155D2hm55(i) + UUNLOrealE155D2hm55(i))
!      ALLNLOE155D2hm55min(i) = 
!     &          (LLLOE155D2hm55min(i) + LLWWE155D2hm55min(i)
!     &      + LLNLOvirE155D2hm55min(i) + LLNLOrealE155D2hm55min(i))/
!     &           (UULOE155D2hm55min(i) + UUWWE155D2hm55min(i) + 
!     &         UUNLOvirE155D2hm55min(i) + UUNLOrealE155D2hm55min(i))
!      ALLNLOE155D2hm55max(i) = 
!     &          (LLLOE155D2hm55max(i) + LLWWE155D2hm55max(i)
!     &      + LLNLOvirE155D2hm55max(i) + LLNLOrealE155D2hm55max(i))/
!     &           (UULOE155D2hm55max(i) + UUWWE155D2hm55max(i) + 
!     &         UUNLOvirE155D2hm55max(i) + UUNLOrealE155D2hm55max(i))
!      
!      write(222,'(8(F12.6))') P(i),ALLLOE155D2hm55(i),
!     &                  ALLLOE155D2hm55min(i),ALLLOE155D2hm55max(i), 
!     &                       ALLWWE155D2hm55(i),
!     & ALLNLOE155D2hm55(i),ALLNLOE155D2hm55min(i),
!     &   ALLNLOE155D2hm55max(i)
!      end do
!      close(222)


      End Program

!***********************************************************************
!     Unpolarized Weizsäcker-Williams Distribution

      function f1WW(y,mu,nl)
      implicit none
      integer nl
      real*8 f1WW,y,mu,ml,me,mmu

      mmu = 0.105d0    ! Muon mass
      me  = 0.000511d0 ! Electron mass

      if(nl.eq.1) then
      ml = me
      end if

      if(nl.eq.2) then
      ml = mmu
      end if
         
      f1WW =(1d0 +(1d0 - y)**2d0)/y*(dlog(mu**2d0/y**2d0/ml**2d0) - 1d0)
      
      end function

!     Longitudinally polarized Weizsäcker-Williams Distribution

      function g1WW(y,mu,nl)
      implicit none
      integer nl
      real*8 g1WW,y,mu,ml,me,mmu

      mmu = 0.105d0    ! Muon mass
      me  = 0.000511d0 ! Electron mass

      if(nl.eq.1) then
      ml = me
      end if

      if(nl.eq.2) then
      ml = mmu
      end if
      
      g1WW =(2d0 - y)*(dlog(mu**2d0/y**2d0/ml**2d0))
      end function


! ***** Test Fragmentation Function DSS ********************************
      function D1(z,mu,parton,IH,IC)
      implicit none
      integer IH,IC,parton
      real*8 D1,z,mu,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl

      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      if(parton .eq. 1) then
      D1 = zD1u/z
      end if

      if(parton .eq. 2) then
      D1 = zD1d/z
      end if

      if(parton .eq. -1) then
      D1 = zD1ub/z
      end if

      if(parton .eq. -2) then
      D1 = zD1db/z
      end if

      if(parton .eq. 3) then
      D1 = zD1s/z
      end if

      if(parton .eq. -3) then
      D1 = zD1sb/z
      end if

      if(parton .eq. 4) then
      D1 = zD1c/z
      end if

      if(parton .eq. -4) then
      D1 = zD1b/z
      end if

      if(parton .eq. 0) then
      D1 = zD1gl/z
      end if

      end function

!***********************************************************************
!     UNPOLARIZED CROSS SECTION
!***********************************************************************

!*************  LEADING ORDER ******************************************
      function UULOInt(S,T,U,mu,IH,IC,IT,v)
      implicit none
      integer IH,IC,IT,FINI,iset
      character*32 prefix
      real*8 UULOInt,S,T,U,mu,v,x,z,alphaem,Pi,Conv
      real*8 H,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl
      real*8 upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3
      common/ FRAGINID / FINI

      

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0

      x = (1d0 - v)/v*U/T
      z = -T/(1d0 - v)/S

      if((x.ge.1d0) .or. (z.ge.1d0) .or. ((S + T + U).le.0d0)) then
      UULOInt = 0d0

      else

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if

      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)

      if(IT.eq.1) then
!     Proton PDFs:
      H = (2d0/3d0)**2d0*( ((upv + usea)/x**2d0)*(zD1u/z**3d0) +
     &                     (usea/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((dnv + dsea)/x**2d0)*(zD1d/z**3d0) +
     &                     (dsea/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (str/x**2d0)*(zD1s/z**3d0) +
     &                     (sbar/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (chm/x**2d0)*(zD1c/z**3d0) +
     &                     (cbar/x**2d0)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (bot/x**2d0)*(zD1b/z**3d0) +
     &                     (bbar/x**2d0)*(zD1b/z**3d0) )

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*(upv + usea + dnv + dsea)/x
      fdD  = 1d0/2d0*(upv + usea + dnv + dsea)/x
      fubD = 1d0/2d0*(usea + dsea)/x
      fdbD = 1d0/2d0*(usea + dsea)/x
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x


      H = (2d0/3d0)**2d0*( (fuD/x)*(zD1u/z**3d0) +
     &                     (fubD/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdD/x)*(zD1d/z**3d0) +
     &                     (fdbD/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsD/x)*(zD1s/z**3d0) +
     &                     (fsbD/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcD/x)*(zD1c/z**3d0) +
     &                     (fcbD/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbD/x)*(zD1b/z**3d0) +
     &                     (fbbD/x)*(zD1b/z**3d0) )
      end if

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*(2d0*upv + 2d0*usea + dnv + dsea)/x
      fdHe3  = 1d0/3d0*(upv + usea + 2d0*dnv + 2d0*dsea)/x
      fubHe3 = 1d0/3d0*(2d0*usea + dsea)/x
      fdbHe3 = 1d0/3d0*(usea + 2d0*dsea)/x
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x


      H = (2d0/3d0)**2d0*( (fuHe3/x)*(zD1u/z**3d0) +
     &                     (fubHe3/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdHe3/x)*(zD1d/z**3d0) +
     &                     (fdbHe3/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsHe3/x)*(zD1s/z**3d0) +
     &                     (fsbHe3/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcHe3/x)*(zD1c/z**3d0) +
     &                     (fcbHe3/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbHe3/x)*(zD1b/z**3d0) +
     &                     (fbbHe3/x)*(zD1b/z**3d0) )
      end if


      UULOInt = Conv* 2d0*alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)* (T/(1d0 - v)/S/U) * H 
     &     *(1d0 + v**2d0)/(1d0 - v)**2d0

      end if
      end function

!*************  LEADING ORDER (Jet Production) *************************
      function UULOJetInt(S,T,U,mu,IT)
      implicit none
      integer IT,FINI,iset
      character*32 prefix
      real*8 UULOJetInt,S,T,U,mu,v,x,alphaem,Pi,Conv
      real*8 H,upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0

      x = -U/(S+T)
      v = 1d0 + T/S

      if((x.ge.1d0).or. ((S+T+U).le.0d0)) then
      UULOJetInt = 0d0

      else

      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)

      if(IT.eq.1) then
!     Proton PDFs:
      H = (2d0/3d0)**2d0*( (upv + 2d0*usea)/x ) +
     &    (1d0/3d0)**2d0*( (dnv + 2d0*dsea)/x ) +
     &    (1d0/3d0)**2d0*( (str + sbar)/x ) +
     &    (2d0/3d0)**2d0*( (chm + cbar)/x ) +
     &    (1d0/3d0)**2d0*( (bot + bbar)/x )

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*( (upv + usea)/x + (dnv + dsea)/x )
      fdD  = 1d0/2d0*( (upv + usea)/x + (dnv + dsea)/x )
      fubD = 1d0/2d0*( (usea)/x + (dsea)/x )
      fdbD = 1d0/2d0*( (usea)/x + (dsea)/x )
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x


      H = (2d0/3d0)**2d0*( fuD + fubD ) +
     &    (1d0/3d0)**2d0*( fdD + fdbD ) +
     &    (1d0/3d0)**2d0*( fsD + fsbD ) +
     &    (2d0/3d0)**2d0*( fcD + fcbD ) +
     &    (1d0/3d0)**2d0*( fbD + fbbD )
      end if

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*( 2d0*(upv + usea)/x + (dnv + dsea)/x )
      fdHe3  = 1d0/3d0*( (upv + usea)/x + 2d0*(dnv + dsea)/x )
      fubHe3 = 1d0/3d0*( 2d0*(usea)/x + (dsea)/x )
      fdbHe3 = 1d0/3d0*( (usea)/x + 2d0*(dsea)/x )
      fdbHe3 = dsea/x
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x


      H = (2d0/3d0)**2d0*( fuHe3 + fubHe3 ) +
     &    (1d0/3d0)**2d0*( fdHe3 + fdbHe3 ) +
     &    (1d0/3d0)**2d0*( fsHe3 + fsbHe3 ) +
     &    (2d0/3d0)**2d0*( fcHe3 + fcbHe3 ) +
     &    (1d0/3d0)**2d0*( fbHe3 + fbbHe3 )
      end if


      UULOJetInt = Conv* 2d0*(alphaem**2d0)/S*
     &    (T/(1d0 - v)/S/U) * H 
     &     *(1d0 + v**2d0)/(1d0 - v)**2d0

      end if
      end function

! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function UULOFTM1Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UULOFTM1Aux,Y(*),UULOInt,S,T,U,mu,v,xF,PT,xT,Pi
      common / varUUFTM1LO / S,xF,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      UULOFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             UULOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UULOFTM1(Ss,xFf,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UULOFTM1,Ss,xFf,PTt,muu,UULOFTM1Aux,S,xF,PT,mu,xT,T,U
      Common / varUUFTM1LO / S,xF,PT,mu,IH,IC,IT
      External UULOFTM1Aux

      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UULOFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UULOFTM1 = out
      end function

! **********  E155 *****************************************************

      function UULOFTM2Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UULOFTM2Aux,Y(*),UULOInt,S,T,U,mu,v,P,th,M,Pi
      common / varUUFTM2LO / S,P,th,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      UULOFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             UULOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UULOFTM2(Ss,Pp,thh,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UULOFTM2,Ss,Pp,thh,muu,UULOFTM1Aux,S,P,th,mu,M,Pi,T,U
      Common / varUUFTM2LO / S,P,th,mu,IH,IC,IT
      External UULOFTM2Aux

      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UULOFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UULOFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jet) ***********************************

      function UULOCMAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UULOCMAux,Y(*),UULOInt,S,T,U,mu,v,eta,PT,Pi
      common / varUUCMLO / S,eta,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UULOCMAux = 2d0*Pi*PT* UULOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UULOCM(Ss,etaa,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UULOCM,Ss,etaa,PTt,muu,UULOFTM1Aux,S,eta,PT,mu,T,U
      Common / varUUCMLO / S,eta,PT,mu,IH,IC,IT
      External UULOCMAux

      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UULOCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UULOCM = out
      end function


      function UULOJetCM(S,eta,PT,mu,IT)
      implicit none
      integer IT
      real*8 UULOJetCM,UULOJetInt,S,eta,PT,mu,T,U,Pi

      Pi = 2d0*dasin(1d0)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)


      UULOJetCM = 2d0*Pi*PT*UULOJetInt(S,T,U,mu,IT)
      end function

! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function UULOFTM1PTAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UULOFTM1PTAux,Y(*),UULOInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       PTmin,PTmax
      real*8 a
      common / varUUFTM1PTLO / S,xF,PTmin,PTmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      PT = Y(2)
      mu = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UULOFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   UULOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UULOFTM1PT(Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UULOFTM1PT,Ss,xFf,PTminn,PTmaxx,UULOFTM1PTAux,S,xF,PTmin,
     &       PTmax
      Common / varUUFTM1PTLO / S,xF,PTmin,PTmax,IH,IC,IT
      External UULOFTM1PTAux

      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = PTmin
      region(3) = 1d0
      region(4) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UULOFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UULOFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function UULOFTM1xFAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UULOFTM1xFAux,Y(*),UULOInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       xFmin,xFmax
      real*8 a
      common / varUUFTM1xFLO / S,PT,mu,xFmin,xFmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      xF = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UULOFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   UULOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UULOFTM1xF(Ss,PTt,muu,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UULOFTM1xF,Ss,PTt,muu,xFminn,xFmaxx,UULOFTM1xFAux,
     &       S,PT,mu,xFmin,xFmax
      Common / varUUFTM1xFLO / S,PT,mu,xFmin,xFmax,IH,IC,IT
      External UULOFTM1xFAux

      S     = Ss
      PT    = PTt
      mu    = muu
      xFmin = xFminn
      xFmax = xFmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = xFmin
      region(3) = 1d0
      region(4) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UULOFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UULOFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

! **********  eta - bins  **********************************************

      function UULOCMetaAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UULOCMetaAux,Y(*),UULOInt,S,T,U,mu,v,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varUUCMetaLO / S,PT,mu,etamin,etamax,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UULOCMetaAux = 2d0*Pi*PT*
     &         ((S+T)/S - U/(T+U)) *   UULOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UULOCMeta(Ss,PTt,muu,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UULOCMeta,Ss,PTt,muu,etaminn,etamaxx,UULOCMetaAux,
     &       S,PT,mu,etamin,etamax
      Common / varUUCMetaLO / S,PT,mu,etamin,etamax,IH,IC,IT
      External UULOCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UULOCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UULOCMeta = out
      end function


      function UULOJetCMetaAux(Y)
      implicit none
      integer IT
      real*8 UULOJetCMetaAux,Y(*),UULOJetInt,S,T,U,mu,eta,PT,Pi,
     &       etamin,etamax
      common / varUUCMetaLOJet / S,PT,mu,etamin,etamax,IT

      Pi  = 2d0*dasin(1d0)
      eta = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UULOJetCMetaAux = 2d0*Pi*PT**5d0*UULOJetInt(S,T,U,mu,IT)
      end function

      function UULOJetCMeta(Ss,PTt,muu,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UULOJetCMeta,Ss,PTt,muu,etaminn,etamaxx,UULOJetCMetaAux,
     &       S,PT,mu,etamin,etamax
      Common / varUUCMetaLOJet / S,PT,mu,etamin,etamax,IT
      External UULOJetCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = etamin
      region(2) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UULOJetCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UULOJetCMeta = out
      end function

!***********************************************************************
!******  WEIZSÄCKER - WILLIAMS CONTRIBUTION  ***************************
!***********************************************************************
!     flag n = 1: q -> q, n = 2: q -> g, n = 3: g -> q, n = 4: total

      function UUWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      implicit none
      integer n,nl,IH,IC,IT,FINI,iset
      character*32 prefix
      real*8 UUWWInt,S,T,U,mu,mu0,v,w,x,z,alphaem,alphastr,Pi,Conv
      real*8 upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl
      real*8 f1WW,CF,TR,Hqq,Hqg,Hgq,UUWWqq,UUWWqg,UUWWgq,alphas,preCS
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD,fglD
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3,fglHe3
      common/ FRAGINID / FINI

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      x = (1d0 - v)/v/w*U/T
      z = -T/(1d0 - v)/S

      if((w.lt.(1d0 - v)/v*U/T) .or. (x.ge.1d0) .or. (z.ge.1d0) .or. 
     &                 ((S + T + U).le.0d0)) then
      UUWWInt = 0d0
      else 

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if
 
      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)

      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( ((upv + usea)/x**2d0)*(zD1u/z**3d0) +
     &                     (usea/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((dnv + dsea)/x**2d0)*(zD1d/z**3d0) +
     &                     (dsea/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (str/x**2d0)*(zD1s/z**3d0) +
     &                     (sbar/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (chm/x**2d0)*(zD1c/z**3d0) +
     &                     (cbar/x**2d0)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (bot/x**2d0)*(zD1b/z**3d0) +
     &                     (bbar/x**2d0)*(zD1b/z**3d0) )

      Hqg = (2d0/3d0)**2d0*( ((upv + usea)/x**2d0)*(zD1gl/z**3d0) +
     &                     (usea/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((dnv + dsea)/x**2d0)*(zD1gl/z**3d0) +
     &                     (dsea/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (str/x**2d0)*(zD1gl/z**3d0) +
     &                     (sbar/x**2d0)*(zD1gl/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (chm/x**2d0)*(zD1gl/z**3d0) +
     &                     (cbar/x**2d0)*(zD1gl/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (bot/x**2d0)*(zD1gl/z**3d0) +
     &                     (bbar/x**2d0)*(zD1gl/z**3d0) )

      Hgq = (2d0/3d0)**2d0*( (glu/x**2d0)*(zD1u/z**3d0) +
     &                     (glu/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (glu/x**2d0)*(zD1d/z**3d0) +
     &                     (glu/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (glu/x**2d0)*(zD1s/z**3d0) +
     &                     (glu/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (glu/x**2d0)*(zD1c/z**3d0) +
     &                     (glu/x**2d0)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (glu/x**2d0)*(zD1b/z**3d0) +
     &                     (glu/x**2d0)*(zD1b/z**3d0) )    

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fdD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fubD = 1d0/2d0*( (usea + dsea)/x )
      fdbD = 1d0/2d0*( (usea + dsea)/x )
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x
      fglD = glu/x


      Hqq = (2d0/3d0)**2d0*( (fuD/x)*(zD1u/z**3d0) +
     &                     (fubD/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdD/x)*(zD1d/z**3d0) +
     &                     (fdbD/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsD/x)*(zD1s/z**3d0) +
     &                     (fsbD/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcD/x)*(zD1c/z**3d0) +
     &                     (fcbD/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbD/x)*(zD1b/z**3d0) +
     &                     (fbbD/x)*(zD1b/z**3d0) )

      Hqg = (2d0/3d0)**2d0*( (fuD/x)*(zD1gl/z**3d0) +
     &                     (fubD/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdD/x)*(zD1gl/z**3d0) +
     &                     (fdbD/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsD/x)*(zD1gl/z**3d0) +
     &                     (fsbD/x)*(zD1gl/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcD/x)*(zD1gl/z**3d0) +
     &                     (fcbD/x)*(zD1gl/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbD/x)*(zD1gl/z**3d0) +
     &                     (fbbD/x)*(zD1gl/z**3d0) )

      Hgq = (2d0/3d0)**2d0*( (fglD/x)*(zD1u/z**3d0) +
     &                     (fglD/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglD/x)*(zD1d/z**3d0) +
     &                     (fglD/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglD/x)*(zD1s/z**3d0) +
     &                     (fglD/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fglD/x)*(zD1c/z**3d0) +
     &                     (fglD/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fglD/x)*(zD1b/z**3d0) +
     &                     (fglD/x)*(zD1b/z**3d0) )    
      endif

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*( (2d0*upv + 2d0*usea + dnv + dsea)/x )
      fdHe3  = 1d0/3d0*( (upv + usea + 2d0*dnv + 2d0*dsea)/x)
      fubHe3 = 1d0/3d0*( (2d0*usea + dsea)/x )
      fdbHe3 = 1d0/3d0*( (usea + 2d0*dsea)/x)
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x
      fglHe3 = glu/x


      Hqq = (2d0/3d0)**2d0*( (fuHe3/x)*(zD1u/z**3d0) +
     &                     (fubHe3/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdHe3/x)*(zD1d/z**3d0) +
     &                     (fdbHe3/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsHe3/x)*(zD1s/z**3d0) +
     &                     (fsbHe3/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcHe3/x)*(zD1c/z**3d0) +
     &                     (fcbHe3/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbHe3/x)*(zD1b/z**3d0) +
     &                     (fbbHe3/x)*(zD1b/z**3d0) )

      Hqg = (2d0/3d0)**2d0*( (fuHe3/x)*(zD1gl/z**3d0) +
     &                     (fubHe3/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdHe3/x)*(zD1gl/z**3d0) +
     &                     (fdbHe3/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsHe3/x)*(zD1gl/z**3d0) +
     &                     (fsbHe3/x)*(zD1gl/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcHe3/x)*(zD1gl/z**3d0) +
     &                     (fcbHe3/x)*(zD1gl/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbHe3/x)*(zD1gl/z**3d0) +
     &                     (fbbHe3/x)*(zD1gl/z**3d0) )

      Hgq = (2d0/3d0)**2d0*( (fglHe3/x)*(zD1u/z**3d0) +
     &                     (fglHe3/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglHe3/x)*(zD1d/z**3d0) +
     &                     (fglHe3/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglHe3/x)*(zD1s/z**3d0) +
     &                     (fglHe3/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fglHe3/x)*(zD1c/z**3d0) +
     &                     (fglHe3/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fglHe3/x)*(zD1b/z**3d0) +
     &                     (fglHe3/x)*(zD1b/z**3d0) )    
      endif


      UUWWqq = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)/w**2d0*(T/S/U)*Hqq*
     &   f1WW((1d0 - v)/(1d0 -v*w),mu0,nl)*
     &   CF*alphastr/(Pi)*(1d0 + v**2d0*w**2d0)/(1d0 - v)**2d0

      UUWWqg = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)/w**2d0*(v*w*T/(1d0 - v)/S/U)*Hqg*
     &   f1WW((1d0 - v)/(1d0 -v*w),mu0,nl)*
     &CF*alphastr/(Pi)*(1d0 + (1 - v*w)**2d0)/(1d0 - v)/(1d0 - v*w)

      UUWWgq = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)/w**2d0*(v*w*T/(1d0 - v)/S/U)*Hgq*
     &   f1WW((1d0 - v)/(1d0 -v*w),mu0,nl)*
     &   TR*alphastr/(Pi)*
     &      ((v*w)**2d0 + (1d0 - v*w)**2d0)/(v*w)/(1d0 - v)/(1d0 - v*w)

      if(n.eq.1) then
      UUWWInt = UUWWqq
      end if
      
      if(n.eq.2) then
      UUWWInt = UUWWqg
      end if

      if(n.eq.3) then
      UUWWInt = UUWWgq
      end if

      if(n.eq.4) then
      UUWWInt = UUWWqq + UUWWqg + UUWWgq
      end if

      end if
      end function

! ********** Jet Production ********************************************

      function UUWWJetInt(n,nl,S,T,U,mu,mu0,IT,w)
      implicit none
      character*32 prefix
      integer n,nl,IT,FINI,iset
      real*8 UUWWJetInt,S,T,U,mu,mu0,v,w,x,alphaem,alphastr,Pi,Conv
      real*8 upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 f1WW,CF,TR,Hqq,Hqg,Hgq,UUWWJetqq,UUWWJetqg,UUWWJetgq,alphas
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD,fglD
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3,fglHe3

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)


!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      x = -U/w/(S+T)
      v = 1d0 + T/S

      if((w.lt.(-U/(S+T))) .or. (x.ge.1d0) .or. ((S+T+U).lt.0d0) ) then
      UUWWJetInt = 0d0
      else 

      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)


      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( (upv + 2d0*usea)/x ) +
     &    (1d0/3d0)**2d0*( (dnv + 2d0*dsea)/x ) +
     &    (1d0/3d0)**2d0*( (str + sbar)/x ) +
     &    (2d0/3d0)**2d0*( (chm + cbar)/x )  +
     &    (1d0/3d0)**2d0*( (bot + bbar)/x )

      Hqg = (2d0/3d0)**2d0*( (upv + 2d0*usea)/x ) +
     &    (1d0/3d0)**2d0*( (dnv + 2d0*dsea)/x ) +
     &    (1d0/3d0)**2d0*( (str + sbar)/x ) +
     &    (2d0/3d0)**2d0*( (chm + cbar)/x ) +
     &    (1d0/3d0)**2d0*( (bot + bbar)/x )

!    Work with Nf = 4 active flavors

      Hgq = 2d0*(glu/x)*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &    (1d0/3d0)**2d0 + (2d0/3d0)**2d0  )    

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fdD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fubD = 1d0/2d0*( (usea + dsea)/x )
      fdbD = 1d0/2d0*( (usea + dsea)/x )
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x
      fglD = glu/x


      Hqq = (2d0/3d0)**2d0*( fuD + fubD ) +
     &    (1d0/3d0)**2d0*( fdD + fdbD ) +
     &    (1d0/3d0)**2d0*( fsD + fsbD ) +
     &    (2d0/3d0)**2d0*( fcD + fcbD ) +
     &    (1d0/3d0)**2d0*( fbD + fbbD )

      Hqg = (2d0/3d0)**2d0*( fuD + fubD ) +
     &    (1d0/3d0)**2d0*( fdD + fdbD ) +
     &    (1d0/3d0)**2d0*( fsD + fsbD ) +
     &    (2d0/3d0)**2d0*( fcD + fcbD ) +
     &    (1d0/3d0)**2d0*( fbD + fbbD )

!    Work with Nf = 4 active flavors

      Hgq = 2d0*fglD*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &    (1d0/3d0)**2d0 + (2d0/3d0)**2d0  )    
      endif

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*( (2d0*upv + 2d0*usea + dnv + dsea)/x )
      fdHe3  = 1d0/3d0*( (upv + usea + 2d0*dnv + 2d0*dsea)/x )
      fubHe3 = 1d0/3d0*( (2d0*usea + dsea)/x )
      fdbHe3 = 1d0/3d0*( (usea + 2d0*dsea)/x )
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x
      fglHe3 = glu/x


      Hqq = (2d0/3d0)**2d0*( fuHe3 + fubHe3 ) +
     &    (1d0/3d0)**2d0*( fdHe3 + fdbHe3 ) +
     &    (1d0/3d0)**2d0*( fsHe3 + fsbHe3 ) +
     &    (2d0/3d0)**2d0*( fcHe3 + fcbHe3 ) +
     &    (1d0/3d0)**2d0*( fbHe3 + fbbHe3 )

      Hqg = (2d0/3d0)**2d0*( fuHe3 + fubHe3 ) +
     &    (1d0/3d0)**2d0*( fdHe3 + fdbHe3 ) +
     &    (1d0/3d0)**2d0*( fsHe3 + fsbHe3 ) +
     &    (2d0/3d0)**2d0*( fcHe3 + fcbHe3 ) +
     &    (1d0/3d0)**2d0*( fbHe3 + fbbHe3 )

!    Work with Nf = 4 active flavors

      Hgq = 2d0*fglHe3*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &    (1d0/3d0)**2d0 + (2d0/3d0)**2d0  )    
      endif


      UUWWJetqq = Conv*
     &   alphaem**2d0*(1d0/S/w)*Hqq*CF*alphastr/Pi*
     &   f1WW(-T/(S*(1d0 - w)-w*T),mu0,nl)*(T/S/U)*
     &   (1d0 + v**2d0*w**2d0)/(1d0 - v)**2d0

      UUWWJetqg = Conv*
     &   alphaem**2d0*(1d0/S/w)*Hqg*CF*alphastr/Pi*
     &   f1WW(-T/(S*(1d0 - w)-w*T),mu0,nl)*(T/S/U)*
     &   v*w*(1d0 + (1d0 - v*w)**2d0)/(1d0 - v)**2d0/(1d0 - v*w)

      UUWWJetgq = Conv*
     &   alphaem**2d0*(1d0/S/w)*Hgq*TR*alphastr/Pi*
     &   f1WW(-T/(S*(1d0 - w)-w*T),mu0,nl)*(T/S/U)*
     &   (v**2d0*w**2d0 + (1d0 - v*w)**2d0)/(1d0 - v)**2d0/(1d0 - v*w)

      if(n.eq.1) then
      UUWWJetInt = UUWWJetqq
      end if
      
      if(n.eq.2) then
      UUWWJetInt = UUWWJetqg
      end if

      if(n.eq.3) then
      UUWWJetInt = UUWWJetgq
      end if

      if(n.eq.4) then
      UUWWJetInt = UUWWJetqq + UUWWJetqg + UUWWJetgq
      end if

      end if
      end function

! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function UUWWFTM1Aux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 UUWWFTM1Aux,Y(*),UUWWInt,S,T,U,mu,mu0,v,w,xF,PT,xT,Pi
      common / varUUFTM1WW / S,xF,PT,mu,mu0,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      UUWWFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             UUWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function UUWWFTM1(nn,nll,Ss,xFf,PTt,muu,mu00,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWFTM1,Ss,xFf,PTt,muu,mu00,UUWWFTM1Aux,S,xF,PT,mu,mu0,
     &       xT,T,U
      Common / varUUFTM1WW / S,xF,PT,mu,mu0,n,nl,IH,IC,IT
      External UUWWFTM1Aux

      n   = nn
      nl  = nll
      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      mu0 = mu00
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWFTM1 = out
      end function

! **********  E155 *****************************************************

      function UUWWFTM2Aux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 UUWWFTM2Aux,Y(*),UUWWInt,S,T,U,mu,mu0,v,w,P,th,M,Pi
      common / varUUFTM2WW / S,P,th,mu,mu0,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)
      w = Y(2)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      UUWWFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             UUWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function UUWWFTM2(nn,nll,Ss,Pp,thh,muu,mu00,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWFTM2,Ss,Pp,thh,muu,mu00,UUWWFTM1Aux,S,P,th,mu,mu0,
     &       M,Pi,T,U
      Common / varUUFTM2WW / S,P,th,mu,mu0,n,nl,IH,IC,IT
      External UUWWFTM2Aux

      n   = nn
      nl  = nll
      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      mu0 = mu00
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

      function UUWWCMAux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 UUWWCMAux,Y(*),UUWWInt,S,T,U,mu,mu0,v,w,eta,PT,Pi
      common / varWWCMLO / S,eta,PT,mu,mu0,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUWWCMAux = 2d0*Pi*PT* UUWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function UUWWCM(nn,nll,Ss,etaa,PTt,muu,mu00,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWCM,Ss,etaa,PTt,muu,mu00,UUWWCMAux,S,eta,PT,mu,mu0,T,U
      Common / varWWCMLO / S,eta,PT,mu,mu0,n,nl,IH,IC,IT
      External UUWWCMAux

      n   = nn
      nl  = nll
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      mu0 = mu00
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWCM = out
      end function

!  ********  Jets  *****************************************************

      function UUWWCMJetAux(Y)
      implicit none
      integer n,nl,IT
      real*8 UUWWCMJetAux,Y(*),UUWWJetInt,S,T,U,mu,mu0,w,eta,PT,Pi
      common / varWWCMJetLO / S,eta,PT,mu,mu0,n,nl,IT

      Pi = 2d0*dasin(1d0)
      w = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUWWCMJetAux = 2d0*Pi*PT* 
     &                 UUWWJetInt(n,nl,S,T,U,mu,mu0,IT,w)
      end function

      function UUWWCMJet(nn,nll,Ss,etaa,PTt,muu,mu00,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWCMJet,Ss,etaa,PTt,muu,mu00,UUWWCMJetAux,
     &       S,eta,PT,mu,mu0,T,U
      Common / varWWCMJetLO / S,eta,PT,mu,mu0,n,nl,IT
      External UUWWCMJetAux

      n   = nn
      nl  = nll
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      mu0 = mu00
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = 0d0
      region(2) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWCMJetAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWCMJet = out
      end function

! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function UUWWFTM1PTAux(Y)
      implicit none
      integer n,nl,nmu,IH,IC,IT
      real*8 UUWWFTM1PTAux,Y(*),UUWWInt,S,T,U,mu,mu0,v,w,xF,PT,xT,Pi,
     &       PTmin,PTmax
      real*8 a
      common / varUUFTM1PTWW / S,xF,PTmin,PTmax,n,nl,IH,IC,IT,nmu

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      PT = Y(3)
      mu = Y(3)

      if(nmu.eq.1) then
      mu0 = Y(3)
      else
      mu0 = dsqrt(S)/2d0
      end if

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUWWFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &      ((S+T)/S - U/(T+U)) *UUWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function UUWWFTM1PT(nn,nll,Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt,nmuu)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT,nmu,nmuu
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWFTM1PT,Ss,xFf,PTminn,PTmaxx,UUWWFTM1PTAux,S,xF,PTmin,
     &       PTmax
      Common / varUUFTM1PTWW / S,xF,PTmin,PTmax,n,nl,IH,IC,IT,nmu
      External UUWWFTM1PTAux

      n     = nn
      nl    = nll
      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt
      nmu   = nmuu

      region(1) = 0d0
      region(2) = 0d0
      region(3) = PTmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function UUWWFTM1xFAux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 UUWWFTM1xFAux,Y(*),UUWWInt,S,T,U,mu,mu0,v,w,xF,PT,xT,Pi,
     &       xFmin,xFmax
      real*8 a
      common / varUUFTM1xFWW / S,PT,mu,mu0,xFmin,xFmax,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      xF = Y(3)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUWWFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &     ((S+T)/S - U/(T+U)) * UUWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function UUWWFTM1xF
     &                (nn,nll,Ss,PTt,muu,mu00,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWFTM1xF,Ss,PTt,muu,mu00,xFminn,xFmaxx,UUWWFTM1xFAux,
     &       S,PT,mu,mu0,xFmin,xFmax
      Common / varUUFTM1xFWW / S,PT,mu,mu0,xFmin,xFmax,n,nl,IH,IC,IT
      External UUWWFTM1xFAux

      n     = nn
      nl    = nll
      S     = Ss
      PT    = PTt
      mu    = muu
      mu0   = mu00
      xFmin = xFminn
      xFmax = xFmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = xFmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

! **********  eta - bins  **********************************************

      function UUWWCMetaAux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 UUWWCMetaAux,Y(*),UUWWInt,S,T,U,mu,mu0,v,w,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varUUCMetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      w   = Y(2)
      eta = Y(3)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUWWCMetaAux = 2d0*Pi*PT*
     &      ((S+T)/S - U/(T+U)) *UUWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function UUWWCMeta
     &              (nn,nll,Ss,PTt,muu,mu00,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWCMeta,Ss,PTt,muu,mu00,etaminn,etamaxx,UUWWCMetaAux,
     &       S,PT,mu,mu0,etamin,etamax
      Common / varUUCMetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IH,IC,IT
      External UUWWCMetaAux

      n      = nn
      nl     = nll
      S      = Ss
      PT     = PTt
      mu     = muu
      mu0    = mu00
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = etamin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWCMeta = out
      end function

! ********  Jets  ******************************************************

      function UUWWCMJetetaAux(Y)
      implicit none
      integer n,nl,IT
      real*8 UUWWCMJetetaAux,Y(*),UUWWJetInt,S,T,U,mu,mu0,w,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varUUCMJetetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IT

      Pi  = 2d0*dasin(1d0)
      w   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUWWCMJetetaAux = 2d0*Pi*PT**5d0*
     &      UUWWJetInt(n,nl,S,T,U,mu,mu0,IT,w)
      end function

      function UUWWCMJeteta(nn,nll,Ss,PTt,muu,mu00,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,ITt,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUWWCMJeteta,Ss,PTt,muu,mu00,etaminn,etamaxx,
     &       UUWWCMJetetaAux,S,PT,mu,mu0,etamin,etamax
      Common / varUUCMJetetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IT
      External UUWWCMJetetaAux

      n      = nn
      nl     = nll
      S      = Ss
      PT     = PTt
      mu     = muu
      mu0    = mu00
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUWWCMJetetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUWWCMJeteta = out
      end function


!***********************************************************************
!*********  NEXT-TO-LEADING ORDER (virtual corrections)  ***************
!***********************************************************************

      function UUNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      implicit none
      character*32 prefix
      integer IH,IC,IT,FINI,iset
      real*8 UUNLOvirInt,S,mu,v,T,U,x,z,alphaem,Pi,Conv,
     &       alphas,alphastr,CF
      real*8 upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 H,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD,fglD
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3,fglHe3
      common/ FRAGINID / FINI

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0

      x = (1d0 - v)/v*U/T
      z = -T/(1d0 - v)/S

      if((x.ge.1d0) .or. (z.ge.1d0).or. ((S + T + U).le.0d0)) then
      UUNLOvirInt = 0d0

      else

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if

      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)

      if(IT.eq.1) then
!     Proton PDFs:
      H = (2d0/3d0)**2d0*( ((upv + usea)/x**2d0)*(zD1u/z**3d0) +
     &                     (usea/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((dnv + dsea)/x**2d0)*(zD1d/z**3d0) +
     &                     (dsea/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (str/x**2d0)*(zD1s/z**3d0) +
     &                     (sbar/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (chm/x**2d0)*(zD1c/z**3d0) +
     &                     (cbar/x**2d0)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (bot/x**2d0)*(zD1b/z**3d0) +
     &                     (bbar/x**2d0)*(zD1b/z**3d0) )

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*(upv + usea + dnv + dsea)/x
      fdD  = 1d0/2d0*(upv + usea + dnv + dsea)/x
      fubD = 1d0/2d0*(usea + dsea)/x
      fdbD = 1d0/2d0*(usea + dsea)/x
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x


      H = (2d0/3d0)**2d0*( (fuD/x)*(zD1u/z**3d0) +
     &                     (fubD/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdD/x)*(zD1d/z**3d0) +
     &                     (fdbD/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsD/x)*(zD1s/z**3d0) +
     &                     (fsbD/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcD/x)*(zD1c/z**3d0) +
     &                     (fcbD/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbD/x)*(zD1b/z**3d0) +
     &                     (fbbD/x)*(zD1b/z**3d0) )
      end if

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*(2d0*upv + 2d0*usea + dnv + dsea)/x
      fdHe3  = 1d0/3d0*(upv + usea + 2d0*dnv + 2d0*dsea)/x
      fubHe3 = 1d0/3d0*(2d0*usea + dsea)/x
      fdbHe3 = 1d0/3d0*(usea + 2d0*dsea)/x
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x


      H = (2d0/3d0)**2d0*( (fuHe3/x)*(zD1u/z**3d0) +
     &                     (fubHe3/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdHe3/x)*(zD1d/z**3d0) +
     &                     (fdbHe3/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsHe3/x)*(zD1s/z**3d0) +
     &                     (fsbHe3/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcHe3/x)*(zD1c/z**3d0) +
     &                     (fcbHe3/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbHe3/x)*(zD1b/z**3d0) +
     &                     (fbbHe3/x)*(zD1b/z**3d0) )
      end if




      UUNLOvirInt = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)*H*
     &            CF*alphastr/Pi*(1d0/(1d0 - v)*(T/S/U))*
     &    ((1d0 + v**2d0)/(1d0 - v)**2d0)*
     &    (  (3d0 + 2d0*dlog(v))*dlog((1d0 - v)**2d0/v*(S*U/T/mu**2d0)) 
     &              + dlog(v)**2d0 - 8d0
     &     + 4d0*dlog(1d0 - x)**2d0 + 
     &       4d0*dlog(1d0 - x)*dlog((1d0 - v)**2d0 * S*U/T/mu**2d0) 
     &     )


      endif
      end function

! ************ Jet Production (virtual corrections) ********************

      function UUNLOvirJetInt(S,T,U,mu,R,IT)
      implicit none
      character*32 prefix
      integer IT,FINI,iset
      real*8 UUNLOvirJetInt,S,mu,v,T,U,R,x,alphaem,Pi,Conv,
     &       alphas,alphastr,CF
      real*8 H,upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD,fglD
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3,fglHe3

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)


!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0


      v = (S + T)/S
      x = (1d0 - v)/v*U/T

      if((x.ge.1d0) .or. ((S + T + U).le.0d0)) then
      UUNLOvirJetInt = 0d0

      else

      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)


      if(IT.eq.1) then
!     Proton PDFs:
      H = (2d0/3d0)**2d0*( (upv + 2d0*usea)/x ) +
     &    (1d0/3d0)**2d0*( (dnv + 2d0*dsea)/x ) +
     &    (1d0/3d0)**2d0*( (str + sbar)/x ) +
     &    (2d0/3d0)**2d0*( (chm + cbar)/x ) +
     &    (1d0/3d0)**2d0*( (bot + bbar)/x )

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*( (upv + usea)/x + (dnv + dsea)/x )
      fdD  = 1d0/2d0*( (upv + usea)/x + (dnv + dsea)/x )
      fubD = usea/x
      fdbD = dsea/x
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x


      H = (2d0/3d0)**2d0*( fuD + fubD ) +
     &    (1d0/3d0)**2d0*( fdD + fdbD ) +
     &    (1d0/3d0)**2d0*( fsD + fsbD ) +
     &    (2d0/3d0)**2d0*( fcD + fcbD ) +
     &    (1d0/3d0)**2d0*( fbD + fbbD )
      end if

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*( 2d0*(upv + usea)/x + (dnv + dsea)/x )
      fdHe3  = 1d0/3d0*( (upv + usea)/x + 2d0*(dnv + dsea)/x )
      fubHe3 = usea/x
      fdbHe3 = dsea/x
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x


      H = (2d0/3d0)**2d0*( fuHe3 + fubHe3 ) +
     &    (1d0/3d0)**2d0*( fdHe3 + fdbHe3 ) +
     &    (1d0/3d0)**2d0*( fsHe3 + fsbHe3 ) +
     &    (2d0/3d0)**2d0*( fcHe3 + fcbHe3 ) +
     &    (1d0/3d0)**2d0*( fbHe3 + fbbHe3 )
      end if




      UUNLOvirJetInt = Conv*
     &   alphaem**2d0/S*H*
     &            CF*alphastr/Pi*(1d0/(1d0 - v)*(T/S/U))*
     &    ((1d0 + v**2d0)/(1d0 - v)**2d0)*
     &    (1d0*( (3d0 + 2d0*dlog(v))*
     &             dlog((1d0 - v)**2d0/v*(S*U/T/mu**2d0)) 
     &              + dlog(v)**2d0 - 8d0
     &     + 4d0*dlog(1d0 - x)**2d0 + 
     &       4d0*dlog(1d0 - x)*dlog((1d0 - v)**2d0 * S*U/T/mu**2d0) )
!
!          Jet's Specific Contributions
!
     &     - (3d0/2d0 + 2d0*dlog(v))*dlog((T*U)/(S*mu**2d0)*R**2d0)
     &     - 2d0*dlog(v)**2d0 + 13d0/2d0 - 2d0/3d0*Pi**2d0
     &     - 2d0*dlog(1d0 - x)**2d0 
     &     - 2d0*dlog(1d0 - x)*dlog((v**2d0*T*U)/(S*mu**2d0)*R**2d0) )


      endif
      end function


! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function UUNLOvirFTM1Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UUNLOvirFTM1Aux,Y(*),UUNLOvirInt,S,T,U,mu,v,xF,PT,xT,Pi
      common / varUUFTM1NLOvir / S,xF,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      UUNLOvirFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             UUNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UUNLOvirFTM1(Ss,xFf,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOvirFTM1,Ss,xFf,PTt,muu,
     &       UUNLOvirFTM1Aux,S,xF,PT,mu,xT,T,U
      Common / varUUFTM1NLOvir / S,xF,PT,mu,IH,IC,IT
      External UUNLOvirFTM1Aux

      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UUNLOvirFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOvirFTM1 = out
      end function

! **********  E155 *****************************************************

      function UUNLOvirFTM2Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UUNLOvirFTM2Aux,Y(*),UUNLOvirInt,S,T,U,mu,v,P,th,M,Pi
      common / varUUFTM2NLOvir / S,P,th,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      UUNLOvirFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             UUNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UUNLOvirFTM2(Ss,Pp,thh,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOvirFTM2,Ss,Pp,thh,muu,
     &       UUNLOvirFTM1Aux,S,P,th,mu,M,Pi,T,U
      Common / varUUFTM2NLOvir / S,P,th,mu,IH,IC,IT
      External UUNLOvirFTM2Aux

      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UUNLOvirFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOvirFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC,EIC(Jets) ***********************************

      function UUNLOvirCMAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UUNLOvirCMAux,Y(*),UUNLOvirInt,S,T,U,mu,v,eta,PT,Pi
      common / varUUCMNLOvir / S,eta,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUNLOvirCMAux = 2d0*Pi*PT* UUNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UUNLOvirCM(Ss,etaa,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOvirCM,Ss,etaa,PTt,muu,UUNLOvirCMAux,S,eta,PT,mu,T,U
      Common / varUUCMNLOvir / S,eta,PT,mu,IH,IC,IT
      External UUNLOvirCMAux

      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UUNLOvirCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOvirCM = out
      end function

      function UUNLOvirJetCM(S,eta,PT,mu,R,IT)
      implicit none
      integer IT
      real*8 UUNLOvirJetCM,UUNLOvirJetInt,S,eta,PT,mu,T,U,R,Pi

      Pi = 2d0*dasin(1d0)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)


      UUNLOvirJetCM = 2d0*Pi*PT*UUNLOvirJetInt(S,T,U,mu,R,IT)
      end function

! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function UUNLOvirFTM1PTAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UUNLOvirFTM1PTAux,Y(*),UUNLOvirInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       PTmin,PTmax
      real*8 a
      common / varUUFTM1PTNLOvir / S,xF,PTmin,PTmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      PT = Y(2)
      mu = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUNLOvirFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   UUNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UUNLOvirFTM1PT(Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOvirFTM1PT,Ss,xFf,PTminn,PTmaxx,
     &       UUNLOvirFTM1PTAux,S,xF,PTmin,PTmax
      Common / varUUFTM1PTNLOvir / S,xF,PTmin,PTmax,IH,IC,IT
      External UUNLOvirFTM1PTAux

      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = PTmin
      region(3) = 1d0
      region(4) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UUNLOvirFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOvirFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function UUNLOvirFTM1xFAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UUNLOvirFTM1xFAux,Y(*),UUNLOvirInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       xFmin,xFmax
      real*8 a
      common / varUUFTM1xFNLOvir / S,PT,mu,xFmin,xFmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      xF = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUNLOvirFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   UUNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UUNLOvirFTM1xF(Ss,PTt,muu,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOvirFTM1xF,Ss,PTt,muu,xFminn,xFmaxx,UUNLOvirFTM1xFAux,
     &       S,PT,mu,xFmin,xFmax
      Common / varUUFTM1xFNLOvir / S,PT,mu,xFmin,xFmax,IH,IC,IT
      External UUNLOvirFTM1xFAux

      S     = Ss
      PT    = PTt
      mu    = muu
      xFmin = xFminn
      xFmax = xFmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = xFmin
      region(3) = 1d0
      region(4) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UUNLOvirFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOvirFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

! **********  eta - bins  **********************************************

      function UUNLOvirCMetaAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 UUNLOvirCMetaAux,Y(*),UUNLOvirInt,S,T,U,mu,v,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varUUCMetaNLOvir / S,PT,mu,etamin,etamax,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUNLOvirCMetaAux = 2d0*Pi*PT*
     &         ((S+T)/S - U/(T+U)) *   UUNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function UUNLOvirCMeta(Ss,PTt,muu,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOvirCMeta,Ss,PTt,muu,etaminn,etamaxx,UUNLOvirCMetaAux,
     &       S,PT,mu,etamin,etamax
      Common / varUUCMetaNLOvir / S,PT,mu,etamin,etamax,IH,IC,IT
      External UUNLOvirCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UUNLOvirCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOvirCMeta = out
      end function

      function UUNLOvirJetCMetaAux(Y)
      implicit none
      integer IT
      real*8 UUNLOvirJetCMetaAux,Y(*),UUNLOvirJetInt,S,T,U,mu,eta,PT,Pi,
     &       R,etamin,etamax
      common / varUUCMetaNLOvirJet / S,PT,mu,R,etamin,etamax,IT

      Pi  = 2d0*dasin(1d0)
      eta = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUNLOvirJetCMetaAux = 2d0*Pi*PT**5d0*UUNLOvirJetInt(S,T,U,mu,R,IT)
      end function

      function UUNLOvirJetCMeta(Ss,PTt,muu,Rr,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOvirJetCMeta,Ss,PTt,muu,Rr,etaminn,etamaxx,
     &       UUNLOvirJetCMetaAux,S,PT,mu,R,etamin,etamax
      Common / varUUCMetaNLOvirJet / S,PT,mu,R,etamin,etamax,IT
      External UUNLOvirJetCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      R      = Rr
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = etamin
      region(2) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,UUNLOvirJetCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOvirJetCMeta = out
      end function


!***********************************************************************
!****** NEXT-TO-LEADING ORDER (real corrections)  **********************
!***********************************************************************
!     flag n = 1: q -> q, n = 2: q -> g, n = 3: g -> q, n = 4: total

      function UUNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      implicit none
      character*32 prefix 
      integer n,IH,IC,IT,FINI,iset
      real*8 UUNLOrealInt,S,T,U,mu,v,w,x,z,
     &       alphaem,alphastr,Pi,Conv
      real*8 upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 upv1,dnv1,usea1,dsea1,str1,sbar1,
     &       chm1,cbar1,bot1,bbar1,glu1,phot1
      real*8 zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl
      real*8 CF,TR,Hqq,Hqg,Hgq,UUNLOrealqqInt,
     &        UUNLOrealqgInt,UUNLOrealgqInt,alphas,Hqq1,xb
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD,fglD
      real*8 fuD1,fdD1,fubD1,fdbD1,fsD1,fsbD1,fcD1,fcbD1,fbD1,
     &       fbbD1,fglD1
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3,fglHe3
      real*8 fuHe31,fdHe31,fubHe31,fdbHe31,fsHe31,fsbHe31,fcHe31,
     &       fcbHe31,fbHe31,fbbHe31,fglHe31
      real*8 Aqq1,Bqq1,Bqq2,Bqq3,Cqq1,Cqq2,Cqq3,Cqq4,Cqq5,
     &       Cqg1,Cqg2,Cqg3,Cqg4,Cgq1,Cgq2,Cgq3,Aqq11,Bqq11,Bqq21,Bqq31
      common/ FRAGINID / FINI

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      x = (1d0 - v)/v/w*U/T
      xb= (1d0 - v)/v*U/T
      z = -T/(1d0 - v)/S

      if((x.ge.1d0) .or. (z.ge.1d0) .or. (w.lt.xb) 
     &                     .or. ((S + T + U).le.0d0)) then
      UUNLOrealInt = 0d0
      else 

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if

      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)

      call GetAllPDFs(prefix,iset,xb,mu,
     &     upv1,dnv1,usea1,dsea1,str1,sbar1,chm1,cbar1,
     &     bot1,bbar1,glu1,phot1)
      

      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( ((upv + usea)/x**2d0)*(zD1u/z**3d0) +
     &                     (usea/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((dnv + dsea)/x**2d0)*(zD1d/z**3d0) +
     &                     (dsea/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (str/x**2d0)*(zD1s/z**3d0) +
     &                     (sbar/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (chm/x**2d0)*(zD1c/z**3d0) +
     &                     (cbar/x**2d0)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (bot/x**2d0)*(zD1b/z**3d0) +
     &                     (bbar/x**2d0)*(zD1b/z**3d0) )

      Hqq1 = (2d0/3d0)**2d0*( ((upv1 + usea1)/xb**2d0)*(zD1u/z**3d0) +
     &                     (usea1/xb**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((dnv1 + dsea1)/xb**2d0)*(zD1d/z**3d0) +
     &                     (dsea1/xb**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (str1/xb**2d0)*(zD1s/z**3d0) +
     &                     (sbar1/xb**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (chm1/xb**2d0)*(zD1c/z**3d0) +
     &                     (cbar1/xb**2d0)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (bot1/xb**2d0)*(zD1b/z**3d0) +
     &                     (bbar1/xb**2d0)*(zD1b/z**3d0) )

      Hqg = (2d0/3d0)**2d0*( ((upv + usea)/x**2d0)*(zD1gl/z**3d0) +
     &                     (usea/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((dnv + dsea)/x**2d0)*(zD1gl/z**3d0) +
     &                     (dsea/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (str/x**2d0)*(zD1gl/z**3d0) +
     &                     (sbar/x**2d0)*(zD1gl/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (chm/x**2d0)*(zD1gl/z**3d0) +
     &                     (cbar/x**2d0)*(zD1gl/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (bot/x**2d0)*(zD1gl/z**3d0) +
     &                     (bbar/x**2d0)*(zD1gl/z**3d0) )

      Hgq = (2d0/3d0)**2d0*( (glu/x**2d0)*(zD1u/z**3d0) +
     &                     (glu/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (glu/x**2d0)*(zD1d/z**3d0) +
     &                     (glu/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (glu/x**2d0)*(zD1s/z**3d0) +
     &                     (glu/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (glu/x**2d0)*(zD1c/z**3d0) +
     &                     (glu/x**2d0)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (glu/x**2d0)*(zD1b/z**3d0) +
     &                     (glu/x**2d0)*(zD1b/z**3d0) )    

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fdD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fubD = 1d0/2d0*( (usea + dsea)/x )
      fdbD = 1d0/2d0*( (usea + dsea)/x )
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x
      fglD = glu/x

      fuD1  = 1d0/2d0*( (upv1 + usea1 + dnv1 + dsea1)/xb )
      fdD1  = 1d0/2d0*( (upv1 + usea1 + dnv1 + dsea1)/xb )
      fubD1 = 1d0/2d0*( (usea1 + dsea1)/xb )
      fdbD1 = 1d0/2d0*( (usea1 + dsea1)/xb )
      fsD1  = str1/xb
      fsbD1 = sbar1/xb
      fcD1  = chm1/xb
      fcbD1 = cbar1/xb
      fbD1  = bot1/xb
      fbbD1 = bbar1/xb
      fglD1 = glu1/xb


      Hqq = (2d0/3d0)**2d0*( (fuD/x)*(zD1u/z**3d0) +
     &                     (fubD/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdD/x)*(zD1d/z**3d0) +
     &                     (fdbD/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsD/x)*(zD1s/z**3d0) +
     &                     (fsbD/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcD/x)*(zD1c/z**3d0) +
     &                     (fcbD/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbD/x)*(zD1b/z**3d0) +
     &                     (fbbD/x)*(zD1b/z**3d0) )

      Hqq1 = (2d0/3d0)**2d0*( (fuD1/xb)*(zD1u/z**3d0) +
     &                     (fubD1/xb)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdD1/xb)*(zD1d/z**3d0) +
     &                     (fdbD1/xb)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsD1/xb)*(zD1s/z**3d0) +
     &                     (fsbD1/xb)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcD1/xb)*(zD1c/z**3d0) +
     &                     (fcbD1/xb)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbD1/xb)*(zD1b/z**3d0) +
     &                     (fbbD1/xb)*(zD1b/z**3d0) )

      Hqg = (2d0/3d0)**2d0*( (fuD/x)*(zD1gl/z**3d0) +
     &                     (fubD/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdD/x)*(zD1gl/z**3d0) +
     &                     (fdbD/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsD/x)*(zD1gl/z**3d0) +
     &                     (fsbD/x)*(zD1gl/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcD/x)*(zD1gl/z**3d0) +
     &                     (fcbD/x)*(zD1gl/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbD/x)*(zD1gl/z**3d0) +
     &                     (fbbD/x)*(zD1gl/z**3d0) )

      Hgq = (2d0/3d0)**2d0*( (fglD/x)*(zD1u/z**3d0) +
     &                     (fglD/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglD/x)*(zD1d/z**3d0) +
     &                     (fglD/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglD/x)*(zD1s/z**3d0) +
     &                     (fglD/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fglD/x)*(zD1c/z**3d0) +
     &                     (fglD/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fglD/x)*(zD1b/z**3d0) +
     &                     (fglD/x)*(zD1b/z**3d0) )    
      endif

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*( (2d0*upv + 2d0*usea + dnv + dsea)/x )
      fdHe3  = 1d0/3d0*( (upv + usea + 2d0*dnv + 2d0*dsea)/x)
      fubHe3 = 1d0/3d0*( (2d0*usea + dsea)/x )
      fdbHe3 = 1d0/3d0*( (usea + 2d0*dsea)/x)
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x
      fglHe3 = glu/x

      fuHe31  = 1d0/3d0*( (2d0*upv1 + 2d0*usea1 + dnv1 + dsea1)/xb )
      fdHe31  = 1d0/3d0*( (upv1 + usea1 + 2d0*dnv1 + 2d0*dsea1)/xb )
      fubHe31 = 1d0/3d0*( (2d0*usea1 + dsea1)/xb )
      fdbHe31 = 1d0/3d0*( (usea1 + 2d0*dsea1)/xb )
      fsHe31  = str1/xb
      fsbHe31 = sbar1/xb
      fcHe31  = chm1/xb
      fcbHe31 = cbar1/xb
      fbHe31  = bot1/xb
      fbbHe31 = bbar1/xb
      fglHe31 = glu1/xb


      Hqq = (2d0/3d0)**2d0*( (fuHe3/x)*(zD1u/z**3d0) +
     &                     (fubHe3/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdHe3/x)*(zD1d/z**3d0) +
     &                     (fdbHe3/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsHe3/x)*(zD1s/z**3d0) +
     &                     (fsbHe3/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcHe3/x)*(zD1c/z**3d0) +
     &                     (fcbHe3/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbHe3/x)*(zD1b/z**3d0) +
     &                     (fbbHe3/x)*(zD1b/z**3d0) )

      Hqq1 = (2d0/3d0)**2d0*( (fuHe31/xb)*(zD1u/z**3d0) +
     &                     (fubHe31/xb)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdHe31/xb)*(zD1d/z**3d0) +
     &                     (fdbHe31/xb)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsHe31/xb)*(zD1s/z**3d0) +
     &                     (fsbHe31/xb)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcHe31/xb)*(zD1c/z**3d0) +
     &                     (fcbHe31/xb)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbHe31/xb)*(zD1b/z**3d0) +
     &                     (fbbHe31/xb)*(zD1b/z**3d0) )

      Hqg = (2d0/3d0)**2d0*( (fuHe3/x)*(zD1gl/z**3d0) +
     &                     (fubHe3/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fdHe3/x)*(zD1gl/z**3d0) +
     &                     (fdbHe3/x)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fsHe3/x)*(zD1gl/z**3d0) +
     &                     (fsbHe3/x)*(zD1gl/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fcHe3/x)*(zD1gl/z**3d0) +
     &                     (fcbHe3/x)*(zD1gl/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fbHe3/x)*(zD1gl/z**3d0) +
     &                     (fbbHe3/x)*(zD1gl/z**3d0) )

      Hgq = (2d0/3d0)**2d0*( (fglHe3/x)*(zD1u/z**3d0) +
     &                     (fglHe3/x)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglHe3/x)*(zD1d/z**3d0) +
     &                     (fglHe3/x)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (fglHe3/x)*(zD1s/z**3d0) +
     &                     (fglHe3/x)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (fglHe3/x)*(zD1c/z**3d0) +
     &                     (fglHe3/x)*(zD1c/z**3d0) )  +
     &    (1d0/3d0)**2d0*( (fglHe3/x)*(zD1b/z**3d0) +
     &                     (fglHe3/x)*(zD1b/z**3d0) )    
      endif




      Aqq1 = (8d0*(1d0 + v**2d0)*w)/((-1d0 + v)**2d0)

      Aqq11 = (8d0*(1d0 + v**2d0))/((-1d0 + v)**2d0)
      

      Bqq1 = (4d0*w*(1d0 + v*(-1d0 + w) + 
     -      v**2d0*(1d0 - w + w**2d0)))/
     -  ((-1d0 + v)**2d0)

      Bqq11 = 4d0*(1d0 + v**2d0)/(1d0 - v)**2d0

      Bqq2 = (2d0*(2d0 + 2d0*v*(-1d0 + w) + v**2d0*(-1d0 + w)**2d0)*w*
     -    (1d0 + 2d0*v*(-1d0 + w) + 
     -      v**2d0*(1d0 - 2d0*w + 2d0*w**2d0)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w)))

      Bqq21 = 4d0/(1d0 - v)**2d0*(1d0 + v**2d0)

      Bqq3 = (4d0*(1d0 + v**2d0)*w)/((-1d0 + v)**2d0)

      Bqq31 = 4d0*(1d0 + v**2d0)/(1d0 - v)**2d0


      Cqq1 = (-2d0 + w + v*(2d0 + 8d0*w) + 
     -    v**4d0*w*(-2d0 + 2d0*w + 7d0*w**2d0 - 8d0*w**3d0) + 
     -    2d0*v**5d0*w**2d0*(1d0 - 3d0*w + 4d0*w**2d0 - 2d0*w**3d0) - 
     -    v**2d0*(2d0 + 9d0*w - 10d0*w**2d0 + w**3d0) - 
     -    2d0*v**3d0*(-1d0 + w - w**2d0 + 4d0*w**3d0))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))*(-1d0 + v*w))

      Cqq2 = (2d0*(1d0 + v**2d0*(1d0 + 2d0*w**2d0)))/(-1d0 + v)**2d0

      Cqq3 = (-2d0*v*w*(3d0 + 2d0*v*(-1d0 + w) + 
     -      v**2d0*(1d0 - 2d0*w + 2d0*w**2d0)))/(-1d0 + v)**2d0

      Cqq4 = (-2d0 + w + v*(2d0 + 4d0*w) + 
     -    v**4d0*w*(-2d0 + 2d0*w + 3d0*w**2d0 - 4d0*w**3d0) + 
     -    2d0*v**5d0*w**2d0*(1d0 - 3d0*w + 4d0*w**2d0 - 2d0*w**3d0) - 
     -    v**2d0*(2d0 + 5d0*w - 6d0*w**2d0 + w**3d0) - 
     -    2d0*v**3d0*(-1d0 + w - w**2d0 + 2d0*w**3d0))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))*(-1d0 + v*w))

      Cqq5 = (-2d0 + 2d0*v**3d0 - 2d0*v*(-2d0 + w) + w + 
     -    v**4d0*w*(-2d0 + w**2d0) + 
     -    v**2d0*(-4d0 + w + 2d0*w**2d0 - w**3d0) - 
     -    2d0*v**5d0*w**2d0*(-1d0 + 2d0*w - 2d0*w**2d0 + w**3d0))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))*(-1d0 + v*w))


      Cqg1 =  (2d0*v*(1d0 + v**2d0*(-1d0 + w)**2d0)*w*
     -    (1d0 + 2d0*v*(-1d0 + w) + 
     -      v**2d0*(1d0 + 2d0*(-1d0 + w)*w)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))**2d0)

      Cqg2 = (2d0*v*w*(3d0 - 2d0*v*w + 
     -      v**2d0*(1d0 - 2d0*w + 2d0*w**2d0)))/(-1d0 + v)**2d0

      Cqg3 = (v*w*(3d0 - 2d0*v*(3d0 + w) + 
     -      v**2d0*(6d0 + 4d0*w - w**2d0) + 
     -      2d0*v**6d0*(-1d0 + w)**2d0*w**2d0*
     -       (1d0 - 2d0*w + 2d0*w**2d0) + 
     -      v**4d0*(3d0 - 4d0*w + 5d0*w**2d0 - 2d0*w**3d0) + 
     -      2d0*v**3d0*(-3d0 + 3d0*w - 5d0*w**2d0 + 2d0*w**3d0) - 
     -      2d0*v**5d0*w*
     -       (2d0 - 6d0*w + 9d0*w**2d0 - 7d0*w**3d0 + 2d0*w**4d0)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))**2d0*
     -    (-1d0 + v*w)**2d0)

      Cqg4 = (v*w*(2d0 + 2d0*v*(-5d0 + 3d0*w) + 
     -      v**2d0*(16d0 - 3d0*w - 11d0*w**2d0) + 
     -      2d0*v**6d0*(-1d0 + w)**2d0*w**2d0*
     -       (1d0 - w + w**2d0) - 
     -      v**3d0*(10d0 + 15d0*w - 27d0*w**2d0 + 2d0*w**3d0) + 
     -      v**4d0*(2d0 + 17d0*w - 23d0*w**2d0 + 7d0*w**3d0 - 
     -         3d0*w**4d0) + 
     -      v**5d0*w*(-5d0 + 5d0*w + w**2d0 - 3d0*w**3d0 + 
     -         2d0*w**4d0)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))**2d0*
     -    (-1d0 + v*w)**2d0)


      Cgq1 = (2d0*(1d0 + v*(v - 2d0*(1d0 + v)*w + 4d0*v*w**2d0)))/
     -  (-1d0 + v)**2d0


      Cgq2 = (2d0*(1d0 - w + w**2d0) + 
     -    3d0*v**4d0*w**2d0*(1d0 - 2d0*w + 2d0*w**2d0) - 
     -    2d0*v*w*(3d0 - 2d0*w + 2d0*w**2d0) - 
     -    4d0*v**3d0*w*(1d0 - 2d0*w + 3d0*w**2d0) + 
     -    v**2d0*(2d0 - 4d0*w + 11d0*w**2d0 - 2d0*w**3d0 + 2d0*w**4d0)
     -    )/((-1d0 + v)**2d0*(-1d0 + v*w)**2d0)

      Cgq3 = (1d0 + 4d0*w - 6d0*w**2d0 + 
     -    2d0*v*(-1d0 - 3d0*w - w**2d0 + 6d0*w**3d0) + 
     -    v**3d0*w*(-3d0 - 9d0*w + 4d0*w**2d0 + 6d0*w**3d0) + 
     -    v**2d0*(1d0 + 9d0*w + 4d0*w**2d0 - 8d0*w**3d0 - 
     -       6d0*w**4d0) + v**4d0*(w**2d0 + 4d0*w**3d0 - 4d0*w**4d0)
     -    )/((-1d0 + v)**2d0*(-1d0 + v*w)**2d0)


   

      UUNLOrealqqInt = Conv*
     &            (-U/S**2d0)*1d0/v/(1d0 - v)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &       dlog(1d0 - w)/(1d0 - w)*(Aqq1/w**2d0*Hqq - Aqq11*Hqq1)
     &       + 1d0/(1d0 - w)*(
     &       Bqq1*dlog((1d0 - v)/v/(1d0 - v*(1d0 - w)))/w**2d0*Hqq 
     &                                  - Bqq11*dlog((1d0 - v)/v)*Hqq1
     &       + Bqq2*dlog(1d0 - v*(1d0 - w))/w**2d0*Hqq
     &       + Bqq3*dlog((1d0 - v)*v/w*(S*U/T/mu**2d0))/w**2d0*Hqq
     &       - Bqq31*dlog((1d0 - v)*v*(S*U/T/mu**2d0))*Hqq1)
     &       + Cqq1*dlog(v*(1d0 - w))/w**2d0*Hqq
     &       + Cqq2*dlog((1d0 - v)*w/(1d0 - v*w))/w**2d0*Hqq
     & + Cqq3*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w**2d0*Hqq
     &       + Cqq4*dlog((1d0 - v)/v/w*(S*U/T/mu**2d0))/w**2d0*Hqq
     &       + Cqq5/w**2d0*Hqq)


      UUNLOrealqgInt = Conv*
     &          (-U/S**2d0)*1d0/v/(1d0 - v)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cqg1*dlog(1d0 - v*(1d0 - w))/w**2d0*Hqg
     & + Cqg2*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w**2d0*Hqg
     & + Cqg3*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w**2d0*Hqg
     & + Cqg4/w**2d0*Hqg   )


      UUNLOrealgqInt = Conv*
     &               (-U/S**2d0)*1d0/v/(1d0 - v)*(TR*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cgq1*dlog((1d0 - v)*w/(1d0 - v*w))/w**2d0*Hgq
     & + Cgq2*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w**2d0*Hgq
     & + Cgq3/w**2d0*Hgq   )


      if(n.eq.1) then
      UUNLOrealInt = UUNLOrealqqInt
      end if
      
      if(n.eq.2) then
      UUNLOrealInt = UUNLOrealqgInt
      end if

      if(n.eq.3) then
      UUNLOrealInt = UUNLOrealgqInt
      end if

      if(n.eq.4) then
      UUNLOrealInt = UUNLOrealqqInt + UUNLOrealqgInt + UUNLOrealgqInt
      end if

      end if
      end function

! ********** Jet Production (real corrections) *************************

      function UUNLOrealJetInt(n,S,T,U,mu,R,IT,w)
      implicit none
      character*32 prefix
      integer n,IT,iset
      real*8 UUNLOrealJetInt,S,T,U,mu,R,v,w,x,
     &       alphaem,alphastr,Pi,Conv
      real*8 upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot
      real*8 upv1,dnv1,usea1,dsea1,str1,sbar1,
     &       chm1,cbar1,bot1,bbar1,glu1,phot1
      real*8 CF,TR,Hqq,Hqg,Hgq,UUNLOrealJetqqInt,
     &        UUNLOrealJetqgInt,UUNLOrealJetgqInt,alphas,Hqq1,xb
      real*8 fuD,fdD,fubD,fdbD,fsD,fsbD,fcD,fcbD,fbD,fbbD,fglD
      real*8 fuD1,fdD1,fubD1,fdbD1,fsD1,fsbD1,fcD1,fcbD1,fbD1,
     &       fbbD1,fglD1
      real*8 fuHe3,fdHe3,fubHe3,fdbHe3,fsHe3,fsbHe3,fcHe3,fcbHe3,
     &       fbHe3,fbbHe3,fglHe3
      real*8 fuHe31,fdHe31,fubHe31,fdbHe31,fsHe31,fsbHe31,fcHe31,
     &       fcbHe31,fbHe31,fbbHe31,fglHe31
      real*8 Aqq1,Bqq1,Bqq2,Bqq3,Cqq1,Cqq2,Cqq3,Cqq4,Cqq5,
     &       Cqg1,Cqg2,Cqg3,Cqg4,Cgq1,Cgq2,Cgq3,Aqq11,Bqq11,Bqq21,Bqq31
      real*8 AqqJet,Aqq1Jet,BqqJet,Bqq1Jet,CqqJet,BqgJet,CqgJet


      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      v = (S + T)/S
      x = (1d0 - v)/v/w*U/T
      xb= (1d0 - v)/v*U/T

      if((x.ge.1d0) .or. ((S + T + U).le.0d0) .or. (w.lt.xb)) then
      UUNLOrealJetInt = 0d0
      else 
      
      iset = 0
      prefix = "mstw2008nlo"

      call GetAllPDFs(prefix,iset,x,mu,
     &     upv,dnv,usea,dsea,str,sbar,chm,cbar,bot,bbar,glu,phot)

      call GetAllPDFs(prefix,iset,xb,mu,
     &     upv1,dnv1,usea1,dsea1,str1,sbar1,
     &     chm1,cbar1,bot1,bbar1,glu1,phot1)


      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( (upv + 2d0*usea)/x ) +
     &    (1d0/3d0)**2d0*( (dnv + 2d0*dsea)/x ) +
     &    (1d0/3d0)**2d0*( (str + sbar)/x ) +
     &    (2d0/3d0)**2d0*( (chm + cbar)/x )  +
     &    (1d0/3d0)**2d0*( (bot + bbar)/x )

      Hqq1 = (2d0/3d0)**2d0*( (upv1 + 2d0*usea1)/xb ) +
     &    (1d0/3d0)**2d0*( (dnv1 + 2d0*dsea1)/xb ) +
     &    (1d0/3d0)**2d0*( (str1 + sbar1)/xb ) +
     &    (2d0/3d0)**2d0*( (chm1 + cbar1)/xb )  +
     &    (1d0/3d0)**2d0*( (bot1 + bbar1)/xb )

      Hqg = (2d0/3d0)**2d0*( (upv + 2d0*usea)/x ) +
     &    (1d0/3d0)**2d0*( (dnv + 2d0*dsea)/x ) +
     &    (1d0/3d0)**2d0*( (str + sbar)/x ) +
     &    (2d0/3d0)**2d0*( (chm + cbar)/x ) +
     &    (1d0/3d0)**2d0*( (bot + bbar)/x )

!    Work with Nf = 4 active flavors

      Hgq = 2d0*(glu/x)*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &    (1d0/3d0)**2d0 + (2d0/3d0)**2d0  )    

      endif

      if(IT.eq.2) then
!     Deuteron PDFs:
      fuD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fdD  = 1d0/2d0*( (upv + usea + dnv + dsea)/x )
      fubD = 1d0/2d0*( (usea + dsea)/x )
      fdbD = 1d0/2d0*( (usea + dsea)/x )
      fsD  = str/x
      fsbD = sbar/x
      fcD  = chm/x
      fcbD = cbar/x
      fbD  = bot/x
      fbbD = bbar/x
      fglD = glu/x

      fuD1  = 1d0/2d0*( (upv1 + usea1 + dnv1 + dsea1)/xb )
      fdD1  = 1d0/2d0*( (upv1 + usea1 + dnv1 + dsea1)/xb )
      fubD1 = 1d0/2d0*( (usea1 + dsea1)/xb )
      fdbD1 = 1d0/2d0*( (usea1 + dsea1)/xb )
      fsD1  = str1/xb
      fsbD1 = sbar1/xb
      fcD1  = chm1/xb
      fcbD1 = cbar1/xb
      fbD1  = bot1/xb
      fbbD1 = bbar1/xb
      fglD1 = glu1/xb


      Hqq = (2d0/3d0)**2d0*( fuD + fubD ) +
     &    (1d0/3d0)**2d0*( fdD + fdbD ) +
     &    (1d0/3d0)**2d0*( fsD + fsbD ) +
     &    (2d0/3d0)**2d0*( fcD + fcbD ) +
     &    (1d0/3d0)**2d0*( fbD + fbbD )

      Hqq1 = (2d0/3d0)**2d0*( fuD1 + fubD1 ) +
     &    (1d0/3d0)**2d0*( fdD1 + fdbD1 ) +
     &    (1d0/3d0)**2d0*( fsD1 + fsbD1 ) +
     &    (2d0/3d0)**2d0*( fcD1 + fcbD1 ) +
     &    (1d0/3d0)**2d0*( fbD1 + fbbD1 )

      Hqg = (2d0/3d0)**2d0*( fuD + fubD ) +
     &    (1d0/3d0)**2d0*( fdD + fdbD ) +
     &    (1d0/3d0)**2d0*( fsD + fsbD ) +
     &    (2d0/3d0)**2d0*( fcD + fcbD ) +
     &    (1d0/3d0)**2d0*( fbD + fbbD )

!    Work with Nf = 4 active flavors

      Hgq = 2d0*fglD*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &    (1d0/3d0)**2d0 + (2d0/3d0)**2d0  )    
      endif

      if(IT.eq.3) then
!     Helium3 PDFs:
      fuHe3  = 1d0/3d0*( (2d0*upv + 2d0*usea + dnv + dsea)/x )
      fdHe3  = 1d0/3d0*( (upv + usea + 2d0*dnv + 2d0*dsea)/x )
      fubHe3 = 1d0/3d0*( (2d0*usea + dsea)/x )
      fdbHe3 = 1d0/3d0*( (usea + 2d0*dsea)/x )
      fsHe3  = str/x
      fsbHe3 = sbar/x
      fcHe3  = chm/x
      fcbHe3 = cbar/x
      fbHe3  = bot/x
      fbbHe3 = bbar/x
      fglHe3 = glu/x

      fuHe31  = 1d0/3d0*( (2d0*upv1 + 2d0*usea1 + dnv1 + dsea1)/xb )
      fdHe31  = 1d0/3d0*( (upv1 + usea1 + 2d0*dnv1 + 2d0*dsea1)/xb )
      fubHe31 = 1d0/3d0*( (2d0*usea1 + dsea1)/xb )
      fdbHe31 = 1d0/3d0*( (usea1 + 2d0*dsea1)/xb )
      fsHe31  = str1/xb
      fsbHe31 = sbar1/xb
      fcHe31  = chm1/xb
      fcbHe31 = cbar1/xb
      fbHe31  = bot1/xb
      fbbHe31 = bbar1/xb
      fglHe31 = glu1/xb


      Hqq = (2d0/3d0)**2d0*( fuHe3 + fubHe3 ) +
     &    (1d0/3d0)**2d0*( fdHe3 + fdbHe3 ) +
     &    (1d0/3d0)**2d0*( fsHe3 + fsbHe3 ) +
     &    (2d0/3d0)**2d0*( fcHe3 + fcbHe3 ) +
     &    (1d0/3d0)**2d0*( fbHe3 + fbbHe3 )

      Hqg = (2d0/3d0)**2d0*( fuHe3 + fubHe3 ) +
     &    (1d0/3d0)**2d0*( fdHe3 + fdbHe3 ) +
     &    (1d0/3d0)**2d0*( fsHe3 + fsbHe3 ) +
     &    (2d0/3d0)**2d0*( fcHe3 + fcbHe3 ) +
     &    (1d0/3d0)**2d0*( fbHe3 + fbbHe3 )

!    Work with Nf = 4 active flavors

      Hgq = 2d0*fglHe3*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &    (1d0/3d0)**2d0 + (2d0/3d0)**2d0  )    
      endif




      Aqq1 = (8d0*(1d0 + v**2d0)*w)/((-1d0 + v)**2d0)

      Aqq11 = (8d0*(1d0 + v**2d0))/((-1d0 + v)**2d0)

      AqqJet = - 4d0*w*(1d0 - 2d0*v*(1d0 - w) + 
     &           v**2d0*(w**2d0 + (1d0 - w)**2d0))/(1d0 - v)**2d0

      Aqq1Jet = -4d0*(1d0 + v**2d0)/(1d0 - v)**2d0
      

      Bqq1 = (4d0*w*(1d0 + v*(-1d0 + w) + 
     -      v**2d0*(1d0 - w + w**2d0)))/
     -  ((-1d0 + v)**2d0)

      Bqq11 = 4d0*(1d0 + v**2d0)/(1d0 - v)**2d0

      Bqq2 = (2d0*(2d0 + 2d0*v*(-1d0 + w) + v**2d0*(-1d0 + w)**2d0)*w*
     -    (1d0 + 2d0*v*(-1d0 + w) + 
     -      v**2d0*(1d0 - 2d0*w + 2d0*w**2d0)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w)))

      Bqq21 = 4d0/(1d0 - v)**2d0*(1d0 + v**2d0)

      Bqq3 = (4d0*(1d0 + v**2d0)*w)/((-1d0 + v)**2d0)

      Bqq31 = 4d0*(1d0 + v**2d0)/(1d0 - v)**2d0

      BqqJet = - 2d0*w*(1d0 - 2d0*v*(1d0 - w) + 
     &           v**2d0*(w**2d0 + (1d0 - w)**2d0))/(1d0 - v)**2d0 * 
     &           dlog((v**2d0*T*U)/(S*mu**2d0)*R**2d0)

      Bqq1Jet = -2d0*(1d0 + v**2d0)/(1d0 - v)**2d0 * 
     &           dlog((v**2d0*T*U)/(S*mu**2d0)*R**2d0)


      Cqq1 = (-2d0 + w + v*(2d0 + 8d0*w) + 
     -    v**4d0*w*(-2d0 + 2d0*w + 7d0*w**2d0 - 8d0*w**3d0) + 
     -    2d0*v**5d0*w**2d0*(1d0 - 3d0*w + 4d0*w**2d0 - 2d0*w**3d0) - 
     -    v**2d0*(2d0 + 9d0*w - 10d0*w**2d0 + w**3d0) - 
     -    2d0*v**3d0*(-1d0 + w - w**2d0 + 4d0*w**3d0))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))*(-1d0 + v*w))

      Cqq2 = (2d0*(1d0 + v**2d0*(1d0 + 2d0*w**2d0)))/(-1d0 + v)**2d0

      Cqq3 = (-2d0*v*w*(3d0 + 2d0*v*(-1d0 + w) + 
     -      v**2d0*(1d0 - 2d0*w + 2d0*w**2d0)))/(-1d0 + v)**2d0

      Cqq4 = (-2d0 + w + v*(2d0 + 4d0*w) + 
     -    v**4d0*w*(-2d0 + 2d0*w + 3d0*w**2d0 - 4d0*w**3d0) + 
     -    2d0*v**5d0*w**2d0*(1d0 - 3d0*w + 4d0*w**2d0 - 2d0*w**3d0) - 
     -    v**2d0*(2d0 + 5d0*w - 6d0*w**2d0 + w**3d0) - 
     -    2d0*v**3d0*(-1d0 + w - w**2d0 + 2d0*w**3d0))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))*(-1d0 + v*w))

      Cqq5 = (-2d0 + 2d0*v**3d0 - 2d0*v*(-2d0 + w) + w + 
     -    v**4d0*w*(-2d0 + w**2d0) + 
     -    v**2d0*(-4d0 + w + 2d0*w**2d0 - w**3d0) - 
     -    2d0*v**5d0*w**2d0*(-1d0 + 2d0*w - 2d0*w**2d0 + w**3d0))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))*(-1d0 + v*w))

      CqqJet = - (1d0 - 2d0*v*(1d0 - w) + 
     &           v**2d0*(w**2d0 + (1d0 - w)**2d0))/(1d0 - v)**2d0 *
     &           w*v**2d0*(1d0 - w)/(1d0 - v*(1d0 - w)) *
     &     (1d0 + dlog((v**2d0*(1d0 - w)**2d0*T*U)/(S*mu**2d0)*R**2d0) )


      Cqg1 =  (2d0*v*(1d0 + v**2d0*(-1d0 + w)**2d0)*w*
     -    (1d0 + 2d0*v*(-1d0 + w) + 
     -      v**2d0*(1d0 + 2d0*(-1d0 + w)*w)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))**2d0)

      Cqg2 = (2d0*v*w*(3d0 - 2d0*v*w + 
     -      v**2d0*(1d0 - 2d0*w + 2d0*w**2d0)))/(-1d0 + v)**2d0

      Cqg3 = (v*w*(3d0 - 2d0*v*(3d0 + w) + 
     -      v**2d0*(6d0 + 4d0*w - w**2d0) + 
     -      2d0*v**6d0*(-1d0 + w)**2d0*w**2d0*
     -       (1d0 - 2d0*w + 2d0*w**2d0) + 
     -      v**4d0*(3d0 - 4d0*w + 5d0*w**2d0 - 2d0*w**3d0) + 
     -      2d0*v**3d0*(-3d0 + 3d0*w - 5d0*w**2d0 + 2d0*w**3d0) - 
     -      2d0*v**5d0*w*
     -       (2d0 - 6d0*w + 9d0*w**2d0 - 7d0*w**3d0 + 2d0*w**4d0)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))**2d0*
     -    (-1d0 + v*w)**2d0)

      Cqg4 = (v*w*(2d0 + 2d0*v*(-5d0 + 3d0*w) + 
     -      v**2d0*(16d0 - 3d0*w - 11d0*w**2d0) + 
     -      2d0*v**6d0*(-1d0 + w)**2d0*w**2d0*
     -       (1d0 - w + w**2d0) - 
     -      v**3d0*(10d0 + 15d0*w - 27d0*w**2d0 + 2d0*w**3d0) + 
     -      v**4d0*(2d0 + 17d0*w - 23d0*w**2d0 + 7d0*w**3d0 - 
     -         3d0*w**4d0) + 
     -      v**5d0*w*(-5d0 + 5d0*w + w**2d0 - 3d0*w**3d0 + 
     -         2d0*w**4d0)))/
     -  ((-1d0 + v)**2d0*(1d0 + v*(-1d0 + w))**2d0*
     -    (-1d0 + v*w)**2d0)

      BqgJet = - (1d0 - 2d0*v*(1d0 - w) + 
     &           v**2d0*(w**2d0 + (1d0 - w)**2d0))/(1d0 - v)**2d0 * 
     &  v*w*(1d0 + v**2d0*(1d0 - w)**2d0)/(1d0 - v*(1d0 - w))**2d0 * 
     &            dlog((v**2d0*(1d0 - w)**2d0*T*U)/(S*mu**2d0)*R**2d0)

      CqgJet = - (1d0 - 2d0*v*(1d0 - w) + 
     &           v**2d0*(w**2d0 + (1d0 - w)**2d0))/(1d0 - v)**2d0*v*w


      Cgq1 = (2d0*(1d0 + v*(v - 2d0*(1d0 + v)*w + 4d0*v*w**2d0)))/
     -  (-1d0 + v)**2d0


      Cgq2 = (2d0*(1d0 - w + w**2d0) + 
     -    3d0*v**4d0*w**2d0*(1d0 - 2d0*w + 2d0*w**2d0) - 
     -    2d0*v*w*(3d0 - 2d0*w + 2d0*w**2d0) - 
     -    4d0*v**3d0*w*(1d0 - 2d0*w + 3d0*w**2d0) + 
     -    v**2d0*(2d0 - 4d0*w + 11d0*w**2d0 - 2d0*w**3d0 + 2d0*w**4d0)
     -    )/((-1d0 + v)**2d0*(-1d0 + v*w)**2d0)

      Cgq3 = (1d0 + 4d0*w - 6d0*w**2d0 + 
     -    2d0*v*(-1d0 - 3d0*w - w**2d0 + 6d0*w**3d0) + 
     -    v**3d0*w*(-3d0 - 9d0*w + 4d0*w**2d0 + 6d0*w**3d0) + 
     -    v**2d0*(1d0 + 9d0*w + 4d0*w**2d0 - 8d0*w**3d0 - 
     -       6d0*w**4d0) + v**4d0*(w**2d0 + 4d0*w**3d0 - 4d0*w**4d0)
     -    )/((-1d0 + v)**2d0*(-1d0 + v*w)**2d0)


   

      UUNLOrealJetqqInt = Conv*
     &            (1d0/S)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*( 1d0*(
     &       dlog(1d0 - w)/(1d0 - w)*(Aqq1/w*Hqq - Aqq11*Hqq1)
     &       + 1d0/(1d0 - w)*(
     &       Bqq1*dlog((1d0 - v)/v/(1d0 - v*(1d0 - w)))/w*Hqq 
     &                                  - Bqq11*dlog((1d0 - v)/v)*Hqq1
     &       + Bqq2*dlog(1d0 - v*(1d0 - w))/w*Hqq
     &       + Bqq3*dlog((1d0 - v)*v/w*(S*U/T/mu**2d0))/w*Hqq
     &       - Bqq31*dlog((1d0 - v)*v*(S*U/T/mu**2d0))*Hqq1)
     &       + Cqq1*dlog(v*(1d0 - w))/w*Hqq
     &       + Cqq2*dlog((1d0 - v)*w/(1d0 - v*w))/w*Hqq
     & + Cqq3*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w*Hqq
     &       + Cqq4*dlog((1d0 - v)/v/w*(S*U/T/mu**2d0))/w*Hqq
     &       + Cqq5/w*Hqq )
!
!     Jet's Specific Contributions
!  
     &        + (dlog(1d0 - w)/(1d0 - w)*(AqqJet/w*Hqq - Aqq1Jet*Hqq1)
     &        + 1d0/(1d0 - w)*(BqqJet/w*Hqq - Bqq1Jet*Hqq1) 
     &        + CqqJet/w*Hqq ) )


      UUNLOrealJetqgInt = Conv*
     &          (1d0/S)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cqg1*dlog(1d0 - v*(1d0 - w))/w*Hqg
     & + Cqg2*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w*Hqg
     & + Cqg3*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w*Hqg
     & + Cqg4/w*Hqg   
!
!     Jet's Specific Contributions
!
     & + BqgJet/w*Hqg + CqgJet/w*Hqg)
!     & )

      UUNLOrealJetgqInt = Conv*
     &               (1d0/S)*(TR*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cgq1*dlog((1d0 - v)*w/(1d0 - v*w))/w*Hgq
     & + Cgq2*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w*Hgq
     & + Cgq3/w*Hgq   )


      if(n.eq.1) then
      UUNLOrealJetInt = UUNLOrealJetqqInt
      end if
      
      if(n.eq.2) then
      UUNLOrealJetInt = UUNLOrealJetqgInt
      end if

      if(n.eq.3) then
      UUNLOrealJetInt = UUNLOrealJetgqInt
      end if

      if(n.eq.4) then
      UUNLOrealJetInt = 
     &        UUNLOrealJetqqInt + UUNLOrealJetqgInt + UUNLOrealJetgqInt
      end if

      end if
      end function

      
! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function UUNLOrealFTM1Aux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 UUNLOrealFTM1Aux,Y(*),UUNLOrealInt,S,T,U,mu,v,w,xF,PT,xT,Pi
      common / varUUFTM1NLOreal / S,xF,PT,mu,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      UUNLOrealFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             UUNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function UUNLOrealFTM1(nn,Ss,xFf,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealFTM1,Ss,xFf,PTt,muu,UUNLOrealFTM1Aux,S,xF,PT,mu,
     &       xT,T,U
      Common / varUUFTM1NLOreal / S,xF,PT,mu,n,IH,IC,IT
      External UUNLOrealFTM1Aux

      n   = nn
      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealFTM1 = out
      end function

! **********  E155 *****************************************************

      function UUNLOrealFTM2Aux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 UUNLOrealFTM2Aux,Y(*),UUNLOrealInt,S,T,U,mu,v,w,P,th,M,Pi
      common / varUUFTM2NLOreal / S,P,th,mu,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)
      w = Y(2)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      UUNLOrealFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             UUNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function UUNLOrealFTM2(nn,Ss,Pp,thh,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealFTM2,Ss,Pp,thh,muu,UUNLOrealFTM1Aux,S,P,th,mu,
     &       M,Pi,T,U
      Common / varUUFTM2NLOreal / S,P,th,mu,n,IH,IC,IT
      External UUNLOrealFTM2Aux

      n   = nn
      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

      function UUNLOrealCMAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 UUNLOrealCMAux,Y(*),UUNLOrealInt,S,T,U,mu,v,w,eta,PT,Pi
      common / varUUCMNLOreal / S,eta,PT,mu,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUNLOrealCMAux = 2d0*Pi*PT* 
     &                     UUNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function UUNLOrealCM(nn,Ss,etaa,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealCM,Ss,etaa,PTt,muu,UUNLOrealCMAux,S,eta,PT
     &       ,mu,T,U
      Common / varUUCMNLOreal / S,eta,PT,mu,n,IH,IC,IT
      External UUNLOrealCMAux

      n   = nn
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealCM = out
      end function

!  ********  Jets  *****************************************************

      function UUNLOrealCMJetAux(Y)
      implicit none
      integer n,IT
      real*8 UUNLOrealCMJetAux,Y(*),UUNLOrealJetInt,S,T,U,mu,R,w,
     &       eta,PT,Pi
      common / varNLOrealCMJet / S,eta,PT,mu,R,n,IT

      Pi = 2d0*dasin(1d0)
      w = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUNLOrealCMJetAux = 2d0*Pi*PT* 
     &                 UUNLOrealJetInt(n,S,T,U,mu,R,IT,w)
      end function

      function UUNLOrealCMJet(nn,Ss,etaa,PTt,muu,Rr,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealCMJet,Ss,etaa,PTt,muu,Rr,UUNLOrealCMJetAux,
     &       S,eta,PT,mu,R,T,U
      Common / varNLOrealCMJet / S,eta,PT,mu,R,n,IT
      External UUNLOrealCMJetAux

      n   = nn
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      R   = Rr
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = 0d0
      region(2) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealCMJetAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealCMJet = out
      end function


! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function UUNLOrealFTM1PTAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 UUNLOrealFTM1PTAux,Y(*),UUNLOrealInt,S,T,U,mu,v,w,
     &       xF,PT,xT,Pi,PTmin,PTmax
      real*8 a
      common / varUUFTM1PTNLOreal / S,xF,PTmin,PTmax,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      PT = Y(3)
      mu = Y(3)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUNLOrealFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &      ((S+T)/S - U/(T+U)) *UUNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function UUNLOrealFTM1PT(nn,Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealFTM1PT,Ss,xFf,PTminn,PTmaxx,UUNLOrealFTM1PTAux,
     &       S,xF,PTmin,PTmax
      Common / varUUFTM1PTNLOreal / S,xF,PTmin,PTmax,n,IH,IC,IT
      External UUNLOrealFTM1PTAux

      n     = nn
      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = PTmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function UUNLOrealFTM1xFAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 UUNLOrealFTM1xFAux,Y(*),UUNLOrealInt,S,T,U,mu,v,w,
     &       xF,PT,xT,Pi,xFmin,xFmax
      real*8 a
      common / varUUFTM1xFNLOreal / S,PT,mu,xFmin,xFmax,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      xF = Y(3)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUNLOrealFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &     ((S+T)/S - U/(T+U)) * UUNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function UUNLOrealFTM1xF(nn,Ss,PTt,muu,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealFTM1xF,Ss,PTt,muu,xFminn,xFmaxx,
     &       UUNLOrealFTM1xFAux,S,PT,mu,xFmin,xFmax
      Common / varUUFTM1xFNLOreal / S,PT,mu,xFmin,xFmax,n,IH,IC,IT
      External UUNLOrealFTM1xFAux

      n     = nn
      S     = Ss
      PT    = PTt
      mu    = muu
      xFmin = xFminn
      xFmax = xFmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = xFmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC *********************************************

! **********  eta - bins  ***********************************************

      function UUNLOrealCMetaAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 UUNLOrealCMetaAux,Y(*),UUNLOrealInt,S,T,U,mu,v,w,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varUUCMetaNLOreal / S,PT,mu,etamin,etamax,n,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      w   = Y(2)
      eta = Y(3)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      UUNLOrealCMetaAux = 2d0*Pi*PT*
     &      ((S+T)/S - U/(T+U)) *UUNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function UUNLOrealCMeta(nn,Ss,PTt,muu,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealCMeta,Ss,PTt,muu,etaminn,etamaxx,
     &       UUNLOrealCMetaAux,S,PT,mu,etamin,etamax
      Common / varUUCMetaNLOreal / S,PT,mu,etamin,etamax,n,IH,IC,IT
      External UUNLOrealCMetaAux

      n      = nn
      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = etamin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealCMeta = out
      end function


! ********  Jets  ******************************************************

      function UUNLOrealCMJetetaAux(Y)
      implicit none
      integer n,IT
      real*8 UUNLOrealCMJetetaAux,Y(*),UUNLOrealJetInt,S,T,U,mu,R,w,
     &       eta,PT,Pi,etamin,etamax
      common / varUUCMJetetaNLOreal / S,PT,mu,R,etamin,etamax,n,IT

      Pi  = 2d0*dasin(1d0)
      w   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      UUNLOrealCMJetetaAux = 2d0*Pi*PT**5d0*
     &      UUNLOrealJetInt(n,S,T,U,mu,R,IT,w)
      end function

      function UUNLOrealCMJeteta(nn,Ss,PTt,muu,Rr,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,ITt,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 UUNLOrealCMJeteta,Ss,PTt,muu,Rr,etaminn,etamaxx,
     &       UUNLOrealCMJetetaAux,S,PT,mu,R,etamin,etamax
      Common / varUUCMJetetaNLOreal / S,PT,mu,R,etamin,etamax,n,IT
      External UUNLOrealCMJetetaAux

      n      = nn
      S      = Ss
      PT     = PTt
      mu     = muu
      R      = Rr
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,UUNLOrealCMJetetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      UUNLOrealCMJeteta = out
      end function





!***********************************************************************
!     DOUBLE LONGITUDINALLY POLARIZED CROSS SECTION
!***********************************************************************


!     LEADING ORDER
      function LLLOInt(S,T,U,mu,IH,IC,IT,v)
      implicit none
      integer IH,IC,IT,FINI
      real*8 LLLOInt,S,T,U,mu,v,x,z,alphaem,Pi,Conv
      real*8 H,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl
      real*8 xg1uv,xg1ub,xg1dv,xg1db,xg1s,xg1gl
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3
      common/ FRAGINID / FINI

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0


      x = (1d0 - v)/v*U/T
      z = -T/(1d0 - v)/S

      if((x.ge.1d0) .or. (z.ge.1d0) .or. ((S + T + U).le.0d0)) then
      LLLOInt = 0d0

      else

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if

      

!      call DSSVFIT(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

!     Helium3 helicity pdfs

      xguHe3  = 1d0/1d0*(0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*(0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      if(IT.eq.1) then

      H = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x**2d0)*(zD1u/z**3d0) +
     &                     (xg1ub/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x**2d0)*(zD1d/z**3d0) +
     &                     (xg1db/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x**2d0)*(zD1s/z**3d0) +
     &                     (xg1s/x**2d0)*(zD1sb/z**3d0) ) 

      endif

      if(IT.eq.2) then
      
      H = (2d0/3d0)**2d0*( (xguD/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubD/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbD/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsD/x**2d0)*(zD1sb/z**3d0) ) 

      endif

      if(IT.eq.3) then
      
      H = (2d0/3d0)**2d0*( (xguHe3/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubHe3/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbHe3/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsHe3/x**2d0)*(zD1sb/z**3d0) ) 

      endif


      LLLOInt = Conv*(-U/S**2d0)*
     &   alphaem**2d0*
     &   (1d0/v/(1d0 - v))*(T/(1d0 - v)/S/U)*H*2d0*(1d0 + v)/(1d0 - v)

      end if
      end function

!*************  LEADING ORDER (Jet Production) *************************
      function LLLOJetInt(S,T,U,mu,IT)
      implicit none
      integer IT,FINI
      real*8 LLLOJetInt,S,T,U,mu,v,x,alphaem,Pi,Conv
      real*8 H,Ctq6Pdf
      real*8 xg1uv,xg1ub,xg1dv,xg1db,xg1s,xg1gl
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0

      x = -U/(S+T)
      v = 1d0 + T/S

      if((x.ge.1d0).or. ((S+T+U).le.0d0)) then
      LLLOJetInt = 0d0

      else

!      call DSSVFIT(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

!     Helium3 helicity pdfs

      xguHe3  = 1d0/1d0*(0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*(0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      if(IT.eq.1) then

      H = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x) + (xg1ub/x) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x) + (xg1db/x) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x) +  (xg1s/x) ) 

      endif

      if(IT.eq.2) then
      
      H = (2d0/3d0)**2d0*( (xguD/x) + (xgubD/x) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x) + (xgdbD/x) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x) + (xgsD/x) ) 

      endif

      if(IT.eq.3) then
      
      H = (2d0/3d0)**2d0*( (xguHe3/x) + (xgubHe3/x) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x) + (xgdbHe3/x) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x) + (xgsHe3/x) ) 

      endif



      LLLOJetInt = Conv* (alphaem**2d0)/S*
     &    (T/(1d0 - v)/S/U) * H 
     &     *2d0*(1d0 + v)/(1d0 - v)

      end if
      end function


! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function LLLOFTM1Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLLOFTM1Aux,Y(*),LLLOInt,S,T,U,mu,v,xF,PT,xT,Pi
      common / varLLFTM1LO / S,xF,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      LLLOFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             LLLOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLLOFTM1(Ss,xFf,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLLOFTM1,Ss,xFf,PTt,muu,LLLOFTM1Aux,S,xF,PT,mu,xT,T,U
      Common / varLLFTM1LO / S,xF,PT,mu,IH,IC,IT
      External LLLOFTM1Aux

      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLLOFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLLOFTM1 = out
      end function

! **********  E155 *****************************************************

      function LLLOFTM2Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLLOFTM2Aux,Y(*),LLLOInt,S,T,U,mu,v,P,th,M,Pi
      common / varLLFTM2LO / S,P,th,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      LLLOFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             LLLOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLLOFTM2(Ss,Pp,thh,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLLOFTM2,Ss,Pp,thh,muu,LLLOFTM1Aux,S,P,th,mu,M,Pi,T,U
      Common / varLLFTM2LO / S,P,th,mu,IH,IC,IT
      External LLLOFTM2Aux

      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLLOFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLLOFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jet) ***********************************

      function LLLOCMAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLLOCMAux,Y(*),LLLOInt,S,T,U,mu,v,eta,PT,Pi
      common / varLLCMLO / S,eta,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLLOCMAux = 2d0*Pi*PT* LLLOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLLOCM(Ss,etaa,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLLOCM,Ss,etaa,PTt,muu,LLLOFTM1Aux,S,eta,PT,mu,T,U
      Common / varLLCMLO / S,eta,PT,mu,IH,IC,IT
      External LLLOCMAux

      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLLOCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLLOCM = out
      end function


      function LLLOJetCM(S,eta,PT,mu,IT)
      implicit none
      integer IT
      real*8 LLLOJetCM,LLLOJetInt,S,eta,PT,mu,T,U,Pi

      Pi = 2d0*dasin(1d0)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)


      LLLOJetCM = 2d0*Pi*PT*LLLOJetInt(S,T,U,mu,IT)
      end function

! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function LLLOFTM1PTAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLLOFTM1PTAux,Y(*),LLLOInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       PTmin,PTmax
      real*8 a
      common / varLLFTM1PTLO / S,xF,PTmin,PTmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      PT = Y(2)
      mu = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLLOFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   LLLOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLLOFTM1PT(Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLLOFTM1PT,Ss,xFf,PTminn,PTmaxx,LLLOFTM1PTAux,S,xF,PTmin,
     &       PTmax
      Common / varLLFTM1PTLO / S,xF,PTmin,PTmax,IH,IC,IT
      External LLLOFTM1PTAux

      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = PTmin
      region(3) = 1d0
      region(4) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLLOFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLLOFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function LLLOFTM1xFAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLLOFTM1xFAux,Y(*),LLLOInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       xFmin,xFmax
      real*8 a
      common / varLLFTM1xFLO / S,PT,mu,xFmin,xFmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      xF = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLLOFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   LLLOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLLOFTM1xF(Ss,PTt,muu,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLLOFTM1xF,Ss,PTt,muu,xFminn,xFmaxx,LLLOFTM1xFAux,
     &       S,PT,mu,xFmin,xFmax
      Common / varLLFTM1xFLO / S,PT,mu,xFmin,xFmax,IH,IC,IT
      External LLLOFTM1xFAux

      S     = Ss
      PT    = PTt
      mu    = muu
      xFmin = xFminn
      xFmax = xFmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = xFmin
      region(3) = 1d0
      region(4) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLLOFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLLOFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

! **********  eta - bins  **********************************************

      function LLLOCMetaAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLLOCMetaAux,Y(*),LLLOInt,S,T,U,mu,v,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varLLCMetaLO / S,PT,mu,etamin,etamax,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLLOCMetaAux = 2d0*Pi*PT*
     &         ((S+T)/S - U/(T+U)) *   LLLOInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLLOCMeta(Ss,PTt,muu,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLLOCMeta,Ss,PTt,muu,etaminn,etamaxx,LLLOCMetaAux,
     &       S,PT,mu,etamin,etamax
      Common / varLLCMetaLO / S,PT,mu,etamin,etamax,IH,IC,IT
      External LLLOCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLLOCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLLOCMeta = out
      end function


      function LLLOJetCMetaAux(Y)
      implicit none
      integer IT
      real*8 LLLOJetCMetaAux,Y(*),LLLOJetInt,S,T,U,mu,eta,PT,Pi,
     &       etamin,etamax
      common / varLLCMetaLOJet / S,PT,mu,etamin,etamax,IT

      Pi  = 2d0*dasin(1d0)
      eta = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLLOJetCMetaAux = 2d0*Pi*PT**5d0*LLLOJetInt(S,T,U,mu,IT)
      end function

      function LLLOJetCMeta(Ss,PTt,muu,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLLOJetCMeta,Ss,PTt,muu,etaminn,etamaxx,LLLOJetCMetaAux,
     &       S,PT,mu,etamin,etamax
      Common / varLLCMetaLOJet / S,PT,mu,etamin,etamax,IT
      External LLLOJetCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = etamin
      region(2) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLLOJetCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLLOJetCMeta = out
      end function


! **********************************************************************
!     WEIZSÄCKER - WILLIAMS CONTRIBUTION
!     flag n = 1: q -> q, n = 2: q -> g, n = 3: g -> q, n = 4: total
! **********************************************************************


      function LLWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      implicit none
      integer n,nl,IH,IC,IT,FINI
      real*8 LLWWInt,S,T,U,mu,mu0,v,w,x,z,alphaem,alphastr,Pi,Conv
      real*8 zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl,Ctq6Pdf
      real*8 xg1uv,xg1ub,xg1dv,xg1db,xg1s,xg1gl
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3
      real*8 g1WW,CF,TR,Hqq,Hqg,Hgq,LLWWqq,LLWWqg,LLWWgq,alphas
      common/ FRAGINID / FINI

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      x = (1d0 - v)/v/w*U/T
      z = -T/(1d0 - v)/S

      if((w.lt.(1d0 - v)/v*U/T) .or. (x.ge.1d0) .or. (z.ge.1d0) 
     &                   .or. ((S + T + U).le.0d0)) then
      LLWWInt = 0d0
      else 

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if

      

!      call DSSVFIT(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

!     He3 helicity pdfs
      xguHe3  = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x**2d0)*(zD1u/z**3d0) +
     &                     (xg1ub/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x**2d0)*(zD1d/z**3d0) +
     &                     (xg1db/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x**2d0)*(zD1s/z**3d0) +
     &                     (xg1s/x**2d0)*(zD1sb/z**3d0) ) 

      Hqg = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x**2d0)*(zD1gl/z**3d0) +
     &                     (xg1ub/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x**2d0)*(zD1gl/z**3d0) +
     &                     (xg1db/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x**2d0)*(zD1gl/z**3d0) +
     &                     (xg1s/x**2d0)*(zD1gl/z**3d0) ) 

      Hgq = (2d0/3d0)**2d0*((xg1gl/x**2d0)*(zD1u/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1d/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1s/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1c/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1c/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1b/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1b/z**3d0) )



      endif

      if(IT.eq.2) then
      
      Hqq = (2d0/3d0)**2d0*( (xguD/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubD/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbD/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsD/x**2d0)*(zD1sb/z**3d0) ) 

      Hqg = (2d0/3d0)**2d0*( (xguD/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgubD/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgdbD/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgsD/x**2d0)*(zD1gl/z**3d0) ) 

      Hgq = (2d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1u/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1d/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1s/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1c/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1c/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1b/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1b/z**3d0) )

      endif

      if(IT.eq.3) then
      
      Hqq = (2d0/3d0)**2d0*( (xguHe3/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubHe3/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbHe3/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsHe3/x**2d0)*(zD1sb/z**3d0) ) 

      Hqg = (2d0/3d0)**2d0*( (xguHe3/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgubHe3/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgdbHe3/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgsHe3/x**2d0)*(zD1gl/z**3d0) ) 

      Hgq = (2d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1u/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1d/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1s/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1c/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1c/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1b/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1b/z**3d0) ) 

      endif


      LLWWqq = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)/w**2d0*Hqq*
     &   g1WW((1d0 - v)/(1d0 -v*w),mu0,nl)*
     &   CF*alphastr/(Pi)*(1d0/((1d0 - v)**2d0/v/w*S*U/T))*
     &       (1d0 - v**2d0*w**2d0)/(v*w)

      LLWWqg = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)/w**2d0*Hqg*
     &   g1WW((1d0 - v)/(1d0 -v*w),mu0,nl)*
     &   CF*alphastr/(Pi)*(1d0/((1d0 - v)**2d0/v/w*S*U/T))*
     &   (v*w*(2d0 - v*w))/(1d0 - v*w)

      LLWWgq = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)/w**2d0*Hgq*
     &   g1WW((1d0 - v)/(1d0 -v*w),mu0,nl)*
     &   TR*alphastr/(Pi)*(1d0/((1d0 - v)**2d0/v/w*S*U/T))*
     &   (-1d0)*(v**2d0*w**2d0 + (1d0 - v*w)**2d0)/v/w/(1d0 - v*w)

      if(n.eq.1) then
      LLWWInt = LLWWqq
      end if
      
      if(n.eq.2) then
      LLWWInt = LLWWqg
      end if

      if(n.eq.3) then
      LLWWInt = LLWWgq
      end if

      if(n.eq.4) then
      LLWWInt = LLWWqq + LLWWqg + LLWWgq
      end if

      end if
      end function

! ********** Jet Production ********************************************

      function LLWWJetInt(n,nl,S,T,U,mu,mu0,IT,w)
      implicit none
      integer n,nl,IT,FINI
      real*8 LLWWJetInt,S,T,U,mu,mu0,v,w,x,alphaem,alphastr,Pi,Conv
      real*8 Ctq6Pdf
      real*8 g1WW,CF,TR,Hqq,Hqg,Hgq,LLWWJetqq,LLWWJetqg,LLWWJetgq,alphas
      real*8 xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3


      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      x = -U/w/(S+T)
      v = 1d0 + T/S

      if((w.lt.(-U/(S+T))) .or. (x.ge.1d0) .or. ((S+T+U).lt.0d0) ) then
      LLWWJetInt = 0d0
      else 


!      call DSSVFIT(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

!     He3 helicity pdfs
      xguHe3  = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x) + (xg1ub/x) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x) + (xg1db/x) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x) + (xg1s/x) ) 

      Hqg = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x) + (xg1ub/x) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x) + (xg1db/x)) +
     &    (1d0/3d0)**2d0*( (xg1s/x) + (xg1s/x) ) 

!     Work with Nf = 4 active flavors

      Hgq = 2d0*(xg1gl/x)*((2d0/3d0)**2d0 + 
     &               (1d0/3d0)**2d0 + (1d0/3d0)**2d0 + (2d0/3d0)**2d0)

      end if

      if(IT.eq.2) then

      Hqq = (2d0/3d0)**2d0*( (xguD + xgubD)/x ) +
     &    (1d0/3d0)**2d0*( (xgdD + xgdbD)/x ) +
     &    (1d0/3d0)**2d0*( (xgsD/x) + (xgsD/x) ) 

      Hqg = (2d0/3d0)**2d0*( (xguD + xgubD)/x ) +
     &    (1d0/3d0)**2d0*( (xgdD + xgdbD)/x ) +
     &    (1d0/3d0)**2d0*( (xgsD/x) + (xgsD/x) )  

!     Work with Nf = 4 active flavors

      Hgq = 2d0*(xgglD/x)*((2d0/3d0)**2d0 + 
     &               (1d0/3d0)**2d0 + (1d0/3d0)**2d0 + (2d0/3d0)**2d0)

      end if

      if(IT.eq.3) then

      Hqq = (2d0/3d0)**2d0*( (xguHe3 + xgubHe3)/x ) +
     &    (1d0/3d0)**2d0*( (xgdHe3 + xgdbHe3)/x ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x) + (xgsHe3/x) ) 

      Hqg = (2d0/3d0)**2d0*( (xguHe3 + xgubHe3)/x ) +
     &    (1d0/3d0)**2d0*( (xgdHe3 + xgdbHe3)/x ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x) + (xgsHe3/x) )  

!     Work with Nf = 4 active flavors

      Hgq = 2d0*(xgglHe3/x)*((2d0/3d0)**2d0 + 
     &               (1d0/3d0)**2d0 + (1d0/3d0)**2d0 + (2d0/3d0)**2d0)

      end if

      

      LLWWJetqq = Conv*
     &   alphaem**2d0*(1d0/S/w)*Hqq*
     &   g1WW(-T/(S*(1d0 - w)-w*T),mu0,nl)*
     &   CF*alphastr/Pi*(1d0/((1d0 - v)**2d0/v/w*S*U/T))*
     &   (1d0 - v**2d0*w**2d0)/v/w

      LLWWJetqg = Conv*
     &   alphaem**2d0*(1d0/S/w)*Hqg*
     &   g1WW(-T/(S*(1d0 - w)-w*T),mu0,nl)*
     %   CF*alphastr/Pi*(1d0/((1d0 - v)**2d0/v/w*S*U/T))*
     &   v*w*(2d0 - v*w)/(1d0 - v*w)

      LLWWJetgq = Conv*
     &   alphaem**2d0*(1d0/S/w)*Hgq*
     &   g1WW(-T/(S*(1d0 - w)-w*T),mu0,nl)*
     &   TR*alphastr/Pi*(1d0/((1d0 - v)**2d0/v/w*S*U/T))*
     &   (-1d0)*(v**2d0*w**2d0 + (1d0 - v*w)**2d0)/(1d0 - v*w)/v/w

      if(n.eq.1) then
      LLWWJetInt = LLWWJetqq
      end if
      
      if(n.eq.2) then
      LLWWJetInt = LLWWJetqg
      end if

      if(n.eq.3) then
      LLWWJetInt = LLWWJetgq
      end if

      if(n.eq.4) then
      LLWWJetInt = LLWWJetqq + LLWWJetqg + LLWWJetgq
      end if

      end if
      end function


! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function LLWWFTM1Aux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 LLWWFTM1Aux,Y(*),LLWWInt,S,T,U,mu,mu0,v,w,xF,PT,xT,Pi
      common / varLLFTM1WW / S,xF,PT,mu,mu0,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      LLWWFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             LLWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function LLWWFTM1(nn,nll,Ss,xFf,PTt,muu,mu00,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWFTM1,Ss,xFf,PTt,muu,mu00,LLWWFTM1Aux,S,xF,PT,mu,mu0,
     &       xT,T,U
      Common / varLLFTM1WW / S,xF,PT,mu,mu0,n,nl,IH,IC,IT
      External LLWWFTM1Aux

      n   = nn
      nl  = nll
      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      mu0 = mu00
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWFTM1 = out
      end function

! **********  E155 *****************************************************

      function LLWWFTM2Aux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 LLWWFTM2Aux,Y(*),LLWWInt,S,T,U,mu,mu0,v,w,P,th,M,Pi
      common / varLLFTM2WW / S,P,th,mu,mu0,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)
      w = Y(2)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      LLWWFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             LLWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function LLWWFTM2(nn,nll,Ss,Pp,thh,muu,mu00,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWFTM2,Ss,Pp,thh,muu,mu00,LLWWFTM1Aux,S,P,th,mu,mu0,
     &       M,Pi,T,U
      Common / varLLFTM2WW / S,P,th,mu,mu0,n,nl,IH,IC,IT
      External LLWWFTM2Aux

      n   = nn
      nl  = nll
      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      mu0 = mu00
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

      function LLWWCMAux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 LLWWCMAux,Y(*),LLWWInt,S,T,U,mu,mu0,v,w,eta,PT,Pi
      common / varWWCMLOLL / S,eta,PT,mu,mu0,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLWWCMAux = 2d0*Pi*PT* LLWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function LLWWCM(nn,nll,Ss,etaa,PTt,muu,mu00,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWCM,Ss,etaa,PTt,muu,mu00,LLWWCMAux,S,eta,PT,mu,mu0,T,U
      Common / varWWCMLOLL / S,eta,PT,mu,mu0,n,nl,IH,IC,IT
      External LLWWCMAux

      n   = nn
      nl  = nll
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      mu0 = mu00
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWCM = out
      end function

!  ********  Jets  *****************************************************

      function LLWWCMJetAux(Y)
      implicit none
      integer n,nl,IT
      real*8 LLWWCMJetAux,Y(*),LLWWJetInt,S,T,U,mu,mu0,w,eta,PT,Pi
      common / varWWCMJetLOLL / S,eta,PT,mu,mu0,n,nl,IT

      Pi = 2d0*dasin(1d0)
      w = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLWWCMJetAux = 2d0*Pi*PT* 
     &                 LLWWJetInt(n,nl,S,T,U,mu,mu0,IT,w)
      end function

      function LLWWCMJet(nn,nll,Ss,etaa,PTt,muu,mu00,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWCMJet,Ss,etaa,PTt,muu,mu00,LLWWCMJetAux,
     &       S,eta,PT,mu,mu0,T,U
      Common / varWWCMJetLOLL / S,eta,PT,mu,mu0,n,nl,IT
      External LLWWCMJetAux

      n   = nn
      nl  = nll
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      mu0 = mu00
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = 0d0
      region(2) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWCMJetAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWCMJet = out
      end function

! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function LLWWFTM1PTAux(Y)
      implicit none
      integer n,nl,nmu,IH,IC,IT
      real*8 LLWWFTM1PTAux,Y(*),LLWWInt,S,T,U,mu,mu0,v,w,xF,PT,xT,Pi,
     &       PTmin,PTmax
      real*8 a
      common / varLLFTM1PTWW / S,xF,PTmin,PTmax,n,nl,IH,IC,IT,nmu

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      PT = Y(3)
      mu = Y(3)

      if(nmu.eq.1) then
      mu0 = Y(3)
      else
      mu0 = dsqrt(S)/2d0
      end if

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLWWFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &      ((S+T)/S - U/(T+U)) *LLWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function LLWWFTM1PT(nn,nll,Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt,nmuu)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT,nmu,nmuu
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWFTM1PT,Ss,xFf,PTminn,PTmaxx,LLWWFTM1PTAux,S,xF,PTmin,
     &       PTmax
      Common / varLLFTM1PTWW / S,xF,PTmin,PTmax,n,nl,IH,IC,IT,nmu
      External LLWWFTM1PTAux

      n     = nn
      nl    = nll
      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt
      nmu   = nmuu

      region(1) = 0d0
      region(2) = 0d0
      region(3) = PTmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function LLWWFTM1xFAux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 LLWWFTM1xFAux,Y(*),LLWWInt,S,T,U,mu,mu0,v,w,xF,PT,xT,Pi,
     &       xFmin,xFmax
      real*8 a
      common / varLLFTM1xFWW / S,PT,mu,mu0,xFmin,xFmax,n,nl,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      xF = Y(3)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLWWFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &     ((S+T)/S - U/(T+U)) * LLWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function LLWWFTM1xF
     &                (nn,nll,Ss,PTt,muu,mu00,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWFTM1xF,Ss,PTt,muu,mu00,xFminn,xFmaxx,LLWWFTM1xFAux,
     &       S,PT,mu,mu0,xFmin,xFmax
      Common / varLLFTM1xFWW / S,PT,mu,mu0,xFmin,xFmax,n,nl,IH,IC,IT
      External LLWWFTM1xFAux

      n     = nn
      nl    = nll
      S     = Ss
      PT    = PTt
      mu    = muu
      mu0   = mu00
      xFmin = xFminn
      xFmax = xFmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = xFmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

! **********  eta - bins  **********************************************

      function LLWWCMetaAux(Y)
      implicit none
      integer n,nl,IH,IC,IT
      real*8 LLWWCMetaAux,Y(*),LLWWInt,S,T,U,mu,mu0,v,w,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varLLCMetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      w   = Y(2)
      eta = Y(3)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLWWCMetaAux = 2d0*Pi*PT*
     &      ((S+T)/S - U/(T+U)) *LLWWInt(n,nl,S,T,U,mu,mu0,IH,IC,IT,v,w)
      end function

      function LLWWCMeta
     &              (nn,nll,Ss,PTt,muu,mu00,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWCMeta,Ss,PTt,muu,mu00,etaminn,etamaxx,LLWWCMetaAux,
     &       S,PT,mu,mu0,etamin,etamax
      Common / varLLCMetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IH,IC,IT
      External LLWWCMetaAux

      n      = nn
      nl     = nll
      S      = Ss
      PT     = PTt
      mu     = muu
      mu0    = mu00
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = etamin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWCMeta = out
      end function

! ********  Jets  ******************************************************

      function LLWWCMJetetaAux(Y)
      implicit none
      integer n,nl,IT
      real*8 LLWWCMJetetaAux,Y(*),LLWWJetInt,S,T,U,mu,mu0,w,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varLLCMJetetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IT

      Pi  = 2d0*dasin(1d0)
      w   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLWWCMJetetaAux = 2d0*Pi*PT**5d0*
     &      LLWWJetInt(n,nl,S,T,U,mu,mu0,IT,w)
      end function

      function LLWWCMJeteta(nn,nll,Ss,PTt,muu,mu00,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,nl,nll,ITt,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLWWCMJeteta,Ss,PTt,muu,mu00,etaminn,etamaxx,
     &       LLWWCMJetetaAux,S,PT,mu,mu0,etamin,etamax
      Common / varLLCMJetetaWW / S,PT,mu,mu0,etamin,etamax,n,nl,IT
      External LLWWCMJetetaAux

      n      = nn
      nl     = nll
      S      = Ss
      PT     = PTt
      mu     = muu
      mu0    = mu00
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLWWCMJetetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLWWCMJeteta = out
      end function



! **********************************************************************
!     NEXT-TO-LEADING ORDER (virtual corrections)
! **********************************************************************

      function LLNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      implicit none
      integer IH,IC,IT,FINI
      real*8 LLNLOvirInt,S,T,U,mu,v,x,z,alphaem,Pi,Conv,
     &       alphas,alphastr,CF
      real*8 H,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl
      real*8 xg1uv,xg1ub,xg1dv,xg1db,xg1s,xg1gl
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3
      common/ FRAGINID / FINI

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0

      x = (1d0 - v)/v*U/T
      z = -T/(1d0 - v)/S

      if((x.ge.1d0) .or. (z.ge.1d0) .or. ((S + T + U).le.0d0)) then
      LLNLOvirInt = 0d0

      else

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if

!      call DSSVFIT(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

!     He3 helicity pdfs
      xguHe3  = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      if(IT.eq.1) then

      H = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x**2d0)*(zD1u/z**3d0) +
     &                     (xg1ub/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x**2d0)*(zD1d/z**3d0) +
     &                     (xg1db/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x**2d0)*(zD1s/z**3d0) +
     &                     (xg1s/x**2d0)*(zD1sb/z**3d0) ) 

      endif

      if(IT.eq.2) then
      
      H = (2d0/3d0)**2d0*( (xguD/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubD/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbD/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsD/x**2d0)*(zD1sb/z**3d0) ) 

      endif

      if(IT.eq.3) then
      
      H = (2d0/3d0)**2d0*( (xguHe3/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubHe3/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbHe3/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsHe3/x**2d0)*(zD1sb/z**3d0) ) 

      endif


      LLNLOvirInt = Conv*
     &   alphaem**2d0*(-U/S**2d0)*
     &   1d0/v/(1d0 - v)*H*
     &            CF*alphastr/Pi*(1d0/(1d0 - v)*(T/S/U))*
     &    ((1d0 + v)/(1d0 - v))*
     &    (  (3d0 + 2d0*dlog(v))*dlog((1d0 - v)**2d0/v*(S*U/T/mu**2d0)) 
     &              + dlog(v)**2d0 - 8d0
     &     + 4d0*dlog(1d0 - x)**2d0 + 
     &       4d0*dlog(1d0 - x)*dlog((1d0 - v)**2d0 * S*U/T/mu**2d0) 
     &     )

      endif
      end function

! ************ Jet Production (virtual corrections) ********************

      function LLNLOvirJetInt(S,T,U,mu,R,IT)
      implicit none
      integer IT,FINI
      real*8 LLNLOvirJetInt,S,mu,v,T,U,R,x,alphaem,Pi,Conv,
     &       alphas,alphastr,CF
      real*8 H
      real*8 xg1uv,xg1ub,xg1dv,xg1db,xg1s,xg1gl
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)
      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0


      v = (S + T)/S
      x = (1d0 - v)/v*U/T

      if((x.ge.1d0) .or. ((S + T + U).le.0d0)) then
      LLNLOvirJetInt = 0d0

      else

!      call DSSVFIT(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

!     He3 helicity pdfs
      xguHe3  = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      if(IT.eq.1) then

      H = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x) + (xg1ub/x) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x) + (xg1db/x) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x) + (xg1s/x) ) 

      endif

      if(IT.eq.2) then
      
      H = (2d0/3d0)**2d0*( (xguD/x) + (xgubD/x) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x) + (xgdbD/x) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x) + (xgsD/x) ) 

      endif

      if(IT.eq.3) then
      
      H = (2d0/3d0)**2d0*( (xguHe3/x) + (xgubHe3/x)) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x) + (xgdbHe3/x) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x) + (xgsHe3/x) ) 

      endif


      LLNLOvirJetInt = Conv*
     &   alphaem**2d0/S*H*
     &            CF*alphastr/Pi*(1d0/(1d0 - v)*(T/S/U))*
     &    ((1d0 + v)/(1d0 - v))*
     &    (1d0*( (3d0 + 2d0*dlog(v))*
     &             dlog((1d0 - v)**2d0/v*(S*U/T/mu**2d0)) 
     &              + dlog(v)**2d0 - 8d0
     &     + 4d0*dlog(1d0 - x)**2d0 + 
     &       4d0*dlog(1d0 - x)*dlog((1d0 - v)**2d0 * S*U/T/mu**2d0) )
!
!          Jet's Specific Contributions
!
     &     - (3d0/2d0 + 2d0*dlog(v))*dlog((T*U)/(S*mu**2d0)*R**2d0)
     &     - 2d0*dlog(v)**2d0 + 13d0/2d0 - 2d0/3d0*Pi**2d0
     &     - 2d0*dlog(1d0 - x)**2d0 
     &     - 2d0*dlog(1d0 - x)*dlog((v**2d0*T*U)/(S*mu**2d0)*R**2d0) )


      endif
      end function


! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function LLNLOvirFTM1Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLNLOvirFTM1Aux,Y(*),LLNLOvirInt,S,T,U,mu,v,xF,PT,xT,Pi
      common / varLLFTM1NLOvir / S,xF,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      LLNLOvirFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             LLNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLNLOvirFTM1(Ss,xFf,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOvirFTM1,Ss,xFf,PTt,muu,
     &       LLNLOvirFTM1Aux,S,xF,PT,mu,xT,T,U
      Common / varLLFTM1NLOvir / S,xF,PT,mu,IH,IC,IT
      External LLNLOvirFTM1Aux

      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLNLOvirFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOvirFTM1 = out
      end function

! **********  E155 *****************************************************

      function LLNLOvirFTM2Aux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLNLOvirFTM2Aux,Y(*),LLNLOvirInt,S,T,U,mu,v,P,th,M,Pi
      common / varLLFTM2NLOvir / S,P,th,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      LLNLOvirFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             LLNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLNLOvirFTM2(Ss,Pp,thh,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOvirFTM2,Ss,Pp,thh,muu,
     &       LLNLOvirFTM1Aux,S,P,th,mu,M,Pi,T,U
      Common / varLLFTM2NLOvir / S,P,th,mu,IH,IC,IT
      External LLNLOvirFTM2Aux

      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLNLOvirFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOvirFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC,EIC(Jets) ***********************************

      function LLNLOvirCMAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLNLOvirCMAux,Y(*),LLNLOvirInt,S,T,U,mu,v,eta,PT,Pi
      common / varLLCMNLOvir / S,eta,PT,mu,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLNLOvirCMAux = 2d0*Pi*PT* LLNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLNLOvirCM(Ss,etaa,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOvirCM,Ss,etaa,PTt,muu,LLNLOvirFTM1Aux,S,eta,PT,mu,T,U
      Common / varLLCMNLOvir / S,eta,PT,mu,IH,IC,IT
      External LLNLOvirCMAux

      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = (S + T)/S

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLNLOvirCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOvirCM = out
      end function

      function LLNLOvirJetCM(S,eta,PT,mu,R,IT)
      implicit none
      integer IT
      real*8 LLNLOvirJetCM,LLNLOvirJetInt,S,eta,PT,mu,T,U,R,Pi

      Pi = 2d0*dasin(1d0)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)


      LLNLOvirJetCM = 2d0*Pi*PT*LLNLOvirJetInt(S,T,U,mu,R,IT)
      end function

! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function LLNLOvirFTM1PTAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLNLOvirFTM1PTAux,Y(*),LLNLOvirInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       PTmin,PTmax
      real*8 a
      common / varLLFTM1PTNLOvir / S,xF,PTmin,PTmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      PT = Y(2)
      mu = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLNLOvirFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   LLNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLNLOvirFTM1PT(Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOvirFTM1PT,Ss,xFf,PTminn,PTmaxx,
     &       LLNLOvirFTM1PTAux,S,xF,PTmin,PTmax
      Common / varLLFTM1PTNLOvir / S,xF,PTmin,PTmax,IH,IC,IT
      External LLNLOvirFTM1PTAux

      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = PTmin
      region(3) = 1d0
      region(4) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLNLOvirFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOvirFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function LLNLOvirFTM1xFAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLNLOvirFTM1xFAux,Y(*),LLNLOvirInt,S,T,U,mu,v,xF,PT,xT,Pi,
     &       xFmin,xFmax
      real*8 a
      common / varLLFTM1xFNLOvir / S,PT,mu,xFmin,xFmax,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      xF = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLNLOvirFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &         ((S+T)/S - U/(T+U)) *   LLNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLNLOvirFTM1xF(Ss,PTt,muu,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOvirFTM1xF,Ss,PTt,muu,xFminn,xFmaxx,LLNLOvirFTM1xFAux,
     &       S,PT,mu,xFmin,xFmax
      Common / varLLFTM1xFNLOvir / S,PT,mu,xFmin,xFmax,IH,IC,IT
      External LLNLOvirFTM1xFAux

      S     = Ss
      PT    = PTt
      mu    = muu
      xFmin = xFminn
      xFmax = xFmaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = xFmin
      region(3) = 1d0
      region(4) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLNLOvirFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOvirFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

! **********  eta - bins  **********************************************

      function LLNLOvirCMetaAux(Y)
      implicit none
      integer IH,IC,IT
      real*8 LLNLOvirCMetaAux,Y(*),LLNLOvirInt,S,T,U,mu,v,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varLLCMetaNLOvir / S,PT,mu,etamin,etamax,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLNLOvirCMetaAux = 2d0*Pi*PT*
     &         ((S+T)/S - U/(T+U)) *   LLNLOvirInt(S,T,U,mu,IH,IC,IT,v)
      end function

      function LLNLOvirCMeta(Ss,PTt,muu,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum,n,nn
      integer IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOvirCMeta,Ss,PTt,muu,etaminn,etamaxx,LLNLOvirCMetaAux,
     &       S,PT,mu,etamin,etamax
      Common / varLLCMetaNLOvir / S,PT,mu,etamin,etamax,IH,IC,IT
      External LLNLOvirCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLNLOvirCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOvirCMeta = out
      end function

      function LLNLOvirJetCMetaAux(Y)
      implicit none
      integer IT
      real*8 LLNLOvirJetCMetaAux,Y(*),LLNLOvirJetInt,S,T,U,mu,eta,PT,Pi,
     &       R,etamin,etamax
      common / varLLCMetaNLOvirJet / S,PT,mu,R,etamin,etamax,IT

      Pi  = 2d0*dasin(1d0)
      eta = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLNLOvirJetCMetaAux = 2d0*Pi*PT**5d0*LLNLOvirJetInt(S,T,U,mu,R,IT)
      end function

      function LLNLOvirJetCMeta(Ss,PTt,muu,Rr,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOvirJetCMeta,Ss,PTt,muu,Rr,etaminn,etamaxx,
     &       LLNLOvirJetCMetaAux,S,PT,mu,R,etamin,etamax
      Common / varLLCMetaNLOvirJet / S,PT,mu,R,etamin,etamax,IT
      External LLNLOvirJetCMetaAux

      S      = Ss
      PT     = PTt
      mu     = muu
      R      = Rr
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = etamin
      region(2) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 30000

      call vegas(region,ndim,LLNLOvirJetCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOvirJetCMeta = out
      end function



!***********************************************************************
!****** NEXT-TO-LEADING ORDER (real corrections)  **********************
!***********************************************************************
!     flag n = 1: q -> q, n = 2: q -> g, n = 3: g -> q, n = 4: total

      function LLNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      implicit none
      integer n,IH,IC,IT,FINI
      real*8 LLNLOrealInt,S,T,U,mu,v,w,x,z,
     &       alphaem,alphastr,Pi,Conv
      real*8 zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,zD1c,zD1b,zD1gl
      real*8 CF,TR,Hqq,Hqg,Hgq,LLNLOrealqqInt,
     &        LLNLOrealqgInt,LLNLOrealgqInt,alphas,Hqq1,xb
      real*8 xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl
      real*8 xg1uv1,xg1dv1,xg1ub1,xg1db1,xg1s1,xg1gl1
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguD1,xgdD1,xgubD1,xgdbD1,xgsD1,xgglD1
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3
      real*8 xguHe31,xgdHe31,xgubHe31,xgdbHe31,xgsHe31,xgglHe31
      real*8 Aqq1,Bqq1,Bqq2,Bqq3,Cqq1,Cqq2,Cqq3,Cqq4,Cqq5,Cqq6,
     &       Cqg1,Cqg2,Cqg3,Cqg4,Cgq1,Cgq2,Cgq3,Aqq11,Bqq11,Bqq21,Bqq31
      common/ FRAGINID / FINI

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      x = (1d0 - v)/v/w*U/T
      xb= (1d0 - v)/v*U/T
      z = -T/(1d0 - v)/S

      if((x.ge.1d0) .or. (z.ge.1d0) .or. (w.lt.xb) 
     &               .or. ((S + T + U).le.0d0)) then
      LLNLOrealInt = 0d0
      else 

!     For Pions: Initiate new DSS fragmentation functions
      if(IH.eq.1) then

      call fDSSH(0,0,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      else

!     For Kaons, Protons, Charged Hadrons: Initiate old DSS fragmentation functions
      call fDSS(IH,IC,1,z,mu**2d0,zD1u,zD1ub,zD1d,zD1db,zD1s,zD1sb,
     &                            zD1c,zD1b,zD1gl)

      end if

!      call DSSVFIT(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
!      call DSSVFIT(xb,mu**2d0,xg1uv1,xg1dv1,xg1ub1,xg1db1,xg1s1,xg1gl1)

      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(xb,mu**2d0,xg1uv1,xg1dv1,xg1ub1,
     &                                              xg1db1,xg1s1,xg1gl1)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

      xguD1  = 1d0/2d0*( (xg1uv1 + xg1ub1) + (xg1dv1 + xg1db1))
      xgdD1  = 1d0/2d0*( (xg1uv1 + xg1ub1) + (xg1dv1 + xg1db1))
      xgubD1 = 1d0/2d0*( (xg1ub1) + (xg1db1))
      xgdbD1 = 1d0/2d0*( (xg1ub1) + (xg1db1))
      xgsD1  = xg1s1
      xgglD1 = xg1gl1


!     He3 helicity pdfs
      xguHe3  = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      xguHe31  = 1d0/1d0*( 0d0*(xg1uv1 + xg1ub1) + (xg1dv1 + xg1db1))
      xgdHe31  = 1d0/1d0*( (xg1uv1 + xg1ub1) + 0d0*(xg1dv1 + xg1db1))
      xgubHe31 = 1d0/1d0*( 0d0*(xg1uv1 + xg1ub1) + (xg1db1))
      xgdbHe31 = 1d0/1d0*( (xg1ub1) + 0d0*(xg1dv1 + xg1db1))
      xgsHe31  = xg1s1
      xgglHe31 = xg1gl1



      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x**2d0)*(zD1u/z**3d0) +
     &                     (xg1ub/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x**2d0)*(zD1d/z**3d0) +
     &                     (xg1db/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x**2d0)*(zD1s/z**3d0) +
     &                     (xg1s/x**2d0)*(zD1sb/z**3d0) ) 

      Hqq1= (2d0/3d0)**2d0*( ((xg1uv1 + xg1ub1)/xb**2d0)*(zD1u/z**3d0) +
     &                     (xg1ub1/xb**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv1 + xg1db1)/xb**2d0)*(zD1d/z**3d0) +
     &                     (xg1db1/xb**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1s1/xb**2d0)*(zD1s/z**3d0) +
     &                     (xg1s1/xb**2d0)*(zD1sb/z**3d0) ) 

      Hqg = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x**2d0)*(zD1gl/z**3d0) +
     &                     (xg1ub/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x**2d0)*(zD1gl/z**3d0) +
     &                     (xg1db/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x**2d0)*(zD1gl/z**3d0) +
     &                     (xg1s/x**2d0)*(zD1gl/z**3d0) ) 

      Hgq = (2d0/3d0)**2d0*((xg1gl/x**2d0)*(zD1u/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1d/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1s/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1c/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1c/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xg1gl/x**2d0)*(zD1b/z**3d0) +
     &                     (xg1gl/x**2d0)*(zD1b/z**3d0) )

      endif

      if(IT.eq.2) then
      
      Hqq = (2d0/3d0)**2d0*( (xguD/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubD/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbD/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsD/x**2d0)*(zD1sb/z**3d0) )

      Hqq1 = (2d0/3d0)**2d0*( (xguD1/xb**2d0)*(zD1u/z**3d0) +
     &                     (xgubD1/xb**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdD1/xb**2d0)*(zD1d/z**3d0) +
     &                     (xgdbD1/xb**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsD1/xb**2d0)*(zD1s/z**3d0) +
     &                     (xgsD1/xb**2d0)*(zD1sb/z**3d0) ) 

      Hqg = (2d0/3d0)**2d0*( (xguD/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgubD/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgdbD/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgsD/x**2d0)*(zD1gl/z**3d0) ) 

      Hgq = (2d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1u/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1d/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1s/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1c/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1c/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglD/x**2d0)*(zD1b/z**3d0) +
     &                     (xgglD/x**2d0)*(zD1b/z**3d0) ) 

      endif

      if(IT.eq.3) then
      
      Hqq = (2d0/3d0)**2d0*( (xguHe3/x**2d0)*(zD1u/z**3d0) +
     &                     (xgubHe3/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x**2d0)*(zD1d/z**3d0) +
     &                     (xgdbHe3/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x**2d0)*(zD1s/z**3d0) +
     &                     (xgsHe3/x**2d0)*(zD1sb/z**3d0) )

      Hqq1 = (2d0/3d0)**2d0*( (xguHe31/xb**2d0)*(zD1u/z**3d0) +
     &                     (xgubHe31/xb**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdHe31/xb**2d0)*(zD1d/z**3d0) +
     &                     (xgdbHe31/xb**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsHe31/xb**2d0)*(zD1s/z**3d0) +
     &                     (xgsHe31/xb**2d0)*(zD1sb/z**3d0) ) 

      Hqg = (2d0/3d0)**2d0*( (xguHe3/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgubHe3/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgdbHe3/x**2d0)*(zD1gl/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x**2d0)*(zD1gl/z**3d0) +
     &                     (xgsHe3/x**2d0)*(zD1gl/z**3d0) ) 

      Hgq = (2d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1u/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1ub/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1d/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1db/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1s/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1sb/z**3d0) ) +
     &    (2d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1c/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1c/z**3d0) ) +
     &    (1d0/3d0)**2d0*( (xgglHe3/x**2d0)*(zD1b/z**3d0) +
     &                     (xgglHe3/x**2d0)*(zD1b/z**3d0) ) 

      endif



      Aqq1 = 8d0*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Aqq11 = 8d0*(1d0 + v)/(1d0 - v)
      

      Bqq1 = 4d0*w*(1d0 + v*w)/(1d0 - v)

      Bqq11 = 4d0*(1d0 + v)/(1d0 - v)

      Bqq2 = 4d0*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Bqq21 = 4d0*(1d0 + v)/(1d0 - v)

      Bqq3 = 4d0*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Bqq31 = 4d0*(1d0 + v)/(1d0 - v)


      Cqq1 = 1d0/(1d0 - v)/(1d0 - v*(1d0 - w))*
     &     ( 2d0 - w + v*w*(9d0 - w) - v**2d0*(2d0 + 2d0*w - 3d0*w**2d0)
     &       - 2d0*v**3d0*w*(1d0 - 3d0*w + 2d0*w**2d0) )

      Cqq2 = 2d0*(1d0 + v)/(1d0 - v)

      Cqq3 = -2d0*v*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Cqq4 = 2d0*v**2d0*w*(1d0 - w)*(1d0 - v*(1d0 - 2d0*w))/
     &            (1d0 - v)/(1d0 - v*(1d0 - w))

      Cqq5 = 1d0/(1d0 - v)/(1d0 - v*(1d0 - w))*
     &     ( 2d0 - w + v*w*(5d0 - w) - v**2d0*(2d0 - 2d0*w + w**2d0)
     &       - 2d0*v**3d0*w*(1d0 - 3d0*w + 2d0*w**2d0) )

      Cqq6 = (1d0 - v)*(1d0 - w)*(1d0 + v*w)/(1d0 - v*(1d0 - w))


      Cqg1 =  2d0*v*w*(1d0 - v*(1d0 - 2d0*w))*
     &        (1d0 + v**2d0*(1d0 - w)**2d0)/(1d0 - v)/
     &        (1d0 - v*(1d0 - w))**2d0

      Cqg2 = -2d0*v*w*(1d0 + v*(1d0 - 2d0*w))/(1d0 - v)

      Cqg3 = v*w/(1d0 - v)/(1d0 - v*w)**2d0/(1d0 - v*(1d0 - w))**2d0*
     &       ( 1d0 - v*(1d0 - 2d0*w) + v**2d0*(1d0 - 2d0*w - 3d0*w**2d0)
     &       - v**3d0*(1d0 - 7d0*w**2d0 + 4d0*w**3d0)
     &       + 2d0*v**4d0*w*(2d0 - 7d0*w + 7d0*w**2d0 - 2d0*w**3d0)
     &       - 2d0*v**5d0*w**2d0*(1d0 - w)**2d0*(1d0 - 2d0*w) )

      Cqg4 = v*w/(1d0 - v)/(1d0 - v*w)**2d0/(1d0 - v*(1d0 - w))**2d0*
     &       ( 2d0 - 4d0*v + v**2d0*(4d0 - 13d0*w + 12d0*w**2d0) 
     &       - v**3d0*(2d0 - 14d0*w + 13d0*w**2d0)
     &       - v**4d0*w*(3d0 - 4d0*w + 3d0*w**2d0 - 2d0*w**3d0)
     &       + v**5d0*w**2d0*(1d0 - w)**2d0 )


      Cgq1 = - 2d0*(1d0 + v*(1d0 - 2d0*w))/(1d0 - v)


      Cgq2 = -1d0/(1d0 - v)/(1d0 - v*w)**2d0*(
     &       2d0 - 2d0*w + 2d0*v*(1d0 - 4d0*w + 2d0*w**2d0)
     &       - v**2d0*w*(4d0 - 11d0*w + 2d0*w**2d0)
     &       + 3d0*v**3d0*w**2d0*(1d0 - 2d0*w) )

      Cgq3 = 1d0/(1d0 - v)/(1d0 - v*w)**2d0*(
     &       2d0*(1d0 - w) + 2d0*v*(2d0 - 5d0*w + 2d0*w**2d0)
     &       - v**2d0*w*(7d0 - 12d0*w + 2d0*w**2d0)
     &       + v**3d0*w**2d0*(7d0 - 8d0*w) )


   

      LLNLOrealqqInt = Conv*
     &            (-U/S**2d0)*1d0/v/(1d0 - v)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &       dlog(1d0 - w)/(1d0 - w)*(Aqq1/w**2d0*Hqq - Aqq11*Hqq1)
     &       + 1d0/(1d0 - w)*(
     &       Bqq1*dlog((1d0 - v)/v/(1d0 - v*(1d0 - w)))/w**2d0*Hqq 
     &                                  - Bqq11*dlog((1d0 - v)/v)*Hqq1
     &       + Bqq2*dlog(1d0 - v*(1d0 - w))/w**2d0*Hqq
     &       + Bqq3*dlog((1d0 - v)*v/w*(S*U/T/mu**2d0))/w**2d0*Hqq
     &       - Bqq31*dlog((1d0 - v)*v*(S*U/T/mu**2d0))*Hqq1)
     &       + Cqq1*dlog(v*(1d0 - w))/w**2d0*Hqq
     &       + Cqq2*dlog((1d0 - v)*w/(1d0 - v*w))/w**2d0*Hqq
     & + Cqq3*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w**2d0*Hqq
     &       + Cqq4*dlog(1d0 - v*(1d0 - w))/w**2d0*Hqq
     &       + Cqq5*dlog((1d0 - v)/v/w*(S*U/T/mu**2d0))/w**2d0*Hqq
     &       + Cqq6/w**2d0*Hqq)


      LLNLOrealqgInt = Conv*
     &          (-U/S**2d0)*1d0/v/(1d0 - v)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cqg1*dlog(1d0 - v*(1d0 - w))/w**2d0*Hqg
     & + Cqg2*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w**2d0*Hqg
     & + Cqg3*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w**2d0*Hqg
     & + Cqg4/w**2d0*Hqg   )


      LLNLOrealgqInt = Conv*
     &               (-U/S**2d0)*1d0/v/(1d0 - v)*(TR*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cgq1*dlog((1d0 - v)*w/(1d0 - v*w))/w**2d0*Hgq
     & + Cgq2*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w**2d0*Hgq
     & + Cgq3/w**2d0*Hgq   )


      if(n.eq.1) then
      LLNLOrealInt = LLNLOrealqqInt
      end if
      
      if(n.eq.2) then
      LLNLOrealInt = LLNLOrealqgInt
      end if

      if(n.eq.3) then
      LLNLOrealInt = LLNLOrealgqInt
      end if

      if(n.eq.4) then
      LLNLOrealInt = LLNLOrealqqInt + LLNLOrealqgInt + LLNLOrealgqInt
      end if

      end if
      end function

! ********** Jet Production (real corrections) *************************

      function LLNLOrealJetInt(n,S,T,U,mu,R,IT,w)
      implicit none
      integer n,IT
      real*8 LLNLOrealJetInt,S,T,U,mu,R,v,w,x,
     &       alphaem,alphastr,Pi,Conv
      real*8 CF,TR,Hqq,Hqg,Hgq,LLNLOrealJetqqInt,
     &        LLNLOrealJetqgInt,LLNLOrealJetgqInt,alphas,Hqq1,xb
      real*8 xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl
      real*8 xg1uv1,xg1dv1,xg1ub1,xg1db1,xg1s1,xg1gl1
      real*8 xguD,xgdD,xgubD,xgdbD,xgsD,xgglD
      real*8 xguD1,xgdD1,xgubD1,xgdbD1,xgsD1,xgglD1
      real*8 xguHe3,xgdHe3,xgubHe3,xgdbHe3,xgsHe3,xgglHe3
      real*8 xguHe31,xgdHe31,xgubHe31,xgdbHe31,xgsHe31,xgglHe31
      real*8 Aqq1,Bqq1,Bqq2,Bqq3,Cqq1,Cqq2,Cqq3,Cqq4,Cqq5,Cqq6,
     &       Cqg1,Cqg2,Cqg3,Cqg4,Cgq1,Cgq2,Cgq3,Aqq11,Bqq11,Bqq21,Bqq31
      real*8 AqqJet,Aqq1Jet,BqqJet,Bqq1Jet,CqqJet,BqgJet,CqgJet

      CALL INITALPHAS(1, 1d0, 1d0, 0.5d0, 1.4d0, 4.75d0, 1.d10)
C--
C--   Then get alpha_s at a renormalisation scale mu with:
C--
      alphastr = ALPHAS(mu)

!     Conversion factor for a cross section in 1/GeV^2 to a cross section in picobarn (pb)
      Conv = 0.389379d0*10d0**(9d0)

      Pi = 2d0*dasin(1d0)
      alphaem = 1d0/137d0
!      alphastr= 4d0*Pi*alphas(mu**2d0)
      CF = 4d0/3d0
      TR = 1d0/2d0

      v = (S + T)/S
      x = (1d0 - v)/v/w*U/T
      xb= (1d0 - v)/v*U/T

      if((x.ge.1d0) .or. ((S + T + U).le.0d0) .or. (w.lt.xb)) then
      LLNLOrealJetInt = 0d0
      else 
      

      call DSSVGUPDATE(x,mu**2d0,xg1uv,xg1dv,xg1ub,xg1db,xg1s,xg1gl)
      call DSSVGUPDATE(xb,mu**2d0,xg1uv1,xg1dv1,xg1ub1,
     &                                              xg1db1,xg1s1,xg1gl1)

!     Deuteron helicity pdfs
      xguD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdD  = 1d0/2d0*( (xg1uv + xg1ub) + (xg1dv + xg1db))
      xgubD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgdbD = 1d0/2d0*( (xg1ub) + (xg1db))
      xgsD  = xg1s
      xgglD = xg1gl

      xguD1  = 1d0/2d0*( (xg1uv1 + xg1ub1) + (xg1dv1 + xg1db1))
      xgdD1  = 1d0/2d0*( (xg1uv1 + xg1ub1) + (xg1dv1 + xg1db1))
      xgubD1 = 1d0/2d0*( (xg1ub1) + (xg1db1))
      xgdbD1 = 1d0/2d0*( (xg1ub1) + (xg1db1))
      xgsD1  = xg1s1
      xgglD1 = xg1gl1


!     He3 helicity pdfs
      xguHe3  = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1dv + xg1db))
      xgdHe3  = 1d0/1d0*( (xg1uv + xg1ub) + 0d0*(xg1dv + xg1db))
      xgubHe3 = 1d0/1d0*( 0d0*(xg1uv + xg1ub) + (xg1db))
      xgdbHe3 = 1d0/1d0*( (xg1ub) + 0d0*(xg1dv + xg1db))
      xgsHe3  = xg1s
      xgglHe3 = xg1gl

      xguHe31  = 1d0/1d0*( 0d0*(xg1uv1 + xg1ub1) + (xg1dv1 + xg1db1))
      xgdHe31  = 1d0/1d0*( (xg1uv1 + xg1ub1) + 0d0*(xg1dv1 + xg1db1))
      xgubHe31 = 1d0/1d0*( 0d0*(xg1uv1 + xg1ub1) + (xg1db1))
      xgdbHe31 = 1d0/1d0*( (xg1ub1) + 0d0*(xg1dv1 + xg1db1))
      xgsHe31  = xg1s1
      xgglHe31 = xg1gl1



      if(IT.eq.1) then

      Hqq = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x) + (xg1ub/x) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x) + (xg1db/x) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x) + (xg1s/x) ) 

      Hqq1= (2d0/3d0)**2d0*( ((xg1uv1 + xg1ub1)/xb) + (xg1ub1/xb) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv1 + xg1db1)/xb) + (xg1db1/xb) ) +
     &    (1d0/3d0)**2d0*( (xg1s1/xb) + (xg1s1/xb) ) 

      Hqg = (2d0/3d0)**2d0*( ((xg1uv + xg1ub)/x) + (xg1ub/x) ) +
     &    (1d0/3d0)**2d0*( ((xg1dv + xg1db)/x) + (xg1db/x) ) +
     &    (1d0/3d0)**2d0*( (xg1s/x) + (xg1s/x) ) 

!     Work with Nf = 4 active flavors

      Hgq = 2d0*(xg1gl/x)*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &                      (1d0/3d0)**2d0 + (2d0/3d0)**2d0   )

      endif

      if(IT.eq.2) then
      
      Hqq = (2d0/3d0)**2d0*( (xguD/x) + (xgubD/x) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x) + (xgdbD/x) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x) + (xgsD/x) )

      Hqq1 = (2d0/3d0)**2d0*( (xguD1/xb) + (xgubD1/xb) ) +
     &    (1d0/3d0)**2d0*( (xgdD1/xb) + (xgdbD1/xb) ) +
     &    (1d0/3d0)**2d0*( (xgsD1/xb) + (xgsD1/xb) ) 

      Hqg = (2d0/3d0)**2d0*( (xguD/x) + (xgubD/x) ) +
     &    (1d0/3d0)**2d0*( (xgdD/x) + (xgdbD/x) ) +
     &    (1d0/3d0)**2d0*( (xgsD/x) + (xgsD/x) ) 

!     Work with Nf = 4 active flavors

      Hgq = 2d0*(xgglD/x)*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &                      (1d0/3d0)**2d0 + (2d0/3d0)**2d0 ) 

      endif

      if(IT.eq.2) then
      
      Hqq = (2d0/3d0)**2d0*( (xguHe3/x) + (xgubHe3/x) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x) + (xgdbHe3/x) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x) + (xgsHe3/x) )

      Hqq1 = (2d0/3d0)**2d0*( (xguHe31/xb) + (xgubHe31/xb) ) +
     &    (1d0/3d0)**2d0*( (xgdHe31/xb) + (xgdbHe31/xb) ) +
     &    (1d0/3d0)**2d0*( (xgsHe31/xb) + (xgsHe31/xb) ) 

      Hqg = (2d0/3d0)**2d0*( (xguHe3/x) + (xgubHe3/x) ) +
     &    (1d0/3d0)**2d0*( (xgdHe3/x) + (xgdbHe3/x) ) +
     &    (1d0/3d0)**2d0*( (xgsHe3/x) + (xgsHe3/x) ) 

!     Work with Nf = 4 active flavors

      Hgq = 2d0*(xgglHe3/x)*( (2d0/3d0)**2d0 + (1d0/3d0)**2d0 +
     &                      (1d0/3d0)**2d0 + (2d0/3d0)**2d0 ) 

      endif



      

      Aqq1 = 8d0*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Aqq11 = 8d0*(1d0 + v)/(1d0 - v)
      

      Bqq1 = 4d0*w*(1d0 + v*w)/(1d0 - v)

      Bqq11 = 4d0*(1d0 + v)/(1d0 - v)

      Bqq2 = 4d0*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Bqq21 = 4d0*(1d0 + v)/(1d0 - v)

      Bqq3 = 4d0*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Bqq31 = 4d0*(1d0 + v)/(1d0 - v)


      Cqq1 = 1d0/(1d0 - v)/(1d0 - v*(1d0 - w))*
     &     ( 2d0 - w + v*w*(9d0 - w) - v**2d0*(2d0 + 2d0*w - 3d0*w**2d0)
     &       - 2d0*v**3d0*w*(1d0 - 3d0*w + 2d0*w**2d0) )

      Cqq2 = 2d0*(1d0 + v)/(1d0 - v)

      Cqq3 = -2d0*v*w*(1d0 - v*(1d0 - 2d0*w))/(1d0 - v)

      Cqq4 = 2d0*v**2d0*w*(1d0 - w)*(1d0 - v*(1d0 - 2d0*w))/
     &            (1d0 - v)/(1d0 - v*(1d0 - w))

      Cqq5 = 1d0/(1d0 - v)/(1d0 - v*(1d0 - w))*
     &     ( 2d0 - w + v*w*(5d0 - w) - v**2d0*(2d0 - 2d0*w + w**2d0)
     &       - 2d0*v**3d0*w*(1d0 - 3d0*w + 2d0*w**2d0) )

      Cqq6 = (1d0 - v)*(1d0 - w)*(1d0 + v*w)/(1d0 - v*(1d0 - w))


      Cqg1 =  2d0*v*w*(1d0 - v*(1d0 - 2d0*w))*
     &        (1d0 + v**2d0*(1d0 - w)**2d0)/(1d0 - v)/
     &        (1d0 - v*(1d0 - w))**2d0

      Cqg2 = -2d0*v*w*(1d0 + v*(1d0 - 2d0*w))/(1d0 - v)

      Cqg3 = v*w/(1d0 - v)/(1d0 - v*w)**2d0/(1d0 - v*(1d0 - w))**2d0*
     &       ( 1d0 - v*(1d0 - 2d0*w) + v**2d0*(1d0 - 2d0*w - 3d0*w**2d0)
     &       - v**3d0*(1d0 - 7d0*w**2d0 + 4d0*w**3d0)
     &       + 2d0*v**4d0*w*(2d0 - 7d0*w + 7d0*w**2d0 - 2d0*w**3d0)
     &       - 2d0*v**5d0*w**2d0*(1d0 - w)**2d0*(1d0 - 2d0*w) )

      Cqg4 = v*w/(1d0 - v)/(1d0 - v*w)**2d0/(1d0 - v*(1d0 - w))**2d0*
     &       ( 2d0 - 4d0*v + v**2d0*(4d0 - 13d0*w + 12d0*w**2d0) 
     &       - v**3d0*(2d0 - 14d0*w + 13d0*w**2d0)
     &       - v**4d0*w*(3d0 - 4d0*w + 3d0*w**2d0 - 2d0*w**3d0)
     &       + v**5d0*w**2d0*(1d0 - w)**2d0 )


      Cgq1 = - 2d0*(1d0 + v*(1d0 - 2d0*w))/(1d0 - v)


      Cgq2 = -1d0/(1d0 - v)/(1d0 - v*w)**2d0*(
     &       2d0 - 2d0*w + 2d0*v*(1d0 - 4d0*w + 2d0*w**2d0)
     &       - v**2d0*w*(4d0 - 11d0*w + 2d0*w**2d0)
     &       + 3d0*v**3d0*w**2d0*(1d0 - 2d0*w) )

      Cgq3 = 1d0/(1d0 - v)/(1d0 - v*w)**2d0*(
     &       2d0*(1d0 - w) + 2d0*v*(2d0 - 5d0*w + 2d0*w**2d0)
     &       - v**2d0*w*(7d0 - 12d0*w + 2d0*w**2d0)
     &       + v**3d0*w**2d0*(7d0 - 8d0*w) )



      AqqJet = - 4d0*w*(1d0 - v + 2d0*v*w)/(1d0 - v)

      Aqq1Jet = -4d0*(1d0 + v)/(1d0 - v)


      BqqJet = - 2d0*w*(1d0 - v + 2d0*v*w)/(1d0 - v)* 
     &           dlog((v**2d0*T*U)/(S*mu**2d0)*R**2d0)

      Bqq1Jet = -2d0*(1d0 + v)/(1d0 - v)* 
     &           dlog((v**2d0*T*U)/(S*mu**2d0)*R**2d0)
   
      CqqJet = - (1d0 - v + 2d0*v*w)/(1d0 - v) *
     &           w*v**2d0*(1d0 - w)/(1d0 - v*(1d0 - w)) *
     &     (1d0 + dlog((v**2d0*(1d0 - w)**2d0*T*U)/(S*mu**2d0)*R**2d0) )


      BqgJet = - (1d0 - v + 2d0*v*w)/(1d0 - v) * 
     &  v*w*(1d0 + v**2d0*(1d0 - w)**2d0)/(1d0 - v*(1d0 - w))**2d0 * 
     &            dlog((v**2d0*(1d0 - w)**2d0*T*U)/(S*mu**2d0)*R**2d0)

      CqgJet = - (1d0 - v + 2d0*v*w)/(1d0 - v)*v*w

      LLNLOrealJetqqInt = Conv*
     &            (1d0/S)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*( 1d0*(
     &       dlog(1d0 - w)/(1d0 - w)*(Aqq1/w*Hqq - Aqq11*Hqq1)
     &       + 1d0/(1d0 - w)*(
     &       Bqq1*dlog((1d0 - v)/v/(1d0 - v*(1d0 - w)))/w*Hqq 
     &                                  - Bqq11*dlog((1d0 - v)/v)*Hqq1
     &       + Bqq2*dlog(1d0 - v*(1d0 - w))/w*Hqq
     &       + Bqq3*dlog((1d0 - v)*v/w*(S*U/T/mu**2d0))/w*Hqq
     &       - Bqq31*dlog((1d0 - v)*v*(S*U/T/mu**2d0))*Hqq1)
     &       + Cqq1*dlog(v*(1d0 - w))/w*Hqq
     &       + Cqq2*dlog((1d0 - v)*w/(1d0 - v*w))/w*Hqq
     & + Cqq3*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w*Hqq
     &       + Cqq4*dlog(1d0 - v*(1d0 - w))/w*Hqq
     &       + Cqq5*dlog((1d0 - v)/v/w*(S*U/T/mu**2d0))/w*Hqq
     &       + Cqq6/w*Hqq )
!
!     Jet's Specific Contributions
!  
     &        + (dlog(1d0 - w)/(1d0 - w)*(AqqJet/w*Hqq - Aqq1Jet*Hqq1)
     &        + 1d0/(1d0 - w)*(BqqJet/w*Hqq - Bqq1Jet*Hqq1) 
     &        + CqqJet/w*Hqq ) )


      LLNLOrealJetqgInt = Conv*
     &          (1d0/S)*(CF*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cqg1*dlog(1d0 - v*(1d0 - w))/w*Hqg
     & + Cqg2*dlog((1d0 - v)/(1d0 - v*w)/(1d0 - v*(1d0 - w)))/w*Hqg
     & + Cqg3*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w*Hqg
     & + Cqg4/w*Hqg   
!
!     Jet's Specific Contributions
!
     & + BqgJet/w*Hqg + CqgJet/w*Hqg)
!     & )

      LLNLOrealJetgqInt = Conv*
     &               (1d0/S)*(TR*alphastr/Pi)*
     &                 alphaem**2d0*1d0/(1d0 - v)*(T/S/U)*(
     &   Cgq1*dlog((1d0 - v)*w/(1d0 - v*w))/w*Hgq
     & + Cgq2*dlog((1d0 - v)*(1d0 - w)/w*(S*U/T/mu**2d0))/w*Hgq
     & + Cgq3/w*Hgq   )


      if(n.eq.1) then
      LLNLOrealJetInt = LLNLOrealJetqqInt
      end if
      
      if(n.eq.2) then
      LLNLOrealJetInt = LLNLOrealJetqgInt
      end if

      if(n.eq.3) then
      LLNLOrealJetInt = LLNLOrealJetgqInt
      end if

      if(n.eq.4) then
      LLNLOrealJetInt = 
     &        LLNLOrealJetqqInt + LLNLOrealJetqgInt + LLNLOrealJetgqInt
      end if

      end if
      end function

      
! ********** Unintegrated Cross Section ********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

      function LLNLOrealFTM1Aux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 LLNLOrealFTM1Aux,Y(*),LLNLOrealInt,S,T,U,mu,v,w,xF,PT,xT,Pi
      common / varLLFTM1NLOreal / S,xF,PT,mu,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      LLNLOrealFTM1Aux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &             LLNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function LLNLOrealFTM1(nn,Ss,xFf,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealFTM1,Ss,xFf,PTt,muu,LLNLOrealFTM1Aux,S,xF,PT,mu,
     &       xT,T,U
      Common / varLLFTM1NLOreal / S,xF,PT,mu,n,IH,IC,IT
      External LLNLOrealFTM1Aux

      n   = nn
      S   = Ss
      xF  = xFf
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealFTM1Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealFTM1 = out
      end function

! **********  E155 *****************************************************

      function LLNLOrealFTM2Aux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 LLNLOrealFTM2Aux,Y(*),LLNLOrealInt,S,T,U,mu,v,w,P,th,M,Pi
      common / varLLFTM2NLOreal / S,P,th,mu,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      M  = 0.94d0 ! Nucleon mass
      v = Y(1)
      w = Y(2)

      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      LLNLOrealFTM2Aux = 2d0*Pi*P*dsin(th)*
     &             LLNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function LLNLOrealFTM2(nn,Ss,Pp,thh,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealFTM2,Ss,Pp,thh,muu,LLNLOrealFTM1Aux,S,P,th,mu,
     &       M,Pi,T,U
      Common / varLLFTM2NLOreal / S,P,th,mu,n,IH,IC,IT
      External LLNLOrealFTM2Aux

      n   = nn
      S   = Ss
      P   = Pp
      th  = thh
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      M  = 0.94d0 ! Nucleon mass
      T = 1d0*M**2d0 - 2d0*M*P
      U = -(S - M**2d0)*(P/M)*(1d0 - dcos(th))

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealFTM2Aux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealFTM2 = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC, EIC(Jets) **********************************

      function LLNLOrealCMAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 LLNLOrealCMAux,Y(*),LLNLOrealInt,S,T,U,mu,v,w,eta,PT,Pi
      common / varLLCMNLOreal / S,eta,PT,mu,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      v = Y(1)
      w = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLNLOrealCMAux = 2d0*Pi*PT* 
     &                     LLNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function LLNLOrealCM(nn,Ss,etaa,PTt,muu,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealCM,Ss,etaa,PTt,muu,LLNLOrealCMAux,S,eta,PT
     &       ,mu,T,U
      Common / varLLCMNLOreal / S,eta,PT,mu,n,IH,IC,IT
      External LLNLOrealCMAux

      n   = nn
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      IH  = IHh
      IC  = ICc
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = U/(T + U)
      region(2) = 0d0
      region(3) = (S + T)/S
      region(4) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealCMAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealCM = out
      end function

!  ********  Jets  *****************************************************

      function LLNLOrealCMJetAux(Y)
      implicit none
      integer n,IT
      real*8 LLNLOrealCMJetAux,Y(*),LLNLOrealJetInt,S,T,U,mu,R,w,
     &       eta,PT,Pi
      common / varNLOrealCMJetLL / S,eta,PT,mu,R,n,IT

      Pi = 2d0*dasin(1d0)
      w = Y(1)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLNLOrealCMJetAux = 2d0*Pi*PT* 
     &                 LLNLOrealJetInt(n,S,T,U,mu,R,IT,w)
      end function

      function LLNLOrealCMJet(nn,Ss,etaa,PTt,muu,Rr,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,ITt,IT
      parameter(ndim=1)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealCMJet,Ss,etaa,PTt,muu,Rr,LLNLOrealCMJetAux,
     &       S,eta,PT,mu,R,T,U
      Common / varNLOrealCMJetLL / S,eta,PT,mu,R,n,IT
      External LLNLOrealCMJetAux

      n   = nn
      S   = Ss
      eta = etaa
      PT  = PTt
      mu  = muu
      R   = Rr
      IT  = ITt

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      region(1) = 0d0
      region(2) = 1d0

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealCMJetAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealCMJet = out
      end function


! *********  Integrated Cross Sections *********************************

! ********** Fixed Target Mode *****************************************
! **********  JLab, HERMES *********************************************

! **********  PT - bins  ***********************************************

      function LLNLOrealFTM1PTAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 LLNLOrealFTM1PTAux,Y(*),LLNLOrealInt,S,T,U,mu,v,w,
     &       xF,PT,xT,Pi,PTmin,PTmax
      real*8 a
      common / varLLFTM1PTNLOreal / S,xF,PTmin,PTmax,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      PT = Y(3)
      mu = Y(3)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLNLOrealFTM1PTAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &      ((S+T)/S - U/(T+U)) *LLNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function LLNLOrealFTM1PT(nn,Ss,xFf,PTminn,PTmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealFTM1PT,Ss,xFf,PTminn,PTmaxx,LLNLOrealFTM1PTAux,
     &       S,xF,PTmin,PTmax
      Common / varLLFTM1PTNLOreal / S,xF,PTmin,PTmax,n,IH,IC,IT
      External LLNLOrealFTM1PTAux

      n     = nn
      S     = Ss
      xF    = xFf
      PTmin = PTminn
      PTmax = PTmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = PTmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = PTmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealFTM1PTAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealFTM1PT = out
      end function

! **********  xF - bins  ***********************************************

      function LLNLOrealFTM1xFAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 LLNLOrealFTM1xFAux,Y(*),LLNLOrealInt,S,T,U,mu,v,w,
     &       xF,PT,xT,Pi,xFmin,xFmax
      real*8 a
      common / varLLFTM1xFNLOreal / S,PT,mu,xFmin,xFmax,n,IH,IC,IT

      Pi = 2d0*dasin(1d0)
      a  = Y(1)
      w  = Y(2)
      xF = Y(3)

      xT = 2d0*PT/dsqrt(S) 
      T = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) + xF)
      U = -S/2d0* (dsqrt(xF**2d0 + xT**2d0) - xF)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLNLOrealFTM1xFAux = 2d0*Pi*PT/dsqrt(xF**2d0 + xT**2d0)*
     &     ((S+T)/S - U/(T+U)) * LLNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function LLNLOrealFTM1xF(nn,Ss,PTt,muu,xFminn,xFmaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealFTM1xF,Ss,PTt,muu,xFminn,xFmaxx,
     &       LLNLOrealFTM1xFAux,S,PT,mu,xFmin,xFmax
      Common / varLLFTM1xFNLOreal / S,PT,mu,xFmin,xFmax,n,IH,IC,IT
      External LLNLOrealFTM1xFAux

      n     = nn
      S     = Ss
      PT    = PTt
      mu    = muu
      xFmin = xFminn
      xFmax = xFmaxx
      IH    = IHh
      IC    = ICc
      IT    = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = xFmin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = xFmax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealFTM1xFAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealFTM1xF = out
      end function

! ********** Collider Mode *********************************************
! **********  COMPASS, EIC *********************************************

! **********  eta - bins  ***********************************************

      function LLNLOrealCMetaAux(Y)
      implicit none
      integer n,IH,IC,IT
      real*8 LLNLOrealCMetaAux,Y(*),LLNLOrealInt,S,T,U,mu,v,w,eta,PT,Pi,
     &       etamin,etamax
      real*8 a
      common / varLLCMetaNLOreal / S,PT,mu,etamin,etamax,n,IH,IC,IT

      Pi  = 2d0*dasin(1d0)
      a   = Y(1)
      w   = Y(2)
      eta = Y(3)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)
      v = (1d0 - a)*U/(T + U) + a*(S + T)/S

      LLNLOrealCMetaAux = 2d0*Pi*PT*
     &      ((S+T)/S - U/(T+U)) *LLNLOrealInt(n,S,T,U,mu,IH,IC,IT,v,w)
      end function

      function LLNLOrealCMeta(nn,Ss,PTt,muu,etaminn,etamaxx,IHh,ICc,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,IHh,ICc,ITt,IH,IC,IT
      parameter(ndim=3)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealCMeta,Ss,PTt,muu,etaminn,etamaxx,
     &       LLNLOrealCMetaAux,S,PT,mu,etamin,etamax
      Common / varLLCMetaNLOreal / S,PT,mu,etamin,etamax,n,IH,IC,IT
      External LLNLOrealCMetaAux

      n      = nn
      S      = Ss
      PT     = PTt
      mu     = muu
      etamin = etaminn
      etamax = etamaxx
      IH  = IHh
      IC  = ICc
      IT  = ITt

      region(1) = 0d0
      region(2) = 0d0
      region(3) = etamin
      region(4) = 1d0
      region(5) = 1d0
      region(6) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealCMetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealCMeta = out
      end function


! ********  Jets  ******************************************************

      function LLNLOrealCMJetetaAux(Y)
      implicit none
      integer n,IT
      real*8 LLNLOrealCMJetetaAux,Y(*),LLNLOrealJetInt,S,T,U,mu,R,w,
     &       eta,PT,Pi,etamin,etamax
      common / varLLCMJetetaNLOreal / S,PT,mu,R,etamin,etamax,n,IT

      Pi  = 2d0*dasin(1d0)
      w   = Y(1)
      eta = Y(2)

      T = -PT*dsqrt(S)*dexp( eta)
      U = -PT*dsqrt(S)*dexp(-eta)

      LLNLOrealCMJetetaAux = 2d0*Pi*PT**5d0*
     &      LLNLOrealJetInt(n,S,T,U,mu,R,IT,w)
      end function

      function LLNLOrealCMJeteta(nn,Ss,PTt,muu,Rr,etaminn,etamaxx,ITt)
      implicit none
      integer ndim,init,ncall,itmx,nprn,idum
      integer n,nn,ITt,IT
      parameter(ndim=2)
      real*8 region(2*ndim),out,sd,chi2a
      real*8 LLNLOrealCMJeteta,Ss,PTt,muu,Rr,etaminn,etamaxx,
     &       LLNLOrealCMJetetaAux,S,PT,mu,R,etamin,etamax
      Common / varLLCMJetetaNLOreal / S,PT,mu,R,etamin,etamax,n,IT
      External LLNLOrealCMJetetaAux

      n      = nn
      S      = Ss
      PT     = PTt
      mu     = muu
      R      = Rr
      etamin = etaminn
      etamax = etamaxx
      IT  = ITt

      region(1) = 0d0
      region(2) = etamin
      region(3) = 1d0
      region(4) = etamax

      idum  = -7653
      sd    = 0d0
      chi2a = 0d0
      init  = 0

      itmx = 5
      nprn = -1

      ncall = 100000

      call vegas(region,ndim,LLNLOrealCMJetetaAux,
     &           init,ncall,itmx,nprn,out,sd,chi2a)
      LLNLOrealCMJeteta = out
      end function







c*********************************************************************        
      
       
       SUBROUTINE DERIV(FUNC, X0, RES)
       IMPLICIT NONE
!      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
       DOUBLE PRECISION
!       Functions
     1  FUNC,
!       Variables
     1  ACC, DIFF, ERR, FAC, RES, RES1, X0, X10
!      XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

!----&------------------------------------------------------------------
!----&--Configuration---------------------------------------------------
!----&------------------------------------------------------------------

!      Accuracy of calculation (small value for high accuracy)
       ACC = 0.0001D0
!      Starting value for Delta x in differentiation formula       
       DIFF = 0.001D0
!      Factor to make DIFF smaller in iteration steps
       FAC = 0.1D0

!----&------------------------------------------------------------------
!----&--Iteration routine-----------------------------------------------
!----&------------------------------------------------------------------

       ERR = 1.D0
       X10 = X0 + DIFF
       RES1 = (FUNC(X10) - FUNC(X0)) / DIFF
       
 1     IF (ERR .LT. ACC) THEN
          GOTO 2
       ELSE
          DIFF = DIFF * FAC
          X10 = X0 + DIFF
          RES = (FUNC(X10) - FUNC(X0)) / DIFF
          ERR = DABS(RES1 - RES)
          RES1 = RES         
          GOTO 1
       ENDIF
       
 2     RETURN
       END

  
                
      SUBROUTINE vegas(region,ndim,fxn,init,ncall,
     &itmx,nprn,tgral,sd,chi2a)
      INTEGER init,itmx,ncall,ndim,nprn,NDMX,MXDIM
c      REAL tgral,chi2a,sd,region(2*ndim),fxn,ALPH,TINY
      REAL*8 tgral,chi2a,sd,region(2*ndim),fxn,ALPH,TINY
      PARAMETER (ALPH=1.5d0,NDMX=50,MXDIM=10,TINY=1.d-30)
      EXTERNAL fxn
CU    USES fxn,ran2,rebin
      INTEGER i,idum,it,j,k,mds,nd,ndo,ng,npg,ia(MXDIM),kg(MXDIM)
c      REAL calls,dv2g,dxg,f,f2,f2b,fb,rc,ti,tsi,wgt,xjac,xn,xnd,xo,
      REAL*8 calls,dv2g,dxg,f,f2,f2b,fb,rc,ti,tsi,wgt,xjac,xn,xnd,xo,
     *d(NDMX,MXDIM),di(NDMX,MXDIM),dt(MXDIM),dx(MXDIM),r(NDMX),x(MXDIM),
     *xi(NDMX,MXDIM),xin(NDMX),ran2
      DOUBLE PRECISION schi,si,swgt
c...
      COMMON /ranno/ idum
      SAVE
      if(init.le.0)then
        mds=1
        ndo=1
        do 11 j=1,ndim
          xi(1,j)=1.d0
11      continue
      endif
      if (init.le.1)then
        si=0.d0
        swgt=0.d0
        schi=0.d0
      endif
      if (init.le.2)then
        nd=NDMX
        ng=1
        if(mds.ne.0)then
          ng=(ncall/2.d0+0.25d0)**(1.d0/ndim)
          mds=1
          if((2*ng-NDMX).ge.0)then
            mds=-1
            npg=ng/NDMX+1
            nd=ng/npg
            ng=npg*nd
          endif
        endif
        k=ng**ndim
        npg=max(ncall/k,2)
        calls=npg*k
        dxg=1.d0/ng
        dv2g=(calls*dxg**ndim)**2/npg/npg/(npg-1.d0)
        xnd=nd
        dxg=dxg*xnd
        xjac=1.d0/calls
        do 12 j=1,ndim
          dx(j)=region(j+ndim)-region(j)
          xjac=xjac*dx(j)
12      continue
        if(nd.ne.ndo)then
          do 13 i=1,nd
            r(i)=1.d0
13        continue
          do 14 j=1,ndim
            call rebin(ndo/xnd,nd,r,xin,xi(1,j))
14        continue
          ndo=nd
        endif
        if(nprn.ge.0) write(*,200) ndim,calls,it,itmx,nprn,ALPH,mds,nd,&
     *(j,region(j),j,region(j+ndim),j=1,ndim)
      endif
      do 28 it=1,itmx
        ti=0.d0
        tsi=0.d0
        do 16 j=1,ndim
          kg(j)=1
          do 15 i=1,nd
            d(i,j)=0.d0
            di(i,j)=0.d0
15        continue
16      continue
10      continue
          fb=0.d0
          f2b=0.d0
          do 19 k=1,npg
            wgt=xjac
            do 17 j=1,ndim
              xn=(kg(j)-ran2(idum))*dxg+1.d0
              ia(j)=max(min(int(xn),NDMX),1)
              if(ia(j).gt.1)then
                xo=xi(ia(j),j)-xi(ia(j)-1,j)
                rc=xi(ia(j)-1,j)+(xn-ia(j))*xo
              else
                xo=xi(ia(j),j)
                rc=(xn-ia(j))*xo
              endif
              x(j)=region(j)+rc*dx(j)
              wgt=wgt*xo*xnd
17          continue
            f=wgt*fxn(x,wgt)
            f2=f*f
            fb=fb+f
            f2b=f2b+f2
            do 18 j=1,ndim
              di(ia(j),j)=di(ia(j),j)+f
              if(mds.ge.0) d(ia(j),j)=d(ia(j),j)+f2
18          continue
19        continue
          f2b=sqrt(f2b*npg)
          f2b=(f2b-fb)*(f2b+fb)
          if (f2b.le.0.) f2b=TINY
          ti=ti+fb
          tsi=tsi+f2b
          if(mds.lt.0)then
            do 21 j=1,ndim
              d(ia(j),j)=d(ia(j),j)+f2b
21          continue
          endif
        do 22 k=ndim,1,-1
          kg(k)=mod(kg(k),ng)+1
          if(kg(k).ne.1) goto 10
22      continue
        tsi=tsi*dv2g
        wgt=1.d0/tsi
        si=si+dble(wgt)*dble(ti)
        schi=schi+dble(wgt)*dble(ti)**2
        swgt=swgt+dble(wgt)
        tgral=si/swgt
        chi2a=max((schi-si*tgral)/(it-.99d0),0.d0)
        sd=sqrt(1.d0/swgt)
        tsi=sqrt(tsi)
        if(nprn.ge.0)then
          write(*,201) it,ti,tsi,tgral,sd,chi2a
          if(nprn.ne.0)then
            do 23 j=1,ndim
              write(*,202) j,(xi(i,j),di(i,j),i=1+nprn/2,nd,nprn)
23          continue
          endif
        endif
        do 25 j=1,ndim
          xo=d(1,j)
          xn=d(2,j)
          d(1,j)=(xo+xn)/2.d0
          dt(j)=d(1,j)
          do 24 i=2,nd-1
            rc=xo+xn
            xo=xn
            xn=d(i+1,j)
            d(i,j)=(rc+xn)/3.d0
            dt(j)=dt(j)+d(i,j)
24        continue
          d(nd,j)=(xo+xn)/2.d0
          dt(j)=dt(j)+d(nd,j)
25      continue
        do 27 j=1,ndim
          rc=0.d0
          do 26 i=1,nd
            if(d(i,j).lt.TINY) d(i,j)=TINY
            r(i)=((1.-d(i,j)/dt(j))/(dlog(dt(j))-dlog(d(i,j))))**ALPH
            rc=rc+r(i)
26        continue
          call rebin(rc/xnd,nd,r,xin,xi(1,j))
27      continue
28    continue
      return
200   FORMAT(/' input parameters for vegas:  ndim=',i3,'  ncall=',
     *f8.0/28x,'  it=',i5,'  itmx=',i5/28x,'  nprn=',i3,'  alph=',
     *f5.2/28x,'  mds=',i3,'   nd=',i4/(30x,'xl(',i2,')= ',g14.7,' xu(',
     *i2,')= ',g14.7))
201   FORMAT(/' iteration no.',I3,': ','integral =',g14.7,'+/- ',g9.2/
     *' all iterations: integral =',g14.7,'+/- ',g9.2,' chi**2/it''n ='
     *,g9.2)
202   WRITE(*,FORMAT(/' data for axis ',I2/'    X       delta i       ',
     *'   x       delta i       ','    x       delta i       ',/(1x,
     *f7.5,1x,g11.4,5x,f7.5,1x,g11.4,5x,f7.5,1x,g11.4)))
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.
c...
c...
      SUBROUTINE rebin(rc,nd,r,xin,xi)
      INTEGER nd
c      REAL rc,r(*),xi(*),xin(*)
      REAL*8 rc,r(*),xi(*),xin(*)
      INTEGER i,k
c      REAL dr,xn,xo
      REAL*8 dr,xn,xo
      k=0
      xn=0.d0
      dr=0.d0
      do 11 i=1,nd-1
1       if(rc.gt.dr)then
          k=k+1
          dr=dr+r(k)
          xo=xn
          xn=xi(k)
        goto 1
        endif
        dr=dr-rc
        xin(i)=xn-(xn-xo)*dr/r(k)
11    continue
      do 12 i=1,nd-1
        xi(i)=xin(i)
12    continue
      xi(nd)=1.d0
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.
c...
c...
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
c      REAL ran2,AM,EPS,RNMX
      REAL*8 ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1.d0/IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2d-7,RNMX=1.d0-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0!5,.

      
