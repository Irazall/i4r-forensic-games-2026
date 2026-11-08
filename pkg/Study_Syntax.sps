*filter data (do not analyse those lost to follow-up)

USE ALL.
COMPUTE filter_$=(Concentration_KL_POST > 0).
VARIABLE LABELS filter_$ 'Concentration_KL_POST > 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
_SLINE OFF.



*characteristics of study groups

ONEWAY PE_week Sport_leisure_week SchoolGrade BY Intervention
  /ES=OVERALL
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95)
  /POSTHOC=BONFERRONI ALPHA(0.05).


ONEWAY BORG BY Intervention
  /ES=OVERALL
  /STATISTICS DESCRIPTIVES HOMOGENEITY 
  /MISSING ANALYSIS
  /CRITERIA=CILEVEL(0.95)
  /POSTHOC=BONFERRONI ALPHA(0.05).



*Hypotheses testing (Concentration performance)
    
GLM Concentration_KL_PRE Concentration_KL_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /CONTRAST(Intervention)=Difference
  /METHOD=SSTYPE(3)
  /POSTHOC=Intervention(BONFERRONI) 
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention.

GLM Concentration_KL_PRE Concentration_KL_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention 1 1 1 -3
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Concentration_KL_PRE -1 Concentration_KL_POST 1.

GLM Concentration_KL_PRE Concentration_KL_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention -1 -1 2 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Concentration_KL_PRE -1 Concentration_KL_POST 1.

GLM Concentration_KL_PRE Concentration_KL_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention 2 -1 -1 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Concentration_KL_PRE -1 Concentration_KL_POST 1.

GLM Concentration_KL_PRE Concentration_KL_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention -1 2 -1 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Concentration_KL_PRE -1 Concentration_KL_POST 1.



*Hypotheses testing (Error score F% )

GLM ErrorScore_PRE ErrorScore_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /CONTRAST(Intervention)=Difference
  /METHOD=SSTYPE(3)
  /POSTHOC=Intervention(BONFERRONI) 
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention.

GLM ErrorScore_PRE ErrorScore_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention 1 1 1 -3
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = ErrorScore_PRE -1 ErrorScore_POST 1.

GLM ErrorScore_PRE ErrorScore_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention -1 -1 2 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = ErrorScore_PRE -1 ErrorScore_POST 1.

GLM ErrorScore_PRE ErrorScore_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention 2 -1 -1 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = ErrorScore_PRE -1 ErrorScore_POST 1.

GLM ErrorScore_PRE ErrorScore_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention -1 2 -1 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = ErrorScore_PRE -1 ErrorScore_POST 1.



*Hypotheses testing (Tempo)

GLM Speed_BZO_PRE Speed_BZO_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /CONTRAST(Intervention)=Difference
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention.

GLM Speed_BZO_PRE Speed_BZO_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention 1 1 1 -3
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Speed_BZO_PRE -1 Speed_BZO_POST 1.

GLM Speed_BZO_PRE Speed_BZO_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention -1 -1 2 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Speed_BZO_PRE -1 Speed_BZO_POST 1.

GLM Speed_BZO_PRE Speed_BZO_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention 2 -1 -1 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Speed_BZO_PRE -1 Speed_BZO_POST 1.

GLM Speed_BZO_PRE Speed_BZO_POST BY Intervention
  /WSFACTOR=factor1 2 Polynomial 
  /METHOD=SSTYPE(3)
  /PLOT=PROFILE(factor1*Intervention) TYPE=LINE ERRORBAR=NO MEANREFERENCE=NO YAXIS=AUTO
  /PRINT=DESCRIPTIVE ETASQ 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=factor1 
  /DESIGN=Intervention
  /LMATRIX = intercept 0 Intervention -1 2 -1 0
  /EMMEANS tables(Intervention*factor1)
  /MMATRIX = Speed_BZO_PRE -1 Speed_BZO_POST 1.



*filter off

FILTER OFF.
USE ALL.
EXECUTE.
_SLINE OFF.