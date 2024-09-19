# simpleLME returns correct model fixed slope

    == Simple LMEs standard model ==================================================
      LME fit method         REML
      Fixed effects          yij ~ time
      Random effects         ~time | pid
      (Intercept)            975.23
      time                   56.09
      Nobs                   118
      N_Grps                 20
      AIC                    1588.69
      BIC                    1605.21
    -- Summary t-table -------------------------------------------------------------
                  Value Std.Error DF t-value    p-value
    (Intercept) 975.234   43.8024 97 22.2644 6.6249e-40
    time         56.091    6.9825 97  8.0331 2.2820e-12
    ================================================================================

# simpleLME returns correct model random slope

    == Simple LMEs standard model ==================================================
      LME fit method         REML
      Fixed effects          yij ~ time
      Random effects         ~1 | pid
      (Intercept)            976.23
      time                   55.51
      Nobs                   118
      N_Grps                 20
      AIC                    1587.3
      BIC                    1598.32
    -- Summary t-table -------------------------------------------------------------
                  Value Std.Error DF t-value    p-value
    (Intercept) 976.231   46.9385 97 20.7981 1.5810e-37
    time         55.508    5.8673 97  9.4605 1.9614e-15
    ================================================================================

# simpleLME returns model when comparing 2 data sets

    == Simple LMEs ... 2 data set model comparison =================================
      (Intercept)    time
    A      975.23 56.0913
    B     1010.16  2.8757
    
    Compared Data        A vs. B
    Fixed effects        yij ~ time
    Random effects       ~time | pid
    No. Observations     'A = 118', 'B = 116'
    No. Groups           'A = 20', 'B = 20'
    ================================================================================

