# `fitMixedEffectsModels()` function creates correct values

      Fixed effects             TimePoint*Response
      Random effects            ~ 1 | Pop
      Number of models          2
      Subject field             Pop
      Number of subjects        10
      Number of observations    40
    
    -- Stat Table ------------------------------------------------------------------
                Response_F.value Response_p.value TimePoint_F.value
    seq.1234.56         2.478046        0.1291020         0.4542716
    seq.6969.4          1.629771        0.2144745         0.9445052
                TimePoint_p.value TimePoint.Response_F.value
    seq.1234.56         0.7168000                  1.5714238
    seq.6969.4          0.4353884                  0.3198189
                TimePoint.Response_p.value converged       fdr p.bonferroni rank
    seq.1234.56                  0.2233823      TRUE 0.4467645    0.4467645    1
    seq.6969.4                   0.8109327      TRUE 0.8109327    1.0000000    2

