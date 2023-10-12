# statistical criteria

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["group", "situation", "variable", "n_obs", "mean_obs", "mean_sim", "r_means", "sd_obs", "sd_sim", "CV_obs", "CV_sim", "R2", "SS_res", "Inter", "Slope", "RMSE", "RMSEs", "RMSEu", "nRMSE", "rRMSE", "rRMSEs", "rRMSEu", "pMSEs", "pMSEu", "Bias2", "SDSD", "LCS", "rbias2", "rSDSD", "rLCS", "MAE", "FVU", "MSE", "EF", "Bias", "ABS", "MAPE", "RME", "tSTUD", "tLimit", "Decision"]
        },
        "description": {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["n_obs", "mean_obs", "mean_sim", "r_means", "sd_obs", "sd_sim", "CV_obs", "CV_sim", "R2", "SS_res", "Inter", "Slope", "RMSE", "RMSEs", "RMSEu", "nRMSE", "rRMSE", "rRMSEs", "rRMSEu", "pMSEs", "pMSEu", "Bias2", "SDSD", "LCS", "rbias2", "rSDSD", "rLCS", "MAE", "FVU", "MSE", "EF", "Bias", "ABS", "MAPE", "RME", "tSTUD", "tLimit", "Decision"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["Number of observations"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Mean of the observations"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Mean of the simulations"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Ratio between mean simulated values and mean observed values (%)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Standard deviation of the observations"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Standard deviation of the simulation"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Coefficient of variation of the observations"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Coefficient of variation of the simulation"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["coefficient of determination for obs~sim"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Residual sum of squares"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Intercept of regression line"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Slope of regression line"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Root Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Systematic Root Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Unsystematic Root Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Normalized Root Mean Squared Error, CV(RMSE)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Relative Root Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Relative Systematic Root Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Relative Unsystematic Root Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Proportion of Systematic Mean Squared Error in Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Proportion of Unsystematic Mean Squared Error in Mean Squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Bias squared (1st term of Kobayashi and Salam (2000) MSE decomposition)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Difference between sd_obs and sd_sim squared (2nd term of Kobayashi and Salam (2000) MSE decomposition)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Correlation between observed and simulated values (3rd term of Kobayashi and Salam (2000) MSE decomposition)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Relative bias squared"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Relative difference between sd_obs and sd_sim squared"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Relative correlation between observed and simulated values"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Mean Absolute Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Fraction of variance unexplained"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Mean squared Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Model efficiency"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Bias"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Mean Absolute Bias"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Mean Absolute Percentage Error"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Relative mean error (%)"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["T student test of the mean difference"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["T student threshold"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Decision of the t student test of the mean difference"]
            }
          ]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["statistics", "tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["stics_1", "stics_1"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["all_situations", "all_situations"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["lai_n", "masec_n"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [14, 18]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.27048, 4.32676]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.02368, 4.14275]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [80.57474, 95.74715]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1.08543, 2.71005]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.86985, 2.98361]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [85.43465, 62.63468]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [84.97306, 72.02007]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.69239, 0.89945]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [5.57851, 16.06896]
        },
        {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["(Intercept)", "(Intercept)"]
            }
          },
          "value": [0.17648, -0.37494]
        },
        {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["obs", "obs"]
            }
          },
          "value": [0.66684, 1.04413]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.63124, 0.94484]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.42701, 0.21764]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.4649, 0.91943]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [49.68537, 21.8371]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.49685, 0.21837]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.3361, 0.0503]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.36592, 0.2125]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.4576, 0.05306]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.5424, 0.94694]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.06091, 0.03386]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.04647, 0.07483]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.31705, 0.83455]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.03773, 0.00181]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.02879, 0.004]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.19643, 0.04458]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.43176, 0.59059]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.30855, 0.12382]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.39846, 0.89272]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.63577, 0.8713]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.24679, -0.18401]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.43176, 0.59059]
        },
        {
          "type": "double",
          "attributes": {},
          "value": ["Inf", 0.19984]
        },
        {
          "type": "double",
          "attributes": {},
          "value": ["Inf", -0.13346]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-1.53155, -0.81866]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [2.16037, 2.10982]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["OK", "OK"]
        }
      ]
    }

