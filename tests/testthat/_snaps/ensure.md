# ensure function

    Code
      res
    Output
      $object
      $data
      # A tibble: 28 x 56
         id      type   argi organic_N_conc profhum  calc    pH concseuil albedo    q0
         <chr>   <chr> <dbl>          <dbl>   <dbl> <dbl> <dbl>     <dbl>  <dbl> <dbl>
       1 solcan~ sols   30.2           0.27      40  0     7         0.2    0.17  12  
       2 solesc~ sols   21             0.1       31  0.7   7         0      0.2    9  
       3 solpat~ sols   27             0.21      27  3.7   7         0      0.28  12  
       4 solpois sols   39             0.15      27  0     7         0      0.28  12  
       5 solcar~ sols    1             0.7       40  0.82  5.36      0      0.05   7.5
       6 sollin  sols   12.2           0.11      30  0.6   8.1       0      0.22  21  
       7 solban~ sols   70             0.2       50  0     7         0.2    0.17  25  
       8 solsor~ sols   22             0.12      35  1     7         0      0.2    0  
       9 solorge sols    9.9           0.07      50  9     6.1       0      0.25   3.6
      10 solciM~ sols   10.2           0.16      30 65.3   8.4       0.05   0.28  11.5
      # ... with 18 more rows, and 46 more variables: ruisolnu <int>, obstarac <int>,
      #   pluiebat <int>, mulchbat <dbl>, zesx <int>, cfes <int>, z0solnu <dbl>,
      #   csurNsol <int>, penterui <dbl>, codecailloux <int>, codemacropor <int>,
      #   codefente <int>, codrainage <int>, profimper <int>, ecartdrain <int>,
      #   ksol <int>, profdrain <int>, coderemontcap <int>, capiljour <int>,
      #   humcapil <int>, codenitrif <int>, codedenit <int>, profdenit <int>,
      #   vpotdenit <int>, cailloux_1 <dbl>, cailloux_2 <dbl>, cailloux_3 <dbl>,
      #   cailloux_4 <dbl>, cailloux_5 <dbl>, typecailloux_1 <int>,
      #   typecailloux_2 <int>, typecailloux_3 <int>, typecailloux_4 <int>,
      #   typecailloux_5 <int>, infil_1 <int>, infil_2 <int>, infil_3 <int>,
      #   infil_4 <int>, infil_5 <int>, epd_1 <int>, epd_2 <int>, epd_3 <int>,
      #   epd_4 <int>, epd_5 <int>, depth <int>, saturated_wtr_cont <dbl>
      
      $data_byLayer
      # A tibble: 140 x 7
         id     layer layer_depth layer_water_fiel~ layer_water_wil~ layer_bulk_densi~
         <chr>  <chr>       <int>             <dbl>            <dbl>             <dbl>
       1 solca~ 1              20              46.8             26.2              1.08
       2 solca~ 2              20              46.4             27.4              1.09
       3 solca~ 3              20              48.5             29.1              1.02
       4 solca~ 4              20              50.1             25.5              0.99
       5 solca~ 5              20              50.1             25.5              0.99
       6 soles~ 1              30              22.4             13.3              1.44
       7 soles~ 2              30              22               11.8              1.65
       8 soles~ 3              30              21               11                1.65
       9 soles~ 4              30              22                9                1.65
      10 soles~ 5              30              21                9                1.49
      # ... with 130 more rows, and 1 more variable: layer_saturated_wtr_cont <dbl>
      
      $dict
      $dict$id
      [1] "name"
      
      $dict$organic_N_conc
      [1] "norg"
      
      $dict$layer_depth
      [1] "epc"
      
      $dict$layer_water_field_cap
      [1] "HCCF"
      
      $dict$layer_water_wilting_pt
      [1] "HMINF"
      
      $dict$layer_bulk_density_moist
      [1] "DAF"
      
      
      attr(,"class")
      [1] "cropr_input"
      
      $missing
      NULL
      
      $success
      [1] TRUE
      

# ensure_wrapper function

    Code
      soil
    Output
      $data
      # A tibble: 28 x 56
         id      type   argi organic_N_conc profhum  calc    pH concseuil albedo    q0
         <chr>   <chr> <dbl>          <dbl>   <dbl> <dbl> <dbl>     <dbl>  <dbl> <dbl>
       1 solcan~ sols   30.2           0.27      40  0     7         0.2    0.17  12  
       2 solesc~ sols   21             0.1       31  0.7   7         0      0.2    9  
       3 solpat~ sols   27             0.21      27  3.7   7         0      0.28  12  
       4 solpois sols   39             0.15      27  0     7         0      0.28  12  
       5 solcar~ sols    1             0.7       40  0.82  5.36      0      0.05   7.5
       6 sollin  sols   12.2           0.11      30  0.6   8.1       0      0.22  21  
       7 solban~ sols   70             0.2       50  0     7         0.2    0.17  25  
       8 solsor~ sols   22             0.12      35  1     7         0      0.2    0  
       9 solorge sols    9.9           0.07      50  9     6.1       0      0.25   3.6
      10 solciM~ sols   10.2           0.16      30 65.3   8.4       0.05   0.28  11.5
      # ... with 18 more rows, and 46 more variables: ruisolnu <int>, obstarac <int>,
      #   pluiebat <int>, mulchbat <dbl>, zesx <int>, cfes <int>, z0solnu <dbl>,
      #   csurNsol <int>, penterui <dbl>, codecailloux <int>, codemacropor <int>,
      #   codefente <int>, codrainage <int>, profimper <int>, ecartdrain <int>,
      #   ksol <int>, profdrain <int>, coderemontcap <int>, capiljour <int>,
      #   humcapil <int>, codenitrif <int>, codedenit <int>, profdenit <int>,
      #   vpotdenit <int>, cailloux_1 <dbl>, cailloux_2 <dbl>, cailloux_3 <dbl>,
      #   cailloux_4 <dbl>, cailloux_5 <dbl>, typecailloux_1 <int>,
      #   typecailloux_2 <int>, typecailloux_3 <int>, typecailloux_4 <int>,
      #   typecailloux_5 <int>, infil_1 <int>, infil_2 <int>, infil_3 <int>,
      #   infil_4 <int>, infil_5 <int>, epd_1 <int>, epd_2 <int>, epd_3 <int>,
      #   epd_4 <int>, epd_5 <int>, depth <int>, saturated_wtr_cont <dbl>
      
      $data_byLayer
      # A tibble: 140 x 7
         id     layer layer_depth layer_water_fiel~ layer_water_wil~ layer_bulk_densi~
         <chr>  <chr>       <int>             <dbl>            <dbl>             <dbl>
       1 solca~ 1              20              46.8             26.2              1.08
       2 solca~ 2              20              46.4             27.4              1.09
       3 solca~ 3              20              48.5             29.1              1.02
       4 solca~ 4              20              50.1             25.5              0.99
       5 solca~ 5              20              50.1             25.5              0.99
       6 soles~ 1              30              22.4             13.3              1.44
       7 soles~ 2              30              22               11.8              1.65
       8 soles~ 3              30              21               11                1.65
       9 soles~ 4              30              22                9                1.65
      10 soles~ 5              30              21                9                1.49
      # ... with 130 more rows, and 1 more variable: layer_saturated_wtr_cont <dbl>
      
      $dict
      $dict$id
      [1] "name"
      
      $dict$organic_N_conc
      [1] "norg"
      
      $dict$layer_depth
      [1] "epc"
      
      $dict$layer_water_field_cap
      [1] "HCCF"
      
      $dict$layer_water_wilting_pt
      [1] "HMINF"
      
      $dict$layer_bulk_density_moist
      [1] "DAF"
      
      
      attr(,"class")
      [1] "cropr_input"

# print_missingTree function

    Code
      cat(print_missingTree(lst))
    Output
        `A`
        AND `B`
      	OR  `C`
      		OR  `D`
      			OR  `E`
      		AND `F`
      		AND `G`
      			OR  `H`
      			AND `I`
        AND `J`
      	OR  `K`
        AND `L`
        AND `N`

# ensure_wrapper function failed

    Graph type `thickness.mswc.norg` requires the following parameters:
      `depth`
    	OR  `layer_depth`
      AND `saturated_wtr_cont`
    	OR  `layer_saturated_wtr_cont`
    		OR  `layer_depth`
    		AND `layer_bulk_density_moist`
    		AND `layer_water_field_cap`
    		AND `layer_water_wilting_pt`
      AND `organic_N_conc`

# expect_depth

    Code
      ensure_depth(soil)
    Output
      $object
      $object$data
        id depth
      1  A     3
      2  B     7
      
      $object$data_byLayer
        id layer_depth
      1  A           1
      2  A           2
      3  B           3
      4  B           4
      
      $object$dict
      list()
      
      
      $missing
      NULL
      
      $success
      [1] TRUE
      

# ensure_layer_saturated_wtr_cont

    Code
      ensure_layer_saturated_wtr_cont(soil)
    Output
      $object
      $object$data
      data frame with 0 columns and 0 rows
      
      $object$data_byLayer
        layer_depth layer_bulk_density_moist layer_water_field_cap
      1    8.966972                 9.082078             6.6079779
      2    2.655087                 2.016819             6.2911404
      3    3.721239                 8.983897             0.6178627
      4    5.728534                 9.446753             2.0597457
        layer_water_wilting_pt id layer_saturated_wtr_cont
      1               1.765568  A               39.4359793
      2               6.870228  A               -0.3100918
      3               3.841037  B              -10.7754679
      4               7.698414  B              -30.5142410
      
      $object$dict
      list()
      
      
      $missing
      NULL
      
      $success
      [1] TRUE
      

# ensure_saturated_wtr_cont

    Code
      ensure_saturated_wtr_cont(soil)
    Output
      $object
      $object$data
        id saturated_wtr_cont
      1  A          11.622059
      2  B           9.449773
      
      $object$data_byLayer
        layer_saturated_wtr_cont id
      1                 8.966972  A
      2                 2.655087  A
      3                 3.721239  B
      4                 5.728534  B
      
      $object$dict
      list()
      
      
      $missing
      NULL
      
      $success
      [1] TRUE
      

