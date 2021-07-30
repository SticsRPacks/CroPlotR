# detect soil data frame large

    Code
      set_soil(soil_data_large, id = "name", layer_depth = "epc",
        layer_water_field_cap = "HCCF", layer_water_wilting_pt = "HMINF",
        layer_bulk_density_moist = "DAF", organic_N_conc = "norg", verbose = T)
    Message <cliMessage>
      i Soil data format detected: large data frame

# detect soil data frame long

    Code
      set_soil(soil_data_long, id = "name", layer_depth = "epc",
        layer_water_field_cap = "HCCF", layer_water_wilting_pt = "HMINF",
        layer_bulk_density_moist = "DAF", organic_N_conc = "norg", verbose = T)
    Message <cliMessage>
      i Soil data format detected: long data frame
      i Long data frame column detected for soil identification: `name`
      i Long data frame column detected for soil parameter names: `param`
      i Long data frame column detected for soil layers: `id`
      i Long data frame column detected for parameter values: `value`

# detect soil data frame list

    Code
      set_soil(soil_data_list, id = "name", layer_depth = "epc",
        layer_water_field_cap = "HCCF", layer_water_wilting_pt = "HMINF",
        layer_bulk_density_moist = "DAF", organic_N_conc = "norg", verbose = T)
    Message <cliMessage>
      i Soil data format detected: Tibble containing lists for soil layer information

# soil data frame large

    Code
      soil
    Output
      $data
      # A tibble: 28 x 54
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
      # ... with 18 more rows, and 44 more variables: ruisolnu <int>, obstarac <int>,
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
      #   epd_4 <int>, epd_5 <int>
      
      $data_byLayer
      # A tibble: 140 x 6
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
      # ... with 130 more rows
      
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

# soil data frame long

    Code
      soil
    Output
      $data
      # A tibble: 28 x 33
         id          argi organic_N_conc profhum  calc    pH concseuil albedo    q0
         <chr>      <dbl>          <dbl>   <dbl> <dbl> <dbl>     <dbl>  <dbl> <dbl>
       1 solcanne    30.2           0.27      40  0     7         0.2    0.17  12  
       2 solescourg  21             0.1       31  0.7   7         0      0.2    9  
       3 solpatate   27             0.21      27  3.7   7         0      0.28  12  
       4 solpois     39             0.15      27  0     7         0      0.28  12  
       5 solcarotte   1             0.7       40  0.82  5.36      0      0.05   7.5
       6 sollin      12.2           0.11      30  0.6   8.1       0      0.22  21  
       7 solbanane   70             0.2       50  0     7         0.2    0.17  25  
       8 solsorgho   22             0.12      35  1     7         0      0.2    0  
       9 solorge      9.9           0.07      50  9     6.1       0      0.25   3.6
      10 solciMout   10.2           0.16      30 65.3   8.4       0.05   0.28  11.5
      # ... with 18 more rows, and 24 more variables: ruisolnu <dbl>, obstarac <dbl>,
      #   pluiebat <dbl>, mulchbat <dbl>, zesx <dbl>, cfes <dbl>, z0solnu <dbl>,
      #   csurNsol <dbl>, penterui <dbl>, codecailloux <dbl>, codemacropor <dbl>,
      #   codefente <dbl>, codrainage <dbl>, profimper <dbl>, ecartdrain <dbl>,
      #   ksol <dbl>, profdrain <dbl>, coderemontcap <dbl>, capiljour <dbl>,
      #   humcapil <dbl>, codenitrif <dbl>, codedenit <dbl>, profdenit <dbl>,
      #   vpotdenit <dbl>
      
      $data_byLayer
      # A tibble: 140 x 10
         id     layer layer_depth layer_water_fiel~ layer_water_wil~ layer_bulk_densi~
         <chr>  <int>       <dbl>             <dbl>            <dbl>             <dbl>
       1 solca~     1          20              46.8             26.2              1.08
       2 solca~     2          20              46.4             27.4              1.09
       3 solca~     3          20              48.5             29.1              1.02
       4 solca~     4          20              50.1             25.5              0.99
       5 solca~     5          20              50.1             25.5              0.99
       6 soles~     1          30              22.4             13.3              1.44
       7 soles~     2          30              22               11.8              1.65
       8 soles~     3          30              21               11                1.65
       9 soles~     4          30              22                9                1.65
      10 soles~     5          30              21                9                1.49
      # ... with 130 more rows, and 4 more variables: cailloux <dbl>,
      #   typecailloux <dbl>, infil <dbl>, epd <dbl>
      
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
      
      $dict$layer
      id
      
      
      attr(,"class")
      [1] "cropr_input"

# soil data frame list

    Code
      soil
    Output
      $data
      # A tibble: 28 x 34
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
      # ... with 18 more rows, and 24 more variables: ruisolnu <dbl>, obstarac <dbl>,
      #   pluiebat <dbl>, mulchbat <dbl>, zesx <dbl>, cfes <dbl>, z0solnu <dbl>,
      #   csurNsol <dbl>, penterui <dbl>, codecailloux <dbl>, codemacropor <dbl>,
      #   codefente <dbl>, codrainage <dbl>, profimper <dbl>, ecartdrain <dbl>,
      #   ksol <dbl>, profdrain <dbl>, coderemontcap <dbl>, capiljour <dbl>,
      #   humcapil <dbl>, codenitrif <dbl>, codedenit <dbl>, profdenit <dbl>,
      #   vpotdenit <dbl>
      
      $data_byLayer
      # A tibble: 140 x 11
         id        id layer_depth layer_water_fiel~ layer_water_wil~ layer_bulk_densi~
         <chr>  <int>       <dbl>             <dbl>            <dbl>             <dbl>
       1 solca~     1          20              46.8             26.2              1.08
       2 solca~     2          20              46.4             27.4              1.09
       3 solca~     3          20              48.5             29.1              1.02
       4 solca~     4          20              50.1             25.5              0.99
       5 solca~     5          20              50.1             25.5              0.99
       6 soles~     1          30              22.4             13.3              1.44
       7 soles~     2          30              22               11.8              1.65
       8 soles~     3          30              21               11                1.65
       9 soles~     4          30              22                9                1.65
      10 soles~     5          30              21                9                1.49
      # ... with 130 more rows, and 5 more variables: cailloux <dbl>,
      #   typecailloux <dbl>, infil <dbl>, epd <dbl>, layer <int>
      
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

# soil data frame large misspell

    Could not find parameter names: `noGrg`, `epcc`.

# soil data frame long misspell

    Could not find parameter names: `noGrg`, `epcc`.

# soil data frame list misspell

    Could not find parameter names: `noGrg`, `epcc`.

# No data format found

    Automatic data format detection failed. See documentation of the `data_format` argument to select the data format manually.

# Soil data frame long modify data columns

    Code
      set_soil(soil_data_long, id = "name", layer_depth = "epc",
        layer_water_field_cap = "HCCF", layer_water_wilting_pt = "HMINF",
        layer_bulk_density_moist = "DAF", organic_N_conc = "norg", verbose = T,
        data_format = list("long", list(id = "name", param = 3, layer = "id", value = 5)))
    Message <cliMessage>
      i Soil data format detected: long data frame
      i Long data frame column detected for soil identification: `name`
      i Long data frame column detected for soil parameter names: `param`
      i Long data frame column detected for soil layers: `id`
      i Long data frame column detected for parameter values: `value`

---

    Code
      set_soil(soil_data_long, id = "name", layer_depth = "epc",
        layer_water_field_cap = "HCCF", layer_water_wilting_pt = "HMINF",
        layer_bulk_density_moist = "DAF", organic_N_conc = "norg", verbose = T,
        data_format = list("long", list(param = 3, layer = "id", value = 5)))
    Message <cliMessage>
      i Soil data format detected: long data frame
      i Long data frame column detected for soil identification: `name`
      i Long data frame column detected for soil parameter names: `param`
      i Long data frame column detected for soil layers: `id`
      i Long data frame column detected for parameter values: `value`

# Soil data frame long data columns error

    Column indices/names given in the argument `data_format` have to be integers or characters.

---

    Column index for `id`, `value` is missing.

---

    No candidate for column(s) `layer`, `value` was found. Use argument `data_format` to select the data columns manually.

---

    Code
      set_soil(soil_data_long_mod, id = "name", layer_depth = "epc",
        layer_water_field_cap = "HCCF", layer_water_wilting_pt = "HMINF",
        layer_bulk_density_moist = "DAF", organic_N_conc = "norg", verbose = T)
    Warning <simpleWarning>
      Automatic column detection for long data format was ambiguous. Selected `id` for `layer`, `value` for `value`. Specify columns using the argument `data_format` to suppress this warning.
    Message <cliMessage>
      i Soil data format detected: long data frame
      i Long data frame column detected for soil identification: `name`
      i Long data frame column detected for soil parameter names: `param`
      i Long data frame column detected for soil layers: `id`
      i Long data frame column detected for parameter values: `value`

# Soil data units

    Code
      soil
    Output
      $data
      # A tibble: 28 x 54
         id      type   argi organic_N_conc profhum  calc    pH concseuil albedo    q0
         <chr>   <chr> <dbl>          [g/g]   <dbl> <dbl> <dbl>     <dbl>  <dbl> <dbl>
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
      # ... with 18 more rows, and 44 more variables: ruisolnu <int>, obstarac <int>,
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
      #   epd_4 <int>, epd_5 <int>
      
      $data_byLayer
      # A tibble: 140 x 6
         id     layer layer_depth layer_water_fiel~ layer_water_wil~ layer_bulk_densi~
         <chr>  <chr>        [cm]             [g/g]            [g/g]           [g/cm3]
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
      # ... with 130 more rows
      
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

