# token columns are created

    Code
      tokenize(data)
    Output
      # A tibble: 770 x 6
         source              uid                participant nwords token relative_time
         <chr>               <chr>              <chr>        <int> <chr>         <dbl>
       1 catalan1/file01.cha catalan1file01cha~ def              5 "hig~        315009
       2 catalan1/file01.cha catalan1file01cha~ def              5 "lev~        315324
       3 catalan1/file01.cha catalan1file01cha~ def              5 ""           315638
       4 catalan1/file01.cha catalan1file01cha~ def              5 ""           315953
       5 catalan1/file01.cha catalan1file01cha~ def              5 "eh?"        316267
       6 catalan1/file01.cha catalan1file01cha~ abc              1 "sí"         315042
       7 catalan1/file01.cha catalan1file01cha~ abc              6 "que"        316002
       8 catalan1/file01.cha catalan1file01cha~ abc              6 "com"        316133
       9 catalan1/file01.cha catalan1file01cha~ abc              6 "ha"         316263
      10 catalan1/file01.cha catalan1file01cha~ abc              6 "can~        316394
      # i 760 more rows

