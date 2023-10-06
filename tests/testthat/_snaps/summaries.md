# summary reports are accurate

    Code
      cat(report_summaries(data, lang = "dutch", allsources = FALSE))
    Output
      
      ### 5 hours
      
      | turns| translated| words| turnduration| talkprop| people| hours| turns_per_h|
      |-----:|----------:|-----:|------------:|--------:|------:|-----:|-----------:|
      | 14022|          0| 69169|         1257|     0.98|      3|     5|        2804|
      
      
      ### nature
      
      |nature |     n|
      |:------|-----:|
      |laugh  |   599|
      |talk   | 13366|
      |NA     |    57|
      
      
      Showing only the first 10 sources; use `allsources=T` to show all
      
      ### 20 sources
      
      |source          | turns| translated| words| people| talkprop| minutes| hours|
      |:---------------|-----:|----------:|-----:|------:|--------:|-------:|-----:|
      |/dutch2/DVA10O  |   501|          0|  3498|      2|      0.9|      15|  0.25|
      |/dutch2/DVA11Q  |   792|          0|  3318|      2|      1.0|      15|  0.25|
      |/dutch2/DVA12S  |   640|          0|  3112|      2|      0.9|      15|  0.25|
      |/dutch2/DVA13U  |   717|          0|  3548|      2|      1.0|      15|  0.25|
      |/dutch2/DVA14W  |   721|          0|  3099|      2|      0.9|      15|  0.25|
      |/dutch2/DVA15Y  |   770|          0|  3387|      2|      1.1|      15|  0.25|
      |/dutch2/DVA16AA |   604|          0|  3889|      2|      1.1|      15|  0.25|
      |/dutch2/DVA17AC |   782|          0|  3888|      2|      1.0|      15|  0.25|
      |/dutch2/DVA19AG |   648|          0|  2957|      2|      0.9|      15|  0.25|
      |/dutch2/DVA1A   |   681|          0|  3432|      2|      1.0|      15|  0.25|
      

---

    Code
      cat(report_summaries(data, lang = "dutch", allsources = TRUE))
    Output
      
      ### 5 hours
      
      | turns| translated| words| turnduration| talkprop| people| hours| turns_per_h|
      |-----:|----------:|-----:|------------:|--------:|------:|-----:|-----------:|
      | 14022|          0| 69169|         1257|     0.98|      3|     5|        2804|
      
      
      ### nature
      
      |nature |     n|
      |:------|-----:|
      |laugh  |   599|
      |talk   | 13366|
      |NA     |    57|
      
      
      ### 20 sources
      
      |source          | turns| translated| words| people| talkprop| minutes| hours|
      |:---------------|-----:|----------:|-----:|------:|--------:|-------:|-----:|
      |/dutch2/DVA10O  |   501|          0|  3498|      2|      0.9|      15|  0.25|
      |/dutch2/DVA11Q  |   792|          0|  3318|      2|      1.0|      15|  0.25|
      |/dutch2/DVA12S  |   640|          0|  3112|      2|      0.9|      15|  0.25|
      |/dutch2/DVA13U  |   717|          0|  3548|      2|      1.0|      15|  0.25|
      |/dutch2/DVA14W  |   721|          0|  3099|      2|      0.9|      15|  0.25|
      |/dutch2/DVA15Y  |   770|          0|  3387|      2|      1.1|      15|  0.25|
      |/dutch2/DVA16AA |   604|          0|  3889|      2|      1.1|      15|  0.25|
      |/dutch2/DVA17AC |   782|          0|  3888|      2|      1.0|      15|  0.25|
      |/dutch2/DVA19AG |   648|          0|  2957|      2|      0.9|      15|  0.25|
      |/dutch2/DVA1A   |   681|          0|  3432|      2|      1.0|      15|  0.25|
      |/dutch2/DVA20AI |   758|          0|  3328|      2|      1.0|      15|  0.25|
      |/dutch2/DVA22AL |   745|          0|  3549|      2|      1.0|      15|  0.25|
      |/dutch2/DVA24AK |   685|          0|  3087|      2|      1.0|      15|  0.25|
      |/dutch2/DVA2C   |   765|          0|  4448|      2|      1.2|      15|  0.25|
      |/dutch2/DVA3E   |   753|          0|  3135|      2|      0.9|      15|  0.25|
      |/dutch2/DVA4C   |   719|          0|  3298|      2|      0.9|      15|  0.25|
      |/dutch2/DVA6H   |   683|          0|  3499|      2|      1.0|      15|  0.25|
      |/dutch2/DVA7B   |   691|          0|  3754|      2|      1.0|      15|  0.25|
      |/dutch2/DVA8K   |   612|          0|  2998|      2|      0.9|      15|  0.25|
      |/dutch2/DVA9M   |   755|          0|  3945|      2|      0.9|      15|  0.25|
      

