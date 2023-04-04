# code from https://ideophone.org/snailplot-rmd/

local_elpaco_dir <- file.path("","Users", "evaviviani", "github", "benchmarking_elpaco")
d <- readr::read.csv(file.path(local_elpaco_dir, "d_sample.csv"))

d_simple <- d |> select(begin, end, duration, participant, uid,
                        nwords, nchar, n, rank, freq, overlap,
                        priorby, FTO, overlapped, talk_all, talk_rel,
                        load, transitions)

d_simple$participant <- as.factor(d_simple$participant)
d_simple$participant_int <- as.numeric(d_simple$participant)
summary(d_simple)

extract_length <- 600000 # 10 min
window_size <- 60000 # 1 min
window_breaks <- as.integer(c(0:round(extract_length/window_size)) * window_size)

extract <- d_simple %>%
  mutate(end = end - min(begin, na.rm = TRUE), # reset timestamps to start from 0
         begin = begin - min(begin, na.rm = TRUE),
         line = cut(begin, window_breaks, right = FALSE, labels = FALSE)) %>%
  drop_na(line) %>%
  group_by(line) %>%
  mutate(begin0 = begin - min(begin, na.rm = TRUE), # reset timestamps to 0 for each new line
         end0 = end - min(begin, na.rm = TRUE)) %>%
  ungroup()

plot.base <- extract[1:2000,] %>%
  ggplot(aes(y = participant_int)) +
  ggthemes::theme_tufte() + theme(legend.position = "none",
                                  strip.text = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  plot.title.position = "plot") +
  ylab("") + xlab("time (s)") +
  viridis::scale_fill_viridis(option = "plasma", direction = 1, begin = 0.2, end = 0.8) +
  scale_y_reverse(breaks = seq(1, max(extract$line, 1)),
                  labels = seq(1, max(extract$line, 1))) +
  scale_x_continuous(limits = c(0, window_size),
                     breaks = seq(0, window_size, 10000),
                     label = seq(0, window_size / 1000, 10),
                     oob = scales::oob_keep)

plot.simple_grid <- plot.base +
  theme(axis.text.y = element_text()) +
  ggtitle("Ten minutes of conversation",
          subtitle = "Points are interjections, time moves left-right and top-bottom") +
  geom_rect(aes(xmin = begin0, xmax = end0,
                ymin = line - 0.5 + participant_int / 3 - 0.2,
                ymax = line - 0.5 + participant_int / 3 + 0.2),
            linewidth = 0, colour = NA, fill = "lightgrey") +
  geom_point(data = . %>% filter(nwords == 1 & transitions == 1),
             aes(x = begin0 + 200,
                 fill = rank,
                 y = line - 0.5 + participant_int / 3),
             colour = "white",
             size = 2,
             shape = 21,
             stroke = 1)
plot.simple_grid


#write.csv(extract[1:2000, ], 'tests/testthat/testdata/sample_test.csv', col.names = TRUE, row.names = FALSE, quote = FALSE)
