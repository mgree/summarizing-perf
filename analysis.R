data <- read.csv('raw.csv')

times <- reshape(data, varying = c('tool1', 'tool1.min', 'tool2'), v.names='time', timevar='tool', times = c('tool1', 'tool1.min', 'tool2'), new.row.names = 1:1000, direction='long')
write.csv(times, 'times.csv', row.names = FALSE)

library('ggplot2')
p <- 
  ggplot(times, aes(y=time, fill=tool)) +
  labs(y = "time (s); log10")
box <- p + geom_boxplot(aes(x=tool)) + scale_y_log10(labels = signif)
ggsave("box.png", box, width=6, height=6)
violin <- p + geom_violin(aes(x=tool)) + scale_y_log10(labels = signif)
ggsave("violin.png", violin, width=6, height=6)

logshift = scales::trans_new("logshift", function (x) { log10(x)+2 },  function (x) { 10^(x-2) })
bars <- 
  p + 
  geom_col(aes(x=test), position = 'dodge') + 
  scale_y_continuous(
    trans=logshift,
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
    labels = signif,
    expand = c(0,0)) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x = element_blank())
ggsave("bar.png", bars, width=9, height=6)
