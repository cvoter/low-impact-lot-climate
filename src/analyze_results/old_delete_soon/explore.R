load(sprintf("%s/hourly.precipitation.ET0.Rda", sprintf('%s/results/met', project.dir)))

subset.precip = subset(hourly.precipitation.ET0, city.index == 4)

subset.precip = subset.precip[order(subset.precip$precipitation),]
for (i in 1:length(subset.precip$precipitation)) {
  subset.precip$cum[i] = sum(subset.precip$precipitation[1:i])
  subset.precip$cum.percent[i] = 100*subset.precip$cum[i]/
                                 sum(subset.precip$precipitation)
}
percentiles <- c(50, 75, 90)
threshold = NULL
for (i in 1:length(percentiles)){
  threshold[i] = subset.precip$precipitation[which.min(abs(subset.precip$cum.percent - percentiles[i]))] 
}

ggplot(subset.precip) +
  geom_line(aes(x = precipitation, 
                y = cum.percent)) + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Precipitation Intensity (mm/hr)",
       y = "Percent of Total Precipitation") +
  theme_bw() +
  theme(text = element_text(size=11, family="Segoe UI Semilight"),
        axis.title = element_text(size=11, family="Segoe UI Semibold"),
        legend.title = element_text(size=11, family="Segoe UI Semibold"))

all.storms = NULL
for (i in 1:nlocations) {
  # Storm data
  storms = subset(all.city.storms.runoff, city.index==i)
  
  # Storm metrics
  nstorms = length(storms$storm.index)
  total.reduction.runoff = sum(storms$runoff.delta)
  
  storms = storms[order(-storms$runoff.delta),]
  
  sum.change = 0
  for (s in 1:nrow(storms)){
    sum.change = sum.change + storms$runoff.delta[s]
    if(sum.change <= total.reduction.runoff/3 || s == 1){
      storms$tertile[s] = 1
    } else if (sum.change <= 2*total.reduction.runoff/3){
      storms$tertile[s] = 2
    } else {
      storms$tertile[s] = 3
    }
  }

  all.storms = as.data.frame(rbind(all.storms, storms))
}

ggplot(data = all.storms) +
  geom_point(aes(x = (intensity.max - intensity.avg), 
                 y = runoff.delta,
                 color = as.factor(tertile)),
             shape = 16, size = 3) +
  scale_x_log10()+
  scale_color_brewer(name = "Diff",
                        palette = "Blues",
                        direction = 1) +
  labs(x = "Intensity*Depth (mm^2/hr)",
       y = "Reduction in Runoff (mm)") +
  theme_bw()+
  theme(text = element_text(size=11, family="Segoe UI Semilight"),
        axis.title = element_text(size=11, family="Segoe UI Semibold"),
        legend.title = element_text(size=11, family="Segoe UI Semibold"))
