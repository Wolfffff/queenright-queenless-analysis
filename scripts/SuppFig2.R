```{r Supp Figure 2 (Filtered Pipeline Statistics)}
treatment <-  c('QL', 'QR')
preinterp <- c(2.4638e7, 2.427e7)
deadbeefilter <- c(2.4635e7,2.422e7)
speedfilter <- c(2.09e7,2.09e7)
sizefilter <- c(2.063e7,2.064e7)
spacefilter <- c(2.061e7,2.062e7)
interp <- c(2.54e7,2.48e7)
data <- data.frame(treatment, preinterp, deadbeefilter, speedfilter, sizefilter,spacefilter,interp)
df <- melt(data ,  id.vars = 'treatment', variable.name = 'filter')
ggplot(data=df, aes(x=filter, y=value)) +
  geom_bar(stat="identity", aes(fill=treatment))+
  scale_fill_manual(
    labels = c( "Queenless Worker","Queenright Worker"),
    values = c( "#161414","#429CF0"))+
  theme_minimal()+
  facet_wrap(~treatment,scales = "free_x")

```
