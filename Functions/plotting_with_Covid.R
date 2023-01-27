plotting_covid <- function(df,facet_var,title){
  ggplot() +
    geom_area(data=cases.weeks, aes(x=weeks, y=cases), fill="white", colour="black") + 
    geom_line(data=df,aes(x=weeks, y=contacts*1000, colour=factor(facet_var), linetype=factor(wgt), shape=factor(wgt), group=interaction(factor(facet_var), factor(wgt)))) + 
    geom_point() +  
    geom_vline(xintercept=c(23,31), linetype="dotted", size = 1, colour = "red") +
    labs(title=as.character(title), x="Calender Weeks", y="Number of Contacts",colour= as.character(facet_var)) +
    scale_linetype_discrete(name="Weighting", labels=c("Poststratification", "No Weighting")) +
    scale_y_continuous("COVID-19 cases", sec.axis = sec_axis(~ . / 1000, name = "Number of Contacts")) +
    guides(shape="none") +
    theme_minimal()
}