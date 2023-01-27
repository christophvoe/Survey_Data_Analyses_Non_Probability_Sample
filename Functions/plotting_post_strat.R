plotting_post <- function(data,facet_var,title){
    ggplot(data, aes(x=weeks, y=contacts, colour= factor(facet_var), linetype=factor(wgt), shape=factor(wgt), group=interaction(factor(facet_var), factor(wgt)))) + 
    geom_line() +
    geom_point() +
    labs(title=as.character(title), x="Calender Weeks", y="Number of Contacts",colour= as.character(facet_var)) +
    scale_linetype_discrete(name="Weighting", labels=c("Poststratification", "No Weighting")) +
    guides(shape="none") +
    theme_minimal()
}
