library (ggplot2)

mut_data = read.delim("~/Downloads/MutationRate.txt")

log_plot = ggplot(data=mut_data, aes(x=Genotype, y=MutationRate, color=Genotype)) + geom_jitter(width=0.1, alpha=0.5, size=4) + facet_grid(rows=vars(mut_data$MutationType), cols=vars(mut_data$Genome)) + scale_y_log10() + theme_test() + scale_color_brewer(palette = "Dark2") + theme(legend.position = "none")

linear_plot = ggplot(data=mut_data, aes(x=Genotype, y=MutationRate, color=Genotype)) + geom_jitter(width=0.1, alpha=0.5, size=4) + facet_grid(rows=vars(mut_data$MutationType), cols=vars(mut_data$Genome)) + theme_test() + scale_color_brewer(palette = "Dark2") + theme(legend.position = "none")
