###
# See https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html
###

library(GeoPressureR)

## OPTION 1: Run workflow step-by-step for a single tag
id <- "F24" # Run a single tag
geopressuretemplate_config(id)
tag <- geopressuretemplate_tag(id)
graph <- geopressuretemplate_graph(id)
geopressuretemplate_pressurepath(id)


## OPTION 2: Run entire workflow for all tags
list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)

for (id in list_id){
  cli::cli_h1("Run for {id}")
  geopressuretemplate(id)
}



###### PLOT of the products #####
#library(maps)
path_most_likely <- graph_most_likely(graph)
#kable(path_most_likely)
PML <- plot_path(path_most_likely,  plot_leaflet = F)
PML

path_most_likely$duration <- round(stap2duration(tag$stap),2)
path_most_likely$id <- id
write.csv(path_most_likely, file = paste("./output/PathMostLikely/pathmostlikely_",id,".csv"))



##Marginal porbability map
marginal <- graph_marginal(graph)
plot(marginal,  path = path_most_likely)
#plot(marginal, path = path_most_likely, plot_leaflet = FALSE, add=TRUE)

#Only for checking probability, no editing:
geopressureviz(tag, marginal=marginal, path=path_most_likely)


