library(linkprediction)
library(igraph)

g <- igraph::make_graph(~ A -- C:D:E -- B -- F -- G:H -- I)
plot(g)
# Adamic-Adar
proxfun(g, method="aa", value="edgelist")

# Random Walk with Restart
proxfun(g, method="rwr", value="edgelist")

proxfun(g, method="aa", v1 = V(g), value = "matrix")
proxfun(g, method="aa", value = "matrix")
proxfun(g, method="aa", value = "edgelist")
proxfun(g, method="aa")


