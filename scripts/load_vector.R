# load VMB

# path <- readline(prompt = "Zadejte cestu k referencni vektorove Vrstve mapovani biotopu: \n(neco/jako/tohle/vector.shp)\ndata/VMB/VMB_bio_hab.shp\n") # cesta
path <- "data/VMB/VMB_Boletice.shp"
vector <- vect(path)

source("scripts/call_out.R", echo = F)

# year filtration
# do you want to filter something based on date?
# than should be question, which collumn contain date
