# load vector

source("scripts/call_out.R", echo = F)
source("scripts/logical_question.R", echo = F)
source("scripts/column_filtration.R", echo = F)

# path <- readline(prompt = "Zadejte cestu k referencni vektorove Vrstve mapovani biotopu: \n(neco/jako/tohle/vector.shp)\ndata/VMB/VMB_bio_hab.shp\n") # cesta
path <- "data/VMB/VMB_Boletice.shp"
vector <- vect(path)

if(logical_question("Is this vector layer the Habitat Mapping layer of Czechia?")){
  source("scripts/VMB_spec.R", echo = F)
  stop("Vrstva mapování biototopů zprocesována, končím skript")
}

# year filtration
# do you want to filter something based on date?
# than should be question, which collumn contain date

if (call_out("any layer column")){
  vector <- select_cols(vector)
}

