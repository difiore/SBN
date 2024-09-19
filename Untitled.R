library(tidyverse)
library(jsonlite)

matches <- data.frame() # grabbing all the match-level data (teams, scores, etc.)

for (i in c(21:27,37:41)){ # selecting the season id's corresponding to the free Messi data

  df <- fromJSON(paste("https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/11/",i,".json",sep=""),flatten=T) %>% data.frame()

  matches <- plyr::rbind.fill(matches,df)

}

events <- data.frame() # grabbing the detailed event-level data

for (i in 1:nrow(matches)){

  df <- fromJSON(paste("https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/",matches$match_id[i],".json",sep=""),flatten=T) %>% data.frame()

  events <- plyr::rbind.fill(events,data.frame(df,match_id=matches$match_id[i]))

}

# combining the match- and event-level data, plus adding goal dummy for each possession, and a unique identifier for each possession

messi <- left_join(events,matches,by="match_id") %>%
  group_by(match_id,possession) %>%
  mutate(goal = sum(shot.outcome.id==97,na.rm=T))

messi$possid <- group_indices(messi)

messi <- messi %>%
  mutate(player.name = recode(player.name,
                              "Adriano Correia Claro"="Adriano",
                              "Aleix Vidal Parreu"="Vidal",
                              "Alexandre Dimitri Song-Billong"="Song",
                              "Alexis Alejandro Sánchez Sánchez"="Alexis",
                              "Aliaksandr Hleb"="Hleb",
                              "Anderson Luís de Souza"="Deco",
                              "Andrés Iniesta Luján"="Iniesta",
                              "Andreu Fontàs Prat"="Fontas",
                              "Arda Turan"="Arda",
                              "Bojan Krkíc Pérez"="Bojan",
                              "Carles Puyol i Saforcada"="Puyol",
                              "Claudio Andrés Bravo Muñoz"="Bravo",
                              "Cristian Tello Herrera"="Tello",
                              "Damià Abella Pérez"="Abella",
                              "Daniel Alves da Silva"="Dani Alves",
                              "David Villa Sánchez"="Villa",
                              "Dmytro Chygrynskiy"="Chygrynskiy",
                              "Douglas Pereira dos Santos"="Douglas",
                              "Eiður Smári Guðjohnsen"="Gudjohnsen",
                              "Eric-Sylvain Bilal Abidal"="Abidal",
                              "Fernando Navarro i Corbacho"="Navarro",
                              "Francesc Fàbregas i Soler"="Fabregas",
                              "Gabriel Alejandro Milito"="Milito",
                              "Gabriel Francisco García de la Torre"="Gabri",
                              "Gerard Deulofeu Lázaro"="Deulofeu",
                              "Gerard Gumbau Garriga"="Gumbau",
                              "Gerard Piqué Bernabéu"="Pique",
                              "Gianluca Zambrotta"="Zambrotta",
                              "Giovani dos Santos Ramírez"="Giovano",
                              "Giovanni van Bronckhorst"="van Bronckhorst",
                              "Gnégnéri Yaya Touré"="Toure",
                              "Henrik Larsson"="Larsson",
                              "Ibrahim Afellay"="Afellay",
                              "Ivan Rakitic"="Rakitic",
                              "Javier Alejandro Mascherano"="Mascherano",
                              "Javier Pedro Saviola Fernández"="Saviola",
                              "Jean Marie Dongou Tsafack"="Dongou",
                              "Jeffren Isaac Suárez Bermúdez"="Jeffren",
                              "Jérémy Mathieu"="Mathieu",
                              "Jonathan dos Santos Ramírez"="Jonathan dos Santos",
                              "Jordi Alba Ramos"="Alba",
                              "Jordi Masip López"="Masip",
                              "José Edmílson Gomes de Moraes"="Edmilson",
                              "José Manuel Pinto Colorado"="Pinto",
                              "José Manuel Rueda Sampedro"="Rueda",
                              "José Martín Cáceres Silva"="Caceres",
                              "Juan Isaac Cuenca López"="Cuenca",
                              "Juliano Haus Belletti"="Belletti",
                              "Lilian Thuram"="Thuram",
                              "Lionel Andrés Messi Cuccittini"="Messi",
                              "Ludovic Giuly"="Giuly",
                              "Luis Alberto Suárez Díaz"="Suarez",
                              "Manuel Agudo Durán"="Nolito",
                              "Marc-André ter Stegen"="ter Stegen",
                              "Marc Bartra Aregall"="Bartra",
                              "Marc Muniesa Martínez"="Muniesa",
                              "Mark van Bommel"="van Bommel",
                              "Martín Montoya Torralbo"="Montoya",
                              "Maximiliano Gastón López"="Maxi Lopez",
                              "Maxwell Scherrer Cabelino Andrade"="Maxwell",
                              "Munir El Haddadi Mohamed"="Munir",
                              "Neymar da Silva Santos Junior"="Neymar",
                              "Oleguer Presas Renom"="Oleguer",
                              "Pedro Eliezer Rodríguez Ledesma"="Pedro",
                              "Rafael Alcântara do Nascimento"="Rafinha",
                              "Rafael Márquez Álvarez"="Marquez",
                              "Rafael Romero Serrano"="Fali",
                              "Ronaldo de Assis Moreira"="Ronaldinho",
                              "Rubén Iván Martínez Andrade"="Ruben",
                              "Samuel Eto'o Fils"="Eto'o",
                              "Sandro Ramírez Castillo"="Sandro",
                              "Santiago Ezquerro Marín"="Ezquerro",
                              "Sergi Roberto Carnicer"="Sergi Roberto",
                              "Sergi Samper Montaña"="Samper",
                              "Sergio Busquets i Burgos"="Busquets",
                              "Sergio Rodríguez García"="Rodri",
                              "Seydou Kéita"="Kéita",
                              "Sylvio Mendes Campos Junior"="Sylvinho",
                              "Thiago Alcântara do Nascimento"="Thiago",
                              "Thiago Motta"="Motta",
                              "Thierry Henry"="Henry",
                              "Thomas Vermaelen"="Vermaelen",
                              "Víctor Sánchez Mata"="Victor Sanchez",
                              "Víctor Valdés Arribas"="Valdes",
                              "Víctor Vázquez Solsona"="Vazquez",
                              "Xavier Hernández Creus"="Xavi",
                              "Zlatan Ibrahimovic"="Zlatan")) %>%
  mutate(pass.recipient.name = recode(pass.recipient.name,
                                      "Adriano Correia Claro"="Adriano",
                                      "Aleix Vidal Parreu"="Vidal",
                                      "Alexandre Dimitri Song-Billong"="Song",
                                      "Alexis Alejandro Sánchez Sánchez"="Alexis",
                                      "Aliaksandr Hleb"="Hleb",
                                      "Anderson Luís de Souza"="Deco",
                                      "Andrés Iniesta Luján"="Iniesta",
                                      "Andreu Fontàs Prat"="Fontas",
                                      "Arda Turan"="Arda",
                                      "Bojan Krkíc Pérez"="Bojan",
                                      "Carles Puyol i Saforcada"="Puyol",
                                      "Claudio Andrés Bravo Muñoz"="Bravo",
                                      "Cristian Tello Herrera"="Tello",
                                      "Damià Abella Pérez"="Abella",
                                      "Daniel Alves da Silva"="Dani Alves",
                                      "David Villa Sánchez"="Villa",
                                      "Dmytro Chygrynskiy"="Chygrynskiy",
                                      "Douglas Pereira dos Santos"="Douglas",
                                      "Eiður Smári Guðjohnsen"="Gudjohnsen",
                                      "Eric-Sylvain Bilal Abidal"="Abidal",
                                      "Fernando Navarro i Corbacho"="Navarro",
                                      "Francesc Fàbregas i Soler"="Fabregas",
                                      "Gabriel Alejandro Milito"="Milito",
                                      "Gabriel Francisco García de la Torre"="Gabri",
                                      "Gerard Deulofeu Lázaro"="Deulofeu",
                                      "Gerard Gumbau Garriga"="Gumbau",
                                      "Gerard Piqué Bernabéu"="Pique",
                                      "Gianluca Zambrotta"="Zambrotta",
                                      "Giovani dos Santos Ramírez"="Giovano",
                                      "Giovanni van Bronckhorst"="van Bronckhorst",
                                      "Gnégnéri Yaya Touré"="Toure",
                                      "Henrik Larsson"="Larsson",
                                      "Ibrahim Afellay"="Afellay",
                                      "Ivan Rakitic"="Rakitic",
                                      "Javier Alejandro Mascherano"="Mascherano",
                                      "Javier Pedro Saviola Fernández"="Saviola",
                                      "Jean Marie Dongou Tsafack"="Dongou",
                                      "Jeffren Isaac Suárez Bermúdez"="Jeffren",
                                      "Jérémy Mathieu"="Mathieu",
                                      "Jonathan dos Santos Ramírez"="Jonathan dos Santos",
                                      "Jordi Alba Ramos"="Alba",
                                      "Jordi Masip López"="Masip",
                                      "José Edmílson Gomes de Moraes"="Edmilson",
                                      "José Manuel Pinto Colorado"="Pinto",
                                      "José Manuel Rueda Sampedro"="Rueda",
                                      "José Martín Cáceres Silva"="Caceres",
                                      "Juan Isaac Cuenca López"="Cuenca",
                                      "Juliano Haus Belletti"="Belletti",
                                      "Lilian Thuram"="Thuram",
                                      "Lionel Andrés Messi Cuccittini"="Messi",
                                      "Ludovic Giuly"="Giuly",
                                      "Luis Alberto Suárez Díaz"="Suarez",
                                      "Manuel Agudo Durán"="Nolito",
                                      "Marc-André ter Stegen"="ter Stegen",
                                      "Marc Bartra Aregall"="Bartra",
                                      "Marc Muniesa Martínez"="Muniesa",
                                      "Mark van Bommel"="van Bommel",
                                      "Martín Montoya Torralbo"="Montoya",
                                      "Maximiliano Gastón López"="Maxi Lopez",
                                      "Maxwell Scherrer Cabelino Andrade"="Maxwell",
                                      "Munir El Haddadi Mohamed"="Munir",
                                      "Neymar da Silva Santos Junior"="Neymar",
                                      "Oleguer Presas Renom"="Oleguer",
                                      "Pedro Eliezer Rodríguez Ledesma"="Pedro",
                                      "Rafael Alcântara do Nascimento"="Rafinha",
                                      "Rafael Márquez Álvarez"="Marquez",
                                      "Rafael Romero Serrano"="Fali",
                                      "Ronaldo de Assis Moreira"="Ronaldinho",
                                      "Rubén Iván Martínez Andrade"="Ruben",
                                      "Samuel Eto'o Fils"="Eto'o",
                                      "Sandro Ramírez Castillo"="Sandro",
                                      "Santiago Ezquerro Marín"="Ezquerro",
                                      "Sergi Roberto Carnicer"="Sergi Roberto",
                                      "Sergi Samper Montaña"="Samper",
                                      "Sergio Busquets i Burgos"="Busquets",
                                      "Sergio Rodríguez García"="Rodri",
                                      "Seydou Kéita"="Kéita",
                                      "Sylvio Mendes Campos Junior"="Sylvinho",
                                      "Thiago Alcântara do Nascimento"="Thiago",
                                      "Thiago Motta"="Motta",
                                      "Thierry Henry"="Henry",
                                      "Thomas Vermaelen"="Vermaelen",
                                      "Víctor Sánchez Mata"="Victor Sanchez",
                                      "Víctor Valdés Arribas"="Valdes",
                                      "Víctor Vázquez Solsona"="Vazquez",
                                      "Xavier Hernández Creus"="Xavi",
                                      "Zlatan Ibrahimovic"="Zlatan"))

# Messi's involvement in goalscoring possessions

poss_inv <- messi %>%
  filter(possession_team.name == "Barcelona") %>% # filtering for Barcelona possessions
  filter(type.id %in% c(42,14,16,21,30,43)) %>% # filtering our attacking actions
  filter(goal > 0) %>% # selecting only goalscoring possessions
  ungroup() %>%
  group_by(player.name, season.season_name) %>%
  summarize(messi_poss = n_distinct(possid)) %>%
  filter(player.name == "Messi")

# Total involvement in goalscoring possessions

poss_tot <- messi %>%
  filter(possession_team.name == "Barcelona") %>%
  filter(type.id %in% c(42,14,16,21,30,43)) %>%
  filter(goal > 0) %>%
  ungroup() %>%
  group_by(season.season_name) %>%
  summarize(total_poss = n_distinct(match_id, possession))

# Combined set

comp_inv <- left_join(poss_inv,poss_tot,by="season.season_name") %>%
  mutate(rate = messi_poss / total_poss)

# Plotting

library(hrbrthemes)
# update_geom_font_defaults(font_rc)

comp_inv %>%
  ggplot() +
  geom_col(aes(x=season.season_name,y=rate),colour=ft_cols$red,width=0.02) + # creating slim columns for each season, styled with hrbrthemes' FT colour palette
  geom_point(aes(x=season.season_name,y=rate),size=5,shape=21,fill=ft_cols$red,colour="black") + # adding large points at the column end
  coord_flip() + # flipping to a horizontal view
  labs(
    x="Season",
    y="Rate of Messi involvement in goalscoring possessions",
    caption="Statsbomb data | @wiscostretford",
    title="The GOAT, Omnipresent",
    subtitle="Messi's rate of involvement (having an attacking touch) in Barcelona possessions leading to goals.\nIncludes La Liga games Messi played in.")

poss_inv2 <- messi %>%
  filter(possession_team.name == "Barcelona") %>%
  filter(type.id %in% c(42,14,16,21,30,43)) %>%
  filter(goal > 0) %>%
  ungroup() %>%
  group_by(player.name, season.season_name) %>%
  summarize(messi_poss = n_distinct(possid))

comp_inv2 <- left_join(poss_inv2,poss_tot,by="season.season_name") %>%
  mutate(rate = messi_poss / total_poss)

# finding the *other* (non-Messi) top two in rate of involvementment in goalscoring possessions

other_tops <- comp_inv2 %>%
  tibble::rowid_to_column(., "id") %>%
  filter(player.name != "Messi") %>%
  group_by(season.season_name) %>%
  top_n(2,rate) %>%
  arrange(-rate)

# plotting

library(ggrepel)

ggplot() +

  ## adding the usual Messi layer

  geom_col(data=comp_inv2 %>% filter(player.name == "Messi"), aes(x=season.season_name,y=rate),colour=ft_cols$red,width=0.02) +
  geom_point(data=comp_inv2 %>% filter(player.name == "Messi"), aes(x=season.season_name,y=rate),size=5,shape=21,fill=ft_cols$red,colour="black") +

  ## adding the additional points, labelled with ggrepel's geom_text_repel to avoid overlap

  geom_point(data = other_tops,aes(x=season.season_name,y=rate),shape=21,colour="red",fill="lightgrey") +
  geom_text_repel(data = other_tops,aes(x=season.season_name,y=rate,label=player.name),size=2) +

  ## flip, labels, and theme

  coord_flip() +
  labs(
    x="Season",
    y="Rate of Messi involvement in goalscoring possessions",
    caption="Statsbomb data | @wiscostretford",
    title="The GOAT, Omnipresent",
    subtitle="Messi's rate of involvement (having an attacking touch) in Barcelona possessions leading to goals.\nIncludes only La Liga games Messi played in.")




# Messi Pass Partners

# First, we need to structure the data on passing connections. For each 'Pass' (type.name), we grab the player.name and pass.recipient.name, filtering out missing recipients.

partners <- messi %>%
  filter(type.name == "Pass", team.name == "Barcelona") %>%
  group_by(player.name, pass.recipient.name, season.season_name) %>%
  summarize(n=n()) %>% # adding a column representing the 'weights' or the intensity (frequency) of passing connections
  filter(!is.na(pass.recipient.name)) %>%
  ungroup()

colnames(partners) <- c("from","to","season","weight") # because igraph requires it, we'll rename the columns of the 'partners' data.frame (essentially an edgelist)

library(tidyverse)
partners <- read_csv("data/partners.csv")

# Creating a global layout across season that pins the layout and the weights

library(igraph)

graph <- graph_from_data_frame(partners, directed=F) # the passing data *is*, in fact, directed (one passer, one pass recipient), but since we're interested in the overall patterns, we set the network as undirected
layout <- layout_in_circle(graph) # this sets the fixed layout as a linear, circular chord
weights <- data.frame(as_edgelist(graph),weight=E(graph)$weight,season=E(graph)$season)

plot(graph, layout = layout)

# Creating a chord diagram for each season with a for loop

library(ggraph)

for (i in 2004:2004){

  ## creating the graph element with igraph

  graph <- graph_from_data_frame(
    partners %>%
      ungroup() %>%
      filter(season == paste0(i,"/",i+1)) %>%
      select(-season)
    , directed=F)

  ## adding a 'strength' variable to size the connections

  V(graph)$strength <- strength(graph)

  # First, we need to structure the data on passing connections. For each 'Pass' (type.name), we grab the player.name and pass.recipient.name, filtering out missing recipients.

partners <- messi %>%
  filter(type.name == "Pass", team.name == "Barcelona") %>%
  group_by(player.name, pass.recipient.name, season.season_name) %>%
  summarize(n=n()) %>% # adding a column representing the 'weights' or the intensity (frequency) of passing connections
  filter(!is.na(pass.recipient.name)) %>%
  ungroup()

colnames(partners) <- c("from","to","season","weight") # because igraph requires it, we'll rename the columns of the 'partners' data.frame (essentially an edgelist)

# Creating a global layout across season that pins the layout and the weights

library(igraph)

graph <- graph_from_data_frame(partners, directed=F) # the passing data *is*, in fact, directed (one passer, one pass recipient), but since we're interested in the overall patterns, we set the network as undirected
layout <- create_layout(graph,layout="linear",circular=T) # this sets the fixed layout as a linear, circular chord
weights <- data.frame(as_edgelist(graph),weight=E(graph)$weight,season=E(graph)$season)

# Creating a chord diagram for each season with a for loop

library(ggraph)

for (i in 2004:2015){

  ## creating the graph element with igraph

  graph <- graph_from_data_frame(
    partners %>%
    ungroup() %>%
    filter(season == paste0(i,"/",i+1)) %>%
    select(-season)
    , directed=F)

  ## adding a 'strength' variable to size the connections

  V(graph)$strength <- strength(graph)

  plot(graph, layout = layout)

}


# First, we need to structure the data on passing connections. For each 'Pass' (type.name), we grab the player.name and pass.recipient.name, filtering out missing recipients.

partners <- messi %>%
  filter(type.name == "Pass", team.name == "Barcelona") %>%
  group_by(player.name, pass.recipient.name, season.season_name) %>%
  summarize(n=n()) %>% # adding a column representing the 'weights' or the intensity (frequency) of passing connections
  filter(!is.na(pass.recipient.name)) %>%
  ungroup()

colnames(partners) <- c("from","to","season","weight") # because igraph requires it, we'll rename the columns of the 'partners' data.frame (essentially an edgelist)

write_csv(partners, "data/partners.csv")

library(tidyverse)
partners <- read_csv("data/partners.csv")

# Creating a global layout across season that pins the layout and the weights

library(igraph)
graph <- graph_from_data_frame(partners, directed = FALSE) # the passing data *is*, in fact, directed (one passer, one pass recipient), but since we're interested in the overall patterns, we set the network as undirected
global_layout <- layout_in_circle(graph) # this sets the fixed layout as a linear, circular chord
# global_layout <- tibble(name = as_ids(V(graph)), x = global_layout[,1], y = global_layout[,2])
weights <- data.frame(as_edgelist(graph), weight=E(graph)$weight, season=E(graph)$season)
V(graph)$strength <- strength(graph)
V(graph)$label <- 1:length(V(graph))
ids <- tibble(name = V(graph)$name, label = V(graph)$label)

library(ggraph)
ggraph(graph, layout = global_layout) +
  geom_edge_link(aes(width=weight,alpha=weight),colour="lightgrey") +
  geom_node_point(aes(size=strength),shape=21,colour="#004C99",fill="#A70042",alpha=0.8) +
  geom_node_text(aes(label=name,size=strength),colour="black",repel=T)

j <- 0
for (i in 2004:2015) {
  j <- j + 1
  ## creating the graph for each year with igraph
  p <- partners %>%
    filter(season == paste0(i,"/",i+1)) %>%
    select(-season)
  g <- graph_from_data_frame(p, directed = FALSE)
  w <- data.frame(as_edgelist(g), weight=E(g)$weight)
  V(g)$strength <- strength(g)
  v <- as_ids(V(g))
  # print(v)
  s <- induced_subgraph(graph, v)
  id <- filter(ids, name %in% v) %>% pull(label)
  layout <- global_layout[id,]
  plot[[j]] <- ggraph(s, layout = layout) +
    geom_edge_link(aes(width=weight,alpha=weight),colour="lightgrey") +
    geom_node_point(aes(size=strength),shape=21,colour="#004C99",fill="#A70042",alpha=0.8) +
    geom_node_text(aes(label=name,size=strength),colour="black",repel=T) +
    scale_edge_width(limits=c(min(w$weight),max(w$weight)),range = c(0.01,5)) + # scaling each connection (edge) compraed to the global weights
    scale_alpha(range = c(0.25,0.75)) +
    scale_size(range = c(1,5)) +
    scale_x_continuous(limits=c(-1.1,1.1),breaks=NULL) + # slightly expanding the plot area size to allow space for player names
    scale_y_continuous(limits=c(-1.1,1.1),breaks=NULL) +
    theme(
      plot.subtitle=element_text(colour="lightgrey",size=9),
      plot.caption=element_text(face="italic"),
      legend.position="none",
      plot.margin = unit(c(0.01,0.01,0.01,0.01),"cm")
    ) +
    labs(
      y = "",
      x = ""
    )
}

cowplot::plot_grid(plot[[1]],plot[[2]],plot[[3]],plot[[4]],
                   plot[[5]],plot[[6]],plot[[7]],plot[[8]])
