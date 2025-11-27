#Todo lo referido al callejero y distancias esta en este script.

#si el archivo existe lo cargo directamente, sino descargo los datos
if (file.exists("data/callejero.gpkg")) {
  # Load the geopackage
  callejero <- st_read("data/callejero.gpkg")
} else {
  
  clases_principales <- c(
    "motorway","trunk","primary","secondary","tertiary",
    "motorway_link","trunk_link","primary_link","secondary_link","tertiary_link"
  )
  
  #creo y ejecuto la query
  q <- opq(bbox = st_bbox(bbox_poly)) |>
    add_osm_feature(key = "highway")
  
  osm <- osmdata_sf(q)
  calles_raw <- osm$osm_lines
  poligonos_raw <- osm$osm_polygons
  
  poligonos_filtro <- poligonos_raw %>% filter(highway %in% clases_principales)
  
  convertido_lineas <- st_cast(poligonos_filtro, "LINESTRING")
  
  callejero <- rbind(calles_raw,convertido_lineas)
  
  # Conservar sólo columnas útiles para bajar memoria
  callejero <- dplyr::select(callejero, osm_id, name, highway)
  
  #guardado en archivo para no tener que volver a descargar.
  st_write(callejero, "callejero.gpkg", layer = "roads", delete_dsn = TRUE)
}

rm(calles_raw,poligonos_raw,poligonos_filtro,convertido_lineas)

gc()

print("Carga de datos lista, continuando...")

#callejero <- st_simplify(callejero, dTolerance = 2, preserveTopology = TRUE)

if (file.exists("data/net_m.rds")) {
  # Load the geopackage
  print("Usando archivo net_m.rds")
  net_m <- readRDS("data/net_m.rds")
} else {

# ---- 3) RED Y LIMPIEZA -----------------------------------------------------
# Red no dirigida
net0 <- as_sfnetwork(callejero, directed = FALSE)

# Subdividir en intersecciones y simplificar paralelas (en volúmenes chicos rinde bien)
net1 <- net0 |>
  convert(to_spatial_subdivision) |>
  convert(to_simple)

# Remover nodos aislados
net2 <- net1 |>
  activate(nodes) |>
  filter(!node_is_isolated())

# Etiquetar a qué componente pertenece cada nodo
# activate(nodes): le decís a sfnetworks/tidygraph que las operaciones siguientes van
#  sobre la tabla de nodos (no sobre aristas).

# group_components(): calcula las componentes conexas de la red y devuelve un ID por
# nodo (1, 2, 3, …). Dos nodos con el mismo comp están en la misma isla de la red
# (hay algún camino que los conecta).

# mutate(comp = ...): agrega esa columna comp a cada nodo.

# Mantener componente gigante
net2 <- net2 |>
  activate(nodes) |>
  mutate(comp = group_components())

# Medir el tamaño de cada componente (cuántos nodos tiene)

comp_sizes <- net2 |>
  activate(nodes) |>
  as_tibble() |>
  count(comp, name = "n") |>
  arrange(desc(n))

# Guardar el ID del componente gigante
giant_id <- comp_sizes$comp[1]

# Filtrar la red para quedarte solo con el componente gigante
net_giant <- net2 |>
  activate(nodes) |>
  filter(comp == giant_id) |>
  select(-comp)

# Reportar tamaño (nodos y aristas)
# gorder(net_giant): devuelve número de nodos de la red.
# gsize(net_giant): devuelve número de aristas.
# cat(...): lo imprime prolijo.

#cat("Nodos:", gorder(net_giant), " - Aristas:", gsize(net_giant), "\n")

gc()

print("Red y limpieza lista, continuando...")

# ---- 4) REPROYECCIÓN Y LONGITUDES ------------------------------------------
# Métrico (Web Mercator). Si querés mayor precisión local podés usar 5347.
net_m <- st_transform(net_giant, 3857)

# Asegurate de estar en un CRS métrico (si estás en 4326, pasá a 3857 o 5347)
# Si el CRS es geográfico (grados), paso toda la red a métrico
if (sf::st_is_longlat(st_as_sf(net_m, "nodes"))) {
  net_m <- sf::st_transform(net_m, 3857)  # metros
}

net_m <- net_m |>
  activate(edges) |>
  mutate(
    length_m = edge_length(),                 # en m
    weight   = units::drop_units(length_m)    # numérico
  )

summary_lengths <- net_m |>
  activate(edges) |>
  as_tibble() |>
  summarise(n_aristas = n(),
            long_total_km = sum(as.numeric(length_m), na.rm = TRUE)/1000)

print(summary_lengths)

print("continuando...")


# 1) Activá NODOS y convertí a sf
net_m <- net_m |> convert(to_spatial_explicit)

} 
print("net_m listo")


# Ahora sí, activá nodes y convertí
net_m <- activate(net_m, nodes)
edges_sf <- st_as_sf(net_m, "edges")
nodes_sf  <- st_as_sf(net_m, "nodes")

#cargo shp de areas de cordoba y filtro por areas verdes
temp <- tempdir()
unzip("data/shp_cordoba_areas.zip", exdir = temp)
shp_cordoba <- st_read(fs::path(temp, 
                                "/shp_cordoba_areas/shp_cordoba_areas.shp"),
                       options = "ENCODING=UTF-8")

shp_cordoba$categoria <- gsub("�reas verdes", "Áreas verdes", shp_cordoba$categoria)

areas_verdes <- shp_cordoba |>
  filter(categoria == "Áreas verdes")

#descargo areas verdes del osm
parks <- opq(bbox = st_bbox(bbox_poly)) |>
  add_osm_feature(key = "leisure", value = "park") |>
  osmdata_sf()

parques_poligonos <- parks[["osm_polygons"]] 
parques_multipoligonos <- parks[["osm_multipolygons"]] 
parques_puntos <- parks[["osm_points"]] 

#combino los polygon con multipolygon para no perder los parques grandes
parques_osm_total <- st_union(parques_poligonos,st_cast(parques_multipoligonos,"POLYGON"))

#intersecto los parques del osm con las areas verdes para filtrar y dejar mas limpio
areas_verdes_intersect_total <- areas_verdes |> st_intersection(st_transform(parques_osm_total,st_crs(areas_verdes)))


proyect_cordo_sf_planas <- proyect_cordo_sf |> st_transform(3857)
parques_poligonos_planas <- areas_verdes_intersect_total |> st_transform(3857)

origins <- proyect_cordo_sf_planas$geometry
parks <- parques_poligonos_planas$geometry

origins_net <- st_nearest_feature(origins, net_m)
parks_net   <- st_nearest_feature(parks, net_m)


# Make sure houses and parks are sf objects
houses <- st_as_sf(origins)
parks  <- st_as_sf(parks)

# Get network nodes
nodes <- net_m %>% activate("nodes") %>% st_as_sf()

# Snap houses to nearest network node
houses_node_index <- st_nearest_feature(houses, nodes)
houses <- houses %>%
  mutate(node_id = houses_node_index)

# Snap parks to nearest network node (use centroids of polygons)
parks_node_index <- st_nearest_feature(st_centroid(parks), nodes)
parks <- parks %>%
  mutate(node_id = parks_node_index)


# Activate edges with weight = length
network <- net_m %>% activate("edges") %>% mutate(weight = st_length(geometry))

# Remove duplicate park nodes
unique_park_nodes <- unique(parks$node_id)

# Compute distances from each house node to all unique park nodes
dist_matrix <- igraph::distances(
  graph = network %>% activate("nodes") %>% as_tbl_graph(),
  v = houses$node_id,
  to = unique_park_nodes,
  weights = network %>% activate("edges") %>% pull(weight)
)

# Minimum distance for each house
houses$min_dist_to_park <- apply(dist_matrix, 1, min)

dist_matrix <- st_distance(st_transform(proyect_cordo_sf,3857), parques_poligonos_planas)  # returns a matrix

# Get the minimum distance for each point
proyect_cordo_sf$min_dist_to_areas <- apply(dist_matrix, 1, min)
