#############################################
# MAP CO2 EMISSIONS WITH R
# Milos Popovic 2023/05/13
#############################################

main_dir <- getwd()
dir.create("co2_emissions")
co2_emissions_dir_path <- main_dir |>
    paste0("/", "co2_emissions")
setwd(co2_emissions_dir_path)

# 0. INSTALL & LOAD LIBRARIES
#----------------------------
# libraries we need

libs <- c(
    "tidyverse", "readr", "janitor",
    "classInt", "rayshader", "terra",
    "giscoR", "sf"
)

# install missing libraries
installed_libs <- libs %in% rownames(
    installed.packages()
)

if (any(installed_libs == F)) {
    install.packages(
        libs[!installed_libs]
    )
}

# load libraries

invisible(lapply(libs, library, character.only = T))

# 1. DOWNLOAD CO2 DATA
#---------------------

get_co2_data <- function() {
    url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/CO2_excl_short-cycle_org_C/TOTALS/v7.0_FT2021_CO2_excl_short-cycle_org_C_2021_TOTALS.zip"
    download.file(
        url,
        destfile = basename(url), mode = "wb"
    )
}

get_co2_data()

# 2. UNZIP FILE
#---------------

co2_zip_file <- list.files(
    path = co2_emissions_dir_path,
    pattern = "*.zip",
    full.names = T
)

unzip(co2_zip_file)

# 3. LOAD AND CLEAN THE DATA
#---------------------------

file <- list.files(
    path = co2_emissions_dir_path,
    pattern = "*.txt",
    full.names = T
)

read_df <- function() {
    main_df <- readr::read_delim(
        file,
        delim = ";",
        col_names = T
    ) |>
        janitor::row_to_names(
            row_number = 2
        )

    names(main_df) <- "lat;long;emission"

    df <- main_df |>
        tidyr::separate(
            "lat;long;emission",
            into = c(
                "lat", "long", "emission"
            ),
            sep = ";"
        )

    final_df <- df |>
        dplyr::mutate_if(
            is.character, as.numeric
        )
    return(final_df)
}

final_df <- read_df()

# 4. MAKE HEXAGONS
#-----------------

japan_sf <- giscoR::gisco_get_countries(
    country = "JP",
    resolution = 3
)

japan_transformed <- japan_sf |>
    sf::st_transform(3857)

japan_hex <- sf::st_make_grid(
    japan_transformed,
    cellsize = units::as_units(
        100, "km^2"
    ),
    what = "polygons",
    square = F
) |>
    sf::st_intersection(
        sf::st_buffer(
            japan_transformed, 0
        )
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(
        id = row_number()
    ) |>
    sf::st_make_valid()

sf::st_geometry(japan_hex) <- "geometry"

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

japan_final <- japan_hex |>
    dplyr::filter(
        !grepl(
            "POINT",
            sf::st_geometry_type(geometry)
        )
    ) |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_transform(crsLONGLAT)

plot(sf::st_geometry(japan_hex))

# 5. POINT-WITHIN HEX POLYGON
#----------------------------

co2_sf <- final_df |>
    sf::st_as_sf(
        coords = c(
            "long", "lat"
        )
    ) |>
    sf::st_set_crs(crsLONGLAT)

co2_japan_sf <- sf::st_join(
    co2_sf, japan_final,
    join = sf::st_within
) |>
    drop_na()

# 6. AGGREGATE AND JOIN
#----------------------

get_aggregated_co2 <- function() {
    co2_japan_sum <- co2_japan_sf |>
        dplyr::group_by(id) |>
        dplyr::summarise_at(
            vars(emission),
            list(sum_co2 = sum)
        ) |>
        sf::st_set_geometry(
            NULL
        )

    co2_japan_hex <- dplyr::left_join(
        japan_final, co2_japan_sum,
        by = "id"
    )

    co2_japan_hex$sum_co2 <- round(
        co2_japan_hex$sum_co2, 0
    )

    co2_japan_hex$sum_co2[
        is.na(co2_japan_hex$sum_co2)
    ] <- 0

    co2_japan_hex$sum_co2 <- co2_japan_hex$sum_co2 / 1000

    return(co2_japan_hex)
}

co2_japan_hex <- get_aggregated_co2()
summary(co2_japan_hex$sum_co2)

# 7. BREAKS AND COLORS
#----------------------

breaks <- classInt::classIntervals(
    co2_japan_hex$sum_co2,
    n = 8,
    style = "pretty"
)$brks

cols <- colorRampPalette(
    rev(c(
        "#451a40", "#822b4c", "#b74952",
        "#e17350", "#f4a959", "#eae2b7"
    ))
)

# 8. HEX MAP IN GGPLOT2
#----------------------
install.packages("ggrepel")
library(ggrepel)

p <- ggplot(data = co2_japan_hex) +
    geom_sf(aes(fill = sum_co2), color = NA) +
    ggrepel::geom_text_repel(
        data = japan_cities,
        aes(x = long, y = lat, label = name),
        color = "grey40",
        segment.size = .25,
        force = .75,
        segment.curvature = -.3
    ) +
    scale_fill_gradientn(
        name = "tons (thousands)",
        colors = cols(11),
        breaks = breaks
    ) +
    guides(
        fill = guide_legend(
            direction = "horizontal",
            keyheight = unit(1.5, "mm"),
            keywidth = unit(15, "mm"),
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = 0,
            nrow = 1,
            byrow = T
        )
    ) +
    theme_void() +
    theme(
        plot.title = element_text(
            size = 18, color = "#451a40",
            hjust = .5, vjust = 3
        ),
        legend.position = "top",
        legend.title = element_text(
            size = 10, color = "grey10"
        ),
        legend.text = element_text(
            size = 9, color = "grey10"
        ),
        plot.margin = unit(
            c(t = 1, r = 0, b = 0, l = 0), "lines"
        )
    ) +
    labs(
        title = "CO2 emissions for Japan in 2021"
    )

ggsave(
    filename = "japan-co2-emissions-2021-annotated.png",
    width = 7, height = 7, dpi = 600, bg = "white", p
)

# 9. ANNOTATE OUR HEX MAP
#------------------------

install.packages("maps")
library(maps)
data(world.cities)
head(world.cities)

japan_cities <- world.cities |>
    dplyr::filter(
        country.etc == "Japan"
    ) |>
    dplyr::slice_max(
        pop,
        n = 10
    )

# 10. CO2 EMISSIONS 3D MAP
#-------------------------

make_raster_matrix <- function() {
    co2_rast <- terra::rasterize(
        co2_japan_hex,
        terra::rast(
            co2_japan_hex,
            resolution = .01
        ),
        co2_japan_hex$sum_co2
    ) |> terra::na.omit()

    co2_mat <- rayshader::raster_to_matrix(
        co2_rast
    )

    return(co2_mat)
}

co2_mat <- make_raster_matrix()

# Create the initial 3D object

texture <- colorRampPalette("#b74952")(256)

co2_mat |>
    rayshader::height_shade(texture) |>
    rayshader::plot_3d(
        heightmap = co2_mat,
        solid = F,
        soliddepth = 0,
        zscale = 20,
        shadowdepth = 0,
        shadow_darkness = .99,
        windowsize = c(800, 800),
        phi = 65,
        zoom = .65,
        theta = -30,
        background = "white"
    )

# use render_camera to adjust the view
rayshader::render_camera(
    phi = 85, zoom = .5, theta = 0
)

# annotate
for (i in 1:max(nrow(japan_cities))){
rayshader::render_label(
    co2_mat,
    lat = japan_cities$lat[i],
    long = japan_cities$long[i],
    extent = co2_japan_hex,
    altitude = (150000 - i * 10000),
    zscale = 50,
    text = japan_cities$name[i],
    textcolor = "navyblue",
    linecolor = "navyblue",
    linewidth = 1,
    dashed = F,
    family = "mono"
)
}

# render

rayshader::render_highquality(
    filename = "japan_co2_3d.png",
    preview = T,
    interactive = F,
    light = T,
    lightdirection = c(
        225, 215, 225, 215
    ),
    lightaltitude = c(
        15, 15, 75, 75
    ),
    lightintensity = c(
        750, 1000, 150, 150
    ),
    parallel = T,
    width = 4000,
    height = 4000,
    size = 30
)
