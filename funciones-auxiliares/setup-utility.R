print_file <- function(file) {
    cat(paste(readLines(file), "\n", sep=""), sep="")
}

comma <- function(x) format(x, digits = 2, big.mark = ",")

color.blues <- c(NA,"#BDD7E7", "#6BAED6", "#3182BD", "#08519C", "#074789", "#063e77", "#053464")
color.itam  <- c("#00362b", "#00503f", "#006953", "#008367", "#009c7b", "#00b68f")


sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), 
                  axis.text = element_blank())
