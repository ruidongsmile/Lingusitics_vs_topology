# data_points
my_points <- function(idx, dim=c(1,2)) {
  cat_coord <- gb_mjca$colpcoord[rownames(gb_mjca$colpcoord)[gb_dummy[idx, 2:ncol(gb_dummy)] == 1], ]
  return(cat_coord[,dim])
}

# data_plot
my_plot <- function(idx, dim=c(1,2), text = FALSE, xlim=c(-0.5,0.5), ylim=c(-0.5,0.5), cex=0.8) {
  cat_coord <- my_points(idx=idx, dim=dim)
  main <- gb_data[idx, "glottocode"]
  plot(cat_coord[, dim], xlim = xlim, ylim = ylim, cex=cex, main=main)
  if (text == TRUE){
    text(cat_coord[,1]-0.01, cat_coord[,2]-0.01, labels=rownames(cat_coord), cex=0.5)
  }
}

pd_family_plot <- function(family_name, coord_dim=c(1:2), hom_dim=1){
  lst_family <- which(gb_data$family_name == family_name) |>
    lapply(FUN = function(idx){
      my_points(idx, dim=coord_dim)
    })

  hom_lst_family <- lapply(lst_family, FUN = function(x){
    result <- calculate_homology(x, dim = hom_dim, return_df = F)
    # result[result[, "dimension"] == 1, ]
    result
  })

  main <- gb_data[which(gb_data$family_name == family_name), "glottocode"]

  pd_family <- hom_lst_family |>
    lapply(function(pd){
      plot_persist(pd) +
        xlim(0, 0.2) +
        ylim(0, 0.2)
    })


}

my_ggplot_mjca_adjust <- function(idx, size=1.8, dim=c(1, 2), maxlim=0.3, minlim=-0.45,
                                  text = F, title_hjust = 0.5, title_vjust = 0, title_size=15) {
  pts <- as.data.frame(my_points(idx, dim=dim))
  colnames(pts) <- c("X", "Y")
  pts$labels <- row.names(pts)

  x_dim <- paste0("Dimension ", dim[1], " ")
  y_dim <- paste0("Dimension ", dim[2], " ")

  x_lab <-paste0(x_dim, "(", round(gb_mjca$inertia.e[1] * 100, 1), "%)")
  y_lab <-paste0(y_dim, "(", round(gb_mjca$inertia.e[2] * 100, 1), "%)")

  # c("Dim1", "Dim2")
  p <- ggplot(pts, aes(X, Y, label = labels, color = "red")) +
    theme_bw() +
    geom_point(size=size) +
    labs(
      x = x_lab,
      y = y_lab
    ) +
    (if (text == T){
      geom_text(hjust=.45, vjust=-0.7, color = "black",
                size=3)
    }) +
    ggtitle(gb_data$glottocode[idx]) +
    theme(plot.title = element_text(color = "black", hjust = title_hjust, vjust = title_vjust, size=title_size)) +
    xlim(minlim, maxlim) +
    ylim(minlim, maxlim) +
    theme(aspect.ratio=1,
          legend.position = "none")
  return(p)
}

wasserstein_dist <- function(num_vec, coord_dim = 1:2, p=1, dimension=1){
  vec_length <- length(num_vec)
  result_dist <- matrix(nrow = vec_length,
                        ncol = vec_length)
  colnames(result_dist) <- gb_data$glottocode[num_vec]
  rownames(result_dist) <- gb_data$glottocode[num_vec]

  for (i in 2:vec_length){
    for (j in 1:(i - 1)){
      # Diag1 <- ripsDiag(my_points(num_vec[i], dim = coord_dim), maxdimension = 1, maxscale = maxscale)
      # Diag2 <- ripsDiag(my_points(num_vec[j], dim = coord_dim), maxdimension = 1, maxscale = maxscale)

      Diag1 <- calculate_homology(my_points(num_vec[i], dim = coord_dim), dim = dimension)
      Diag2 <- calculate_homology(my_points(num_vec[j], dim = coord_dim), dim = dimension)

      result_dist[i, j] <-
        TDApplied::diagram_distance(Diag1, Diag2,
                                    dim = dimension, p = p,
                                    distance = "wasserstein")

        # TDA::wasserstein(Diag1$diagram, Diag2$diagram, p = p, dimension = dimension)
    }
  }
  result_dist <- as.dist(result_dist)
  result_dist
}

# wasserstein_dist <- function(num_vec, coord_dim = 1:2, p=1, dimension=1, maxscale=1){
#   vec_length <- length(num_vec)
#   result_dist <- matrix(nrow = vec_length,
#                         ncol = vec_length)
#   colnames(result_dist) <- gb_data$glottocode[num_vec]
#   rownames(result_dist) <- gb_data$glottocode[num_vec]
#
#   for (i in 2:vec_length){
#     for (j in 1:(i - 1)){
#       Diag1 <- ripsDiag(my_points(num_vec[i], dim = coord_dim), maxdimension = dimension, maxscale = maxscale)
#       Diag2 <- ripsDiag(my_points(num_vec[j], dim = coord_dim), maxdimension = dimension, maxscale = maxscale)
#
#       # Diag1 <- calculate_homology(my_points(num_vec[i], dim = coord_dim), dim = dimension)
#       # Diag2 <- calculate_homology(my_points(num_vec[j], dim = coord_dim), dim = dimension)
#
#       result_dist[i, j] <-
#         # TDApplied::diagram_distance(Diag1, Diag2,
#         #                             dim = dimension, p = p,
#         #                             distance = "wasserstein")
#
#         TDA::wasserstein(Diag1$diagram, Diag2$diagram, p = p, dimension = dimension)
#     }
#   }
#   result_dist <- as.dist(result_dist)
#   result_dist
# }

wass_mds_plot <- function(num_vec, coord_dim = 1:2, p=1, dimension=1){
  wass_dist <- wasserstein_dist(num_vec = num_vec, coord_dim = coord_dim, p=p, dimension = dimension)
  wass_mds <- cmdscale(wass_dist)
  wass_mds_df <- as.data.frame(wass_mds)

  wass_mds_df[, "glottocode"] <- gb_data$glottocode[num_vec]
  # mds_df[, "Language_Name"] <- gb_data$[olga_idx]
  wass_mds_df[, "Family_Name"] <- gb_data$family_name[num_vec]

  ggscatter(wass_mds_df, x = "V1", y = "V2",
            label = "glottocode",
            color = "Family_Name",
            size = 1.5,
            repel = TRUE) +
    ggtitle("") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0))

}

# pss dist

pss_dist <- function(pd1, pd2, dim=1, sigma=0.1){
  result <- sqrt(heat_kernel(pd1, pd1, dim = dim, sigma = sigma) +
                   heat_kernel(pd2, pd2, dim = dim, sigma = sigma) -
                   2 * heat_kernel(pd1, pd2, dim = dim, sigma = sigma))

  return(result)
}

# Detect homological generators
Simplex_vertices <- function(DiagRips_list, coords_list, num, max_dim = 1, t=0.1, s=0.15) {
  lang_coords <- coords_list[[num]]
  indices <- which(DiagRips_list[[num]]$diagram[, "dimension"] == max_dim & DiagRips_list[[num]]$diagram[, "Death"] >= t & DiagRips_list[[num]]$diagram[, "Birth"] <= s)

  simplex_features <- indices |>
    lapply(
      FUN = function(ind){
        matrix(nrow = dim(DiagRips_list[[num]][["cycleLocation"]][[ind]])[1], ncol = max_dim + 1)
      }
    )

  for (i in seq(length(indices))) {
    for (j in seq_len(dim(DiagRips_list[[num]][["cycleLocation"]][[indices[i]]])[1])) {
      simplex <- DiagRips_list[[num]][["cycleLocation"]][[indices[i]]][j, , ]
      simp_vertex <- match(apply(round(simplex, 3), 1, paste, collapse = ","),
                           apply(round(lang_coords, 3), 1, paste, collapse = ","))
      simp_vertex_feature <- rownames(lang_coords)[simp_vertex]
      simplex_features[[i]][j, ] <- simp_vertex_feature
    }
  }


  return(simplex_features)
}

Simplex_vertices_2 <- function(DiagRips_list, coords_list, num, max_dim = 1, pst_time = 0.03) {
  lang_coords <- coords_list[[num]]
  indices <- which(DiagRips_list[[num]]$diagram[, "dimension"] == max_dim & (DiagRips_list[[num]]$diagram[, "Death"] -
                                                                               DiagRips_list[[num]]$diagram[, "Birth"] >= pst_time))

  simplex_features <- indices |>
    lapply(
      FUN = function(ind){
        matrix(nrow = dim(DiagRips_list[[num]][["cycleLocation"]][[ind]])[1], ncol = max_dim + 1)
      }
    )

  for (i in seq(length(indices))) {
    for (j in seq_len(dim(DiagRips_list[[num]][["cycleLocation"]][[indices[i]]])[1])) {
      simplex <- DiagRips_list[[num]][["cycleLocation"]][[indices[i]]][j, , ]
      simp_vertex <- match(apply(round(simplex, 5), 1, paste, collapse = ","),
                           apply(round(lang_coords, 5), 1, paste, collapse = ","))
      simp_vertex_feature <- rownames(lang_coords)[simp_vertex]
      simplex_features[[i]][j, ] <- simp_vertex_feature
    }
  }


  return(simplex_features)
}

my_persist_plot <- function(idx, coord_dim=c(1:2), hom_dim=1, title_hjust = 0.5, title_vjust = 0, title_size = 14){
  x <- my_points(idx, dim=coord_dim) |>
    calculate_homology(dim = hom_dim)

  x <- x[x[, "dimension"] == hom_dim, ]

  title_text <- paste0(gb_data$glottocode[idx], " (", gb_data$family_name[idx], ")")

  p <- ggplot2::ggplot(data = data.frame(x)) +
    ggplot2::xlim(0, .21) +
    ggplot2::ylim(0, .21) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::xlab("Birth") +
    ggplot2::ylab("Death") +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank()) +
    ggplot2::geom_point(ggplot2::aes_string(x = "birth",
                                            y = "death"), shape = 17, colour = "#00BFC4") +
    ggplot2::coord_fixed(ratio = 1) +
    ggtitle(title_text) +
    theme(plot.title = element_text(color = "black", hjust = title_hjust, vjust = title_vjust, size=title_size))

  p
}


my_loss <- function (pds1, pds2, dim = 1, power_p = 2, power_q=1) {
  n1 <- length(pds1)
  n2 <- length(pds2)

  loss.1 <- 1:(n1-1) |>
    sapply(FUN = function(i) {
      sapply((i+1):n1, FUN = function(j){
        (TDApplied::diagram_distance(pds1[[i]], pds1[[j]], dim = dim, p = power_p, distance = "wasserstein"))^power_q
      }) |>
        sum()
    }) |>
    sum()

  loss.1 <- loss.1 / (n1*(n1-1))

  loss.2 <- 1:(n2-1) |>
    sapply(FUN = function(i) {
      sapply((i+1):n2, FUN = function(j){
        (TDApplied::diagram_distance(pds2[[i]], pds2[[j]], dim = dim, p = power_p, distance = "wasserstein"))^power_q
      }) |>
        sum()
    }) |>
    sum()
  loss.2 <- loss.2 / (n2*(n2-1))

  return(loss.1 + loss.2)
}


my_perm_test <- function(pds1, pds2, ...) {
  n1 <- length(pds1)
  n2 <- length(pds2)
  z <- 0

  all_comb <- combn(1:(n1+n2), n1)




  all.pds <- append(pds1, pds2)

  obs.loss <- my_loss(pds1=pds1, pds2=pds2, ...)

  # Initializes the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = ncol(all_comb), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar

  for (i in 1:ncol(all_comb)) {
    # curr.order <- c(all_comb[, i], setdiff(1:(n1+n2), all_comb[, i]))
    curr.pds1 <- all.pds[all_comb[, i]]
    curr.pds2 <- all.pds[setdiff(1:(n1+n2), all_comb[, i])]

    curr.loss <- my_loss(curr.pds1, curr.pds2, ...)

    if ( curr.loss<= obs.loss) {
      z <- z + 1
    }

    Sys.sleep(0.5) # simulate some computation time
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  close(pb)
  # result <- (z + 1) / (iter + 1)
  return(z  / ncol(all_comb))
}








