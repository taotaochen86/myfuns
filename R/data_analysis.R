# ANOVA for split plot experiment ----------------------------------------

#' ANOVA for split plot experiment and it's HSD test#'
#'
#' @param DS data.frame dataset
#' @param block a string block factor Name
#' @param pplot a string main factor factor Name
#' @param splot a string split factor factor Name
#' @examples
#' Packages.InstallandLoad(c('tidyverse','glue'))
#' @export

HSDAnalysis.sp.plot <- function(DS, block, pplot, splot) {
    Packages.InstallandLoad(c("tidyverse", "agricolae"))
    respones <- names(DS) %>% setdiff(c(block, pplot, splot))

    ANOVA <- map(respones, ~sp.plot(block = DS[[block]], pplot = DS[[pplot]], splot = DS[[splot]],
        Y = DS[[.x]]))

    out <- map2_df(.x = respones, .y = ANOVA, ~list(respones = .x, data = list(DS %>% dplyr::select(one_of(c(block,
        pplot, splot, .x)))), ANOVA = .y$ANOVA %>% as.data.frame() %>% list(), ANOVA_params = list(gl.a = .y$gl.a,
        gl.b = .y$gl.b, Ea = .y$Ea, Eb = .y$Eb) %>% list()))

    out$HSD_pplot <- map2(out$respones, out$ANOVA_params, ~HSD.test(y = DS[[.x]], trt = DS[pplot],
        MSerror = .y[["Ea"]], DFerror = .y[["gl.a"]])) %>% map(~merge(.x$groups, .x$means %>%
        select(one_of("std")), by = "row.names") %>% purrr::set_names(nm = c("trt", "means",
        "M", "SD")))



    out$HSD_splot <- map2(out$respones, out$ANOVA_params, ~HSD.test(y = DS[[.x]], trt = DS[splot],
        MSerror = .y[["Eb"]], DFerror = .y[["gl.b"]])) %>% map(~merge(.x$groups, .x$means %>%
        select(one_of("std")), by = "row.names") %>% purrr::set_names(nm = c("trt", "means",
        "M", "SD")))



    out$HSD_Interaction <- map2(out$respones, out$ANOVA_params, ~HSD.test(y = DS[.x], trt = interaction(DS[[pplot]],
        DS[[splot]]), MSerror = .y[["Eb"]], DFerror = .y[["gl.b"]])) %>% map(~merge(.x$groups,
        .x$means %>% select(one_of("std")), by = "row.names") %>% purrr::set_names(nm = c("trt",
        "means", "M", "SD")))
    out$ANOVA <- out$ANOVA %>% map(function(ds) {
        rownames(ds) <- recode(rownames(ds), `DS[[block]]` = block, `DS[[pplot]]` = pplot, `DS[[splot]]` = splot,
            `DS[[pplot]]:DS[[splot]]` = sprintf("%s*%s", pplot, splot))
        ds
    })
    return(out)
}
