simular_ensemble <- function(modelo, sim_datos, R = 1000){
  # simular
  ensemble_1 <- modelo$sample(
    data = sim_datos,
    iter_sampling = R, iter_warmup = 0, 
    chains = 1,
    refresh = R, seed = 432,
    fixed_param = TRUE
  )
  ensemble_1
}

ajustar_modelo <- function(modelo, datos, beta, iter_sampling = 1000, iter_warmup = 1000){
  
  ajuste <- modelo$sample(data = datos, 
                          seed = 2210,
                          iter_sampling = iter_sampling, 
                          iter_warmup = iter_sampling,
                          refresh = 0, 
                          show_messages = FALSE)
  ajuste
}

ajustar_diagnosticos <- function(rep, 
                                 modelo, 
                                 datos, 
                                 params, 
                                 iter_sampling=1000, 
                                 iter_warmup = 1000){
  
  ajuste <- ajustar_modelo(modelo, datos, iter_sampling = iter_sampling, iter_warmup = iter_warmup)
  suppressMessages(diagnostico <- ajuste$cmdstan_diagnose())
  suppressMessages(resumen <- ajuste$summary())
  
  # diagnosticar parámetros
  sims_tbl <- ajuste$draws(names(params)) %>% as_draws_df() %>% as_tibble()
  sbc_tbl <- sbc_rank(params, sims_tbl)
  tibble(rep = rep, params = list(params), sbc_rank = list(sbc_tbl), 
         resumen = list(resumen), diagnosticos = list(diagnostico))
}

sbc_rank <- function(params_tbl, sims_tbl){
  params_nom <- names(params_tbl)
  sims_tbl_larga <- sims_tbl %>% 
    filter((row_number() %% 10) == 0) %>% # adelgazar la cadena
    pivot_longer(cols = any_of(params_nom), names_to = "parametro", values_to = "valor") 
  params_tbl_larga <- params_tbl %>% 
    pivot_longer(cols = any_of(params_nom), names_to = "parametro", values_to = "valor_real")
  sbc_tbl <- sims_tbl_larga %>% 
    left_join(params_tbl_larga, by = "parametro") %>% 
    group_by(parametro) %>% 
    summarise(sbc_rank = mean(valor_real < valor))
  sbc_tbl %>% pivot_wider( names_from = "parametro", values_from ="sbc_rank")
}


curvas_exito <- function(sims, sim_data){
  exitos <- sims$draws("exitos") %>% as_draws_df %>% 
    mutate(rep = 1:1000) %>% 
    pivot_longer(cols = starts_with("exitos"))
  sim_data_tbl <- tibble(x = sim_data$x, n = sim_data$n, id = 1:length(sim_data$x)) %>% 
    mutate(name = paste0("exitos[", id, "]"))
  exitos_tbl <- exitos %>% left_join(sim_data_tbl) %>% 
    mutate(prop_exitos = value / n)
  exitos_tbl
}


calcular_post_check <- function(ajuste, datos){
  prob_sim_tbl <- ajuste$draws(c("prob_sim")) %>% as_draws_df() %>% 
    mutate(rep = 1:nrow(.)) %>% 
    pivot_longer(cols = starts_with("prob_sim")) %>% 
    separate(name, into=c("a", "dist_num"), sep="\\[") %>% 
    mutate(dist_num = str_sub(dist_num, 1, -2) %>% as.integer) %>% 
    select(rep, dist_num, value)
  pred_check_tbl <- prob_sim_tbl %>% 
    group_by(dist_num) %>% 
    summarise(q_05 = quantile(value, 0.025), q_95 = quantile(value, 0.975))
  
  datos_2 <- datos %>% mutate(dist_num = 1:nrow(.))
  pred_check_tbl <- pred_check_tbl %>% left_join(datos_2, by = "dist_num")
  pred_check_tbl
}