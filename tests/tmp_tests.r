devtools::load_all()

out <- plot_situations(sim_sole_crop, obs = obs)
out


out <- plot_situations(sim_sole_crop, overlap = list(list("lai_n", "masec_n")), all_situations = FALSE)
out$`SC_Pea_2005-2006_N0`
out

out <- plot_situations(sim, overlap = list(list("lai_n", "masec_n")), all_situations = FALSE)


out <- plot_situations(sim, type = "dynamic")
out


out <- plot_situations(sim, obs = obs, all_situations = TRUE)
out$`IC_Wheat_Pea_2005-2006_N0`
