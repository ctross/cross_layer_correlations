###################################################### Export results to latex
DR_res_BS = precis(res_bs, pars="D_corr", depth=3)
DR_res_SC = precis(res_sc, pars="D_corr", depth=3)

GR_res_BS = precis(res_bs, pars="G_corr", depth=3)
GR_res_SC = precis(res_sc, pars="G_corr", depth=3)

prep_latex_variables = function(named_list) {
  out = character()
  for (i in 1:length(named_list)) {
    out[i] = paste0("\\newcommand{\\", names(named_list)[i], "}{", named_list[[i]], "}")
  }
  return(out)
}

make_result = function(bob, line, site){
  mean_res = sprintf("%.2f", bob[line, 1]) 
  L_res = sprintf("%.2f", bob[line, 3]) 
  H_res = sprintf("%.2f", bob[line, 4]) 
   
  if(site=="c") 
  form_res = paste0("$\\rho_c =$ ", mean_res, ", CI: ", L_res, ", ", H_res)
  else
  form_res = paste0("$\\rho_l =$ ", mean_res, ", CI: ", L_res, ", ", H_res)

  return(form_res)
}

make_result_point = function(bob, line, site){
  mean_res = sprintf("%.2f", bob[line, 1]) 
  L_res = sprintf("%.2f", bob[line, 3]) 
  H_res = sprintf("%.2f", bob[line, 4]) 
   
  if(site=="c") 
  form_res = paste0("$\\rho_c =$ ", mean_res)
  else
  form_res = paste0("$\\rho_l =$ ", mean_res)

  return(form_res)
}

calcs = prep_latex_variables(list(
  fGEsc = make_result_point(DR_res_SC, 2, "l"),
  fQGsc = make_result_point(GR_res_SC, 60, "l"),
  #
  dGGsc = make_result(DR_res_SC, 6, "l"), 
  dEEsc = make_result(DR_res_SC, 17, "l"),
  dPPsc = make_result(DR_res_SC, 28, "l"),
  dQQsc = make_result(DR_res_SC, 39, "l"),
  dSSsc = make_result(DR_res_SC, 50, "l"),
  #
  dGGbs = make_result(DR_res_BS, 6, "c"),
  dEEbs = make_result(DR_res_BS, 17, "c"),
  dPPbs = make_result(DR_res_BS, 28, "c"),
  dQQbs = make_result(DR_res_BS, 39, "c"), 
  dSSbs = make_result(DR_res_BS, 50, "c"),
  #
  dGSsc = make_result(DR_res_SC, 4, "l"), 
  dGQsc = make_result(DR_res_SC, 5, "l"),
  dGSbs = make_result(DR_res_BS, 4, "c"), 
  dGQbs = make_result(DR_res_BS, 5, "c"),
  #
  dESsc = make_result(DR_res_SC, 14, "l"), 
  dEQsc = make_result(DR_res_SC, 15, "l"),
  dESbs = make_result(DR_res_BS, 14, "c"), 
  dEQbs = make_result(DR_res_BS, 15, "c"),
  #
  dPSsc = make_result(DR_res_SC, 24, "l"), 
  dPQsc = make_result(DR_res_SC, 25, "l"),
  dPSbs = make_result(DR_res_BS, 24, "c"), 
  dPQbs = make_result(DR_res_BS, 25, "c"),
  #
  dQzGsc = make_result(DR_res_SC, 10, "l"),
  dQzEsc = make_result(DR_res_SC, 20, "l"),
  dQzPsc = make_result(DR_res_SC, 30, "l"),
  dQzGbs = make_result(DR_res_BS, 10, "c"),
  dQzEbs = make_result(DR_res_BS, 20, "c"),
  dQzPbs = make_result(DR_res_BS, 30, "c"),
  #
  dSzGsc = make_result(DR_res_SC, 9, "l"),
  dSzEsc = make_result(DR_res_SC, 18, "l"),
  dSzPsc = make_result(DR_res_SC, 27, "l"),
  dSzGbs = make_result(DR_res_BS, 9, "c"),
  dSzEbs = make_result(DR_res_BS, 18, "c"),
  dSzPbs = make_result(DR_res_BS, 27, "c"),
  #
  gQGsc = make_result(GR_res_SC, 60, "l"),
  gQGbs = make_result(GR_res_BS, 60, "c"),
  gQEsc = make_result(GR_res_SC, 70, "l"),
  gQEbs = make_result(GR_res_BS, 70, "c"),
  gQPsc = make_result(GR_res_SC, 80, "l"),
  gQPbs = make_result(GR_res_BS, 80, "c"),
  #
  gSEsc = make_result(GR_res_SC, 69, "l"),
  gSEbs = make_result(GR_res_BS, 69, "c"),
  gSPsc = make_result(GR_res_SC, 79, "l"),
  gSPbs = make_result(GR_res_BS, 79, "c")
  ))

writeLines(calcs, "calcs.tex")
