###################################################### Export results to latex
DR_res_BS = precis(res_bs, pars="D_corr", depth=3)
DR_res_SC = precis(res_sc, pars="D_corr", depth=3)
DR_res_TB = precis(res_tb, pars="D_corr", depth=3)
DR_res_SU = precis(res_su, pars="D_corr", depth=3)

GR_res_BS = precis(res_bs, pars="G_corr", depth=3)
GR_res_SC = precis(res_sc, pars="G_corr", depth=3)
GR_res_TB = precis(res_tb, pars="G_corr", depth=3)
GR_res_SU = precis(res_su, pars="G_corr", depth=3)


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
  if(site=="l") 
  form_res = paste0("$\\rho_l =$ ", mean_res, ", CI: ", L_res, ", ", H_res)
  if(site=="h") 
  form_res = paste0("$\\rho_h =$ ", mean_res, ", CI: ", L_res, ", ", H_res)
  if(site=="a") 
  form_res = paste0("$\\rho_a =$ ", mean_res, ", CI: ", L_res, ", ", H_res)

  return(form_res)
}

make_result_point = function(bob, line, site){
  mean_res = sprintf("%.2f", bob[line, 1]) 
  L_res = sprintf("%.2f", bob[line, 3]) 
  H_res = sprintf("%.2f", bob[line, 4]) 
   
  if(site=="c") 
  form_res = paste0("$\\rho_c =$ ", mean_res)
  if(site=="l") 
  form_res = paste0("$\\rho_l =$ ", mean_res)
  if(site=="h") 
  form_res = paste0("$\\rho_h =$ ", mean_res)
  if(site=="a") 
  form_res = paste0("$\\rho_a =$ ", mean_res)

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

calcs2 = prep_latex_variables(list(
  fGEsu = make_result_point(DR_res_SU, 2, "a"),
  fQGsu = make_result_point(GR_res_SU, 60, "a"),
  #
  dGGsu = make_result(DR_res_SU, 6, "a"), 
  dEEsu = make_result(DR_res_SU, 17, "a"),
  dPPsu = make_result(DR_res_SU, 28, "a"),
  dQQsu = make_result(DR_res_SU, 39, "a"),
  dSSsu = make_result(DR_res_SU, 50, "a"),
  #
  dGGtb = make_result(DR_res_TB, 6, "h"),
  dEEtb = make_result(DR_res_TB, 17, "h"),
  dPPtb = make_result(DR_res_TB, 28, "h"),
  dQQtb = make_result(DR_res_TB, 39, "h"), 
  dSStb = make_result(DR_res_TB, 50, "h"),
  #
  dGSsu = make_result(DR_res_SU, 4, "a"), 
  dGQsu = make_result(DR_res_SU, 5, "a"),
  dGStb = make_result(DR_res_TB, 4, "h"), 
  dGQtb = make_result(DR_res_TB, 5, "h"),
  #
  dESsu = make_result(DR_res_SU, 14, "a"), 
  dEQsu = make_result(DR_res_SU, 15, "a"),
  dEStb = make_result(DR_res_TB, 14, "h"), 
  dEQtb = make_result(DR_res_TB, 15, "h"),
  #
  dPSsu = make_result(DR_res_SU, 24, "a"), 
  dPQsu = make_result(DR_res_SU, 25, "a"),
  dPStb = make_result(DR_res_TB, 24, "h"), 
  dPQtb = make_result(DR_res_TB, 25, "h"),
  #
  dQzGsu = make_result(DR_res_SU, 10, "a"),
  dQzEsu = make_result(DR_res_SU, 20, "a"),
  dQzPsu = make_result(DR_res_SU, 30, "a"),
  dQzGtb = make_result(DR_res_TB, 10, "h"),
  dQzEtb = make_result(DR_res_TB, 20, "h"),
  dQzPtb = make_result(DR_res_TB, 30, "h"),
  #
  dSzGsu = make_result(DR_res_SU, 9, "a"),
  dSzEsu = make_result(DR_res_SU, 18, "a"),
  dSzPsu = make_result(DR_res_SU, 27, "a"),
  dSzGtb = make_result(DR_res_TB, 9, "h"),
  dSzEtb = make_result(DR_res_TB, 18, "h"),
  dSzPtb = make_result(DR_res_TB, 27, "h"),
  #
  gQGsu = make_result(GR_res_SU, 60, "a"),
  gQGtb = make_result(GR_res_TB, 60, "h"),
  gQEsu = make_result(GR_res_SU, 70, "a"),
  gQEtb = make_result(GR_res_TB, 70, "h"),
  gQPsu = make_result(GR_res_SU, 80, "a"),
  gQPtb = make_result(GR_res_TB, 80, "h"),
  #
  gSEsu = make_result(GR_res_SU, 69, "a"),
  gSEtb = make_result(GR_res_TB, 69, "h"),
  gSPsu = make_result(GR_res_SU, 79, "a"),
  gSPtb = make_result(GR_res_TB, 79, "h")
  ))

writeLines(calcs2, "calcs2.tex")
