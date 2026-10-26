options(warn = 1, width = 160)
suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(furrr)
})
Sys.setenv(VECLIB_MAXIMUM_THREADS = "1", OMP_NUM_THREADS = "1")
args <- commandArgs(trailingOnly = TRUE)
asama <- if (length(args)) args[1] else "tam"
sdir <- "R_Data ve Python Data/simulasyon"
fdir <- "R_Data ve Python Data/figur_verileri"
log_dosya <- "Results/log/simulasyon_log.txt"
yaz <- function(...) {
  m <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste(..., collapse = " "))
  cat(m, "\n")
  cat(m, "\n", file = log_dosya, append = TRUE)
}
durum_guncelle <- function(metin) {
  r <- readLines("Rapor.md", encoding = "UTF-8")
  satir <- paste0("Simulasyon ilerlemesi (", format(Sys.time(), "%d.%m.%Y %H:%M"), "): ", metin)
  i <- grep("^Simulasyon ilerlemesi", r)
  if (length(i)) {
    r[i[1]] <- satir
  } else {
    j <- grep("^Tahmini s", r)[1]
    r <- append(r, satir, after = j)
  }
  writeLines(r, "Rapor.md", useBytes = TRUE)
}
G <- readRDS(file.path(sdir, "sim_girdi.rds"))
list2env(G$fonk, environment())
plan(multisession, workers = 16)
t_bas <- Sys.time()
yaz("simulasyon.R basladi, asama:", asama)

kirp <- function(p) pmin(pmax(p, 0.005), 0.95)
h <- G$hane
n_h <- nrow(h)
w_h <- h$HB040
f_h <- list(ana = paste("Y_saglik_yuk ~ A * C_hane +", G$h1_kov), esnek = paste("Y_saglik_yuk ~ A * C_hane + A * (", G$h1_kov, ")"))
M_h <- lapply(f_h, function(f) mm_kur(as.formula(f), h, sen_AC("C_hane")))
B0_h <- lapply(M_h, function(m) gc_tahmin(m, w_h, olcu_AC, katsayi = "A:C_hane")$b)
m_t <- mm_kur(as.formula(paste("Y_saglik_yuk ~ A + C_hane +", G$h1_kov)), h, list(s00 = list(A = 0, C_hane = 0)))
b_t <- irls(m_t$X, m_t$y, w_h, setNames(numeric(ncol(m_t$X)), colnames(m_t$X)))
taban_h <- senaryo_mu(m_t, b_t)$mus$s00
bA_h <- (G$h1_hucre[["p10"]] - G$h1_hucre[["p00"]]) / 100
bC_h <- (G$h1_hucre[["p01"]] - G$h1_hucre[["p00"]]) / 100
gercek_D <- function(taban, bA, bC, D, w) 100 * sum(w * (kirp(taban + bA + bC + D) - kirp(taban + bA) - kirp(taban + bC) + kirp(taban))) / sum(w)
yaz("Hane DGP: agirlikli taban ortalamasi", round(100 * sum(w_h * taban_h) / sum(w_h), 3), "beta_A", round(100 * bA_h, 3), "beta_C", round(100 * bC_h, 3))

lpm_D <- function(X, y, w, k, kume) {
  XtWX <- crossprod(X, X * w)
  b <- solve(XtWX, crossprod(X, w * y))
  r <- drop(y - X %*% b)
  Bi <- solve(XtWX)
  V <- Bi %*% var_toplam(X * (w * r), kume) %*% Bi
  c(lpm_D = 100 * b[k, 1], lpm_seD = 100 * sqrt(V[k, k]))
}

hane_tekrar <- function(seed, p_obs) {
  set.seed(seed)
  y <- rbinom(n_h, 1, p_obs)
  out <- c(seed = seed, prev = 100 * sum(w_h * y) / sum(w_h))
  for (e in names(M_h)) {
    m <- M_h[[e]]
    m$y <- y
    r <- gc_tahmin(m, w_h, olcu_AC, katsayi = "A:C_hane", b0 = B0_h[[e]])
    s <- gc_if(m, w_h, r, olcu_AC, kume = seq_len(n_h), katsayi = "A:C_hane")
    out <- c(out, setNames(c(r$olc[["Delta"]], s$se[["Delta"]], r$b[["A:C_hane"]], sqrt(s$Vcoef["A:C_hane", "A:C_hane"]), r$olc[["RERI"]], s$se[["RERI"]], r$iter),
                           paste0(e, c("_D", "_seD", "_bAC", "_seAC", "_RERI", "_seRERI", "_iter"))))
  }
  c(out, lpm_D(M_h$ana$X, y, w_h, match("A:C_hane", colnames(M_h$ana$X)), seq_len(n_h)))
}

hane_boot_tekrar <- function(seed, p_obs, B = 200) {
  set.seed(seed)
  y <- rbinom(n_h, 1, p_obs)
  m <- M_h$ana
  m$y <- y
  r <- gc_tahmin(m, w_h, olcu_AC, b0 = B0_h$ana)
  s <- gc_if(m, w_h, r, olcu_AC, kume = seq_len(n_h))
  Db <- numeric(B)
  for (b in seq_len(B)) {
    mult <- tabulate(sample.int(n_h, n_h - 1L, replace = TRUE), n_h)
    wb <- w_h * mult * n_h / (n_h - 1)
    bb <- irls(m$X, m$y, wb, r$b, tol = 1e-8)
    sm <- senaryo_mu(m, bb)
    Db[b] <- olcu_AC(p_hesap(sm$mus, wb))[["Delta"]]
  }
  c(seed = seed, D = r$olc[["Delta"]], seD = s$se[["Delta"]], boot_se = sd(Db), boot_alt = unname(quantile(Db, 0.025)), boot_ust = unname(quantile(Db, 0.975)))
}

yy <- G$yetiskin
n_y <- nrow(yy)
w_y <- yy$FB030
kume_y <- yy$hane_kimlik
hh_y <- match(kume_y, unique(kume_y))
n_kume <- max(hh_y)
f_y <- if (G$h1y_ana == "nihai_esnek") paste("Y_saglik_yuk ~ A * C_fert + A * (", G$h1y_kov, ")") else paste("Y_saglik_yuk ~ A * C_fert +", G$h1y_kov)
M_y <- mm_kur(as.formula(f_y), yy, sen_AC("C_fert"))
B0_y <- gc_tahmin(M_y, w_y, olcu_AC, katsayi = "A:C_fert")$b
m_ty <- mm_kur(as.formula(paste("Y_saglik_yuk ~ A + C_fert +", G$h1y_kov)), yy, list(s00 = list(A = 0, C_fert = 0)))
b_ty <- irls(m_ty$X, m_ty$y, w_y, setNames(numeric(ncol(m_ty$X)), colnames(m_ty$X)))
taban_y <- senaryo_mu(m_ty, b_ty)$mus$s00
bA_y <- (G$h1y_hucre[["p10"]] - G$h1y_hucre[["p00"]]) / 100
bC_y <- (G$h1y_hucre[["p01"]] - G$h1y_hucre[["p00"]]) / 100
yaz("Yetiskin DGP: agirlikli taban ortalamasi", round(100 * sum(w_y * taban_y) / sum(w_y), 3), "beta_A", round(100 * bA_y, 3), "beta_C", round(100 * bC_y, 3), "kume", n_kume)

yetiskin_tekrar <- function(seed, p, rho) {
  set.seed(seed)
  u <- rnorm(n_kume)[hh_y]
  z <- sqrt(rho) * u + sqrt(1 - rho) * rnorm(n_y)
  y <- as.integer(pnorm(z) < p)
  m <- M_y
  m$y <- y
  r <- gc_tahmin(m, w_y, olcu_AC, katsayi = "A:C_fert", b0 = B0_y)
  s <- gc_if(m, w_y, r, olcu_AC, kume = kume_y, katsayi = "A:C_fert")
  c(seed = seed, D = r$olc[["Delta"]], seD_kume = s$se[["Delta"]], seD_kumesiz = s$se_kumesiz[["Delta"]], bAC = r$b[["A:C_fert"]],
    seAC_kume = sqrt(s$Vcoef["A:C_fert", "A:C_fert"]), prev = 100 * sum(w_y * y) / sum(w_y))
}

sen_h <- CJ(Delta = c(0, 1, 2, 3, 4), yanlilik = c("yok", "toplamsal", "carpimsal"))
sen_h[, `:=`(duzey = "hane", R = fifelse(Delta == 0, 2000L, 1000L), delta_fp = fifelse(yanlilik == "toplamsal", 0.02, 0), gamma = fifelse(yanlilik == "carpimsal", 0.3, 0))]
sen_y <- CJ(Delta = c(0, 2), rho = c(0.3, 0.6))
sen_y[, `:=`(duzey = "yetiskin", yanlilik = "yok", R = fifelse(Delta == 0, 2000L, 1000L))]
sen_b <- data.table(Delta = c(0, 2), duzey = "hane_bootstrap", yanlilik = "yok", R = 200L)
senaryolar <- rbind(sen_h, sen_y, sen_b, fill = TRUE)
senaryolar[, sid := sprintf("S%02d", .I)]
senaryolar[, tohum := 20251101L + .I * 1000L]
if (asama == "pilot") senaryolar[, R := 50L]
if (asama == "pilot") senaryolar[duzey == "hane_bootstrap", R := 16L]
fwrite(senaryolar, file.path(sdir, if (asama == "pilot") "senaryo_tablosu_pilot.csv" else "senaryo_tablosu.csv"))

sonuc_ozet <- list()
for (i in seq_len(nrow(senaryolar))) {
  sc <- senaryolar[i]
  f_out <- file.path(sdir, paste0(if (asama == "pilot") "pilot_" else "ham_", sc$sid, ".rds"))
  t0 <- Sys.time()
  set.seed(sc$tohum)
  tohumlar <- sample.int(.Machine$integer.max, sc$R)
  if (file.exists(f_out)) {
    ham <- readRDS(f_out)
  } else {
    if (sc$duzey %in% c("hane", "hane_bootstrap")) {
      p_true <- kirp(taban_h + bA_h * h$A + bC_h * h$C_hane + sc$Delta / 100 * h$A * h$C_hane)
      p_obs <- if (sc$yanlilik == "toplamsal") p_true + h$A * sc$delta_fp * (1 - p_true) else if (sc$yanlilik == "carpimsal") pmin(1, p_true * (1 + sc$gamma * h$A)) else p_true
      fn <- if (sc$duzey == "hane") hane_tekrar else hane_boot_tekrar
      ham <- future_map(tohumlar, function(s) fn(s, p_obs), .options = furrr_options(seed = TRUE, chunk_size = max(1L, ceiling(sc$R / 64))))
    } else {
      p_true <- kirp(taban_y + bA_y * yy$A + bC_y * yy$C_fert + sc$Delta / 100 * yy$A * yy$C_fert)
      ham <- future_map(tohumlar, function(s) yetiskin_tekrar(s, p_true, sc$rho), .options = furrr_options(seed = TRUE, chunk_size = max(1L, ceiling(sc$R / 64))))
    }
    ham <- as.data.table(do.call(rbind, ham))
    attr(ham, "sure_sn") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    saveRDS(ham, f_out)
  }
  sure <- attr(ham, "sure_sn")
  if (sc$duzey == "yetiskin") {
    gD <- gercek_D(taban_y, bA_y, bC_y, sc$Delta / 100, w_y)
  } else {
    gD <- gercek_D(taban_h, bA_h, bC_h, sc$Delta / 100, w_h)
  }
  mc <- function(x) sqrt(x * (1 - x) / nrow(ham))
  ozet_satir <- function(est, D, se, z_ac = NULL) {
    cov <- mean(abs(D - gD) <= 1.96 * se)
    rej <- mean(abs(D / se) > qnorm(0.975))
    rej1 <- mean(D / se > qnorm(0.95))
    data.table(tahminci = est, R = nrow(ham), gercek_Delta = gD, ort_Delta = mean(D), yanlilik = mean(D) - gD, ampirik_se = sd(D), ort_model_se = mean(se),
               se_orani = mean(se) / sd(D), kapsama = cov, kapsama_mcse = mc(cov), red_iki_yonlu = rej, red_mcse = mc(rej), red_tek_yonlu = rej1,
               red_carpim = if (is.null(z_ac)) NA_real_ else mean(abs(z_ac) > qnorm(0.975)), rmse = sqrt(mean((D - gD)^2)))
  }
  oz <- if (sc$duzey == "hane") {
    rbind(ozet_satir("lojistik_ana_etki", ham$ana_D, ham$ana_seD, ham$ana_bAC / ham$ana_seAC),
          ozet_satir("lojistik_esnek", ham$esnek_D, ham$esnek_seD, ham$esnek_bAC / ham$esnek_seAC),
          ozet_satir("dogrusal_olasilik", ham$lpm_D, ham$lpm_seD, ham$lpm_D / ham$lpm_seD))
  } else if (sc$duzey == "yetiskin") {
    rbind(ozet_satir("kumeli_se", ham$D, ham$seD_kume, ham$bAC / ham$seAC_kume), ozet_satir("kumesiz_se", ham$D, ham$seD_kumesiz))
  } else {
    o1 <- ozet_satir("ana_delta_yontemi", ham$D, ham$seD)
    o2 <- copy(o1)
    o2[, `:=`(tahminci = "ana_bootstrap_yuzdelik", ort_model_se = mean(ham$boot_se), se_orani = mean(ham$boot_se) / sd(ham$D),
              kapsama = mean(ham$boot_alt <= gD & ham$boot_ust >= gD), red_iki_yonlu = mean(ham$boot_alt > 0 | ham$boot_ust < 0), red_tek_yonlu = NA_real_)]
    o2[, `:=`(kapsama_mcse = mc(kapsama), red_mcse = mc(red_iki_yonlu))]
    rbind(o1, o2)
  }
  oz <- cbind(sc[, .(sid, duzey, Delta_nominal = Delta, yanlilik_turu = yanlilik, rho, tohum)], oz)
  oz[, sure_sn := sure]
  sonuc_ozet[[sc$sid]] <- oz
  yaz(sc$sid, sc$duzey, "Delta", sc$Delta, sc$yanlilik, if (!is.na(sc$rho)) paste("rho", sc$rho) else "", "R", sc$R, "sure", round(sure, 1), "sn")
  if (asama != "pilot") durum_guncelle(paste0(i, "/", nrow(senaryolar), " senaryo tamamlandi (son: ", sc$sid, ", ", sc$duzey, ", Delta = ", sc$Delta, ", ", sc$yanlilik, if (!is.na(sc$rho)) paste0(", rho = ", sc$rho) else "", ")"))
}
ozet <- rbindlist(sonuc_ozet, fill = TRUE)
fwrite(ozet, file.path(sdir, if (asama == "pilot") "simulasyon_ozet_pilot.csv" else "simulasyon_ozet.csv"))
if (asama != "pilot") {
  fwrite(ozet[duzey == "hane" & yanlilik_turu == "yok", .(gercek_Delta, Delta_nominal, tahminci, red_iki_yonlu, red_mcse, red_tek_yonlu, red_carpim)], file.path(fdir, "fig5a_guc.csv"))
  fwrite(ozet[duzey == "hane", .(Delta_nominal, gercek_Delta, yanlilik_turu, tahminci, yanlilik, ampirik_se, ort_Delta)], file.path(fdir, "fig5b_yanit_yanliligi.csv"))
  fwrite(ozet[duzey %in% c("hane", "hane_bootstrap"), .(Delta_nominal, gercek_Delta, yanlilik_turu, tahminci, kapsama, kapsama_mcse)], file.path(fdir, "fig5c_kapsama.csv"))
  fwrite(ozet[duzey == "yetiskin", .(Delta_nominal, rho, tahminci, ampirik_se, ort_model_se, se_orani, kapsama, kapsama_mcse)], file.path(fdir, "fig5d_kumeleme.csv"))
  durum_guncelle(paste0("tamamlandi (", nrow(senaryolar), " senaryo, toplam ", round(as.numeric(difftime(Sys.time(), t_bas, units = "mins")), 1), " dk)"))
}
print(ozet)
yaz("simulasyon.R bitti, toplam sure", round(as.numeric(difftime(Sys.time(), t_bas, units = "mins")), 2), "dk")
