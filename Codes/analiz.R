options(warn = 1, width = 160, survey.lonely.psu = "adjust", datatable.print.nrows = 200)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(readxl)
  library(survey)
  library(splines)
  library(flextable)
  library(officer)
  library(future)
  library(furrr)
  library(jsonlite)
  library(car)
  library(logistf)
  library(poLCA)
  library(openxlsx)
})
set.seed(20251025)
baslangic <- Sys.time()
py <- Sys.getenv("PYTHON", "python3")
dir.create("Results/log", showWarnings = FALSE, recursive = TRUE)
sink("Results/log/analiz_log.txt", split = TRUE)
cat("analiz.R basladi:", format(baslangic), "\n")

ddir <- "R_Data ve Python Data"
pq <- file.path(ddir, "gyka_2023_2025.parquet")
if (!file.exists(pq)) {
  t0 <- Sys.time()
  s <- read_excel("GYKA_2023_2025_birlesik.xlsx", sheet = "sozluk")
  d <- as.data.table(read_excel("GYKA_2023_2025_birlesik.xlsx", sheet = "veri", col_types = s$okuma_turu))
  kodlar <- read_excel("GYKA_2023_2025_birlesik.xlsx", sheet = "kodlar")
  dogr_sayfa <- read_excel("GYKA_2023_2025_birlesik.xlsx", sheet = "dogrulama")
  cat("xlsx okuma suresi (sn):", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "\n")
  write_parquet(d, pq)
  saveRDS(d, file.path(ddir, "gyka_2023_2025.rds"))
  fwrite(s, file.path(ddir, "sozluk.csv"))
  fwrite(kodlar, file.path(ddir, "kodlar.csv"))
  fwrite(dogr_sayfa, file.path(ddir, "dogrulama_sayfasi.csv"))
  rm(d, kodlar, dogr_sayfa)
  gc()
}
sozluk <- fread(file.path(ddir, "sozluk.csv"))
kodlar <- fread(file.path(ddir, "kodlar.csv"))
d <- as.data.table(read_parquet(pq))
cat("Veri:", nrow(d), "satir,", ncol(d), "sutun\n")

dogr <- list()
kontrol <- function(ad, beklenen, bulunan, tol = 0) {
  durum <- if (is.numeric(beklenen) && is.numeric(bulunan)) {
    if (abs(bulunan - beklenen) <= tol) "Tuttu" else "Tutmadi"
  } else if (identical(as.character(beklenen), as.character(bulunan))) "Tuttu" else "Tutmadi"
  dogr[[length(dogr) + 1]] <<- data.table(kontrol = ad, beklenen = as.character(beklenen), bulunan = as.character(bulunan), durum = durum)
  invisible(NULL)
}
d25 <- d[yil == 2025]
h25 <- d25[hane_reisi == 1]
f25 <- d25[f_kayit == 1]
kontrol("Satir sayisi", 239985, nrow(d))
kontrol("Sutun sayisi", 266, ncol(d))
kontrol("2025 hane (hane_reisi == 1)", 25836, nrow(h25))
kontrol("2025 fert anketi (f_kayit == 1)", 61812, nrow(f25))
kontrol("2025 tum fert", 80206, nrow(d25))
kontrol("2025 HE090 = 1 (hane)", 15772, h25[HE090 == 1, .N])
kontrol("2025 HE090 = 2 (hane)", 10064, h25[HE090 == 2, .N])
kontrol("2025 FS050 = 1", 3950, f25[FS050 == 1, .N])
kontrol("2025 FS050 = 2", 52977, f25[FS050 == 2, .N])
kontrol("2025 FS050 = 3", 4885, f25[FS050 == 3, .N])
kontrol("2025 FS060 = 1", 340, f25[FS060 == 1, .N])
kontrol("2025 FS060 = 5", 2531, f25[FS060 == 5, .N])
kontrol("2025 FS080 = 1", 497, f25[FS080 == 1, .N])
kontrol("2025 MODUL1_H_1 = 1", 1573, h25[MODUL1_H_1 == 1, .N])
kontrol("2025 MODUL1_H_1 = 2", 13126, h25[MODUL1_H_1 == 2, .N])
kontrol("2025 MODUL1_H_1 = 3", 10484, h25[MODUL1_H_1 == 3, .N])
kontrol("2025 MODUL1_H_1 = 4", 653, h25[MODUL1_H_1 == 4, .N])
kontrol("2025 MODUL1_H_3 = 1", 1377, h25[MODUL1_H_3 == 1, .N])
kontrol("2025 MODUL1_H_3 = 4", 20, h25[MODUL1_H_3 == 4, .N])
kontrol("2025 MODUL1_H_2 = 1", 1256, h25[MODUL1_H_2 == 1, .N])
kontrol("2025 MODUL1_H_2 = 4", 7946, h25[MODUL1_H_2 == 4, .N])
kontrol("2025 FS020 = 1", 24674, f25[FS020 == 1, .N])
kontrol("2025 gunluk_meyve = 1", 22527, f25[gunluk_meyve == 1, .N])
kontrol("2025 gunluk_sebze = 1", 27222, f25[gunluk_sebze == 1, .N])
kontrol("2025 et_tavuk_balik_siklik = 0", 214, f25[et_tavuk_balik_siklik == 0, .N])
kontrol("2025 et_tavuk_balik_siklik = 1", 20893, f25[et_tavuk_balik_siklik == 1, .N])
kontrol("2025 et_tavuk_balik_siklik = 2", 37122, f25[et_tavuk_balik_siklik == 2, .N])
kontrol("2025 et_tavuk_balik_siklik = 3", 3583, f25[et_tavuk_balik_siklik == 3, .N])
kontrol("2025 bki_grup = 1 (18+)", 1128, f25[bki_grup == 1, .N])
kontrol("2025 bki_grup = 2 (18+)", 21718, f25[bki_grup == 2, .N])
kontrol("2025 bki_grup = 3 (18+)", 22470, f25[bki_grup == 3, .N])
kontrol("2025 bki_grup = 4 (18+)", 12583, f25[bki_grup == 4, .N])
kontrol("2023 HB031 = TR63 hane", 0, d[yil == 2023 & HB031 == "TR63", .N])
kontrol("2025 FK050 toplami (milyon kisi, ~84)", 84, round(d25[, sum(FK050)] / 1e6, 2), tol = 1)
kontrol("2025 FB030 toplami (milyon, 65-66)", 65.5, round(f25[, sum(FB030)] / 1e6, 2), tol = 0.5)
kontrol("2025 FB030 toplami = FK070 >= 15 FK050 toplami (milyon)", round(d25[FK070 >= 15, sum(FK050)] / 1e6, 3), round(f25[, sum(FB030)] / 1e6, 3), tol = 0.001)
kontrol("2025 HB040 toplami (milyon hane, 26,7-27,2)", 26.95, round(h25[, sum(HB040)] / 1e6, 2), tol = 0.25)
kontrol("2025 yoksul_60 (FK050, tum fertler, %)", 20.6, round(100 * d25[, weighted.mean(yoksul_60, FK050)], 1), tol = 0.05)
gini <- function(y, w) {
  o <- order(y)
  y <- y[o]
  w <- w[o]
  p <- cumsum(w) / sum(w)
  q <- cumsum(w * y) / sum(w * y)
  1 - sum((p - c(0, head(p, -1))) * (q + c(0, head(q, -1))))
}
kontrol("2025 Gini (esdeger gelir, FK050)", 0.410, round(d25[, gini(esdeger_gelir, FK050)], 3), tol = 0.0005)
kontrol("2025 karsilanamayan_tibbi_ihtiyac (FB030, 16+, %)", 0.7, round(100 * f25[FK070 >= 16, weighted.mean(karsilanamayan_tibbi_ihtiyac, FB030)], 1), tol = 0.05)
dogr <- rbindlist(dogr)
print(dogr)
fwrite(dogr, "Results/dogrulama_kontrolleri.csv")

d25[, egitim5 := fcase(FE030 %in% 0:1, 1L, FE030 %in% 2:3, 2L, FE030 == 4, 3L, FE030 %in% 5:6, 4L, FE030 %in% 7:10, 5L, default = NA_integer_)]
enbuyuk <- function(x) if (all(is.na(x))) NA_integer_ else max(x, na.rm = TRUE)
hagg <- d25[, .(
  n_yetiskin = sum(FK070 >= 18),
  n_yetiskin_fkayit = sum(FK070 >= 18 & f_kayit == 1),
  n_kronik = sum(FK070 >= 18 & f_kayit == 1 & FS020 %in% 1),
  n_gorusmesiz = sum(FK070 >= 18 & f_kayit == 0),
  n_gali = sum(FK070 >= 18 & FS030 %in% 1:2),
  n_calisan = sum(FK070 >= 18 & FI010 %in% 1:4),
  n_engelli = sum(FK070 >= 18 & wg_engellilik %in% 1),
  n_kotu_saglik = sum(FK070 >= 18 & FS010 %in% 4:5),
  egitim_max = enbuyuk(egitim5[FK070 >= 18 & f_kayit == 1]),
  yas_ort_yetiskin = if (any(FK070 >= 18)) mean(FK070[FK070 >= 18]) else NA_real_,
  n_yetiskin17 = sum(FK070 >= 17),
  n_kronik17 = sum(FK070 >= 17 & f_kayit == 1 & FS020 %in% 1),
  n_kronik_15 = sum(f_kayit == 1 & FS020 %in% 1)
), by = hane_kimlik]
hv <- c("hane_kimlik", "BULTEN", "HB040", "FK050", "HB030", "ibbs1_ad", "HB031", "HB050", "HH020", "HH010", "hane_buyuklugu", "hane_0_13", "hane_0_17", "hane_65plus",
        "esdeger_gelir", "goreli_esdeger_gelir", "gelir_dilim5", "gelir_dilim10", "hane_gelir_dilim5", "yoksul_60", "HE040", "HE090", "HE100", "MODUL1_H_1", "MODUL1_H_2", "MODUL1_H_3",
        "msy_beklenmedik_harcama", "msy_tatil", "msy_borc_odeme", "msy_et_tavuk_balik", "msy_isinma", "msy_otomobil", "msy_mobilya", "msy_sayi", "msy_7", "konut_maliyet_asiri_yuk")
hane_2025 <- merge(h25[, ..hv], hagg, by = "hane_kimlik")
hane_2025[h25, on = "hane_kimlik", `:=`(reis_yas = i.FK070, reis_cinsiyet = i.FK090, reis_medeni = i.FB100, reis_fkayit = i.f_kayit)]
hane_2025[, `:=`(
  A = as.integer(HE090 == 2),
  C_hane = as.integer(n_kronik >= 1),
  Y_saglik_yuk = as.integer(MODUL1_H_1 == 1 | MODUL1_H_3 == 1),
  Y_tibbi_yuk = as.integer(MODUL1_H_1 == 1),
  Y_ilac_yuk = as.integer(MODUL1_H_3 == 1),
  Y_saglik_yuk_genis = as.integer(MODUL1_H_1 %in% 1:2 | MODUL1_H_3 %in% 1:2),
  Y_dis_yuk = as.integer(MODUL1_H_2 == 1),
  dis_harcama_yok = as.integer(MODUL1_H_2 == 4),
  log_esdeger_gelir = log(esdeger_gelir),
  hane_yoksunluk_6 = msy_beklenmedik_harcama + msy_tatil + msy_borc_odeme + msy_isinma + msy_otomobil + msy_mobilya,
  C_hane_sayi = pmin(n_kronik, 2L),
  C_hane_oran = fifelse(n_yetiskin_fkayit > 0, n_kronik / n_yetiskin_fkayit, NA_real_),
  C_hane_gali = as.integer(n_gali >= 1),
  C_hane17 = as.integer(n_kronik17 >= 1),
  kronik_bilinmeyen = as.integer(n_gorusmesiz >= 1 & n_kronik == 0)
)]
setorder(hane_2025, hane_kimlik)

y_ekle <- c("hane_kimlik", "n_yetiskin", "n_yetiskin_fkayit", "n_kronik", "n_gorusmesiz", "n_gali", "n_calisan", "egitim_max")
yetiskin_2025 <- merge(d25[f_kayit == 1 & FK070 >= 17], hagg[, ..y_ekle], by = "hane_kimlik")
yetiskin_2025[, `:=`(
  yas17 = as.integer(FK070 == 17),
  A = as.integer(HE090 == 2),
  C_fert = as.integer(FS020 == 1),
  C_hane = as.integer(n_kronik >= 1),
  Y_saglik_yuk = as.integer(MODUL1_H_1 == 1 | MODUL1_H_3 == 1),
  Y_saglik_yuk_genis = as.integer(MODUL1_H_1 %in% 1:2 | MODUL1_H_3 %in% 1:2),
  Y_dis_yuk = as.integer(MODUL1_H_2 == 1),
  meyve_gunluk_degil = as.integer(gunluk_meyve == 0),
  sebze_gunluk_degil = as.integer(gunluk_sebze == 0),
  et_haftada_az = fcase(et_tavuk_balik_siklik == 0, NA_integer_, et_tavuk_balik_siklik == 1, 1L, et_tavuk_balik_siklik >= 2, 0L),
  et_haftada_az_veg = as.integer(et_tavuk_balik_siklik <= 1),
  vejetaryen = as.integer(et_tavuk_balik_siklik == 0),
  B = as.integer(!(gunluk_meyve == 1 & gunluk_sebze == 1)),
  diyet_skor = meyve_siklik + sebze_siklik,
  ihtiyac_var = as.integer(FS050 %in% 1:2),
  dis_ihtiyac_var = as.integer(FS070 %in% 1:2),
  odeme_tibbi = fifelse(FS050 %in% 1:2, as.integer(FS050 == 1 & FS060 %in% 1), NA_integer_),
  odeme_dis = fifelse(FS070 %in% 1:2, as.integer(FS070 == 1 & FS080 %in% 1), NA_integer_),
  neden_grubu = fcase(FS060 == 1, 1L, FS060 %in% c(3, 5), 2L, FS060 %in% c(2, 4, 6, 7, 8), 3L, default = NA_integer_),
  uzman_hic = fcase(MODUL1_F_3 == 6, 1L, MODUL1_F_3 %in% 7:10, 0L, default = NA_integer_),
  uzman_hic_bos_hic = fcase(MODUL1_F_3 == 6, 1L, MODUL1_F_3 %in% 7:10, 0L, is.na(MODUL1_F_3) & FS050 == 3, 1L, default = NA_integer_),
  pratisyen_hic = fcase(MODUL1_F_2 == 6, 1L, MODUL1_F_2 %in% 7:10, 0L, default = NA_integer_),
  pratisyen_hic_bos_hic = fcase(MODUL1_F_2 == 6, 1L, MODUL1_F_2 %in% 7:10, 0L, is.na(MODUL1_F_2) & FS050 == 3, 1L, default = NA_integer_),
  dis_hekimi_hic = fcase(MODUL1_F_1 == 6, 1L, MODUL1_F_1 %in% 7:10, 0L, default = NA_integer_),
  dis_hekimi_hic_bos_hic = fcase(MODUL1_F_1 == 6, 1L, MODUL1_F_1 %in% 7:10, 0L, is.na(MODUL1_F_1) & FS070 == 3, 1L, default = NA_integer_),
  saglik_kotu = as.integer(FS010 %in% 4:5),
  gali = as.integer(FS030 %in% 1:2),
  calisma7 = fcase(FI010 %in% 1:4, 1L, FI010 == 5, 2L, FI010 == 6, 3L, FI010 == 7, 4L, FI010 == 8, 5L, FI010 == 9, 6L, FI010 == 10, 7L, default = NA_integer_),
  yas_grup = fcase(FK070 < 30, 1L, FK070 < 45, 2L, FK070 < 65, 3L, FK070 >= 65, 4L),
  log_esdeger_gelir = log(esdeger_gelir),
  hane_yoksunluk_6 = msy_beklenmedik_harcama + msy_tatil + msy_borc_odeme + msy_isinma + msy_otomobil + msy_mobilya,
  hane_reisi_degil = as.integer(hane_reisi == 0)
)]
yetiskin_2025[, odeme_herhangi := fifelse(ihtiyac_var == 1 | dis_ihtiyac_var == 1, as.integer(odeme_tibbi %in% 1 | odeme_dis %in% 1), NA_integer_)]
yetiskin_2025[, ihtiyac_herhangi := as.integer(ihtiyac_var == 1 | dis_ihtiyac_var == 1)]
setorder(yetiskin_2025, hane_kimlik, fert_kimlik)
yetiskin17_2025 <- copy(yetiskin_2025)
yetiskin_2025 <- yetiskin_2025[FK070 >= 18]
cat("hane_2025:", nrow(hane_2025), " yetiskin_2025:", nrow(yetiskin_2025), " yetiskin17_2025:", nrow(yetiskin17_2025), "\n")
write_parquet(hane_2025, file.path(ddir, "hane_2025.parquet"))
write_parquet(yetiskin_2025, file.path(ddir, "yetiskin_2025.parquet"))

degisken_tanimlari <- rbindlist(list(
  list("A", "HE090", "1 = household cannot afford a meal with meat, chicken or fish every second day (HE090 = 2); 0 = can afford (HE090 = 1)", "All households; written to every member", "No missing"),
  list("C_hane", "FS020, FK070, f_kayit", "1 = at least one adult (FK070 >= 18) with individual record reports a chronic illness (FS020 = 1)", "Households", "Adults without individual record counted as not chronic; 30 households with unknown status flagged (kronik_bilinmeyen)"),
  list("C_fert", "FS020", "1 = adult reports a chronic illness", "Adults 18+ with individual record", "No missing"),
  list("Y_saglik_yuk", "MODUL1_H_1, MODUL1_H_3", "1 = medical or medicine expenditure was a heavy burden (code 1 in either); 0 = codes 2, 3, 4", "2025 households", "No missing"),
  list("Y_tibbi_yuk", "MODUL1_H_1", "1 = code 1 (heavy burden)", "2025 households", "No missing"),
  list("Y_ilac_yuk", "MODUL1_H_3", "1 = code 1 (heavy burden)", "2025 households", "No missing"),
  list("Y_saglik_yuk_genis", "MODUL1_H_1, MODUL1_H_3", "1 = heavy or some burden (code 1 or 2 in either)", "2025 households", "No missing"),
  list("Y_dis_yuk", "MODUL1_H_2", "1 = dental expenditure was a heavy burden; code 4 (no dental expenditure) analyzed separately", "2025 households", "No missing"),
  list("meyve_siklik, sebze_siklik", "MODUL1_F_8, MODUL1_F_9", "Ordinal 0-5: never, <1/week, 1-3/week, 4-6/week, once a day, >=2 a day", "2025 adults", "No missing"),
  list("et_tavuk_balik_siklik", "MODUL2_F_5", "Ordinal 0-3: never (vegetarian/vegan), <1/week, weekly not daily, daily", "2025 adults", "No missing"),
  list("meyve_gunluk_degil, sebze_gunluk_degil", "gunluk_meyve, gunluk_sebze", "1 = less than once a day", "2025 adults", "No missing"),
  list("et_haftada_az", "et_tavuk_balik_siklik", "1 = less than once a week (code 1); 0 = weekly or daily (codes 2-3)", "2025 adults; vegetarians (code 0) excluded in main analysis", "Code 0 set to missing (sensitivity: counted as 1)"),
  list("B", "gunluk_meyve, gunluk_sebze", "1 = not both fruit and vegetables daily", "2025 adults", "No missing"),
  list("diyet_skor", "meyve_siklik + sebze_siklik", "Sum 0-10", "2025 adults", "No missing"),
  list("ihtiyac_var", "FS050", "1 = needed medical examination or treatment in past 12 months (FS050 = 1 or 2)", "Adults", "Universe of unmet-need analyses"),
  list("odeme_tibbi", "FS050, FS060", "1 = unmet medical need because of cost (FS050 = 1 and FS060 = 1)", "ihtiyac_var = 1", "Missing outside universe"),
  list("odeme_dis", "FS070, FS080", "1 = unmet dental need because of cost (FS070 = 1 and FS080 = 1)", "Dental need (FS070 = 1 or 2)", "Missing outside universe"),
  list("odeme_herhangi", "odeme_tibbi, odeme_dis", "1 = unmet medical or dental need because of cost", "Medical or dental need", "Missing outside universe"),
  list("neden_grubu", "FS060", "1 = cost (code 1); 2 = system (3 distance/transport, 5 late appointment); 3 = personal (2, 4, 6, 7, 8)", "FS050 = 1", "Missing outside universe"),
  list("uzman_hic", "MODUL1_F_3", "1 = no specialist visit in past 12 months (code 6); 0 = codes 7-10", "Adults; analyzed among C_fert = 1", "Blanks (only FS050 = 3) missing; sensitivity: blanks = never"),
  list("karsilanamayan_tibbi_ihtiyac", "FS050, FS060", "Eurostat-compatible: cost or distance (FS060 = 1 or 3)", "Adults", "Provided in data"),
  list("gelir_dilim5", "esdeger_gelir", "Equivalized disposable income quintile, FK050-weighted within year (income reference year 2024)", "All", "No missing"),
  list("egitim5 / egitim_max", "FE030", "1 = no diploma (0-1), 2 = primary (2-3), 3 = lower secondary (4), 4 = upper secondary (5-6), 5 = tertiary (7-10); household: highest among adults", "Adults with individual record", "Household without adult record: missing (1 household)"),
  list("calisma7", "FI010", "1 = employed (1-4), 2 = seeking work (5), 3 = student (6), 4 = retired (7), 5 = unable to work (8), 6 = domestic tasks/care (9), 7 = other (10)", "Adults", "No missing"),
  list("n_yetiskin, n_kronik, n_gali, n_calisan, n_gorusmesiz", "FK070, FS020, FS030, FI010, f_kayit", "Counts of adults (FK070 >= 18) in household: all, chronic, GALI-limited (FS030 = 1-2), employed (FI010 = 1-4), without individual record", "Households", "Computed from all 2025 rows"),
  list("reis_yas, reis_cinsiyet", "FK070, FK090", "Age and sex of household reference person (hane_reisi = 1)", "Households", "No missing")
))
setnames(degisken_tanimlari, c("ad", "kaynak_degisken", "kodlama", "evren", "eksik_kurali"))
fwrite(degisken_tanimlari, "Results/degisken_tanimlari.csv")

dir.create("Results/on_kontroller", showWarnings = FALSE)
hane_m <- hane_2025[n_yetiskin > 0]
des_h <- svydesign(ids = ~1, weights = ~HB040, data = hane_m)
des_y <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = yetiskin_2025)
z_mde <- qnorm(0.95) + qnorm(0.80)

ok1 <- dcast(hane_2025[, .N, by = .(MODUL1_H_1, MODUL1_H_3)], MODUL1_H_1 ~ MODUL1_H_3, value.var = "N", fill = 0)
fwrite(ok1, "Results/on_kontroller/ok1_modul1_h1_h3.csv")
ok1b <- rbindlist(lapply(c("Y_saglik_yuk", "Y_tibbi_yuk", "Y_ilac_yuk", "Y_saglik_yuk_genis", "Y_dis_yuk", "dis_harcama_yok"), function(v) {
  e <- svymean(as.formula(paste0("~", v)), des_h)
  ci <- confint(svyciprop(as.formula(paste0("~", v)), des_h, method = "logit"))
  data.table(degisken = v, olay = hane_m[get(v) == 1, .N], n = nrow(hane_m), yuzde = 100 * unname(coef(e)), se = 100 * as.numeric(SE(e)), alt = 100 * unname(ci[1]), ust = 100 * unname(ci[2]))
}))
fwrite(ok1b, "Results/on_kontroller/ok1_y_yayginlik.csv")
print(ok1b)

hucre_svy <- function(des, y, a, b, dat, w) {
  s <- svyby(as.formula(paste0("~", y)), as.formula(paste0("~", a, " + ", b)), des, svymean, covmat = TRUE)
  ss <- data.table(a = s[[a]], b = s[[b]], ham_yuzde = 100 * coef(s), se = 100 * SE(s))
  setnames(ss, c("a", "b"), c(a, b))
  nn <- dat[!is.na(get(y)), .(n = .N, olay = sum(get(y)), pay = 100 * sum(get(w))), keyby = c(a, b)]
  nn[, pay := 100 * pay / sum(pay)]
  out <- merge(nn, ss, by = c(a, b))
  setorderv(out, c(a, b))
  k <- (ss[[a]] == 1 & ss[[b]] == 1) - (ss[[a]] == 1 & ss[[b]] == 0) - (ss[[a]] == 0 & ss[[b]] == 1) + (ss[[a]] == 0 & ss[[b]] == 0)
  list(hucre = out, kontrast = svycontrast(s, k))
}
hh <- hucre_svy(des_h, "Y_saglik_yuk", "A", "C_hane", hane_m, "HB040")
hucre_h_n <- hh$hucre
dham_h <- hh$kontrast
mde_h <- data.table(duzey = "hane", delta_ham = 100 * unname(coef(dham_h)), se = 100 * as.numeric(SE(dham_h)), mde = z_mde * 100 * as.numeric(SE(dham_h)))
hy <- hucre_svy(des_y, "Y_saglik_yuk", "A", "C_fert", yetiskin_2025, "FB030")
hucre_y_n <- hy$hucre
dham_y <- hy$kontrast
mde_y <- data.table(duzey = "yetiskin", delta_ham = 100 * unname(coef(dham_y)), se = 100 * as.numeric(SE(dham_y)), mde = z_mde * 100 * as.numeric(SE(dham_y)))
fwrite(hucre_h_n, "Results/on_kontroller/ok2_hucre_hane.csv")
fwrite(hucre_y_n, "Results/on_kontroller/ok2_hucre_yetiskin.csv")
print(hucre_h_n)
print(hucre_y_n)

ok3 <- data.table(
  gorusmesiz_yetiskinli_hane = hane_2025[n_gorusmesiz > 0, .N],
  gorusmesiz_yetiskinli_hane_C1 = hane_2025[n_gorusmesiz > 0 & C_hane == 1, .N],
  kronik_bilinmeyen_hane = hane_2025[kronik_bilinmeyen == 1, .N],
  kronik_bilinmeyen_agirlikli_yuzde = 100 * hane_2025[kronik_bilinmeyen == 1, sum(HB040)] / hane_2025[, sum(HB040)],
  yetiskinsiz_hane = hane_2025[n_yetiskin == 0, .N]
)
fwrite(ok3, "Results/on_kontroller/ok3_kronik_eksik.csv")
print(ok3)

des_ih <- subset(des_y, ihtiyac_var == 1)
hab <- hucre_svy(des_ih, "odeme_tibbi", "A", "B", yetiskin_2025[ihtiyac_var == 1], "FB030")
hucre_ab_n <- hab$hucre
dham_ab <- hab$kontrast
mde_ab <- data.table(duzey = "eski_hipotez", delta_ham = 100 * unname(coef(dham_ab)), se = 100 * as.numeric(SE(dham_ab)), mde = z_mde * 100 * as.numeric(SE(dham_ab)))
fwrite(hucre_ab_n, "Results/on_kontroller/ok4_eski_hipotez_hucre.csv")
print(hucre_ab_n)

ok5 <- sozluk[grepl("yanit|cevap|bilgi ver|respond", paste(aciklama, tanim), ignore.case = TRUE), .(degisken, aciklama)]
fwrite(ok5, "Results/on_kontroller/ok5_yanitlayici_degisken_arama.csv")
print(ok5)

ok6 <- yetiskin_2025[, .(n = .N, vejetaryen = sum(vejetaryen), agirlikli_yuzde = 100 * weighted.mean(vejetaryen, FB030)), keyby = A]
ok6b <- yetiskin_2025[, .(vejetaryen = sum(vejetaryen), agirlikli_yuzde = 100 * weighted.mean(vejetaryen, FB030)), keyby = gelir_dilim5]
fwrite(ok6, "Results/on_kontroller/ok6_vejetaryen_A.csv")
fwrite(ok6b, "Results/on_kontroller/ok6_vejetaryen_gelir.csv")
print(ok6)

havuz_y <- c("FK070", "FK090", "FE030", "FB100", "FI010", "HB050", "hane_buyuklugu", "hane_0_17", "hane_65plus", "HB030", "gelir_dilim5", "esdeger_gelir", "HH020", "FS020", "FS030", "FS010", "wg_engellilik",
             "meyve_siklik", "sebze_siklik", "et_tavuk_balik_siklik", "HE090", "MODUL1_H_1", "MODUL1_H_3")
havuz_h <- c("gelir_dilim5", "n_yetiskin", "hane_0_17", "hane_65plus", "HB050", "reis_yas", "reis_cinsiyet", "egitim_max", "n_calisan", "n_gali", "HB030", "HH020", "reis_medeni", "HE090", "MODUL1_H_1", "MODUL1_H_3")
ok7 <- rbind(
  data.table(duzey = "yetiskin", degisken = havuz_y, eksik = sapply(havuz_y, function(v) sum(is.na(yetiskin_2025[[v]]))), n = nrow(yetiskin_2025)),
  data.table(duzey = "hane", degisken = havuz_h, eksik = sapply(havuz_h, function(v) sum(is.na(hane_2025[[v]]))), n = nrow(hane_2025))
)
ok7[, eksik_yuzde := 100 * eksik / n]
ok7_tam <- data.table(
  duzey = c("yetiskin", "hane", "hane_reis_medeni_haric"),
  tam_vaka = c(nrow(na.omit(yetiskin_2025[, ..havuz_y])), nrow(na.omit(hane_2025[, ..havuz_h])), nrow(na.omit(hane_2025[, setdiff(havuz_h, "reis_medeni"), with = FALSE]))),
  n = c(nrow(yetiskin_2025), nrow(hane_2025), nrow(hane_2025))
)
ok7_tam[, kayip_yuzde := 100 * (1 - tam_vaka / n)]
fwrite(ok7, "Results/on_kontroller/ok7_eksik_veri.csv")
fwrite(ok7_tam, "Results/on_kontroller/ok7_tam_vaka.csv")
print(ok7[eksik > 0])
print(ok7_tam)

agirlik_ozet <- function(w, ad) data.table(agirlik = ad, n = length(w), min = min(w), q1 = quantile(w, 0.25), medyan = median(w), q3 = quantile(w, 0.75), max = max(w), max_medyan = max(w) / median(w), cv = sd(w) / mean(w), kish_neff = sum(w)^2 / sum(w^2), deff_w = 1 + (sd(w) / mean(w))^2 * (length(w) - 1) / length(w))
ok8 <- rbind(agirlik_ozet(hane_2025$HB040, "HB040 (hane)"), agirlik_ozet(yetiskin_2025$FB030, "FB030 (18+ yetiskin)"))
fwrite(ok8, "Results/on_kontroller/ok8_agirliklar.csv")
print(ok8)

kat_say <- function(dat, v, y, w) dat[, .(n = .N, olay = sum(get(y), na.rm = TRUE), agirlikli_yuzde = 100 * sum(get(w)) / dat[, sum(get(w))]), keyby = .(deger = get(v))][, degisken := v][]
ok9h <- rbindlist(lapply(c("gelir_dilim5", "n_yetiskin", "hane_0_17", "hane_65plus", "HB050", "reis_cinsiyet", "egitim_max", "n_calisan", "n_gali", "HB030", "HH020", "reis_medeni", "n_engelli", "n_kotu_saglik", "n_kronik"),
                         function(v) kat_say(hane_m, v, "Y_saglik_yuk", "HB040")))
ok9y <- rbindlist(lapply(c("yas_grup", "FK090", "egitim5", "FB100", "calisma7", "HB050", "HB030", "gelir_dilim5", "HH020", "FS020", "FS030", "FS010", "wg_engellilik", "hane_buyuklugu", "hane_0_17"),
                         function(v) kat_say(yetiskin_2025, v, "Y_saglik_yuk", "FB030")))
ok9y_et <- rbindlist(lapply(c("egitim5", "FB100", "calisma7", "HB050", "HB030", "gelir_dilim5", "HH020", "FS010"), function(v) kat_say(yetiskin_2025[!is.na(et_haftada_az)], v, "et_haftada_az", "FB030")))
fwrite(ok9h, "Results/on_kontroller/ok9_kategori_hane.csv")
fwrite(ok9y, "Results/on_kontroller/ok9_kategori_yetiskin.csv")
fwrite(ok9y_et, "Results/on_kontroller/ok9_kategori_yetiskin_et.csv")
print(ok9h[n < 100 | olay < 10])
print(ok9y[n < 100 | olay < 10])

h2_ham <- rbindlist(lapply(c("et_haftada_az", "meyve_gunluk_degil", "sebze_gunluk_degil"), function(v) {
  dd <- subset(des_y, !is.na(get(v)))
  s <- svyby(as.formula(paste0("~", v)), ~A, dd, svymean, covmat = TRUE)
  k <- svycontrast(s, (s$A == 1) - (s$A == 0))
  data.table(duzey = v, delta_ham = 100 * as.numeric(coef(k)), se = 100 * as.numeric(SE(k)), mde = z_mde * 100 * as.numeric(SE(k)), p0 = 100 * unname(coef(s)[s$A == 0]), p1 = 100 * unname(coef(s)[s$A == 1]))
}))
mde_tum <- rbindlist(list(mde_h, mde_y, mde_ab, h2_ham), fill = TRUE)
fwrite(mde_tum, "Results/on_kontroller/mde_kaba.csv")
print(mde_tum)

etiket <- list(
  yas_grup = c("1" = "18\u201329", "2" = "30\u201344", "3" = "45\u201364", "4" = "\u226565"),
  FK090 = c("1" = "Male", "2" = "Female"),
  egitim5 = c("1" = "No diploma", "2" = "Primary", "3" = "Lower secondary", "4" = "Upper secondary", "5" = "Tertiary"),
  egitim_max = c("1" = "No diploma", "2" = "Primary", "3" = "Lower secondary", "4" = "Upper secondary", "5" = "Tertiary"),
  calisma7 = c("1" = "Employed", "2" = "Seeking work", "3" = "Student", "4" = "Retired", "5" = "Unable to work (health)", "6" = "Domestic tasks or care", "7" = "Other"),
  FB100 = c("1" = "Married", "2" = "Never married", "3" = "Widowed", "4" = "Divorced"),
  HB050 = c("1" = "One person", "21" = "Couple without children", "22" = "Couple with children", "23" = "Single parent with children", "3" = "Nuclear family with others", "4" = "Other multi-person"),
  HB030 = c(TR1 = "Istanbul", TR2 = "West Marmara", TR3 = "Aegean", TR4 = "East Marmara", TR5 = "West Anatolia", TR6 = "Mediterranean", TR7 = "Central Anatolia", TR8 = "West Black Sea", TR9 = "East Black Sea", TRA = "Northeast Anatolia", TRB = "Central East Anatolia", TRC = "Southeast Anatolia"),
  gelir_dilim5 = c("1" = "Q1 (lowest)", "2" = "Q2", "3" = "Q3", "4" = "Q4", "5" = "Q5 (highest)"),
  FS020 = c("1" = "Yes", "2" = "No"),
  FS030 = c("1" = "Severely limited", "2" = "Limited", "3" = "Not limited"),
  FS010 = c("1" = "Very good", "2" = "Good", "3" = "Fair", "4" = "Bad", "5" = "Very bad"),
  wg_engellilik = c("0" = "No", "1" = "Yes"),
  meyve_siklik = c("0" = "Never", "1" = "Less than once a week", "2" = "1\u20133 times a week", "3" = "4\u20136 times a week", "4" = "Once a day", "5" = "Twice a day or more"),
  sebze_siklik = c("0" = "Never", "1" = "Less than once a week", "2" = "1\u20133 times a week", "3" = "4\u20136 times a week", "4" = "Once a day", "5" = "Twice a day or more"),
  et_tavuk_balik_siklik = c("0" = "Never (vegetarian or vegan)", "1" = "Less than once a week", "2" = "Weekly, not daily", "3" = "Daily"),
  bki_grup = c("1" = "Underweight (<18.5)", "2" = "Normal (18.5\u2013<25)", "3" = "Overweight (25\u2013<30)", "4" = "Obesity (\u226530)"),
  HH020 = c("1" = "Owner", "2" = "Tenant", "3" = "Employer-provided", "4" = "Rent-free"),
  hane_boyut = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "\u22656"),
  cocuk4 = c("0" = "0", "1" = "1", "2" = "2", "3" = "3", "4" = "\u22654"),
  yasli3 = c("0" = "0", "1" = "1", "2" = "\u22652"),
  C_hane = c("0" = "No", "1" = "Yes"),
  gali4 = c("0" = "0", "1" = "1", "2" = "2", "3" = "\u22653"),
  MODUL1_H_1 = c("1" = "Heavy burden", "2" = "Some burden", "3" = "No burden", "4" = "No such expenditure"),
  MODUL1_H_3 = c("1" = "Heavy burden", "2" = "Some burden", "3" = "No burden", "4" = "No such expenditure"),
  MODUL1_H_2 = c("1" = "Heavy burden", "2" = "Some burden", "3" = "No burden", "4" = "No such expenditure"),
  Y_saglik_yuk = c("0" = "No", "1" = "Yes")
)
degisken_ad <- c(yas_grup = "Age group (years)", FK090 = "Sex", egitim5 = "Education", calisma7 = "Current activity status", FB100 = "Marital status", HB050 = "Household type",
                 HB030 = "Region (NUTS-1)", gelir_dilim5 = "Equivalized income quintile", FS020 = "Chronic illness", FS030 = "Activity limitation (GALI)", FS010 = "Self-rated health",
                 wg_engellilik = "Disability (WG-SS)", meyve_siklik = "Fruit consumption", sebze_siklik = "Vegetable or salad consumption", et_tavuk_balik_siklik = "Meat, chicken or fish consumption",
                 bki_grup = "Body mass index (kg/m\u00b2)", hane_boyut = "Household size", cocuk4 = "Members aged <18 years", yasli3 = "Members aged \u226565 years", HH020 = "Tenure status",
                 C_hane = "Adult with chronic illness in household", gali4 = "Adults with activity limitation", MODUL1_H_1 = "Medical care expenditure", MODUL1_H_3 = "Medicine expenditure",
                 MODUL1_H_2 = "Dental care expenditure", Y_saglik_yuk = "Medical or medicine expenditure a heavy burden", egitim_max = "Highest education of adults")

yetiskin_2025[, bki_grup_t := fifelse(bki_supheli == 1, NA_real_, bki_grup)]
hane_m[, `:=`(hane_boyut = pmin(hane_buyuklugu, 6), cocuk4 = pmin(hane_0_17, 4), yasli3 = pmin(hane_65plus, 2), gali4 = pmin(n_gali, 3))]
des_h <- svydesign(ids = ~1, weights = ~HB040, data = hane_m)
des_y <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = yetiskin_2025)

kat_tablo <- function(dat, des, v, w, grup = "A", vk = v) {
  dd <- dat[!is.na(get(v))]
  lab <- etiket[[vk]]
  tot <- dd[, .(n = .N, ww = sum(get(w))), keyby = .(k = as.character(get(v)))][, pct := 100 * ww / sum(ww)][, g := "Total"]
  byg <- dd[, .(n = .N, ww = sum(get(w))), keyby = .(g = as.character(get(grup)), k = as.character(get(v)))][, pct := 100 * ww / sum(ww), by = g]
  out <- rbind(tot, byg)
  dd2 <- des[!is.na(des$variables[[v]]), ]
  dd2$variables$.vv <- factor(dd2$variables[[v]])
  dd2$variables$.gg <- factor(dd2$variables[[grup]])
  p <- tryCatch(svychisq(~.vv + .gg, dd2, statistic = "F")$p.value, error = function(e) NA_real_)
  out[, `:=`(degisken = v, degisken_ad = degisken_ad[[vk]], kategori = unname(lab[k]), sira = match(k, names(lab)), p = as.numeric(p))]
  out[]
}
kucuk_hucre <- function(n) fcase(n < 20, "suppressed", n <= 50, "flag", default = "")

t1_v <- c("yas_grup", "FK090", "egitim5", "calisma7", "FB100", "HB050", "HB030", "gelir_dilim5", "FS020", "FS030", "FS010", "wg_engellilik", "meyve_siklik", "sebze_siklik", "et_tavuk_balik_siklik", "bki_grup_t")
t1 <- rbindlist(lapply(t1_v, function(v) kat_tablo(yetiskin_2025, des_y, v, "FB030", vk = sub("_t$", "", v))))
t1[, kucuk := kucuk_hucre(n)]
t1_n <- yetiskin_2025[, .(n = .N, ww = sum(FB030)), keyby = .(g = as.character(A))][, pct := 100 * ww / sum(ww)]
fwrite(t1, "Results/table1_veri_uzun.csv")
print(t1[g == "Total" & sira == 1])

ts1_v <- c("HB050", "hane_boyut", "cocuk4", "yasli3", "gelir_dilim5", "HB030", "HH020", "C_hane", "gali4", "MODUL1_H_1", "MODUL1_H_3", "MODUL1_H_2", "Y_saglik_yuk")
ts1 <- rbindlist(lapply(ts1_v, function(v) kat_tablo(hane_m, des_h, v, "HB040")))
ts1[, kucuk := kucuk_hucre(n)]
fwrite(ts1, "Results/tableS1_veri_uzun.csv")

dh <- d[hane_reisi == 1, .(yil, hane_kimlik, HB040, HE090, A = as.integer(HE090 == 2), gelir_dilim5, HB030, HB031, HB050)]
dp <- d[, .(yil, hane_kimlik, FK050, A = as.integer(HE090 == 2), gelir_dilim5, HB030, HB031, HB050)]
des_dh <- svydesign(ids = ~1, weights = ~HB040, data = dh)
des_dp <- svydesign(ids = ~hane_kimlik, weights = ~FK050, data = dp)
baglam_hesap <- function(des, duzey, by_v = NULL, tr63_haric = FALSE) {
  dd <- if (tr63_haric) des[des$variables$HB031 != "TR63", ] else des
  f <- if (is.null(by_v)) ~yil else as.formula(paste0("~yil + ", by_v))
  s <- svyby(~A, f, dd, svyciprop, method = "logit", vartype = c("se", "ci"))
  out <- as.data.table(s)
  setnames(out, c("A", "se.as.numeric(A)", "ci_l", "ci_u"), c("yuzde", "se", "alt", "ust"), skip_absent = TRUE)
  if (!"se" %in% names(out)) setnames(out, grep("^se", names(out), value = TRUE)[1], "se")
  out[, `:=`(yuzde = 100 * yuzde, se = 100 * se, alt = 100 * alt, ust = 100 * ust, duzey = duzey, kirilim = if (is.null(by_v)) "Total" else by_v, tr63_haric = tr63_haric)]
  if (!is.null(by_v)) setnames(out, by_v, "kategori") else out[, kategori := "Total"]
  out[, kategori := as.character(kategori)]
  nn <- dd$variables[dd$prob < Inf, .N, by = c("yil", if (is.null(by_v)) NULL else by_v)]
  if (!is.null(by_v)) setnames(nn, by_v, "kategori") else nn[, kategori := "Total"]
  nn[, kategori := as.character(kategori)]
  merge(out, nn, by = c("yil", "kategori"))
}
baglam <- rbindlist(list(
  baglam_hesap(des_dh, "hane"), baglam_hesap(des_dp, "kisi"),
  baglam_hesap(des_dh, "hane", "gelir_dilim5"), baglam_hesap(des_dp, "kisi", "gelir_dilim5"),
  baglam_hesap(des_dh, "hane", "HB030"), baglam_hesap(des_dp, "kisi", "HB030"),
  baglam_hesap(des_dh, "hane", "HB050"), baglam_hesap(des_dp, "kisi", "HB050"),
  baglam_hesap(des_dh, "hane", tr63_haric = TRUE), baglam_hesap(des_dp, "kisi", tr63_haric = TRUE),
  baglam_hesap(des_dh, "hane", "gelir_dilim5", tr63_haric = TRUE), baglam_hesap(des_dp, "kisi", "gelir_dilim5", tr63_haric = TRUE)
), fill = TRUE)
baglam[, kucuk := kucuk_hucre(N)]
dir.create(file.path(ddir, "figur_verileri"), showWarnings = FALSE)
fwrite(baglam, "Results/baglam_he090_2023_2025.csv")
print(baglam[kirilim == "Total"])
print(baglam[duzey == "kisi" & kirilim == "gelir_dilim5" & tr63_haric == FALSE])

frek <- rbindlist(lapply(c("meyve_siklik", "sebze_siklik", "et_tavuk_balik_siklik"), function(v) {
  f1 <- as.formula(paste0("~factor(", v, ")"))
  r <- rbindlist(lapply(list(c("A"), c("gelir_dilim5"), c("A", "gelir_dilim5")), function(b) {
    s <- svyby(f1, as.formula(paste0("~", paste(b, collapse = " + "))), des_y, svymean)
    m <- as.data.table(s)
    cn <- grep("^factor", names(m), value = TRUE)
    est <- melt(m[, c(b, cn[!grepl("^se", cn)]), with = FALSE], id.vars = b, variable.name = "k", value.name = "oran")
    sec <- melt(m[, c(b, grep("^se", names(m), value = TRUE)), with = FALSE], id.vars = b, variable.name = "k", value.name = "se")
    est[, k := sub(".*\\)", "", k)]
    sec[, k := sub(".*\\)", "", k)]
    o <- merge(est, sec, by = c(b, "k"))
    o[, kirilim := paste(b, collapse = "_")]
    if (!"A" %in% b) o[, A := NA_integer_]
    if (!"gelir_dilim5" %in% b) o[, gelir_dilim5 := NA_real_]
    o
  }), use.names = TRUE)
  nn <- rbind(yetiskin_2025[, .N, by = .(A, k = as.character(get(v)))][, `:=`(gelir_dilim5 = NA_real_, kirilim = "A")],
              yetiskin_2025[, .N, by = .(gelir_dilim5, k = as.character(get(v)))][, `:=`(A = NA_integer_, kirilim = "gelir_dilim5")],
              yetiskin_2025[, .N, by = .(A, gelir_dilim5, k = as.character(get(v)))][, kirilim := "A_gelir_dilim5"], use.names = TRUE)
  r <- merge(r, nn, by = c("kirilim", "A", "gelir_dilim5", "k"), all.x = TRUE)
  r[is.na(N), N := 0L]
  r[, `:=`(gida = v, yuzde = 100 * oran, alt = pmax(0, 100 * (oran - 1.96 * se)), ust = pmin(100, 100 * (oran + 1.96 * se)), kategori = unname(etiket[[v]][k]))]
  r
}))
frek[, kucuk := kucuk_hucre(N)]
fwrite(frek, "Results/tuketim_sikligi_dagilim.csv")

ny <- d[f_kayit == 1 & FK070 >= 18 & FS050 %in% 1:2, .(yil, hane_kimlik, FB030, A = as.integer(HE090 == 2), FS050, FS060)]
ny[, sonuc := fcase(FS050 == 2, "No unmet need", FS060 == 1, "Cost", FS060 %in% c(3, 5), "System (distance, waiting time)", FS060 %in% c(2, 4, 6, 7, 8), "Personal (time, fear, wait-and-see, other)")]
des_ny <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = ny)
neden <- as.data.table(svyby(~sonuc, ~yil + A, des_ny, svymean))
neden_l <- melt(neden, id.vars = c("yil", "A"), measure.vars = patterns("^sonuc", "^se\\."), variable.name = "k", value.name = c("oran", "se"))
neden_l[, kategori := sort(unique(ny$sonuc))[as.integer(k)]]
neden_n <- ny[, .N, by = .(yil, A, kategori = sonuc)]
neden_l <- merge(neden_l, neden_n, by = c("yil", "A", "kategori"), all.x = TRUE)
neden_l[is.na(N), N := 0L]
neden_l[, `:=`(yuzde = 100 * oran, alt = pmax(0, 100 * (oran - 1.96 * se)), ust = 100 * (oran + 1.96 * se), kucuk = kucuk_hucre(N))]
neden_tum <- as.data.table(svyby(~sonuc, ~yil, des_ny, svymean))
fwrite(neden_l, "Results/karsilanmamis_ihtiyac_nedenleri_2023_2025.csv")
print(neden_l[kategori != "No unmet need"])

eurostat <- data.table(
  geo = rep(c("EU27_2020", "TR"), each = 4), yil = rep(2022:2025, 2),
  yuzde = c(8.3, 9.5, 8.5, 8.5, 41.5, 39.1, 39.3, 35.1),
  kaynak = "Eurostat ilc_mdes03 (unit = PC, hhcomp = TOTAL, rskpovth = TOTAL), last update 2026-10-20, retrieved 2026-10-25 via Eurostat dissemination API"
)
fwrite(eurostat, file.path(ddir, "figur_verileri", "eurostat_ilc_mdes03.csv"))
dogr_ek <- merge(eurostat[geo == "TR" & yil >= 2023], baglam[duzey == "kisi" & kirilim == "Total" & tr63_haric == FALSE, .(yil, bulunan = round(yuzde, 1))], by = "yil")
dogr <- rbind(dogr, dogr_ek[, .(kontrol = paste0(yil, " HE090 = 2 kisi payi (FK050) = Eurostat ilc_mdes03 TR (%)"), beklenen = as.character(yuzde), bulunan = as.character(bulunan), durum = fifelse(abs(yuzde - bulunan) < 0.05, "Tuttu", "Tutmadi"))])
fwrite(dogr, "Results/dogrulama_kontrolleri.csv")
print(dogr[(.N - 2):.N])
fig2a <- baglam[duzey == "kisi" & kirilim %in% c("Total", "gelir_dilim5") & tr63_haric == FALSE, .(yil, kategori, yuzde, alt, ust, N)]
fwrite(fig2a, file.path(ddir, "figur_verileri", "fig2a_he090_trend.csv"))

Sys.setenv(VECLIB_MAXIMUM_THREADS = "1", OMP_NUM_THREADS = "1")
plan(multisession, workers = 16)
obdir <- file.path(ddir, "onbellek")
dir.create(obdir, showWarnings = FALSE)

ns_bilesen <- function(x, df, ad) {
  B <- ns(x, df = df)
  L <- cbind(1, x)
  R <- B - L %*% qr.solve(L, B)
  sv <- svd(R)
  k <- df - 1
  Z <- sv$u[, seq_len(k), drop = FALSE] * sqrt(length(x))
  colnames(Z) <- paste0(ad, seq_len(k))
  as.data.table(Z)
}

irls <- function(X, y, w, b, tol = 1e-9, maxit = 30) {
  for (it in seq_len(maxit)) {
    mu <- plogis(drop(X %*% b))
    H <- crossprod(X, X * (w * mu * (1 - mu)))
    g <- crossprod(X, w * (y - mu))
    ok <- diag(H) > 1e-12
    st <- numeric(length(b))
    st[ok] <- tryCatch(solve(H[ok, ok, drop = FALSE], g[ok]), error = function(e) drop(MASS::ginv(H[ok, ok, drop = FALSE]) %*% g[ok]))
    b <- b + st
    if (max(abs(st)) < tol) break
  }
  attr(b, "iter") <- it
  b
}

mm_kur <- function(formul, dat, senaryo) {
  dv <- dat[, unique(c(all.vars(formul))), with = FALSE]
  mf <- model.frame(formul, dv, na.action = na.fail)
  tt <- terms(mf)
  X <- model.matrix(tt, mf)
  q <- qr(X)
  if (q$rank < ncol(X)) X <- X[, sort(q$pivot[seq_len(q$rank)]), drop = FALSE]
  y <- as.numeric(model.response(mf))
  xl <- .getXlevels(tt, mf)
  tx <- delete.response(tt)
  Xs <- lapply(senaryo, function(s) {
    nd <- copy(dv)
    for (v in names(s)) set(nd, j = v, value = rep(s[[v]], nrow(nd)))
    Z <- model.matrix(tx, model.frame(tx, nd, xlev = xl, na.action = na.fail))
    Z[, colnames(X), drop = FALSE]
  })
  deg <- which(Reduce(`|`, lapply(Xs, function(Z) colSums(abs(Z - X)) > 0)))
  list(X = X, y = y, Xd = lapply(Xs, function(Z) Z[, deg, drop = FALSE]), deg = deg, formul = formul)
}

p_hesap <- function(mus, w, grup = NULL) {
  out <- vapply(mus, function(mu) sum(w * mu) / sum(w), 0)
  names(out) <- paste0(names(mus), "|T")
  if (!is.null(grup)) {
    W <- rowsum(w, grup)
    for (s in names(mus)) {
      v <- rowsum(w * mus[[s]], grup) / W
      out <- c(out, setNames(drop(v), paste0(s, "|", rownames(v))))
    }
  }
  out
}

senaryo_mu <- function(m, b) {
  eta <- drop(m$X %*% b)
  Xdeg <- m$X[, m$deg, drop = FALSE]
  bd <- b[m$deg]
  list(eta = eta, mus = lapply(m$Xd, function(Z) plogis(eta + drop((Z - Xdeg) %*% bd))))
}

olc_cikar <- function(p, b, olcu, katsayi) {
  o <- olcu(p)
  if (length(katsayi)) o <- c(o, setNames(exp(b[katsayi]), paste0("OR_", katsayi)))
  o
}

gc_tahmin <- function(m, w, olcu, grup = NULL, katsayi = character(0), b0 = NULL) {
  if (is.null(b0)) b0 <- setNames(numeric(ncol(m$X)), colnames(m$X))
  b <- irls(m$X, m$y, w, b0)
  sm <- senaryo_mu(m, b)
  p <- p_hesap(sm$mus, w, grup)
  list(b = setNames(as.numeric(b), colnames(m$X)), iter = attr(b, "iter"), eta = sm$eta, mus = sm$mus, p = p, olc = olc_cikar(p, setNames(as.numeric(b), colnames(m$X)), olcu, katsayi))
}

var_toplam <- function(z, kume, tabaka = NULL) {
  z <- as.matrix(z)
  Tc <- rowsum(z, kume, reorder = FALSE)
  if (is.null(tabaka)) {
    nc <- nrow(Tc)
    Tc <- sweep(Tc, 2, colMeans(Tc))
    return(crossprod(Tc) * nc / (nc - 1))
  }
  th <- tabaka[!duplicated(kume)]
  V <- matrix(0, ncol(z), ncol(z))
  for (h in unique(th)) {
    Th <- Tc[th == h, , drop = FALSE]
    nh <- nrow(Th)
    if (nh > 1) V <- V + crossprod(sweep(Th, 2, colMeans(Th))) * nh / (nh - 1)
  }
  V
}

jakobyen <- function(f, p) {
  f0 <- f(p)
  J <- matrix(0, length(f0), length(p), dimnames = list(names(f0), names(p)))
  for (j in seq_along(p)) {
    h <- 1e-6 * max(1, abs(p[j]))
    pp <- p
    pp[j] <- pp[j] + h
    pm <- p
    pm[j] <- pm[j] - h
    J[, j] <- (f(pp) - f(pm)) / (2 * h)
  }
  J
}

gc_if <- function(m, w, r, olcu, grup = NULL, kume, tabaka = NULL, katsayi = character(0)) {
  X <- m$X
  mu <- plogis(r$eta)
  H <- crossprod(X, X * (w * mu * (1 - mu)))
  Hinv <- tryCatch(solve(H), error = function(e) MASS::ginv(H))
  U <- X * (w * (m$y - mu))
  Xdeg <- X[, m$deg, drop = FALSE]
  gruplar <- c(list(T = rep(TRUE, nrow(X))), if (is.null(grup)) list() else lapply(split(seq_len(nrow(X)), grup), function(ii) seq_len(nrow(X)) %in% ii))
  Zl <- list()
  for (s in names(r$mus)) {
    ms <- r$mus[[s]]
    a <- w * ms * (1 - ms)
    for (g in names(gruplar)) {
      ig <- gruplar[[g]]
      Wg <- sum(w[ig])
      ps <- sum(w[ig] * ms[ig]) / Wg
      D <- colSums(X[ig, , drop = FALSE] * a[ig]) / Wg
      D[m$deg] <- colSums(m$Xd[[s]][ig, , drop = FALSE] * a[ig]) / Wg
      Zl[[paste0(s, "|", g)]] <- ig * w * (ms - ps) / Wg + drop(U %*% (Hinv %*% D))
    }
  }
  Z <- do.call(cbind, Zl)
  Z <- Z[, names(r$p), drop = FALSE]
  J <- jakobyen(olcu, r$p)
  zt <- Z %*% t(J)
  if (length(katsayi)) {
    zb <- U %*% Hinv[, match(katsayi, colnames(X)), drop = FALSE]
    zb <- sweep(zb, 2, exp(r$b[katsayi]), `*`)
    colnames(zb) <- paste0("OR_", katsayi)
    zt <- cbind(zt, zb)
  }
  Vk <- var_toplam(zt, kume, tabaka)
  Vb <- var_toplam(zt, seq_len(nrow(X)))
  Vcoef <- Hinv %*% var_toplam(U, kume, tabaka) %*% Hinv
  dimnames(Vcoef) <- list(colnames(X), colnames(X))
  list(se = sqrt(diag(Vk)), se_kumesiz = sqrt(diag(Vb)), Vcoef = Vcoef, V = Vk)
}

hane_id <- sort(unique(hane_2025$hane_kimlik))
B_boot <- 2000L
n_hane <- length(hane_id)
set.seed(20251025)
M_boot <- matrix(0L, n_hane, B_boot)
for (b in seq_len(B_boot)) M_boot[, b] <- tabulate(sample.int(n_hane, n_hane - 1L, replace = TRUE), n_hane)
fac_hane <- rep(n_hane / (n_hane - 1), n_hane)
tab_hane <- hane_2025$HB031[match(hane_id, hane_2025$hane_kimlik)]
set.seed(20251026)
M_boot_tab <- matrix(0L, n_hane, B_boot)
fac_hane_tab <- numeric(n_hane)
for (h in unique(tab_hane)) {
  ih <- which(tab_hane == h)
  nh <- length(ih)
  fac_hane_tab[ih] <- nh / (nh - 1)
  for (b in seq_len(B_boot)) M_boot_tab[ih, b] <- tabulate(sample.int(nh, nh - 1L, replace = TRUE), nh)
}
set.seed(20251027)
M_boot_ek <- matrix(0L, n_hane, B_boot)
for (b in seq_len(B_boot)) M_boot_ek[, b] <- tabulate(sample.int(n_hane, n_hane - 1L, replace = TRUE), n_hane)
parca_sut <- split(seq_len(B_boot), cut(seq_len(B_boot), 16, labels = FALSE))
M_parca_ek <- lapply(parca_sut, function(cc) M_boot_ek[, cc, drop = FALSE])
M_parca <- lapply(parca_sut, function(cc) M_boot[, cc, drop = FALSE])
M_parca_tab <- lapply(parca_sut, function(cc) M_boot_tab[, cc, drop = FALSE])

boot_gc <- function(anahtar, m, w, idx, olcu, b0, grup = NULL, katsayi = character(0), Mp = M_parca, fac = fac_hane) {
  f <- file.path(obdir, paste0("boot_", anahtar, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  t0 <- Sys.time()
  Xdeg <- m$X[, m$deg, drop = FALSE]
  res <- future_map(Mp, function(Mc) {
    out <- NULL
    for (j in seq_len(ncol(Mc))) {
      wb <- w * fac[idx] * Mc[idx, j]
      b <- irls(m$X, m$y, wb, b0, tol = 1e-8)
      eta <- drop(m$X %*% b)
      bd <- b[m$deg]
      mus <- lapply(m$Xd, function(Z) plogis(eta + drop((Z - Xdeg) %*% bd)))
      p <- p_hesap(mus, wb, grup)
      o <- olc_cikar(p, setNames(as.numeric(b), colnames(m$X)), olcu, katsayi)
      out <- rbind(out, c(o, iter = attr(b, "iter")))
    }
    out
  }, .options = furrr_options(seed = NULL, packages = "data.table"))
  Bm <- do.call(rbind, res)
  attr(Bm, "sure_sn") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  saveRDS(Bm, f)
  Bm
}

boot_uzat <- function(anahtar, g, ix, olcu, Bm, grup = NULL, katsayi = character(0)) {
  Be <- boot_gc(paste0(anahtar, "_ek"), g$m, g$w, ix, olcu, g$r$b, grup = grup, katsayi = katsayi, Mp = M_parca_ek)
  out <- rbind(Bm, Be)
  attr(out, "sure_sn") <- attr(Bm, "sure_sn") + attr(Be, "sure_sn")
  out
}

gc_ozet <- function(r, s, Bm, olcek = 100, oran_olc = character(0)) {
  k <- names(r$olc)
  q <- function(x, pr) as.numeric(quantile(x, pr, na.rm = TRUE))
  B2 <- min(nrow(Bm), 2000)
  data.table(olcu = k, tahmin = r$olc[k], se_if = s$se[k], se_kumesiz = s$se_kumesiz[k],
             se_boot = apply(Bm[, k, drop = FALSE], 2, sd, na.rm = TRUE),
             alt = apply(Bm[, k, drop = FALSE], 2, q, pr = 0.025), ust = apply(Bm[, k, drop = FALSE], 2, q, pr = 0.975),
             alt_if = r$olc[k] - 1.96 * s$se[k], ust_if = r$olc[k] + 1.96 * s$se[k],
             alt500 = apply(Bm[1:500, k, drop = FALSE], 2, q, pr = 0.025), ust500 = apply(Bm[1:500, k, drop = FALSE], 2, q, pr = 0.975),
             alt1000 = apply(Bm[1:1000, k, drop = FALSE], 2, q, pr = 0.025), ust1000 = apply(Bm[1:1000, k, drop = FALSE], 2, q, pr = 0.975),
             alt2000 = apply(Bm[1:B2, k, drop = FALSE], 2, q, pr = 0.025), ust2000 = apply(Bm[1:B2, k, drop = FALSE], 2, q, pr = 0.975),
             B = nrow(Bm))
}

yetiskin_2025[, `:=`(f_cins = factor(FK090), f_egit = factor(egitim5), f_gelir = factor(gelir_dilim5), f_bolge = factor(HB030, levels = names(etiket$HB030)),
                     f_hb = factor(pmin(hane_buyuklugu, 7)), f_cocuk = factor(pmin(hane_0_17, 4)), f_yasli = factor(pmin(hane_65plus, 2)),
                     f_kronik = factor(FS020), f_gali = factor(FS030), f_medeni = factor(FB100), f_calisma = factor(calisma7), f_mulk = factor(HH020),
                     f_htip = factor(HB050), f_engel = factor(wg_engellilik), f_saglik = factor(FS010))]
yetiskin_2025 <- cbind(yetiskin_2025, ns_bilesen(yetiskin_2025$FK070, 3, "yas3_"), ns_bilesen(yetiskin_2025$FK070, 4, "yas4_"), ns_bilesen(yetiskin_2025$FK070, 5, "yas5_"))
idx_y <- match(yetiskin_2025$hane_kimlik, hane_id)
sen_A <- list(A0 = list(A = 0), A1 = list(A = 1))
olcu_A <- function(p) c(p0 = 100 * unname(p["A0|T"]), p1 = 100 * unname(p["A1|T"]), PD = 100 * unname(p["A1|T"] - p["A0|T"]), PR = unname(p["A1|T"] / p["A0|T"]))
gc_hizli <- function(formul, dat, w, senaryo, olcu, grup = NULL, katsayi = character(0), kume = dat$hane_kimlik, tabaka = NULL) {
  m <- mm_kur(formul, dat, senaryo)
  ww <- dat[[w]]
  r <- gc_tahmin(m, ww, olcu, grup, katsayi)
  s <- gc_if(m, ww, r, olcu, grup, kume = kume, tabaka = tabaka, katsayi = katsayi)
  list(m = m, r = r, s = s, w = ww, tab = data.table(olcu = names(r$olc), tahmin = r$olc, se = s$se[names(r$olc)], se_kumesiz = s$se_kumesiz[names(r$olc)]))
}
h2_y <- c("et_haftada_az", "meyve_gunluk_degil", "sebze_gunluk_degil")
yas_terim <- c(dogrusal = "FK070", df3 = "FK070 + yas3_1 + yas3_2", df4 = "FK070 + yas4_1 + yas4_2 + yas4_3", df5 = "FK070 + yas5_1 + yas5_2 + yas5_3 + yas5_4")
h2_cekirdek <- "f_cins + f_egit + f_gelir + f_bolge + f_hb + f_cocuk"
h2_veri <- function(v) yetiskin_2025[!is.na(get(v))]


mo_dosya <- "Results/model_ozetleri.txt"
cat("Model ozetleri (analiz.R)\n", format(Sys.time()), "\n", file = mo_dosya)
ozet_yaz <- function(baslik, nesne) {
  cat("\n\n==== ", baslik, " ====\n", file = mo_dosya, append = TRUE)
  cat(capture.output(print(nesne)), sep = "\n", file = mo_dosya, append = TRUE)
}
protokol <- list()
prot_ekle <- function(kontrol, olcut, sonuc, karar, model) protokol[[length(protokol) + 1]] <<- data.table(kontrol = kontrol, olcut = olcut, sonuc = sonuc, karar = karar, model = model)
sayilar <- list()
wald_p <- function(sg, terim) as.numeric(regTermTest(sg, as.formula(paste("~", terim)), method = "Wald")$p)
fm <- function(x, d = 2) formatC(x, format = "f", digits = d)

ff_h2 <- rbindlist(lapply(h2_y, function(v) {
  dt <- h2_veri(v)
  res <- rbindlist(lapply(names(yas_terim), function(k) {
    g <- gc_hizli(as.formula(paste(v, "~ A +", yas_terim[k], "+", h2_cekirdek)), dt, "FB030", sen_A, olcu_A)
    data.table(sonuc = v, yas = k, PD = g$r$olc[["PD"]], PR = g$r$olc[["PR"]], se_PD = g$s$se[["PD"]])
  }))
  des <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = dt)
  sg <- svyglm(as.formula(paste(v, "~ A +", yas_terim["df4"], "+", h2_cekirdek)), des, family = quasibinomial())
  res[, p_dogrusal_disi_df4 := wald_p(sg, "yas4_1 + yas4_2 + yas4_3")]
  res
}))
ff_h2[, fark_df4 := PD - PD[yas == "df4"], by = sonuc]
print(ff_h2)
fwrite(ff_h2, "Results/log/h2_fonksiyonel_bicim_yas.csv")
yas_karar_h2 <- ff_h2[, .(dogrusal = p_dogrusal_disi_df4[1] >= 0.10 & abs(fark_df4[yas == "dogrusal"]) < 0.2, df_degismez = all(abs(fark_df4[yas %in% c("df3", "df5")]) < 0.2)), by = sonuc]
print(yas_karar_h2)
h2_yas <- if (all(yas_karar_h2$dogrusal)) yas_terim["dogrusal"] else yas_terim["df4"]
for (v in h2_y) prot_ekle("2. Fonksiyonel bicim (yas)", "Dogrusal disi bilesen p >= 0,10 ve PD farki < 0,2 puan ise dogrusal; df 3/4/5 PD farki < 0,2 ise df 4",
                          paste0("p = ", formatC(ff_h2[sonuc == v, p_dogrusal_disi_df4[1]], format = "g", digits = 3), "; PD dogrusal - df4 = ", fm(ff_h2[sonuc == v & yas == "dogrusal", fark_df4], 3),
                                 "; df3 = ", fm(ff_h2[sonuc == v & yas == "df3", fark_df4], 3), "; df5 = ", fm(ff_h2[sonuc == v & yas == "df5", fark_df4], 3)),
                          if (h2_yas == yas_terim["df4"]) "Yas 4 df dogal spline" else "Yas dogrusal", paste0("H2 ", v))

h2_aday <- c(FS020 = "f_kronik", FS030 = "f_gali", FB100 = "f_medeni", FI010 = "f_calisma", HH020 = "f_mulk", HB050 = "f_htip", wg_engellilik = "f_engel", FS010 = "f_saglik")
h2_temel <- paste("A +", h2_yas, "+", h2_cekirdek)
aday_h2 <- rbindlist(lapply(h2_y, function(v) {
  dt <- h2_veri(v)
  des <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = dt)
  g0 <- gc_hizli(as.formula(paste(v, "~", h2_temel)), dt, "FB030", sen_A, olcu_A)
  rbindlist(lapply(names(h2_aday), function(a) {
    fa <- as.formula(paste(v, "~", h2_temel, "+", h2_aday[[a]]))
    g1 <- gc_hizli(fa, dt, "FB030", sen_A, olcu_A)
    sg <- svyglm(fa, des, family = quasibinomial())
    data.table(sonuc = v, aday = a, PD_once = g0$r$olc[["PD"]], PD_sonra = g1$r$olc[["PD"]], PR_once = g0$r$olc[["PR"]], PR_sonra = g1$r$olc[["PR"]],
               se_once = g0$s$se[["PD"]], se_sonra = g1$s$se[["PD"]], wald_p = wald_p(sg, h2_aday[[a]]))
  }))
}))
aday_h2[, `:=`(PD_degisim = PD_sonra - PD_once, PR_goreli_degisim = 100 * (PR_sonra - PR_once) / PR_once, se_degisim_yuzde = 100 * (se_sonra - se_once) / se_once)]
aday_h2[, karar := fcase(abs(PD_degisim) >= 0.5 | abs(PR_goreli_degisim) >= 10, "Girer (tahmin degisimi)",
                         wald_p < 0.05 & se_degisim_yuzde < -5, "Girer (kesinlik)",
                         default = "Girmez")]
print(aday_h2)
fwrite(aday_h2, "Results/kovaryat_secimi_H2.csv")
h2_secilen <- unique(aday_h2[karar != "Girmez", aday])
cat("H2 secilen adaylar:", h2_secilen, "\n")

agirlikli_ondalik <- function(x, w, k = 10) {
  o <- order(x)
  cw <- cumsum(w[o]) / sum(w)
  g <- integer(length(x))
  g[o] <- pmin(k, floor(cw * k - 1e-12) + 1L)
  g
}
al_test <- function(y, phat, w, kume, etiket_ad) {
  dec <- agirlikli_ondalik(phat, w)
  dd <- data.frame(r = y - phat, dec = factor(dec), kume = kume, w = w, y = y, phat = phat)
  des <- svydesign(ids = ~kume, weights = ~w, data = dd)
  sg <- svyglm(r ~ dec - 1, des)
  tt <- regTermTest(sg, ~dec, method = "Wald")
  kal <- as.data.table(dd)[, .(gozlenen = weighted.mean(y, w), tahmin = weighted.mean(phat, w), n = .N), keyby = dec]
  kal[, model := etiket_ad]
  list(F = as.numeric(tt$Ftest), df = tt$df, ddf = tt$ddf, p = as.numeric(tt$p), kalibrasyon = kal)
}
artik_grafik <- function(y, phat, x, w, dosya, xad) {
  png(dosya, width = 2400, height = 1100, res = 200)
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
  for (k in 1:2) {
    z <- if (k == 1) phat else x
    b <- agirlikli_ondalik(z, w, 40)
    r <- as.data.table(data.frame(b, z, res = y - phat, w))[, .(z = weighted.mean(z, w), r = weighted.mean(res, w), se = sqrt(weighted.mean(res^2, w) / .N)), keyby = b]
    plot(r$z, r$r, pch = 16, cex = 0.7, xlab = if (k == 1) "Predicted probability" else xad, ylab = "Mean residual (binned)", ylim = range(c(r$r - 2 * r$se, r$r + 2 * r$se)))
    lines(r$z, 2 * r$se, col = "grey50")
    lines(r$z, -2 * r$se, col = "grey50")
    abline(h = 0, lty = 2)
  }
  par(op)
  dev.off()
}
ortusme <- function(dt, w, gruplar, tabaka_v, esik = 20) {
  dt <- copy(dt)
  dt[, .grp := do.call(paste, c(.SD, sep = "_")), .SDcols = gruplar]
  dt[, .tab := do.call(paste, c(.SD, sep = "_")), .SDcols = tabaka_v]
  hucre <- dcast(dt[, .N, by = .(.tab, .grp)], .tab ~ .grp, value.var = "N", fill = 0)
  gl <- setdiff(names(hucre), ".tab")
  hucre[, en_az := do.call(pmin, .SD), .SDcols = gl]
  ww <- dt[, .(ww = sum(get(w))), by = .tab]
  hucre <- merge(hucre, ww, by = ".tab")
  list(tabaka_sayisi = nrow(hucre), seyrek_tabaka = hucre[en_az < esik, .N], bos_tabaka = hucre[en_az == 0, .N],
       seyrek_pay = 100 * hucre[en_az < esik, sum(ww)] / hucre[, sum(ww)], ortusen = hucre[en_az >= esik, .tab], tablo = hucre,
       satir_ortusen = dt$.tab %in% hucre[en_az >= esik, .tab])
}

h2_son <- paste(c(h2_temel, if (length(h2_secilen)) h2_aday[h2_secilen]), collapse = " + ")
h2_min <- paste("A +", h2_yas, "+ f_cins + f_gelir")
esnek_yap <- function(sag) paste("A * (", sub("^A \\+ ", "", sag), ")")
h2_modeller <- c(minimal = h2_min, cekirdek = h2_temel, nihai = h2_son, nihai_esnek = esnek_yap(h2_son))
yetiskin_2025[, egitim3 := fcase(egitim5 <= 2, 1L, egitim5 <= 4, 2L, egitim5 == 5, 3L)]
olcu_A_gelir <- function(p) {
  o <- olcu_A(p)
  for (q in 1:5) {
    p0 <- unname(p[paste0("A0|", q)])
    p1 <- unname(p[paste0("A1|", q)])
    o <- c(o, setNames(c(100 * p0, 100 * p1, 100 * (p1 - p0), p1 / p0), paste0(c("p0_Q", "p1_Q", "PD_Q", "PR_Q"), q)))
  }
  o
}
bk_metin <- function(oz, k) paste0(k, " sinirlari: 500 [", fm(oz[olcu == k, alt500]), ", ", fm(oz[olcu == k, ust500]), "], 1000 [", fm(oz[olcu == k, alt1000]), ", ", fm(oz[olcu == k, ust1000]), "], 2000 [",
                                    fm(oz[olcu == k, alt2000]), ", ", fm(oz[olcu == k, ust2000]), "], ", oz$B[1], " [", fm(oz[olcu == k, alt]), ", ", fm(oz[olcu == k, ust]), "]")
bk_karar <- function(oz, k) {
  d12 <- oz[olcu == k, max(abs(c(alt1000 - alt2000, ust1000 - ust2000)))]
  d24 <- oz[olcu == k, max(abs(c(alt2000 - alt, ust2000 - ust)))]
  if (d12 <= 0.1) return(paste0("1.000-2.000 farki ", fm(d12), " <= 0,1: yeterli; ana tahminde yine de ", oz$B[1], " tekrar"))
  paste0("1.000-2.000 farki ", fm(d12), " > 0,1: tekrar ", oz$B[1], "'e cikarildi; 2.000-", oz$B[1], " farki ", fm(d24), if (d24 <= 0.1) " (yeterli)" else " (hala > 0,1; Monte Carlo hatasi duzeyinde, ek artirma yapilmadi)")
}
kaba_ortusme <- list()
h2_sonuc <- list()
h2_tab <- list()
h2_boot <- list()
h2_ana <- character(0)
for (v in h2_y) {
  dt <- h2_veri(v)
  ix <- idx_y[!is.na(yetiskin_2025[[v]])]
  des <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = dt)
  gl <- list()
  for (mk in names(h2_modeller)) {
    fo <- as.formula(paste(v, "~", h2_modeller[[mk]]))
    es <- mk == "nihai_esnek"
    oo <- if (es) olcu_A_gelir else olcu_A
    g <- gc_hizli(fo, dt, "FB030", sen_A, oo, grup = if (es) dt$gelir_dilim5 else NULL)
    Bm <- boot_gc(paste0("h2_", v, "_", mk), g$m, g$w, ix, oo, g$r$b, grup = if (es) dt$gelir_dilim5 else NULL)
    h2_tab[[paste(v, mk)]] <- gc_ozet(g$r, g$s, Bm)[, `:=`(sonuc = v, model = mk, n = nrow(dt), olay = sum(dt[[v]]), npar = ncol(g$m$X), sure_boot = attr(Bm, "sure_sn"))]
    gl[[mk]] <- list(g = g, B = Bm)
  }
  lpm <- svyglm(as.formula(paste(v, "~", h2_son)), des, family = gaussian())
  baglanti <- c(lojistik = gl$nihai$g$r$olc[["PD"]], dogrusal_olasilik = 100 * unname(coef(lpm)["A"]), esnek_lojistik = gl$nihai_esnek$g$r$olc[["PD"]])
  fark <- max(baglanti) - min(baglanti)
  em <- sapply(c(gelir = "f_gelir", bolge = "f_bolge", cinsiyet = "f_cins"), function(z) {
    sgz <- svyglm(as.formula(paste(v, "~", h2_son, "+ A:", z)), des, family = quasibinomial())
    wald_p(sgz, paste0("A:", z))
  })
  ana <- if (any(em < 0.01)) "nihai_esnek" else "nihai"
  h2_ana[v] <- ana
  h2_tab[[paste(v, "baglanti")]] <- data.table(olcu = names(baglanti), tahmin = baglanti, sonuc = v, model = "baglanti",
                                               se_if = c(gl$nihai$g$s$se[["PD"]], 100 * unname(SE(lpm)["A"]), gl$nihai_esnek$g$s$se[["PD"]]))
  prot_ekle("4. Baglanti fonksiyonu ve model bicimi", "Ilk kural: uc tahmin farki < 0,5 puan ise ana model korunur. Inceltilmis kural (sonuclar goruldukten sonra, yazar karari): adim 6 taramasinda p < 0,01 heterojenlik varsa esnek model ana model; yoksa ana etkili model ana model, esnek model ve LPM duyarlilik",
            paste0("PD: lojistik ", fm(baglanti[1]), ", LPM ", fm(baglanti[2]), ", esnek ", fm(baglanti[3]), "; en buyuk fark ", fm(fark), "; adim 6 en kucuk p ", formatC(min(em), format = "g", digits = 3)),
            if (ana == "nihai") "Heterojenlik yok: ana etkili lojistik + g-comp ana model" else "Heterojenlik var (p < 0,01): esnek lojistik + g-comp ana model; ana etkili model ve LPM duyarlilik", paste0("H2 ", v))
  gl[[ana]]$B <- boot_uzat(paste0("h2_", v, "_", ana), gl[[ana]]$g, ix, if (ana == "nihai_esnek") olcu_A_gelir else olcu_A, gl[[ana]]$B, grup = if (ana == "nihai_esnek") dt$gelir_dilim5 else NULL)
  h2_tab[[paste(v, ana)]] <- gc_ozet(gl[[ana]]$g$r, gl[[ana]]$g$s, gl[[ana]]$B)[, `:=`(sonuc = v, model = ana, n = nrow(dt), olay = sum(dt[[v]]), npar = ncol(gl[[ana]]$g$m$X), sure_boot = attr(gl[[ana]]$B, "sure_sn"))]
  g <- gl[[ana]]$g
  h2_sonuc[[v]] <- g
  h2_boot[[v]] <- gl[[ana]]$B
  h2_sonuc[[paste0(v, "_esnek")]] <- gl$nihai_esnek$g
  h2_boot[[paste0(v, "_esnek")]] <- gl$nihai_esnek$B
  fo <- as.formula(paste(v, "~", h2_modeller[[ana]]))
  sg <- svyglm(fo, des, family = quasibinomial())
  ozet_yaz(paste("H2 ana lojistik model:", v, "(", ana, ")"), summary(sg))
  ozet_yaz(paste("H2 dogrusal olasilik modeli:", v), summary(lpm))
  ort <- ortusme(dt, "FB030", "A", c("gelir_dilim5", "HB030", "egitim5"))
  ort3 <- ortusme(dt, "FB030", "A", c("gelir_dilim5", "HB030", "egitim3"))
  ort_gb <- ortusme(dt, "FB030", "A", c("gelir_dilim5", "HB030"))
  ort_ge <- ortusme(dt, "FB030", "A", c("gelir_dilim5", "egitim3"))
  kaba_ortusme[[paste("H2", v)]] <- data.table(model = paste("H2", v), tabakalama = c("gelir x bolge", "gelir x egitim (3)", "gelir x bolge x egitim (3)", "gelir x bolge x egitim (5)"),
                                                 tabaka = c(ort_gb$tabaka_sayisi, ort_ge$tabaka_sayisi, ort3$tabaka_sayisi, ort$tabaka_sayisi), seyrek_tabaka = c(ort_gb$seyrek_tabaka, ort_ge$seyrek_tabaka, ort3$seyrek_tabaka, ort$seyrek_tabaka),
                                                 seyrek_nufus_payi = c(ort_gb$seyrek_pay, ort_ge$seyrek_pay, ort3$seyrek_pay, ort$seyrek_pay))
  pd_ort <- g$r$olc[["PD"]]
  if (ort3$seyrek_tabaka > 0) {
    go <- gc_hizli(fo, dt[ort3$satir_ortusen], "FB030", sen_A, olcu_A)
    pd_ort <- go$r$olc[["PD"]]
    h2_tab[[paste(v, "ortusen")]] <- data.table(olcu = names(go$r$olc), tahmin = go$r$olc, se_if = go$s$se[names(go$r$olc)], sonuc = v, model = "ortusen_tabakalar", n = sum(ort3$satir_ortusen))
  }
  prot_ekle("3. Ortusme ve pozitiflik", "A gruplarinda gelir x bolge x egitim tabakasinda agirliksiz n < 20",
            paste0("Kaba kontroller: gelir x bolge ", ort_gb$tabaka_sayisi, " tabakanin ", ort_gb$seyrek_tabaka, "'i seyrek (nufusun %", fm(ort_gb$seyrek_pay, 1), "); gelir x egitim (3) ", ort_ge$tabaka_sayisi, " tabakanin ", ort_ge$seyrek_tabaka, "'i (%", fm(ort_ge$seyrek_pay, 1),
                   "). Ince: 5 egitim grubuyla ", ort$tabaka_sayisi, " tabakanin ", ort$seyrek_tabaka, "'i (%", fm(ort$seyrek_pay, 1), "); egitim 3 grupla ", ort3$tabaka_sayisi, " tabakanin ", ort3$seyrek_tabaka,
                   "'i (%", fm(ort3$seyrek_pay, 1), "); ortusen ince tabakalarla PD ", fm(pd_ort), " (tum nufus ", fm(g$r$olc[["PD"]]), ")"),
            "Ana tahmin tum nufusa standardize; ortusen tabakalarla sinirli tahmin duyarlilik olarak verildi", paste0("H2 ", v))
  for (mk in unique(c("nihai", ana))) {
    gg <- gl[[mk]]$g
    phat <- plogis(gg$r$eta)
    al <- al_test(dt[[v]], phat, gg$w, dt$hane_kimlik, paste0("H2 ", v, " ", mk))
    fwrite(al$kalibrasyon, paste0("Results/log/kalibrasyon_h2_", v, "_", mk, ".csv"))
    artik_grafik(dt[[v]], phat, dt$FK070, gg$w, paste0("Results/log/artik_h2_", v, "_", mk, ".png"), "Age (FK070)")
    prot_ekle("5. Uyum iyiligi", "Archer-Lemeshow F (anket, onda birlik gruplar) ve kalibrasyon", paste0(mk, ": F = ", fm(al$F), " (", al$df, ", ", fm(al$ddf, 0), "), p = ", formatC(al$p, format = "g", digits = 3),
              "; ondaliklarda gozlenen - tahmin en buyuk mutlak fark ", fm(100 * max(abs(al$kalibrasyon$gozlenen - al$kalibrasyon$tahmin))), " puan"),
              if (al$p >= 0.05) "Uyum yeterli" else if (mk == ana) "Test reddetti; ana model zaten esnek model (adim 4), kalibrasyon farki raporlandi" else "Test reddetti; adim 4 geregi esnek model kullanildi", paste0("H2 ", v))
  }
  prot_ekle("6. Etki degisimi taramasi (kesifsel)", "A x gelir, A x bolge, A x cinsiyet tasarim Wald (ana etkili nihai modele eklenerek); p < 0,01 ise tabakali tahmin ek tabloya",
            paste0("p: gelir ", formatC(em[1], format = "g", digits = 3), ", bolge ", formatC(em[2], format = "g", digits = 3), ", cinsiyet ", formatC(em[3], format = "g", digits = 3)),
            paste0("Tabakali tahmin: ", if (any(em < 0.01)) paste(names(em)[em < 0.01], collapse = ", ") else "yok", "; gelir dilimine gore tahmin her durumda esnek modelden verildi"), paste0("H2 ", v))
  h2_tab[[paste(v, "em")]] <- data.table(olcu = paste0("EM_p_", names(em)), tahmin = em, sonuc = v, model = "em")
  ne <- sum(dt[[v]])
  npar <- ncol(g$m$X)
  cs <- summary(sg)$coefficients
  prot_ekle("7. Ayrisma ve seyreklik", "Olay/parametre >= 10; SE > 5 x |katsayi| veya SE > 5 olan parametre",
            paste0("olay ", ne, ", olay olmayan ", nrow(dt) - ne, ", parametre ", npar, ", oran ", fm(min(ne, nrow(dt) - ne) / npar, 1), "; SE > 5 olan parametre ", sum(cs[, 2] > 5),
                   "; SE > 5|b| olan ", sum(cs[, 2] > 5 * abs(cs[, 1])), " (en buyuk SE ", fm(max(cs[, 2])), ")"),
            "Ayrisma yok, model kucultulmedi", paste0("H2 ", v))
  deff <- (g$s$se[["PD"]] / g$s$se_kumesiz[["PD"]])^2
  prot_ekle("11. Kumeleme", "Hane kumelemesinin tasarim etkisi", paste0("PD SE kumeli ", fm(g$s$se[["PD"]], 3), ", kumesiz ", fm(g$s$se_kumesiz[["PD"]], 3), ", tasarim etkisi ", fm(deff)), "Hane kumelemesi korunur", paste0("H2 ", v))
  oz <- h2_tab[[paste(v, ana)]]
  prot_ekle("12. Bootstrap yakinsamasi", "1.000 ve 2.000 tekrarda sinirlar 0,1 puandan az oynamali; oynarsa tekrar artirilir", bk_metin(oz, "PD"), bk_karar(oz, "PD"), paste0("H2 ", v))
}
h2_tab <- rbindlist(h2_tab, fill = TRUE)
fwrite(h2_tab, "Results/H2_gcomp_sonuclari.csv")
fwrite(rbindlist(protokol), "Results/model_protokolu.csv")
print(h2_tab[model %in% names(h2_modeller) & olcu %in% c("PD", "PR"), .(sonuc, model, olcu, tahmin, alt, ust, se_if, se_boot, npar)])

ord_y <- c(et_haftada_az = "et_tavuk_balik_siklik", meyve_gunluk_degil = "meyve_siklik", sebze_gunluk_degil = "sebze_siklik")
ord_sag <- sub("^A \\+ ", "", h2_son)
ord_tab <- list()
po_tab <- list()
for (v in names(ord_y)) {
  x <- ord_y[[v]]
  dt <- yetiskin_2025[if (x == "et_tavuk_balik_siklik") get(x) >= 1 else TRUE]
  mx <- max(dt[[x]])
  mn <- min(dt[[x]])
  dt[, ters := mx - get(x)]
  dt[, ters_f := factor(ters, ordered = TRUE)]
  des <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = dt)
  t0 <- Sys.time()
  so <- svyolr(as.formula(paste("ters_f ~ A +", ord_sag)), des)
  ozet_yaz(paste("H2 sirali lojistik (svyolr), daha seyrek tuketim yonu:", x), summary(so))
  b <- coef(so)["A"]
  se <- sqrt(vcov(so)["A", "A"])
  ord_tab[[v]] <- data.table(sonuc = v, siklik = x, OR = exp(b), alt = exp(b - 1.96 * se), ust = exp(b + 1.96 * se), n = nrow(dt), kategori = mx - mn + 1, sure_sn = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  po <- rbindlist(lapply(seq_len(mx - mn), function(k) {
    dt[, ikili := as.integer(ters >= k)]
    desk <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = dt)
    sk <- svyglm(as.formula(paste("ikili ~ A +", ord_sag)), desk, family = quasibinomial())
    data.table(sonuc = v, kesme = k, kesme_ad = paste0("<= ", etiket[[x]][as.character(mx - k)]), OR = exp(coef(sk)["A"]), alt = exp(coef(sk)["A"] - 1.96 * SE(sk)["A"]), ust = exp(coef(sk)["A"] + 1.96 * SE(sk)["A"]), olay = sum(dt$ikili))
  }))
  po_tab[[v]] <- po
  oran <- max(po$OR) / min(po$OR)
  ortusmez <- max(po$alt) > min(po$ust)
  po_karar <- if (oran > 1.3 || ortusmez) "Orantili odds bozuk: kesme noktasi bazli ikili OR'ler ana raporlama, svyolr ekte" else "Orantili odds varsayimi kabul edilebilir: svyolr OR raporlanir"
  ord_tab[[v]][, `:=`(po_oran = oran, po_ortusmez = ortusmez, po_karar = po_karar)]
  prot_ekle("9. Sirali modeller (orantili odds)", "Kesme noktasi bazli ikili lojistiklerde A OR'leri arasinda > %30 fark veya ortusmeyen GA varsa kismi orantili / ikili raporlama",
            paste0("Kesme OR'leri ", paste(fm(po$OR), collapse = ", "), "; en buyuk / en kucuk = ", fm(oran), if (ortusmez) "; GA'lar ortusmuyor" else "; GA'lar ortusuyor"), po_karar, paste0("H2 ", x))
}
ord_tab <- rbindlist(ord_tab)
po_tab <- rbindlist(po_tab)
fwrite(ord_tab, "Results/H2_sirali_model.csv")
fwrite(po_tab, "Results/H2_orantili_odds_kontrol.csv")
print(ord_tab)
print(po_tab)

ana_kol <- function(v) h2_boot[[v]]
karsilastir <- function(a, b, olcu) {
  da <- ana_kol(a)[, olcu]
  db <- ana_kol(b)[, olcu]
  ta <- h2_sonuc[[a]]$r$olc[[olcu]]
  tb <- h2_sonuc[[b]]$r$olc[[olcu]]
  fark <- da - db
  oran <- da / db
  data.table(karsilastirma = paste(a, "vs", b), olcu = olcu, fark = ta - tb, fark_alt = quantile(fark, 0.025), fark_ust = quantile(fark, 0.975),
             oran = ta / tb, oran_alt = quantile(oran, 0.025), oran_ust = quantile(oran, 0.975), p_boot = 2 * min(mean(fark <= 0), mean(fark >= 0)))
}
gida_kars <- rbindlist(lapply(list(c("et_haftada_az", "meyve_gunluk_degil"), c("et_haftada_az", "sebze_gunluk_degil"), c("meyve_gunluk_degil", "sebze_gunluk_degil")), function(z) rbind(karsilastir(z[1], z[2], "PR"), karsilastir(z[1], z[2], "PD"))))
fwrite(gida_kars, "Results/H2_gida_gruplari_karsilastirma.csv")
print(gida_kars)

h2_gelir <- rbindlist(lapply(h2_y, function(v) {
  g <- h2_sonuc[[paste0(v, "_esnek")]]
  Bm <- h2_boot[[paste0(v, "_esnek")]]
  rbindlist(lapply(c("T", 1:5), function(q) {
    sfx <- if (q == "T") "" else paste0("_Q", q)
    k <- paste0(c("p0", "p1", "PD", "PR"), sfx)
    data.table(sonuc = v, gelir = q, p0 = g$r$olc[[k[1]]], p1 = g$r$olc[[k[2]]], PD = g$r$olc[[k[3]]], PD_alt = quantile(Bm[, k[3]], 0.025), PD_ust = quantile(Bm[, k[3]], 0.975),
               PR = g$r$olc[[k[4]]], PR_alt = quantile(Bm[, k[4]], 0.025), PR_ust = quantile(Bm[, k[4]], 0.975),
               n_A0 = if (q == "T") yetiskin_2025[!is.na(get(v)) & A == 0, .N] else yetiskin_2025[!is.na(get(v)) & A == 0 & gelir_dilim5 == as.numeric(q), .N],
               n_A1 = if (q == "T") yetiskin_2025[!is.na(get(v)) & A == 1, .N] else yetiskin_2025[!is.na(get(v)) & A == 1 & gelir_dilim5 == as.numeric(q), .N])
  }))
}))
h2_gelir[, kucuk := kucuk_hucre(pmin(n_A0, n_A1))]
fwrite(h2_gelir, "Results/H2_gelir_dilimine_gore.csv")
fwrite(h2_gelir, file.path(ddir, "figur_verileri", "fig2c_standardize_fark_gelir.csv"))
fwrite(frek[kirilim == "A"], file.path(ddir, "figur_verileri", "fig2b_tuketim_sikligi_A.csv"))
print(h2_gelir)

hane_m[, `:=`(f_gelir = factor(gelir_dilim5), f_nyet = factor(pmin(n_yetiskin, 5)), f_cocuk = factor(pmin(hane_0_17, 4)), f_yasli = factor(pmin(hane_65plus, 2)),
              f_rcins = factor(reis_cinsiyet), f_egmax = factor(egitim_max), f_bolge = factor(HB030, levels = names(etiket$HB030)), f_ngali = factor(pmin(n_gali, 3)),
              f_ncal = factor(pmin(n_calisan, 3)), f_mulk = factor(HH020), f_htip = factor(HB050), f_nengel = factor(pmin(n_engelli, 2)), f_nkotu = factor(pmin(n_kotu_saglik, 2)),
              f_rmedeni = factor(fifelse(is.na(reis_medeni), 1, reis_medeni)), egitim3 = fcase(egitim_max <= 2, 1L, egitim_max <= 4, 2L, egitim_max == 5, 3L))]
hane_m <- cbind(hane_m, ns_bilesen(hane_m$reis_yas, 3, "ryas3_"), ns_bilesen(hane_m$reis_yas, 4, "ryas4_"), ns_bilesen(hane_m$reis_yas, 5, "ryas5_"))
idx_h <- match(hane_m$hane_kimlik, hane_id)
sen_AC <- function(cv) setNames(list(setNames(list(0, 0), c("A", cv)), setNames(list(0, 1), c("A", cv)), setNames(list(1, 0), c("A", cv)), setNames(list(1, 1), c("A", cv)), list(A = 0), list(A = 1)),
                                 c("s00", "s01", "s10", "s11", "A0", "A1"))
olcu_AC <- function(p) {
  p00 <- unname(p["s00|T"])
  p01 <- unname(p["s01|T"])
  p10 <- unname(p["s10|T"])
  p11 <- unname(p["s11|T"])
  a0 <- unname(p["A0|T"])
  a1 <- unname(p["A1|T"])
  D <- p11 - p10 - p01 + p00
  c(p00 = 100 * p00, p01 = 100 * p01, p10 = 100 * p10, p11 = 100 * p11, Delta = 100 * D, RERI = D / p00, AP = D / p11, PR10 = p10 / p00, PR01 = p01 / p00, PR11 = p11 / p00,
    PD_C0 = 100 * (p10 - p00), PD_C1 = 100 * (p11 - p01), beklenti11 = 100 * (p10 + p01 - p00), pA0 = 100 * a0, pA1 = 100 * a1, PD_A = 100 * (a1 - a0), PR_A = a1 / a0)
}
ryas_terim <- c(dogrusal = "reis_yas", df3 = "reis_yas + ryas3_1 + ryas3_2", df4 = "reis_yas + ryas4_1 + ryas4_2 + ryas4_3", df5 = "reis_yas + ryas5_1 + ryas5_2 + ryas5_3 + ryas5_4")
h1_cekirdek <- "f_gelir + f_nyet + f_cocuk + f_yasli + f_rcins + f_egmax + f_bolge"
h1_ihtiyac <- "f_ngali"
des_hm <- svydesign(ids = ~1, weights = ~HB040, data = hane_m)
ff_h1 <- rbindlist(lapply(names(ryas_terim), function(k) {
  g <- gc_hizli(as.formula(paste("Y_saglik_yuk ~ A * C_hane +", ryas_terim[k], "+", h1_cekirdek, "+", h1_ihtiyac)), hane_m, "HB040", sen_AC("C_hane"), olcu_AC, katsayi = "A:C_hane", kume = seq_len(nrow(hane_m)))
  data.table(yas = k, Delta = g$r$olc[["Delta"]], PR_A = g$r$olc[["PR_A"]], se_Delta = g$s$se[["Delta"]])
}))
sg_ff <- svyglm(as.formula(paste("Y_saglik_yuk ~ A * C_hane +", ryas_terim["df4"], "+", h1_cekirdek, "+", h1_ihtiyac)), des_hm, family = quasibinomial())
ff_h1[, `:=`(p_dogrusal_disi_df4 = wald_p(sg_ff, "ryas4_1 + ryas4_2 + ryas4_3"), fark_df4 = Delta - Delta[yas == "df4"])]
print(ff_h1)
fwrite(ff_h1, "Results/log/h1_fonksiyonel_bicim_reis_yas.csv")
h1_yas <- if (ff_h1$p_dogrusal_disi_df4[1] >= 0.10 && abs(ff_h1[yas == "dogrusal", fark_df4]) < 0.2) ryas_terim["dogrusal"] else ryas_terim["df4"]
prot_ekle("2. Fonksiyonel bicim (reis yasi)", "Dogrusal disi bilesen p >= 0,10 ve Delta farki < 0,2 puan ise dogrusal; df 3/4/5 Delta farki < 0,2 ise df 4",
          paste0("p = ", formatC(ff_h1$p_dogrusal_disi_df4[1], format = "g", digits = 3), "; Delta dogrusal - df4 = ", fm(ff_h1[yas == "dogrusal", fark_df4], 3), "; df3 = ", fm(ff_h1[yas == "df3", fark_df4], 3), "; df5 = ", fm(ff_h1[yas == "df5", fark_df4], 3)),
          if (h1_yas == ryas_terim["df4"]) "Reis yasi 4 df dogal spline" else "Reis yasi dogrusal", "H1 hane")

h1_temel <- paste("A * C_hane +", h1_yas, "+", h1_cekirdek, "+", h1_ihtiyac)
h1_aday <- c(n_calisan = "f_ncal", HH020 = "f_mulk", HB050 = "f_htip", FB100_reis = "f_rmedeni", wg_engellilik_sayi = "f_nengel", FS010_kotu_sayi = "f_nkotu")
g_h1_0 <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", h1_temel)), hane_m, "HB040", sen_AC("C_hane"), olcu_AC, katsayi = "A:C_hane", kume = seq_len(nrow(hane_m)))
aday_h1 <- rbindlist(lapply(names(h1_aday), function(a) {
  fa <- as.formula(paste("Y_saglik_yuk ~", h1_temel, "+", h1_aday[[a]]))
  g1 <- gc_hizli(fa, hane_m, "HB040", sen_AC("C_hane"), olcu_AC, katsayi = "A:C_hane", kume = seq_len(nrow(hane_m)))
  sg <- svyglm(fa, des_hm, family = quasibinomial())
  data.table(model = "H1 hane", aday = a, Delta_once = g_h1_0$r$olc[["Delta"]], Delta_sonra = g1$r$olc[["Delta"]], PR_once = g_h1_0$r$olc[["PR_A"]], PR_sonra = g1$r$olc[["PR_A"]],
             se_once = g_h1_0$s$se[["Delta"]], se_sonra = g1$s$se[["Delta"]], wald_p = wald_p(sg, h1_aday[[a]]))
}))
aday_h1[, `:=`(Delta_degisim = Delta_sonra - Delta_once, PR_goreli_degisim = 100 * (PR_sonra - PR_once) / PR_once, se_degisim_yuzde = 100 * (se_sonra - se_once) / se_once)]
aday_h1[, karar := fcase(abs(Delta_degisim) >= 0.25 | abs(PR_goreli_degisim) >= 10, "Girer (tahmin degisimi)", wald_p < 0.05 & se_degisim_yuzde < -5, "Girer (kesinlik)", default = "Girmez")]
print(aday_h1)
h1_secilen <- aday_h1[karar != "Girmez", aday]
h1_son <- paste(c(h1_temel, h1_aday[h1_secilen]), collapse = " + ")
g_h1_son <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", h1_son)), hane_m, "HB040", sen_AC("C_hane"), olcu_AC, katsayi = "A:C_hane", kume = seq_len(nrow(hane_m)))
aday_h1_birlikte <- data.table(model = "H1 hane", aday = paste(h1_secilen, collapse = " + "), Delta_once = g_h1_0$r$olc[["Delta"]], Delta_sonra = g_h1_son$r$olc[["Delta"]], PR_once = g_h1_0$r$olc[["PR_A"]], PR_sonra = g_h1_son$r$olc[["PR_A"]],
                               se_once = g_h1_0$s$se[["Delta"]], se_sonra = g_h1_son$s$se[["Delta"]], wald_p = NA_real_, karar = "Birlikte ekleme (secilenler tutuldu)")
fwrite(rbind(aday_h1, aday_h1_birlikte, fill = TRUE), "Results/kovaryat_secimi_H1_hane.csv")
print(g_h1_son$tab)

gvif_kontrol <- function(sg, model_ad) {
  v <- tryCatch(car::vif(sg, type = "terms"), error = function(e) tryCatch(car::vif(sg), error = function(e2) NULL))
  if (is.null(v)) return(data.table(model = model_ad, terim = NA_character_, gvif_olcekli = NA_real_))
  if (is.matrix(v)) data.table(model = model_ad, terim = rownames(v), gvif_olcekli = v[, ncol(v)]) else data.table(model = model_ad, terim = names(v), gvif_olcekli = sqrt(v))
}
gvif_tab <- list()
for (v in h2_y) {
  dt <- h2_veri(v)
  des <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = dt)
  gvif_tab[[v]] <- gvif_kontrol(svyglm(as.formula(paste(v, "~", sub("A \\+ FK070 \\+ yas4_1 \\+ yas4_2 \\+ yas4_3", "A + FK070", h2_son))), des, family = quasibinomial()), paste("H2", v))
}

ana_etki_sag <- function(sag, ac = "A \\* C_hane \\+ ") sub(ac, "", sag)
h1_min <- paste("A * C_hane +", h1_yas, "+ f_rcins + f_gelir")
h1_modeller <- c(minimal = h1_min, cekirdek = paste("A * C_hane +", h1_yas, "+", h1_cekirdek), cekirdek_ihtiyac = h1_temel, nihai = h1_son, nihai_esnek = paste("A * C_hane + A * (", ana_etki_sag(h1_son), ")"),
                 cekirdek_esnek = paste("A * C_hane + A * (", ana_etki_sag(h1_temel), ")"))
h1_tab <- list()
h1_gl <- list()
for (mk in names(h1_modeller)) {
  g <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", h1_modeller[[mk]])), hane_m, "HB040", sen_AC("C_hane"), olcu_AC, katsayi = "A:C_hane", kume = seq_len(nrow(hane_m)))
  Bm <- boot_gc(paste0("h1_hane_", mk), g$m, g$w, idx_h, olcu_AC, g$r$b, katsayi = "A:C_hane")
  h1_tab[[mk]] <- gc_ozet(g$r, g$s, Bm)[, `:=`(model = mk, duzey = "hane", n = nrow(hane_m), olay = sum(hane_m$Y_saglik_yuk), npar = ncol(g$m$X), sure_boot = attr(Bm, "sure_sn"))]
  h1_gl[[mk]] <- list(g = g, B = Bm)
}
em_h1 <- sapply(c(gelir = "f_gelir", bolge = "f_bolge", reis_cinsiyet = "f_rcins"), function(z) {
  sgz <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1_temel, "+ A:", z)), des_hm, family = quasibinomial())
  wald_p(sgz, paste0("A:", z))
})
prot_ekle("6. Etki degisimi taramasi (kesifsel)", "A x gelir, A x bolge, A x reis cinsiyeti tasarim Wald (ana kovaryat seti: cekirdek + ihtiyac); p < 0,01 ve yeterli olay varsa tabakali Delta ek tabloya",
          paste0("p: gelir ", formatC(em_h1[1], format = "g", digits = 3), ", bolge ", formatC(em_h1[2], format = "g", digits = 3), ", reis cinsiyeti ", formatC(em_h1[3], format = "g", digits = 3)),
          paste0("Tabakali Delta: ", if (any(em_h1 < 0.01)) paste(names(em_h1)[em_h1 < 0.01], collapse = ", ") else "gerekmedi; inceltilmis model bicimi kurali geregi ana etkili model ana model"), "H1 hane")
lpm_h1 <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1_temel)), des_hm, family = gaussian())
lpm_h1_tam <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1_son)), des_hm, family = gaussian())
ozet_yaz("H1 hane dogrusal olasilik modeli, cekirdek + ihtiyac (A:C_hane = Delta)", summary(lpm_h1))
ozet_yaz("H1 hane dogrusal olasilik modeli, tam duzeltilmis (A:C_hane = Delta)", summary(lpm_h1_tam))
baglanti_h1 <- c(lojistik = h1_gl$cekirdek_ihtiyac$g$r$olc[["Delta"]], dogrusal_olasilik = 100 * unname(coef(lpm_h1)["A:C_hane"]), esnek_lojistik = h1_gl$cekirdek_esnek$g$r$olc[["Delta"]])
fark_h1 <- max(baglanti_h1) - min(baglanti_h1)
h1_ana <- if (any(em_h1 < 0.01)) "cekirdek_esnek" else "cekirdek_ihtiyac"
h1_tab[["baglanti"]] <- data.table(olcu = names(baglanti_h1), tahmin = baglanti_h1, se_if = c(h1_gl$cekirdek_ihtiyac$g$s$se[["Delta"]], 100 * unname(SE(lpm_h1)["A:C_hane"]), h1_gl$cekirdek_esnek$g$s$se[["Delta"]]), model = "baglanti", duzey = "hane")
prot_ekle("4. Baglanti fonksiyonu ve model bicimi", "Ilk kural: uc tahmin farki < 0,5 puan ise ana model korunur. Inceltilmis kural (sonuclar goruldukten sonra, yazar karari): adim 6'da p < 0,01 heterojenlik varsa esnek model ana model; yoksa ana etkili model ana model, esnek model ve LPM duyarlilik",
          paste0("Ana kovaryat seti (cekirdek + ihtiyac) ile Delta: lojistik ", fm(baglanti_h1[1]), ", LPM ", fm(baglanti_h1[2]), ", esnek ", fm(baglanti_h1[3]), "; en buyuk fark ", fm(fark_h1),
                 ". Tam duzeltilmis set ile: lojistik ", fm(h1_gl$nihai$g$r$olc[["Delta"]]), ", LPM ", fm(100 * unname(coef(lpm_h1_tam)["A:C_hane"])), ", esnek ", fm(h1_gl$nihai_esnek$g$r$olc[["Delta"]])),
          if (h1_ana == "cekirdek_ihtiyac") "Adim 6'da heterojenlik yok: ana etkili lojistik + g-comp (cekirdek + ihtiyac) ana model; esnek model ve LPM duyarlilik (Table S6, S11)" else "Heterojenlik var: esnek model ana model", "H1 hane")
h1_gl[[h1_ana]]$B <- boot_uzat(paste0("h1_hane_", h1_ana), h1_gl[[h1_ana]]$g, idx_h, olcu_AC, h1_gl[[h1_ana]]$B, katsayi = "A:C_hane")
h1_tab[[h1_ana]] <- gc_ozet(h1_gl[[h1_ana]]$g$r, h1_gl[[h1_ana]]$g$s, h1_gl[[h1_ana]]$B)[, `:=`(model = h1_ana, duzey = "hane", n = nrow(hane_m), olay = sum(hane_m$Y_saglik_yuk), npar = ncol(h1_gl[[h1_ana]]$g$m$X), sure_boot = attr(h1_gl[[h1_ana]]$B, "sure_sn"))]
g_h1 <- h1_gl[[h1_ana]]$g
B_h1 <- h1_gl[[h1_ana]]$B
sg_h1 <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1_modeller[[h1_ana]])), des_hm, family = quasibinomial())
ozet_yaz(paste("H1 hane ana lojistik model (", h1_ana, ")"), summary(sg_h1))
ozet_yaz("H1 hane tam duzeltilmis lojistik model (duyarlilik)", summary(svyglm(as.formula(paste("Y_saglik_yuk ~", h1_son)), des_hm, family = quasibinomial())))
gvif_tab[["h1"]] <- gvif_kontrol(svyglm(as.formula(paste("Y_saglik_yuk ~", sub("A \\* C_hane", "A + C_hane", h1_temel))), des_hm, family = quasibinomial()), "H1 hane (ana)")
gvif_tab[["h1_tam"]] <- gvif_kontrol(svyglm(as.formula(paste("Y_saglik_yuk ~", sub("A \\* C_hane", "A + C_hane", h1_son))), des_hm, family = quasibinomial()), "H1 hane (tam duzeltilmis)")

hane_m[, AC := paste0(A, C_hane)]
ort_h1 <- ortusme(hane_m, "HB040", "AC", c("gelir_dilim5", "HB030", "egitim_max"))
ort_h1_3 <- ortusme(hane_m, "HB040", "AC", c("gelir_dilim5", "HB030", "egitim3"))
ort_h1_gb <- ortusme(hane_m, "HB040", "AC", c("gelir_dilim5", "HB030"))
ort_h1_ge <- ortusme(hane_m, "HB040", "AC", c("gelir_dilim5", "egitim3"))
kaba_ortusme[["H1 hane"]] <- data.table(model = "H1 hane (dort A x C hucresi)", tabakalama = c("gelir x bolge", "gelir x egitim (3)", "gelir x bolge x egitim (3)", "gelir x bolge x egitim (5)"),
                                        tabaka = c(ort_h1_gb$tabaka_sayisi, ort_h1_ge$tabaka_sayisi, ort_h1_3$tabaka_sayisi, ort_h1$tabaka_sayisi), seyrek_tabaka = c(ort_h1_gb$seyrek_tabaka, ort_h1_ge$seyrek_tabaka, ort_h1_3$seyrek_tabaka, ort_h1$seyrek_tabaka),
                                        seyrek_nufus_payi = c(ort_h1_gb$seyrek_pay, ort_h1_ge$seyrek_pay, ort_h1_3$seyrek_pay, ort_h1$seyrek_pay))
g_h1_ort <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", h1_modeller[[h1_ana]])), hane_m[ort_h1_3$satir_ortusen], "HB040", sen_AC("C_hane"), olcu_AC, katsayi = "A:C_hane", kume = seq_len(sum(ort_h1_3$satir_ortusen)))
prot_ekle("3. Ortusme ve pozitiflik", "Dort A x C hucresinin gelir x bolge x egitim tabakalarinda agirliksiz n < 20",
          paste0("Kaba kontroller: gelir x bolge ", ort_h1_gb$tabaka_sayisi, " tabakanin ", ort_h1_gb$seyrek_tabaka, "'inde en az bir hucre < 20 (hanelerin %", fm(ort_h1_gb$seyrek_pay, 1), "); gelir x egitim (3) ", ort_h1_ge$tabaka_sayisi, " tabakanin ", ort_h1_ge$seyrek_tabaka,
                 " (%", fm(ort_h1_ge$seyrek_pay, 1), "). Ince: 5 egitim grubuyla ", ort_h1$tabaka_sayisi, " tabakanin ", ort_h1$seyrek_tabaka, " (%", fm(ort_h1$seyrek_pay, 1), "); egitim 3 grupla ", ort_h1_3$tabaka_sayisi, " tabakanin ",
                 ort_h1_3$seyrek_tabaka, "'i (%", fm(ort_h1_3$seyrek_pay, 1), "); ortusen tabakalarla Delta ", fm(g_h1_ort$r$olc[["Delta"]]), " (tum nufus ", fm(g_h1$r$olc[["Delta"]]), ")"),
          "Ana tahmin tum hanelere standardize; ortusen ince tabakalarla sinirli tahmin duyarlilikta (Table S6)", "H1 hane")
fwrite(rbindlist(kaba_ortusme), "Results/ortusme_kaba_kontroller.csv")
phat <- plogis(g_h1$r$eta)
al <- al_test(hane_m$Y_saglik_yuk, phat, g_h1$w, seq_len(nrow(hane_m)), "H1 hane")
fwrite(al$kalibrasyon, "Results/log/kalibrasyon_h1_hane.csv")
artik_grafik(hane_m$Y_saglik_yuk, phat, hane_m$reis_yas, g_h1$w, "Results/log/artik_h1_hane.png", "Age of reference person")
prot_ekle("5. Uyum iyiligi", "Archer-Lemeshow F (anket, onda birlik gruplar) ve kalibrasyon",
          paste0("F = ", fm(al$F), " (", al$df, ", ", fm(al$ddf, 0), "), p = ", formatC(al$p, format = "g", digits = 3), "; ondaliklarda gozlenen - tahmin en buyuk mutlak fark ", fm(100 * max(abs(al$kalibrasyon$gozlenen - al$kalibrasyon$tahmin))), " puan"),
          if (al$p >= 0.05) "Uyum yeterli" else "Uyum testi reddetti; fonksiyonel bicim, seyrek kategori ve etkilesimler gozden gecirildi, esnek model karsilastirmasi adim 4'te", "H1 hane")
cs <- summary(sg_h1)$coefficients
ne_h1 <- sum(hane_m$Y_saglik_yuk)
prot_ekle("7. Ayrisma ve seyreklik", "Olay/parametre >= 10; SE > 5 x |katsayi| veya SE > 5 olan parametre",
          paste0("olay ", ne_h1, ", parametre ", ncol(g_h1$m$X), ", oran ", fm(ne_h1 / ncol(g_h1$m$X), 1), "; SE > 5 olan parametre ", sum(cs[, 2] > 5), "; SE > 5|b| olan ", sum(cs[, 2] > 5 * abs(cs[, 1])), " (en buyuk SE ", fm(max(cs[, 2])), ")"),
          if (ne_h1 / ncol(g_h1$m$X) >= 10) "Ayrisma yok, model kucultulmedi" else "Oran < 10: model kucultulmeli", "H1 hane")
prot_ekle("8. Agirlik dagilimi", "En buyuk/medyan > 10 ise %1-%99 kirpilmis agirlikla duyarlilik", paste0("HB040 en buyuk/medyan ", fm(ok8$max_medyan[1]), ", Kish etkili n ", fm(ok8$kish_neff[1], 0), "; FB030 (18+) ", fm(ok8$max_medyan[2]), ", ", fm(ok8$kish_neff[2], 0)),
          "Kirpma gerekmedi", "Tum modeller")
prot_ekle("11. Kumeleme", "Tasarim etkisi", "Hane duzeyinde her hane tek birim (ids = ~1); kumeleme yok. Agirlik kaynakli tasarim etkisi (Kish) 1,40", "Yetiskin duzeyi tekrar ve simulasyonda kumeleme incelenir", "H1 hane")
oz_h1 <- h1_tab[[h1_ana]]
prot_ekle("12. Bootstrap yakinsamasi", "1.000 ve 2.000 tekrarda sinirlar 0,1 puandan az oynamali; oynarsa tekrar artirilir", bk_metin(oz_h1, "Delta"), bk_karar(oz_h1, "Delta"), "H1 hane")
gvif_tab <- rbindlist(gvif_tab, fill = TRUE)
fwrite(gvif_tab, "Results/log/gvif.csv")
prot_ekle("Coklu baglanti (4.5)", "GVIF^(1/(2 df)) > 2,2 olan terim", paste0("En buyuk olcekli GVIF: ", paste(gvif_tab[, .(m = max(gvif_olcekli, na.rm = TRUE)), by = model][, paste0(model, " ", fm(m))], collapse = "; ")),
          if (any(gvif_tab$gvif_olcekli > 2.2, na.rm = TRUE)) paste("Esigi asan:", paste(gvif_tab[gvif_olcekli > 2.2, paste(model, terim)], collapse = ", ")) else "Esigi asan terim yok", "H1 ve H2 nihai modeller")
h1_tab <- rbindlist(h1_tab, fill = TRUE)
h1_tab[, mde := 2.487 * se_boot]
fwrite(h1_tab, "Results/H1_hane_gcomp_sonuclari.csv")
fwrite(rbindlist(protokol), "Results/model_protokolu.csv")
print(h1_tab[olcu %in% c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PR_A", "OR_A:C_hane") | model == "baglanti", .(model, olcu, tahmin, alt, ust, se_if, se_boot, mde)])
print(gvif_tab[gvif_olcekli > 2])

h1_bloklar <- c(strsplit(h1_cekirdek, " \\+ ")[[1]], h1_ihtiyac, unname(h1_aday[h1_secilen]))
h1_ayristir <- rbindlist(c(
  lapply(h1_bloklar, function(z) {
    g <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", h1_min, "+", z)), hane_m, "HB040", sen_AC("C_hane"), olcu_AC, kume = seq_len(nrow(hane_m)))
    data.table(yon = "minimale tek tek ekleme", terim = z, Delta = g$r$olc[["Delta"]], se = g$s$se[["Delta"]], fark = g$r$olc[["Delta"]] - h1_gl$minimal$g$r$olc[["Delta"]])
  }),
  lapply(h1_bloklar, function(z) {
    sag <- paste(setdiff(c("A * C_hane", h1_yas, h1_bloklar), z), collapse = " + ")
    g <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", sag)), hane_m, "HB040", sen_AC("C_hane"), olcu_AC, kume = seq_len(nrow(hane_m)))
    data.table(yon = "nihaiden tek tek cikarma", terim = z, Delta = g$r$olc[["Delta"]], se = g$s$se[["Delta"]], fark = g$r$olc[["Delta"]] - h1_gl$nihai$g$r$olc[["Delta"]])
  })))
fwrite(h1_ayristir, "Results/H1_model_farki_ayristirma.csv")
print(h1_ayristir)

yas_terim_y <- yas_terim
des_y <- svydesign(ids = ~hane_kimlik, weights = ~FB030, data = yetiskin_2025)
h1y_cekirdek <- "f_cins + f_egit + f_gelir + f_bolge + f_hb + f_cocuk"
h1y_ihtiyac <- "f_gali"
ff_h1y <- rbindlist(lapply(names(yas_terim_y), function(k) {
  g <- gc_hizli(as.formula(paste("Y_saglik_yuk ~ A * C_fert +", yas_terim_y[k], "+", h1y_cekirdek, "+", h1y_ihtiyac)), yetiskin_2025, "FB030", sen_AC("C_fert"), olcu_AC, katsayi = "A:C_fert")
  data.table(yas = k, Delta = g$r$olc[["Delta"]], PR_A = g$r$olc[["PR_A"]], se_Delta = g$s$se[["Delta"]])
}))
sg_ff <- svyglm(as.formula(paste("Y_saglik_yuk ~ A * C_fert +", yas_terim_y["df4"], "+", h1y_cekirdek, "+", h1y_ihtiyac)), des_y, family = quasibinomial())
ff_h1y[, `:=`(p_dogrusal_disi_df4 = wald_p(sg_ff, "yas4_1 + yas4_2 + yas4_3"), fark_df4 = Delta - Delta[yas == "df4"])]
fwrite(ff_h1y, "Results/log/h1_yetiskin_fonksiyonel_bicim_yas.csv")
h1y_yas <- if (ff_h1y$p_dogrusal_disi_df4[1] >= 0.10 && abs(ff_h1y[yas == "dogrusal", fark_df4]) < 0.2) yas_terim_y["dogrusal"] else yas_terim_y["df4"]
prot_ekle("2. Fonksiyonel bicim (yas)", "Dogrusal disi bilesen p >= 0,10 ve Delta farki < 0,2 ise dogrusal", paste0("p = ", formatC(ff_h1y$p_dogrusal_disi_df4[1], format = "g", digits = 3), "; Delta dogrusal - df4 = ", fm(ff_h1y[yas == "dogrusal", fark_df4], 3)),
          if (h1y_yas == yas_terim_y["df4"]) "Yas 4 df dogal spline" else "Yas dogrusal", "H1 yetiskin")
h1y_temel <- paste("A * C_fert +", h1y_yas, "+", h1y_cekirdek, "+", h1y_ihtiyac)
h1y_aday <- c(FS010 = "f_saglik", FB100 = "f_medeni", FI010 = "f_calisma", HH020 = "f_mulk", HB050 = "f_htip", wg_engellilik = "f_engel")
g_h1y_0 <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", h1y_temel)), yetiskin_2025, "FB030", sen_AC("C_fert"), olcu_AC, katsayi = "A:C_fert")
aday_h1y <- rbindlist(lapply(names(h1y_aday), function(a) {
  fa <- as.formula(paste("Y_saglik_yuk ~", h1y_temel, "+", h1y_aday[[a]]))
  g1 <- gc_hizli(fa, yetiskin_2025, "FB030", sen_AC("C_fert"), olcu_AC, katsayi = "A:C_fert")
  sg <- svyglm(fa, des_y, family = quasibinomial())
  data.table(model = "H1 yetiskin", aday = a, Delta_once = g_h1y_0$r$olc[["Delta"]], Delta_sonra = g1$r$olc[["Delta"]], PR_once = g_h1y_0$r$olc[["PR_A"]], PR_sonra = g1$r$olc[["PR_A"]],
             se_once = g_h1y_0$s$se[["Delta"]], se_sonra = g1$s$se[["Delta"]], wald_p = wald_p(sg, h1y_aday[[a]]))
}))
aday_h1y[, `:=`(Delta_degisim = Delta_sonra - Delta_once, PR_goreli_degisim = 100 * (PR_sonra - PR_once) / PR_once, se_degisim_yuzde = 100 * (se_sonra - se_once) / se_once)]
aday_h1y[, karar := fcase(abs(Delta_degisim) >= 0.25 | abs(PR_goreli_degisim) >= 10, "Girer (tahmin degisimi)", wald_p < 0.05 & se_degisim_yuzde < -5, "Girer (kesinlik)", default = "Girmez")]
h1y_secilen <- aday_h1y[karar != "Girmez", aday]
h1y_son <- paste(c(h1y_temel, h1y_aday[h1y_secilen]), collapse = " + ")
fwrite(aday_h1y, "Results/kovaryat_secimi_H1_yetiskin.csv")
print(aday_h1y)
h1y_min <- paste("A * C_fert +", h1y_yas, "+ f_cins + f_gelir")
h1y_modeller <- c(minimal = h1y_min, cekirdek = paste("A * C_fert +", h1y_yas, "+", h1y_cekirdek), cekirdek_ihtiyac = h1y_temel, nihai = h1y_son, nihai_esnek = paste("A * C_fert + A * (", ana_etki_sag(h1y_son, "A \\* C_fert \\+ "), ")"),
                  cekirdek_esnek = paste("A * C_fert + A * (", ana_etki_sag(h1y_temel, "A \\* C_fert \\+ "), ")"))
h1y_tab <- list()
h1y_gl <- list()
for (mk in names(h1y_modeller)) {
  g <- gc_hizli(as.formula(paste("Y_saglik_yuk ~", h1y_modeller[[mk]])), yetiskin_2025, "FB030", sen_AC("C_fert"), olcu_AC, katsayi = "A:C_fert")
  Bm <- boot_gc(paste0("h1_yetiskin_", mk), g$m, g$w, idx_y, olcu_AC, g$r$b, katsayi = "A:C_fert")
  h1y_tab[[mk]] <- gc_ozet(g$r, g$s, Bm)[, `:=`(model = mk, duzey = "yetiskin", n = nrow(yetiskin_2025), olay = sum(yetiskin_2025$Y_saglik_yuk), npar = ncol(g$m$X), sure_boot = attr(Bm, "sure_sn"))]
  h1y_gl[[mk]] <- list(g = g, B = Bm)
}
em_h1y <- sapply(c(gelir = "f_gelir", bolge = "f_bolge", cinsiyet = "f_cins"), function(z) {
  sgz <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1y_temel, "+ A:", z)), des_y, family = quasibinomial())
  wald_p(sgz, paste0("A:", z))
})
prot_ekle("6. Etki degisimi taramasi (kesifsel)", "A x gelir, A x bolge, A x cinsiyet tasarim Wald (ana kovaryat seti: cekirdek + ihtiyac)",
          paste0("p: gelir ", formatC(em_h1y[1], format = "g", digits = 3), ", bolge ", formatC(em_h1y[2], format = "g", digits = 3), ", cinsiyet ", formatC(em_h1y[3], format = "g", digits = 3)),
          if (any(em_h1y < 0.01)) "p < 0,01 heterojenlik var" else "p < 0,01 yok: ana etkili model ana model", "H1 yetiskin")
lpm_h1y <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1y_temel)), des_y, family = gaussian())
lpm_h1y_tam <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1y_son)), des_y, family = gaussian())
baglanti_h1y <- c(lojistik = h1y_gl$cekirdek_ihtiyac$g$r$olc[["Delta"]], dogrusal_olasilik = 100 * unname(coef(lpm_h1y)["A:C_fert"]), esnek_lojistik = h1y_gl$cekirdek_esnek$g$r$olc[["Delta"]])
fark_h1y <- max(baglanti_h1y) - min(baglanti_h1y)
h1y_ana <- if (any(em_h1y < 0.01)) "cekirdek_esnek" else "cekirdek_ihtiyac"
h1y_tab[["baglanti"]] <- data.table(olcu = names(baglanti_h1y), tahmin = baglanti_h1y, se_if = c(h1y_gl$cekirdek_ihtiyac$g$s$se[["Delta"]], 100 * unname(SE(lpm_h1y)["A:C_fert"]), h1y_gl$cekirdek_esnek$g$s$se[["Delta"]]), model = "baglanti", duzey = "yetiskin")
prot_ekle("4. Baglanti fonksiyonu ve model bicimi", "Inceltilmis kural (sonuclar goruldukten sonra, yazar karari): adim 6'da p < 0,01 heterojenlik varsa esnek model ana model; yoksa ana etkili model ana model",
          paste0("Ana kovaryat seti ile Delta: lojistik ", fm(baglanti_h1y[1]), ", LPM ", fm(baglanti_h1y[2]), ", esnek ", fm(baglanti_h1y[3]), "; en buyuk fark ", fm(fark_h1y),
                 ". Tam duzeltilmis set ile: lojistik ", fm(h1y_gl$nihai$g$r$olc[["Delta"]]), ", LPM ", fm(100 * unname(coef(lpm_h1y_tam)["A:C_fert"])), ", esnek ", fm(h1y_gl$nihai_esnek$g$r$olc[["Delta"]])),
          if (h1y_ana == "cekirdek_ihtiyac") "Heterojenlik yok: ana etkili lojistik + g-comp (cekirdek + ihtiyac) ana model; esnek model ve LPM duyarlilik" else "Heterojenlik var: esnek model ana model", "H1 yetiskin")
h1y_gl[[h1y_ana]]$B <- boot_uzat(paste0("h1_yetiskin_", h1y_ana), h1y_gl[[h1y_ana]]$g, idx_y, olcu_AC, h1y_gl[[h1y_ana]]$B, katsayi = "A:C_fert")
h1y_tab[[h1y_ana]] <- gc_ozet(h1y_gl[[h1y_ana]]$g$r, h1y_gl[[h1y_ana]]$g$s, h1y_gl[[h1y_ana]]$B)[, `:=`(model = h1y_ana, duzey = "yetiskin", n = nrow(yetiskin_2025), olay = sum(yetiskin_2025$Y_saglik_yuk), npar = ncol(h1y_gl[[h1y_ana]]$g$m$X), sure_boot = attr(h1y_gl[[h1y_ana]]$B, "sure_sn"))]
g_h1y <- h1y_gl[[h1y_ana]]$g
sg_h1y <- svyglm(as.formula(paste("Y_saglik_yuk ~", h1y_modeller[[h1y_ana]])), des_y, family = quasibinomial())
ozet_yaz(paste("H1 yetiskin ana lojistik model (", h1y_ana, ")"), summary(sg_h1y))
phat <- plogis(g_h1y$r$eta)
al <- al_test(yetiskin_2025$Y_saglik_yuk, phat, g_h1y$w, yetiskin_2025$hane_kimlik, "H1 yetiskin")
fwrite(al$kalibrasyon, "Results/log/kalibrasyon_h1_yetiskin.csv")
artik_grafik(yetiskin_2025$Y_saglik_yuk, phat, yetiskin_2025$FK070, g_h1y$w, "Results/log/artik_h1_yetiskin.png", "Age (FK070)")
prot_ekle("5. Uyum iyiligi", "Archer-Lemeshow F ve kalibrasyon", paste0("F = ", fm(al$F), ", p = ", formatC(al$p, format = "g", digits = 3), "; en buyuk ondalik farki ", fm(100 * max(abs(al$kalibrasyon$gozlenen - al$kalibrasyon$tahmin))), " puan"),
          if (al$p >= 0.05) "Uyum yeterli" else "Uyum testi reddetti; esnek model karsilastirmasi adim 4'te", "H1 yetiskin")
deff_h1y <- (g_h1y$s$se[["Delta"]] / g_h1y$s$se_kumesiz[["Delta"]])^2
prot_ekle("11. Kumeleme", "Hane kumelemesinin tasarim etkisi", paste0("Delta SE kumeli ", fm(g_h1y$s$se[["Delta"]], 3), ", kumesiz ", fm(g_h1y$s$se_kumesiz[["Delta"]], 3), ", tasarim etkisi ", fm(deff_h1y)), "Hane kumelemesi korunur", "H1 yetiskin")
oz <- h1y_tab[[h1y_ana]]
prot_ekle("12. Bootstrap yakinsamasi", "1.000 ve 2.000 tekrarda sinirlar 0,1 puandan az oynamali; oynarsa tekrar artirilir", bk_metin(oz, "Delta"), bk_karar(oz, "Delta"), "H1 yetiskin")
h1y_tab <- rbindlist(h1y_tab, fill = TRUE)
h1y_tab[, mde := 2.487 * se_boot]
fwrite(h1y_tab, "Results/H1_yetiskin_gcomp_sonuclari.csv")
fwrite(rbindlist(protokol), "Results/model_protokolu.csv")
print(h1y_tab[olcu %in% c("p00", "p01", "p10", "p11", "Delta", "RERI", "PR_A", "OR_A:C_fert") | model == "baglanti", .(model, olcu, tahmin, alt, ust, se_if, se_kumesiz, se_boot)])

h1_kov <- ana_etki_sag(h1_son)
h1y_kov <- ana_etki_sag(h1y_son, "A \\* C_fert \\+ ")
h1_ana_kov <- ana_etki_sag(h1_temel)
h1y_ana_kov <- ana_etki_sag(h1y_temel, "A \\* C_fert \\+ ")
esnek_f <- function(cv, sag) paste("A *", cv, "+ A * (", sag, ")")
kucuk_kov <- paste(yas_terim["df4"], "+ f_cins + f_gelir + f_bolge + f_egit")
yanit_kov <- "FK070 + f_cins + f_gelir"
ikincil <- list()
ikincil_hucre <- list()
ikincil_calistir <- function(ad, y, dat, ix, w, sag, senaryo, olcu, cvar = NULL, kume = dat$hane_kimlik, Mp = M_parca, fac = fac_hane, tabaka = NULL) {
  dat <- dat[!is.na(get(y))]
  ix <- ix[seq_len(nrow(dat))]
  g <- gc_hizli(as.formula(paste(y, "~", sag)), dat, w, senaryo, olcu, kume = kume[seq_len(nrow(dat))], tabaka = tabaka)
  Bm <- boot_gc(ad, g$m, g$w, ix, olcu, g$r$b, Mp = Mp, fac = fac)
  oz <- gc_ozet(g$r, g$s, Bm)[, `:=`(analiz = ad, sonuc = y, n = nrow(dat), olay = sum(dat[[y]]), npar = ncol(g$m$X))]
  oz[, olay_parametre := olay / npar]
  if (!is.null(cvar)) {
    hc <- dat[, .(n = .N, olay = sum(get(y))), keyby = c("A", cvar)]
    hc[, `:=`(analiz = ad, kucuk = kucuk_hucre(olay))]
    ikincil_hucre[[ad]] <<- hc
  }
  ikincil[[ad]] <<- oz
  invisible(list(g = g, B = Bm, oz = oz))
}
yt <- copy(yetiskin_2025)
ix_yt <- function(kosul) idx_y[kosul]
k1 <- yt$ihtiyac_var == 1
ikincil_calistir("odeme_tibbi_AxC", "odeme_tibbi", yt[k1], idx_y[k1], "FB030", paste("A * C_fert +", kucuk_kov), sen_AC("C_fert"), olcu_AC, cvar = "C_fert")
k2 <- yt$ihtiyac_herhangi == 1
ikincil_calistir("odeme_herhangi_AxC", "odeme_herhangi", yt[k2], idx_y[k2], "FB030", paste("A * C_fert +", kucuk_kov), sen_AC("C_fert"), olcu_AC, cvar = "C_fert")
k3 <- yt$C_fert == 1 & !is.na(yt$uzman_hic)
ikincil_calistir("uzman_hic_kronik_A", "uzman_hic", yt[k3], idx_y[k3], "FB030", paste("A +", h2_yas, "+", h1y_cekirdek, "+", h1y_ihtiyac), sen_A, olcu_A, cvar = "C_fert")
k3b <- yt$C_fert == 1 & !is.na(yt$uzman_hic_bos_hic)
ikincil_calistir("uzman_hic_kronik_A_boslar_hic", "uzman_hic_bos_hic", yt[k3b], idx_y[k3b], "FB030", paste("A +", h2_yas, "+", h1y_cekirdek, "+", h1y_ihtiyac), sen_A, olcu_A)
ikincil_calistir("dis_yuk_A", "Y_dis_yuk", hane_m, idx_h, "HB040", paste("A +", h1_ana_kov, "+ C_hane"), sen_A, olcu_A, kume = seq_len(nrow(hane_m)))
ikincil_calistir("dis_harcama_yok_A", "dis_harcama_yok", hane_m, idx_h, "HB040", paste("A +", h1_ana_kov, "+ C_hane"), sen_A, olcu_A, kume = seq_len(nrow(hane_m)))
k4 <- hane_m$dis_harcama_yok == 0
ikincil_calistir("dis_yuk_A_harcama_var", "Y_dis_yuk", hane_m[k4], idx_h[k4], "HB040", paste("A +", h1_ana_kov, "+ C_hane"), sen_A, olcu_A, kume = seq_len(sum(k4)))
k5 <- yt$hane_reisi == 0 & yt$ihtiyac_var == 1
ikincil_calistir("yanitlayici_odeme_tibbi_reis_degil", "odeme_tibbi", yt[k5], idx_y[k5], "FB030", paste("A +", yanit_kov), sen_A, olcu_A)
k6 <- yt$hane_reisi == 0 & yt$dis_ihtiyac_var == 1
ikincil_calistir("yanitlayici_odeme_dis_reis_degil", "odeme_dis", yt[k6], idx_y[k6], "FB030", paste("A +", yanit_kov), sen_A, olcu_A)
k7 <- yt$hane_reisi == 0 & !is.na(yt$uzman_hic)
ikincil_calistir("yanitlayici_uzman_hic_reis_degil", "uzman_hic", yt[k7], idx_y[k7], "FB030", paste("A +", kucuk_kov, "+ f_gali"), sen_A, olcu_A)
k8 <- yt$hane_reisi == 1 & yt$ihtiyac_var == 1
ikincil_calistir("yanitlayici_odeme_tibbi_reis", "odeme_tibbi", yt[k8], idx_y[k8], "FB030", paste("A +", yanit_kov), sen_A, olcu_A)
k9 <- yt$hane_reisi == 1 & yt$dis_ihtiyac_var == 1
ikincil_calistir("yanitlayici_odeme_dis_reis", "odeme_dis", yt[k9], idx_y[k9], "FB030", paste("A +", yanit_kov), sen_A, olcu_A)
k10 <- yt$hane_reisi == 1 & !is.na(yt$uzman_hic)
ikincil_calistir("yanitlayici_uzman_hic_reis", "uzman_hic", yt[k10], idx_y[k10], "FB030", paste("A +", kucuk_kov, "+ f_gali"), sen_A, olcu_A)

eski_kov <- paste(yas_terim["df4"], "+ f_cins + f_gelir + f_bolge")
k11 <- yt$ihtiyac_var == 1
eh <- ikincil_calistir("eski_hipotez_AxB", "odeme_tibbi", yt[k11], idx_y[k11], "FB030", paste("A * B +", eski_kov), sen_AC("B"), olcu_AC, cvar = "B")
eh_epv <- sum(yt[k11]$odeme_tibbi) / ncol(eh$g$m$X)
yt11 <- yt[k11]
sg_eh <- svyglm(as.formula(paste("odeme_tibbi ~ A * B +", eski_kov)), svydesign(ids = ~hane_kimlik, weights = ~FB030, data = yt11), family = quasibinomial())
ozet_yaz("Kesifsel eski hipotez: odeme_tibbi ~ A * B (ihtiyac_var)", summary(sg_eh))
firth_eh <- logistf(as.formula(paste("odeme_tibbi ~ A * B + FK070 + f_cins + f_gelir + f_bolge")), data = as.data.frame(yt11))
ml_eh <- glm(as.formula(paste("odeme_tibbi ~ A * B + FK070 + f_cins + f_gelir + f_bolge")), data = yt11, family = binomial())
eh_firth <- data.table(yontem = c("Firth (agirliksiz)", "ML (agirliksiz)", "Anket agirlikli"), A_B_log_OR = c(coef(firth_eh)["A:B"], coef(ml_eh)["A:B"], coef(sg_eh)["A:B"]))
ozet_yaz("Kesifsel eski hipotez: Firth ve ML karsilastirmasi (A:B log OR)", eh_firth)
prot_ekle("7. Ayrisma ve seyreklik", "Olay/parametre >= 10; aksi halde bolge cikarilir veya Firth", paste0("odeme_tibbi olay ", sum(yt11$odeme_tibbi), ", parametre ", ncol(eh$g$m$X), ", oran ", fm(eh_epv, 1),
          "; A:B log OR anket ", fm(coef(sg_eh)["A:B"]), ", agirliksiz ML ", fm(coef(ml_eh)["A:B"]), ", Firth ", fm(coef(firth_eh)["A:B"])),
          if (eh_epv >= 10) "Oran >= 10: bolge modelde kaldi; Firth karsilastirma icin raporlandi" else "Oran < 10: bolge cikarilmali", "Kesifsel eski hipotez")

yt11 <- cbind(yt11, ns_bilesen(yt11$diyet_skor, 4, "dsk4_"))
sg_ds <- svyglm(as.formula(paste("odeme_tibbi ~ A * (diyet_skor + dsk4_1 + dsk4_2 + dsk4_3) +", eski_kov)), svydesign(ids = ~hane_kimlik, weights = ~FB030, data = yt11), family = quasibinomial())
p_ds_nl <- wald_p(sg_ds, "dsk4_1 + dsk4_2 + dsk4_3 + A:dsk4_1 + A:dsk4_2 + A:dsk4_3")
ds_form <- if (p_ds_nl >= 0.10) "A * diyet_skor" else "A * (diyet_skor + dsk4_1 + dsk4_2 + dsk4_3)"
prot_ekle("2. Fonksiyonel bicim (diyet_skor)", "Dogrusal disi bilesenler p >= 0,10 ise dogrusal", paste0("p = ", formatC(p_ds_nl, format = "g", digits = 3)), if (p_ds_nl >= 0.10) "diyet_skor dogrusal" else "diyet_skor spline", "Kesifsel sirali B")
ds_sen <- list()
for (a in 0:1) for (s in 0:10) {
  sc <- list(A = a, diyet_skor = s)
  if (ds_form != "A * diyet_skor") {
    rr <- yt11[which.min(abs(diyet_skor - s))][1, .(dsk4_1, dsk4_2, dsk4_3)]
    sc <- c(sc, as.list(rr))
  }
  ds_sen[[paste0("a", a, "_d", s)]] <- sc
}
olcu_ds <- function(p) setNames(100 * unname(p[paste0(names(ds_sen), "|T")]), names(ds_sen))
ikincil_calistir("eski_hipotez_A_x_diyet_skor", "odeme_tibbi", yt11, idx_y[k11], "FB030", paste(ds_form, "+", eski_kov), ds_sen, olcu_ds)
ikincil <- rbindlist(ikincil, fill = TRUE)
ikincil_hucre <- rbindlist(ikincil_hucre, fill = TRUE)
fwrite(ikincil, "Results/ikincil_kesifsel_sonuclar.csv")
fwrite(ikincil_hucre, "Results/ikincil_hucre_sayilari.csv")
figS3 <- ikincil[analiz == "eski_hipotez_A_x_diyet_skor", .(olcu, tahmin, alt, ust)]
figS3[, `:=`(A = as.integer(sub("a(\\d)_d.*", "\\1", olcu)), diyet_skor = as.integer(sub(".*_d", "", olcu)))]
fwrite(figS3, file.path(ddir, "figur_verileri", "figS3b_diyet_skor_egrisi.csv"))
fwrite(merge(ikincil[analiz == "eski_hipotez_AxB" & olcu %in% c("p00", "p01", "p10", "p11"), .(olcu, tahmin, alt, ust)], ikincil_hucre[analiz == "eski_hipotez_AxB", .(olcu = paste0("p", A, B), n, olay, kucuk)], by = "olcu"),
       file.path(ddir, "figur_verileri", "figS3a_eski_hipotez_hucre.csv"))
print(ikincil[olcu %in% c("p00", "p01", "p10", "p11", "Delta", "PD_A", "PR_A", "PD", "PR", "p0", "p1"), .(analiz, olcu, tahmin, alt, ust, n, olay, olay_parametre)])
print(ikincil_hucre)

hane_m[, `:=`(f_he040 = factor(pmin(HE040, 5)), f_ckat = factor(C_hane_sayi), f_nkron = factor(pmin(n_kronik, 3)), f_ibbs2 = factor(HB031), f_he100 = factor(HE100), f_yoks6 = factor(pmin(hane_yoksunluk_6, 5)),
              f_konut = factor(fifelse(is.na(konut_maliyet_asiri_yuk), 9, konut_maliyet_asiri_yuk)), dis_kod4 = as.integer(MODUL1_H_1 == 4))]
hane_m <- cbind(hane_m, ns_bilesen(hane_m$log_esdeger_gelir, 4, "lgel4_"))
h1_esnek_sag <- function(sag) paste("A * C_hane + A * (", sag, ")")
h1_me_sag <- function(sag, cv = "C_hane") paste("A *", cv, "+", sag)
duy <- list()
duy_ekle <- function(ad, aciklama, y, dat, sag, cvar = "C_hane", sen = sen_AC(cvar), olcu = olcu_AC, w = "HB040", kume = NULL, tabaka = NULL, Mp = M_parca, fac = fac_hane, ix = NULL, olc = "Delta") {
  if (is.null(kume)) kume <- seq_len(nrow(dat))
  if (is.null(ix)) ix <- match(dat$hane_kimlik, hane_id)
  g <- gc_hizli(as.formula(paste(y, "~", sag)), dat, w, sen, olcu, kume = kume, tabaka = tabaka)
  Bm <- boot_gc(paste0("duyH1_", ad), g$m, g$w, ix, olcu, g$r$b, Mp = Mp, fac = fac)
  oz <- gc_ozet(g$r, g$s, Bm)
  oz[, `:=`(analiz = ad, aciklama = aciklama, n = nrow(dat), olay = sum(dat[[y]]), npar = ncol(g$m$X))]
  duy[[ad]] <<- oz
  invisible(oz[olcu == olc])
}
h1_ana_sag <- h1_modeller[[h1_ana]]
duy[["00_ana"]] <- copy(h1_tab[model == h1_ana])[, `:=`(analiz = "00_ana", aciklama = "Main estimate: main-effects logistic, core + need covariates")]
duy[["00a_minimal"]] <- copy(h1_tab[model == "minimal"])[, `:=`(analiz = "00a_minimal", aciklama = "Minimal covariates (reference person age and sex, income)")]
duy[["00b_cekirdek"]] <- copy(h1_tab[model == "cekirdek"])[, `:=`(analiz = "00b_cekirdek", aciklama = "Core covariates without need indicator")]
duy[["00b_tam"]] <- copy(h1_tab[model == "nihai"])[, `:=`(analiz = "00b_tam", aciklama = "Fully adjusted (adds disability and bad self-rated health counts)")]
duy[["00c_esnek_tam"]] <- copy(h1_tab[model == "nihai_esnek"])[, `:=`(analiz = "00c_esnek_tam", aciklama = "Flexible logistic, fully adjusted")]
duy[["00d_esnek_cekirdek"]] <- copy(h1_tab[model == "cekirdek_esnek"])[, `:=`(analiz = "00d_esnek_cekirdek", aciklama = "Flexible logistic, core + need")]
duy[["00e_lpm"]] <- data.table(olcu = "Delta", tahmin = 100 * unname(coef(lpm_h1)["A:C_hane"]), se_if = 100 * unname(SE(lpm_h1)["A:C_hane"]), analiz = "00e_lpm", aciklama = "Linear probability model, core + need (design-based CI)", n = nrow(hane_m), olay = sum(hane_m$Y_saglik_yuk))
duy[["00e_lpm"]][, `:=`(alt = tahmin - 1.96 * se_if, ust = tahmin + 1.96 * se_if)]
duy_ekle("01_HE040", "Adding ability to make ends meet (HE040)", "Y_saglik_yuk", hane_m, h1_me_sag(paste(h1_ana_kov, "+ f_he040")))
duy_ekle("02_genis_sonuc", "Outcome: heavy or some burden", "Y_saglik_yuk_genis", hane_m, h1_ana_sag)
hk <- hane_m[MODUL1_H_1 != 4]
duy_ekle("03_harcamasiz_dislandi", "Excluding households with no medical expenditure (MODUL1_H_1 = 4)", "Y_saglik_yuk", hk, h1_ana_sag)
olcu_ckat <- function(p) {
  q <- function(a, c) unname(p[paste0("a", a, "c", c, "|T")])
  c(Delta_1v0 = 100 * (q(1, 1) - q(1, 0) - q(0, 1) + q(0, 0)), Delta_2v0 = 100 * (q(1, 2) - q(1, 0) - q(0, 2) + q(0, 0)))
}
sen_ckat <- list()
for (a in 0:1) for (cc in 0:2) sen_ckat[[paste0("a", a, "c", cc)]] <- list(A = a, f_ckat = factor(cc, levels = 0:2))
duy_ekle("04a_kronik_sayisi", "Modifier: number of adults with chronic illness (1 and 2+ vs 0)", "Y_saglik_yuk", hane_m, h1_me_sag(h1_ana_kov, "f_ckat"), sen = sen_ckat, olcu = olcu_ckat, olc = "Delta_1v0")
duy_ekle("04b_kronik_oran", "Modifier: proportion of adults with chronic illness (1 vs 0)", "Y_saglik_yuk", hane_m[!is.na(C_hane_oran)], h1_me_sag(h1_ana_kov, "C_hane_oran"), cvar = "C_hane_oran")
h1_kov_gali <- gsub(" \\+ f_ngali", "", h1_ana_kov)
duy_ekle("04c_gali", "Modifier: adult with activity limitation (GALI) in household", "Y_saglik_yuk", hane_m, h1_me_sag(h1_kov_gali, "C_hane_gali"), cvar = "C_hane_gali")
duy_ekle("04d_gali_kronik", "Modifier: adult with activity limitation (GALI), adjusted for number of adults with chronic illness", "Y_saglik_yuk", hane_m, h1_me_sag(paste(h1_kov_gali, "+ f_nkron"), "C_hane_gali"), cvar = "C_hane_gali")
duy_ekle("05_yas17", "Chronic illness defined among adults aged FK070 >= 17", "Y_saglik_yuk", hane_m, h1_me_sag(h1_ana_kov, "C_hane17"), cvar = "C_hane17")
duy_ekle("08_ibbs2_sozde_tabaka", "NUTS-2 regions as pseudo-strata in variance estimation", "Y_saglik_yuk", hane_m, h1_ana_sag, tabaka = hane_m$HB031, Mp = M_parca_tab, fac = fac_hane_tab)
duy_ekle("09_log_gelir_spline", "Log equivalized income (spline) instead of quintiles", "Y_saglik_yuk", hane_m, h1_me_sag(sub("f_gelir", "log_esdeger_gelir + lgel4_1 + lgel4_2 + lgel4_3", h1_ana_kov)))
duy_ekle("11a_bolgesiz", "Without region", "Y_saglik_yuk", hane_m, h1_me_sag(gsub(" \\+ f_bolge", "", h1_ana_kov)))
duy_ekle("11b_ibbs2_sabit_etki", "NUTS-2 instead of NUTS-1 region fixed effects", "Y_saglik_yuk", hane_m, h1_me_sag(sub("f_bolge", "f_ibbs2", h1_ana_kov)))
duy_ekle("12_kronik_bilinmeyen_dislandi", "Excluding 30 households with unknown chronic status", "Y_saglik_yuk", hane_m[kronik_bilinmeyen == 0], h1_ana_sag)
duy_ekle("13_yoksunluk_kovaryatlari", "Adding HE100, household deprivation count and housing cost overburden", "Y_saglik_yuk", hane_m, h1_me_sag(paste(h1_ana_kov, "+ f_he100 + f_yoks6 + f_konut")))
duy_ekle("14_ortusen_tabakalar", "Standardization restricted to overlapping fine strata (income x region x education)", "Y_saglik_yuk", hane_m[ort_h1_3$satir_ortusen], h1_ana_sag)
duy_ekle("15_tibbi_bilesen", "Outcome component: medical care heavy burden", "Y_tibbi_yuk", hane_m, h1_ana_sag)
duy_ekle("16_ilac_bilesen", "Outcome component: medicine heavy burden", "Y_ilac_yuk", hane_m, h1_ana_sag)
duy[["10m_yetiskin_minimal"]] <- copy(h1y_tab[model == "minimal"])[, `:=`(analiz = "10m_yetiskin_minimal", aciklama = "Adult level, minimal covariates")]
duy[["10n_yetiskin_cekirdek"]] <- copy(h1y_tab[model == "cekirdek"])[, `:=`(analiz = "10n_yetiskin_cekirdek", aciklama = "Adult level, core covariates without need indicator")]
duy[["10a_yetiskin_ana"]] <- copy(h1y_tab[model == h1y_ana])[, `:=`(analiz = "10a_yetiskin_ana", aciklama = "Adult level, main model (core + need, household clusters)")]
duy[["10b_yetiskin_tam"]] <- copy(h1y_tab[model == "nihai"])[, `:=`(analiz = "10b_yetiskin_tam", aciklama = "Adult level, fully adjusted (adds self-rated health)")]
duy[["10c_yetiskin_esnek"]] <- copy(h1y_tab[model == "nihai_esnek"])[, `:=`(analiz = "10c_yetiskin_esnek", aciklama = "Adult level, flexible, fully adjusted")]
duy <- rbindlist(duy, fill = TRUE)
duy[analiz == "10a_yetiskin_ana" & olcu == "Delta", `:=`(alt_kumesiz = tahmin - 1.96 * se_kumesiz, ust_kumesiz = tahmin + 1.96 * se_kumesiz)]
fwrite(duy, "Results/duyarlilik_H1.csv")
orman <- duy[olcu %in% c("Delta", "Delta_1v0", "Delta_2v0"), .(analiz, aciklama = fifelse(olcu == "Delta_2v0", sub("\\(1 and 2\\+ vs 0\\)", "(2+ vs 0)", aciklama), fifelse(olcu == "Delta_1v0", sub("\\(1 and 2\\+ vs 0\\)", "(1 vs 0)", aciklama), aciklama)),
                                                             olcu, tahmin, alt, ust, se_boot, se_if, n, olay)]
fwrite(orman, file.path(ddir, "figur_verileri", "fig4d_duyarlilik_orman.csv"))
print(orman)

h2_duy <- list()
for (v in h2_y) {
  dt <- h2_veri(v)
  ix <- idx_y[!is.na(yetiskin_2025[[v]])]
  sag <- h2_modeller[[h2_ana[[v]]]]
  g <- gc_hizli(as.formula(paste(v, "~", sag)), dt, "FB030", sen_A, olcu_A, tabaka = dt$HB031)
  Bm <- boot_gc(paste0("duy_h2_", v, "_ibbs2"), g$m, g$w, ix, olcu_A, g$r$b, Mp = M_parca_tab, fac = fac_hane_tab)
  h2_duy[[paste(v, "ibbs2")]] <- gc_ozet(g$r, g$s, Bm)[, `:=`(sonuc = v, analiz = "NUTS-2 pseudo-strata")]
  g2 <- gc_hizli(as.formula(paste(v, "~", h2_esnek <- esnek_yap(sub("f_gelir", "log_esdeger_gelir + lg4_1 + lg4_2 + lg4_3", h2_son)))), cbind(dt, ns_bilesen(dt$log_esdeger_gelir, 4, "lg4_")), "FB030", sen_A, olcu_A)
  Bm2 <- boot_gc(paste0("duy_h2_", v, "_loggelir"), g2$m, g2$w, ix, olcu_A, g2$r$b)
  h2_duy[[paste(v, "loggelir")]] <- gc_ozet(g2$r, g2$s, Bm2)[, `:=`(sonuc = v, analiz = "Log income spline")]
  g3 <- gc_hizli(as.formula(paste(v, "~", sag)), dt, "FB030", sen_A, olcu_A)
  h2_duy[[paste(v, "kumesiz")]] <- data.table(olcu = names(g3$r$olc), tahmin = g3$r$olc, se_if = g3$s$se_kumesiz[names(g3$r$olc)], alt = g3$r$olc - 1.96 * g3$s$se_kumesiz[names(g3$r$olc)], ust = g3$r$olc + 1.96 * g3$s$se_kumesiz[names(g3$r$olc)], sonuc = v, analiz = "No household clustering (linearization SE)")
}
dtv <- yetiskin_2025
gv <- gc_hizli(as.formula(paste("et_haftada_az_veg ~", h2_modeller[[h2_ana[["et_haftada_az"]]]])), dtv, "FB030", sen_A, olcu_A)
Bv <- boot_gc("duy_h2_et_vejetaryen_dahil", gv$m, gv$w, idx_y, olcu_A, gv$r$b)
h2_duy[["veg"]] <- gc_ozet(gv$r, gv$s, Bv)[, `:=`(sonuc = "et_haftada_az", analiz = "Vegetarians counted as less than weekly")]
y17 <- yetiskin17_2025
y17[, `:=`(f_cins = factor(FK090), f_egit = factor(egitim5), f_gelir = factor(gelir_dilim5), f_bolge = factor(HB030, levels = names(etiket$HB030)), f_hb = factor(pmin(hane_buyuklugu, 7)), f_cocuk = factor(pmin(hane_0_17, 4)))]
y17 <- cbind(y17, ns_bilesen(y17$FK070, 4, "yas4_"))
ix17 <- match(y17$hane_kimlik, hane_id)
for (v in h2_y) {
  k <- !is.na(y17[[v]])
  g <- gc_hizli(as.formula(paste(v, "~", h2_modeller[[h2_ana[[v]]]])), y17[k], "FB030", sen_A, olcu_A)
  Bm <- boot_gc(paste0("duy_h2_", v, "_yas17"), g$m, g$w, ix17[k], olcu_A, g$r$b)
  h2_duy[[paste(v, "yas17")]] <- gc_ozet(g$r, g$s, Bm)[, `:=`(sonuc = v, analiz = "Adults defined as FK070 >= 17")]
}
for (v in h2_y) h2_duy[[paste(v, "ortusen")]] <- h2_tab[sonuc == v & model == "ortusen_tabakalar"][, analiz := "Standardization restricted to overlapping strata"]
h2_duy <- rbindlist(h2_duy, fill = TRUE)
fwrite(h2_duy, "Results/duyarlilik_H2.csv")
print(h2_duy[olcu %in% c("PD", "PR"), .(sonuc, analiz, olcu, tahmin, alt, ust)])

dir.create(file.path(ddir, "simulasyon"), showWarnings = FALSE)
sim_h_v <- unique(c("hane_kimlik", "HB040", "HB031", "Y_saglik_yuk", "A", "C_hane", all.vars(as.formula(paste("~", h1_ana_kov)))))
sim_y_v <- unique(c("hane_kimlik", "FB030", "Y_saglik_yuk", "A", "C_fert", all.vars(as.formula(paste("~", h1y_ana_kov)))))
saveRDS(list(hane = hane_m[, ..sim_h_v], yetiskin = yetiskin_2025[, ..sim_y_v], h1_kov = h1_ana_kov, h1y_kov = h1y_ana_kov, h1_ana = h1_ana, h1y_ana = h1y_ana,
             h1_hucre = g_h1$r$olc, h1y_hucre = g_h1y$r$olc,
             fonk = list(irls = irls, mm_kur = mm_kur, p_hesap = p_hesap, senaryo_mu = senaryo_mu, olc_cikar = olc_cikar, gc_tahmin = gc_tahmin, var_toplam = var_toplam, jakobyen = jakobyen, gc_if = gc_if, olcu_AC = olcu_AC, sen_AC = sen_AC)),
        file.path(ddir, "simulasyon", "sim_girdi.rds"))

akis <- rbindlist(list(
  data.table(adim = "kaynak", etiket = "ILCS cross-sections 2023-2025, persons", n = nrow(d)),
  data.table(adim = "kaynak_hane", etiket = "ILCS cross-sections 2023-2025, households", n = d[hane_reisi == 1, .N]),
  data.table(adim = paste0("yil_", 2023:2025, "_kisi"), etiket = paste("Persons", 2023:2025), n = d[, .N, keyby = yil]$N),
  data.table(adim = paste0("yil_", 2023:2025, "_hane"), etiket = paste("Households", 2023:2025), n = d[hane_reisi == 1, .N, keyby = yil]$N),
  data.table(adim = "k2025_hane", etiket = "2025 households", n = nrow(h25)),
  data.table(adim = "k2025_kisi", etiket = "2025 persons", n = nrow(d25)),
  data.table(adim = "k2025_fkayit", etiket = "2025 individual questionnaires (aged 15+)", n = nrow(f25)),
  data.table(adim = "dis_0_14", etiket = "Aged <15 years", n = d25[FK070 < 15, .N]),
  data.table(adim = "dis_15_17", etiket = "Aged 15-17 years (FK070) with individual record", n = d25[FK070 %in% 15:17 & f_kayit == 1, .N]),
  data.table(adim = "dis_15plus_kayitsiz", etiket = "Aged 15+ without individual record", n = d25[FK070 >= 15 & f_kayit == 0, .N]),
  data.table(adim = "dis_18plus_kayitsiz", etiket = "Aged 18+ without individual record", n = d25[FK070 >= 18 & f_kayit == 0, .N]),
  data.table(adim = "dis_hane_yetiskinsiz", etiket = "Households without an adult member", n = hane_2025[n_yetiskin == 0, .N]),
  data.table(adim = "analitik_hane", etiket = "Household analytic sample", n = nrow(hane_m)),
  data.table(adim = "analitik_yetiskin", etiket = "Adult analytic sample (FK070 >= 18)", n = nrow(yetiskin_2025)),
  data.table(adim = "et_vejetaryen_dislandi", etiket = "Vegetarian or vegan adults excluded from meat outcome", n = yetiskin_2025[vejetaryen == 1, .N]),
  data.table(adim = "bki_supheli_dislandi", etiket = "Implausible height/weight excluded from BMI description", n = yetiskin_2025[bki_supheli == 1, .N]),
  data.table(adim = "ihtiyac_var", etiket = "Adults reporting a medical care need (unmet-need universe)", n = yetiskin_2025[ihtiyac_var == 1, .N]),
  data.table(adim = "dis_ihtiyac_var", etiket = "Adults reporting a dental care need", n = yetiskin_2025[dis_ihtiyac_var == 1, .N]),
  data.table(adim = "kronik_yetiskin", etiket = "Adults with chronic illness (specialist-visit universe)", n = yetiskin_2025[C_fert == 1, .N]),
  data.table(adim = "hane_A1", etiket = "Households unable to afford the meal", n = hane_m[A == 1, .N]),
  data.table(adim = "hane_A0", etiket = "Households able to afford the meal", n = hane_m[A == 0, .N]),
  data.table(adim = "hane_C1", etiket = "Households with an adult with chronic illness", n = hane_m[C_hane == 1, .N]),
  data.table(adim = "yetiskin_A1", etiket = "Adults in households unable to afford the meal", n = yetiskin_2025[A == 1, .N]),
  data.table(adim = "yetiskin_A0", etiket = "Adults in households able to afford the meal", n = yetiskin_2025[A == 0, .N]),
  data.table(adim = "Y_olay_hane", etiket = "Households reporting heavy medical or medicine burden", n = hane_m[Y_saglik_yuk == 1, .N])
))
fwrite(akis, file.path(ddir, "figur_verileri", "fig1_akis.csv"))
print(akis)

fig4_hucre <- function(tb, hc, cv, duzey) {
  o <- tb[olcu %in% c("p00", "p01", "p10", "p11", "beklenti11", "Delta", "RERI", "AP", "PR_A", "PD_A"), .(olcu, tahmin, alt, ust)]
  hcc <- hc[, .(olcu = paste0("p", A, get(cv)), n, olay, ham_yuzde)]
  o <- merge(o, hcc, by = "olcu", all.x = TRUE)
  o[, duzey := duzey]
  o
}
fwrite(fig4_hucre(h1_tab[model == h1_ana], hucre_h_n, "C_hane", "hane"), file.path(ddir, "figur_verileri", "fig4a_hucre_hane.csv"))
fwrite(fig4_hucre(h1y_tab[model == h1y_ana], hucre_y_n, "C_fert", "yetiskin"), file.path(ddir, "figur_verileri", "fig4b_hucre_yetiskin.csv"))
fwrite(neden_l[, .(yil, A, kategori, yuzde, alt, ust, N, kucuk)], file.path(ddir, "figur_verileri", "figS1_karsilanmamis_neden.csv"))
mb <- rbindlist(lapply(c("MODUL1_F_1", "MODUL1_F_2", "MODUL1_F_3"), function(v) {
  ih <- if (v == "MODUL1_F_1") "FS070" else "FS050"
  f25[, .(n = .N), by = .(ihtiyac = get(ih), durum = fcase(is.na(get(v)), "Blank", get(v) == 6, "Never (code 6)", default = "One or more visits"))][, `:=`(degisken = v, ihtiyac_degiskeni = ih)]
}))
fwrite(mb, file.path(ddir, "figur_verileri", "figS4_modul_bosluk.csv"))
eks <- rbind(ok7[, .(duzey, degisken, eksik, n, eksik_yuzde)],
             data.table(duzey = "yetiskin", degisken = c("uzman_hic (MODUL1_F_3)", "pratisyen_hic (MODUL1_F_2)", "dis_hekimi_hic (MODUL1_F_1)"),
                        eksik = c(yetiskin_2025[is.na(uzman_hic), .N], yetiskin_2025[is.na(pratisyen_hic), .N], yetiskin_2025[is.na(dis_hekimi_hic), .N]), n = nrow(yetiskin_2025))[, eksik_yuzde := 100 * eksik / n])
fwrite(eks, file.path(ddir, "figur_verileri", "figS4b_eksik_oranlari.csv"))
kal <- rbindlist(lapply(list.files("Results/log", pattern = "^kalibrasyon_.*\\.csv$", full.names = TRUE), function(f) fread(f)[, dosya := basename(f)]), fill = TRUE)
fwrite(kal, file.path(ddir, "figur_verileri", "figS5_kalibrasyon.csv"))

bolge_kod <- names(etiket$HB030)
olcu_A_bolge <- function(p) {
  o <- olcu_A(p)
  for (b in bolge_kod) {
    p0 <- unname(p[paste0("A0|", b)])
    p1 <- unname(p[paste0("A1|", b)])
    o <- c(o, setNames(c(100 * (p1 - p0), p1 / p0), paste0(c("PD_", "PR_"), b)))
  }
  o
}
h2_bolge <- rbindlist(lapply(h2_y, function(v) {
  dt <- h2_veri(v)
  ix <- idx_y[!is.na(yetiskin_2025[[v]])]
  g <- gc_hizli(as.formula(paste(v, "~", h2_modeller[["nihai_esnek"]])), dt, "FB030", sen_A, olcu_A_bolge, grup = dt$HB030)
  Bm <- boot_gc(paste0("h2_", v, "_bolge"), g$m, g$w, ix, olcu_A_bolge, g$r$b, grup = dt$HB030)
  rbindlist(lapply(bolge_kod, function(b) data.table(sonuc = v, bolge = b, bolge_ad = etiket$HB030[[b]],
    PD = g$r$olc[[paste0("PD_", b)]], PD_alt = quantile(Bm[, paste0("PD_", b)], 0.025), PD_ust = quantile(Bm[, paste0("PD_", b)], 0.975),
    PR = g$r$olc[[paste0("PR_", b)]], PR_alt = quantile(Bm[, paste0("PR_", b)], 0.025), PR_ust = quantile(Bm[, paste0("PR_", b)], 0.975),
    n_A0 = dt[HB030 == b & A == 0, .N], n_A1 = dt[HB030 == b & A == 1, .N])))
}))
h2_bolge[, kucuk := kucuk_hucre(pmin(n_A0, n_A1))]
fwrite(h2_bolge, "Results/H2_bolgeye_gore.csv")

ETIKET_H2 <- c(et_haftada_az = "Meat, chicken or fish less than once a week", meyve_gunluk_degil = "Fruit less than once a day", sebze_gunluk_degil = "Vegetables or salad less than once a day")

dir.create("Results/Supplementary", showWarnings = FALSE)
f1 <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
f2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
nfmt <- function(n) formatC(n, format = "d", big.mark = ",")
ga <- function(e, l, u, d = 1) paste0(formatC(e, format = "f", digits = d), " (", formatC(l, format = "f", digits = d), ", ", formatC(u, format = "f", digits = d), ")")
pfmt <- function(p) fifelse(is.na(p), "", fifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
yazi_tipi_t <- "Times New Roman"
tablo_bicim <- function(ft, notlar = character(0), grup_satir = integer(0)) {
  ft <- font(ft, fontname = yazi_tipi_t, part = "all")
  ft <- fontsize(ft, size = 9, part = "all")
  ft <- border_remove(ft)
  ust <- fp_border(color = "black", width = 1)
  ince <- fp_border(color = "black", width = 0.5)
  ft <- hline_top(ft, border = ust, part = "header")
  ft <- hline_bottom(ft, border = ince, part = "header")
  ft <- hline_bottom(ft, border = ust, part = "body")
  ft <- bold(ft, part = "header")
  ft <- align(ft, align = "center", part = "header")
  ft <- align(ft, j = 1, align = "left", part = "all")
  ft <- valign(ft, valign = "center", part = "header")
  ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  if (length(grup_satir)) ft <- bold(ft, i = grup_satir, j = 1, part = "body")
  for (k in seq_along(notlar)) ft <- add_footer_lines(ft, values = notlar[k])
  if (length(notlar)) {
    ft <- fontsize(ft, size = 8, part = "footer")
    ft <- font(ft, fontname = yazi_tipi_t, part = "footer")
  }
  ft <- autofit(ft)
  ft
}
simge_re <- "(_|\\^)\\{([^}]*)\\}"
duz <- function(x) gsub(simge_re, "\\2", x)
parcala <- function(s) {
  m <- gregexpr(simge_re, s, perl = TRUE)[[1]]
  bas <- c(1, m + attr(m, "match.length"))
  out <- list()
  for (k in seq_along(m)) {
    if (m[k] > bas[k]) out[[length(out) + 1]] <- as_chunk(substr(s, bas[k], m[k] - 1))
    tok <- substr(s, m[k], m[k] + attr(m, "match.length")[k] - 1)
    ic <- sub(simge_re, "\\2", tok)
    out[[length(out) + 1]] <- if (substr(tok, 1, 1) == "_") as_sub(ic) else as_sup(ic)
  }
  if (bas[length(bas)] <= nchar(s)) out[[length(out) + 1]] <- as_chunk(substr(s, bas[length(bas)], nchar(s)))
  out
}
simge_uygula <- function(ft, disp, notlar) {
  for (j in seq_len(ncol(disp))) for (i in which(grepl(simge_re, disp[[j]]))) ft <- compose(ft, i = i, j = j, part = "body", value = do.call(as_paragraph, parcala(disp[[j]][i])))
  for (k in which(grepl(simge_re, notlar))) ft <- compose(ft, i = k, j = 1, part = "footer", value = do.call(as_paragraph, parcala(notlar[k])))
  ft
}
tablo_yaz <- function(ft, dosya, baslik, yatay = FALSE) {
  doc <- read_docx()
  doc <- body_set_default_section(doc, prop_section(page_size = page_size(orient = if (yatay) "landscape" else "portrait"), page_margins = page_mar(top = 0.8, bottom = 0.8, left = 0.8, right = 0.8)))
  doc <- body_add_fpar(doc, fpar(ftext(baslik, prop = fp_text(font.family = yazi_tipi_t, font.size = 10, bold = TRUE))))
  doc <- body_add_flextable(doc, ft)
  print(doc, target = dosya)
}
wb <- createWorkbook()
xl_stil <- list(
  govde = createStyle(fontName = yazi_tipi_t, fontSize = 10, valign = "center"),
  baslik = createStyle(fontName = yazi_tipi_t, fontSize = 10, textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE),
  ust = createStyle(border = "top", borderStyle = "medium"),
  alt_ince = createStyle(border = "bottom", borderStyle = "thin"),
  alt_kalin = createStyle(border = "bottom", borderStyle = "medium"),
  not = createStyle(fontName = yazi_tipi_t, fontSize = 8),
  kalin = createStyle(textDecoration = "bold"),
  tbaslik = createStyle(fontName = yazi_tipi_t, fontSize = 10, textDecoration = "bold")
)
tablo_listesi <- list()
tablo_cikar <- function(disp, ad, baslik, ust = NULL, notlar = character(0), grup_satir = integer(0), girinti = integer(0), yatay = FALSE, klasor = "Results", kalin = list()) {
  disp <- as.data.frame(disp, stringsAsFactors = FALSE, check.names = FALSE)
  disp[] <- lapply(disp, function(x) gsub("(^|[ (\\[,;])-([0-9.])", "\\1\u2212\\2", x))
  ft <- flextable(disp)
  jj_cizgi <- integer(0)
  if (!is.null(ust)) {
    seg <- rep(ust$degerler, ust$genislik)
    alt_lab <- trimws(names(disp))
    alt_lab[ust$dikey] <- seg[ust$dikey]
    ft <- set_header_df(ft, mapping = data.frame(col_keys = names(disp), ust = seg, alt = alt_lab, stringsAsFactors = FALSE), key = "col_keys")
    ft <- merge_h(ft, i = 1, part = "header")
    ft <- merge_v(ft, part = "header")
    bas_j <- cumsum(c(1, head(ust$genislik, -1)))
    jj_cizgi <- unlist(lapply(which(ust$cizgi), function(k) bas_j[k]:(bas_j[k] + ust$genislik[k] - 1)))
  } else {
    ft <- set_header_labels(ft, values = setNames(as.list(trimws(names(disp))), names(disp)))
  }
  ft <- tablo_bicim(ft, duz(notlar), grup_satir)
  ft <- simge_uygula(ft, disp, notlar)
  ft <- font(ft, fontname = yazi_tipi_t, part = "all")
  ft <- fontsize(ft, size = 8, part = "footer")
  if (length(girinti)) ft <- padding(ft, i = girinti, j = 1, padding.left = 12, part = "body")
  for (kh in kalin) ft <- bold(ft, i = kh[1], j = kh[2], part = "body")
  if (length(jj_cizgi)) ft <- hline(ft, i = 1, j = jj_cizgi, border = fp_border(color = "black", width = 0.5), part = "header")
  ft <- align(ft, j = 1, align = "left", part = "header")
  tablo_yaz(ft, file.path(klasor, paste0(ad, ".docx")), baslik, yatay)
  disp_x <- disp
  notlar_x <- notlar
  disp[] <- lapply(disp, duz)
  notlar <- duz(notlar)
  fwrite(disp, file.path(klasor, paste0(ad, ".csv")))
  sayfa <- ad
  addWorksheet(wb, sayfa)
  writeData(wb, sayfa, baslik, startRow = 1, startCol = 1)
  addStyle(wb, sayfa, xl_stil$tbaslik, rows = 1, cols = 1)
  r0 <- 3
  nc <- ncol(disp)
  if (!is.null(ust)) {
    bas <- unlist(mapply(function(v, w) c(v, rep("", w - 1)), ust$degerler, ust$genislik))
    writeData(wb, sayfa, t(bas), startRow = r0, startCol = 1, colNames = FALSE)
    cc <- 1
    for (k in seq_along(ust$genislik)) {
      if (ust$genislik[k] > 1) mergeCells(wb, sayfa, cols = cc:(cc + ust$genislik[k] - 1), rows = r0)
      cc <- cc + ust$genislik[k]
    }
    for (j in ust$dikey) {
      mergeCells(wb, sayfa, cols = j, rows = r0:(r0 + 1))
      writeData(wb, sayfa, trimws(names(disp)[j]), startRow = r0, startCol = j)
    }
    addStyle(wb, sayfa, xl_stil$baslik, rows = r0, cols = 1:nc, gridExpand = TRUE)
    addStyle(wb, sayfa, xl_stil$ust, rows = r0, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
    r0 <- r0 + 1
    hdr <- trimws(names(disp))
    hdr[ust$dikey] <- ""
    writeData(wb, sayfa, t(hdr), startRow = r0, startCol = 1, colNames = FALSE)
  } else {
    writeData(wb, sayfa, t(trimws(names(disp))), startRow = r0, startCol = 1, colNames = FALSE)
    addStyle(wb, sayfa, xl_stil$ust, rows = r0, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  }
  addStyle(wb, sayfa, xl_stil$baslik, rows = r0, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sayfa, xl_stil$alt_ince, rows = r0, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  writeData(wb, sayfa, disp_x, startRow = r0 + 1, startCol = 1, colNames = FALSE)
  addStyle(wb, sayfa, xl_stil$govde, rows = (r0 + 1):(r0 + nrow(disp)), cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sayfa, xl_stil$alt_kalin, rows = r0 + nrow(disp), cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  if (length(grup_satir)) addStyle(wb, sayfa, xl_stil$kalin, rows = r0 + grup_satir, cols = 1, stack = TRUE)
  if (length(notlar)) {
    writeData(wb, sayfa, notlar_x, startRow = r0 + nrow(disp) + 1, startCol = 1)
    addStyle(wb, sayfa, xl_stil$not, rows = r0 + nrow(disp) + seq_along(notlar), cols = 1, stack = TRUE)
  }
  setColWidths(wb, sayfa, cols = 1:nc, widths = c(46, rep(15, nc - 1)))
  tablo_listesi[[ad]] <<- disp
  invisible(ft)
}

kat_disp <- function(tt, toplam_satir, gruplar = c("Total", "0", "1"), birim_ad) {
  satirlar <- list(toplam_satir)
  grup_i <- integer(0)
  gir_i <- integer(0)
  for (v in unique(tt$degisken)) {
    s <- tt[degisken == v]
    pv <- s$p[1]
    satirlar[[length(satirlar) + 1]] <- c(s$degisken_ad[1], rep("", 2 * length(gruplar)), pfmt(pv))
    grup_i <- c(grup_i, length(satirlar))
    for (kk in s[g == "Total"][order(sira)]$k) {
      r <- c(s[g == "Total" & k == kk]$kategori)
      for (gg in gruplar) {
        x <- s[g == gg & k == kk]
        if (!nrow(x)) {
          r <- c(r, "0", "\u2013")
        } else if (x$n < 20) {
          r <- c(r, "<20", "\u2013")
        } else {
          r <- c(r, nfmt(x$n), paste0(f1(x$pct), if (x$n <= 50) "\u2020" else ""))
        }
      }
      satirlar[[length(satirlar) + 1]] <- c(r, "")
      gir_i <- c(gir_i, length(satirlar))
    }
  }
  m <- do.call(rbind, satirlar)
  list(m = m, grup = grup_i, girinti = gir_i)
}

t1_top <- c("Adults", nfmt(nrow(yetiskin_2025)), "100.0", nfmt(t1_n[g == "0", n]), f1(t1_n[g == "0", pct]), nfmt(t1_n[g == "1", n]), f1(t1_n[g == "1", pct]), "")
t1_ekte <- c("FB100", "bki_grup_t")
k1 <- kat_disp(t1[!degisken %in% t1_ekte], t1_top)
colnames(k1$m) <- c("Characteristic", "n", "%", "n ", "% ", "n  ", "%  ", "p")
tablo_cikar(k1$m, "Table1", "Table 1. Characteristics of adults aged 18 years and older by household ability to afford a meal with meat, chicken or fish every second day, Income and Living Conditions Survey 2025.",
            ust = list(degerler = c("Characteristic", "Total", "Can afford", "Cannot afford", "p"), genislik = c(1, 2, 2, 2, 1), dikey = c(1, 8), cizgi = c(FALSE, TRUE, TRUE, TRUE, FALSE)),
            notlar = c("n, unweighted number of adults; %, weighted column percentage (weight FB030). p, Rao\u2013Scott F-adjusted chi-square test accounting for household clustering. Age is age on 31 December of the income reference year (2024).",
                       "Ability to afford: HE090 (household questionnaire). Marital status and body mass index in Supplementary Table S1. \u2020 Unweighted cell count 20\u201350; estimate may be unreliable. Cells with n < 20 are not shown."),
            grup_satir = k1$grup, girinti = k1$girinti, yatay = TRUE)

ts1_n <- hane_m[, .(n = .N, ww = sum(HB040)), keyby = .(g = as.character(A))][, pct := 100 * ww / sum(ww)]
ts1_top <- c("Households", nfmt(nrow(hane_m)), "100.0", nfmt(ts1_n[g == "0", n]), f1(ts1_n[g == "0", pct]), nfmt(ts1_n[g == "1", n]), f1(ts1_n[g == "1", pct]), "")
ks1 <- kat_disp(ts1, ts1_top)
ks1y <- kat_disp(t1[degisken %in% t1_ekte], replace(t1_top, 1, "Adults aged 18 years and older"))
ns1 <- nrow(ks1$m)
ms1 <- rbind(ks1$m, ks1y$m)
colnames(ms1) <- colnames(k1$m)
tablo_cikar(ms1, "TableS1", "Table S1. Characteristics of households, and marital status and body mass index of adults, by ability to afford a meal with meat, chicken or fish every second day, Income and Living Conditions Survey 2025.",
            ust = list(degerler = c("Characteristic", "Total", "Can afford", "Cannot afford", "p"), genislik = c(1, 2, 2, 2, 1), dikey = c(1, 8), cizgi = c(FALSE, TRUE, TRUE, TRUE, FALSE)),
            notlar = c("Household rows: n, unweighted number of households; %, weighted column percentage (household weight HB040); p, Rao\u2013Scott F-adjusted chi-square test. Equivalized income quintiles are defined over persons, so household shares differ from 20%.",
                       "Adult rows: n, unweighted number of adults; %, weighted column percentage (weight FB030); p accounts for household clustering. Body mass index from self-reported height and weight; 105 implausible values excluded.",
                       "Medical care, medicine and dental care expenditure burden refer to the past 12 months (2025 health module). \u2020 Unweighted cell count 20\u201350. Cells with n < 20 are not shown."),
            grup_satir = c(ks1$grup, ns1 + 1L, ns1 + ks1y$grup), girinti = c(ks1$girinti, ns1 + ks1y$girinti), klasor = "Results/Supplementary", yatay = TRUE)

h2_ana_tab <- rbindlist(lapply(h2_y, function(v) h2_tab[sonuc == v & model == h2_ana[[v]]]))
t2 <- rbindlist(lapply(h2_y, function(v) {
  s <- h2_ana_tab[sonuc == v]
  o <- ord_tab[sonuc == v]
  ordtxt <- if (grepl("bozuk", o$po_karar)) "\u2013^{a}" else ga(o$OR, o$alt, o$ust, 2)
  data.table(Outcome = c(`et_haftada_az` = "Meat, chicken or fish less than once a week", `meyve_gunluk_degil` = "Fruit less than once a day", `sebze_gunluk_degil` = "Vegetables or salad less than once a day")[[v]],
             n = nfmt(s$n[1]), Events = nfmt(s$olay[1]),
             `Can afford, % (95% CI)` = ga(s[olcu == "p0", tahmin], s[olcu == "p0", alt], s[olcu == "p0", ust]),
             `Cannot afford, % (95% CI)` = ga(s[olcu == "p1", tahmin], s[olcu == "p1", alt], s[olcu == "p1", ust]),
             `Difference, pp (95% CI)` = ga(s[olcu == "PD", tahmin], s[olcu == "PD", alt], s[olcu == "PD", ust]),
             `Prevalence ratio (95% CI)` = ga(s[olcu == "PR", tahmin], s[olcu == "PR", alt], s[olcu == "PR", ust], 2),
             `Ordinal OR (95% CI)` = ordtxt)
}))
tablo_cikar(t2, "Table2", "Table 2. Standardized prevalence of low-frequency consumption among adults by household food affordability, Income and Living Conditions Survey 2025.",
            notlar = c("Standardized prevalences from survey-weighted logistic regression with g-computation over the adult population (weight FB030); the model includes affordability interacted with age (natural spline, 4 df), sex, education, equivalized income quintile, region (NUTS-1), household size and number of members aged <18 years.",
                       "95% CI, percentile intervals from 4,000 household-level Rao\u2013Wu\u2013Yue\u2013Beaumont bootstrap replicates. pp, percentage points. Meat outcome excludes vegetarian or vegan adults (n = 195).",
                       "Ordinal OR, odds ratio for a lower consumption-frequency category from survey-weighted proportional odds models with the same covariates (main effects). ^{a} Proportional odds assumption not met for vegetables; cut-point-specific odds ratios in Supplementary Table S14."), yatay = TRUE)

h1_ana_tab <- h1_tab[model == h1_ana]
h1y_ana_tab <- h1y_tab[model == h1y_ana]
sat <- function(tb, k, d = 1) if (nrow(tb[olcu == k])) ga(tb[olcu == k, tahmin], tb[olcu == k, alt], tb[olcu == k, ust], d) else ""
t3_modeller <- c(minimal = "Minimal", cekirdek = "Core", cekirdek_ihtiyac = "Core + need (main)", nihai = "Fully adjusted", nihai_esnek = "Flexible")
t3_blok <- function(tab, hc, cv, ad, nn, modeller = t3_modeller, et = NULL, olc_ek = NULL) {
  if (is.null(et)) et <- c("00" = "Can afford, no adult with chronic illness", "01" = "Can afford, adult with chronic illness", "10" = "Cannot afford, no adult with chronic illness", "11" = "Cannot afford, adult with chronic illness")
  if (cv == "C_fert") et <- c("00" = "Can afford, no chronic illness", "01" = "Can afford, chronic illness", "10" = "Cannot afford, no chronic illness", "11" = "Cannot afford, chronic illness")
  bos <- function(g) {
    r <- data.table(Group = g, n = "", Events = "", `Crude %` = "")
    for (mm in names(modeller)) r[[modeller[[mm]]]] <- ""
    r
  }
  hucre <- rbindlist(lapply(names(et), function(k) {
    x <- hc[A == as.integer(substr(k, 1, 1)) & get(cv) == as.integer(substr(k, 2, 2))]
    r <- data.table(Group = et[[k]], n = nfmt(x$n), Events = nfmt(x$olay), `Crude %` = f1(x$ham_yuzde))
    for (mm in names(modeller)) r[[modeller[[mm]]]] <- sat(tab[model == mm], paste0("p", k))
    r
  }))
  olc_k <- c(beklenti11 = "Expected p_{11} under additivity, %", Delta = "Additive interaction \u0394, pp", RERI = "RERI", AP = "Attributable proportion (AP)", PR_A = "Prevalence ratio, cannot vs can afford", OR = "Product-term odds ratio (logistic)", olc_ek)
  olc_k <- olc_k[names(olc_k) %in% c(unique(tab$olcu), if (any(grepl("^OR_", tab$olcu))) "OR")]
  olc <- rbindlist(lapply(names(olc_k), function(k) {
    r <- bos(olc_k[[k]])
    for (mm in names(modeller)) {
      tb <- tab[model == mm]
      r[[modeller[[mm]]]] <- if (k == "OR") ga(tb[grepl("^OR_", olcu), tahmin], tb[grepl("^OR_", olcu), alt], tb[grepl("^OR_", olcu), ust], 2) else sat(tb, k, if (k %in% c("RERI", "AP", "PR_A")) 2 else 1)
    }
    r
  }))
  rbind(bos(paste0(ad, " (n = ", nfmt(nn), ")")), hucre, olc)
}
t3a <- t3_blok(h1_tab, hucre_h_n, "C_hane", "Households", nrow(hane_m))
t3b <- t3_blok(h1y_tab, hucre_y_n, "C_fert", "Adults aged 18 years and older", nrow(yetiskin_2025))
t3 <- rbind(t3a, t3b)
tablo_cikar(t3, "Table3", "Table 3. Household subjective burden of medical or medicine expenditure by food affordability and chronic illness, Income and Living Conditions Survey 2025: adjustment ladder.",
            ust = list(degerler = c("Group", "n", "Events", "Crude %", "Standardized % or estimate (95% CI)"), genislik = c(1, 1, 1, 1, 5), dikey = 1:4, cizgi = c(FALSE, FALSE, FALSE, FALSE, TRUE)),
            notlar = c("Outcome: medical care or medicine expenditure in the past 12 months was a heavy burden for the household (MODUL1_H_1 = 1 or MODUL1_H_3 = 1); in the adult block the household outcome is linked to each adult and the modifier is the adult's own chronic illness. n and events unweighted; crude % weighted.",
                       "Standardized prevalences from survey-weighted logistic regression with g-computation (households: weight HB040; adults: weight FB030, household clusters). Minimal: affordability \u00d7 chronic illness, age and sex (households: of the reference person) and equivalized income quintile. Core: adds to the minimal model the number of adults, members aged <18 and \u226565 years, highest education of adults and region (households) or education, region, household size and members aged <18 years (adults). Core + need (main model): adds to the core model the number of adults with activity limitation (households) or the adult's own activity limitation (adults). Fully adjusted: adds the numbers of adults with disability (WG-SS) and with bad self-rated health (households) or self-rated health (adults), which may lie on the pathway from chronic illness. Flexible: fully adjusted model with affordability interacted with every covariate.",
                       "\u0394 = p_{11} \u2212 p_{10} \u2212 p_{01} + p_{00} (first subscript affordability, second chronic illness); RERI = \u0394/p_{00}; AP = \u0394/p_{11}. 95% CI, percentile intervals from household-level bootstrap replicates (main model 4,000; other columns 2,000). pp, percentage points. The product-term odds ratio is shown for comparison only."),
            grup_satir = c(1L, nrow(t3a) + 1L), yatay = TRUE)

bos_satir <- function(ad, nc) as.data.table(as.list(setNames(c(ad, rep("", nc - 1)), paste0("V", seq_len(nc)))))

neden_tab <- neden_l[order(yil, kategori, A)]
neden_tab[, deger := fifelse(N < 20, "<20", paste0(ga(yuzde, alt, ust, 2), fifelse(N <= 50, "\u2020", "")))]
s5a <- dcast(neden_tab, yil + kategori ~ A, value.var = "deger")
setnames(s5a, c("0", "1"), c("Can afford", "Cannot afford"))
s5a <- s5a[kategori != "No unmet need"]
s5_satir <- list()
s5r <- function(it, a0 = "", a1 = "", pd = "", pr = "") data.table(Item = it, `Can afford` = a0, `Cannot afford` = a1, `Difference, pp (95% CI)` = pd, `PR (95% CI)` = pr)
s5_satir[[1]] <- s5r("Main reason for unmet medical need, adults with a medical care need, % (95% CI)")
grp <- 1L
alt_bas <- integer(0)
for (yy in 2023:2025) {
  s5_satir[[length(s5_satir) + 1]] <- s5r(as.character(yy))
  alt_bas <- c(alt_bas, length(s5_satir))
  for (kk in c("Cost", "System (distance, waiting time)", "Personal (time, fear, wait-and-see, other)")) {
    x <- s5a[yil == yy & kategori == kk]
    s5_satir[[length(s5_satir) + 1]] <- s5r(kk, x$`Can afford`, x$`Cannot afford`)
  }
}
s5_satir[[length(s5_satir) + 1]] <- s5r("Standardized prevalence, 2025, % (95% CI)")
grp <- c(grp, length(s5_satir))
ik <- function(a) ikincil[analiz == a]
s5_kalin <- list()
for (cc in list(list("odeme_tibbi_AxC", "Unmet medical need because of cost (adults with a medical care need)"), list("odeme_herhangi_AxC", "Unmet medical or dental need because of cost (adults with a medical or dental care need)"))) {
  t <- ik(cc[[1]])
  hc <- ikincil_hucre[analiz == cc[[1]]]
  s5_satir[[length(s5_satir) + 1]] <- s5r(paste0(cc[[2]], ", all adults"), "", "", sat(t, "PD_A", 2), sat(t, "PR_A", 2))
  alt_bas <- c(alt_bas, length(s5_satir))
  ref_az <- hc[A == 0 & C_fert == 0, olay] < 20
  for (ci in 0:1) {
    v0 <- hc[A == 0 & C_fert == ci]
    v1 <- hc[A == 1 & C_fert == ci]
    lab <- paste0("Difference within stratum: ", if (ci == 0) "no chronic illness" else "chronic illness", " (events ", v0$olay, " / ", v1$olay, ")")
    pdc <- paste0(sat(t, paste0("PD_C", ci), 2), if (ci == 0 && ref_az) "\u2021" else "")
    s5_satir[[length(s5_satir) + 1]] <- s5r(lab, if (v0$olay < 20) "<20 events" else sat(t, paste0("p0", ci), 2), if (v1$olay < 20) "<20 events" else sat(t, paste0("p1", ci), 2), pdc)
    s5_kalin[[length(s5_kalin) + 1]] <- c(length(s5_satir), 4L)
  }
  s5_satir[[length(s5_satir) + 1]] <- s5r(paste0("Additive interaction \u0394 with chronic illness, pp (exploratory", if (ref_az) "; reference cell <20 events)" else ")"), "", "", paste0(sat(t, "Delta", 2), if (ref_az) "\u2021" else ""))
}
for (cc in list(list("uzman_hic_kronik_A", "No specialist visit in past 12 months, adults with chronic illness"), list("uzman_hic_kronik_A_boslar_hic", "No specialist visit, routing blanks counted as no visit"),
                list("dis_yuk_A", "Dental expenditure a heavy burden, households"), list("dis_yuk_A_harcama_var", "Dental expenditure a heavy burden, households with dental expenditure"),
                list("dis_harcama_yok_A", "No dental expenditure in past 12 months, households"))) {
  t <- ik(cc[[1]])
  s5_satir[[length(s5_satir) + 1]] <- s5r(cc[[2]], sat(t, "p0"), sat(t, "p1"), sat(t, "PD"), sat(t, "PR", 2))
}
s5 <- rbindlist(s5_satir)
gir5 <- setdiff(seq_len(nrow(s5)), c(grp, alt_bas))
tablo_cikar(s5, "TableS5", "Table S5. Unmet need for medical and dental care, reasons, specialist visits and dental expenditure burden by household food affordability.",
            notlar = c("Reasons: adults aged 18+ reporting a medical care need in each survey year (FS050 = 1 or 2); cost = FS060 code 1; system = distance or transport (3) and late appointment (5); personal = time, fear, waiting for the problem to resolve, not knowing a good doctor, other (2, 4, 6, 7, 8). Weighted % with 95% CI (household clusters).",
                       "Standardized prevalences (2025) from survey-weighted logistic models with g-computation; covariates for unmet need: age spline, sex, income quintile, region, education; specialist visits: core adult covariates and activity limitation; dental burden: household main-model covariates (core + need) and chronic illness. Differences within chronic-illness strata (bold) are the primary contrasts for unmet need; \u0394 is exploratory. 95% CI, bootstrap percentiles (2,000 replicates). Cells with <20 events are not shown separately; their model-based values enter the contrasts. \u2020 20\u201350 observations. \u2021 Uses the reference cell (can afford, no chronic illness) with 19 events."),
            grup_satir = grp, girinti = gir5, klasor = "Results/Supplementary", yatay = TRUE, kalin = s5_kalin)

s6h <- orman[, .(Analysis = aciklama, Level = fifelse(grepl("^10", analiz), "Adults", "Households"), n = nfmt(n), Events = nfmt(olay), `Estimate (95% CI)` = ga(tahmin, alt, ust, 2))]
s6h[, Measure := "\u0394, pp"]
s6d <- h2_duy[olcu == "PD", .(Analysis = paste0(c(`et_haftada_az` = "Meat less than weekly", `meyve_gunluk_degil` = "Fruit less than daily", `sebze_gunluk_degil` = "Vegetables less than daily")[sonuc], ": ", analiz),
                               Level = "Adults", n = "", Events = "", `Estimate (95% CI)` = fifelse(is.na(alt), paste0(f2(tahmin), " (\u2013)"), ga(tahmin, alt, ust, 2)), Measure = "Difference, pp")]
s6 <- rbind(data.table(Analysis = "Hypothesis 1: additive interaction between affordability and chronic illness", Level = "", n = "", Events = "", `Estimate (95% CI)` = "", Measure = ""), s6h,
            data.table(Analysis = "Hypothesis 2: standardized prevalence difference, cannot vs can afford", Level = "", n = "", Events = "", `Estimate (95% CI)` = "", Measure = ""), s6d)
setcolorder(s6, c("Analysis", "Level", "Measure", "n", "Events", "Estimate (95% CI)"))
tablo_cikar(s6, "TableS6", "Table S6. Sensitivity analyses.",
            notlar = c("Unless stated otherwise, each row re-estimates the main model with one change (Hypothesis 1: main-effects logistic g-computation with core and need covariates; Hypothesis 2: flexible logistic g-computation). In the two activity-limitation modifier rows the number of adults with activity limitation is removed from the covariates; the second row adds the number of adults with chronic illness (0, 1, 2, \u22653). 95% CI, bootstrap percentiles (main estimates 4,000, other rows 2,000 household-level replicates). The overlapping-strata row restricts standardization to income \u00d7 region \u00d7 education strata in which every cell had \u226520 observations; for Hypothesis 2 only linearization-based uncertainty is computed for that row and is not shown.",
                       "No household clustering: linearization standard errors treating adults as independent (shown to document the clustering effect only)."),
            grup_satir = c(1L, nrow(s6h) + 2L), girinti = setdiff(seq_len(nrow(s6)), c(1L, nrow(s6h) + 2L)), klasor = "Results/Supplementary", yatay = TRUE)

s8 <- copy(degisken_tanimlari)
setnames(s8, c("Variable", "Source variables", "Coding", "Universe", "Missing data rule"))
tablo_cikar(s8, "TableS8", "Table S8. Definitions of analytic variables.", klasor = "Results/Supplementary", yatay = TRUE)

ehc <- ikincil_hucre[analiz == "eski_hipotez_AxB"]
eht <- ik("eski_hipotez_AxB")
s9 <- rbindlist(lapply(c("00", "01", "10", "11"), function(k) {
  x <- ehc[A == as.integer(substr(k, 1, 1)) & B == as.integer(substr(k, 2, 2))]
  data.table(Group = c("00" = "Can afford, daily fruit and vegetables", "01" = "Can afford, not both daily", "10" = "Cannot afford, daily fruit and vegetables", "11" = "Cannot afford, not both daily")[[k]],
             n = nfmt(x$n), Events = nfmt(x$olay), `Standardized % (95% CI)` = if (x$olay < 20) "<20 events" else paste0(sat(eht, paste0("p", k), 2), if (x$olay <= 50) "\u2020" else ""))
}))
s9 <- rbind(s9, data.table(Group = c("Additive interaction \u0394, pp", "Difference cannot vs can afford (pp)", "Prevalence ratio cannot vs can afford", "Events per model parameter"),
                           n = "", Events = "", `Standardized % (95% CI)` = c(sat(eht, "Delta", 2), sat(eht, "PD_A", 2), sat(eht, "PR_A", 2), f1(eh_epv))))
tablo_cikar(s9, "TableS9", "Table S9. Exploratory analysis: unmet medical need because of cost by food affordability and fruit and vegetable consumption (adults reporting a medical care need).",
            notlar = c("Survey-weighted logistic model with affordability \u00d7 fruit and vegetable consumption, age spline, sex, income quintile and region; g-computation; 95% CI from 2,000 household bootstrap replicates. The cell with <20 events is not shown separately; \u0394 uses its model-based value and is exploratory. \u2020 20\u201350 events.",
                       paste0("Log odds ratio of the product term: survey-weighted ", f2(coef(sg_eh)["A:B"]), "; unweighted maximum likelihood ", f2(coef(ml_eh)["A:B"]), "; unweighted Firth ", f2(coef(firth_eh)["A:B"]), ".")),
            klasor = "Results/Supplementary")

s10 <- rbindlist(lapply(list(c("odeme_tibbi", "Unmet medical need because of cost"), c("odeme_dis", "Unmet dental need because of cost"), c("uzman_hic", "No specialist visit in past 12 months")), function(z) {
  rbindlist(lapply(c("reis_degil", "reis"), function(r) {
    t <- ik(paste0("yanitlayici_", z[1], "_", r))
    data.table(Outcome = z[2], Respondent = if (r == "reis") "Household reference persons" else "Other adults (not household reference person)", n = nfmt(t$n[1]), Events = nfmt(t$olay[1]),
               `Can afford, %` = sat(t, "p0", 2), `Cannot afford, %` = sat(t, "p1", 2), `Difference, pp` = sat(t, "PD", 2), `Prevalence ratio` = sat(t, "PR", 2))
  }))
}))
tablo_cikar(s10, "TableS10", "Table S10. Household-respondent check: associations of household food affordability with outcomes reported by each adult in the individual questionnaire.",
            notlar = c("The affordability item is answered for the household; the outcomes come from each adult's individual interview. Adults who are not the household reference person are less likely to have answered the household questionnaire themselves, so their associations are less exposed, but not immune, to same-respondent reporting; the household respondent is not identified in the data. Standardized prevalences from survey-weighted logistic models with g-computation (cost outcomes: age, sex and income quintile because of few events; specialist visits: age spline, sex, income quintile, region, education, activity limitation); 95% CI, 2,000 household bootstrap replicates."),
            klasor = "Results/Supplementary", yatay = TRUE)

mk_ad <- c(minimal = "Minimal", cekirdek_ihtiyac = "Core + need", cekirdek = "Core", nihai = "Final (main effects)", nihai_esnek = "Final, flexible (main)")
mk_ad_h1 <- c(minimal = "Minimal", cekirdek = "Core (sociodemographic, no need indicator)", cekirdek_ihtiyac = "Core + need (main)", nihai = "Fully adjusted", nihai_esnek = "Flexible, fully adjusted", cekirdek_esnek = "Flexible, core + need")
s11a <- rbindlist(lapply(list(list(h1_tab, "Households"), list(h1y_tab, "Adults")), function(z) {
  tb <- z[[1]][model %in% names(mk_ad_h1)]
  rbindlist(lapply(intersect(names(mk_ad_h1), unique(tb$model)), function(mm) {
    t <- tb[model == mm]
    data.table(Level = z[[2]], Model = mk_ad_h1[[mm]], Parameters = t$npar[1], Delta_pp = sat(t, "Delta", 2), RERI = sat(t, "RERI", 2), `PR cannot vs can afford` = sat(t, "PR_A", 2), `Product-term OR` = ga(t[grepl("^OR_", olcu), tahmin], t[grepl("^OR_", olcu), alt], t[grepl("^OR_", olcu), ust], 2))
  }))
}))
s11b <- rbindlist(lapply(h2_y, function(v) {
  tb <- h2_tab[sonuc == v & model %in% names(mk_ad)]
  rbindlist(lapply(c("minimal", "cekirdek", "nihai", "nihai_esnek"), function(mm) {
    t <- tb[model == mm]
    data.table(Level = ETIKET_H2[[v]], Model = mk_ad[[mm]], Parameters = t$npar[1], Delta_pp = "", RERI = "", `PR cannot vs can afford` = sat(t, "PR", 2), `Product-term OR` = "", `Difference, pp` = sat(t, "PD", 2))
  }))
}))
s11a[, `Difference, pp` := ""]
s11 <- rbind(s11a, s11b)
setnames(s11, "Level", "Outcome / level")
setcolorder(s11, c("Outcome / level", "Model", "Parameters", "Delta_pp", "RERI", "Difference, pp", "PR cannot vs can afford", "Product-term OR"))
setnames(s11, "Delta_pp", "\u0394, pp")
tablo_cikar(s11, "TableS11", "Table S11. Comparison of minimal, core, final and flexible final models.",
            notlar = c("Hypothesis 1 rows: household (weight HB040) and adult (weight FB030, household clusters) burden of medical or medicine expenditure; Hypothesis 2 rows: low-frequency consumption. Minimal: age, sex and income quintile (household: reference person); core: all sociodemographic covariates of the analysis plan (in addition to the minimal covariates, number of adults, members aged <18 and \u226565 years, highest education of adults and region for households; education, region, household size and members aged <18 years for adults) without a need indicator; core + need (main model for Hypothesis 1): core plus the number of adults with activity limitation (households) or the adult's own activity limitation (adults); final: core plus need plus the covariates selected by the change-in-estimate rule (for Hypothesis 2, core plus the selected covariates; none was selected); flexible: final model with affordability interacted with every covariate (the main model for Hypothesis 2 under the refined model-form rule and a sensitivity model for Hypothesis 1). 95% CI, bootstrap percentiles."),
            klasor = "Results/Supplementary", yatay = TRUE)

aday_tum <- rbind(aday_h2[, .(Model = paste("H2:", ETIKET_H2[sonuc]), Candidate = aday, Before = PD_once, After = PD_sonra, PR_before = PR_once, PR_after = PR_sonra, SE_change = se_degisim_yuzde, Wald_p = wald_p, Decision = karar)],
                  rbind(aday_h1, aday_h1_birlikte, fill = TRUE)[, .(Model = "H1: households, \u0394", Candidate = aday, Before = Delta_once, After = Delta_sonra, PR_before = PR_once, PR_after = PR_sonra, SE_change = 100 * (se_sonra - se_once) / se_once, Wald_p = wald_p, Decision = karar)],
                  aday_h1y[, .(Model = "H1: adults, \u0394", Candidate = aday, Before = Delta_once, After = Delta_sonra, PR_before = PR_once, PR_after = PR_sonra, SE_change = se_degisim_yuzde, Wald_p = wald_p, Decision = karar)])
karar_en <- c("Girmez" = "Not included", "Girer (tahmin degisimi)" = "Included (change in estimate)", "Girer (kesinlik)" = "Included (precision)", "Birlikte ekleme (secilenler tutuldu)" = "Joint addition (selected kept)")
s12 <- aday_tum[, .(Model, Candidate, `Estimate before` = f2(Before), `Estimate after` = f2(After), `PR before` = f2(PR_before), `PR after` = f2(PR_after), `SE change, %` = f1(SE_change), `Wald p` = pfmt(Wald_p), Decision = karar_en[Decision])]
tablo_cikar(s12, "TableS12", "Table S12. Covariate selection by the change-in-estimate rule.",
            notlar = c("Each candidate was added one at a time to the core (+ need) model. Estimate: standardized prevalence difference in percentage points for Hypothesis 2 and additive interaction \u0394 for Hypothesis 1; PR, standardized prevalence ratio of affordability. Inclusion rule: change in the difference \u22650.5 pp (Hypothesis 2) or in \u0394 \u22650.25 pp (Hypothesis 1), or relative change in PR \u226510%; candidates with design-based Wald p < 0.05 could also enter if they reduced the standard error by >5%. Stepwise selection by p values was not used."),
            klasor = "Results/Supplementary", yatay = TRUE)

s13 <- h2_gelir[, .(Outcome = ETIKET_H2[sonuc], `Income quintile` = fifelse(gelir == "T", "All adults", c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")[suppressWarnings(as.integer(gelir))]),
                    `n can / cannot afford` = paste0(nfmt(n_A0), " / ", nfmt(n_A1)), `Can afford, %` = f1(p0), `Cannot afford, %` = f1(p1),
                    `Difference, pp (95% CI)` = ga(PD, PD_alt, PD_ust), `PR (95% CI)` = ga(PR, PR_alt, PR_ust, 2))]
s13k <- gida_kars[, .(Outcome = paste0("Comparison: ", sub("et_haftada_az", "meat", sub("meyve_gunluk_degil", "fruit", sub("sebze_gunluk_degil", "vegetables", sub("vs (.*)", "vs \\1", karsilastirma))))),
                      `Income quintile` = "All adults", `n can / cannot afford` = olcu, `Can afford, %` = "", `Cannot afford, %` = "",
                      `Difference, pp (95% CI)` = paste0(ga(fark, fark_alt, fark_ust, 2), "; p = ", pfmt(p_boot)), `PR (95% CI)` = ga(oran, oran_alt, oran_ust, 2))]
setnames(s13k, "n can / cannot afford", "n can / cannot afford")
tablo_cikar(rbind(s13, s13k), "TableS13", "Table S13. Standardized prevalence of low-frequency consumption by food affordability within equivalized income quintiles, and comparison between food groups.",
            notlar = c("Standardized prevalences from the flexible survey-weighted logistic model (affordability interacted with all covariates, including income quintile), standardized to the adults of each quintile. 95% CI, bootstrap percentiles (4,000 replicates).",
                       "Comparison rows: difference and ratio of the prevalence ratios (or prevalence differences) of two food groups computed within each paired bootstrap replicate; p, two-sided bootstrap p value. Exploratory."),
            klasor = "Results/Supplementary", yatay = TRUE)

s14 <- rbind(ord_tab[, .(Outcome = ETIKET_H2[sonuc], Model = "Proportional odds (svyolr)", Threshold = "All", `OR (95% CI)` = ga(OR, alt, ust, 2), Events = "")],
             po_tab[, .(Outcome = ETIKET_H2[sonuc], Model = "Cut-point logistic", Threshold = kesme_ad, `OR (95% CI)` = ga(OR, alt, ust, 2), Events = nfmt(olay))])
s14 <- s14[order(match(Outcome, ETIKET_H2[h2_y]))]
tablo_cikar(s14, "TableS14", "Table S14. Ordinal models of consumption frequency and proportional odds check.",
            notlar = c("Odds ratios for less frequent consumption among adults in households that cannot afford the meal. Cut-point models: binary logistic regression of consumption at or below the stated frequency; same covariates as the main-effects model. Proportional odds was judged violated if cut-point odds ratios differed by >30% or their CIs did not overlap (vegetables)."),
            klasor = "Results/Supplementary")

bg <- baglam[kirilim %in% c("Total", "gelir_dilim5", "HB030", "HB050")]
bg[, Category := fcase(kirilim == "Total" & tr63_haric, "Total excluding TR63", kirilim == "Total", "Total", kirilim == "gelir_dilim5", paste0("Income ", c("Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)")[suppressWarnings(as.integer(kategori))]),
                       kirilim == "HB030", etiket$HB030[kategori], kirilim == "HB050", etiket$HB050[kategori])]
bg <- bg[!(kirilim == "gelir_dilim5" & tr63_haric)]
bg[, deger := fifelse(N < 20, "<20", ga(yuzde, alt, ust))]
s15 <- dcast(bg, kirilim + tr63_haric + Category ~ duzey + yil, value.var = "deger")
s15[, sira := match(kirilim, c("Total", "gelir_dilim5", "HB030", "HB050"))]
s15 <- s15[order(sira, tr63_haric)]
s15 <- s15[, .(Category, `Households 2023` = hane_2023, `Households 2024` = hane_2024, `Households 2025` = hane_2025, `Persons 2023` = kisi_2023, `Persons 2024` = kisi_2024, `Persons 2025` = kisi_2025)]
tablo_cikar(s15, "TableS15", "Table S15. Share unable to afford a meal with meat, chicken or fish every second day, 2023\u20132025, households and persons, % (95% CI).",
            notlar = c("Households weighted by HB040; persons (all ages) weighted by FK050 with household clusters; each survey year estimated separately (repeated cross-sections, not pooled). The 2023 survey has no households from TR63 (Hatay, Kahramanmara\u015f, Osmaniye); 'Total excluding TR63' restricts 2024 and 2025 to the 25 regions covered in 2023. Person-level totals equal the Eurostat ilc_mdes03 values for T\u00fcrkiye (39.1, 39.3, 35.1)."),
            klasor = "Results/Supplementary", yatay = TRUE)
s16 <- h2_bolge[, .(Outcome = ETIKET_H2[sonuc], Region = bolge_ad, `n can / cannot afford` = paste0(nfmt(n_A0), " / ", nfmt(n_A1)),
                    `Difference, pp (95% CI)` = paste0(ga(PD, PD_alt, PD_ust), fifelse(kucuk == "flag", "\u2020", "")), `PR (95% CI)` = ga(PR, PR_alt, PR_ust, 2))]
tablo_cikar(s16, "TableS16", "Table S16. Standardized prevalence difference of low-frequency consumption by food affordability within NUTS-1 regions (exploratory).",
            notlar = c("Effect-modification screen (protocol step 6): affordability \u00d7 region interaction Wald p < 0.01 for all three outcomes. Estimates from the flexible survey-weighted logistic model standardized to the adults of each region; 95% CI, bootstrap percentiles (2,000 replicates). The national estimate remains the main result. \u2020 20\u201350 adults in a group."),
            klasor = "Results/Supplementary", yatay = TRUE)

hg <- hucre_svy(des_h, "Y_saglik_yuk", "A", "C_hane_gali", hane_m, "HB040")
hucre_g_n <- hg$hucre
fwrite(hucre_g_n, "Results/on_kontroller/ok2_hucre_hane_gali.csv")
print(hucre_g_n)
gali_tab <- duy[analiz %in% c("04c_gali", "04d_gali_kronik")][, model := analiz]
s17 <- t3_blok(gali_tab, hucre_g_n, "C_hane_gali", "Households", nrow(hane_m),
               modeller = c(`04c_gali` = "Core covariates", `04d_gali_kronik` = "+ Adults with chronic illness"),
               et = c("00" = "Can afford, no adult with activity limitation", "01" = "Can afford, adult with activity limitation", "10" = "Cannot afford, no adult with activity limitation", "11" = "Cannot afford, adult with activity limitation"),
               olc_ek = c(PD_C0 = "Difference, cannot vs can afford: no adult with activity limitation, pp", PD_C1 = "Difference, cannot vs can afford: adult with activity limitation, pp"))
tablo_cikar(s17, "TableS17", "Table S17. Household subjective burden of medical or medicine expenditure by food affordability and activity limitation (exploratory).",
            ust = list(degerler = c("Group", "n", "Events", "Crude %", "Standardized % or estimate (95% CI)"), genislik = c(1, 1, 1, 1, 2), dikey = 1:4, cizgi = c(FALSE, FALSE, FALSE, FALSE, TRUE)),
            notlar = c("Exploratory analysis. Modifier: at least one adult in the household with a long-standing limitation in usual activities because of health problems (GALI, FS030 = 1 or 2). Outcome as in Table 3. n and events unweighted; crude % weighted (HB040).",
                       "Standardized prevalences from survey-weighted main-effects logistic regression with g-computation. Core covariates: equivalized income quintile, number of adults, members aged <18 and \u226565 years, age and sex of the reference person, highest education of adults and region; the number of adults with activity limitation is not included because it defines the modifier. Second column: additionally adjusted for the number of adults with chronic illness (0, 1, 2, \u22653); this column was added after the first had been inspected.",
                       "\u0394 = p_{11} \u2212 p_{10} \u2212 p_{01} + p_{00} (first subscript affordability, second activity limitation); RERI = \u0394/p_{00}; AP = \u0394/p_{11}. 95% CI, percentile intervals from 2,000 household-level bootstrap replicates. pp, percentage points."),
            grup_satir = 1L, klasor = "Results/Supplementary")
lca_gerekli <- c(file.path(ddir, "lca_siniflar.parquet"), "Results/lca_uyum.csv", "Results/lca_madde_olasiliklari.csv", "Results/lca_bch_uzak_sonuclar.csv")
if (!all(file.exists(lca_gerekli))) {
  t0 <- Sys.time()
  system2(py, "Codes/lca.py", stdout = "Results/log/lca_konsol.txt", stderr = "Results/log/lca_konsol.txt")
  cat("lca.py suresi (dk):", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), "\n")
}
lca_uyum <- fread("Results/lca_uyum.csv")
lca_madde <- fread("Results/lca_madde_olasiliklari.csv")
lca_pay <- fread("Results/lca_sinif_paylari.csv")
lca_bch <- fread("Results/lca_bch_uzak_sonuclar.csv")
lca_mn <- fread("Results/lca_multinomial.csv")
lca_ana <- "ortak"
lca_K <- lca_madde[model == lca_ana & K == 4L, K[1]]
s2 <- lca_uyum[, .(Model = model, Classes = K, `Log-likelihood` = f1(LL), Parameters = npar, BIC = f1(BIC), aBIC = f1(aBIC), CAIC = f1(CAIC),
                   Entropy = fifelse(is.na(rel_entropi), "\u2013", f2(rel_entropi)), `Smallest class, %` = f1(en_kucuk_sinif_yuzde),
                   `Best LL replicated, %` = f1(en_iyi_tekrar_yuzde), `BLRT p` = fifelse(is.na(BLRT_p), "\u2013", formatC(BLRT_p, format = "f", digits = 3)))]
tablo_cikar(s2, "TableS2", "Table S2. Fit statistics of latent class models with 1 to 6 classes, adults aged 18 years and older.",
            notlar = c("Weighted (FB030) latent class models estimated with StepMix; LL, log-likelihood of the best of the random starts; BIC, Bayesian information criterion; aBIC, sample-size adjusted BIC; CAIC, consistent Akaike information criterion; entropy, relative entropy; best LL replicated, share of random starts reaching the best log-likelihood; BLRT, bootstrap likelihood ratio test of K vs K \u2212 1 classes.",
                       "Model labels as in the analysis log; the main model is described in the Methods notes."),
            klasor = "Results/Supplementary", yatay = TRUE)
lm_sec <- lca_madde[model == lca_ana & K == lca_K]
sinif_ad <- unique(lm_sec[, .(sinif, sinif_ad, sinif_payi)])[order(sinif)]
s3 <- dcast(lm_sec, gosterge_ad + kategori_kod + kategori ~ sinif, value.var = "olasilik")
s3 <- s3[order(match(gosterge_ad, unique(lm_sec$gosterge_ad)), kategori_kod)]
sutun <- as.character(sinif_ad$sinif)
s3d <- s3[, c(list(Indicator = gosterge_ad, Category = kategori), lapply(.SD, function(x) f2(x))), .SDcols = sutun]
setnames(s3d, sutun, paste0(sinif_ad$sinif_ad, " (", f1(sinif_ad$sinif_payi), "%)"))
tablo_cikar(s3d, "TableS3", paste0("Table S3. Item-response probabilities and weighted class shares of the ", lca_K, "-class model."),
            notlar = c("Probabilities of each indicator category within each latent class; column headers give the weighted class share (FB030). Neighbouring solutions (K \u2212 1 and K + 1 classes) are in Results/lca_madde_olasiliklari.csv and the workbook."),
            klasor = "Results/Supplementary", yatay = TRUE)
lb <- lca_bch[model == lca_ana]
s4 <- lb[, .(Outcome = sonuc_ad, Adjustment = ayar, Class = sinif_ad, Measure = olcu, `Estimate (95% CI)` = fifelse(is.na(alt), f2(tahmin), ga(tahmin, alt, ust, 2)), `Modal n` = nfmt(modal_n), `Modal events` = nfmt(modal_olay))]
tablo_cikar(s4, "TableS4", "Table S4. Distal outcomes by latent class: three-step BCH estimates, unadjusted and covariate-adjusted.",
            notlar = c("BCH three-step estimates with survey weight FB030; adjusted estimates standardized over the adult population with covariates income quintile, age, sex, education, region and household type. 95% CI from household-level bootstrap with posterior probabilities held fixed. Household burden is linked to each adult, so it estimates the share of adults in a profile who live in a high-burden household. Unmet medical need because of cost is estimated among adults reporting a medical care need."),
            klasor = "Results/Supplementary", yatay = TRUE)
lmn <- lca_mn[model == lca_ana]
s4b <- lmn[, .(Class = sinif_ad, Reference = referans, Term = terim, `OR (95% CI)` = ga(OR, OR_alt, OR_ust, 2))]
tablo_cikar(s4b, "TableS4b", "Table S4b. Class membership by covariates: multinomial model with three-step BCH correction.", klasor = "Results/Supplementary", yatay = TRUE)
lca_sayilar <- list(model = lca_ana, K = lca_K, siniflar = sinif_ad, uyum = lca_uyum[model == lca_ana], uzak = lb[, .(sonuc, ayar, sinif, sinif_ad, olcu, tahmin, alt, ust)])

sim_dosya <- file.path(ddir, "simulasyon", "simulasyon_ozet.csv")
if (!file.exists(sim_dosya)) {
  t0 <- Sys.time()
  system2("Rscript", c("Codes/simulasyon.R", "tam"), stdout = "Results/log/simulasyon_konsol.txt", stderr = "Results/log/simulasyon_konsol.txt")
  cat("simulasyon.R suresi (dk):", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), "\n")
}
sim_ozet <- fread(sim_dosya)
tah_ad <- c(lojistik_esnek = "Flexible logistic g-computation", lojistik_ana_etki = "Main-effects logistic g-computation (H1 main estimator)", dogrusal_olasilik = "Linear probability product term", ana_delta_yontemi = "Main-effects, delta-method CI", ana_bootstrap_yuzdelik = "Main-effects, percentile bootstrap CI",
            kumeli_se = "Household-clustered SE", kumesiz_se = "SE ignoring clustering", esnek_delta_yontemi = "Flexible, delta-method CI", esnek_bootstrap_yuzdelik = "Flexible, percentile bootstrap CI")
yan_ad <- c(yok = "None", toplamsal = "Additive false positives (0.02)", carpimsal = "Multiplicative (\u00d71.3)")
s7 <- sim_ozet[, .(Level = c(hane = "Households", yetiskin = "Adults", hane_bootstrap = "Households (bootstrap)")[duzey], True_Delta = f1(gercek_Delta), `Response bias` = yan_ad[yanlilik_turu],
                   ICC = fifelse(is.na(rho), "", f1(rho)), Estimator = tah_ad[tahminci], R = nfmt(R), Bias = f2(yanlilik), `Empirical SE` = f2(ampirik_se), `Model SE / empirical SE` = f2(se_orani),
                   Coverage = formatC(kapsama, format = "f", digits = 3), `Rejection rate` = formatC(red_iki_yonlu, format = "f", digits = 3), `Product-term rejection` = fifelse(is.na(red_carpim), "", formatC(red_carpim, format = "f", digits = 3)))]
setnames(s7, "True_Delta", "True \u0394")
tablo_cikar(s7, "TableS7", "Table S7. Plasmode simulation: performance of estimators of the additive interaction \u0394 on the 2025 household and adult samples.",
            notlar = c("Outcome generated on the additive scale on the real 2025 data (covariates, exposure, modifier and weights fixed): p = baseline + \u03b2A\u00b7A + \u03b2C\u00b7C + \u0394\u00b7A\u00b7C, truncated to [0.005, 0.95]. Bias and SE in percentage points; coverage of nominal 95% intervals; rejection rate of two-sided 5% tests (type I error when \u0394 = 0, power otherwise); product-term rejection: Wald test of the logistic product term (or of the LPM product term). Adult scenarios induce household dependence via a Gaussian copula with latent intraclass correlation (ICC). Replicates: 2,000 (\u0394 = 0), 1,000 otherwise; bootstrap rows 200 data sets \u00d7 200 replicates."),
            klasor = "Results/Supplementary", yatay = TRUE)
lca_post <- as.data.table(read_parquet(file.path(ddir, "lca_siniflar.parquet")))
pg <- yetiskin_2025[, .(fert_kimlik, ortak = as.integer(A * 3L + fcase(et_tavuk_balik_siklik <= 1, 1L, et_tavuk_balik_siklik == 2, 2L, default = 3L)),
                        meyve4 = fcase(meyve_siklik <= 1, 1L, meyve_siklik == 2, 2L, meyve_siklik == 3, 3L, default = 4L),
                        sebze4 = fcase(sebze_siklik <= 1, 1L, sebze_siklik == 2, 2L, sebze_siklik == 3, 3L, default = 4L))]
set.seed(20251028)
t0 <- Sys.time()
pl <- poLCA(cbind(ortak, meyve4, sebze4) ~ 1, data = as.data.frame(pg), nclass = 4, nrep = 10, maxiter = 5000, verbose = FALSE, calc.se = FALSE)
pg[, polca := pl$predclass]
pg <- merge(pg, lca_post[, .(fert_kimlik, stepmix = sinif)], by = "fert_kimlik")
ct <- table(pg$polca, pg$stepmix)
perm <- function(v) if (length(v) <= 1) list(v) else do.call(c, lapply(seq_along(v), function(i) lapply(perm(v[-i]), function(r) c(v[i], r))))
pp <- perm(seq_len(nrow(ct)))
uyum <- sapply(pp, function(o) sum(ct[cbind(o, seq_len(ncol(ct)))]))
eslesme <- pp[[which.max(uyum)]]
polca_ozet <- data.table(stepmix_sinif = seq_len(ncol(ct)), polca_sinif = eslesme, polca_pay_agirliksiz = 100 * pl$P[eslesme],
                         stepmix_pay_agirliksiz = 100 * as.numeric(colSums(ct)) / sum(ct), modal_uyum_yuzde = 100 * max(uyum) / sum(ct), polca_LL = pl$llik, polca_BIC = pl$bic,
                         sure_sn = as.numeric(difftime(Sys.time(), t0, units = "secs")))
fwrite(polca_ozet, "Results/log/polca_capraz_kontrol.csv")
ozet_yaz("LCA capraz kontrol: poLCA (agirliksiz, 4 sinif, 10 baslangic) ile StepMix ana model modal sinif uyumu", list(capraz_tablo = ct, ozet = polca_ozet))
prot_ekle("10. LCA capraz kontrol", "Agirliksiz poLCA ile StepMix modal siniflarinin uyumu", paste0("Eslestirilmis modal uyum %", fm(max(uyum) / sum(ct) * 100, 1), "; poLCA sinif paylari ", paste(fm(100 * pl$P[eslesme], 1), collapse = " / ")),
          "StepMix cozumu poLCA ile yeniden uretildi (agirliksiz; agirlikli paylardan fark beklenir)", "LCA ana model")
fwrite(rbindlist(protokol), "Results/model_protokolu.csv")
saveWorkbook(wb, "Results/tablolar_tumu.xlsx", overwrite = TRUE)
system2(py, c("-I", "Codes/xlsx_simge.py", shQuote("Results/tablolar_tumu.xlsx")))
sj <- function(tb, k) {
  x <- tb[olcu == k]
  if (!nrow(x)) return(NULL)
  list(tahmin = round(x$tahmin, 4), alt = round(x$alt, 4), ust = round(x$ust, 4), se_boot = round(x$se_boot, 4), se_dogrusallastirma = round(x$se_if, 4))
}
sayilar <- list(
  orneklem = list(hane_2025 = nrow(hane_2025), hane_analitik = nrow(hane_m), yetiskin_analitik = nrow(yetiskin_2025), fert_2025 = nrow(d25), fert_anketi_2025 = nrow(f25),
                  hane_A1 = hane_m[A == 1, .N], hane_C1 = hane_m[C_hane == 1, .N], yetiskin_A1 = yetiskin_2025[A == 1, .N], Y_olay_hane = sum(hane_m$Y_saglik_yuk),
                  Y_olay_yetiskin = sum(yetiskin_2025$Y_saglik_yuk), vejetaryen = yetiskin_2025[vejetaryen == 1, .N], ihtiyac_var = yetiskin_2025[ihtiyac_var == 1, .N]),
  agirlikli_yuzde = list(Y_saglik_yuk_hane = ok1b[degisken == "Y_saglik_yuk", as.list(round(c(yuzde = yuzde, alt = alt, ust = ust), 3))],
                         Y_tibbi_yuk_hane = ok1b[degisken == "Y_tibbi_yuk", as.list(round(c(yuzde = yuzde, alt = alt, ust = ust), 3))],
                         Y_ilac_yuk_hane = ok1b[degisken == "Y_ilac_yuk", as.list(round(c(yuzde = yuzde, alt = alt, ust = ust), 3))],
                         A_hane_2025 = round(100 * hane_m[, weighted.mean(A, HB040)], 3), A_yetiskin_2025 = round(100 * yetiskin_2025[, weighted.mean(A, FB030)], 3),
                         A_kisi_yil = setNames(as.list(round(baglam[duzey == "kisi" & kirilim == "Total" & tr63_haric == FALSE, yuzde], 3)), baglam[duzey == "kisi" & kirilim == "Total" & tr63_haric == FALSE, yil]),
                         A_hane_yil = setNames(as.list(round(baglam[duzey == "hane" & kirilim == "Total" & tr63_haric == FALSE, yuzde], 3)), baglam[duzey == "hane" & kirilim == "Total" & tr63_haric == FALSE, yil]),
                         A_kisi_2025_gelir = setNames(as.list(round(baglam[duzey == "kisi" & kirilim == "gelir_dilim5" & yil == 2025 & tr63_haric == FALSE, yuzde], 3)), paste0("Q", baglam[duzey == "kisi" & kirilim == "gelir_dilim5" & yil == 2025 & tr63_haric == FALSE, kategori]))),
  H1_hane = c(list(model = h1_ana, n = nrow(hane_m), olay = sum(hane_m$Y_saglik_yuk), hucre_ham = hucre_h_n[, .(A, C_hane, n, olay, ham_yuzde = round(ham_yuzde, 3))]),
              setNames(lapply(c("p00", "p01", "p10", "p11", "beklenti11", "Delta", "RERI", "AP", "PR10", "PR01", "PR11", "PD_A", "PR_A", "OR_A:C_hane"), function(k) sj(h1_tab[model == h1_ana], k)),
                       c("p00", "p01", "p10", "p11", "beklenti11", "Delta", "RERI", "AP", "PR10", "PR01", "PR11", "PD_A", "PR_A", "OR_carpim")),
              list(mde_gozlenen_se = round(2.487 * h1_tab[model == h1_ana & olcu == "Delta", se_boot], 3),
                   modeller = setNames(lapply(c("minimal", "cekirdek", "cekirdek_ihtiyac", "nihai", "nihai_esnek", "cekirdek_esnek"), function(mm) sj(h1_tab[model == mm], "Delta")), c("minimal", "cekirdek", "cekirdek_ihtiyac_ana", "tam_duzeltilmis", "esnek_tam", "esnek_cekirdek")),
                   cekirdek_ihtiyacsiz = setNames(lapply(c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PR_A"), function(k) sj(h1_tab[model == "cekirdek"], k)), c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PR_A")),
                   lpm_cekirdek = list(tahmin = round(100 * unname(coef(lpm_h1)["A:C_hane"]), 4), se = round(100 * unname(SE(lpm_h1)["A:C_hane"]), 4)),
                   etki_degisimi_p = as.list(signif(em_h1, 3)), baglanti = as.list(round(baglanti_h1, 3)))),
  H1_gali_kesifsel = list(hucre_ham = hucre_g_n[, .(A, C_hane_gali, n, olay, ham_yuzde = round(ham_yuzde, 3))],
                          cekirdek = setNames(lapply(c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PD_C0", "PD_C1", "PR_A"), function(k) sj(gali_tab[model == "04c_gali"], k)), c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PD_C0", "PD_C1", "PR_A")),
                          kronik_duzeltilmis = setNames(lapply(c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PD_C0", "PD_C1", "PR_A"), function(k) sj(gali_tab[model == "04d_gali_kronik"], k)), c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PD_C0", "PD_C1", "PR_A"))),
  H1_yetiskin = c(list(model = h1y_ana, n = nrow(yetiskin_2025), olay = sum(yetiskin_2025$Y_saglik_yuk)),
                  setNames(lapply(c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PR_A", "OR_A:C_fert"), function(k) sj(h1y_tab[model == h1y_ana], k)), c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PR_A", "OR_carpim")),
                  list(mde_gozlenen_se = round(2.487 * h1y_tab[model == h1y_ana & olcu == "Delta", se_boot], 3), tasarim_etkisi_kumeleme = round(deff_h1y, 3),
                       modeller = setNames(lapply(c("minimal", "cekirdek", "cekirdek_ihtiyac", "nihai", "nihai_esnek", "cekirdek_esnek"), function(mm) sj(h1y_tab[model == mm], "Delta")), c("minimal", "cekirdek", "cekirdek_ihtiyac_ana", "tam_duzeltilmis", "esnek_tam", "esnek_cekirdek")),
                   cekirdek_ihtiyacsiz = setNames(lapply(c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PR_A"), function(k) sj(h1y_tab[model == "cekirdek"], k)), c("p00", "p01", "p10", "p11", "Delta", "RERI", "AP", "PR_A")),
                       etki_degisimi_p = as.list(signif(em_h1y, 3)))),
  H2 = setNames(lapply(h2_y, function(v) {
    tb <- h2_tab[sonuc == v & model == h2_ana[[v]]]
    c(list(model = h2_ana[[v]], n = tb$n[1], olay = tb$olay[1]), setNames(lapply(c("p0", "p1", "PD", "PR"), function(k) sj(tb, k)), c("p0", "p1", "PD", "PR")),
      list(sirali_OR = as.list(round(unlist(ord_tab[sonuc == v, .(OR, alt, ust)]), 4)), orantili_odds = ord_tab[sonuc == v, po_karar],
           gelir = h2_gelir[sonuc == v, .(gelir, PD = round(PD, 3), PD_alt = round(PD_alt, 3), PD_ust = round(PD_ust, 3), PR = round(PR, 4))]))
  }), h2_y),
  H2_gida_karsilastirma = gida_kars[, .(karsilastirma, olcu, fark = round(fark, 4), fark_alt = round(fark_alt, 4), fark_ust = round(fark_ust, 4), oran = round(oran, 4), p_boot)],
  ikincil = ikincil[olcu %in% c("p0", "p1", "PD", "PR", "p00", "p01", "p10", "p11", "Delta", "PD_A", "PR_A"), .(analiz, olcu, tahmin = round(tahmin, 4), alt = round(alt, 4), ust = round(ust, 4), n, olay)],
  duyarlilik_H1 = orman[, .(analiz, olcu, tahmin = round(tahmin, 3), alt = round(alt, 3), ust = round(ust, 3), n, olay)],
  agirlik = ok8[, .(agirlik, max_medyan = round(max_medyan, 3), kish_neff = round(kish_neff, 1))],
  ortusme = rbindlist(kaba_ortusme)[, .(model, tabakalama, tabaka, seyrek_tabaka, seyrek_nufus_payi = round(seyrek_nufus_payi, 2))],
  model_bicimi_kurali = "Inceltilmis (sonuclar goruldukten sonra): etki degisimi taramasinda p < 0,01 heterojenlik varsa esnek model ana model, yoksa ana etkili model ana model. H1 ana kovaryat seti: cekirdek + ihtiyac.",
  dogrulama = dogr[, .(kontrol, beklenen, bulunan, durum)],
  bootstrap = list(B_ana = 4000, B_diger = 2000, tohum = c(20251025, 20251026, 20251027), yontem = "Rao-Wu-Yue-Beaumont, n-1 households with replacement")
)
if (exists("lca_sayilar")) sayilar$LCA <- lca_sayilar
if (exists("sim_ozet")) sayilar$simulasyon <- sim_ozet[, .(sid, duzey, Delta_nominal, yanlilik_turu, rho, tahminci, R, gercek_Delta = round(gercek_Delta, 4), yanlilik = round(yanlilik, 4), ampirik_se = round(ampirik_se, 4), ort_model_se = round(ort_model_se, 4), kapsama, red_iki_yonlu, red_carpim)]
writeLines(toJSON(sayilar, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null"), "Results/sayilar.json")

writeLines(capture.output(sessionInfo()), "Results/sessionInfo.txt")
pk <- c("base", "data.table", "arrow", "readxl", "survey", "splines", "flextable", "officer", "future", "furrr", "jsonlite", "car", "logistf", "poLCA", "openxlsx", "MASS")
atif <- unlist(lapply(pk, function(x) c(paste0("==== ", x, " ", if (x == "base") as.character(getRversion()) else as.character(packageVersion(x)), " ===="), capture.output(print(citation(x), style = "text")), "")))
py_surum <- tryCatch(system2(py, c("-I", "-c", shQuote("import importlib.metadata as m;print(';'.join(f'{p}={m.version(p)}' for p in ['numpy','pandas','scipy','matplotlib','pyarrow','stepmix','scikit-learn','python-docx','pillow']))")), stdout = TRUE), error = function(e) "")
py_atif <- c("==== Python kutuphaneleri ====", paste("Surumler:", py_surum),
             "Python: Python Software Foundation. Python Language Reference, version 3.13. https://www.python.org",
             "StepMix: Morin S, Legault R, Laliberte F, Bakk Z, Giguere CE, de la Sablonniere R, Lacourse E. StepMix: A Python package for pseudo-likelihood estimation of generalized mixture models with external variables. arXiv:2304.03853 (2023). https://doi.org/10.48550/arXiv.2304.03853",
             "NumPy: Harris CR, Millman KJ, van der Walt SJ, et al. Array programming with NumPy. Nature 2020;585:357-362. doi:10.1038/s41586-020-2649-2",
             "pandas: McKinney W. Data structures for statistical computing in Python. Proceedings of the 9th Python in Science Conference 2010:56-61; The pandas development team. pandas-dev/pandas: Pandas. Zenodo. doi:10.5281/zenodo.3509134",
             "SciPy: Virtanen P, Gommers R, Oliphant TE, et al. SciPy 1.0: fundamental algorithms for scientific computing in Python. Nature Methods 2020;17:261-272. doi:10.1038/s41592-019-0686-2",
             "Matplotlib: Hunter JD. Matplotlib: A 2D graphics environment. Computing in Science & Engineering 2007;9(3):90-95. doi:10.1109/MCSE.2007.55",
             "scikit-learn: Pedregosa F, Varoquaux G, Gramfort A, et al. Scikit-learn: Machine learning in Python. Journal of Machine Learning Research 2011;12:2825-2830.",
             "Apache Arrow (pyarrow): Apache Software Foundation. Apache Arrow. https://arrow.apache.org",
             "Not: StepMix icin hakemli dergi surumu cikmis olabilir; gonderimden once paketin README atif onerisiyle karsilastirin.")
writeLines(c(atif, py_atif), "Results/paket_atiflari.txt")
py_env <- tryCatch(system2(py, c("-I", "-c", shQuote("import importlib.metadata as m;print('\\n'.join(sorted(d.metadata['Name'] + '==' + d.version for d in m.distributions())))")), stdout = TRUE, stderr = TRUE), error = function(e) "paket listesi alinamadi")
writeLines(c(system2(py, "--version", stdout = TRUE, stderr = TRUE), "Paketler (importlib.metadata):", "", py_env), "Results/python_ortam.txt")
cat("analiz.R bitti. Toplam sure (dk):", round(as.numeric(difftime(Sys.time(), baslangic, units = "mins")), 2), "\n")
sink()
an <- setNames(akis$n, akis$adim)
strobe <- c("# Sample flow and STROBE notes (generated by analiz.R)", "",
  "## Flow", "",
  paste0("- ILCS cross-sections 2023-2025: ", nfmt(an[["kaynak"]]), " persons in ", nfmt(an[["kaynak_hane"]]), " households (2023: ", nfmt(an[["yil_2023_kisi"]]), "; 2024: ", nfmt(an[["yil_2024_kisi"]]), "; 2025: ", nfmt(an[["yil_2025_kisi"]]), " persons)."),
  "- 2023 and 2024 used only for the context analyses (each year separately; no pooling, no linkage across years).",
  paste0("- 2025 cross-section: ", nfmt(an[["k2025_hane"]]), " households, ", nfmt(an[["k2025_kisi"]]), " persons, ", nfmt(an[["k2025_fkayit"]]), " individual questionnaires (aged 15+)."),
  paste0("- Household analytic sample: ", nfmt(an[["analitik_hane"]]), " (excluded: ", an[["dis_hane_yetiskinsiz"]], " household without an adult member). Events (heavy medical or medicine burden): ", nfmt(an[["Y_olay_hane"]]), "."),
  paste0("- Adult analytic sample (FK070 >= 18, individual record): ", nfmt(an[["analitik_yetiskin"]]), ". Excluded persons: ", nfmt(an[["dis_0_14"]]), " aged <15; ", nfmt(an[["dis_15_17"]]), " aged 15-17 with record; ", nfmt(an[["dis_15plus_kayitsiz"]]), " aged 15+ without individual record (", an[["dis_18plus_kayitsiz"]], " of them aged 18+)."),
  paste0("- Meat outcome: ", an[["et_vejetaryen_dislandi"]], " vegetarian or vegan adults excluded (sensitivity: included). BMI description: ", an[["bki_supheli_dislandi"]], " implausible values excluded."),
  paste0("- Unmet medical need universe: ", nfmt(an[["ihtiyac_var"]]), " adults reporting a medical care need; dental: ", nfmt(an[["dis_ihtiyac_var"]]), "; specialist visits: ", nfmt(an[["kronik_yetiskin"]]), " adults with chronic illness."),
  paste0("- Exposure groups: households ", nfmt(an[["hane_A0"]]), " can afford / ", nfmt(an[["hane_A1"]]), " cannot afford; adults ", nfmt(an[["yetiskin_A0"]]), " / ", nfmt(an[["yetiskin_A1"]]), ". Households with an adult with chronic illness: ", nfmt(an[["hane_C1"]]), "."),
  "- Missing data in model covariates: adults 0%; households 0.05% (marital status of the reference person, 13 households). Complete-case analysis.", "",
  "## STROBE checklist notes (cross-sectional)", "",
  "- Title/abstract (1): state cross-sectional design and data source (TurkStat ILCS 2025).",
  "- Setting (5): fieldwork February-June 2025; income reference year 2024; age at 31 December 2024.",
  "- Participants (6): non-institutional household population; stratified two-stage cluster sample; analytic inclusion as above.",
  "- Variables (7): exposure HE090; modifier C (adult with chronic illness, FS020); outcomes MODUL1_H_1/H_3 (household), dietary frequency (MODUL1_F_8, MODUL1_F_9, MODUL2_F_5), unmet need (FS050/FS060, FS070/FS080); covariates per protocol (Rapor section 4).",
  "- Data sources/measurement (8): self-reported household and individual questionnaires (CAPI); burden items are subjective; household items answered by one respondent (no respondent identifier; reference person used as proxy).",
  "- Bias (9): same-respondent check (Table S10); response-bias simulation (Figure 5b).",
  "- Study size (10): fixed by survey; precision and minimum detectable Delta reported (Rapor sections 7-8).",
  "- Quantitative variables (11): age spline; income quintiles; counts top-coded.",
  "- Statistical methods (12): survey-weighted logistic models with g-computation; household-cluster bootstrap; sensitivity analyses (Table S6); missing data negligible; design without strata/PSU (household clustering).",
  "- Descriptive data (14): Table 1, Table S1, Table S15. Outcome data (15): event counts in Tables 2, 3, S5, S9.",
  "- Main results (16): adjusted estimates with 95% CI; minimal, core and final models (Table S11).",
  "- Other analyses (17): effect-modification screen, latent class profiles, simulation.",
  "- Limitations (19): cross-sectional; subjective burden; single household respondent; no stratum/PSU identifiers; TR63 absent in 2023; income refers to previous year.")
writeLines(strobe, "Results/strobe_akis.md")

