# Kodlar: çalıştırma sırası, süreler, ortam

Bütün komutlar proje kökünden çalıştırılır (`Foods Makalesi 1/`). Göreli yollar proje köküne göredir.

## Sıra

1. `Rscript Codes/analiz.R`
   Tek akış: xlsx okuma (yalnız ilk çalıştırmada; sonra `R_Data ve Python Data/gyka_2023_2025.parquet`), doğrulama, değişken türetme, analitik veri setleri, betimleyici analizler, Hipotez 2, Hipotez 1, ikincil ve keşifsel analizler, duyarlılık analizleri, tablolar, `sayilar.json`, oturum bilgisi ve atıflar.
   - LCA çıktıları (`R_Data ve Python Data/lca_siniflar.parquet`, `Results/lca_*.csv`) yoksa `Codes/lca.py` betiği analiz.R içinden çağrılır.
   - Simülasyon özeti (`R_Data ve Python Data/simulasyon/simulasyon_ozet.csv`) yoksa `Codes/simulasyon.R` analiz.R içinden çağrılır (simülasyon, analiz.R'nin yazdığı `simulasyon/sim_girdi.rds` dosyasını okur).
   - `Results/tablolar_tumu.xlsx` kaydedildikten sonra analiz.R `Codes/xlsx_simge.py` betiğini çağırır; betik `_{}` ve `^{}` işaretlerini hücre içinde gerçek alt/üst simgeye çevirir (openpyxl). CSV'ler düz metin kalır.
   - Bootstrap tekrarları `R_Data ve Python Data/onbellek/boot_*.rds` dosyalarında saklanır; dosya varsa yeniden hesaplanmaz. Sıfırdan yeniden üretmek için `onbellek/` klasörünü boşaltın.
2. `python Codes/figurler.py` (Python: `/Users/salim/veri-ortami/bin/python3`)
   Bütün figürler `R_Data ve Python Data/figur_verileri/*.csv` dosyalarından çizilir; figür kodu veri hesaplamaz. Tek figür için: `python Codes/figurler.py 2` veya `python Codes/figurler.py S1`.
   Ortak stil `Codes/figur_stili.py` içindedir.

Ayrı çalıştırma da mümkündür: `python Codes/lca.py`, `Rscript Codes/simulasyon.R pilot` (50 tekrarlık pilot) ve `Rscript Codes/simulasyon.R tam`.

## Süreler (64 GB, 18 çekirdekli Mac; 16 işçi)

Son temiz çalıştırmanın süreleri `Rapor.md` Bölüm 13 ve aşağıda güncellenir.

| Adım | Süre |
|---|---|
| analiz.R, sıfırdan (xlsx okuma, bütün bootstrap'lar, lca.py ve simulasyon.R dahil; 25.10.2026) | 50,9 dk |
| içinde: lca.py | 16,1 dk |
| içinde: simulasyon.R (tam, 21 senaryo) | 11,6 dk |
| analiz.R, önbellek ile LCA ve simülasyon çıktıları doluyken | 1,9 dk |
| figurler.py (bütün figürler) | 5,8 sn |

Temiz çalıştırma için şu dosya ve klasörler boşaltılır: `R_Data ve Python Data/` altındaki parquet, rds ve csv dosyaları, `onbellek/`, `simulasyon/`, `figur_verileri/`, ayrıca `Results/`. analiz.R gerekli klasörleri kendisi oluşturur.

## Ortam

- R 4.6.1, Apple Accelerate BLAS; paket sürümleri `Results/sessionInfo.txt`.
- Python 3.13.16, `/Users/salim/veri-ortami` (uv sanal ortamı); paketler `Results/python_ortam.txt`.
- Paralel hesap: `future::plan(multisession, workers = 16)`, `furrr`; işçilerde `VECLIB_MAXIMUM_THREADS = 1`.
- Tohumlar: bootstrap çarpan matrisleri 20251025 (ana), 20251026 (İBBS 2 sözde tabaka), 20251027 (ek 2.000 tekrar); simülasyon senaryo tohumları 20251101 + 1000 × sıra, tekrar tohumları ham dosyalarda.

## Veri güvenliği

Mikro veri ve kişi düzeyinde ara dosyalar yalnız `R_Data ve Python Data/` altında; `Results/` ve `Figures/` yalnız toplulaştırılmış çıktı içerir.
