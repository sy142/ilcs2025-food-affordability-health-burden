import os

os.environ["OMP_NUM_THREADS"] = "1"
os.environ["VECLIB_MAXIMUM_THREADS"] = "1"
os.environ["OPENBLAS_NUM_THREADS"] = "1"
os.environ["MKL_NUM_THREADS"] = "1"

import importlib.metadata as im
import itertools
import logging
import sys
import time
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from joblib import Parallel, delayed
from stepmix.stepmix import StepMix

KOK = Path(__file__).resolve().parents[1]
os.chdir(KOK)
VERI = KOK / "R_Data ve Python Data"
SONUC = KOK / "Results"
FIGV = VERI / "figur_verileri"
LOGD = SONUC / "log"
for klasor in (FIGV, LOGD):
    klasor.mkdir(parents=True, exist_ok=True)

NJOB = 8
K_ARALIK = range(1, 7)
BASLANGIC = 50
BASLANGIC_EK = 200
TOL_LL = 0.01
B_BLRT = 100
BLRT_BASLANGIC = 10
B_BOOT = 500
TOHUM = 20251025

KAT_SIKLIK = ["Never or <1/week", "1–3 times/week", "4–6 times/week", "Daily"]
KAT_ET = ["<1/week or never", "Weekly, not daily", "Daily"]
KAT_HE = ["Can afford", "Cannot afford"]
MODELLER = {
    "ortak": dict(
        ad="Joint affordability x meat indicator, fruit, vegetables",
        gosterge=["ortak", "meyve", "sebze"], nkat=[6, 4, 4], secilen_k=4,
        gosterge_ad={"ortak": "Affordability of a protein meal x meat, chicken or fish frequency",
                     "meyve": "Fruit", "sebze": "Vegetables or salad"},
        kat_ad={"ortak": [f"{a}; {e}" for a in KAT_HE for e in KAT_ET], "meyve": KAT_SIKLIK, "sebze": KAT_SIKLIK},
    ),
    "dort": dict(
        ad="Four separate indicators (HE090, fruit, vegetables, meat)",
        gosterge=["he090", "meyve", "sebze", "et"], nkat=[2, 4, 4, 3], secilen_k=5,
        gosterge_ad={"he090": "Can afford a protein meal every second day (household)", "meyve": "Fruit",
                     "sebze": "Vegetables or salad", "et": "Meat, chicken or fish"},
        kat_ad={"he090": KAT_HE, "meyve": KAT_SIKLIK, "sebze": KAT_SIKLIK, "et": KAT_ET},
    ),
}
ANA = "ortak"
GELIR_AD = {1: "Q1 (lowest)", 2: "Q2", 3: "Q3", 4: "Q4", 5: "Q5 (highest)"}
BOLGE_AD = {
    "TR1": "Istanbul", "TR2": "West Marmara", "TR3": "Aegean", "TR4": "East Marmara",
    "TR5": "West Anatolia", "TR6": "Mediterranean", "TR7": "Central Anatolia",
    "TR8": "West Black Sea", "TR9": "East Black Sea", "TRA": "Northeast Anatolia",
    "TRB": "Central East Anatolia", "TRC": "Southeast Anatolia",
}
UZAK = {
    "Y_saglik_yuk": "Lives in a household where medical or medicine spending was a heavy burden",
    "odeme_tibbi": "Unmet medical need because of cost (among adults with medical need)",
    "saglik_kotu": "Bad or very bad self-rated health",
}

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(message)s",
    handlers=[
        logging.FileHandler(LOGD / "lca_log.txt", mode="w", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
log = logging.info


def veri_oku():
    kol = ["fert_kimlik", "hane_kimlik", "FB030", "HE090", "meyve_siklik", "sebze_siklik",
           "et_tavuk_balik_siklik", "Y_saglik_yuk", "odeme_tibbi", "ihtiyac_var", "saglik_kotu",
           "gelir_dilim5", "FK070", "FK090", "egitim5", "HB030", "HB050", "yas_grup"]
    d = pd.read_parquet(VERI / "yetiskin_2025.parquet", columns=kol).reset_index(drop=True)
    rec4 = {0: 0, 1: 0, 2: 1, 3: 2, 4: 3, 5: 3}
    d["he090"] = (d.HE090 == 2).astype(int)
    d["meyve"] = d.meyve_siklik.map(rec4).astype(int)
    d["sebze"] = d.sebze_siklik.map(rec4).astype(int)
    d["et"] = d.et_tavuk_balik_siklik.map({0: 0, 1: 0, 2: 1, 3: 2}).astype(int)
    d["ortak"] = d.he090 * 3 + d.et
    d["w"] = d.FB030 / d.FB030.mean()
    return d


def desen_kodu(X, nkat):
    k = np.zeros(X.shape[0], dtype=int)
    for j, L in enumerate(nkat):
        k = k * L + X[:, j]
    return k


def tum_desenler(nkat):
    return np.array(list(itertools.product(*[range(L) for L in nkat])))


def log_bilesen(P, pi, theta):
    lp = np.log(np.clip(pi, 1e-300, None))[None, :].repeat(P.shape[0], axis=0)
    for j in range(P.shape[1]):
        lp = lp + np.log(np.clip(theta[j][:, P[:, j]].T, 1e-300, None))
    return lp


def sonsal(P, pi, theta):
    lp = log_bilesen(P, pi, theta)
    m = lp.max(axis=1, keepdims=True)
    e = np.exp(lp - m)
    s = e.sum(axis=1, keepdims=True)
    return e / s, (m + np.log(s)).ravel()


def parametre_cikar(model, K, nkat):
    p = model.get_parameters()
    pi = np.asarray(p["weights"], dtype=float)
    pis = np.asarray(p["measurement"]["pis"], dtype=float).reshape(K, len(nkat), -1)
    theta = []
    for j, L in enumerate(nkat):
        t = pis[:, j, :L]
        theta.append(t / t.sum(axis=1, keepdims=True))
    return pi, theta


def tek_uydur(K, tohum, P, Wp, nkat, max_iter=20000, abs_tol=1e-10):
    model = StepMix(n_components=K, measurement="categorical", n_init=1, random_state=int(tohum),
                    progress_bar=0, verbose=0, max_iter=max_iter, abs_tol=abs_tol)
    with warnings.catch_warnings(record=True) as uyarilar:
        warnings.simplefilter("always")
        model.fit(P, sample_weight=Wp)
    yakinsadi = not any("converge" in str(u.message) for u in uyarilar)
    pi, theta = parametre_cikar(model, K, nkat)
    _, ll_desen = sonsal(P, pi, theta)
    return dict(K=K, tohum=int(tohum), ll=float(np.sum(Wp * ll_desen)), yakinsadi=yakinsadi, pi=pi, theta=theta)


def marjinal(spec, theta):
    g = spec["gosterge"]
    if "ortak" in g:
        t = theta[g.index("ortak")]
        pa = t[:, 3:].sum(axis=1)
        et = t[:, :3] + t[:, 3:]
        et_a0 = t[:, :3] / t[:, :3].sum(axis=1, keepdims=True)
        et_a1 = t[:, 3:] / t[:, 3:].sum(axis=1, keepdims=True)
    else:
        pa = theta[g.index("he090")][:, 1]
        et = theta[g.index("et")]
        et_a0 = et_a1 = None
    return dict(pa=pa, et=et, meyve=theta[g.index("meyve")], sebze=theta[g.index("sebze")], et_a0=et_a0, et_a1=et_a1)


def siklik_skor(spec, theta):
    m = marjinal(spec, theta)
    return m["meyve"] @ (np.arange(4) / 3) + m["sebze"] @ (np.arange(4) / 3) + m["et"] @ (np.arange(3) / 2)


def siralama(spec, pi, theta):
    o = np.argsort(-siklik_skor(spec, theta))
    return pi[o], [t[o] for t in theta]


def coklu_baslangic(K, P, Wp, nkat, n, ofset=0):
    tohumlar = TOHUM + 1000 * K + ofset + np.arange(n)
    return Parallel(n_jobs=NJOB)(delayed(tek_uydur)(K, t, P, Wp, nkat) for t in tohumlar)


def ozet_istatistik(K, P, Wp, pi, theta, N, sonuclar, nkat):
    post, ll_desen = sonsal(P, pi, theta)
    ll = float(np.sum(Wp * ll_desen))
    npar = (K - 1) + K * sum(L - 1 for L in nkat)
    lls = np.array([s["ll"] for s in sonuclar])
    tekrar = float(np.mean(lls >= ll - TOL_LL))
    H = -np.sum(Wp[:, None] * post * np.log(np.clip(post, 1e-300, None)))
    rel_ent = 1 - H / (N * np.log(K)) if K > 1 else np.nan
    f = np.exp(ll_desen)
    ok = Wp > 0
    L2 = 2 * np.sum(Wp[ok] * np.log(Wp[ok] / (N * f[ok])))
    return dict(K=K, LL=ll, npar=npar, AIC=-2 * ll + 2 * npar, BIC=-2 * ll + npar * np.log(N),
                aBIC=-2 * ll + npar * np.log((N + 2) / 24), CAIC=-2 * ll + npar * (np.log(N) + 1),
                L2=L2, df_L2=int(ok.sum()) - 1 - npar, rel_entropi=rel_ent,
                en_kucuk_sinif_yuzde=100 * float(pi.min()), baslangic=len(sonuclar),
                yakinsayan=int(sum(s["yakinsadi"] for s in sonuclar)), en_iyi_tekrar_yuzde=100 * tekrar)


def simule_et(pi, theta, N, nkat, rng):
    c = rng.choice(len(pi), size=N, p=pi / pi.sum())
    X = np.zeros((N, len(nkat)), dtype=int)
    for j, L in enumerate(nkat):
        u = rng.random(N)
        cum = np.cumsum(theta[j][c], axis=1)
        X[:, j] = (u[:, None] > cum).sum(axis=1).clip(0, L - 1)
    return np.bincount(desen_kodu(X, nkat), minlength=int(np.prod(nkat))).astype(float)


def blrt_tek(K, pi, theta, N, b, P, nkat):
    rng = np.random.default_rng(TOHUM + 7919 * K + b)
    Wp = simule_et(pi, theta, N, nkat, rng)
    keep = Wp > 0
    Pk, Wk = P[keep], Wp[keep]
    en = []
    for kk in (K, K + 1):
        best = -np.inf
        nb = 1 if kk == 1 else (BLRT_BASLANGIC // 2 if kk == K else BLRT_BASLANGIC)
        for s in range(nb):
            r = tek_uydur(kk, TOHUM + 100000 * kk + 1000 * b + s, Pk, Wk, nkat, max_iter=3000, abs_tol=1e-8)
            best = max(best, r["ll"])
        en.append(best)
    return 2 * (en[1] - en[0])


def ikili_artiklar(d, spec, pi, theta, K, mkey):
    g, nk = spec["gosterge"], spec["nkat"]
    N = d.w.sum()
    satir, hucre = [], []
    for a, b in itertools.combinations(range(len(g)), 2):
        La, Lb = nk[a], nk[b]
        O = np.zeros((La, Lb))
        np.add.at(O, (d[g[a]].to_numpy(), d[g[b]].to_numpy()), d.w.to_numpy())
        E = N * np.einsum("c,ci,cj->ij", pi, theta[a], theta[b])
        z = (O - E) / np.sqrt(E)
        X2 = float(np.sum(z ** 2))
        df = (La - 1) * (Lb - 1)
        satir.append(dict(model=mkey, K=K, duzey="cift", cift=f"{g[a]} x {g[b]}", X2=X2, df=df, BVR=X2 / df,
                          max_abs_z=float(np.abs(z).max()), hucre_z_gt3=int((np.abs(z) > 3).sum())))
        for i in range(La):
            for j in range(Lb):
                hucre.append(dict(model=mkey, K=K, duzey="hucre", cift=f"{g[a]} x {g[b]}",
                                  kategori_1=spec["kat_ad"][g[a]][i], kategori_2=spec["kat_ad"][g[b]][j],
                                  gozlenen=O[i, j], beklenen=E[i, j], z=z[i, j]))
    return pd.DataFrame(satir), pd.DataFrame(hucre)


def etiketle(spec, theta, genel_A):
    m = marjinal(spec, theta)
    etk = []
    for c in range(theta[0].shape[0]):
        pa, f, v, et = m["pa"][c], m["meyve"][c], m["sebze"][c], m["et"][c]
        if f[3] > 0.6 and v[3] > 0.6:
            e = "Daily fruit and vegetables"
        elif f[2] > 0.5 and v[2] > 0.5:
            e = "Near-daily fruit and vegetables"
        elif f[0] > 0.5:
            e = "Very infrequent fruit, vegetables and meat"
        elif pa < 0.5 * genel_A and et[1] + et[2] > 0.6:
            e = "Affording, infrequent fruit and vegetables"
        elif pa >= 0.5:
            e = "Constrained, infrequent fruit, vegetables and meat"
        elif f[1] > 0.5:
            e = "Weekly (1–3 times) fruit and vegetables"
        else:
            e = "Mixed profile"
        etk.append(e)
    sayac, sonuc = {}, []
    for e in etk:
        sayac[e] = sayac.get(e, 0) + 1
        sonuc.append(e if etk.count(e) == 1 else f"{e} ({sayac[e]})")
    return sonuc


def madde_tablosu(spec, mkey, K, pi, theta, etiketler):
    r = []
    g = spec["gosterge"]
    for c in range(K):
        for j, gg in enumerate(g):
            for l in range(spec["nkat"][j]):
                r.append(dict(model=mkey, K=K, sinif=c + 1, sinif_ad=etiketler[c], sinif_payi=100 * pi[c], gosterge=gg,
                              gosterge_ad=spec["gosterge_ad"][gg], kategori_kod=l, kategori=spec["kat_ad"][gg][l],
                              olasilik=float(theta[j][c, l]), turetilmis=False))
        if "ortak" in g:
            m = marjinal(spec, theta)
            for l, ad in enumerate(KAT_HE):
                r.append(dict(model=mkey, K=K, sinif=c + 1, sinif_ad=etiketler[c], sinif_payi=100 * pi[c], gosterge="he090",
                              gosterge_ad="Can afford a protein meal every second day (household; marginal of joint item)",
                              kategori_kod=l, kategori=ad, olasilik=float([1 - m["pa"][c], m["pa"][c]][l]), turetilmis=True))
            for l, ad in enumerate(KAT_ET):
                for anahtar, kosul in (("et", ""), ("et_a0", " | can afford"), ("et_a1", " | cannot afford")):
                    r.append(dict(model=mkey, K=K, sinif=c + 1, sinif_ad=etiketler[c], sinif_payi=100 * pi[c], gosterge=anahtar,
                                  gosterge_ad="Meat, chicken or fish" + kosul + " (from joint item)", kategori_kod=l, kategori=ad,
                                  olasilik=float(m[anahtar][c, l]), turetilmis=True))
    return pd.DataFrame(r)


def kovaryat_matrisi(d, A_ekle=False):
    parca = [np.ones((len(d), 1))]
    adlar = ["(Intercept)"]
    for deg, ref, ad in [("gelir_dilim5", 1, "income"), ("yas_grup", 1, "age"), ("FK090", 1, "sex"),
                         ("egitim5", 1, "education"), ("HB030", "TR1", "region"), ("HB050", 1, "hhtype")]:
        x = d[deg]
        for s in sorted(x.unique(), key=lambda s: (str(type(s)), s)):
            if s == ref:
                continue
            parca.append((x == s).to_numpy(dtype=float)[:, None])
            adlar.append(f"{ad}={s}")
    if A_ekle:
        parca.append(d.he090.to_numpy(dtype=float)[:, None])
        adlar.append("A")
    return np.hstack(parca), adlar


def bch_d(post, modal, W):
    K = post.shape[1]
    D = np.zeros((K, K))
    for s in range(K):
        ii = modal == s
        D[:, s] = (W[ii, None] * post[ii]).sum(axis=0)
    return D / D.sum(axis=1, keepdims=True)


def bch_agirlik(post, modal, W, tabaka=None):
    if tabaka is None:
        return np.linalg.inv(bch_d(post, modal, W))[modal, :]
    ws = np.zeros_like(post)
    for t in np.unique(tabaka):
        ii = tabaka == t
        ws[ii] = np.linalg.inv(bch_d(post[ii], modal[ii], W[ii]))[modal[ii], :]
    return ws


def mnl_bch(Z, ws, W, maxit=100, tol=1e-8):
    n, p = Z.shape
    K = ws.shape[1]
    B = np.zeros((K - 1, p))
    for _ in range(maxit):
        eta = np.hstack([np.zeros((n, 1)), Z @ B.T])
        eta -= eta.max(axis=1, keepdims=True)
        Pm = np.exp(eta)
        Pm /= Pm.sum(axis=1, keepdims=True)
        G = ((W[:, None] * (ws[:, 1:] - Pm[:, 1:])).T @ Z).ravel()
        H = np.zeros(((K - 1) * p, (K - 1) * p))
        for a in range(1, K):
            for b in range(a, K):
                v = W * Pm[:, a] * ((a == b) - Pm[:, b])
                blok = (Z * v[:, None]).T @ Z
                H[(a - 1) * p:a * p, (b - 1) * p:b * p] = blok
                H[(b - 1) * p:b * p, (a - 1) * p:a * p] = blok.T
        adim = np.linalg.solve(H + 1e-10 * np.eye(H.shape[0]), G)
        B += adim.reshape(K - 1, p)
        if np.max(np.abs(adim)) < tol:
            break
    return B


def lojistik_bch(X, y, v, maxit=60, tol=1e-9):
    b = np.zeros(X.shape[1])
    for _ in range(maxit):
        mu = 1 / (1 + np.exp(-(X @ b)))
        H = (X * (v * mu * (1 - mu))[:, None]).T @ X
        g = X.T @ (v * (y - mu))
        try:
            adim = np.linalg.solve(H, g)
        except np.linalg.LinAlgError:
            return None
        b += adim
        if not np.all(np.isfinite(b)) or np.max(np.abs(b)) > 50:
            return None
        if np.max(np.abs(adim)) < tol:
            return b
    return None


def uzak_sonuc(y, post, modal, W, Zc, A=None):
    n, K = post.shape
    ws = bch_agirlik(post, modal, W, tabaka=A)
    ham = (W[:, None] * ws * y[:, None]).sum(axis=0) / (W[:, None] * ws).sum(axis=0)
    Zfull = Zc if A is None else np.hstack([Zc, A[:, None]])
    E = np.eye(K)
    Xs = np.vstack([np.hstack([np.tile(E[s], (n, 1)), Zfull]) for s in range(K)])
    yy = np.tile(y, K)
    vv = (W[:, None] * ws).T.ravel()
    b = lojistik_bch(Xs, yy, vv)
    yontem = "logistic"
    if b is None:
        H = (Xs * vv[:, None]).T @ Xs
        b = np.linalg.solve(H, (Xs * vv[:, None]).T @ yy)
        yontem = "linear probability"
    ters = (lambda e: 1 / (1 + np.exp(-e))) if yontem == "logistic" else (lambda e: e)
    duz = np.array([np.sum(W * ters(np.hstack([np.tile(E[s], (n, 1)), Zfull]) @ b)) / np.sum(W) for s in range(K)])
    a_duz = None
    if A is not None:
        a_duz = np.zeros(2)
        for a in (0, 1):
            Za = np.hstack([Zc, np.full((n, 1), float(a))])
            tot = 0.0
            for s in range(K):
                tot += np.sum(W * ws[:, s] * ters(np.hstack([np.tile(E[s], (n, 1)), Za]) @ b))
            a_duz[a] = tot / np.sum(W)
    return ham, duz, yontem, a_duz


def paylar(post, W, grup=None):
    if grup is None:
        return (W[:, None] * post).sum(axis=0) / W.sum()
    out = {}
    for g in np.unique(grup):
        ii = grup == g
        out[g] = (W[ii, None] * post[ii]).sum(axis=0) / W[ii].sum()
    return out


def boot_tek(carp_list, W0, post, modal, Zc_d, Zm, ys, alt_maske, gelir, A, A_ile):
    sonuc = []
    for carp in carp_list:
        Wb = W0 * carp
        r = {"pay": paylar(post, Wb), "pay_gelir": paylar(post, Wb, gelir)}
        r["mnl"] = mnl_bch(Zm, bch_agirlik(post, modal, Wb), Wb) if Zm is not None else None
        for ad, y in ys.items():
            m = alt_maske[ad]
            r[ad] = uzak_sonuc(y[m], post[m], modal[m], Wb[m], Zc_d[ad][m])
            if A_ile:
                r[ad + "|A"] = uzak_sonuc(y[m], post[m], modal[m], Wb[m], Zc_d[ad][m], A=A[m])
        sonuc.append(r)
    return sonuc


def uydur_hepsi(d, mkey, N):
    spec = MODELLER[mkey]
    nk = spec["nkat"]
    P = tum_desenler(nk)
    kod = desen_kodu(d[spec["gosterge"]].to_numpy(), nk)
    Wp = np.bincount(kod, weights=d.w.to_numpy(), minlength=len(P))
    keep = Wp > 0
    Pk, Wk = P[keep], Wp[keep]
    log(f"[{mkey}] gozlenen desen {keep.sum()} / {len(P)}")
    cozum, uyum, kayit = {}, [], []
    for K in K_ARALIK:
        t0 = time.time()
        sonuclar = coklu_baslangic(K, Pk, Wk, nk, BASLANGIC if K > 1 else 1)
        lls = np.array([s["ll"] for s in sonuclar])
        if K > 1 and np.mean(lls >= lls.max() - TOL_LL) < 0.20:
            log(f"[{mkey}] K={K}: en iyi LL baslangiclarin %{100 * np.mean(lls >= lls.max() - TOL_LL):.0f}'inde; baslangic {BASLANGIC_EK}'e cikarildi")
            sonuclar += coklu_baslangic(K, Pk, Wk, nk, BASLANGIC_EK - BASLANGIC, ofset=BASLANGIC)
        en_iyi = max(sonuclar, key=lambda s: s["ll"])
        pi, theta = siralama(spec, en_iyi["pi"], en_iyi["theta"])
        cozum[K] = dict(pi=pi, theta=theta, tohum=en_iyi["tohum"])
        st = ozet_istatistik(K, Pk, Wk, pi, theta, N, sonuclar, nk)
        st["sure_sn"] = time.time() - t0
        st["model"] = mkey
        uyum.append(st)
        kayit += [dict(model=mkey, K=K, tohum=s["tohum"], LL=s["ll"], yakinsadi=s["yakinsadi"]) for s in sonuclar]
        log(f"[{mkey}] K={K}: LL={st['LL']:.3f} BIC={st['BIC']:.1f} aBIC={st['aBIC']:.1f} entropi={st['rel_entropi']:.3f} en kucuk sinif %{st['en_kucuk_sinif_yuzde']:.1f} "
            f"tekrar %{st['en_iyi_tekrar_yuzde']:.0f} ({st['baslangic']} baslangic, {st['yakinsayan']} yakinsadi)")
    uyum = pd.DataFrame(uyum)
    t0 = time.time()
    gorevler = [(K, b) for K in range(1, 6) for b in range(B_BLRT)]
    ist = np.array(Parallel(n_jobs=NJOB)(delayed(blrt_tek)(K, cozum[K]["pi"], cozum[K]["theta"], N, b, Pk, nk) for K, b in gorevler)).reshape(5, B_BLRT)
    p, gz = [np.nan], [np.nan]
    for K in range(1, 6):
        g = 2 * (uyum.LL[uyum.K == K + 1].iloc[0] - uyum.LL[uyum.K == K].iloc[0])
        gz.append(g)
        p.append((1 + np.sum(ist[K - 1] >= g)) / (B_BLRT + 1))
    uyum["BLRT_istatistik"] = gz
    uyum["BLRT_p"] = p
    uyum["BLRT_null_en_buyuk"] = [np.nan] + [float(ist[k].max()) for k in range(5)]
    uyum["delta_BIC"] = uyum.BIC.diff()
    uyum["delta_aBIC"] = uyum.aBIC.diff()
    sure_blrt = time.time() - t0
    log(f"[{mkey}] BLRT {sure_blrt:.0f} sn: p = {np.round(p, 4).tolist()}")
    return dict(spec=spec, P=P, kod=kod, cozum=cozum, uyum=uyum, kayit=kayit, sure_blrt=sure_blrt)


def bch_analiz(d, sonuc, mkey, K, etiketler, ref, A_ile, carpanlar):
    spec = sonuc["spec"]
    N = len(d)
    pi, theta = sonuc["cozum"][K]["pi"], sonuc["cozum"][K]["theta"]
    post = sonsal(sonuc["P"], pi, theta)[0][sonuc["kod"]]
    modal = post.argmax(axis=1)
    W0 = d.w.to_numpy()
    Zc, zad = kovaryat_matrisi(d)
    Zcov = Zc[:, 1:]
    kucuk = [j for j, a in enumerate(zad) if a.startswith(("income", "age", "sex"))]
    Zc_d = {"Y_saglik_yuk": Zcov, "saglik_kotu": Zcov, "odeme_tibbi": Zc[:, kucuk]}
    gelir = d.gelir_dilim5.to_numpy()
    A = d.he090.to_numpy(dtype=float)
    ys = {"Y_saglik_yuk": d.Y_saglik_yuk.to_numpy(dtype=float), "odeme_tibbi": d.odeme_tibbi.fillna(0).to_numpy(dtype=float),
          "saglik_kotu": d.saglik_kotu.to_numpy(dtype=float)}
    alt_maske = {"Y_saglik_yuk": np.ones(N, bool), "odeme_tibbi": (d.ihtiyac_var == 1).to_numpy() & d.odeme_tibbi.notna().to_numpy(),
                 "saglik_kotu": np.ones(N, bool)}
    Zm = Zc if mkey == ANA else None
    h0, d0, _, _ = uzak_sonuc(ys["Y_saglik_yuk"], post, modal, W0, np.zeros((N, 0)))
    log(f"[{mkey}] BCH denetim: kovaryatsiz lojistik - duzeltmesiz BCH ortalama en buyuk mutlak fark = {np.max(np.abs(h0 - d0)):.2e}")
    t0 = time.time()
    tam = boot_tek([np.ones(N)], W0, post, modal, Zc_d, Zm, ys, alt_maske, gelir, A, A_ile)[0]
    parcalar = np.array_split(np.arange(B_BOOT), NJOB * 4)
    boot = Parallel(n_jobs=NJOB)(delayed(boot_tek)([carpanlar[i] for i in pr], W0, post, modal, Zc_d, Zm, ys, alt_maske, gelir, A, A_ile) for pr in parcalar)
    boot = [r for parca in boot for r in parca]
    sure = time.time() - t0
    log(f"[{mkey}] BCH bootstrap (B = {B_BOOT}) {sure:.0f} sn")

    def ga(x):
        x = np.asarray(x, dtype=float)
        return np.nanpercentile(x, 2.5, axis=0), np.nanpercentile(x, 97.5, axis=0)

    satir = []
    for ad in ys:
        m = alt_maske[ad]
        mod_n = np.bincount(modal[m], minlength=K)
        mod_olay = np.bincount(modal[m], weights=ys[ad][m], minlength=K)
        varyant = [("unadjusted", ad, 0), ("adjusted", ad, 1)] + ([("adjusted + A", ad + "|A", 1)] if A_ile else [])
        for tur, anahtar, kk in varyant:
            t = 100 * np.asarray(tam[anahtar][kk])
            bm = 100 * np.array([r[anahtar][kk] for r in boot])
            lo, hi = ga(bm)
            yontem = tam[anahtar][2] if kk else "BCH weighted mean"
            for s in range(K):
                gz = mod_n[s] < 20
                notu = "BCH estimate or bound below 0, truncated at 0" if (t[s] < 0 or lo[s] < 0) and not gz else ""
                satir.append(dict(model=mkey, sonuc=ad, sonuc_ad=UZAK[ad], ayar=tur, yontem=yontem, sinif=s + 1, sinif_ad=etiketler[s],
                                  olcu="prevalence", tahmin=np.nan if gz else max(t[s], 0.0), alt=np.nan if gz else max(lo[s], 0.0), ust=np.nan if gz else hi[s],
                                  modal_n=int(mod_n[s]), modal_olay=int(mod_olay[s]), n=int(m.sum()), not_=notu))
            ref_sifir = lo[ref] <= 0 or t[ref] <= 0
            for s in range(K):
                if s == ref:
                    continue
                for olc, f, sifir in (("PD vs reference", lambda a, b: a - b, 0.0), ("PR vs reference", lambda a, b: a / b, 1.0)):
                    gz = mod_n[s] < 20 or mod_n[ref] < 20 or (olc.startswith("PR") and ref_sifir)
                    notu = "PR not estimable: reference-class prevalence not distinguishable from 0" if olc.startswith("PR") and ref_sifir else ""
                    bb = f(bm[:, s], bm[:, ref])
                    satir.append(dict(model=mkey, sonuc=ad, sonuc_ad=UZAK[ad], ayar=tur, yontem=yontem, sinif=s + 1, sinif_ad=etiketler[s],
                                      olcu=olc, referans=f"{ref + 1} {etiketler[ref]}", tahmin=np.nan if gz else f(t[s], t[ref]),
                                      alt=np.nan if gz else np.nanpercentile(bb, 2.5), ust=np.nan if gz else np.nanpercentile(bb, 97.5),
                                      p_boot=np.nan if gz else 2 * min(np.mean(bb <= sifir), np.mean(bb >= sifir)),
                                      modal_n=int(mod_n[s]), modal_olay=int(mod_olay[s]), n=int(m.sum()), not_=notu))
            if tur == "adjusted + A":
                ta = 100 * tam[anahtar][3]
                ba = 100 * np.array([r[anahtar][3] for r in boot])
                lo, hi = ga(ba)
                n_a = np.bincount(A[m].astype(int), minlength=2)
                for a in (0, 1):
                    satir.append(dict(model=mkey, sonuc=ad, sonuc_ad=UZAK[ad], ayar=tur, yontem=yontem, sinif=np.nan,
                                      sinif_ad=f"A = {a} (standardized over class and covariates)", olcu="prevalence by A",
                                      tahmin=ta[a], alt=lo[a], ust=hi[a], modal_n=int(n_a[a]), n=int(m.sum())))
                for olc, f, sifir in (("PD A1 vs A0", lambda a, b: a - b, 0.0), ("PR A1 vs A0", lambda a, b: a / b, 1.0)):
                    bb = f(ba[:, 1], ba[:, 0])
                    satir.append(dict(model=mkey, sonuc=ad, sonuc_ad=UZAK[ad], ayar=tur, yontem=yontem, sinif=np.nan,
                                      sinif_ad="A = 1 vs A = 0, net of class", olcu=olc, tahmin=f(ta[1], ta[0]),
                                      alt=np.nanpercentile(bb, 2.5), ust=np.nanpercentile(bb, 97.5),
                                      p_boot=2 * min(np.mean(bb <= sifir), np.mean(bb >= sifir)), n=int(m.sum())))
    uzak = pd.DataFrame(satir)

    pay_satir = []
    lo, hi = ga([r["pay"] for r in boot])
    for s in range(K):
        pay_satir.append(dict(model=mkey, K=K, sinif=s + 1, sinif_ad=etiketler[s], kirilim="Total", grup="Total", yuzde=100 * tam["pay"][s],
                              alt=100 * lo[s], ust=100 * hi[s], modal_n=int((modal == s).sum()), grup_n=N, model_pi=100 * pi[s]))
    for g in sorted(np.unique(gelir)):
        lo, hi = ga([r["pay_gelir"][g] for r in boot])
        ii = gelir == g
        for s in range(K):
            pay_satir.append(dict(model=mkey, K=K, sinif=s + 1, sinif_ad=etiketler[s], kirilim="gelir_dilim5", grup=GELIR_AD[int(g)],
                                  yuzde=100 * tam["pay_gelir"][g][s], alt=100 * lo[s], ust=100 * hi[s],
                                  modal_n=int(((modal == s) & ii).sum()), grup_n=int(ii.sum())))
    mnl = None
    if Zm is not None:
        mb = np.array([r["mnl"] for r in boot])
        mnl = pd.DataFrame([dict(model=mkey, sinif=s + 1, sinif_ad=etiketler[s], referans=f"1 {etiketler[0]}", terim=ad,
                                 katsayi=tam["mnl"][s - 1, j], OR=np.exp(tam["mnl"][s - 1, j]),
                                 OR_alt=np.exp(np.nanpercentile(mb[:, s - 1, j], 2.5)), OR_ust=np.exp(np.nanpercentile(mb[:, s - 1, j], 97.5)),
                                 se_boot=np.nanstd(mb[:, s - 1, j]))
                            for s in range(1, K) for j, ad in enumerate(zad)])
    return dict(post=post, modal=modal, uzak=uzak, pay=pd.DataFrame(pay_satir), mnl=mnl, sure=sure)


def main():
    t_bas = time.time()
    surum = {p: im.version(p) for p in ["stepmix", "numpy", "pandas", "scipy", "joblib", "pyarrow"]}
    log(f"Surumler: {surum}")
    d = veri_oku()
    N = len(d)
    W0 = d.w.to_numpy()
    genel_A = float(np.average(d.he090, weights=W0))
    log(f"Yetiskin: {N}, hane: {d.hane_kimlik.nunique()}, vejetaryen (et kodu 0): {(d.et_tavuk_balik_siklik == 0).sum()}, agirlikli P(A=1) = {genel_A:.4f}")

    sonuc = {mk: uydur_hepsi(d, mk, N) for mk in MODELLER}
    uyum = pd.concat([sonuc[mk]["uyum"] for mk in MODELLER], ignore_index=True)
    uyum.to_csv(SONUC / "lca_uyum.csv", index=False)
    uyum.to_csv(FIGV / "figS2_lca_uyum.csv", index=False)
    pd.DataFrame(sum([sonuc[mk]["kayit"] for mk in MODELLER], [])).to_csv(LOGD / "lca_baslangic_ll.csv", index=False)

    etiketler = {mk: {K: etiketle(MODELLER[mk], sonuc[mk]["cozum"][K]["theta"], genel_A) for K in K_ARALIK} for mk in MODELLER}
    madde, art = [], []
    for mk in MODELLER:
        K = MODELLER[mk]["secilen_k"]
        for k in (K - 1, K, K + 1):
            cz = sonuc[mk]["cozum"][k]
            madde.append(madde_tablosu(MODELLER[mk], mk, k, cz["pi"], cz["theta"], etiketler[mk][k]))
            a, h = ikili_artiklar(d, MODELLER[mk], cz["pi"], cz["theta"], k, mk)
            art += [a, h]
    madde = pd.concat(madde, ignore_index=True)
    madde.to_csv(SONUC / "lca_madde_olasiliklari.csv", index=False)
    Kana = MODELLER[ANA]["secilen_k"]
    madde[(madde.model == ANA) & (madde.K == Kana)].to_csv(FIGV / "fig3a_lca_profil.csv", index=False)
    art = pd.concat(art, ignore_index=True)
    art.to_csv(SONUC / "lca_ikili_artiklar.csv", index=False)
    log("Ikili artiklar (cift ozetleri):\n" + art[art.duzey == "cift"][["model", "K", "cift", "BVR", "max_abs_z", "hucre_z_gt3"]].round(2).to_string(index=False))

    hh_kod, hh_idx = np.unique(d.hane_kimlik.to_numpy(), return_inverse=True)
    nh = len(hh_kod)
    rng = np.random.default_rng(TOHUM)
    carpanlar = [np.bincount(rng.integers(0, nh, nh - 1), minlength=nh)[hh_idx] * nh / (nh - 1) for _ in range(B_BOOT)]

    bch, siniflama = {}, []
    for mk in MODELLER:
        K = MODELLER[mk]["secilen_k"]
        et = etiketler[mk][K]
        if mk == ANA:
            ref = 0
        else:
            r0 = [s for s in range(K) if "Affording, infrequent" in et[s]]
            ref = r0[0] if r0 else int(np.argmin(marjinal(MODELLER[mk], sonuc[mk]["cozum"][K]["theta"])["pa"]))
        bch[mk] = bch_analiz(d, sonuc[mk], mk, K, et, ref, A_ile=(mk == ANA), carpanlar=carpanlar)
        bch[mk]["ref"] = ref
        post, modal = bch[mk]["post"], bch[mk]["modal"]
        D = bch_d(post, modal, W0)
        sn = pd.DataFrame(D, columns=[f"modal_{s + 1}" for s in range(K)])
        sn.insert(0, "gercek_sinif", [f"{s + 1} {et[s]}" for s in range(K)])
        sn.insert(0, "model", mk)
        sn["AvePP_modal"] = [post[modal == s, s].mean() for s in range(K)]
        sn["modal_n"] = np.bincount(modal, minlength=K)
        siniflama.append(sn)
        kay = pd.DataFrame({"fert_kimlik": d.fert_kimlik, "hane_kimlik": d.hane_kimlik})
        for s in range(K):
            kay[f"post_{s + 1}"] = post[:, s]
        kay["sinif"] = modal + 1
        kay["sinif_ad"] = [et[s] for s in modal]
        kay.to_parquet(VERI / ("lca_siniflar.parquet" if mk == ANA else "lca_siniflar_dort_gosterge.parquet"), index=False)
    siniflama = pd.concat(siniflama, ignore_index=True)
    siniflama.to_csv(SONUC / "lca_siniflama.csv", index=False)
    log("Siniflama (D, AvePP):\n" + siniflama.round(3).to_string(index=False))

    pay = [bch[mk]["pay"] for mk in MODELLER]
    bolge = d.HB030.astype(str).to_numpy()
    gelir = d.gelir_dilim5.to_numpy()
    for mk in MODELLER:
        K = MODELLER[mk]["secilen_k"]
        for k in (K - 1, K, K + 1):
            cz = sonuc[mk]["cozum"][k]
            pk = sonsal(sonuc[mk]["P"], cz["pi"], cz["theta"])[0][sonuc[mk]["kod"]]
            pm = pk.argmax(axis=1)
            ek = []
            if k != K:
                tp = paylar(pk, W0)
                ek += [dict(model=mk, K=k, sinif=s + 1, sinif_ad=etiketler[mk][k][s], kirilim="Total", grup="Total", yuzde=100 * tp[s],
                            modal_n=int((pm == s).sum()), grup_n=N, model_pi=100 * cz["pi"][s]) for s in range(k)]
                for g, v in paylar(pk, W0, gelir).items():
                    ii = gelir == g
                    ek += [dict(model=mk, K=k, sinif=s + 1, sinif_ad=etiketler[mk][k][s], kirilim="gelir_dilim5", grup=GELIR_AD[int(g)],
                                yuzde=100 * v[s], modal_n=int(((pm == s) & ii).sum()), grup_n=int(ii.sum())) for s in range(k)]
            for g, v in paylar(pk, W0, bolge).items():
                ii = bolge == g
                ek += [dict(model=mk, K=k, sinif=s + 1, sinif_ad=etiketler[mk][k][s], kirilim="HB030", grup=BOLGE_AD[g], yuzde=100 * v[s],
                            modal_n=int(((pm == s) & ii).sum()), grup_n=int(ii.sum())) for s in range(k)]
            pay.append(pd.DataFrame(ek))
    pay = pd.concat(pay, ignore_index=True)
    pay.to_csv(SONUC / "lca_sinif_paylari.csv", index=False)
    pay[(pay.model == ANA) & (pay.K == Kana) & (pay.kirilim.isin(["Total", "gelir_dilim5"]))].to_csv(FIGV / "fig3b_lca_gelir.csv", index=False)
    pay[(pay.model == ANA) & (pay.K == Kana) & (pay.kirilim == "HB030")].to_csv(FIGV / "fig3c_lca_bolge.csv", index=False)

    bch[ANA]["mnl"].to_csv(SONUC / "lca_multinomial.csv", index=False)
    uzak = pd.concat([bch[mk]["uzak"] for mk in MODELLER], ignore_index=True)
    uzak.to_csv(SONUC / "lca_bch_uzak_sonuclar.csv", index=False)
    uzak[(uzak.model == ANA) & (uzak.sonuc == "Y_saglik_yuk")].to_csv(FIGV / "fig4c_lca_yuk.csv", index=False)
    log("BCH uzak sonuclar:\n" + uzak[["model", "sonuc", "ayar", "sinif", "olcu", "tahmin", "alt", "ust", "modal_n", "modal_olay"]].round(2).to_string(index=False))

    sure = time.time() - t_bas
    ozet_yaz(d, sonuc, uyum, madde, pay, art, siniflama, bch, uzak, etiketler, surum, sure, genel_A)
    log(f"Toplam sure: {sure:.0f} sn")


def f1(x, n=1):
    return "–" if pd.isna(x) else f"{x:,.{n}f}".replace(",", " ").replace(".", ",").replace(" ", ".")


def uyum_tablosu(s, u):
    s.append("| K | LL | Parametre | BIC | aBIC | CAIC | L² (sd) | Bağıl entropi | En küçük sınıf % | En iyi LL tekrarı % | Başlangıç | BLRT p (K−1'e karşı) |")
    s.append("|---|---|---|---|---|---|---|---|---|---|---|---|")
    for _, r in u.iterrows():
        s.append(f"| {int(r.K)} | {f1(r.LL)} | {int(r.npar)} | {f1(r.BIC)} | {f1(r.aBIC)} | {f1(r.CAIC)} | {f1(r.L2)} ({int(r.df_L2)}) | {f1(r.rel_entropi, 3)} | "
                 f"{f1(r.en_kucuk_sinif_yuzde)} | {f1(r.en_iyi_tekrar_yuzde, 0)} | {int(r.baslangic)} | {f1(r.BLRT_p, 3)} |")


def profil_tablosu(s, madde, pay, mk, K):
    mkk = madde[(madde.model == mk) & (madde.K == K)]
    pt = pay[(pay.model == mk) & (pay.K == K) & (pay.kirilim == "Total")]
    s.append("| Sınıf | Pay % (95% GA) | P(karşılayamaz) | Meyve her gün | Meyve <1/hafta | Sebze her gün | Sebze <1/hafta | Et <1/hafta | Et <1/hafta, karşılayabilen | Et <1/hafta, karşılayamayan |")
    s.append("|---|---|---|---|---|---|---|---|---|---|")
    for c in sorted(mkk.sinif.unique()):
        q = mkk[mkk.sinif == c].set_index(["gosterge", "kategori_kod"]).olasilik
        p = pt[pt.sinif == c].iloc[0]
        a0 = f1(q[("et_a0", 0)], 3) if ("et_a0", 0) in q.index else "–"
        a1 = f1(q[("et_a1", 0)], 3) if ("et_a1", 0) in q.index else "–"
        s.append(f"| {c} {mkk[mkk.sinif == c].sinif_ad.iloc[0]} | {f1(p.yuzde)} ({f1(p.alt)}–{f1(p.ust)}) | {f1(q[('he090', 1)], 3)} | {f1(q[('meyve', 3)], 3)} | "
                 f"{f1(q[('meyve', 0)], 3)} | {f1(q[('sebze', 3)], 3)} | {f1(q[('sebze', 0)], 3)} | {f1(q[('et', 0)], 3)} | {a0} | {a1} |")


def artik_tablosu(s, art, mk, K):
    s.append("| Gösterge çifti | X² | sd | BVR | En büyük |z| | |z| > 3 hücre |")
    s.append("|---|---|---|---|---|---|")
    for _, r in art[(art.model == mk) & (art.K == K) & (art.duzey == "cift")].iterrows():
        s.append(f"| {r.cift} | {f1(r.X2)} | {int(r.df)} | {f1(r.BVR)} | {f1(r.max_abs_z)} | {int(r.hucre_z_gt3)} |")


def uzak_tablosu(s, uzak, mk):
    u = uzak[uzak.model == mk]
    s.append("| Sonuç | Ayar | Sınıf / grup | Ölçü | Tahmin (95% GA) | Bootstrap p | Modal n / olay |")
    s.append("|---|---|---|---|---|---|---|")
    for _, r in u.iterrows():
        dd = 2 if "PR" in r.olcu else 1
        sinif = r.sinif_ad if pd.isna(r.sinif) else f"{int(r.sinif)} {r.sinif_ad}"
        no = f"{int(r.modal_n)} / {int(r.modal_olay)}" if not pd.isna(r.get("modal_olay", np.nan)) else (f"{int(r.modal_n)}" if not pd.isna(r.modal_n) else "–")
        tah = f"{f1(r.tahmin, dd)} ({f1(r.alt, dd)}–{f1(r.ust, dd)})" if not pd.isna(r.tahmin) else "tahmin verilmedi"
        notu = r.get("not_", "")
        notu = "" if pd.isna(notu) or notu == "" else f" [{notu}]"
        s.append(f"| {r.sonuc} | {r.ayar} | {sinif} | {r.olcu} | {tah}{notu} | {f1(r.p_boot, 3)} | {no} |")


def ozet_yaz(d, sonuc, uyum, madde, pay, art, siniflama, bch, uzak, etiketler, surum, sure, genel_A):
    s = []
    ua, ud = uyum[uyum.model == "ortak"], uyum[uyum.model == "dort"]
    Ka, Kd = MODELLER["ortak"]["secilen_k"], MODELLER["dort"]["secilen_k"]
    sa, sd_ = ua[ua.K == Ka].iloc[0], ud[ud.K == Kd].iloc[0]
    ad = art[art.duzey == "cift"]
    he_et = lambda k: ad[(ad.model == "dort") & (ad.K == k) & (ad.cift == "he090 x et")].iloc[0]
    s.append("# LCA özeti (Codes/lca.py)\n")
    s.append(f"Çalışma süresi: {sure / 60:.1f} dk. Sürümler: " + ", ".join(f"{k} {v}" for k, v in surum.items())
             + f". En fazla {NJOB} çekirdek (joblib). Evren: 2025, 18+ yetişkin, n = {f1(len(d), 0)} ({f1(d.hane_kimlik.nunique(), 0)} hane); dört göstergede eksik yok. "
             f"Ağırlıklı P(öğünü karşılayamama) = {f1(genel_A, 3)}.\n")
    s.append("## Kararlar (Karar Defteri biçiminde)\n")
    s.append("L1. Göstergeler ve kategoriler. Karar: HE090 (2: karşılayabilir / karşılayamaz), meyve ve sebze 6 → 4 kategori (hiç veya haftada birden az / haftada 1–3 / haftada 4–6 / "
             f"her gün: günde bir ve günde iki+ birleşik), et-tavuk-balık 4 → 3 kategori (kod 0 vejetaryen/vegan {int((d.et_tavuk_balik_siklik == 0).sum())} kişi, %0,34, "
             "'haftada birden az' ile birleştirildi). Gerekçe: talimattaki 4 kategorili meyve-sebze tanımı; vejetaryen kategorisi tek başına sınıflara bilgi taşımayacak kadar seyrek ve "
             "et tüketimi açısından 'haftada birden az' ile aynı düzeyde. Alternatif: kod 0'ı ayrı tutmak (ihmal edilebilir etki). Etki: 96 olası yanıt deseni, hepsi gözlendi.\n")
    s.append("L2. Ağırlık ve kestirim. Karar: FB030, ortalaması 1 olacak biçimde ölçeklendi (toplam = n); ağırlıklı sözde olabilirlik, yanıt desenlerine toplanmış veride (ağırlıklı frekans) "
             "stepmix EM (categorical). Gerekçe: kategorik göstergelerde birey ve desen düzeyi olabilirlik aynıdır; desen düzeyi çok sayıda başlangıcı ucuzlatır. "
             "BIC/aBIC/CAIC n = 57.899 ile; tasarım etkisi hesaba katılmadı. Alternatif: birey düzeyi stepmix (aynı sonuç, daha yavaş).\n")
    s.append(f"L3. Başlangıçlar. Karar: her K için {BASLANGIC} bağımsız rastgele başlangıç (n_init = 1, farklı tohum, en çok 20.000 EM yinelemesi, abs_tol 1e-10); "
             f"en iyi LL başlangıçların %20'sinden azında (tolerans {TOL_LL} LL) tekrarlanırsa {BASLANGIC_EK}'e çıkarılır. Yakınsamayan başlangıç: "
             + "; ".join(f"{mk} K = {int(r.K)}: {int(r.baslangic - r.yakinsayan)} / {int(r.baslangic)}" for mk in MODELLER for _, r in uyum[uyum.model == mk].iterrows() if r.yakinsayan < r.baslangic)
             + ("" if (uyum.yakinsayan == uyum.baslangic).all() else " (diğerlerinin hepsi yakınsadı)")
             + ". Başlangıç sayısı artırılmasına karşın en iyi LL'nin %20'den az tekrarlandığı çözümler: "
             + "; ".join(f"{mk} K = {int(r.K)} (%{f1(r.en_iyi_tekrar_yuzde, 0)}, {int(r.baslangic)} başlangıç)" for mk in MODELLER for _, r in uyum[uyum.model == mk].iterrows() if r.en_iyi_tekrar_yuzde < 20)
             + "; bu çözümler seçilmedi (olabilirlik yüzeyi düz, zayıf tanımlanma). Her başlangıcın LL'si Results/log/lca_baslangic_ll.csv.\n")
    s.append(f"L4. BLRT. Karar: parametrik bootstrap LRT (K−1'e karşı K), B = {B_BLRT}; her benzetimde K−1 modeli {BLRT_BASLANGIC // 2}, K modeli {BLRT_BASLANGIC} başlangıçla "
             "(en çok 3.000 yineleme); benzetim n = 57.899 ağırlıksız. Gerekçe: stepmix'in blrt işlevi örneklem ağırlığını desteklemiyor. Sınırlılık: gözlenen istatistik ağırlıklı "
             "sözde olabilirlikten, null dağılımı ağırlıksız benzetimden; bu büyüklükte örneklemde BLRT her K artışını anlamlı bulmaya yatkın (bütün p = 1/101), seçimde belirleyici kullanılmadı.\n")
    s.append(f"L5. Yerel bağımsızlık ve ana model. Karar: ana LCA, HE090 ile et-tavuk-balık sıklığını tek bir 6 kategorili ortak göstergede (karşılanabilirlik × et sıklığı) birleştiren "
             f"üç göstergeli model (ortak gösterge, meyve, sebze); dört ayrı göstergeli model duyarlılık analizi. Gerekçe: dört göstergeli modelde (talimattaki tanım) HE090 × et çiftinde "
             f"güçlü artık bağımlılık var: K = {Kd - 1} BVR {f1(he_et(Kd - 1).BVR)}, en büyük |z| {f1(he_et(Kd - 1).max_abs_z)}; K = {Kd} BVR {f1(he_et(Kd).BVR)}, |z| {f1(he_et(Kd).max_abs_z)}; "
             f"K = {Kd + 1} BVR {f1(he_et(Kd + 1).BVR)}, |z| {f1(he_et(Kd + 1).max_abs_z)}. İki madde içerikçe örtüşür (HE090 iki günde bir et-tavuk-balık içeren öğünü karşılayabilmeyi sorar). "
             "Talimattaki sıra (Bölüm 6, adım 10; orada meyve–sebze için yazılmış) bağımlılığı gösteren çifte uygulandı: önce sınıf sayısı bir artırıldı (bağımlılık sürdü), sonra iki gösterge "
             "tek göstergede birleştirildi; bu, sınıf içinde HE090–et ilişkisine serbest bir parametre tanımakla eşdeğerdir (yerel bağımlılığın modellenmesi). Meyve–sebze çiftinde "
             "hiçbir çözümde |z| > 3 yok. Ortak göstergeli modelde bütün çiftlerde |z| < 2. Alternatifler: dört göstergeli K = 5 (ekte, tam BCH sonuçlarıyla) ve K = 6.\n")
    s.append(f"L6. Sınıf sayısı (ana model). Karar: K = {Ka}. Gerekçe: BIC ve CAIC K = {int(ua.loc[ua.BIC.idxmin(), 'K'])}'te en küçük (gerçek minimum); aBIC en küçük K = "
             f"{int(ua.loc[ua.aBIC.idxmin(), 'K'])}; K = {Ka}'te bağıl entropi {f1(sa.rel_entropi, 3)}, en küçük sınıf %{f1(sa.en_kucuk_sinif_yuzde)} (> %5), en iyi LL başlangıçların "
             f"%{f1(sa.en_iyi_tekrar_yuzde, 0)}'inde tekrarlandı; K = {Ka + 1}'te entropi {f1(ua[ua.K == Ka + 1].rel_entropi.iloc[0], 3)} ve LL tekrarı %{f1(ua[ua.K == Ka + 1].en_iyi_tekrar_yuzde.iloc[0], 0)}, "
             "200 başlangıçta bile en iyi LL nadiren tekrarlanıyor ve ek sınıf ayrı bir kuramsal profil eklemeden günlük tüketen grubu et sıklığına göre ikiye bölüyor. Dört göstergeli duyarlılık modelinde K = 5 seçildi: BIC 6'ya kadar azalıyor, kazanç 3→4 "
             f"{f1(-ud[ud.K == 4].delta_BIC.iloc[0], 0)}, 4→5 {f1(-ud[ud.K == 5].delta_BIC.iloc[0], 0)}, 5→6 {f1(-ud[ud.K == 6].delta_BIC.iloc[0], 0)} puan (dirsek 4–5); "
             f"K = 5 bağıl entropi {f1(sd_.rel_entropi, 3)}, en küçük sınıf %{f1(sd_.en_kucuk_sinif_yuzde)}; 'karşılayabilen ama seyrek tüketen' profil K = 5'te ayrışıyor.\n")
    s.append("L7. BCH. Karar: üç adımlı BCH elle uygulandı: D = P(modal sınıf | gerçek sınıf) FB030 ağırlıklı sonsal olasılıklardan; BCH ağırlıkları D⁻¹'in modal sınıf satırı. "
             "Kovaryat modeli: BCH ağırlıklı çok terimli lojit (Newton). Uzak sonuçlar: (i) BCH ağırlıklı oran (düzeltmesiz); (ii) BCH ağırlıklı genişletilmiş veride lojistik model "
             "(sınıf + kovaryatlar), g-hesaplamayla standardize sınıf yaygınlıkları; (iii) yalnız ana modelde ayrıca A (HE090) eklenmiş model: sınıf farkları A'dan, A farkı sınıftan arındırılmış "
             "standardize yaygınlıklar (bu modelde D, A katmanlarında ayrı hesaplandı; A göstergenin parçası olduğundan). Kovaryatlar: gelir dilimi, yaş grubu (18–29, 30–44, 45–64, 65+), "
             f"cinsiyet, eğitim (5), bölge (İBBS-1), hane tipi; odeme_tibbi'de (329 olay) olay/parametre oranını korumak için yalnız gelir dilimi, yaş grubu ve cinsiyet (talimattaki küçültülmüş set mantığı; tam setle nokta tahminleri benzer). Doğrulama: kovaryatsız lojistik model düzeltmesiz BCH ortalamalarını birebir verir (günlükte 'BCH denetim'). "
             "BCH ağırlıklı ortalama 0'ın altına düşebilir; bu durumda tahmin/sınır 0'da kesildi ve işaretlendi; referans sınıfın yaygınlığı 0'dan ayırt edilemiyorsa PR verilmedi. GA: hane kümeli Rao-Wu bootstrap (B = {B_BOOT}; n_h − 1 hane iadeli, ağırlık × n/(n−1) × çokluk), sonsal olasılıklar sabit "
             "(ölçme modeli tekrarlarda yeniden kestirilmedi; GA'lar bu belirsizliği içermez). Modal sınıfta ağırlıksız n < 20 hücrede tahmin verilmez (hiçbir hücre bu sınırın altında kalmadı).\n")
    rd = bch["dort"]["ref"]
    kis = [k for k in range(Kd) if marjinal(MODELLER["dort"], sonuc["dort"]["cozum"][Kd]["theta"])["pa"][k] >= 0.5]
    s.append(f"L8. Karşılaştırma. Ana modelde 'karşılayabilen ama seyrek tüketen' ayrı bir sınıf çıkmadı: sınıflar tüketim sıklığı boyunca diziliyor, karşılayamama payı sıklık azaldıkça artıyor. "
             "En yakın karşılaştırma olarak (a) her sınıf, en sık tüketen sınıf 1'e karşı ve (b) A = 1 ile A = 0, sınıftan arındırılmış olarak verildi; böylece yükün tüketim boyutunu mu, "
             f"karşılanabilirlik boyutunu mu izlediği aynı modelde ayrıştırılıyor. Dört göstergeli duyarlılık modelinde talimattaki karşılaştırma yapıldı: referans '{rd + 1} {etiketler['dort'][Kd][rd]}'; "
             "P(karşılayamaz) ≥ 0,5 sınıflar: " + ", ".join(f"{k + 1} {etiketler['dort'][Kd][k]}" for k in kis) + ".\n")
    s.append("\n## Uyum ölçütleri: ana model (ortak gösterge, meyve, sebze)\n")
    uyum_tablosu(s, ua)
    s.append("\n## Uyum ölçütleri: duyarlılık modeli (dört ayrı gösterge)\n")
    uyum_tablosu(s, ud)
    for mk, K in (("ortak", Ka), ("dort", Kd)):
        u = uyum[(uyum.model == mk) & (uyum.K == K)].iloc[0]
        if u.rel_entropi < 0.6:
            s.append(f"\nUYARI ({mk}, K = {K}): bağıl entropi {f1(u.rel_entropi, 3)} < 0,6; sınıflar düşük ayrışmalı, uzak sonuç farkları ihtiyatla raporlanmalı.")
    s.append(f"\nBağıl entropi ana modelde {f1(sa.rel_entropi, 3)}, duyarlılık modelinde {f1(sd_.rel_entropi, 3)}: ikisi de 0,6'nın üstünde.\n")
    s.append("\nSınıflama (AvePP = modal sınıfta ortalama sonsal olasılık; P(doğru) = D'nin köşegeni), `Results/lca_siniflama.csv`:\n")
    s.append("| Model | Sınıf | AvePP | P(doğru modal) | Modal n |")
    s.append("|---|---|---|---|---|")
    for _, r in siniflama.iterrows():
        k = int(r.gercek_sinif.split(" ")[0])
        s.append(f"| {r.model} | {r.gercek_sinif} | {f1(r.AvePP_modal, 3)} | {f1(r[f'modal_{k}'], 3)} | {int(r.modal_n)} |")
    s.append(f"\n## Sınıf profilleri: ana model (K = {Ka})\n")
    profil_tablosu(s, madde, pay, "ortak", Ka)
    s.append("\nHE090 ve et olasılıkları ortak göstergenin marjinalleridir; son iki sütun sınıf içinde karşılanabilirliğe göre koşullu et sıklığıdır (yerel bağımlılık).")
    s.append(f"\n## Sınıf profilleri: dört göstergeli duyarlılık modeli (K = {Kd})\n")
    profil_tablosu(s, madde, pay, "dort", Kd)
    s.append("\nGelir dilimine ve bölgeye göre paylar: `Results/lca_sinif_paylari.csv`, `figur_verileri/fig3b_lca_gelir.csv`, `fig3c_lca_bolge.csv`. Komşu çözümler (K − 1, K + 1) "
             "madde olasılıkları ve paylarla aynı dosyalarda.\n")
    s.append(f"\n## Yerel bağımsızlık (ikili artıklar)\n\nAna model, K = {Ka}:\n")
    artik_tablosu(s, art, "ortak", Ka)
    s.append(f"\nDört göstergeli model, K = {Kd}:\n")
    artik_tablosu(s, art, "dort", Kd)
    s.append("\nKomşu çözümler:")
    for mk, K in (("ortak", Ka), ("dort", Kd)):
        for k in (K - 1, K + 1):
            a = ad[(ad.model == mk) & (ad.K == k)]
            s.append(f"- {mk}, K = {k}: " + "; ".join(f"{r.cift} BVR {f1(r.BVR)}, max|z| {f1(r.max_abs_z)}" for _, r in a.iterrows()))
    s.append("\nArtıklar ağırlıklı sözde frekanslardan; FB030'un ağırlık tasarım etkisi (≈1,42) hesaba katılsa z değerleri yaklaşık 1,19'a bölünür (sonuç değişmez).")
    s.append("\n## Üç adımlı BCH: sınıf üyeliği ~ kovaryatlar (ana model)\n")
    s.append("Çok terimli lojit, referans sınıf 1; bütün ORs ve bootstrap %95 GA `Results/lca_multinomial.csv`. Gelir dilimi ORs (Q1'e göre):\n")
    mnl = bch["ortak"]["mnl"]
    for c in range(2, Ka + 1):
        q = mnl[(mnl.sinif == c) & (mnl.terim.str.startswith("income"))]
        s.append(f"- {c} {etiketler['ortak'][Ka][c - 1]}: " + "; ".join(f"{r.terim.replace('income=', 'Q').replace('.0', '')} OR {f1(r.OR, 2)} ({f1(r.OR_alt, 2)}–{f1(r.OR_ust, 2)})" for _, r in q.iterrows()))
    s.append("\n## Üç adımlı BCH: uzak sonuçlar\n")
    s.append("Y_saglik_yuk hane değişkenidir ve ferde bağlanmıştır: tahmin 'belirli bir profildeki yetişkinlerin ne kadarı sağlık harcaması yükü yüksek bir hanede yaşıyor' sorusunun yanıtıdır; "
             "hane düzeyi analizdeki (hanelerin yüzdesi) tahminle aynı büyüklük değildir. odeme_tibbi yalnız tıbbi ihtiyacı olanlarda (ihtiyac_var = 1). PD yüzde puan, PR oran.\n")
    s.append("\nAna model:\n")
    uzak_tablosu(s, uzak, "ortak")
    s.append("\nDört göstergeli duyarlılık modeli:\n")
    uzak_tablosu(s, uzak, "dort")
    s.append(f"\nBootstrap süreleri: ana {bch['ortak']['sure']:.0f} sn, duyarlılık {bch['dort']['sure']:.0f} sn; BLRT ana {sonuc['ortak']['sure_blrt']:.0f} sn, duyarlılık {sonuc['dort']['sure_blrt']:.0f} sn.\n")
    s.append("\n## Sınırlılıklar\n")
    s.append("- Sözde olabilirlik ve bilgi ölçütleri tasarımı (tabaka/küme) hesaba katmaz.")
    s.append("- HE090 hane düzeyindedir: aynı hanedeki yetişkinler aynı değeri paylaşır; LCA gözlemleri bağımsız sayar (GA'lar hane kümeli bootstrap'la).")
    s.append("- BCH GA'ları ölçme modeli belirsizliğini içermez (sonsal olasılıklar sabit).")
    s.append("- Ana modelde HE090 ile et sıklığı tek göstergede birleştiği için sınıflar karşılanabilirliği ayrı bir boyut olarak tanımlamaz; karşılanabilirlik sınıflar arasında bir eğilim ve sınıf içi bir ilişki olarak görülür.")
    s.append("- Sınıflar keşifsel ve betimleyicidir; uzak sonuç farkları nedensel yorumlanamaz, bir makullük denetimidir.")
    (LOGD / "lca_ozet.md").write_text("\n".join(s), encoding="utf-8")


if __name__ == "__main__":
    main()
