import sys

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
from matplotlib.lines import Line2D

sys.path.insert(0, str(__import__("pathlib").Path(__file__).resolve().parent))
from figur_stili import (VERI, SIM, SONUC, TAM, TEK, CM, MUREKKEP, IKINCIL, SOLUK, IZGARA, RENK_A, ETIKET_A, RENK_GIDA, ETIKET_GIDA,
                         ETIKET_GIDA_KISA, RENK_GELIR, ETIKET_GELIR, RENK_SINIF, stil_kur, panel_etiketi, panel_etiketi_fig, kaydet)

stil_kur()


def sayi(n):
    return f"{int(n):,}"


def kutu(ax, x, y, w, h, metin, kenar, dolgu, boyut=7.2, agirlik="normal", hiza="center", lw=0.8):
    ax.add_patch(FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0,rounding_size=0.012", linewidth=lw, edgecolor=kenar, facecolor=dolgu, transform=ax.transAxes))
    tx = x + w / 2 if hiza == "center" else x + 0.012
    ax.text(tx, y + h / 2, metin, ha=hiza, va="center", fontsize=boyut, fontweight=agirlik, color=MUREKKEP, transform=ax.transAxes, linespacing=1.35)


def ok(ax, x0, y0, x1, y1, renk=IKINCIL, lw=0.9, stil="-|>", ls="-", rad=0.0):
    ax.add_patch(FancyArrowPatch((x0, y0), (x1, y1), arrowstyle=stil, mutation_scale=7, linewidth=lw, color=renk, transform=ax.transAxes,
                                 linestyle=ls, connectionstyle=f"arc3,rad={rad}", shrinkA=0, shrinkB=0))


from matplotlib.path import Path
from matplotlib.patches import PathPatch, Ellipse

KAGIT = "#F4F1EC"
KISI_R = "#3D6E8C"
SON_R = "#1F7A5C"
DIS_R = "#C7B5A3"
GRI_R = "#C9CDD2"


def serit(ax, x0, y0a, y0b, x1, y1a, y1b, renk, alfa=0.38, z=1):
    xm = (x0 + x1) / 2
    v = [(x0, y0a), (xm, y0a), (xm, y1a), (x1, y1a), (x1, y1b), (xm, y1b), (xm, y0b), (x0, y0b), (x0, y0a)]
    k = [Path.MOVETO, Path.CURVE4, Path.CURVE4, Path.CURVE4, Path.LINETO, Path.CURVE4, Path.CURVE4, Path.CURVE4, Path.CLOSEPOLY]
    ax.add_patch(PathPatch(Path(v, k), facecolor=renk, edgecolor="none", alpha=alfa, transform=ax.transAxes, zorder=z))


def cubuk(ax, x, ya, yb, renk, w=0.016):
    ax.add_patch(plt.Rectangle((x, yb), w, ya - yb, facecolor=renk, edgecolor="none", transform=ax.transAxes, zorder=3))


def etiket2(ax, x, y, ust, alt_, renk=MUREKKEP, ha="left", boyut=7.2):
    ax.text(x, y, ust, ha=ha, va="bottom", fontsize=boyut, fontweight="bold", color=renk, transform=ax.transAxes, zorder=5)
    ax.text(x, y - 0.006, alt_, ha=ha, va="top", fontsize=5.9, color=IKINCIL, transform=ax.transAxes, zorder=5, linespacing=1.2)


def egri_ok(ax, p0, p1, p2, renk, lw=1.1, z=4):
    yol = Path([p0, p1, p2], [Path.MOVETO, Path.CURVE3, Path.CURVE3])
    ax.add_patch(FancyArrowPatch(path=yol, arrowstyle="-|>", mutation_scale=7, linewidth=lw, color=renk, transform=ax.transAxes, zorder=z))


def elips(ax, x, y, w, h, dolgu, kenar, z=3):
    ax.add_patch(Ellipse((x, y), w, h, facecolor=dolgu, edgecolor=kenar, linewidth=0.8, transform=ax.transAxes, zorder=z))


def figure1():
    a = pd.read_csv(VERI / "fig1_akis.csv").set_index("adim")["n"]
    fig = plt.figure(figsize=(TAM, 18.0 * CM))
    axa = fig.add_axes([0.0, 0.53, 1.0, 0.45])
    axb = fig.add_axes([0.0, 0.0, 1.0, 0.48])
    for ax in (axa, axb):
        ax.set_axis_off()
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
    panel_etiketi_fig(fig, "a", 0.006, 0.995)
    panel_etiketi_fig(fig, "b", 0.006, 0.508)

    toplam = a["kaynak"]
    sk = 0.56 / toplam
    xA, xB, xC, xD = 0.035, 0.235, 0.515, 0.765
    yA = 0.93
    cubuk(axa, xA, yA, yA - toplam * sk, "#55606B")
    axa.text(xA, yA + 0.018, f"{sayi(toplam)} persons", ha="left", va="bottom", fontsize=7.4, fontweight="bold", color=MUREKKEP, transform=axa.transAxes)
    axa.text(xA + 0.135, yA + 0.019, "ILCS cross-sections 2023–2025", ha="left", va="bottom", fontsize=5.9, color=IKINCIL, transform=axa.transAxes)
    yillar = [(2023, a["yil_2023_kisi"]), (2024, a["yil_2024_kisi"]), (2025, a["yil_2025_kisi"])]
    bos = 0.035
    yb_ust = 0.93
    ya_ust = yA
    bloklar = {}
    for yil, n in yillar:
        h = n * sk
        renk = KISI_R if yil == 2025 else GRI_R
        serit(axa, xA + 0.016, ya_ust, ya_ust - h, xB, yb_ust, yb_ust - h, renk, alfa=0.30 if yil == 2025 else 0.45)
        cubuk(axa, xB, yb_ust, yb_ust - h, renk)
        bloklar[yil] = (yb_ust, yb_ust - h)
        xm, ym = (xA + 0.016 + xB) / 2, (ya_ust + yb_ust - h) / 2
        if yil != 2025:
            etiket2(axa, xm, ym + 0.006, f"{yil} · {sayi(n)}", "context analyses only", renk=SOLUK, ha="center", boyut=6.6)
        else:
            etiket2(axa, xm, ym + 0.006, f"{yil} · {sayi(n)} persons", f"in {sayi(a['k2025_hane'])} households", ha="center", boyut=6.8)
        ya_ust -= h
        yb_ust -= h + bos
    u25, a25 = bloklar[2025]
    n_gor = a["k2025_fkayit"]
    n_15 = a["dis_0_14"]
    n_kay = a["dis_15plus_kayitsiz"]
    hc = n_gor * sk
    yC = 0.80
    serit(axa, xB + 0.016, u25, u25 - hc, xC, yC, yC - hc, KISI_R, alfa=0.30)
    cubuk(axa, xC, yC, yC - hc, KISI_R)
    etiket2(axa, xC + 0.026, yC - 0.004, f"{sayi(n_gor)}", "aged ≥15 with an\nindividual questionnaire", boyut=7.0)
    y_alt = u25 - hc
    h15 = n_15 * sk
    hkay = max(n_kay * sk, 0.0015)
    yk = 0.31
    serit(axa, xB + 0.016, y_alt, y_alt - hkay, xC - 0.07, yk, yk - hkay, DIS_R, alfa=0.9)
    axa.text(xC - 0.065, yk - 0.002, f"{sayi(n_kay)} aged ≥15 without an individual interview", fontsize=5.9, color=IKINCIL, va="center", ha="left", transform=axa.transAxes)
    y15 = 0.27
    serit(axa, xB + 0.016, y_alt - hkay, y_alt - hkay - h15, xC - 0.07, y15, y15 - h15, DIS_R, alfa=0.55)
    axa.text(xC - 0.065, y15 - h15 / 2, f"{sayi(n_15)} aged <15 years", fontsize=5.9, color=IKINCIL, va="center", ha="left", transform=axa.transAxes)
    n_y = a["analitik_yetiskin"]
    n_1517 = a["dis_15_17"]
    hd = n_y * sk
    yD = 0.84
    serit(axa, xC + 0.016, yC, yC - hd, xD, yD, yD - hd, SON_R, alfa=0.30)
    cubuk(axa, xD, yD, yD - hd, SON_R)
    etiket2(axa, xD + 0.026, yD - 0.004, f"{sayi(n_y)} adults", f"aged ≥18 · adult analytic sample\nin {sayi(a['analitik_hane'])} households", renk=SON_R, boyut=7.4)
    h1517 = n_1517 * sk
    y1517 = 0.60
    serit(axa, xC + 0.016, yC - hd, yC - hd - h1517, xD - 0.04, y1517, y1517 - h1517, DIS_R, alfa=0.7)
    axa.text(xD - 0.035, y1517 - h1517 / 2, f"{sayi(n_1517)} aged 15–17 years", fontsize=5.9, color=IKINCIL, va="center", ha="left", transform=axa.transAxes)
    hh = a["k2025_hane"] * sk
    yh = 0.165
    cubuk(axa, xB, yh, yh - hh, "#6F8F7E")
    axa.text(xB - 0.012, yh - hh / 2, f"{sayi(a['k2025_hane'])} households (2025)", ha="right", va="center", fontsize=6.8, fontweight="bold", color=MUREKKEP, transform=axa.transAxes)
    serit(axa, xB + 0.016, yh, yh - hh, xD, yh, yh - hh, SON_R, alfa=0.18)
    cubuk(axa, xD, yh, yh - hh, SON_R)
    etiket2(axa, xD + 0.026, yh - 0.004, f"{sayi(a['analitik_hane'])} households", f"household analytic sample;\n{sayi(a['Y_olay_hane'])} with a heavy burden;\n{sayi(a['dis_hane_yetiskinsiz'])} without an adult excluded", renk=SON_R, boyut=7.4)
    axa.text(0.035, 0.0, f"Band widths are proportional to the number of persons or households. The meat outcome excludes {sayi(a['et_vejetaryen_dislandi'])} vegetarian or vegan adults;\nunmet-need analyses use the {sayi(a['ihtiyac_var'])} adults reporting a medical care need.",
             transform=axa.transAxes, ha="left", va="bottom", fontsize=5.5, color=SOLUK, linespacing=1.3)

    zemin = FancyBboxPatch((0.012, 0.305), 0.976, 0.685, boxstyle="round,pad=0,rounding_size=0.06", facecolor=KAGIT, edgecolor="none", transform=axb.transAxes, zorder=0)
    axb.add_patch(zemin)
    axb.text(0.035, 0.33, "Adjusted throughout for income quintile, household composition, age, sex, education, region and health need",
             transform=axb.transAxes, fontsize=5.9, color=IKINCIL, style="italic", va="bottom")
    ek, mk, ok_ = "#C8551A", "#5B3F99", "#1F7A5C"
    elips(axb, 0.175, 0.66, 0.27, 0.44, "#F8DFCF", ek)
    axb.text(0.175, 0.80, "EXPOSURE", ha="center", fontsize=5.6, fontweight="bold", color=ek, transform=axb.transAxes, zorder=5)
    axb.text(0.175, 0.645, "Household cannot\nafford a meal with\nmeat, chicken or fish\nevery second day", ha="center", va="center", fontsize=6.6, color=MUREKKEP, transform=axb.transAxes, linespacing=1.2, zorder=5)
    axb.text(0.175, 0.475, r"$A$  (HE090)", ha="center", fontsize=6.4, color=ek, transform=axb.transAxes, zorder=5)
    elips(axb, 0.47, 0.875, 0.235, 0.17, "#E8E2F2", mk)
    axb.text(0.47, 0.905, "MODIFIER", ha="center", fontsize=5.6, fontweight="bold", color=mk, transform=axb.transAxes, zorder=5)
    axb.text(0.47, 0.855, "adult with chronic illness\nin the household  " + r"$C$", ha="center", va="center", fontsize=6.3, color=MUREKKEP, transform=axb.transAxes, linespacing=1.2, zorder=5)
    hedefler = [(0.785, 0.80, 0.30, r"PRIMARY · $\mathbf{H}_\mathbf{1}$ · households", "medical or medicine spending\na heavy burden  " + r"$Y$"),
                (0.815, 0.585, 0.325, r"$\mathbf{H}_\mathbf{2}$ · adults", "low-frequency consumption of\nmeat, chicken or fish, fruit, vegetables"),
                (0.785, 0.40, 0.30, "SECONDARY · adults", "unmet medical or dental\nneed because of cost")]
    for x, y, gen, bas, met in hedefler:
        elips(axb, x, y, gen, 0.165, "#D9ECE3", ok_)
        axb.text(x, y + 0.045, bas, ha="center", fontsize=5.5, fontweight="bold", color=ok_, transform=axb.transAxes, zorder=5)
        axb.text(x, y - 0.012, met, ha="center", va="center", fontsize=6.2, color=MUREKKEP, transform=axb.transAxes, linespacing=1.2, zorder=5)
    p0 = (0.305, 0.72)
    y_kontrol = (0.47, 0.77)
    y_son = (0.636, 0.805)
    egri_ok(axb, p0, y_kontrol, y_son, ek, lw=1.3)
    egri_ok(axb, (0.31, 0.64), (0.50, 0.60), (0.653, 0.588), ek, lw=1.1)
    egri_ok(axb, (0.296, 0.56), (0.45, 0.42), (0.636, 0.405), ek, lw=1.1)
    t = 0.5
    mx = (1 - t) ** 2 * p0[0] + 2 * (1 - t) * t * y_kontrol[0] + t ** 2 * y_son[0]
    my = (1 - t) ** 2 * p0[1] + 2 * (1 - t) * t * y_kontrol[1] + t ** 2 * y_son[1]
    axb.plot([0.47, mx], [0.79, my], color=mk, lw=1.2, transform=axb.transAxes, zorder=5, solid_capstyle="butt")
    axb.add_patch(Ellipse((mx, my), 0.013, 0.022, facecolor=mk, edgecolor="white", linewidth=0.8, transform=axb.transAxes, zorder=6))
    axb.text(0.025, 0.245, "Estimands", fontsize=6.6, fontweight="bold", color=MUREKKEP, transform=axb.transAxes, va="center")
    axb.text(0.14, 0.245, r"$\mathrm{H}_1$   $\Delta = p_{11} - p_{10} - p_{01} + p_{00}$   and   RERI $= \Delta / p_{00}$", fontsize=7.4, color=MUREKKEP, transform=axb.transAxes, va="center")
    axb.text(0.62, 0.245, r"$\mathrm{H}_2$   PD $= p_1 - p_0$,   PR $= p_1 / p_0$", fontsize=7.4, color=MUREKKEP, transform=axb.transAxes, va="center")
    axb.text(0.14, 0.205, "standardized prevalences from survey-weighted models with g-computation; household bootstrap intervals", fontsize=5.7, color=IKINCIL, transform=axb.transAxes, va="center")
    xs = np.linspace(0.04, 0.96, 300)
    ys = 0.115 + 0.018 * np.sin((xs - 0.04) / 0.92 * 2 * np.pi * 1.25)
    axb.plot(xs, ys, color="#B9B2A8", lw=1.2, transform=axb.transAxes, zorder=1)
    duraklar = [("Describe", "2025 profile,\n2023–2025 context"), ("Model", "weighted logistic,\ng-computation"), ("Profile", "latent classes,\nthree-step BCH"),
                ("Stress-test", "sensitivity\nanalyses"), ("Simulate", "plasmode precision,\nbias, clustering")]
    for k, (bas, alt_) in enumerate(duraklar):
        xx = 0.05 + k * 0.188
        yy = 0.115 + 0.018 * np.sin((xx - 0.04) / 0.92 * 2 * np.pi * 1.25)
        aralik = ys[(xs >= xx + 0.02) & (xs <= xx + 0.16)]
        axb.add_patch(Ellipse((xx, yy), 0.034, 0.06, facecolor="white", edgecolor=MUREKKEP, linewidth=0.9, transform=axb.transAxes, zorder=3))
        axb.text(xx, yy, str(k + 1), ha="center", va="center", fontsize=6.6, fontweight="bold", color=MUREKKEP, transform=axb.transAxes, zorder=4)
        axb.text(xx + 0.026, aralik.max() + 0.01, bas, ha="left", va="bottom", fontsize=6.6, fontweight="bold", color=MUREKKEP, transform=axb.transAxes)
        axb.text(xx + 0.026, aralik.min() - 0.012, alt_, ha="left", va="top", fontsize=5.7, color=IKINCIL, transform=axb.transAxes, linespacing=1.2)
    return kaydet(fig, "Figure1")


KISA = {"Never": "Never", "Less than once a week": "<1/week", "1–3 times a week": "1–3/week", "4–6 times a week": "4–6/week", "Once a day": "Once a day",
        "Twice a day or more": "≥2/day", "Never (vegetarian or vegan)": "Never (vegetarian)", "Weekly, not daily": "Weekly, not daily", "Daily": "Daily"}
RAMP6 = ["#E4EAF1", "#C4D0DE", "#9CB0C8", "#7290B1", "#486D97", "#244A7C"]


def figure2():
    a = pd.read_csv(VERI / "fig2a_he090_trend.csv")
    eu = pd.read_csv(VERI / "eurostat_ilc_mdes03.csv")
    b = pd.read_csv(VERI / "fig2b_tuketim_sikligi_A.csv")
    c = pd.read_csv(VERI / "fig2c_standardize_fark_gelir.csv")
    fig = plt.figure(figsize=(TAM, 17.0 * CM))
    ax = fig.add_axes([0.085, 0.555, 0.30, 0.41])
    yillar = [2023, 2024, 2025]
    for q in range(1, 6):
        d = a[a.kategori == str(q)].sort_values("yil")
        ax.fill_between(d.yil, d.alt, d.ust, color=RENK_GELIR[q - 1], alpha=0.18, linewidth=0)
        ax.plot(d.yil, d.yuzde, color=RENK_GELIR[q - 1], lw=1.4, marker="o", ms=3.0, mec="white", mew=0.5)
        ax.text(2025.1, d.yuzde.iloc[-1], ETIKET_GELIR[q - 1], va="center", fontsize=6.4, color=MUREKKEP)
    d = a[a.kategori == "Total"].sort_values("yil")
    ax.plot(d.yil, d.yuzde, color=MUREKKEP, lw=1.8, marker="o", ms=3.2, mec="white", mew=0.5)
    ax.text(2025.1, d.yuzde.iloc[-1] + 2.6, "All persons", va="center", fontsize=6.4, color=MUREKKEP, fontweight="bold")
    e = eu[(eu.geo == "EU27_2020") & (eu.yil.isin(yillar))].sort_values("yil")
    ax.plot(e.yil, e.yuzde, color=SOLUK, lw=1.0, ls=(0, (3, 2)))
    ax.text(2025.1, e.yuzde.iloc[-1], "EU-27", va="center", fontsize=6.4, color=IKINCIL)
    ax.set_xticks(yillar)
    ax.set_xlim(2022.8, 2025.95)
    ax.set_ylim(0, 80)
    ax.set_ylabel("Persons unable to afford the meal (%)")
    ax.grid(axis="x", visible=False)
    ax.spines["bottom"].set_bounds(2023, 2025)
    panel_etiketi(ax, "a", x=-0.16, y=1.0)

    gidalar = [("et_tavuk_balik_siklik", "Meat, chicken or fish"), ("meyve_siklik", "Fruit"), ("sebze_siklik", "Vegetables or salad")]
    for i, (g, ad) in enumerate(gidalar):
        axi = fig.add_axes([0.62, 0.875 - i * 0.135, 0.36, 0.072])
        d = b[b.gida == g]
        kodlar = sorted(d.k.unique())
        renkler = RAMP6 if len(kodlar) == 6 else [RAMP6[0], RAMP6[2], RAMP6[4], RAMP6[5]]
        for j, aa in enumerate([0, 1]):
            sol = 0.0
            for kk, rr in zip(kodlar, renkler):
                v = d[(d.A == aa) & (d.k == kk)].yuzde
                v = float(v.iloc[0]) if len(v) else 0.0
                axi.barh(1 - j, v, left=sol, height=0.66, color=rr, edgecolor="white", linewidth=0.8)
                if v >= 8:
                    axi.text(sol + v / 2, 1 - j, f"{v:.0f}", ha="center", va="center", fontsize=5.8, color="white" if rr in RAMP6[3:] else MUREKKEP)
                sol += v
        axi.set_yticks([1, 0])
        axi.set_yticklabels([ETIKET_A[0], ETIKET_A[1]], fontsize=6.6)
        for t, aa in zip(axi.get_yticklabels(), [0, 1]):
            t.set_color(RENK_A[aa])
        axi.set_xlim(0, 100)
        axi.set_ylim(-0.5, 1.5)
        axi.grid(False)
        for sp in ("left", "bottom"):
            axi.spines[sp].set_visible(False)
        axi.set_xticks([])
        axi.tick_params(axis="y", length=0)
        axi.set_title(ad, loc="left", fontsize=7.2, fontweight="bold", pad=2)
        etiketler = [KISA.get(d[d.k == kk].kategori.iloc[0], d[d.k == kk].kategori.iloc[0]) for kk in kodlar]
        tutamak = [plt.Rectangle((0, 0), 1, 1, color=rr) for rr in renkler]
        if i != 1:
            axi.legend(tutamak, etiketler, ncol=len(kodlar) if len(kodlar) == 4 else 3, fontsize=5.8, loc="upper left", bbox_to_anchor=(-0.01, -0.02), handlelength=0.8, handleheight=0.8,
                       columnspacing=0.9, handletextpad=0.3, borderaxespad=0, title="Fruit and vegetables:" if i == 2 else None, title_fontsize=5.8, alignment="left")
        if i == 0:
            panel_etiketi(axi, "b", x=-0.30, y=1.28)

    axc = fig.add_axes([0.14, 0.075, 0.84, 0.37])
    satirlar = ["T", "1", "2", "3", "4", "5"]
    satir_ad = ["All adults"] + ETIKET_GELIR
    ofset = {"et_haftada_az": -0.22, "meyve_gunluk_degil": 0.0, "sebze_gunluk_degil": 0.22}
    for g in ["et_haftada_az", "meyve_gunluk_degil", "sebze_gunluk_degil"]:
        for yi, s_ in enumerate(satirlar):
            r = c[(c.sonuc == g) & (c.gelir.astype(str) == s_)]
            if not len(r):
                continue
            r = r.iloc[0]
            yy = yi + ofset[g]
            axc.plot([r.PD_alt, r.PD_ust], [yy, yy], color=RENK_GIDA[g], lw=1.3, solid_capstyle="round")
            axc.plot(r.PD, yy, "o", ms=4.0 if s_ != "T" else 5.0, color=RENK_GIDA[g], mec="white", mew=0.6)
    axc.axhline(0.5, color=IZGARA, lw=0.8)
    axc.axvline(0, color=IKINCIL, lw=0.6)
    axc.set_yticks(range(len(satirlar)))
    axc.set_yticklabels(satir_ad)
    axc.get_yticklabels()[0].set_fontweight("bold")
    axc.set_ylim(5.5, -0.9)
    axc.grid(axis="y", visible=False)
    axc.set_xlim(-2, 46)
    axc.set_xlabel("Standardized prevalence difference, cannot afford vs can afford (percentage points, 95% CI)")
    axc.set_ylabel("Equivalized income quintile", labelpad=4)
    tut = [Line2D([0], [0], color=RENK_GIDA[g], marker="o", lw=1.3, ms=4, mec="white") for g in RENK_GIDA]
    adlar = ["Meat, chicken or fish less than weekly", "Fruit less than daily", "Vegetables or salad less than daily"]
    axc.legend(tut, adlar, loc="lower left", bbox_to_anchor=(0.0, 1.0), ncol=3, fontsize=6.6, handlelength=1.6, borderaxespad=0.2)
    panel_etiketi(axc, "c", x=-0.105, y=1.07)
    return kaydet(fig, "Figure2")


def hucre_paneli(ax, d, baslik, c_etiket):
    d = d.set_index("olcu")
    xs = {"p00": 0 - 0.13, "p10": 0 + 0.13, "p01": 1 - 0.13, "p11": 1 + 0.13}
    for k, x in xs.items():
        a_ = int(k[1])
        ax.plot([x, x], [d.loc[k, "alt"], d.loc[k, "ust"]], color=RENK_A[a_], lw=1.4, zorder=4)
        ax.plot(x, d.loc[k, "tahmin"], "o", color=RENK_A[a_], ms=5.5, mec="white", mew=0.7, zorder=5)
    be = d.loc["beklenti11"]
    ax.plot(1 + 0.27, be.tahmin, marker="D", ms=5.2, mfc="white", mec=MUREKKEP, mew=0.9, zorder=2)
    ax.set_xticks([0, 1])
    ax.set_xticklabels(c_etiket)
    ax.set_xlim(-0.45, 1.45)
    ax.set_ylim(0, 18)
    ax.set_yticks([0, 5, 10, 15])
    ax.set_ylabel("Standardized prevalence (%)")
    ax.grid(axis="x", visible=False)
    D = d.loc["Delta"]
    R = d.loc["RERI"]
    ax.text(0.02, 0.98, f"Δ = {D.tahmin:.1f} pp ({D.alt:.1f}, {D.ust:.1f})\nRERI = {R.tahmin:.2f} ({R.alt:.2f}, {R.ust:.2f})".replace("-", "−"),
            transform=ax.transAxes, va="top", ha="left", fontsize=6.6, color=MUREKKEP)
    ax.set_title(baslik, loc="left", fontsize=7.4, fontweight="bold", pad=3)


KISA_DUY = {"00a_minimal": "Minimal", "00b_cekirdek": "Core (sociodemographic, no need indicator)", "00_ana": "Core + need (main)", "00b_tam": "Fully adjusted", "00c_esnek_tam": "Flexible, fully adjusted",
            "00d_esnek_cekirdek": "Flexible, core + need", "00e_lpm": "Linear probability model", "01_HE040": "Adding ability to make ends meet",
            "13_yoksunluk_kovaryatlari": "Adding deprivation indicators", "09_log_gelir_spline": "Log income spline", "11a_bolgesiz": "Without region",
            "11b_ibbs2_sabit_etki": "NUTS-2 region effects", "08_ibbs2_sozde_tabaka": "NUTS-2 pseudo-strata", "02_genis_sonuc": "Outcome: heavy or some burden",
            "15_tibbi_bilesen": "Component: medical care", "16_ilac_bilesen": "Component: medicine", "03_harcamasiz_dislandi": "Excluding no medical expenditure",
            "04b_kronik_oran": "Modifier: proportion chronic", "04c_gali": "Modifier: activity limitation", "04d_gali_kronik": "Modifier: activity limitation, adj. chronic", "05_yas17": "Chronic illness from age 17",
            "12_kronik_bilinmeyen_dislandi": "Excluding unknown chronic status", "14_ortusen_tabakalar": "Overlapping fine strata only",
            "10m_yetiskin_minimal": "Minimal", "10n_yetiskin_cekirdek": "Core (sociodemographic, no need indicator)", "10a_yetiskin_ana": "Core + need (main)", "10b_yetiskin_tam": "Fully adjusted", "10c_yetiskin_esnek": "Flexible, fully adjusted"}
ORMAN_GRUP = [("Households: adjustment ladder", ["00a_minimal", "00b_cekirdek", "00_ana", "00b_tam", "00c_esnek_tam"]),
              ("Households: model form", ["00d_esnek_cekirdek", "00e_lpm"]),
              ("Households: sensitivity (main model)", ["01_HE040", "13_yoksunluk_kovaryatlari", "09_log_gelir_spline", "11a_bolgesiz", "11b_ibbs2_sabit_etki", "08_ibbs2_sozde_tabaka",
                                                        "02_genis_sonuc", "15_tibbi_bilesen", "16_ilac_bilesen", "03_harcamasiz_dislandi", "04a_1v0", "04b_kronik_oran", "04c_gali", "04d_gali_kronik", "05_yas17",
                                                        "12_kronik_bilinmeyen_dislandi", "14_ortusen_tabakalar"]),
              ("Adults: adjustment ladder", ["10m_yetiskin_minimal", "10n_yetiskin_cekirdek", "10a_yetiskin_ana", "10b_yetiskin_tam", "10c_yetiskin_esnek"])]


def sinif_etiketi(ad, pay=None, sarma=26):
    import textwrap
    t = textwrap.fill(str(ad), sarma)
    return t if pay is None else f"{t}\n({pay:.1f}%)"


def figure3():
    a = pd.read_csv(VERI / "fig3a_lca_profil.csv")
    b = pd.read_csv(VERI / "fig3b_lca_gelir.csv")
    c = pd.read_csv(VERI / "fig3c_lca_bolge.csv")
    siniflar = a[["sinif", "sinif_ad", "sinif_payi"]].drop_duplicates().sort_values("sinif")
    K = len(siniflar)
    fig = plt.figure(figsize=(TAM, 16.0 * CM))
    paneller = [("he090", "Household food affordability"), ("et", "Meat, chicken or fish"), ("meyve", "Fruit"), ("sebze", "Vegetables or salad")]
    sol0, gen, bos = 0.175, 0.193, 0.012
    ramp4 = ["#E4EAF1", "#9CB0C8", "#486D97", "#244A7C"]
    ramp3 = ["#E4EAF1", "#9B7EC8", "#244A7C"]
    for pi, (g, ad) in enumerate(paneller):
        ax = fig.add_axes([sol0 + pi * (gen + bos), 0.57, gen, 0.345])
        d = a[a.gosterge == g]
        kodlar = sorted(d.kategori_kod.unique())
        if g == "he090":
            renkler = {0: RENK_A[0], 1: RENK_A[1]}
        elif len(kodlar) == 3:
            renkler = dict(zip(kodlar, ramp3))
        else:
            renkler = dict(zip(kodlar, ramp4))
        for yi, sn in enumerate(siniflar.sinif):
            sol = 0.0
            for kk in kodlar:
                v = float(d[(d.sinif == sn) & (d.kategori_kod == kk)].olasilik.iloc[0])
                ax.barh(yi, v, left=sol, height=0.62, color=renkler[kk], edgecolor="white", linewidth=0.8)
                if v >= 0.12:
                    koyu = renkler[kk] in ("#486D97", "#244A7C", RENK_A[0], RENK_A[1])
                    ax.text(sol + v / 2, yi, f"{v:.2f}".lstrip("0"), ha="center", va="center", fontsize=5.6, color="white" if koyu else MUREKKEP)
                sol += v
        ax.set_xlim(0, 1)
        ax.set_ylim(K - 0.5, -0.5)
        ax.set_yticks(range(K))
        ax.set_yticklabels([sinif_etiketi(r.sinif_ad, r.sinif_payi, 24) for r in siniflar.itertuples()] if pi == 0 else [], fontsize=6.3)
        if pi == 0:
            for t, sn in zip(ax.get_yticklabels(), siniflar.sinif):
                t.set_color(RENK_SINIF[(int(sn) - 1) % len(RENK_SINIF)])
        ax.set_xticks([0, 0.5, 1])
        etk = ax.set_xticklabels(["0", "0.5", "1"])
        etk[0].set_ha("left")
        etk[-1].set_ha("right")
        ax.tick_params(axis="y", length=0)
        ax.grid(False)
        ax.spines["left"].set_visible(False)
        ax.set_title(ad, fontsize=6.8, fontweight="bold", loc="left", pad=3)
        if g == "he090":
            tut = [plt.Rectangle((0, 0), 1, 1, color=renkler[kk]) for kk in kodlar]
            ax.legend(tut, ["Can afford", "Cannot afford"], fontsize=5.7, loc="upper left", bbox_to_anchor=(-0.02, -0.10), ncol=2, handlelength=0.8, handleheight=0.8, columnspacing=0.8, handletextpad=0.3, borderaxespad=0)
        if pi == 0:
            panel_etiketi_fig(fig, "a", 0.005, 0.985)
    ortak = [(ramp4[0], "<1/week or never"), (ramp4[1], "1\u20133/week"), (ramp4[2], "4\u20136/week"), (ramp3[1], "Weekly, not daily (meat)"), (ramp4[3], "Daily")]
    fig.legend([plt.Rectangle((0, 0), 1, 1, color=r_) for r_, _ in ortak], [e for _, e in ortak], loc="upper left", bbox_to_anchor=(sol0 + gen + bos - 0.004, 0.535), ncol=5, fontsize=5.7,
               handlelength=0.8, handleheight=0.8, columnspacing=1.0, handletextpad=0.3, borderaxespad=0, frameon=False)

    axb = fig.add_axes([0.075, 0.10, 0.36, 0.25])
    gruplar = ["Total", "Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)"]
    for gi, gr in enumerate(gruplar):
        alt_ = 0.0
        for sn in siniflar.sinif:
            r = b[(b.grup == gr) & (b.sinif == sn)]
            v = float(r.yuzde.iloc[0]) if len(r) else 0.0
            axb.bar(gi, v, bottom=alt_, width=0.68, color=RENK_SINIF[(int(sn) - 1) % len(RENK_SINIF)], edgecolor="white", linewidth=0.8)
            if v >= 7:
                axb.text(gi, alt_ + v / 2, f"{v:.0f}", ha="center", va="center", fontsize=5.8, color=MUREKKEP)
            alt_ += v
    axb.set_xticks(range(len(gruplar)))
    axb.set_xticklabels(["All", "Q1\n(lowest)", "Q2", "Q3", "Q4", "Q5\n(highest)"], fontsize=6.6)
    axb.set_ylim(0, 100)
    axb.set_ylabel("Adults (%)")
    axb.set_xlabel("Equivalized income quintile")
    axb.grid(axis="x", visible=False)
    panel_etiketi_fig(fig, "b", 0.005, 0.385)

    axc = fig.add_axes([0.62, 0.10, 0.365, 0.25])
    bolgeler = list(c.grup.drop_duplicates())
    for yi, gr in enumerate(bolgeler):
        sol = 0.0
        for sn in siniflar.sinif:
            r = c[(c.grup == gr) & (c.sinif == sn)]
            v = float(r.yuzde.iloc[0]) if len(r) else 0.0
            axc.barh(yi, v, left=sol, height=0.7, color=RENK_SINIF[(int(sn) - 1) % len(RENK_SINIF)], edgecolor="white", linewidth=0.6)
            sol += v
    axc.set_yticks(range(len(bolgeler)))
    axc.set_yticklabels(bolgeler, fontsize=6.2)
    axc.set_ylim(len(bolgeler) - 0.5, -0.5)
    axc.set_xlim(0, 100)
    axc.set_xlabel("Adults (%)")
    axc.grid(axis="y", visible=False)
    axc.tick_params(axis="y", length=0)
    panel_etiketi_fig(fig, "c", 0.475, 0.385)
    tut = [plt.Rectangle((0, 0), 1, 1, color=RENK_SINIF[(int(sn) - 1) % len(RENK_SINIF)]) for sn in siniflar.sinif]
    fig.legend(tut, [sinif_etiketi(f"{int(r.sinif)} {r.sinif_ad}", None, 24) for r in siniflar.itertuples()], loc="center", bbox_to_anchor=(0.55, 0.42), ncol=4, fontsize=6.0, handlelength=1.0, columnspacing=1.0)
    return kaydet(fig, "Figure3")


def figure4():
    ha = pd.read_csv(VERI / "fig4a_hucre_hane.csv")
    hy = pd.read_csv(VERI / "fig4b_hucre_yetiskin.csv")
    lc = pd.read_csv(VERI / "fig4c_lca_yuk.csv")
    fo = pd.read_csv(VERI / "fig4d_duyarlilik_orman.csv")
    fig = plt.figure(figsize=(TAM, 19.5 * CM))
    ax_a = fig.add_axes([0.095, 0.713, 0.33, 0.205])
    ax_b = fig.add_axes([0.095, 0.405, 0.33, 0.205])
    ax_c = fig.add_axes([0.235, 0.062, 0.19, 0.24])
    ax_d = fig.add_axes([0.715, 0.062, 0.27, 0.856])
    hucre_paneli(ax_a, ha, "Households", ["No adult with\nchronic illness", "Adult with\nchronic illness"])
    hucre_paneli(ax_b, hy, "Adults aged ≥18 years", ["No chronic\nillness", "Chronic\nillness"])
    tut = [Line2D([0], [0], color=RENK_A[k], marker="o", lw=1.4, ms=5, mec="white") for k in (0, 1)] + [Line2D([0], [0], color="none", marker="D", mfc="white", mec=MUREKKEP, ms=5)]
    fig.legend(tut, [ETIKET_A[0], ETIKET_A[1], r"Expected under additivity, $p_{10} + p_{01} - p_{00}$"], loc="upper left", bbox_to_anchor=(0.04, 0.995), ncol=3, fontsize=6.4, columnspacing=1.0, handlelength=1.4)
    panel_etiketi_fig(fig, "a", 0.005, 0.941)
    panel_etiketi_fig(fig, "b", 0.005, 0.633)
    panel_etiketi_fig(fig, "c", 0.005, 0.326)
    panel_etiketi_fig(fig, "d", 0.455, 0.941)
    sec = lc[(lc.sonuc == "Y_saglik_yuk") & (lc.olcu == "prevalence")]
    stil = [("adjusted", "o", "Covariate-adjusted"), ("adjusted + A", "D", "Also adjusted for affordability")]
    siniflar = sec[["sinif", "sinif_ad"]].drop_duplicates().sort_values("sinif")
    for (ay, mk, ad), of in zip(stil, (-0.14, 0.14)):
        x = sec[sec.ayar == ay].sort_values("sinif")
        for i, r in enumerate(x.itertuples()):
            renk = RENK_SINIF[(int(r.sinif) - 1) % len(RENK_SINIF)]
            ax_c.plot([r.alt, r.ust], [i + of, i + of], color=renk, lw=1.3)
            ax_c.plot(r.tahmin, i + of, mk, color=renk if mk == "o" else "white", mec=renk, mew=1.0, ms=4.6 if mk == "o" else 4.2)
    ax_c.set_yticks(range(len(siniflar)))
    ax_c.set_yticklabels([sinif_etiketi(f"{int(r.sinif)} {r.sinif_ad}", None, 22) for r in siniflar.itertuples()], fontsize=6.0)
    ax_c.set_ylim(len(siniflar) - 0.5, -0.5)
    ax_c.grid(axis="y", visible=False)
    ax_c.set_xlim(0, 16)
    ax_c.set_xlabel("Adults in a high-burden household (%)")
    ax_c.set_title("Latent class profiles", loc="left", fontsize=7.4, fontweight="bold", pad=3)
    tc = [Line2D([0], [0], color=SOLUK, marker="o", lw=1.2, ms=4.4), Line2D([0], [0], color=SOLUK, marker="D", lw=1.2, ms=4, mfc="white", mec=SOLUK)]
    ax_c.legend(tc, ["Adjusted", "+ affordability"], fontsize=5.4, loc="lower left", handlelength=1.2, borderaxespad=0.15, handletextpad=0.4)
    fo = fo[fo.olcu != "Delta_2v0"].copy()
    fo.loc[fo.olcu == "Delta_1v0", "analiz"] = "04a_1v0"
    ad = dict(KISA_DUY, **{"04a_1v0": "Modifier: 1 vs 0 chronic adults"})
    y = 0
    yt, ylab, yb = [], [], []
    for baslik, kodlar in ORMAN_GRUP:
        yb.append((y, baslik))
        y += 1
        for k in kodlar:
            r = fo[fo.analiz == k]
            if not len(r):
                continue
            r = r.iloc[0]
            ana = k in ("00_ana", "10a_yetiskin_ana")
            renk = MUREKKEP if ana else ("#553596" if k.startswith("10") else IKINCIL)
            ax_d.plot([r.alt, r.ust], [y, y], color=renk, lw=1.6 if ana else 1.0)
            ax_d.plot(r.tahmin, y, "D" if ana else "o", color=renk, ms=4.8 if ana else 3.4, mec="white", mew=0.5)
            yt.append(y)
            ylab.append(ad[k])
            y += 1
        y += 0.4
    ax_d.axvline(0, color=IKINCIL, lw=0.6)
    ax_d.axvline(fo.loc[fo.analiz == "00_ana", "tahmin"].iloc[0], color=SOLUK, lw=0.6, ls=(0, (2, 2)))
    ax_d.set_yticks(yt)
    ax_d.set_yticklabels(ylab, fontsize=6.0)
    for t in ax_d.get_yticklabels():
        if t.get_text() == "Core + need (main)":
            t.set_fontweight("bold")
    for yy, baslik in yb:
        ax_d.text(-0.02, yy, baslik, transform=ax_d.get_yaxis_transform(), ha="right", va="center", fontsize=6.2, fontweight="bold", color=MUREKKEP)
    ax_d.set_ylim(y - 0.4, -0.8)
    ax_d.grid(axis="y", visible=False)
    ax_d.set_xlabel("Additive interaction \u0394 (pp, 95% CI)")
    return kaydet(fig, "Figure4")


def figureS2():
    u = pd.read_csv(VERI / "figS2_lca_uyum.csv")
    fig, axs = plt.subplots(1, 3, figsize=(TAM, 6.6 * CM))
    fig.subplots_adjust(left=0.08, right=0.99, top=0.9, bottom=0.27, wspace=0.38)
    modeller = [("ortak", "Main model (joint affordability × meat item)", MUREKKEP, "-"), ("dort", "Four separate indicators (sensitivity)", SOLUK, (0, (3, 1.5)))]
    for m, ad, renk, ls in modeller:
        d = u[u.model == m].sort_values("K")
        axs[0].plot(d.K, d.BIC / 1000, color=renk, ls=ls, marker="o", ms=3.4, lw=1.3, label=ad)
        d2 = d[d.K > 1]
        axs[1].plot(d2.K, d2.rel_entropi, color=renk, ls=ls, marker="o", ms=3.4, lw=1.3)
        axs[2].plot(d2.K, d2.en_kucuk_sinif_yuzde, color=renk, ls=ls, marker="o", ms=3.4, lw=1.3)
    axs[0].set_ylabel("BIC (thousands)")
    axs[1].set_ylabel("Relative entropy")
    axs[1].axhline(0.6, color=SOLUK, lw=0.6, ls=(0, (2, 2)))
    axs[1].set_ylim(0.5, 0.9)
    axs[2].set_ylabel("Smallest class (%)")
    axs[2].axhline(5, color=SOLUK, lw=0.6, ls=(0, (2, 2)))
    for i, ax in enumerate(axs):
        ax.set_xlabel("Number of classes")
        ax.set_xticks(range(1, 7) if i == 0 else range(2, 7))
        panel_etiketi(ax, "abc"[i], x=-0.2, y=1.04)
    fig.legend(loc="lower center", ncol=2, fontsize=6.4, bbox_to_anchor=(0.5, 0.0))
    return kaydet(fig, "FigureS2", ek=True)


def figure5():
    oz = pd.read_csv(SIM / "simulasyon_ozet.csv")
    fig, axs = plt.subplots(2, 2, figsize=(TAM, 15.5 * CM))
    fig.subplots_adjust(left=0.085, right=0.985, top=0.965, bottom=0.17, hspace=0.75, wspace=0.28)
    h = oz[(oz.duzey == "hane") & (oz.yanlilik_turu == "yok")].sort_values("Delta_nominal")
    ax = axs[0, 0]
    stiller = {"lojistik_ana_etki": ("#1A1A1A", r"Δ test, main effects ($\mathrm{H}_1$ main)", "-", "o"), "lojistik_esnek": ("#6F6F6F", "Δ test, flexible", "-", "o"),
               "dogrusal_olasilik": ("#009E73", "LPM product term", "-", "o")}
    for t, (renk, ad, ls, mk) in stiller.items():
        d = h[h.tahminci == t]
        ax.plot(d.gercek_Delta, d.red_iki_yonlu, color=renk, lw=1.4, marker=mk, ms=3.5, mec="white", mew=0.5, ls=ls, label=ad)
    for t, renk, ad in [("lojistik_ana_etki", "#D55E00", "Logistic product term, main effects"), ("lojistik_esnek", "#E69F00", "Logistic product term, flexible")]:
        d = h[h.tahminci == t]
        ax.plot(d.gercek_Delta, d.red_carpim, color=renk, lw=1.2, marker="s", ms=3.2, mec="white", mew=0.5, ls=(0, (3, 1.5)), label=ad)
    ax.axhline(0.05, color=SOLUK, lw=0.7, ls=(0, (2, 2)))
    ax.axhline(0.80, color=SOLUK, lw=0.7, ls=(0, (2, 2)))
    ax.set_ylim(0, 1.02)
    ax.set_xlabel("True Δ (percentage points)")
    ax.set_ylabel("Rejection rate (two-sided 5%)")
    ax.legend(fontsize=5.7, loc="upper center", bbox_to_anchor=(0.5, -0.24), ncol=2, handlelength=2.0, columnspacing=1.0)
    panel_etiketi(ax, "a", x=-0.13)
    ax = axs[0, 1]
    hb = oz[(oz.duzey == "hane") & (oz.tahminci == "lojistik_ana_etki")]
    yr = {"yok": ("#1A1A1A", "No response bias"), "toplamsal": ("#0072B2", "Additive false positives, δ = 0.02"), "carpimsal": ("#D55E00", "Multiplicative, γ = 0.3")}
    for k, (renk, ad) in yr.items():
        d = hb[hb.yanlilik_turu == k].sort_values("Delta_nominal")
        ax.plot(d.gercek_Delta, d.yanlilik, color=renk, lw=1.4, marker="o", ms=3.5, mec="white", mew=0.5, label=ad)
    ax.axhline(0, color=IKINCIL, lw=0.6)
    ax.set_xlabel("True Δ (percentage points)")
    ax.set_ylabel("Bias of Δ estimate (pp)")
    ax.legend(fontsize=5.7, loc="upper center", bbox_to_anchor=(0.5, -0.24), ncol=2, columnspacing=1.0)
    panel_etiketi(ax, "b", x=-0.13)
    ax = axs[1, 0]
    for k, (renk, ad) in yr.items():
        d = hb[hb.yanlilik_turu == k].sort_values("Delta_nominal")
        ax.errorbar(d.gercek_Delta, d.kapsama, yerr=1.96 * d.kapsama_mcse, color=renk, lw=1.2, marker="o", ms=3.5, mec="white", mew=0.5, capsize=0, label=ad)
    bs = oz[(oz.duzey == "hane_bootstrap") & (oz.tahminci.str.contains("bootstrap"))]
    ax.errorbar(bs.gercek_Delta + 0.08, bs.kapsama, yerr=1.96 * bs.kapsama_mcse, fmt="s", color="#553596", ms=3.6, mec="white", mew=0.5, capsize=0, label="Percentile bootstrap (no bias)")
    ax.axhline(0.95, color=SOLUK, lw=0.7, ls=(0, (2, 2)))
    ax.set_ylim(0.3, 1.0)
    ax.set_xlabel("True Δ (percentage points)")
    ax.set_ylabel("Coverage of 95% CI")
    ax.legend(fontsize=5.7, loc="upper center", bbox_to_anchor=(0.5, -0.24), ncol=2, columnspacing=1.0)
    panel_etiketi(ax, "c", x=-0.13)
    ax = axs[1, 1]
    y = oz[oz.duzey == "yetiskin"].copy()
    gruplar = [(0.3, 0), (0.3, 2), (0.6, 0), (0.6, 2)]
    ax.axhline(1, color=IKINCIL, lw=0.7, zorder=1)
    seriler = [("kumeli_se", "#0072B2", "o", "Household-clustered SE"), ("kumesiz_se", "#D55E00", "s", "SE ignoring clustering")]
    noktalar = {}
    for ti, (t, renk, mk, ad) in enumerate(seriler):
        for gi, (rho, D) in enumerate(gruplar):
            r = y[(y.rho == rho) & (y.Delta_nominal == D) & (y.tahminci == t)]
            if len(r):
                noktalar[(t, gi)] = (gi + (ti - 0.5) * 0.42, r.se_orani.iloc[0], r.kapsama.iloc[0])
    for gi in range(len(gruplar)):
        if ("kumeli_se", gi) in noktalar and ("kumesiz_se", gi) in noktalar:
            a_, b_ = noktalar[("kumeli_se", gi)], noktalar[("kumesiz_se", gi)]
            ax.plot([a_[0], b_[0]], [a_[1], b_[1]], color=SOLUK, lw=0.8, zorder=2)
    for t, renk, mk, ad in seriler:
        p = [v for (tt, _), v in noktalar.items() if tt == t]
        ax.plot([v[0] for v in p], [v[1] for v in p], ls="none", marker=mk, ms=5.4, color=renk, mec="white", mew=0.7, label=ad, zorder=3)
        for x_, _, k_ in p:
            ax.text(x_, 1.043, f"{k_:.3f}", ha="center", va="bottom", fontsize=5.2, color=IKINCIL)
    ax.text(-0.46, 1.063, "95% CI coverage", ha="left", va="bottom", fontsize=5.6, style="italic", color=IKINCIL)
    ax.set_xticks(range(4))
    ax.set_xticklabels([f"ICC {r_}\nΔ = {D}" for r_, D in gruplar], fontsize=6.4)
    ax.set_xlim(-0.5, 3.5)
    ax.set_ylim(0.92, 1.08)
    ax.set_yticks([0.95, 1.00])
    ax.set_ylabel("Mean model SE / empirical SE")
    ax.grid(axis="x", visible=False)
    ax.legend(fontsize=5.7, loc="upper center", bbox_to_anchor=(0.5, -0.24), ncol=2, columnspacing=1.0)
    panel_etiketi(ax, "d", x=-0.13)
    return kaydet(fig, "Figure5")


def figureS1():
    d = pd.read_csv(VERI / "figS1_karsilanmamis_neden.csv")
    kat = ["Cost", "System (distance, waiting time)", "Personal (time, fear, wait-and-see, other)"]
    fig, axs = plt.subplots(1, 3, figsize=(TAM, 7.2 * CM), sharey=False)
    fig.subplots_adjust(left=0.07, right=0.99, top=0.78, bottom=0.16, wspace=0.32)
    for i, k in enumerate(kat):
        ax = axs[i]
        for a_ in (0, 1):
            x = d[(d.kategori == k) & (d.A == a_)].sort_values("yil")
            ax.fill_between(x.yil, x.alt, x.ust, color=RENK_A[a_], alpha=0.15, lw=0)
            ax.plot(x.yil, x.yuzde, color=RENK_A[a_], lw=1.4, marker="o", ms=3.5, mec="white", mew=0.5, label=ETIKET_A[a_])
        ax.set_xticks([2023, 2024, 2025])
        ax.set_ylim(0, None)
        ax.grid(axis="x", visible=False)
        ax.set_title(k.split(" (")[0] + ("" if "(" not in k else "\n(" + k.split(" (")[1]), loc="left", fontsize=7.0, fontweight="bold")
        if i == 0:
            ax.set_ylabel("Adults with a medical care need (%)")
            ax.legend(fontsize=6.4, loc="upper right")
        panel_etiketi(ax, "abc"[i], x=-0.12, y=1.18)
    return kaydet(fig, "FigureS1", ek=True)


def figureS3():
    a = pd.read_csv(VERI / "figS3a_eski_hipotez_hucre.csv").set_index("olcu")
    b = pd.read_csv(VERI / "figS3b_diyet_skor_egrisi.csv")
    fig, axs = plt.subplots(1, 2, figsize=(TAM, 7.0 * CM), gridspec_kw={"width_ratios": [0.9, 1.1]})
    fig.subplots_adjust(left=0.095, right=0.99, top=0.88, bottom=0.2, wspace=0.3)
    ax = axs[0]
    xs = {"p00": -0.13, "p10": 0.13, "p01": 0.87, "p11": 1.13}
    for k, x in xs.items():
        a_ = int(k[1])
        r = a.loc[k]
        if r.olay < 20:
            ax.text(x, 0.08, "<20\nevents", ha="center", va="bottom", fontsize=5.6, color=RENK_A[a_])
            continue
        ax.plot([x, x], [r.alt, r.ust], color=RENK_A[a_], lw=1.4)
        dolu = r.olay > 50
        ax.plot(x, r.tahmin, "o", color=RENK_A[a_], ms=5, mec="white" if dolu else RENK_A[a_], mew=0.6 if dolu else 1.1, mfc=RENK_A[a_] if dolu else "white", zorder=3)
    ax.set_xticks([0, 1])
    ax.set_xticklabels(["Daily fruit and\nvegetables", "Not both daily"])
    ax.set_xlim(-0.5, 1.5)
    ax.set_ylim(0, None)
    ax.set_ylabel("Unmet medical need\nbecause of cost (%)", linespacing=1.1)
    ax.grid(axis="x", visible=False)
    tut = [Line2D([0], [0], color=RENK_A[k], marker="o", lw=1.4, ms=5, mec="white") for k in (0, 1)] + [Line2D([0], [0], color=SOLUK, marker="o", lw=0, ms=5, mfc="white", mec=SOLUK, mew=1.1)]
    ax.legend(tut, [ETIKET_A[0], ETIKET_A[1], "20\u201350 events"], fontsize=6.2, loc="upper left")
    panel_etiketi(ax, "a", x=-0.13, y=1.04)
    ax = axs[1]
    for a_ in (0, 1):
        x = b[b.A == a_].sort_values("diyet_skor")
        ax.fill_between(x.diyet_skor, x.alt, x.ust, color=RENK_A[a_], alpha=0.15, lw=0)
        ax.plot(x.diyet_skor, x.tahmin, color=RENK_A[a_], lw=1.5, label=ETIKET_A[a_])
    ax.set_xlabel("Fruit + vegetable frequency score (0–10)")
    ax.set_ylabel("Unmet medical need\nbecause of cost (%)", linespacing=1.1)
    ax.set_xticks(range(0, 11, 2))
    ax.set_ylim(0, None)
    ax.legend(fontsize=6.4, loc="upper right")
    panel_etiketi(ax, "b", x=-0.14, y=1.04)
    return kaydet(fig, "FigureS3", ek=True)


def figureS4():
    m = pd.read_csv(VERI / "figS4_modul_bosluk.csv")
    ad = {"MODUL1_F_1": "Dentist (MODUL1_F_1)", "MODUL1_F_2": "General practitioner (MODUL1_F_2)", "MODUL1_F_3": "Specialist (MODUL1_F_3)"}
    ih = {1: "Need, unmet\nat least once", 2: "Need, always\nmet", 3: "No need"}
    durum = ["One or more visits", "Never (code 6)", "Blank"]
    renk = {"One or more visits": "#486D97", "Never (code 6)": "#9CB0C8", "Blank": "#D55E00"}
    fig, axs = plt.subplots(1, 3, figsize=(TAM, 6.6 * CM), sharex=True)
    fig.subplots_adjust(left=0.155, right=0.985, top=0.86, bottom=0.27, wspace=0.16)
    for i, (v, baslik) in enumerate(ad.items()):
        ax = axs[i]
        x = m[m.degisken == v]
        for yi, kod in enumerate([1, 2, 3]):
            sol = 0
            toplam = x[x.ihtiyac == kod].n.sum()
            for dd in durum:
                n = x[(x.ihtiyac == kod) & (x.durum == dd)].n.sum()
                ax.barh(yi, 100 * n / toplam, left=sol, color=renk[dd], height=0.6, edgecolor="white", linewidth=0.7, label=dd if (i == 0 and yi == 0) else None)
                sol += 100 * n / toplam
            ax.text(101, yi, f"n = {toplam:,}", va="center", fontsize=5.6, color=IKINCIL)
        ax.set_yticks(range(3))
        ax.set_yticklabels([ih[k] for k in [1, 2, 3]] if i == 0 else [], fontsize=6.4)
        ax.set_ylim(2.5, -0.5)
        ax.set_xlim(0, 128)
        ax.set_xticks([0, 25, 50, 75, 100])
        ax.grid(axis="y", visible=False)
        ax.set_title(baslik, loc="left", fontsize=6.8, fontweight="bold")
        ax.set_xlabel("Adults aged \u226515 (%)")
        panel_etiketi(ax, "abc"[i], x=-0.03, y=1.06)
    fig.legend(loc="lower center", ncol=3, fontsize=6.4, bbox_to_anchor=(0.57, 0.0))
    return kaydet(fig, "FigureS4", ek=True)


def figureS5():
    k = pd.read_csv(VERI / "figS5_kalibrasyon.csv")
    sec = {"kalibrasyon_h1_hane.csv": "Burden, households", "kalibrasyon_h1_yetiskin.csv": "Burden, adults",
           "kalibrasyon_h2_et_haftada_az_nihai_esnek.csv": "Meat < weekly", "kalibrasyon_h2_meyve_gunluk_degil_nihai_esnek.csv": "Fruit < daily",
           "kalibrasyon_h2_sebze_gunluk_degil_nihai_esnek.csv": "Vegetables < daily"}
    fig, axs = plt.subplots(1, 5, figsize=(TAM, 4.6 * CM))
    fig.subplots_adjust(left=0.06, right=0.99, top=0.82, bottom=0.24, wspace=0.42)
    for i, (f, ad) in enumerate(sec.items()):
        ax = axs[i]
        d = k[k.dosya == f]
        ust = max(d.gozlenen.max(), d.tahmin.max()) * 1.08
        ax.plot([0, ust], [0, ust], color=SOLUK, lw=0.7, ls=(0, (2, 2)))
        ax.plot(100 * d.tahmin, 100 * d.gozlenen, "o", color="#0072B2", ms=3.2, mec="white", mew=0.4)
        ax.set_xlim(0, 100 * ust)
        ax.set_ylim(0, 100 * ust)
        ax.set_title(ad, fontsize=6.4, loc="left")
        ax.tick_params(labelsize=5.8)
        ax.set_xlabel("Predicted (%)", fontsize=6.2)
        if i == 0:
            ax.set_ylabel("Observed (%)", fontsize=6.2)
        panel_etiketi(ax, "abcde"[i], x=-0.1, y=1.12)
    return kaydet(fig, "FigureS5", ek=True)


if __name__ == "__main__":
    istek = sys.argv[1:] or ["1", "2", "3", "4", "5", "S1", "S2", "S3", "S4", "S5"]
    for k in istek:
        out = globals()[f"figure{k}"]()
        print("yazildi:", [str(p.name) for p in out])
