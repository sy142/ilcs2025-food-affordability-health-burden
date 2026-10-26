from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import font_manager

KOK = Path(__file__).resolve().parents[1]
VERI = KOK / "R_Data ve Python Data" / "figur_verileri"
SIM = KOK / "R_Data ve Python Data" / "simulasyon"
SONUC = KOK / "Results"
FIG = KOK / "Figures"
FIG_EK = FIG / "Supplementary"

CM = 1 / 2.54
TAM = 17.4 * CM
TEK = 8.5 * CM

MUREKKEP = "#1A1A1A"
IKINCIL = "#555555"
SOLUK = "#8A8A8A"
IZGARA = "#E6E6E6"
ZEMIN = "#FFFFFF"

RENK_A = {0: "#0072B2", 1: "#D55E00"}
ETIKET_A = {0: "Can afford", 1: "Cannot afford"}
RENK_GIDA = {"et_haftada_az": "#882255", "meyve_gunluk_degil": "#C98500", "sebze_gunluk_degil": "#009E73"}
ETIKET_GIDA = {
    "et_haftada_az": "Meat, chicken or fish\nless than weekly",
    "meyve_gunluk_degil": "Fruit\nless than daily",
    "sebze_gunluk_degil": "Vegetables or salad\nless than daily",
}
ETIKET_GIDA_KISA = {"et_haftada_az": "Meat, chicken, fish", "meyve_gunluk_degil": "Fruit", "sebze_gunluk_degil": "Vegetables"}
RENK_GELIR = ["#B9A1DB", "#9876C8", "#7652B2", "#553596", "#381B6A"]
ETIKET_GELIR = ["Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)"]
RENK_SINIF = ["#009E73", "#56B4E9", "#E69F00", "#CC79A7", "#6F6F6F", "#882255"]


def yazi_tipi():
    mevcut = {f.name for f in font_manager.fontManager.ttflist}
    for ad in ("Avenir Next", "Helvetica Neue", "Arial"):
        if ad in mevcut:
            return ad
    return "DejaVu Sans"


def stil_kur():
    ad = yazi_tipi()
    mpl.rcParams.update({
        "font.family": ad,
        "mathtext.fontset": "custom",
        "mathtext.rm": ad,
        "mathtext.sf": ad,
        "mathtext.cal": ad,
        "mathtext.it": f"{ad}:italic",
        "mathtext.bf": f"{ad}:bold",
        "font.size": 8,
        "axes.titlesize": 8.5,
        "axes.labelsize": 8,
        "xtick.labelsize": 7.5,
        "ytick.labelsize": 7.5,
        "legend.fontsize": 7.5,
        "axes.edgecolor": IKINCIL,
        "axes.labelcolor": MUREKKEP,
        "axes.linewidth": 0.6,
        "axes.spines.top": False,
        "axes.spines.right": False,
        "axes.grid": True,
        "axes.axisbelow": True,
        "grid.color": IZGARA,
        "grid.linewidth": 0.5,
        "xtick.color": IKINCIL,
        "ytick.color": IKINCIL,
        "xtick.major.width": 0.6,
        "ytick.major.width": 0.6,
        "xtick.major.size": 2.5,
        "ytick.major.size": 2.5,
        "text.color": MUREKKEP,
        "legend.frameon": False,
        "legend.handlelength": 1.4,
        "figure.facecolor": ZEMIN,
        "axes.facecolor": ZEMIN,
        "savefig.facecolor": ZEMIN,
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
        "lines.solid_capstyle": "round",
    })


def panel_etiketi(ax, harf, x=-0.02, y=1.02):
    ax.text(x, y, f"({harf})", transform=ax.transAxes, fontsize=11, fontweight="bold", ha="right", va="bottom", color=MUREKKEP)


def panel_etiketi_fig(fig, harf, x, y):
    fig.text(x, y, f"({harf})", fontsize=11, fontweight="bold", ha="left", va="top", color=MUREKKEP)


def kaydet(fig, ad, ek=False):
    klasor = FIG_EK if ek else FIG
    klasor.mkdir(parents=True, exist_ok=True)
    fig.savefig(klasor / f"{ad}.png", dpi=600)
    fig.savefig(klasor / f"{ad}.pdf")
    fig.savefig(klasor / f"{ad}.tiff", dpi=600, pil_kwargs={"compression": "tiff_lzw"})
    plt.close(fig)
    return [klasor / f"{ad}.{u}" for u in ("png", "pdf", "tiff")]
