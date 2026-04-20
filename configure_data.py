"""
configure_data.py — link project data folders to _ddbb/

Usage:
    python configure_data.py                          # dev mode, auto-detect Dropbox
    python configure_data.py --mode dev               # same
    python configure_data.py --mode dev --dropbox /path/to/Dropbox
    python configure_data.py --mode public            # print download instructions
"""

import argparse
import os
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Data sources: (symlink_name_in_data/A_raw, relative path inside _ddbb/)
# ---------------------------------------------------------------------------
SOURCES = [
    ("MINEDUC",                   "raw/MINEDUC"),
    ("SUPERINTENDENCIA_EDUCACION","raw/SUPERINTENDENCIA_EDUCACION"),
    ("DOH",                       "raw/DOH"),
    ("CR2",                       "raw/CR2"),
    ("FAO",                       "raw/FAO"),
    ("DEIS",                      "raw/DEIS"),
    ("BCN",                       "raw/BCN"),
    ("CENSO",                     "raw/CENSO"),
    ("apr_ddbb",                  "built/apr_ddbb"),
]

PUBLIC_INSTRUCTIONS = {
    "MINEDUC":                    "Request via https://datosabiertos.mineduc.cl — restricted access.",
    "SUPERINTENDENCIA_EDUCACION": "Download from https://www.supereduc.cl/datos-abiertos/",
    "DOH":                        "Request from https://www.doh.gob.cl/ — some files public.",
    "CR2":                        "Download from https://www.cr2.cl/datos-productos-cr2/",
    "FAO":                        "Download from https://www.fao.org/giews/earthobservation/",
    "DEIS":                       "Request via https://deis.minsal.cl/ — restricted access.",
    "BCN":                        "Download from https://www.bcn.cl/siit/mapas_vectoriales/",
    "CENSO":                      "Download from https://www.ine.gob.cl/estadisticas/sociales/censos-de-poblacion-y-vivienda",
    "apr_ddbb":                   "Run the apr_ddbb pipeline to generate this folder.",
}

# ---------------------------------------------------------------------------

def find_dropbox() -> Path:
    candidates = [
        Path.home() / "Dropbox",
        Path.home() / "Library/CloudStorage/Dropbox",
    ]
    for p in candidates:
        if p.exists():
            return p
    sys.exit("ERROR: Dropbox not found. Use --dropbox to specify its location.")


def dev_mode(dropbox: Path) -> None:
    ddbb = dropbox / "_ddbb"
    if not ddbb.exists():
        sys.exit(f"ERROR: {ddbb} does not exist. Set up _ddbb/ first.")

    raw_dir = Path(__file__).parent / "data" / "A_raw"
    raw_dir.mkdir(parents=True, exist_ok=True)

    for name, rel in SOURCES:
        src = ddbb / rel
        dst = raw_dir / name
        if dst.is_symlink():
            dst.unlink()
        if not src.exists():
            print(f"  WARNING: {src} does not exist yet — skipping {name}")
            continue
        dst.symlink_to(src)
        print(f"  linked: data/A_raw/{name} -> {src}")

    print("\nDone.")


def public_mode() -> None:
    print("Download instructions for replication:\n")
    for name, _ in SOURCES:
        print(f"  {name}:")
        print(f"    {PUBLIC_INSTRUCTIONS.get(name, 'See project documentation.')}\n")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=["dev", "public"], default="dev")
    parser.add_argument("--dropbox", type=Path, default=None)
    args = parser.parse_args()

    if args.mode == "public":
        public_mode()
    else:
        dropbox = args.dropbox or find_dropbox()
        dev_mode(dropbox)


if __name__ == "__main__":
    main()
