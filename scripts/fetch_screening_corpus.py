#!/usr/bin/env python3
"""Fetch the screening corpus from the LivingEvidenceMap repository."""
from pathlib import Path
from urllib.request import Request, urlopen

SRC = "https://raw.githubusercontent.com/thesalmonandthetomato/LivingEvidenceMap/main/data/reference/INCLUDES%20fixed%20abstracts.txt"
DEST = Path("data_raw/INCLUDES fixed abstracts.txt")
DEST.parent.mkdir(parents=True, exist_ok=True)
req = Request(SRC, headers={"User-Agent": "salmonscopingreview-screening"})
with urlopen(req, timeout=300) as r, DEST.open("wb") as f:
    while True:
        chunk = r.read(1024 * 1024)
        if not chunk:
            break
        f.write(chunk)
print(f"Fetched {DEST} ({DEST.stat().st_size} bytes)")
