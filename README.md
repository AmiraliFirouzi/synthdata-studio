# SynthData Studio

**Generate synthetic data that preserves real statistical relationships**

[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Built with Shiny](https://img.shields.io/badge/Built%20with-Shiny-447099)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](#contributing)

**SynthData Studio** is a self-contained studio for generating synthetic data: upload your dataset and get a statistically faithful synthetic version of it — distributions, correlations and category structure preserved. No third-party services or APIs are involved, and your data is processed only within the app's own session. You can run this repository locally or use the hosted version.

## 🌐 Live demo

Try it right now — no installation needed:

**🔗 [Run SynthData Studio online](https://01a144fc-b57d-a04c-a5fd-940bb4e2d7be.share.connect.posit.cloud/)**

---

## ✨ Features

- **Three engines with different speed / fidelity trade-offs**
  - 🚀 **Turbo** — whole-row resampling with small perturbations; output in seconds
  - ⚖️ **Balanced** — column-by-column modeling with the strongest predictors (recommended)
  - 💎 **High fidelity** — larger fit sample and all predictors for maximum fidelity
- **Automatic data cleaning** on upload (type conversion, date detection, dropping unusable columns, full change report)
- **Quality validation** after generation: overall score, per-column fidelity and a correlation matrix
- **Compare charts** of the original vs. synthetic distribution for every variable
- **Background execution** with a live progress bar and a cancel button
- Semantic value constraints respected (percentages stay within 0–100, non-negative values stay non-negative, etc.)
- Export to **CSV / Excel / JSON**
- Modern dark UI, reproducible runs via seed

---

## 🚀 Install and run

Requires R 4.1 or newer (tested with R 4.5).

Install the required packages once:

```r
install.packages(c("shiny", "shinyjs", "synthpop", "callr",
                   "readxl", "writexl", "jsonlite"))
```

Then run the app:

```r
shiny::runApp("app.R")
```

> **Note:** `shiny` and `shinyjs` are required. The model-based engines need `synthpop` and background execution needs `callr`; if these are missing the app warns you and the Turbo engine still works without them. `readxl` is needed for Excel input, `writexl` for Excel output and `jsonlite` for JSON export.

---

## 📖 Usage

1. Drag & drop your dataset (CSV / TSV / Excel) — or load the built-in demo dataset
2. Pick the engine, number of rows, method and seed
3. Click **Generate synthetic data**
4. Inspect the output in the **Preview / Compare / Quality** tabs
5. Download the result (CSV / Excel / JSON)

---

## 🏗️ Technical overview

- Single-file, built on R + Shiny, and works without any external services
- The model-based engine is powered by **synthpop** and runs inside a **separate process (callr)** so the UI stays responsive
- If modeling is rejected, a ladder of fallbacks is tried automatically (method switching, imputation, CART)
- If a run exceeds its time budget, Turbo output is served instead so you never leave empty-handed

---

## ⚠️ Project status

This project is under active development and **may have bugs** — I will keep fixing them in future releases. If you run into a problem, please report it.

## 🤝 Contributing

Contributions are warmly welcome! 🙌

- Found a bug? Open an **Issue**
- Have an idea or an improvement? Send a **Pull Request**
- Suggestions and feedback are always welcome

---

## 📄 License

Released under the **MIT** license. See [LICENSE](LICENSE).
