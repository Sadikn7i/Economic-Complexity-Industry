# Economic Complexity Index (ECI) Analysis in R

This repository provides a complete, stepwise pipeline for analyzing regional economic complexity using the Method of Reflections. All stages are documented below for clarity and reproducibility.

## Data Requirements

Prepare an Excel (or CSV) file with at least three columns: `province` (region name or code), `sector2_num` (sector/product code), and `log_exports` (logged export value).

---

## Stage 1: Basic Exploration

The workflow begins by loading your data and checking its structure. It will display dataset dimensions, variable names, check the unique provinces and sectors, summarize the log_exports range, and identify missing values. This helps ensure the data is loaded and formatted correctly before any analysis.

---

## Stage 2: Export Matrix Creation

Your data is restructured into a province × sector matrix where each cell contains the total log_export value for that province-sector combination. This matrix forms the foundation for complexity calculations.

---

## Stage 3: RCA Calculation

The Revealed Comparative Advantage (RCA) is computed for each province-sector. RCA captures whether a province is competitive in a sector—RCA ≥ 1 means competitive, RCA < 1 not competitive.

---

## Stage 4: Binary Matrix for Competitive Sectors

A binary matrix is created where each cell is 1 if the province has RCA ≥ 1 for a sector, and 0 otherwise. At this stage, the diversity (number of competitive sectors per province) and ubiquity (number of provinces per sector) are also calculated.

---

## Stage 5: ECI and PCI Computation

The Method of Reflections is applied using iterative calculations (typically 20 iterations) to obtain the Economic Complexity Index (ECI) for provinces and Product Complexity Index (PCI) for sectors. These indices allow for ranking provinces by their capabilities and sectors by their complexity.

---

## Stage 6: Product Space Density

A proximity matrix is constructed, showing how closely related different products are in terms of province participation. For each province, the density of opportunities in the product space is assessed—how “close” it is to moving into new, more complex sectors.

---

## Stage 7: Complexity Outlook (COI & COG)

The workflow calculates the Complexity Outlook Index (COI) and Complexity Outlook Gain (COG) for each province. COI measures the growth potential based on complex sectors not yet produced by the province; COG adjusts for the province’s current diversity.

---

## Stage 8: Automated Output

All results—tables, rankings, and high-quality PNG visualizations—are saved to your specified output directory. Example code to create your folder (edit as needed):

---

## References

This analysis is based on the original economic complexity methodology by Hausmann & Hidalgo (PNAS 2009; MIT Press 2014). For more theoretical and technical details, see those foundational works.

**Last Updated:** November 2025
---

## Contact

- 📧 Email: [spiritx98@gmail.com](mailto:spiritx98@gmail.com)
- 🐦 Twitter/X: [@sadikadendirir](https://x.com/sadikadendirir)
- 📷 Instagram: [@sadiq_n7i](https://www.instagram.com/sadiq_n7i/)
- 💻 GitHub: [Sadikn7i](https://github.com/Sadikn7i)
