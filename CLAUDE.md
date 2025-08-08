# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is the `gfsynopsis` R package repository that generates reproducible data synopsis reports for over 100 species of British Columbia groundfish. The repository creates technical reports with standardized visualizations of fisheries data including catch statistics, survey maps, length/age compositions, and abundance indices.

## Key Commands

### Building Reports
- `Rscript report/make.R` - Generate all synopsis figures (must be run before report rendering)
- `./report/build_figures.sh` - Parallel processing script that generates figures for all 116+ species using GNU parallel
- `csasdown::render()` - Render bookdown reports (run from within specific report directories)

### Main Report Directories
- `report/tech-report-main/` - Main English technical report
- `report/tech-report-french-main/` - French technical report
- `report/tech-report-haida/` - Haida Gwaii regional report

### Data Processing
- `report/R/03-load-data.R` - Load cached fisheries data 
- `report/R/04-survey-index-standardization.R` - Fit survey abundance index models
- `report/R/05-cpue-index-standardization.R` - Fit commercial CPUE index models
- `report/R/06-build-figure-pages.R` - Generate species-specific figure pages

## Architecture

### Core Package Structure
The repository follows standard R package conventions:
- `R/` - Core package functions (species page generation, data processing utilities)
- `DESCRIPTION` - Package dependencies and metadata
- `inst/` - Installed package files and example data
- `man/` - Documentation files

### Report Generation Pipeline
1. **Data Loading**: Cached data objects stored in `report/data-cache-*/` directories
2. **Model Fitting**: Survey indices and CPUE models fitted using sdmTMB and other statistical packages
3. **Figure Generation**: Species-specific 2-page figure sets created via `make_pages()` function
4. **Report Compilation**: Bookdown reports compiled from R Markdown files

### Key Dependencies
- `gfplot`, `gfdata`, `gfiphc` - Core PBS assessment plotting and data packages
- `sdmTMB` - Spatiotemporal modeling for survey indices and CPUE standardization  
- `csasdown` - CSAS report templating system
- `bookdown` - Multi-format report generation

### Data Architecture
- Species-specific cached data in RDS format organized by common name with hyphens
- Separate cache folders for different report variants (main, haida)
- Raw CPUE cache, survey index cache, and map cache directories
- Figure pages stored as PNG files in `figure-pages/` directories

### Settings and Configuration
- `report/R/01-settings.R` - Main configuration file (cache locations, processing options, languages)
- `_bookdown.yml` files define report structure and file order
- French language support controlled via `french` parameter throughout pipeline

The codebase emphasizes reproducibility, parallel processing capabilities, and standardized formatting for scientific publication through the Canadian Science Advisory Secretariat (CSAS) system.