# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains the code and data for analyzing social network changes in bumblebee colonies following queen loss. The study investigates how the removal of the queen from a bumblebee colony affects social interactions and network dynamics among workers.

## Development Environment & Commands

### Environment Setup
- Python version: 3.10 (specified in mypy.ini)
- Uses tox for testing and quality control
- No package.json, requirements.txt, or Makefile present - dependencies are managed via tox

### Quality Control Commands
```bash
# Run pre-commit hooks on all files
tox -e pre-commit

# Run type checking with mypy
tox -e mypy

# Run all quality checks
tox
```

### Data Processing Pipeline
The data processing scripts are located in `scripts/data_processing/` and should be run in sequence:

```bash
# Basic network parameter calculation (run on interaction files)
python scripts/data_processing/network_params.py <input_file> <output_file> <interaction_type> <use_bouts>

# Generate GML files for network visualization
python scripts/data_processing/gml.py

# Calculate directional parameters
python scripts/data_processing/directionalparams.py

# Batch processing scripts are available with batch_*.sh files
```

## Architecture & Structure

### Core Components

1. **Data Processing Pipeline** (`scripts/data_processing/`)
   - `network_params.py`: Core network analysis including transitivity, clustering, modularity, and efficiency calculations
   - `gml.py`: Converts data to GML format for network visualization
   - `directionalparams.py`: Calculates directional network parameters
   - `assort_and_indiv_variance.py`: Analyzes assortativity and individual variance
   - Batch scripts (`batch_*.sh`) for processing multiple files

2. **Manuscript Generation** (`scripts/manuscript/`)
   - Figure generation scripts (`figure_*.R`, `figure_5_hairballs.py`)
   - Statistical analysis scripts (`lmms_*.R`, `stats_*.R`)
   - Data loading and preprocessing (`load_data.R`, `combine_behavioral_data_with_network_data.py`)
   - Supplementary analysis scripts in `si/` subdirectory

3. **Utilities** (`scripts/utils/`)
   - `base_utils.py`: Core Python utilities for processing tag data and colony groups
   - `base_utils.R`: R utilities for data processing
   - `nx2gt.py`: NetworkX to graph-tool conversion utilities

4. **Main Analysis Notebooks**
   - `RuttenbergEtAl2024.Rmd`: Main manuscript analysis
   - `RuttenbergEtAlSupp.Rmd`: Supplementary analysis

### Data Structure
- `data/`: Processed CSV files including network parameters, wing morphometrics, and experimental metadata
- `raw_data/`: Raw tracking data files with various interaction metrics (Assort, Cent, Deg, NWP, etc.)
- `figures/`: Generated manuscript figures

### Key Analysis Concepts
- **Queenright vs Queenless**: The study compares colonies with queens present vs removed
- **Network Metrics**: Uses transitivity, clustering, modularity, efficiency, and degree-based measures
- **Interaction Types**: Analyzes different behavioral interactions (head-head, head-body, etc.)
- **Bout Analysis**: Groups interactions into behavioral bouts using frame buffer thresholds

### Dependencies
The codebase uses:
- **Python**: NetworkX, pandas, numpy, matplotlib for network analysis and visualization
- **R**: Various packages for statistical modeling, visualization, and R Markdown rendering
- **Mixed Language**: Python for network processing, R for statistical analysis and visualization

### Important Notes
- The repository contains extensive raw data files (>40k characters in directory listing)
- Uses ArUco tag tracking system for individual bee identification
- Network analysis focuses on weighted, directed graphs with reciprocal edge weights
- Implements custom efficiency calculations for weighted networks