# Brain Network Visualization Toolkit 

This repository contains three complementary scripts for generating comprehensive visualizations of brain connectivity alterations in substance use disorders, particularly focused on cocaine use disorder. Together, these scripts can reproduce figures similar to those in Ricard et al. (2024) "A shared spatial topography links the functional connectome correlates of cocaine use disorder and dopamine D2/3 receptor densities" (Communications Biology).

## Overview

1. **makeNetworkMatrix3.R**: Generates network connectivity matrices (heatmaps) showing the patterns of connectivity between different brain networks
2. **PlotsubCortical.ipynb**: Creates 3D visualizations of subcortical structures with degree values as color mapping
3. **Surfplot.R**: Renders cortical surface visualizations and combines them with subcortical renderings

## Execution Order

For optimal results, follow this execution sequence:

1. First run **makeNetworkMatrix3.R** to generate network connectivity heatmaps
   - This produces visualizations similar to Fig 1C & 1E from Ricard et al. (2024)
   - The heatmaps show network-level connectivity patterns (both hyper- and hypo-connectivity)
   - Upper triangles typically represent decreased connectivity while lower triangles show increased connectivity
2. Then run **PlotsubCortical.ipynb** to generate subcortical visualization
   - This creates 3D renders of subcortical nuclei showing degree values
   - The output corresponds to the subcortical components of Fig 1B & 1D from the paper
   - Different viewing angles (xy, yz, xz) are generated to provide comprehensive visualization
3. Finally run **Surfplot.R** to generate cortical visualizations and combine all components
   - This script creates surface-based brain visualizations of cortical connectivity alterations
   - It then combines the cortical surfaces with the subcortical visualizations
   - The output integrates all components into a comprehensive figure similar to Fig 1 from the paper

## Key Features

- **Comprehensive visualization**: Covers both cortical and subcortical regions
- **Multi-scale approach**: Shows connectivity patterns at both the network level (matrices) and regional level (brain surfaces)
- **Color-coded visualization**: Uses standardized color schemes (blue for decreased connectivity, red-yellow for increased connectivity)
- **Integrated output**: Combines multiple visualization components into publication-ready figures

## Requirements

- R with packages: ggseg, ggsegSchaefer, ggseg3d, ggplot2, tidyverse, reshape2, scales, gtable, patchwork, magick
- Python with packages: numpy, nibabel, pyvista, matplotlib

## References

Ricard, J. A., Labache, L., Segal, A., Dhamala, E., Cocuzza, C. V., Jones, G., Yip, S. W., Chopra, S., & Holmes, A. J. (2024). A shared spatial topography links the functional connectome correlates of cocaine use disorder and dopamine D2/3 receptor densities. Communications Biology, 7(1), 1178. https://doi.org/10.1038/s42003-024-06836-9
