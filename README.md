# ParetoPick-R: Multi-Objective Optimization Analysis Tool

## Overview

ParetoPick-R is a comprehensive Shiny web application designed for analyzing Pareto optimization results through advanced clustering and visualization techniques. The application provides tools for exploring multi-objective optimization solutions, performing correlation analysis, applying Principal Component Analysis (PCA), and implementing clustering algorithms to identify representative solutions from Pareto fronts.

## Features

- **Pareto Front Visualization**: Interactive exploration of multi-objective optimization results
- **Correlation Analysis**: Identification and management of highly correlated variables
- **Principal Component Analysis (PCA)**: Dimensionality reduction for clustering preparation
- **Clustering Methods**: K-means and K-medoids clustering algorithms
- **Analytical Hierarchy Process (AHP)**: Alternative prioritization method for Pareto solutions
- **Spatial Analysis**: Geographic visualization of measure implementations
- **Data Export**: Multiple export formats for results and visualizations

## Application Structure

### Main Tabs

1. **Introduction**: Overview of the application and methodology
2. **Data Preparation**: File upload and data preprocessing
3. **Visualising the Pareto Front**: Interactive exploration of optimization results
4. **Configure Clustering**: Setup clustering parameters with default or manual options
5. **Correlation Analysis**: Variable correlation assessment and selection
6. **PCA & K-means/K-medoids**: Dimensionality reduction and clustering execution
7. **Cluster Analysis**: Analysis and visualization of clustering results
8. **AHP (Analytical Hierarchy Process)**: Alternative prioritization methodology

## File Structure

```
/
├── app/
│   ├── ui.R              # Shiny user interface
│   ├── server.R          # Shiny server logic
│   ├── global.R          # Global variables and package loading
│   └── functions.R       # Custom helper functions
├── data/                 # Input data directory
├── input/               # Processed input files
├── output/              # Generated output files
├── support/             # R script alternatives to Python
└── python_files/        # Python executables (optional)
```

## Dependencies

### R Packages

The application automatically installs required packages through `global.R`:

**Core Packages:**
- `shiny`, `shinydashboard`, `shinyjs`, `shinyWidgets`
- `DT`, `plotly`, `ggplot2`, `corrplot`

**Data Processing:**
- `dplyr`, `tidyverse`, `reshape2`
- `cluster`, `config`, `configr`

**Spatial Analysis:**
- `sf`, `sp`, `spdep`, `geosphere`
- `leaflet`, `mapview`, `tmap`

**Visualization:**
- `RColorBrewer`, `viridis`, `gridExtra`
- `ggtext`, `scales`

### System Requirements

- R (version 4.0 or higher recommended)
- Python (optional, R alternatives available)
- Sufficient memory for large datasets
- Web browser for Shiny interface

## Required Input Files

### Essential Files (Minimum for Pareto Front Analysis)

1. **`pareto_fitness.txt`**: Pareto optimization results
2. **Objective names**: Defined through the Data Preparation interface

### Additional Files (For Full Clustering Analysis)

1. **`pareto_genomes.txt`**: Genome/solution representations
2. **`hru.con`**: Hydrological Response Unit connections
3. **`measure_location.csv`**: Spatial measure implementation data
4. **Shapefile components**:
   - `hru.shp`, `hru.shx`, `hru.dbf`, `hru.prj`
5. **`rout_unit.con`**: Routing unit connections
6. **`sq_fitness.txt`**: Status quo fitness values (optional)

### Input File Formats

**pareto_fitness.txt**: Tab-separated values with objectives as columns
```
obj1    obj2    obj3    obj4
1.23    4.56    7.89    0.12
2.34    5.67    8.90    0.23
...
```

**pareto_genomes.txt**: Solution encoding (implementation-specific format)

**measure_location.csv**: Measure-HRU mapping
```
name,nswrm,obj_id
measure1,type1,"hru1,hru2,hru3"
measure2,type2,"hru4,hru5"
...
```

## Installation and Setup

### 1. Clone Repository
```bash
git clone [repository-url]
cd ParetoPick-R
```

### 2. Install Dependencies
Dependencies are automatically installed when running the application. Alternatively, install manually:

```r
install.packages(c(
  "cluster", "config", "configr", "corrplot", "dplyr", "DT", 
  "fs", "fst", "geosphere", "geohashTools", "ggplot2", "ggtext", 
  "gridExtra", "ini", "leaflet", "leafsync", "mapview", "plotly", 
  "processx", "quanteda", "RColorBrewer", "reshape2", "reticulate", 
  "scales", "sf", "shiny", "shinycssloaders", "shinydashboard", 
  "shinyFiles", "shinyjs", "shinythemes", "shinyWidgets", "sp", 
  "spdep", "tidyverse", "tmap", "viridis"
))
```

### 3. Launch Application
```r
shiny::runApp("app/")
```

## Usage Workflow

### 1. Data Preparation
- Upload required input files through the Data Preparation tab
- Define objective names and units
- Run data preprocessing to generate analysis variables

### 2. Pareto Front Exploration
- Visualize optimization results across different objectives
- Set axis ranges and explore solution relationships
- Identify regions of interest for clustering

### 3. Clustering Analysis (Optional)

#### Quick Start with Defaults
1. Navigate to "Configure Clustering"
2. Select default settings
3. Run automatic clustering with predefined parameters

#### Manual Clustering
1. **Correlation Analysis**:
   - Select variables for clustering
   - Set correlation threshold (default: 0.7)
   - Remove highly correlated variables

2. **PCA & Clustering**:
   - Configure PCA components
   - Choose clustering method (K-means or K-medoids)
   - Set number of clusters
   - Execute clustering algorithm

### 4. Results Analysis
- Examine representative solutions from each cluster
- Compare measure implementations across clusters
- Export results and visualizations

### 5. AHP Analysis (Alternative Method)
- Define pairwise comparisons between objectives
- Calculate priority weights
- Combine with clustering results if desired

## Configuration

### Clustering Parameters

The application uses `input/config.ini` for clustering configuration:

```ini
[Clustering]
min_clusters=3
max_clusters=20
fixed_clusters_boolean=true
fixed_clusters=15

[PCA]
min_components=2
max_components=8

[Extreme_solutions]
handle_outliers_boolean=false
deviations_min=3
deviations_max=3
```

### Customization Options

- **Correlation threshold**: Adjustable from 0.0 to 1.0
- **Cluster numbers**: Fixed or range-based testing
- **Outlier handling**: Enable/disable with custom parameters
- **PCA components**: Specify number of principal components
- **Visualization**: Customizable axis labels and ranges

## Output Files

### Generated During Analysis

- `correlation_matrix.csv`: Variable correlation results
- `pca_content.RDS`: Variables included in PCA
- `hru_in_optima.RDS`: HRU-optimum relationships
- Various clustering output files and plots

### Export Options

- **Plots**: PNG format with customizable resolution
- **Data tables**: CSV format
- **Shapefiles**: Zipped spatial data
- **Clustering results**: Multiple formats available

## Troubleshooting

### Common Issues

1. **File Upload Errors**
   - Ensure files match expected formats
   - Check file permissions and size limits
   - Verify shapefile completeness (all components required)

2. **Clustering Failures**
   - Verify all required files are present
   - Check for sufficient data variability
   - Ensure reasonable parameter settings

3. **Memory Issues**
   - Reduce dataset size for testing
   - Increase R memory allocation
   - Consider server deployment for large datasets

### Error Messages

- **"Please provide pareto_fitness.txt"**: Upload fitness file in Data Preparation
- **"All files have been provided, please specify objective names"**: Define objectives before proceeding
- **"Please run the correlation analysis first"**: Complete correlation step before clustering

## Performance Considerations

- **Data Size**: Application tested with datasets up to several thousand solutions
- **Processing Time**: Clustering may take 5-15 minutes depending on dataset size
- **Memory Usage**: Ensure sufficient RAM for large spatial datasets
- **Browser Compatibility**: Tested with modern browsers (Chrome, Firefox, Safari)

## Technical Architecture

### Frontend (Shiny UI)
- **Framework**: Shiny Dashboard
- **Styling**: Custom CSS with responsive design
- **Interactivity**: Real-time updates and conditional panels

### Backend (Server Logic)
- **Processing**: R-based algorithms with optional Python integration
- **Data Handling**: Efficient file I/O and memory management
- **Validation**: Comprehensive input checking and error handling

### Integration Points
- **R-Python Interface**: Fallback R implementations available
- **File System**: Structured directory organization
- **Configuration**: INI-based parameter management

## Contributing

### Development Guidelines
1. Follow existing code structure and naming conventions
2. Test with sample datasets before committing
3. Update documentation for new features
4. Maintain backward compatibility where possible

### Adding New Features
1. Implement in appropriate module (UI/Server/Functions)
2. Add necessary package dependencies to `global.R`
3. Update user interface elements
4. Test integration with existing workflow

## License and Citation

This code is licensed under the GNU LGPL v. 2.1

## Support and Contact

This code was developed by Cordula Witteking as part of the OPTAIN project
This version is maintained by Martyn Futter martyn.futter@slu.se

---

*This documentation covers the core functionality of ParetoPick-R. For specific implementation details or advanced configuration options, refer to the source code comments and inline documentation.*
