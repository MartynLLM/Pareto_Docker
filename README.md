# ParetoPick-R: Multi-Objective Optimization Analysis Tool
## 🐳 **Prototype Docker Application**

> **⚠️ PROTOTYPE STATUS**: This is a prototype Docker application currently under development. Features and functionality may change as development progresses. Use in production environments is not recommended at this time.

## Overview

ParetoPick-R is a **containerized prototype** Shiny web application designed for analyzing Pareto optimization results through advanced clustering and visualization techniques. Based on code developed by Dr. Cordula Wittekind as part of the Horizon Europe funded [OPTAIN project](https://www.optain.eu/), this Docker prototype provides tools for exploring multi-objective optimization solutions, performing correlation analysis, applying Principal Component Analysis (PCA), and implementing clustering algorithms to identify representative solutions from Pareto fronts.

## 🚀 Docker Deployment Features

- **Containerized Environment**: Self-contained R/Shiny application with all dependencies
- **Isolated Processing**: Runs in Docker container for consistent behavior across environments
- **Scalable Architecture**: Designed for potential cloud deployment and scaling
- **Data Persistence**: Configured with volume mounts for data input/output
- **Development Ready**: Prototype setup for testing and iterative development

## Application Features

- **Pareto Front Visualization**: Interactive exploration of multi-objective optimization results
- **Correlation Analysis**: Identification and management of highly correlated variables
- **Principal Component Analysis (PCA)**: Dimensionality reduction for clustering preparation
- **Clustering Methods**: K-means and K-medoids clustering algorithms
- **Analytical Hierarchy Process (AHP)**: Alternative prioritization method for Pareto solutions
- **Spatial Analysis**: Geographic visualization of measure implementations
- **Data Export**: Multiple export formats for results and visualizations

## 🏗️ Container Architecture

### Directory Structure
```
/
├── app/                    # Main Shiny application
│   ├── ui.R               # User interface
│   ├── server.R           # Server logic
│   ├── global.R           # Global variables and packages
│   ├── functions.R        # Custom helper functions
│   └── www/               # Web assets
├── data/                  # Container input data mount
├── input/                 # Processed input files (container volume)
├── output/                # Generated output files (container volume)
├── support/               # R script alternatives to Python
└── python_files/          # Python executables (optional)
```

### Main Application Tabs

1. **Introduction**: Overview of the application and methodology
2. **Data Preparation**: File upload and data preprocessing
3. **Visualising the Pareto Front**: Interactive exploration of optimization results
4. **Configure Clustering**: Setup clustering parameters with default or manual options
5. **Correlation Analysis**: Variable correlation assessment and selection
6. **PCA & K-means/K-medoids**: Dimensionality reduction and clustering execution
7. **Cluster Analysis**: Analysis and visualization of clustering results
8. **AHP (Analytical Hierarchy Process)**: Alternative prioritization methodology

## 🐳 Docker Setup and Deployment

### Prerequisites
- Docker Engine 20.10+
- Docker Compose (optional, for orchestrated deployment)
- Minimum 4GB RAM for container
- 2GB available disk space

### Quick Start (Prototype)
```bash
# Clone the repository
git clone [repository-url]
cd ParetoPick-R

# Build the Docker image (when Dockerfile is available)
docker build -t paretopick-r:prototype .

# Run the container with volume mounts
docker run -p 3838:3838 \
  -v $(pwd)/data:/app/data \
  -v $(pwd)/input:/app/input \
  -v $(pwd)/output:/app/output \
  paretopick-r:prototype
```

### Development Setup
```bash
# For development with live code changes
docker run -p 3838:3838 \
  -v $(pwd)/app:/app/app \
  -v $(pwd)/data:/app/data \
  -v $(pwd)/input:/app/input \
  -v $(pwd)/output:/app/output \
  paretopick-r:prototype
```

### Container Configuration

**Port Mapping**: 
- Container Port: 3838 (Shiny default)
- Host Port: 3838 (configurable)

**Volume Mounts**:
- `/app/data`: Input data files
- `/app/input`: Processed intermediate files  
- `/app/output`: Generated results and exports

**Environment Variables** (Future):
- `SHINY_PORT`: Application port (default: 3838)
- `SHINY_HOST`: Host binding (default: 0.0.0.0)
- `R_MEMORY_LIMIT`: Memory allocation for R processes

## 📋 Required Input Files

### Essential Files (Minimum for Pareto Front Analysis)
1. **`pareto_fitness.txt`**: Pareto optimization results
2. **Objective names**: Defined through the Data Preparation interface

### Additional Files (For Full Clustering Analysis)
1. **`pareto_genomes.txt`**: Genome/solution representations
2. **`hru.con`**: Hydrological Response Unit connections
3. **`measure_location.csv`**: Spatial measure implementation data
4. **Shapefile components**: `hru.shp`, `hru.shx`, `hru.dbf`, `hru.prj`
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

**measure_location.csv**: Measure-HRU mapping
```
name,nswrm,obj_id
measure1,type1,"hru1,hru2,hru3"
measure2,type2,"hru4,hru5"
...
```

## 🔧 Dependencies (Container-Managed)

### R Packages
The Docker container automatically installs and manages all required R packages:

**Core Shiny Packages:**
- `shiny`, `shinydashboard`, `shinyjs`, `shinyWidgets`
- `DT`, `plotly`, `ggplot2`, `corrplot`

**Data Processing:**
- `dplyr`, `tidyverse`, `reshape2`, `cluster`
- `config`, `configr`

**Spatial Analysis:**
- `sf`, `sp`, `spdep`, `geosphere`
- `leaflet`, `mapview`, `tmap`

**Visualization:**
- `RColorBrewer`, `viridis`, `gridExtra`
- `ggtext`, `scales`

### System Requirements (Host)
- Docker Engine with sufficient resources
- Web browser for accessing application interface
- Network access for package installation during build

## 🚦 Usage Workflow

### 1. Container Startup
```bash
# Access the application at http://localhost:3838
# Upload data files through the web interface
```

### 2. Data Preparation
- Upload required input files through the Data Preparation tab
- Define objective names and units
- Run data preprocessing to generate analysis variables

### 3. Pareto Front Exploration
- Visualize optimization results across different objectives
- Set axis ranges and explore solution relationships
- Identify regions of interest for clustering

### 4. Clustering Analysis
- **Quick Start**: Use default clustering parameters
- **Manual Configuration**: Custom correlation thresholds, PCA components, cluster numbers
- **Results Analysis**: Examine representative solutions and export results

## 🛠️ Development and Testing

### Prototype Development Status
- ✅ Core Shiny application functional
- ✅ R package dependencies managed
- ⚠️ Docker configuration in development
- ⚠️ Container optimization ongoing
- ⚠️ Production deployment not ready

### Local Development
```bash
# For local R development without Docker
cd app/
R -e "shiny::runApp()"
```

### Container Testing
```bash
# Test container build
docker build -t paretopick-r:test .

# Test with sample data
docker run --rm -p 3838:3838 \
  -v $(pwd)/test-data:/app/data \
  paretopick-r:test
```

## ⚠️ Prototype Limitations

- **Experimental Features**: Some functionality may be unstable
- **Performance**: Not optimized for large-scale production use
- **Data Persistence**: Container restarts may clear temporary data
- **Error Handling**: Limited error recovery in prototype stage
- **Documentation**: Some features may be undocumented

## 🐛 Troubleshooting

### Container Issues
- **Build Failures**: Check Docker version and available disk space
- **Memory Errors**: Increase Docker memory allocation (minimum 4GB)
- **Port Conflicts**: Use different host port mapping (-p 8080:3838)

### Application Issues
- **File Upload Errors**: Verify volume mounts and file permissions
- **Clustering Failures**: Ensure all required input files are present
- **Performance**: Consider reducing dataset size for testing

## 🤝 Contributing to the Prototype

### Development Guidelines
1. Test changes in containerized environment
2. Update documentation for new features
3. Follow existing code structure and naming conventions
4. Validate with sample datasets before committing

### Docker Development
1. Test container builds locally before pushing
2. Optimize image size where possible
3. Document any new environment variables or configurations
4. Ensure cross-platform compatibility

## 📄 License and Citation

**Project Funding**: The development of this code received funding as part of the OPTAIN project under the European Union's Horizon 2020 research and innovation programme (grant agreement No. 862756).

**Original Code**: Dr. Cordula Wittekind (OPTAIN project)  https://github.com/cowitt/ParetoPick-R
**Container Prototype**: Maintained by Martyn Futter (martyn.futter@slu.se)

## 📞 Support and Contact

**Prototype Support**: For Docker-related issues and prototype feedback  
**Technical Contact**: martyn.futter@slu.se  
**Project Information**: [OPTAIN Project Website](https://www.optain.eu/)

---

**⚠️ Prototype Disclaimer**: This Docker application is in active development. Features, performance, and stability are subject to change. Please report issues and provide feedback to help improve the containerized deployment.
