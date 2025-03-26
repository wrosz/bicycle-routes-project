# Most Popular Bicycle Routes in New Jersey (2016–2020)

## Description  
This **Shiny** app allows interactive analysis and visualization of the most frequently chosen bicycle routes in **New Jersey** based on data from **2016 to 2020**.

The data used for this project is part of a **bike-sharing analysis** and was sourced from the **New York City Bike Share Program**. The data can be found at the following link: [https://s3.amazonaws.com/tripdata/index.html](https://s3.amazonaws.com/tripdata/index.html).

### App Features  
- **Interactive map** with bike routes  
- **Time-based filtering** – months, quarters, years  
- **Select years** – analyze specific periods  
- **Adjustable number of routes** (1–50)  
- **Show/hide bike station markers**  
- **Table displaying selected route data**  

---

## Installation and Setup  

### 1. Install Required Libraries  
Before running the app, ensure the required R libraries are installed:  
```r
install.packages(c("shiny", "shinyjs", "leaflet", "data.table"))
```

### 2. Run the Application  
1. Download the project folder.  
2. Open **R** or **RStudio**.  
3. Set the working directory to the app folder:  
   ```r
   setwd("path/to/folder")
   ```
4. Run the app:  
   ```r
   shiny::runApp("app.R")
   ```

---

## Project Structure  

- **data/** – Folder containing **.csv** data files with bike rental information for **New Jersey** from **2016–2020** (one file per month).  
- **app.R** – Main **Shiny** application file (UI + server logic).  
- **data.R** – Functions for processing and analyzing data.

---

## Data Analysis – Key Functions  

### `group_routes(years, by, combine_years)`  
Groups rides between stations by **month, quarter, or year**.  

### `popular_routes_dt(years, by, combine_years, s_month, s_quarter, s_year)`  
Returns the most popular routes for a given time period. Filters routes based on selected months, quarters, or years.  

### `render_map(dt, limit, show_markers)`  
Displays a map of the most popular routes with optional markers for bike station locations.  

### `table_to_display(dt)`  
Formats the data table for display in the app.
