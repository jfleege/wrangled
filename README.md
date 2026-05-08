# <img src="https://cdn.pixabay.com/animation/2023/06/13/15/13/15-13-13-522_512.gif" width="50"> *wrangled*

An *R* Shiny app that focuses on uploading, previewing, and visually exploring 
structured datasets. This project was originally made for my Statistical Data Management class (STAT 440) at the University of Illinois, Urbana Champaign.

## About 
Users can either upload a local file or paste a direct dataset link, then view the data in an interactive table. Upon providing the given dataset, users then can view any missing values in their data, remove rows with null values, rename columns, reorder columns, and download the edited version of their dataset. This app is designed to be a simple, user-friendly tool for anyone looking to quickly explore and make basic edits to their structured datasets without needing to write any code themselves.

### Datasets 

Users can choose to upload any **structured** dataset to be explored and edited. This means that the dataset fits into a fixed, predefined schema, typically stored in tables with rows and columns with a varied delimeter. A delimeter is an item/character used to specify the boundary between separate, independent data items in a text file, data stream, or programming code. The following file types and delimeters are allowed:

**File types available for upload:**
- `.csv`
- `.tsv`
- `.txt`

**Possible delimeters:**
- `comma: ','`
- `semicolon: ';'`
- `pipe: '|'`
- `tab: '\t'`
- `space: ' '`

#### **Note — Just because a file ends in a certain type does not mean that it necessarily has that delimeter type.*

---

## Installation & Usage

### Installation

First, clone the repository:

```bash
git clone https://github.com/username/repo-name.git
cd repo-name
```

Then open R or RStudio and install the required R packages:

```R
install.packages(c("tidyverse", "shiny", "bslib", "DT"))
```

### Usage

To launch the Shiny app, run:

```R
shiny::runApp("wrangled.R")
```

Or, if you are already inside the project folder in RStudio, open `wrangled.R` and click the **`Run App`** button in the top right-hand corner.

Once the app opens, users can either upload a local structured data file or paste a direct dataset link. The app supports .csv, .tsv, and .txt files, and users can select a delimiter manually or use the default Auto-detect option. <p align="center"> <img width="90%" alt="wrangled UI" src="https://github.com/user-attachments/assets/517e8706-4809-4147-8724-ff5b48db4ec9" /> </p> <p align="center"><em>Main file input dashboard demonstrating example file and delimeter options.</em></p>

After loading a dataset, users can preview the data in an interactive table, filter columns, adjust the number of visible rows, remove rows with missing values, rename columns, reorder/remove columns, and download the edited dataset as a new file. <p align="center"> <img width="90%" alt="wrangled UI" src="https://github.com/user-attachments/assets/3f65abb6-7f19-4bac-b869-4de7e217c847" /> </p> <p align="center"><em>Detail view shown after uploading a file, depicting all the possible editing/manipulation options.</em></p>
