# <img src="https://cdn.pixabay.com/animation/2023/06/13/15/13/15-13-13-522_512.gif" width="40"> *wrangled*

An ***R*** Shiny app that focuses on uploading, previewing, and visually exploring 
structured datasets. This project was originally made for my Statistical Data Management class (STAT 440) at the University of Illinois, Urbana Champaign.

## About 
Users can either upload a local file or paste a direct dataset link, then view the data in an interactive table. Upon providing the given dataset, users then can view any missing values in their data, remove rows with null values, rename columns, reorder columns, and download the edited version of their dataset. This app is designed to be a simple, user-friendly tool for anyone looking to quickly explore and make basic edits to their structured datasets without needing to write any code themselves.

### Datasets 

Users can choose to upload any **structured** dataset to be explored and edited. This means that the dataset fits into a fixed, predefined schema, typically stored in tables with rows and columns with a varied delimeter. A delimeter is an item/character used to specify the boundary between separate, independent data items in a text file, data stream, or programming code. The following file types and delimeters are allowed:

**File types available for upload:**
- **`.csv`**
- **`.tsv`**
- **`.txt`**

**Possible delimeters:**
- **`comma: ','`**
- **`semicolon: ';'`**
- **`pipe: '|'`**
- **`tab: '\t'`**
- **`space: ' '`**

#### **Note — Just because a file ends in a certain type does not mean that it necessarily has that delimeter type.*

---

### Usage


