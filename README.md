# Project: Scalable Visualization Techniques for Big Data Using Distributed Systems

## Team Members
1. Satvik Praveen
2. Matt Palmer
3. Jonathan Tong
4. Kaitlyn Griffin
5. Reed Pafford

## Project Description
This project investigates scalable visualization techniques for big data using distributed systems. The focus is on analyzing diverse data types, including massive tabular data, audio recordings, images, and textual data sourced from the National Hockey League (NHL). Through the use of machine learning techniques and distributed computing frameworks, we generated insightful visualizations that uncover trends, patterns, and relationships in the data. All visual outputs for each data type are stored in their respective `Output` folders.

## Folder Structure
The repository is organized as follows:

```bash
├── Audio
│   ├── Data                # Raw audio data files
│   ├── Output              # Generated audio visualizations (waveforms, spectrograms, MFCC heatmaps)
│   └── Audio.ipynb         # Jupyter notebook for audio data analysis
├── Data                    # General folder for additional datasets
├── Image
│   ├── Data                # Raw images of NHL players in action
│   ├── Output              # Image visualizations (t-SNE plots, clustered images)
│   └── Image.ipynb         # Jupyter notebook for image data analysis
├── Tabular_Massive
│   ├── Output              # Visualizations for massive tabular data (game stats, trends)
│   └── Tabular.ipynb       # Jupyter notebook for tabular data analysis
├── Textual
│   ├── Output              # Visualizations for textual data (word clouds, t-SNE plots, TF-IDF)
│   └── Textual.ipynb       # Jupyter notebook for textual data analysis
├── .gitignore              # Files and folders to be ignored by Git
└── README.md               # This README file
```
---

## **Data Overview**
The project processes and analyzes the following types of data:

### **Massive Tabular Data**
- Play-by-play statistics of NHL games between 2013 and 2023.  
- Includes game events, player statistics, and team performance data.  

### **Audio Data**
- Stadium noise recordings during NHL games.  
- Analyzed using waveforms, MFCCs, and spectrogram visualizations.  

### **Image Data**
- Action images of five NHL players.  
- Processed using deep learning models for feature extraction and clustering.  

### **Textual Data**
- Articles related to NHL games, scraped from the web.  
- Analyzed with TF-IDF, word clouds, and t-SNE visualizations.  

---

## **Methodology**

### **Distributed Computing**
We employed distributed systems to handle the scale and complexity of big data, leveraging:  
- **Dask:** Parallel processing for efficient computation on massive tabular data.  
- **Google Colab:** Cloud resources to perform analysis across large datasets.  

### **Analysis Techniques**
- **Audio Data:** Short-Time Fourier Transform (STFT), Mel-Frequency Cepstral Coefficients (MFCC), waveforms, and spectrograms.  
- **Image Data:** Feature extraction using a pre-trained ResNet-50 model, dimensionality reduction (PCA, t-SNE), and clustering (K-means).  
- **Textual Data:** TF-IDF vectorization, word clouds, t-SNE visualizations, and topic modeling with Plotly dashboards.  
- **Tabular Data:** Statistical analysis and trend visualizations (e.g., bar charts, scatter plots, pie charts).  

--- 

Here’s the content converted to markdown format for your README file:

---

## **How to Run**

### **Steps to Run the Project**
1. Clone this repository:
   ```bash
   git clone https://github.com/kaitlyngrif/dataviz-group5.git
   ```
2. Navigate to the relevant folder for the data type (e.g., `Audio`, `Image`, `Tabular_Massive`, or `Textual`).  
3. Open the associated Jupyter notebook in your preferred environment:  
   ```bash
   jupyter notebook Audio/Audio.ipynb
   ```
4. Execute the notebook cells sequentially to preprocess data, perform analysis, and generate visualizations.  

---

## **Dependencies**

### **Major Libraries Used**
- **`numpy`, `pandas`:** Data manipulation and preprocessing  
- **`matplotlib`, `seaborn`:** Visualization libraries  
- **`scikit-learn`:** Machine learning for dimensionality reduction and clustering  
- **`torch`:** For deep learning-based image feature extraction  
- **`nltk`:** Text preprocessing  
- **`beautifulsoup4`:** Web scraping for textual data  
- **`Dask`:** Distributed computing  

---

## **Challenges and Limitations**
- **Data Scraping Issues:** Security protocols on the NHL website prevented automated scraping for some datasets (e.g., images and audio), necessitating manual downloads.  
- **Video Data Exclusion:** Computational limitations prevented the inclusion of video data in the analysis.  
- **Parallel Processing Challenges:** Memory constraints caused inefficiencies during parallel processing of large datasets.  

---

## **Future Work**
- Incorporate video data into the analysis pipeline.  
- Utilize Large Language Models (LLMs) for automated report generation based on visualizations.  
- Expand the framework to process additional datasets from other sports.  
- Enhance the efficiency of parallel processing and data visualization.  

---

## **Acknowledgments**
We thank the National Hockey League (NHL) for providing the publicly accessible data used in this project.  

---

## **License**
This project is licensed under the MIT License. See the `LICENSE` file for more details.  

---

This is ready to be appended to your existing README.md file. Let me know if there’s anything else you'd like to adjust!
