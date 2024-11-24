# Project: Scalable Visualization Techniques for Big Data Using Distributed Systems

## Team Members
1. Satvik Praveen
2. Matt Palmer
3. Jonathan Tong
4. Kaitlyn Griffin
5. Reed Pafford

## Project Description
This project explores scalable visualization techniques for big data using distributed systems. We worked with diverse data types, including massive tabular data, audio, images, and textual data, sourced from the NHL (National Hockey League). Using various machine learning and distributed computing techniques, we created insightful visualizations that reveal trends, patterns, and relationships in the data. The visualizations for each data type are stored in the respective output folders within this repository.

## Folder Structure
The repository is organized as follows:

```bash
├── Audio
│   ├── Data                # Raw audio data files
│   ├── Output              # Generated audio visualizations (waveforms, spectrograms, MFCC heatmaps)
│   └── Audio.ipynb         # Jupyter notebook for audio data analysis
├── Data                    # General data folder (if additional datasets are included)
├── Image
│   ├── data                # Raw image files of NHL players in action
│   ├── Output              # Generated image visualizations (t-SNE plots, clustered images)
│   └── Image2.ipynb        # Jupyter notebook for image data analysis
├── Tabular_Massive
│   ├── Output              # Visualizations of massive tabular data (game statistics, trends)
│   └── Tabular_Data_Visualization.ipynb  # Jupyter notebook for tabular data analysis
├── Textual
│   ├── Output              # Visualizations of textual data (word clouds, t-SNE plots, TF-IDF analysis)
│   └── Textual.ipynb       # Jupyter notebook for textual data analysis
├── .gitignore              # Files and folders to be ignored by Git
└── README.md               # This README file
```

## Data Overview
The project utilizes the following types of data:
1. **Massive Tabular Data:** Scraped from the NHL API, covering play-by-play statistics for all games from the 2013 to 2023 seasons.
2. **Audio Data:** Consists of stadium noise recordings during NHL games. We extracted features such as waveforms, MFCCs, and spectrograms for analysis.
3. **Image Data:** Action images of 5 NHL players, processed for clustering and feature extraction using pre-trained deep learning models.
4. **Textual Data:** Articles scraped from the NHL website, analyzed using techniques like TF-IDF, word clouds, and t-SNE for topic modeling.

## Methodology
The project employs distributed computing techniques using Python libraries such as Dask for efficient data processing. We used a variety of machine learning and dimensionality reduction techniques, including:
- **Principal Component Analysis (PCA)**
- **t-Distributed Stochastic Neighbor Embedding (t-SNE)**
- **TF-IDF for textual analysis**
- **K-means clustering for image analysis**

## How to Run
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/your-repo-name.git
   ```
2. Navigate to the folder of interest (e.g., `Audio`, `Image`, `Tabular_Massive`, or `Textual`).
3. Open the Jupyter notebook in your preferred environment (e.g., Google Colaboratory or JupyterLab):
   ```bash
   jupyter notebook Audio/Audio.ipynb
   ```
4. Execute the cells to preprocess the data, perform analysis, and generate visualizations.

## Dependencies
The project requires the following Python libraries:
- `numpy`
- `pandas`
- `matplotlib`
- `seaborn`
- `scikit-learn`
- `torch` (PyTorch)
- `nltk`
- `beautifulsoup4`
- `Dask`

Install all dependencies using:
```bash
pip install -r requirements.txt
```

## Challenges and Limitations
- **Data Scraping Issues:** Security measures on the NHL website prevented direct scraping for audio and image data. We manually downloaded the datasets, which was time-consuming.
- **Video Data:** Due to computational and infrastructure constraints, we could not include video data in the analysis.
- **Memory Management:** We faced challenges in parallel processing using Dask, especially with large tabular datasets, due to memory disparities between CSV files and table structures.

## Future Work
- Integration of video data into the analysis pipeline.
- Implementation of a Large Language Model (LLM) for automated report generation based on the visualizations.
- Expansion of the application to support additional big data sources, including other sports datasets.
- Refining memory management and parallel processing to improve the speed and efficiency of data scraping and visualization.

## Acknowledgments
We thank the National Hockey League (NHL) for providing the publicly accessible data used in this project.

## License
This project is licensed under the MIT License - see the `LICENSE` file for details.

### Explanation:
- **Structure:** The README is organized with clear sections for Project Description, Folder Structure, Methodology, How to Run, and more.
- **Details:** Includes explanations of each data type, methods used, challenges faced, and future improvements.
- **Instructions:** Provides users with guidance on how to clone the repo and run the notebooks.
