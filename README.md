<h1>VAERS Clinical Text Analysis</h1>
The VAERS Clinical Text Analysis project aims to leverage data wrangling, data cleaning, and Natural Language Processing (NLP) techniques to analyze clinical text data. The primary objective is to clean the clinical text and match it against the symptoms column to assess how effectively the clinical text represents the derived symptoms.

<h3>Data Sources</h3>
The data for this project can be downloaded from https://langara-my.sharepoint.com/:f:/r/personal/qnguyen_langara_ca/Documents/VAERS?csf=1&web=1&e=IAsFS3.

<h3>Technologies Used</h3>
The project has been implemented using the R language. R provides a robust set of tools and packages for data manipulation, text analysis, and visualization, making it suitable for this analysis.

<h3> Dependencies </h3>
The dependencies for this project can be installed by running the code.

<h3> Matching Approach </h3>
The project used two different approaches to determine the similarity between the symptoms and clinical text columns. The first approach which is basic involved computing the word frequency and comparing it to the symptoms column, while the second approach used the levenshtein algorithm to compute the similarity between the two columns.<br>
The second approach utilized the levenshtein algorithm to determine the similarity, considering rows that had at least a 40% match with the symptoms.

<h3>Contributing</h3>
Contributions to the VAERS Clinical Text Analysis project are welcome. If you'd like to contribute, please follow these guidelines:

Fork the repository and create a new branch for your contribution.
Make your changes and ensure that the code adheres to the project's style guidelines.
Write clear and concise commit messages.
Submit a pull request, describing the changes you've made and the rationale behind them.

