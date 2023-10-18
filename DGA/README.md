# DGA

The below is from https://attack.mitre.org/techniques/T1568/002/: 

Adversaries may make use of Domain Generation Algorithms (DGAs) to dynamically identify a destination domain for command and control traffic rather than relying on a list of static IP addresses or domains. This has the advantage of making it much harder for defenders to block, track, or take over the command and control channel, as there potentially could be thousands of domains that malware can check for instructions. DGAs can take the form of apparently random or "gibberish" strings (ex: istgmxdejdnxuyla.ru) when they construct domain names by generating each letter. Alternatively, some DGAs employ whole words as the unit by concatenating words together instead of letters (ex: cityjulydish.net). Many DGAs are time-based, generating a different domain for each time period (hourly, daily, monthly, etc). Others incorporate a seed value as well to make predicting future domains more difficult for defenders.

## Analysis
This analysis tests and evaluates various data mining and machine learning approaches to detect DGA domains. The analysis ultimately presents results for several classification methods, highlights the recommended modeling approach based on the data, and provides insights into features of domains that demonstrated the strongest correlation with identified malicious DGA domains. The data used in our experiment was obtained by Netlab360 and contains labeled sets of over one million DGA domains and 640,000 legitimate domains. Models tested included:
- Adaboost or Gradient Boosting;
- Logistic Regression;
- Linear and Quadratic Discriminant Analysis;
- Naïve Bayes;
- General Additive Model with Splines;
- Classification and Regression Trees;
- Random Forests; and
- K-Nearest Neighbors

After using a Monte Carlo Cross Validation approach, using Gradient Boosting had the highest prediction accuracy at an average 93.3% with a variance of 0.004%.
