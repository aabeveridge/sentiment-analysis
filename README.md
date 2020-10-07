# Sentiment Analysis with Tidytext

This week's tutorial continues to use the same dataset resulting from `rvest_example2.R`. In other words, you should have a complex dataset of your own, based on modifying that tutorial, with the following columns of data: `date`, `url`, and `text`. As that tutorial explains, you should be able to web scrape a single, complex website with multiple pages, or a long list of simple/static websites. The goal is to use Tidytext principals and techniques in analyzing your text data.

Therefore, `sentiment-analysis.R` should be modified to work with your own dataset, which should be structured similarly to the one I am using in this tutorial. By translating examples from chapters 2 and 4 in [*Text Mining with R*](https://www.tidytextmining.com/), this tutorial provides an introduction to sentiment analysis. Here are additional tutorial you may find useful:

- Alternative Tidytext Approaches:
  - <https://datascienceplus.com/parsing-text-for-emotion-terms-analysis-visualization-using-r-updated-analysis/>
  - <https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r>
- TM Package Sentiment Analysis:
  - <https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/>

Goals:
- Convert date/time stamps to useful forms (months/weeks)
  - Alternatively, you can also bucket your text in other ways
- Explore sentiment for various categories (time/other?)
- Reproduce the follow visuals/figures from chapters 2 and 4
  - Figure 2.2
  - Figure 2.4
  - Figure 4.2
  - Figure 4.3
