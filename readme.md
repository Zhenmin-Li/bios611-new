# Sentiment Analysis of Trump's Tweets before election

The proposal is in the file "proposal"

Running the environment (you could replace your_password with your password):

```
docker build . -t env1 --build-arg linux_user_pwd=your_password
docker run -v "`pwd`:/home/rstudio" -p 8787:8787\-e PASSWORD=your_password -t env1
```

# Make Targets

You can directly run the make file

```
make
```

Or view the figures

```
make figures/top20.png
make figures/bigramms.png
make figures/mostused.png
make figures/wordcloud1.png
make figures/wordcloud2.png
make figures/time_bing.png
```

Or generate the reports
```
make report.pdf
```

You can view the dataset (Trump's tweets) in "source_data" and project proposal in "proposal"