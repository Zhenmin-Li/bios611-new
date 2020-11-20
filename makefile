.PHONY : clean

## Build the final report.
report.pdf: report.tex figures/top20.png figures/bigramms.png figures/mostused.png figures/wordcloud1.png figures/wordcloud2.png figures/time_bing.png
	pdflatex report.tex
	pdflatex report.tex
	mkdir -p versioned_reports
	cp report.pdf versioned_reports/game-timeline-`date | tr ' :' '_'`-`git log -1 | grep commit  | cut -d' ' -f2 |cut -c 1-5`.pdf



## generate the image.
figures/top20.png figures/bigramms.png figures/mostused.png figures/wordcloud1.png figures/wordcloud2.png figures/time_bing.png: project.R source_data/ttweets1.csv source_data/ttweets1.csv
	Rscript project.R

clean:
	rm *.pdf
	rm figures/*