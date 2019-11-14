# LinkScraper
A fast concurrent user friendly CLI spider scraper built in haskell. Please
use responsibly.

## Requirements

Install haskell Stack

``` sh
curl -sSL https://get.haskellstack.org/ | sh
```

Clone and install ScripScrap

``` sh
git clone https://github.com/randomthought/ScripScrap
cd ScripScrap
stack install
```

## Usage

ScripSrap relies on a configuartion file that specifies the list of donains you
wish to scrape and the selectors.

In the example below, we create a `scrap_config.yaml` file with the following
contents. Our goal with this config is to scrape for all the post titles on
hackernews by only looking at the main pages. The pattern urls we are interesed
in is pages is `news.ycombinator.com/news`. As ScripScrap discveres new paths in
`news.ycombinator.com` it would only discard urls that do not contain the pattern.

Same gues for `wikipedia`. ScripScrap would go search for urls containing
`en.wikipedia.org` to visit and look for pages that contain any the selected css information.

``` yaml
workers: 5 # Number of workers you wish to use.
output: /tmp/scrape.txt # Location to start 
targets:
  - targetId: 1
    startingUrl: https://news.ycombinator.com
    includePatterns:
      - news.ycombinator.com/news
    selectors:
      - name: Post Title
        selector: .storylink
  - targetId: 2
    startingUrl: https://en.wikipedia.org/wiki/Ferrari_F50
    includePatterns:
      - "en.wikipedia.org"
    selectors:
      - name: title
        selector: .fn
      - name: picture
        selector: .infobox > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > a:nth-child(1) > img:nth-child(1)
```
