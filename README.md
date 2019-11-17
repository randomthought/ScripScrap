# LinkScraper
A fast concurrent user friendly CLI spider scraper built in haskell. Please
use responsibly.

## Install

### Requirments
- haskell stack

Install haskell Stack

``` sh
curl -sSL https://get.haskellstack.org/ | sh
```

### Build and Install

Clone and install ScripScrap

``` sh
git clone https://github.com/randomthought/ScripScrap
cd ScripScrap
stack install
```

## Usage

ScripSrap relies on a configuartion file that specifies the list of donains you
wish to scrape and the selectors.

In the example below, we create a `scrape_config.yaml` file with the following
contents. Our goal with this config is to scrape for all the post titles on
hackernews by only looking at the main pages. The pattern urls we are interesed
in is pages is `news.ycombinator.com/news`. As ScripScrap discveres new paths in
`news.ycombinator.com` it would only discard urls that do not contain the pattern.

Same gues for `wikipedia`. ScripScrap would go search for urls containing
`en.wikipedia.org` to visit and look for pages that contain any the selected css information.

```yaml
workers: 5 # Number of workers you wish to use.
output: /tmp/scrape.txt # Location to save the scrapped data.
targets:
  - targetName: hacker news
    startingUrl: https://news.ycombinator.com # Website to start
    includePatterns: # Postfix Regex patterns that match on discivered urls you wish to scrape
      - news.ycombinator.com/news
    selectors: # CSS selectors containing that should match the content to be scraped
      - name: Post Title
        selector: .storylink
  # Adding another target example below
  - targetName: wikipedia
    startingUrl: https://en.wikipedia.org/wiki/Ferrari_F50
    includePatterns:
      - "en.wikipedia.org"
    selectors:
      - name: title
        selector: .fn
      - name: picture
        selector: .infobox > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > a:nth-child(1) > img:nth-child(1)
```

Now that a the `scrape_config.yaml` file has been created, we can simple run
ScripScrap using as shown below.

``` sh
$ scripscrap -f scrape-config.yaml
```
