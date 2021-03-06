# ScripScrap
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
stack build
stack install # If you want to install the program as binary.
```

## Usage

ScripSrap relies on a configuration file that specifies the list of "targets" you
wish to scrape. Within each target, you specify the domain, xpath and/or css selectors.

In the example below, we create a `scrape_config.yaml` file with the following
contents. Our goal with this config is to scrape for all the post titles on
hackernews by only looking at the main pages. The pattern urls we are interested
in is pages is `news.ycombinator.com/news`. As ScripScrap discovers new paths in
`news.ycombinator.com` it would only discard urls that do not contain the pattern.

Same goes for `wikipedia`. ScripScrap would go search for urls containing
`en.wikipedia.org` then, look for pages that contain any of the listed css
and xpath in the site selectors.

```yaml
workers: 5 # Number of workers you wish to use.
targets:
  - targetName: hacker news
    startingUrls: 
      - https://news.ycombinator.com # Website to start
    extractPatters: # (Optional) The regex pattern of the url that contains data you wish to extract. Please note, url must match all patterns specified.
      - news
    includePatterns: # (Optional) Postfix Regex patterns that match on discovered urls you wish to scrape
      - news.ycombinator.com
    excludePatterns: # (Optional) Postfix Regex patterns that match on discovered urls you do not wish to scrape
    selectors: # CSS or Xpath selectors that should match the content to be scraped
      - name: Post Title
        type: css
        selector: .storylink
    output: /tmp/hn_data.txt # Location to save the scrapped data.
    visited: /tmp/hn_visited.txt # Location to save urls visited.

  # Adding another target example below
  - targetName: wikipedia
    startingUrls:
      - https://en.wikipedia.org/wiki/Ferrari_F50
      - https://en.wikipedia.org/wiki/Ferrari_Portofino
    extractPatters: # Page link must contain ferrari for the script to extract data from page
      - Ferrari
    includePatterns: # Only search for pages within the english wikipedia
      - en.wikipedia.org
    selectors:
      - name: title
        type: xpath # Using XPath 
        selector: //div[contains(@class, 'title')]/text()
      - name: picture
        type: css
        selector: .infobox > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > a:nth-child(1) > img:nth-child(1)
    output: /tmp/wiki_data.txt
    visited: /tmp/wiki_visited.txt
```

Now that a `scrape_config.yaml` file has been created, we can simply run
ScripScrap as shown below.

``` sh
$ stack run -- -f scrape-config.yaml
```
