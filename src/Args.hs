-- | 

module Args (
  opts
  ) where


import Types

import Options.Applicative
import Data.Semigroup ((<>))

confParser :: Parser ScrapeConfig
confParser = ScrapeConfig
  <$> some (option str (long "css"
                         <> short 'c'
                         <> metavar "SELECTOR"
                         <> help "A list of CSS Selectors you wish to extract from each url"))
  <*> some (option str (long "regex"
                         <> short 'r'
                         <> metavar "PATTERN"
                         <> help "A list of regex patterns you wish to filter out discovered urls you don't wish to scrape"))
  <*> option auto (long "workers"
                   <> short 'w'
                   <> value 3
                   <> metavar "INT"
                   <> help "Number of worker threads you wish to use")
  <*> strOption (long "output"
                   <> short 'o'
                   <> metavar "FILEDIR"
                   <> help "Location where you wish to store the scrapped contents")

newScrapeParser = NewScrape
  <$> strOption (long "url"
                 <> short 'u'
                   <> metavar "URL"
                 <> help "The url to scrape")

targetedScrapeParser = Targeted
  <$> strOption (long "file"
                 <> short 'f'
                 <> metavar "FILEDIR"
                 <> help "Text file containing the list of urls you want to scrape")

modeScrapeParser = newScrapeParser <|> targetedScrapeParser

appArgsParser = AppArgs <$> liftA2 (,) modeScrapeParser confParser

opts = info (appArgsParser <**> helper) fullDesc
