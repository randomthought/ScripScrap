-- | 

module Args (
  opts
  ) where


import Types

import Options.Applicative
import Data.Semigroup ((<>))


newScrapeParser :: Parser ScrapeMode
newScrapeParser = New
  <$> strOption (long "file"
                 <> short 'f'
                 <> metavar "FILEDIR"
                 <> help "Location to save the scraped data.")

resumeScrapeParser :: Parser ScrapeMode
resumeScrapeParser = Resume
  <$> strOption (long "resume-file"
                 <> short 'r'
                 <> metavar "FILEDIR"
                 <> help "File containing the previously scrapped data for resuming purposes.")

modeScrapeParser = newScrapeParser <|> resumeScrapeParser

opts = info (modeScrapeParser <**> helper) fullDesc
