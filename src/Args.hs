-- | 

module Args (
  opts
  ) where


import Types

import Options.Applicative
import Data.Semigroup ((<>))



scrapeParser :: Parser Options
scrapeParser = Options
  <$> strOption (long "file"
                 <> short 'f'
                 <> metavar "FILEDIR"
                 <> help "Location of your configuration yaml")
  <*> strOption (long "output"
                 <> short 'o'
                 <> metavar "FILEDIR"
                 <> help "Location to save the scraped data.")
  <*> switch (long "resume"
                 <> short 'r'
                 <> help "Indicate if this is a resume session")

opts = info (scrapeParser <**> helper) fullDesc
