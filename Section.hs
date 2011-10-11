module Section (Section(..), sections) where

import Parser (parses, parseSectionHeader, sectionHeader)

-- split the file up into sections
-- right now we only support .data sections
data Section = Section Int [String] deriving Show

-- given a list of lines of source code, splits it into a list of data sections
sections :: Monad m => [String] -> m [Section]
sections [] = return []
sections (header:rest) = do
  addr <- parseSectionHeader header
  let (body, afterBody) = span (not . parses sectionHeader) rest
  restSections <- sections afterBody
  return $ (Section addr body):restSections

