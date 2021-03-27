module Command where

import MonadApp

import Data.Text (Text)
import Text.Parsec hiding (label)
import Text.Parsec.Text (Parser)

data Command
  = Trace TraceOpts
  | PrintType PrintTypeOpts
  | Load FilePath
  | Clear

commandParser :: Parser Command
commandParser = do
  char ':'
  clear <|> printType <|> trace <|> load
  where
    trace = Trace <$> do
      try $ string "trace"
      spaces
      try traceOff <|> try traceLocal <|> try traceGlobal
    traceOff = TraceOff <$ string "off"
    traceLocal = TraceLocal <$ string "local"
    traceGlobal = TraceGlobal <$ string "global"

    printType = PrintType <$> do
      try $ string "printType"
      spaces
      try printBoth <|> try printDecls <|> printExprs <|> try printOff
    printBoth = PrintBoth <$ string "both"
    printDecls = PrintDecls <$ string "decls"
    printExprs = PrintExprs <$ string "exprs"
    printOff = PrintOff <$ string "off"

    load = Load <$> do
      try $ string "load"
      spaces
      many1 (noneOf " ")

    clear = Clear <$ try (string "clear")

-- | If the text is not command (i.e. starts with `:`), parse nothing;
-- otherwise, parse the command or return a parse error.
--
-- This allows attempting to parse a command and then falling back
-- to the expression interpreter if the input is not a command,
-- without forgetting about command parse errors.
parseCommand :: Text -> Either ParseError (Maybe Command)
parseCommand = parse ((Just <$> commandParser <* spaces <* eof) <|> pure Nothing) "input"
