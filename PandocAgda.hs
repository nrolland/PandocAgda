-- | PandocAgda.
import Data.Maybe
import Data.List
import Text.XHtml.Strict
import Control.Monad.IO.Class
import System.Environment
import System.Console.GetOpt

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

import Agda.Interaction.Options
import Agda.Utils.IO.UTF8 (readTextFile)
import Agda.Utils.Except (throwError)
import Agda.Interaction.Highlighting.HTML hiding (generateHTML)
import Agda.Main hiding (main)

import Paths_PandocAgda

-- | The name of the default template file for PandocAgda.
defaultTemplateFile :: FilePath
defaultTemplateFile = "Agda.template"

-- | PandocAgda specific options
data PACommandLineOptions = PAOptions
    { optSlideVariant :: Maybe HTMLSlideVariant
    , optTemplateFile :: Maybe FilePath
    , originalOptions :: CommandLineOptions
    }

paDefaultOptions :: PACommandLineOptions
paDefaultOptions = PAOptions
    { optSlideVariant = Nothing
    , optTemplateFile = Nothing
    , originalOptions = defaultOptions
    }

paLift :: Flag CommandLineOptions -> Flag PACommandLineOptions
paLift f = \opts -> do
    ps <- f $ originalOptions opts
    return opts{originalOptions = ps}

slideVariantFlag :: Maybe String -> Flag PACommandLineOptions
slideVariantFlag Nothing o = return o{optSlideVariant = Nothing}
slideVariantFlag (Just s) o = case reads s of
    [(v, "")] -> return o{optSlideVariant = Just v}
    _ -> throwError $ "Available slide variants: "
           ++ unwords (map show [NoSlides, S5Slides, SlidySlides, SlideousSlides, DZSlides, RevealJsSlides])

templateFlag :: FilePath -> Flag PACommandLineOptions
templateFlag f o = return o{optTemplateFile = Just f}

paStandardOptions :: [OptDescr (Flag PACommandLineOptions)]
paStandardOptions =
    [ Option [] ["slidevariant"] (OptArg slideVariantFlag "SLIDEVARIANT")
                ("generate HTML files with highlighted source code; " ++
                 "giving slide variant invokes pandoc")
    , Option [] ["template"] (ReqArg templateFlag "URL")
                "the template file used for HTML generation with pandoc (can be relative)"
    ] ++ map (fmap paLift) standardOptions

-- | Parse the standard options.
paParseStandardOptions :: [String] -> Either String PACommandLineOptions
paParseStandardOptions argv =
    paLift checkOpts =<<
    getOptSimple argv paStandardOptions (fmap paLift inputFlag) paDefaultOptions

-- | Constructs the web page, including headers, with pandoc.
pandocPage
    :: ([(Int, String, a)] -> Html)
    -> String              -- ^ Template
    -> [(Int, String, a)]  -- ^ Highlighting information.
    -> Pandoc
pandocPage code' contents info
    = Pandoc metainfo $ concat $ zipWith ($) (removeCodeBlocks md [])
    $ (++ [[]])
    . map conv
    . getCode
    . splitComment "\n\\begin{code}" . splitComment "\n\\end{code}"
    $ info
 where
    Pandoc metainfo md = readMarkdown def contents 

    conv x = [Para [RawInline (Format "html") $ renderHtmlFragment $ pre << code' x]]

    getCode ((_,s,_):xs)
        | s == "\n\\begin{code}" || isPrefixOf "\\begin{code}" s
        = getCode' xs []
    getCode (_:xs) = getCode xs
    getCode [] = []

    getCode' ((_,s,_):xs)
        | s == "\n\\end{code}" || isPrefixOf "\\end{code}" s
        = (: getCode xs) . reverse
    getCode' (x:xs) = getCode' xs . (x:)
    getCode' [] = error "pandocPage: impossible"

    splitComment w = concatMap f
      where
        f (pos, s, mi) = [(pos, t, mi) | t <- filter (not . null) $ g s []]

        g s | isPrefixOf w s = (: (w : g (drop (length w) s) [])) . reverse
        g [] = (:[]) . reverse
        g (c:cs) = g cs . (c:)

    removeCodeBlocks (RawBlock (Format "latex") s: xs) | isPrefixOf "\\begin{code}" s
        = removeCodeBlocks' (++) xs
    removeCodeBlocks (RawBlock (Format "html") s: xs) | isPrefixOf "<!--\n\\begin{code}" s
        = removeCodeBlocks' const xs
    removeCodeBlocks xs@(Para (Str "\\begin{code}":_):_)
        = removeCodeBlocks' (++) (drop 1 $ dropWhile (not . end) xs)
      where
        end (Para ys@(_:_)) = last ys == Str "\\end{code}"
        end _ = False
    removeCodeBlocks (x:xs) = removeCodeBlocks xs . (x:)
    removeCodeBlocks [] = removeCodeBlocks' (++) []

    removeCodeBlocks' f xs acc = f (reverse acc): case xs of
        [] -> []
        ys -> removeCodeBlocks ys []

-- | Main
main :: IO ()
main = runTCMPrettyErrors $ do
    progName <- liftIO getProgName
    argv     <- liftIO getArgs
    case paParseStandardOptions argv of
        Left err -> liftIO $ optionError err
        Right opts -> do
            templateFile <- liftIO $ maybe (getDataFileName defaultTemplateFile) return
                                        (optTemplateFile opts)
            template <- liftIO $ readTextFile templateFile
            let renderer css = writeHtmlString def
                    { writerStandalone      = True
                    , writerTemplate        = template
                    , writerTableOfContents = True
                    , writerSlideVariant    = fromMaybe NoSlides $ optSlideVariant opts
                    , writerVariables       = [("css", css)]
                    , writerHTMLMathMethod  = MathML Nothing
                    }

                pageGen dir modname highlighting = generatePage gen dir modname
                  where
                    gen css path contents
                        | isLiterate path = renderer css $ pandocPage code contents s
                        | otherwise       = page css modname $ code s
                      where
                        s = tokenStream contents highlighting

            runAgdaWithOptions (generateHTMLWithPageGen pageGen) progName (originalOptions opts)
