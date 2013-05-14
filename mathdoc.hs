module MathDoc ( mathdoc, mathdocInline) where
import Text.Pandoc
import Text.Regex
import Data.Maybe
import Text.Pandoc.Writers.HTML
import Data.String.Utils
import Data.Set (insert)
import System.Environment (getArgs)
import Data.List (nub)

-- On mac, please do `export LANG=C` before using this thing
mathdocRead = def{readerExtensions = insert Ext_tex_math_double_backslash $ 
                                     insert Ext_tex_math_single_backslash $ 
                                     insert Ext_raw_tex pandocExtensions}
mathdocWrite = def{writerHTMLMathMethod = MathJax "", writerHtml5 = True}
readDoc :: String -> Pandoc
readDoc = readMarkdown mathdocRead

writeDoc :: Pandoc -> String
writeDoc = writeHtmlString mathdocWrite

mathdoc :: String->String
mathdoc = compute . formatTheorem

mathdocInline :: String->String
mathdocInline = removeP . writeDoc . readDoc
  where removeP x = drop 3 (take ((length x) - 4) x) 
  
incrementBlock = ["Theorem",
                  "Conjecture",
                  "Definition",
                  "Example",
                  "Lemma",
                  "Problem",
                  "Proposition"]
otherBlock = ["Proof","Remark"]

buildOr [x] = x
buildOr (l:ls) = l ++ '|' : buildOr ls
regex = "^\\{("
                 ++ buildOr (incrementBlock ++ otherBlock)
                 ++ ")\\}(\\((.*)\\)|$)$" 
blocks = mkRegex regex 

matchBlock = matchRegex blocks

buildReplace (t,n,i) = [(concat ["[",t," ",show i,"]"], link2),
                        (concat ["[",t," ",n,"]"], link2)]
  where link2 = "[" ++ t ++ " " ++ show i ++ "]"
                ++ "(#" ++ t ++ "-" ++ show i++")"

formatTheorem s = replaceMany replaceTable (formatBlocks s)
  where replaceTable = nub $ concatMap buildReplace (blocksAssoc s)

replaceMany [] s = s
replaceMany ((x,y):xs) s = replaceMany xs (replace x y s)

-- Format a block
formatBlocks xs = unlines $ fst $ formatBlocks' (lines xs) 1
blocksAssoc xs = snd $ formatBlocks' (lines xs) 1

formatBlocks' :: [String]->Int->([String],[(String,String,Int)])
formatBlocks' [] _ = ([],[])
formatBlocks' (x:xs) n= ([result] ++ results, assoc++assocs)
  where (result,inc,assoc) = formatBlock x n
        (results, assocs)  = formatBlocks' xs (n+inc)

formatBlock :: String->Int->(String,Int,[(String,String,Int)])
formatBlock x n
 | result    = ("######"++ name ++typeDes,inc, [(bType,name,n),(bType,show n,n)])
 | otherwise = (x,0,[])
 where  result = isJust $ matchBlock x
        [bType,_,name] = fromJust $ matchBlock x
        name' = if null name then "" else "\"" ++  name ++ "\""
        index = if bType `elem` otherBlock then "" else show n
        inc = if bType `elem` otherBlock then 0 else 1
        typeDes = " {type="++ bType ++" index="++ index ++" name=" ++ name' ++ "}"

compute x = (writeDoc $ bottomUp latex $ bottomUp theoremize $ readDoc x) ++ "\n"

latex :: Block -> Block
latex (RawBlock "latex" s) = RawBlock "html" ("<span class=\"math\">" ++s ++ "</span>")
latex x = x

theoremize :: [Block] -> [Block]
theoremize xs = t xs
  where t (x:y:xs)
         | isTheorem x = makeTheorem x y ++ (t xs)
         | otherwise   = x:(t (y:xs))
        t x = x
{-
makeTheorem (Header _ (_,_,parm) _) (CodeBlock o xs) = [rawStart] ++ content ++ [rawEnd]
  where t = fromJust $ lookup "type" parm
        name = fromJust $ lookup "name" parm
        index = fromJust $ lookup "index" parm
        divhead = concat ["<div class=\"",
                    t,
                    "\" id=\"",
                    t,
                    "-",
                    index,
                    "\">"]
        modifier = if t `elem` otherBlock then "_" else "**"
        inittext = modifier ++ t ++ indextext ++ initDot ++ modifier
        initDot = if null name then "." else ""
        indextext = if null index then "" else " " ++ index
        nametext = if null name 
                     then "" 
                     else " (" ++ name ++ ")."
        end = "</div>"
        rawEnd = RawBlock "html" end
        rawStart = RawBlock "html" divhead
        content = (getDoc . readDoc) (concat [inittext, nametext,"&#8194;&#8194;"] ++ xs)
-}
makeTheorem (Header _ (_,_,parm) _) (CodeBlock o xs) = [rawStart,rawHead] ++ content ++ [rawEnd]
  where t = fromJust $ lookup "type" parm
        name = fromJust $ lookup "name" parm
        index = fromJust $ lookup "index" parm
        sectionhead = concat ["<section class=\"theorem-environment ",
                    t,
                    "\" id=\"",
                    t,
                    "-",
                    index,
                    "\">"]
        inittext = "<span class=\"theorem-header\">" ++ typetext ++ indextext ++ nametext ++ "</span>"
        typetext = "<span class=\"type\">" ++ t ++ "</span>" 
        indextext = if null index 
                     then "" 
                     else "<span class=\"index\">" ++ index ++ "</span>"
        nametext = if null name 
                     then "" 
                     else "<span class=\"name\">" ++ name ++ "</span>"
        end = "</section>"
        rawEnd = RawBlock "html" end
        rawStart = RawBlock "html" sectionhead
        rawHead = RawBlock "html" inittext
        content = (getDoc . readDoc) xs
        
makeTheorem x y = [x,y]

getDoc (Pandoc _ xs) = xs

isTheorem :: Block -> Bool
isTheorem (Header 6 (_, [], param) _) =
    if isJust t
      then (fromJust t) `elem` (incrementBlock ++ otherBlock)
      else False
  where t = lookup "type" param
isTheorem x = False
