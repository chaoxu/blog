module MathDoc ( mathdoc ) where
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
mathdocWrite = def{writerHTMLMathMethod = MathJax ""}
readDoc :: String -> Pandoc
readDoc = readMarkdown mathdocRead

writeDoc :: Pandoc -> String
writeDoc = writeHtmlString mathdocWrite
--main :: IO ()
--main = interact (compute . formatTheorem)

mathdoc :: String->String
mathdoc = compute . formatTheorem

incrementBlock = ["Theorem",
                  "Conjecture",
                  "Definition",
                  "Example",
                  "Lemma",
                  "Problem"]
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
  where link = concat ["<a href=\"#",
                t ++ "-" ++ show i,
                "\">",
                t ++ " " ++ show i,
                "</a>"
                ]
        link2 = "[" ++ t ++ " " ++ show i ++ "]"
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
--compute2 x = (show $ bottomUp theoremize $ readDoc x) ++ "\n"
--compute3 x = (show $ readDoc x) ++ "\n"

latex :: Block -> Block
latex (RawBlock "latex" s) = RawBlock "html" s
latex x = x

theoremize :: [Block] -> [Block]
theoremize xs = t xs
  where t (x:y:xs)
         | isTheorem x = makeTheorem x y ++ (t xs)
         | otherwise   = x:(t (y:xs))
        t x = x

-- merger s t = s ++ v
--  where u = (lines s) 
--        v = dropWhile (==[]) (lines t)
makeTheorem (Header _ (_,_,parm) _) (CodeBlock _ xs) = 
  (getDoc . readDoc) (concat [divhead,ttext,indextext,nametext," "] ++ xs ++ end)
  -- [RawInline "html" $ unlines [divhead,ttext,indextext,nametext," "]]
  -- ++ (getDoc . readDoc) xs ++
  -- [RawInline "html" end]
  where t = fromJust $ lookup "type" parm
        name = fromJust $ lookup "name" parm
        index = fromJust $ lookup "index" parm
        divhead = concat ["<blockquote class=\"",
                    t,
                    "\" id=\"",
                    t,
                    "-",
                    index,
                    "\">"]
        ttext = if (null index) 
                   then "<span class=\"block_type\">" ++ t ++ ".</span>"
                   else "<span class=\"block_type\">" ++ t ++ "</span>"
        indextext = if (null name && (not $ null index))
                     then " <span class=\"block_index\">" ++ index ++ ".</span>"
                     else " <span class=\"block_index\">" ++ index ++ "</span>"
        nametext = if null name 
                     then "" 
                     else " <span class=\"block_title\">(" ++ name ++ ").</span>"
        end = if t == "Proof"
                 then "<span style=\"float:right;font-size:80%\">&#9632;</span></blockquote>"
                 else "</blockquote>"
makeTheorem x y = [x,y]

getDoc (Pandoc _ xs) = xs

isTheorem :: Block -> Bool
isTheorem (Header 6 (_, [], param) _) =
    if isJust t
      then (fromJust t) `elem` (incrementBlock ++ otherBlock)
      else False
  where t = lookup "type" param
isTheorem x = False
