{-# LANGUAGE Safe #-} 
module Assessed1Part2 where

-- We begin with a sample solution of part 1, produced by Cory.

import Data.Maybe
import Data.Char
import Data.Ord
import Data.List

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

readBit :: Char -> Bit
readBit '0' = Z
readBit '1' = I

readBits :: [Char] -> [Bit]
readBits = map readBit

decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (tree, bits) = decodeAux tree tree bits

decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux fullTree (Leaf c _) [] = [c]
decodeAux fullTree (Leaf c _) bs = c:(decodeAux fullTree fullTree bs) 
decodeAux fullTree (Branch left right _) (Z:bs) = decodeAux fullTree left bs
decodeAux fullTree (Branch left right _) (I:bs) = decodeAux fullTree right bs

{- The input String has the following format:

   * An integer n coded as a sequence of digits.

   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

decompress :: String -> String
decompress str = decode (t,bits)
    where
        (n',str') = span isDigit str
        n         = read n'
        t'        = take n str'
        t         = read t'
        bits      = readBits $ drop n str'

{- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding. -}

charlength :: Int
charlength = 8

-- gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*':s)   = s
decompress' s = decompress s

-- Generate the frequency table
-- An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

-- Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate = foldr update []

-- Removes the existing entry for c (if it exists), updates it, and
-- then reinserts it if no entry exists, we start over at 0, and then
-- "update"
update :: Eq c => c -> [Freq c] -> [Freq c]
update c keys = newFreq : rest
    where
        (old,rest) = (is c) `outOf` keys
        key = fromMaybe (c,0) old
        newFreq = mapSnd (+1) key

is :: Eq c => c -> Freq c -> Bool
is c (d,_) = c == d

outOf :: (a -> Bool) -> [a] -> (Maybe a,[a])
outOf p []     = (Nothing,[])
outOf p (x:xs) = if (p x) then (Just x,xs) else (mapSnd (x:) $ outOf p xs)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (c,a) = (c, f a)

{- End of part 1. Your tasks for part 2 begin here. -}

-- Produce a Huffman tree from a list of Huffman trees.
-- https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-- Question:
-- Collects a list of trees into an optimal prefix tree.
makeTree :: [Tree c] -> Tree c
makeTree [x] = x
makeTree (x:y:xs) = makeTree(sortListOfTrees ([(merge x y)] ++ xs))
   

-- You may wish to use a helper function such as this:
merge :: Tree c -> Tree c -> Tree c
merge (Branch left right freq) (Branch left2 right2 freq2) = Branch (Branch left right freq) (Branch left2 right2 freq2) (freq+freq2)
merge (Leaf c freq) (Leaf c2 freq2) = Branch (Leaf c freq) (Leaf c2 freq2) (freq+freq2)
merge (Leaf c freq) (Branch left right freq2) = Branch (Leaf c freq) (Branch left right freq2) (freq+freq2)
merge (Branch left right freq) (Leaf c freq2) = Branch (Branch left right freq) (Leaf c freq2) (freq+freq2)  

sortListOfTrees :: [Tree c] -> [Tree c]
sortListOfTrees [] = []
sortListOfTrees ((Leaf c x) : xs) = 
    let smallerSorted = sortListOfTrees [a | a <- xs, (freqOfTheTree a) <= x]  
        biggerSorted = sortListOfTrees [a | a <- xs, (freqOfTheTree a) > x]  
    in  smallerSorted ++ [(Leaf c x)] ++ biggerSorted
sortListOfTrees ((Branch l r x) : xs) =   
    let smallerSorted = sortListOfTrees [a | a <- xs, (freqOfTheTree a) <= x]  
        biggerSorted = sortListOfTrees [a | a <- xs, (freqOfTheTree a) > x]  
    in  smallerSorted ++ [(Branch l r x)] ++ biggerSorted

freqOfTheTree (Leaf c freq) = freq
freqOfTheTree (Branch left right freq) = freq

-- Question:
-- Generate a tree from list of Freqs (using makeTree above):
generateTree :: [Freq c] -> Tree c
generateTree xs = makeTree (toLeaves (sortListOfFreq xs))  

toLeaves [(c, freq)] = [(Leaf c freq)]
toLeaves ((c, freq):xs) = [(Leaf c freq)] ++ toLeaves xs

sortListOfFreq xs = Data.List.sortBy (Data.Ord.comparing snd) xs


-- Encoding table.
-- A key is a key-value pair (an entry in a map/table).
type Key c = (c,[Bit])

-- The whole coding table
type CodingTable c = [Key c]

-- Question:
-- Given a tree, generates a coding table
makeTable :: Eq c => Tree c -> CodingTable c
makeTable tree = makeTable2 tree []

makeTable2 (Leaf c freq) acc = [(c, acc)] 
makeTable2 (Branch left right freq) acc = makeTable2 left (acc++[Z]) ++ makeTable2 right (acc++[I])

 

-- Question:
-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable xs ys = concat [encodeUsingTable2 xs y | y <- ys]

encodeUsingTable2 ((c, zs):ys) x = if x==c then zs else encodeUsingTable2 ys x 

-- Question:
-- Encodes directly from the tree (more efficient).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing tree xs = concat [encodeUsing2 tree x []| x <- xs]

encodeUsing2 (Leaf c freq) x acc = if x==c then acc else [] 
encodeUsing2 (Branch left right freq) x acc = encodeUsing2 left x (acc ++ [Z]) ++ encodeUsing2 right x (acc ++ [I])


-- Question:
-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode xs = (tree, encodeUsing tree xs)  
	where
		tree = generateTree (tabulate xs)

-- Encoding trees

-- Question:
-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress xs = show (length (show tree)) ++ (show tree) ++ show (encodeUsing tree xs)
	where
		tree = generateTree (tabulate xs)

-- Question:
-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' xs = if ((8 * length (show (length (show (fst (encode xs)))))) + (8 * (length (show (fst (encode xs))))) + length (show (snd (encode xs))) ) >= 8 * (length xs) then ['*'] ++ xs else compress xs


uncompressedString = "US officials are investigating multiple attacks that caused widespread online disruption on both sides of the Atlantic on Friday.\n\nThe Department of Homeland Security has begun an investigation into the DDoS (distributed denial-of-service) attack, the Guardian confirmed.\n\nThe incident took offline some of the most popular sites on the web, including Netflix, Twitter, Spotify, Reddit, CNN, PayPal, Pinterest and Fox News - as well as newspapers including the Guardian, the New York Times and the Wall Street Journal.\n\nThe attacks seemed to have been focused on Dyn, the company that runs the internet's domain name system (DNS).\n\nAmazon's web services division, one of the world's biggest cloud computing companies, also reported an outage that lasted several hours on Friday morning.\nSign up to the new-look Media Briefing: bigger, better, brighter\nRead more\n\nDoug Madory, director of internet analysis at Dyn, said he was not sure if the outages at Dyn and Amazon were connected.\n\n'We provide service to Amazon, but theirs is a complex network so it is hard to be definitive about causality,' he said.\n\nAmazon was not available for comment.\n\nDyn said it first became aware of the attack shortly after 7am ET on Friday. 'We began monitoring and mitigating a DDoS [distributed denial-of-service] attack against our Dyn Managed DNS infrastructure,' the company said on its website.\n\nThe company sent out updates throughout the day, confirming a second attack at about noon and a third just after 4pm.\n\nDDoS attacks are also becoming more common. Brian Krebs, an independentsecurity researcher, observed earlier this month that the 'source code' to the Mirai botnet had been released by a hacker group, 'virtually guaranteeing that the internet will soon be flooded with attacks from many new botnets powered by insecure routers, IP cameras, digital video recorders and other easily hackable devices'US officials are investigating multiple attacks that caused widespread online disruption on both sides of the Atlantic on Friday.\n\nThe Department of Homeland Security has begun an investigation into the DDoS (distributed denial-of-service) attack, the Guardian confirmed.\n\nThe incident took offline some of the most popular sites on the web, including Netflix, Twitter, Spotify, Reddit, CNN, PayPal, Pinterest and Fox News - as well as newspapers including the Guardian, the New York Times and the Wall Street Journal.\n\nThe attacks seemed to have been focused on Dyn, the company that runs the internet's domain name system (DNS).\n\nAmazon's web services division, one of the world's biggest cloud computing companies, also reported an outage that lasted several hours on Friday morning.\nSign up to the new-look Media Briefing: bigger, better, brighter\nRead more\n\nDoug Madory, director of internet analysis at Dyn, said he was not sure if the outages at Dyn and Amazon were connected.\n\n'We provide service to Amazon, but theirs is a complex network so it is hard to be definitive about causality,' he said.\n\nAmazon was not available for comment.\n\nDyn said it first became aware of the attack shortly after 7am ET on Friday. 'We began monitoring and mitigating a DDoS [distributed denial-of-service] attack against our Dyn Managed DNS infrastructure,' the company said on its website.\n\nThe company sent out updates throughout the day, confirming a second attack at about noon and a third just after 4pm.\n\nDDoS attacks are also becoming more common. Brian Krebs, an independentsecurity researcher, observed earlier this month that the 'source code' to the Mirai botnet had been released by a hacker group, 'virtually guaranteeing that the internet will soon be flooded with attacks from many new botnets powered by insecure routers, IP cameras, digital video recorders and other easily hackable devices'US officials are investigating multiple attacks that caused widespread online disruption on both sides of the Atlantic on Friday.\n\nThe Department of Homeland Security has begun an investigation into the DDoS (distributed denial-of-service) attack, the Guardian confirmed.\n\nThe incident took offline some of the most popular sites on the web, including Netflix, Twitter, Spotify, Reddit, CNN, PayPal, Pinterest and Fox News - as well as newspapers including the Guardian, the New York Times and the Wall Street Journal.\n\nThe attacks seemed to have been focused on Dyn, the company that runs the internet's domain name system (DNS).\n\nAmazon's web services division, one of the world's biggest cloud computing companies, also reported an outage that lasted several hours on Friday morning.\nSign up to the new-look Media Briefing: bigger, better, brighter\nRead more\n\nDoug Madory, director of internet analysis at Dyn, said he was not sure if the outages at Dyn and Amazon were connected.\n\n'We provide service to Amazon, but theirs is a complex network so it is hard to be definitive about causality,' he said.\n\nAmazon was not available for comment.\n\nDyn said it first became aware of the attack shortly after 7am ET on Friday. 'We began monitoring and mitigating a DDoS [distributed denial-of-service] attack against our Dyn Managed DNS infrastructure,' the company said on its website.\n\nThe company sent out updates throughout the day, confirming a second attack at about noon and a third just after 4pm.\n\nDDoS attacks are also becoming more common. Brian Krebs, an independentsecurity researcher, observed earlier this month that the 'source code' to the Mirai botnet had been released by a hacker group, 'virtually guaranteeing that the internet will soon be flooded with attacks from many new botnets powered by insecure routers, IP cameras, digital video recorders and other easily asdasdasd"
