
import Data.List

type Strange = [Letter]

data Letter
  = M
  | I
  | U
  deriving (Show, Eq)


initial :: Strange
initial = [M, I]

goal :: Strange
goal = [M, U]

treeDepth :: Int
treeDepth = 4


-- Rules

ruleOne :: Strange -> Strange
ruleOne xs
  | last xs == I  = xs ++ [U]
  | otherwise = xs
ruleOne' x =  [ruleOne x]

ruleTwo :: Strange -> Strange
ruleTwo (M:xs) = M : xs ++ xs
ruleTwo x = x
ruleTwo' x = [ruleTwo x]


ruleThree :: Strange -> [Strange]
ruleThree x = replacePossiblities [I,I,I] [U] x

ruleFour :: Strange -> [Strange]
ruleFour x = replacePossiblities [U, U] [] x

ruleList :: [Strange -> [Strange]]
ruleList = [ruleOne', ruleTwo', ruleThree, ruleFour]



-- Test 

testMIU x = elem x $ propogateRules initial 5

propogateRules input iterator
  | iterator == 0 = result
  | otherwise = clean (map next result)
  where
    next x = propogateRules x (iterator - 1)
    applyRule rule = rule input
    result = clean $ map applyRule ruleList
    clean = concat . unique . removeEmpty


-- Listing

replacePossiblities :: Eq a => ([a]) -> [a] -> [a] -> [[a]]
replacePossiblities old new input = unique $ map splice here
  where
    splice x = replaceSection x (length old) new input
    here = findSublists old input

unique :: (Eq a) => [a] -> [a]
unique (x:xs) = x : unique (filter (/= x) xs)
unique [] = []

removeEmpty xs = filter (/= []) xs





replaceSection :: Int -> Int -> [a] -> [a] -> [a]
replaceSection begin end splice input =
  take begin input ++ splice ++ drop (begin + end) input


findSublists :: (Eq t1, Num t) => [t1] -> [t1] -> [t]
findSublists a b = findSublists' a b 0
  where
    findSublists' _ [] _ = []
    findSublists' occurance mainList iteration
      | occurance == mainListMatch = iteration : continue
      | otherwise = continue
      where
        mainListMatch = take (length occurance) mainList
        continue = findSublists' occurance (tail mainList) (iteration + 1)


replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst a b cs
  | a == c = b ++ s
  | otherwise = c ++ replaceFirst a b s
  where c = take (length a) cs
        s = drop (length a) cs

