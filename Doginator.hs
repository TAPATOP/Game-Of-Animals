module Main where
    import System.IO
    import Data.Char
    import System.Directory

    data DecisionTree = Animal String | Question String DecisionTree DecisionTree deriving (Read)

    instance Show DecisionTree where
        show (Animal str) = str
        show (Question q l r) = q ++ show l ++ show r

    detailedShow :: DecisionTree -> String
    detailedShow this@(Animal _) = "A:" ++ show this ++ "\n"
    detailedShow (Question q l r) = "Q:" ++ q ++ "\n" ++ detailedShow l ++ detailedShow r

    treePath, tempPath :: FilePath
    treePath = "db.txt"
    tempPath = "db_temp.txt"

    data Answer = Yes | No deriving Show

    main :: IO()
    main = do 
        -- hSetBuffering stdin NoBuffering
        tree <- loadTree treePath
        tree <- play tree
        saveTree tempPath $ detailedShow tree
        renameFile tempPath treePath
        return()

    loadTree :: FilePath -> IO DecisionTree
    loadTree path = do
        file <- openFile path ReadMode
        content <- hGetContents file
        let splitContent = lines content
        tree <- createTree splitContent
        hClose file
        return $ snd tree

    createTree :: [String] -> IO ([String], DecisionTree)
    createTree (line:remainingData) = do
        let prefix = head line : head (tail line) : []
        let rest = tail $ tail line
        case prefix of
            "A:" -> return (remainingData, (Animal rest))
            "Q:" -> do
                left <- createTree remainingData
                right <- createTree $ fst left
                return (fst right, (Question rest (snd left) (snd right)))
            _ -> return ([[]], Animal "Unformatted input")

    saveTree :: FilePath -> [Char] -> IO()
    saveTree file text = writeFile file text

    play :: DecisionTree -> IO DecisionTree
    play tree = do
        tree <- ask tree
        playAgain tree

    ask :: DecisionTree -> IO DecisionTree
    ask (Question text left right) = do
        print text
        response <- getResponse
        case response of
            Yes -> do nextQuestion <- ask left
                      return (Question text nextQuestion right)
            No -> do nextQuestion <- ask right
                     return (Question text left nextQuestion)

    ask this@(Animal name) = do 
        putStrLn ("I believe your animal is: " ++ name ++ ". \n \nPress 'y' if I'm correct and 'n' if I'm not")
        response <- getResponse
        case response of
            Yes -> do 
                putStrLn "Yay, I'm so smart\n"
                return this
            No -> addAnimal this

    addAnimal :: DecisionTree -> IO DecisionTree
    addAnimal otherAnimal = do
        putStrLn "Looks like I don't know what animal you're thinking about. Can you tell me its name, please?"
        newAnimalName <- getLine
        let decapitalizedAnimalName = lowerCase newAnimalName
        
        putStrLn $ "Thanks! Now, please give me a question, for which " ++ 
            decapitalizedAnimalName ++" answers with \"yes\" and " ++ 
            show otherAnimal ++ " answers with \"no\"."

        newQuestion <- getLine
        putStrLn "Thank you!"
        return $ Question newQuestion (Animal decapitalizedAnimalName) otherAnimal

    getResponse :: IO Answer
    getResponse = do
        input <- getLine
        getResponse' $ lowerCase input
    
    getResponse' :: [Char] -> IO Answer
    getResponse' resp
        | resp `elem` ["y", "yes", "ye", "sure"] = do 
            putStrLn ""
            return Yes
        | resp `elem` ["n", "no", "nah"] = do return No
        | otherwise = getResponse
        
    lowerCase :: [Char] -> [Char]
    lowerCase = map toLower

    playAgain :: DecisionTree -> IO DecisionTree
    playAgain tree = do
        putStrLn "Do you want to play again?"
        response <- getResponse
        case response of
            Yes -> play tree
            No -> return tree
