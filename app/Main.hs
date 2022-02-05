module Main where

import Lib
import System.Random
import Control.Monad

main :: IO ()
main = do
    print "insert index of initial board"
    b_ <- getLine
    let b = read b_ :: Int
    print "insert number of iterations to chagne enviroment"
    n_ <- getLine
    let n = read n_ :: Int
    print "insert agent type (reactive or pro-active)"
    agent <- getLine
    if agent /= "reactive" && agent /= "pro-active" then print "Incorrect agent" 
    else do
        putStr "\n\n"
        initial_board <- selectInitialBoard b
        mainLoop n 1 agent initial_board

mainLoop :: Int -> Int -> String -> Board -> IO ()
mainLoop max n agent board = do
    let updated_board = moveRobots agent (robots board) board
    
    if n == max
        then do
            print "Change enviroment"
            let n = 1
            (new_b, new_kids, info) <- moveKids (kids updated_board) (b updated_board) [] []

            let dirts = inspectBoard (w updated_board -1) (h updated_board -1) "di" new_b [] ++ 
                        inspectBoard (w updated_board -1) (h updated_board -1) "rd" new_b [] ++
                        inspectBoard (w updated_board -1) (h updated_board -1) "rkd" new_b []

            let obstacles = inspectBoard (w updated_board -1) (h updated_board -1) "on" new_b []

            let new_board = Board {b=new_b, corrals=corrals updated_board, kids=new_kids,robots=robots updated_board, dirts=dirts, obstacles=obstacles, w=w updated_board, h=h updated_board}
            let b_ = indentedBoard (b new_board)
            print info
            putStr b_ 
            inp <- getLine    
            if inp == "abort"
                then print "Aborting"        
                else do 
                    let condition = checkForStop new_board
                    if condition == "continue" then do
                        mainLoop max n agent new_board
                        else print condition
        else do
            let new_n = n + 1
            let b_ = indentedBoard (b updated_board)
            putStr b_
            inp <- getLine    
            if inp == "abort"
                then print "Aborting"        
                else do
                    let condition = checkForStop updated_board
                    if condition == "continue" then do
                        mainLoop max new_n agent updated_board
                        else print condition

randomGen:: Int -> Int -> IO Int
randomGen l h = randomRIO (l, h)

-- Tis function generates a random Int with value -1, 0 or 1
randomGen1 :: IO Int
randomGen1 = do
    x <- randomGen 0 2
    case x of
        0 -> return 0
        1 -> return 1
        2 -> return (-1)
            


replaceAtPosition :: Int -> Int -> a -> [[a]] -> [[a]]
replaceAtPosition x y board_object board = let
    (x1,y1:ys1) = splitAt x board
    (x2,_:ys2) = splitAt y y1
    new_y = x2 ++ board_object: ys2
    result = x1 ++ new_y : ys1
    in result


-- ===========================================================================================================
-- This section contains the methods made to move the kids and his consecuences 

moveKids :: [(Int, Int)] -> [[BoardObject]] -> [(Int, Int)] -> [String] -> IO ([[BoardObject]], [(Int, Int)], [String])
moveKids [] board kids messages= return (board, kids, messages)
moveKids ((x,y):rest) board kids messages= do
    (new_board, (new_x, new_y)) <- moveKid (x,y) board
    let new_kids = (new_x, new_y):kids
    let message = "from " ++ show (x,y) ++ " to " ++ show (new_x,new_y)
    let new_messages = message:messages
    moveKids rest new_board new_kids new_messages

moveKid :: (Int, Int) -> [[BoardObject]] -> IO ([[BoardObject]], (Int, Int))
moveKid (x,y) board = do
    x_ <- randomGen1
    y_ <- randomGen1
    let new_x = x + x_
    let new_y = y + y_
    let kid_ty = typ ((board !! x) !! y)
    if new_x > 0 && new_x < length (head board) && new_y > 0 && new_y < length board && kid_ty == "ki"
        then do
            let target = (board !! new_x) !! new_y
            let ty = typ target
            (new_board, new_pos) <- tryToMoveKid ty (x,y) (new_x, new_y) board
            return (new_board, new_pos)
        else do
            return (board, (x,y))

tryToMoveKid :: String -> (Int, Int) -> (Int, Int) -> [[BoardObject]] -> IO ([[BoardObject]], (Int, Int))
tryToMoveKid ty (from_x,from_y) (to_x,to_y) board 
    | (from_x,from_y) == (to_x,to_y) = return (board, (from_x,from_y))
    | ty == "em" = do
        
        let n = checkAround (from_x, from_y) board
        let new_board_1 = replaceAtPosition to_x to_y (Kid to_x to_y "ki") board
        let new_board_2 = replaceAtPosition from_x from_y (Empty from_x from_y "em") new_board_1
        new_board_3 <- fillWithDirt n (from_x,from_y) new_board_2

        return (new_board_3, (to_x, to_y))                                     

    | ty == "ob" = do        
        let dir_x = to_x - from_x
        let dir_y = to_y - from_y
        let (new_board, pushed) = tryPush (to_x,to_y) (dir_x,dir_y) board
        
        if pushed
            then do 
                -- putStr "+++++"
                let n = checkAround (from_x, from_y) board
                let new_board_1 = replaceAtPosition to_x to_y (Kid to_x to_y "ki") new_board
                let new_board_2 = replaceAtPosition from_x from_y (Empty from_x from_y "em") new_board_1
                new_board_3 <- fillWithDirt n (from_x,from_y) new_board_2

                return (new_board_3, (to_x, to_y)) 
            else return (board, (from_x,from_y))

    | otherwise = return (board, (from_x,from_y))

tryPush :: (Int, Int) -> (Int, Int) -> [[BoardObject]] -> ([[BoardObject]], Bool)
tryPush (x,y) (dir_x, dir_y) board = let
    ty = if x > 0 && x < length (head board) && y > 0 && y < length board
        then typ ((board !! x) !! y)
        else "null"
    
    in
        case ty of
            "em" -> (replaceAtPosition x y (Obstacle x y "ob") board, True)
            "ob" -> tryPush (x+dir_x,y+dir_y) (dir_x,dir_y) board
            _ -> (board, False)

checkAround :: (Int,Int) -> [[BoardObject]] -> Int
checkAround (x,y) board = let
    n1 = if (x-1) > 0 && (x-1) < length (head board) && y > 0 && y < length board && typ ((board !! (x-1)) !! y) == "ki"
        then 1
        else 0
    
    n2 = if (x+1) > 0 && (x+1) < length (head board) && y > 0 && y < length board && typ ((board !! (x+1)) !! y) == "ki"
        then 1
        else 0

    n3 = if x > 0 && x < length (head board) && (y+1) > 0 && (y+1) < length board && typ ((board !! x) !! (y+1)) == "ki"
        then 1
        else 0

    n4 = if x > 0 && x < length (head board) && (y-1) > 0 && (y-1) < length board && typ ((board !! x) !! (y-1)) == "ki"
        then 1
        else 0

    n5 = if (x+1) > 0 && (x+1) < length (head board) && (y+1) > 0 && (y+1) < length board && typ ((board !! (x+1)) !! (y+1)) == "ki"
        then 1
        else 0

    n6 = if (x+1) > 0 && (x+1) < length (head board) && (y-1) > 0 && (y-1) < length board && typ ((board !! (x+1)) !! (y-1)) == "ki"
        then 1
        else 0

    n7 = if (x-1) > 0 && (x-1) < length (head board) && (y+1) > 0 && (y+1) < length board && typ ((board !! (x-1)) !! (y+1)) == "ki"
        then 1
        else 0

    n8 = if (x-1) > 0 && (x-1) < length (head board) && (y-1) > 0 && (y-1) < length board && typ ((board !! (x-1)) !! (y-1)) == "ki"
        then 1
        else 0
    
    n = n1+n2+n3+n4+n5+n6+n7+n8
    in
        case n of 
            0 -> 1
            1 -> 3
            _ -> 6
    
fillWithDirt :: Int -> (Int, Int) -> [[BoardObject]] -> IO [[BoardObject]]
fillWithDirt 0 _ board = return board
fillWithDirt n (x,y) board = do
    x_ <- randomGen1
    y_ <- randomGen1
    let new_x = x + x_
    let new_y = y + y_
    if new_x > 0 && new_x < length (head board) && new_y > 0 && new_y < length board && typ ((board !! new_x) !! new_y) == "em"
        then do
            let new_board = replaceAtPosition new_x new_y (Dirt new_x new_y "di") board
            fillWithDirt (n-1) (x,y) new_board
        else do fillWithDirt (n-1) (x,y) board

inspectBoard :: Int -> Int -> String -> [[BoardObject]] -> [(Int,Int)] -> [(Int,Int)]
inspectBoard (-1) _ _ _ matches = matches
inspectBoard x y typ board matches =
    let
        new_matches = inspectRow x y typ (board !! x) []
        result = matches ++ new_matches
    in
        inspectBoard (x-1) y typ board result


inspectRow :: Int -> Int -> String -> [BoardObject] -> [(Int,Int)] -> [(Int,Int)]
inspectRow _ (-1) _ _ matches = matches
inspectRow x y t row matches =
    let
        target_t = typ (row !! y)
    in
        if target_t == t
            then inspectRow x (y-1) t row ((x,y):matches)
            else inspectRow x (y-1) t row matches

-- ===========================================================================================================

-- ===========================================================================================================
-- In this section are the functions made to move the agents
    
moveRobots :: String -> [(Int, Int)] -> Board -> Board
moveRobots agent [] board = board
moveRobots agent ((x,y):left_robots) board = let
    ty = typ ((b board !! x) !! y)
    new_b = robotMakeDecision agent (x,y) ty board
    dirts = inspectBoard (w board -1) (h board -1) "di" new_b [] ++ 
                inspectBoard (w board -1) (h board -1) "rd" new_b [] ++
                inspectBoard (w board -1) (h board -1) "rkd" new_b []

    obstacles = inspectBoard (w board -1) (h board -1) "on" new_b []

    robots = inspectBoard (w board -1) (h board -1) "ro" new_b [] ++
                inspectBoard (w board -1) (h board -1) "rd" new_b [] ++
                inspectBoard (w board -1) (h board -1) "rk" new_b [] ++
                inspectBoard (w board -1) (h board -1) "rkd" new_b [] ++
                inspectBoard (w board -1) (h board -1) "cr" new_b [] ++
                inspectBoard (w board -1) (h board -1) "ckr" new_b [] ++
                inspectBoard (w board -1) (h board -1) "ckrr" new_b []

    kids = inspectBoard (w board -1) (h board -1) "ki" new_b []
    
    new_board = Board {b=new_b, corrals=corrals board, kids=kids, robots=robots, dirts=dirts, obstacles=obstacles, w=w board, h=h board}
    
    in
        moveRobots agent left_robots new_board

robotMakeDecision :: String -> (Int,Int) -> String -> Board -> [[BoardObject]]
robotMakeDecision agent (x,y) ty board 
    | agent == "reactive" = 
    let
        distances = bfs (x,y) [(0,[],(x,y))] (b board) (replicate (h board) (replicate (w board) (-1, [])))
    
        (to_x, to_y) = case ty of
            "ro" ->
                let 
                    (nearest_kid_distance, nearest_kid_path) = searchForNearestRestricted "ki" (kids board) distances (b board) (-1, [])
                    (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])

                in 
                    if nearest_kid_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_dirt_distance == -1 ||  (nearest_kid_distance > -1 && nearest_kid_distance < nearest_dirt_distance + 2)
                                then nearest_kid_path !! 1
                                else nearest_dirt_path !! 1            

            "rk" -> let
                (nearest_corral_distance, nearest_corral_path) = searchForNearestRestricted "ec" (corrals board) distances (b board) (-1, [])
                (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])
                in 
                    if nearest_corral_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_dirt_distance == -1 ||  (nearest_corral_distance > -1 && nearest_corral_distance < nearest_dirt_distance)
                                then if length nearest_corral_path > 2 then nearest_corral_path !! 2 else nearest_corral_path !! 1
                                else if length nearest_dirt_path > 2 then nearest_dirt_path !! 2 else nearest_dirt_path !! 1  

            "rd" -> (x,y)

            "rkd" -> (x,y)

            "cr" ->
                let 
                    (nearest_kid_distance, nearest_kid_path) = searchForNearestRestricted "ki" (kids board) distances (b board) (-1, [])
                    (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])

                in 
                    if nearest_kid_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_dirt_distance == -1 ||  (nearest_kid_distance > -1 && nearest_kid_distance < nearest_dirt_distance)
                                then nearest_kid_path !! 1
                                else nearest_dirt_path !! 1    

            "ckr" -> let
                (target_n, (target_x, target_y)) = searchForTargetCorral (x,y) (corrals board) (b board) (8, (-1,-1))
                (corral_distance, corral_path) = (distances !! target_x) !! target_y
                
                in
                    if corral_distance == -1 || corral_distance == 0
                        then (x,y)
                        else if length corral_path > 2 then corral_path !! 2 else corral_path !! 1

            "ckrr" ->
                let 
                    (nearest_kid_distance, nearest_kid_path) = searchForNearestRestricted "ki" (kids board) distances (b board) (-1, [])
                    (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])

                in 
                    if nearest_kid_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_dirt_distance == -1 ||  (nearest_kid_distance > -1 && nearest_kid_distance < nearest_dirt_distance)
                                then if length nearest_kid_path > 2 then nearest_kid_path !! 2 else nearest_kid_path !! 1
                                else if length nearest_dirt_path > 2 then nearest_dirt_path !! 2 else nearest_dirt_path !! 1     
            _ -> (x,y)

        (from_object, to_object) = updateTypes (x,y) (to_x, to_y) (b board)
        new_board_1 = replaceAtPosition x y from_object (b board)
        new_board_2 = replaceAtPosition to_x to_y to_object new_board_1

    in
        new_board_2
    | agent == "pro-active" = 
    let
        distances = bfs (x,y) [(0,[],(x,y))] (b board) (replicate (h board) (replicate (w board) (-1, [])))
    
        (to_x, to_y) = case ty of
            "ro" ->
                let 
                    (nearest_kid_distance, nearest_kid_path) = searchForNearestRestricted "ki" (kids board) distances (b board) (-1, [])
                    (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])

                in 
                    if nearest_kid_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_kid_distance /= -1
                                then nearest_kid_path !! 1
                                else nearest_dirt_path !! 1            

            "rk" -> let
                (nearest_corral_distance, nearest_corral_path) = searchForNearestRestricted "ec" (corrals board) distances (b board) (-1, [])
                (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])
                in 
                    if nearest_corral_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_corral_distance /= -1
                                then if length nearest_corral_path > 2 then nearest_corral_path !! 2 else nearest_corral_path !! 1
                                else if length nearest_dirt_path > 2 then nearest_dirt_path !! 2 else nearest_dirt_path !! 1  

            "rd" -> (x,y)

            "rkd" -> (x,y)

            "cr" ->
                let 
                    (nearest_kid_distance, nearest_kid_path) = searchForNearestRestricted "ki" (kids board) distances (b board) (-1, [])
                    (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])

                in 
                    if nearest_kid_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_kid_distance /= -1 
                                then nearest_kid_path !! 1
                                else nearest_dirt_path !! 1    

            "ckr" -> let
                (target_n, (target_x, target_y)) = searchForTargetCorral (x,y) (corrals board) (b board) (8, (-1,-1))
                (corral_distance, corral_path) = (distances !! target_x) !! target_y
                
                in
                    if corral_distance == -1 || corral_distance == 0
                        then (x,y)
                        else if length corral_path > 2 then corral_path !! 2 else corral_path !! 1

            "ckrr" ->
                let 
                    (nearest_kid_distance, nearest_kid_path) = searchForNearestRestricted "ki" (kids board) distances (b board) (-1, [])
                    (nearest_dirt_distance, nearest_dirt_path) = searchForNearestRestricted "di" (dirts board) distances (b board) (-1, [])

                in 
                    if nearest_kid_distance == (-1) && nearest_dirt_distance == (-1)
                        then (x,y)
                        else
                            if nearest_kid_distance /= -1 
                                then if length nearest_kid_path > 2 then nearest_kid_path !! 2 else nearest_kid_path !! 1
                                else if length nearest_dirt_path > 2 then nearest_dirt_path !! 2 else nearest_dirt_path !! 1     
            _ -> (x,y)

        (from_object, to_object) = updateTypes (x,y) (to_x, to_y) (b board)
        new_board_1 = replaceAtPosition x y from_object (b board)
        new_board_2 = replaceAtPosition to_x to_y to_object new_board_1

    in
        new_board_2

    | otherwise = b board

updateTypes :: (Int, Int) -> (Int, Int) -> [[BoardObject]] -> (BoardObject, BoardObject)
updateTypes (from_x, from_y) (to_x, to_y) board =
    if from_x /= to_x || from_y /= to_y then
        let 
        from_ty = typ ((board !! from_x) !! from_y)
        to_ty = typ ((board !! to_x) !! to_y)

        in case (from_ty, to_ty) of
            ("ro", "em") -> (Empty from_x from_y "em", Robot to_x to_y "ro")
            ("ro", "ec") -> (Empty from_x from_y "em", CorralAndRobot to_x to_y "cr")
            ("ro", "di") -> (Empty from_x from_y "em", RobotAndDirt to_x to_y "rd")
            ("ro", "ki") -> (Empty from_x from_y "em", RobotAndKid to_x to_y "rk")

            ("cr", "em") -> (EmptyCorral from_x from_y "ec", Robot to_x to_y "ro")
            ("cr", "ec") -> (EmptyCorral from_x from_y "ec", CorralAndRobot to_x to_y "cr")
            ("cr", "di") -> (EmptyCorral from_x from_y "ec", RobotAndDirt to_x to_y "rd")
            ("cr", "ki") -> (EmptyCorral from_x from_y "ec", RobotAndKid to_x to_y "rk")

            ("rd", "em") -> (Dirt from_x from_y "di", Robot to_x to_y "ro")
            ("rd", "ec") -> (Dirt from_x from_y "di", CorralAndKidAndRobot to_x to_y "cr")
            ("rd", "di") -> (Dirt from_x from_y "di", RobotAndDirt to_x to_y "rd")
            ("rd", "ki") -> (Dirt from_x from_y "di", RobotAndKid to_x to_y "rk")

            ("rk", "em") -> (Empty from_x from_y "em", RobotAndKid to_x to_y "rk")
            ("rk", "ec") -> (Empty from_x from_y "em", CorralAndKidAndRobot to_x to_y "ckr")
            ("rk", "di") -> (Empty from_x from_y "em", RobotAndKidAndDirt to_x to_y "rkd")

            ("rkd", "em") -> (Dirt from_x from_y "di", RobotAndKid to_x to_y "rk")
            ("rkd", "ec") -> (Dirt from_x from_y "di", CorralAndKidAndRobot to_x to_y "ckr")
            ("rkd", "di") -> (Dirt from_x from_y "di", RobotAndKidAndDirt to_x to_y "rkd")

            ("ckr", "em") -> (EmptyCorral from_x from_y "ec", RobotAndKid to_x to_y "rk")
            ("ckr", "ec") -> (EmptyCorral from_x from_y "ec", CorralAndKidAndRobot to_x to_y "ckr")
            ("ckr", "di") -> (EmptyCorral from_x from_y "ec", RobotAndKidAndDirt to_x to_y "rkd")

            ("ckrr", "em") -> (CorralAndKid from_x from_y "ck", Robot to_x to_y "ro")
            ("ckrr", "ec") -> (CorralAndKid from_x from_y "ck", CorralAndRobot to_x to_y "cr")
            ("ckrr", "di") -> (CorralAndKid from_x from_y "ck", RobotAndDirt to_x to_y "rd")

            (_, _) -> (Empty from_x from_y "em", Empty to_x to_y "em")

        else
            let ty = typ ((board !! from_x) !! from_y)

            in case ty of
                "ro" -> (Robot from_x from_y "ro", Robot from_x from_y "ro")
                "rd" -> (Robot from_x from_y "ro", Robot from_x from_y "ro")
                "cr" -> (CorralAndRobot from_x from_y "cr", CorralAndRobot from_x from_y "cr")
                "rkd" -> (RobotAndKid from_x from_y "rk", RobotAndKid from_x from_y "rk")
                "rk" -> (RobotAndKid from_x from_y "rk", RobotAndKid from_x from_y "rk")
                "ckr" -> (CorralAndKidAndRobotRelease from_x from_y "ckrr", CorralAndKidAndRobotRelease from_x from_y "ckrr")
                "ckrr" -> (CorralAndKidAndRobotRelease from_x from_y "ckrr", CorralAndKidAndRobotRelease from_x from_y "ckrr")
                _ -> (Empty from_x from_y "em", Empty from_x from_y "em")

searchForNearest :: [(Int,Int)] -> [[(Int, [(Int,Int)])]] -> (Int, [(Int,Int)])-> (Int, [(Int,Int)])
searchForNearest [] _ result = result
searchForNearest ((x,y):targets) distances (n, path) = 
    let
        (target_n, target_path) = (distances !! x) !! y
    
    in
        if target_n > (-1) && (target_n < n || n == (-1))
            then searchForNearest targets distances (target_n, target_path)
            else searchForNearest targets distances (n,path)


searchForNearestRestricted :: String -> [(Int,Int)] -> [[(Int, [(Int,Int)])]] -> [[BoardObject]] -> (Int, [(Int,Int)])-> (Int, [(Int,Int)])
searchForNearestRestricted _ [] _ _ result = result
searchForNearestRestricted ty ((x,y):targets) distances board (n, path) = 
    let
        (target_n, target_path) = (distances !! x) !! y
        target_ty = typ ((board !! x) !! y)
    
    in
        if target_n > (-1) && (target_n < n || n == (-1)) && ty == target_ty
            then searchForNearestRestricted ty targets distances board (target_n, target_path)
            else searchForNearestRestricted ty targets distances board (n,path)


searchForTargetCorral :: (Int,Int) -> [(Int,Int)] -> [[BoardObject]] -> (Int, (Int,Int)) -> (Int, (Int, Int))
searchForTargetCorral _ [] _ result = result
searchForTargetCorral (actual_x, actual_y) ((x,y):rest) board (best, result) = let
    n1 = if (x-1) > 0 && (x-1) < length (head board) && y > 0 && y < length board && 
        typ ((board !! (x-1)) !! y) /= "ck" && typ ((board !! (x-1)) !! y) /= "ob"
        then 1
        else 0
    
    n2 = if (x+1) > 0 && (x+1) < length (head board) && y > 0 && y < length board && 
        typ ((board !! (x+1)) !! y) /= "ck" && typ ((board !! (x+1)) !! y) /= "ob"
        then 1
        else 0

    n3 = if x > 0 && x < length (head board) && (y+1) > 0 && (y+1) < length board && 
        typ ((board !! x) !! (y+1)) /= "ck" && typ ((board !! x) !! (y+1)) /= "ob"
        then 1
        else 0

    n4 = if x > 0 && x < length (head board) && (y-1) > 0 && (y-1) < length board && 
        typ ((board !! x) !! (y-1)) /= "ck" && typ ((board !! x) !! (y-1)) /= "ob"
        then 1
        else 0

    n5 = if (x+1) > 0 && (x+1) < length (head board) && (y+1) > 0 && (y+1) < length board && 
        typ ((board !! (x+1)) !! (y+1)) /= "ck" && typ ((board !! (x+1)) !! (y+1)) /= "ob"
        then 1
        else 0

    n6 = if (x+1) > 0 && (x+1) < length (head board) && (y-1) > 0 && (y-1) < length board &&
        typ ((board !! (x+1)) !! (y-1)) /= "ck" && typ ((board !! (x+1)) !! (y-1)) /= "ob"
        then 1
        else 0

    n7 = if (x-1) > 0 && (x-1) < length (head board) && (y+1) > 0 && (y+1) < length board && 
        typ ((board !! (x-1)) !! (y+1)) /= "ck" && typ ((board !! (x-1)) !! (y+1)) /= "ob"
        then 1
        else 0

    n8 = if (x-1) > 0 && (x-1) < length (head board) && (y-1) > 0 && (y-1) < length board && 
        typ ((board !! (x-1)) !! (y-1)) /= "ck" && typ ((board !! (x-1)) !! (y-1)) /= "ob"
        then 1
        else 0
    
    n = n1+n2+n3+n4+n5+n6+n7+n8

    in
        if (typ ((board !! x) !! y) == "ec" || (x,y) == (actual_x,actual_y)) && n <= best
            then searchForTargetCorral (actual_x,actual_y) rest board (n, (x,y))
            else searchForTargetCorral (actual_x,actual_y) rest board (best, result)


checkForStop :: Board -> String
checkForStop board = let
    dirt_condition = fromIntegral (h board * w board) *  60 /  100
    
    in
        if dirt_condition <= fromIntegral (length (dirts board))
            then "Dirt reached 60% of board"
            else if null (kids board) && null (dirts board)
                then "All clean"
                else "continue"
    
checkEmptyCorrals :: [(Int,Int)] -> [[BoardObject]] -> Int -> Int
checkEmptyCorrals [] _ n = n
checkEmptyCorrals ((x,y):rest) board n = 
    if typ ((board !! x) !! y) /= "ck" && typ ((board !! x) !! y) /= "ckrr"
        then checkEmptyCorrals rest board (n+1)
        else checkEmptyCorrals rest board n

-- ===========================================================================================================

-- ===========================================================================================================
-- BFS section

bfs :: (Int, Int) -> [(Int, [(Int,Int)], (Int,Int))] -> [[BoardObject]] -> [[(Int, [(Int,Int)])]] -> [[(Int, [(Int,Int)])]]
bfs _ [] _ result = result
bfs (initial_x, initial_y) stack board result = let
    (n,path,(x,y)) = head stack
    in
    if  x >= 0 && x < length (head board) 
        && y >= 0 && y < length board && 
        (fst ((result !! x) !! y) == -1)
        then
            let
                new_path = path ++ [(x,y)] 
                new_result = replaceAtPosition x y (n, new_path) result

                (up_x, up_y) = (x,y-1)
                (ri_x, ri_y) = (x+1,y)
                (da_x, da_y) = (x,y+1)
                (le_x,le_y) = (x-1,y)

                new_stack = if (initial_x == x && initial_y == y) || (typ ((board !! x) !! y) == "em" || typ ((board !! x) !! y) == "ec" || typ ((board !! x) !! y) == "di")
                    then tail stack ++ [(n+1, new_path, (up_x, up_y)), (n+1,new_path,(ri_x, ri_y)), (n+1,new_path,(da_x, da_y)), (n+1,new_path,(le_x,le_y))]                            
                    else tail stack
            in
                bfs (initial_x, initial_y) new_stack board new_result
        else bfs (initial_x, initial_y) (tail stack) board result

indentedBfs :: [[(Int,[(Int,Int)])]] -> String
indentedBfs = foldr ((++) . indentedBfsRow) "\n"

indentedBfsRow :: [(Int,[(Int,Int)])] -> String
indentedBfsRow [] = "\n"
indentedBfsRow ((x,way):row) = show x ++ concat (replicate (14 - length (show x)) " ") ++ indentedBfsRow row

-- =================================================================================================

-- =================================================================================================
-- In this section are created the Board and BoardObject types

data Board = Board {b :: [[BoardObject]], w :: Int, h :: Int,
                    obstacles :: [(Int,Int)],
                    kids :: [(Int,Int)],
                    corrals :: [(Int, Int)],
                    robots :: [(Int,Int)],
                    dirts :: [(Int,Int)]
                    } deriving (Show)


data BoardObject = Empty {x :: Int, y :: Int, typ :: String}  -- em
                | Obstacle {x :: Int, y :: Int, typ :: String}  -- ob
                | Dirt {x :: Int, y :: Int, typ :: String} -- di
                | EmptyCorral {x :: Int, y :: Int, typ :: String}  -- ec
                | CorralAndKid {x :: Int, y :: Int, typ :: String} -- ck
                | CorralAndKidAndRobot {x :: Int, y :: Int, typ :: String} -- ckr
                | CorralAndRobot {x :: Int, y :: Int, typ :: String} -- cr
                | Kid {x :: Int, y :: Int, typ :: String}  -- ki
                | Robot {x :: Int, y :: Int, typ :: String}  -- ro
                | RobotAndKid {x :: Int, y :: Int, typ :: String}  -- rk     
                | CorralAndKidAndRobotRelease {x :: Int, y :: Int, typ :: String}  -- ckrr
                | RobotAndDirt {x :: Int, y :: Int, typ :: String}  -- rd
                | RobotAndKidAndDirt {x :: Int, y :: Int, typ :: String} -- rkd
                deriving (Eq)



indentedBoard :: [[BoardObject]] -> String
indentedBoard = foldr ((++) . indentedRow) "\n"

indentedRow :: [BoardObject] -> String
indentedRow [] = "\n"
indentedRow (x:row) = show x ++ concat (replicate (14 - length (show x)) " ") ++ indentedRow row



instance Show BoardObject where
    show (Empty x y t) = "Empty"
    show (Obstacle x y t) = "(Obstacle)"
    show (Dirt x y t) = "~~Dirt~~"
    show (EmptyCorral x y t) = "{Corral}"
    show (CorralAndKid x y t) = "{--C+K--}"
    show (CorralAndRobot x y t) = "{[C+R]}"
    show (CorralAndKidAndRobot x y t) = "{[--C+K+R--]}"
    show (Kid x y t) =  show x ++ "--Kid--"  ++ show y
    show (Robot x y t) = "[Robot]"
    show (CorralAndKidAndRobotRelease x y t) = "{[--C+R=>K--]}"
    show (RobotAndDirt x y t) = "[~~R+D~~]"
    show (RobotAndKid x y t) = "[--R+K--]"
    show (RobotAndKidAndDirt x y t) = "[--~~R+K+D--~~]"

-- ===========================================================================================================

-- ===========================================================================================================
-- Board generation section

selectInitialBoard :: Int -> IO Board
selectInitialBoard n 
                    | n <= 0 = do -- generate a random board (8x8)
    let new_board = fillEmpty 8 8 [[]]
    new_board_1 <- fillCorrals 8 8 8 new_board
    let corrals = inspectBoard 8 8 "ec" new_board_1 []

    (new_board_2, kids) <- fillKids 8 8 8 new_board_1 []

    (new_board_3, robots) <- fillRobots 1 8 8 new_board_2 []

    (new_board_4, obstacles) <- fillObstacles 5 8 8 new_board_3 []

    (new_board_5, dirts) <- fillDirts 4 8 8 new_board_4 []

    return Board {b = new_board_5, corrals=corrals, kids = kids, robots = robots, obstacles = obstacles, dirts = dirts, w = 7, h = 8} 

                    | n == 1 = return Board {b = [
    [EmptyCorral 0 0 "ec",  EmptyCorral 0 1 "ec", Empty 0 2 "em",    Empty 0 3 "em",      Kid 0 4 "ki", Empty 0 5 "em", Empty 0 6 "em"],
    [EmptyCorral 1 0 "ec", CorralAndKid 1 1 "ec", Empty 1 2 "em", Obstacle 1 3 "ob",    Empty 1 4 "em", Empty 1 5 "em", Empty 1 6 "em"],
    [EmptyCorral 2 0 "ec",  EmptyCorral 2 1 "ec", Empty 2 2 "em",    Empty 2 3 "em",    Empty 2 4 "em", Empty 2 5 "em", Empty 2 6 "em"],
    [Empty 3 0 "em",               Dirt 3 1 "di", Empty 3 2 "em",      Kid 3 3 "ki",      Kid 3 4 "ki", Empty 3 5 "em", Empty 3 6 "em"],
    [Empty 4 0 "em",                Kid 4 1 "ki",  Dirt 4 2 "di",    Empty 4 3 "em",    Empty 4 4 "em",   Kid 4 5 "ki", Empty 4 6 "em"],
    [Empty 5 0 "em",              Empty 5 1 "em",  Dirt 5 2 "di",    Empty 5 3 "em",    Empty 5 4 "em",   Kid 5 5 "ki", Empty 5 6 "em"],
    [Empty 6 0 "em",              Empty 6 1 "em", Empty 6 2 "em", Obstacle 6 3 "ob", Obstacle 6 4 "ob", Empty 6 5 "em", Robot 6 6 "ro"]
    ], w =7, h = 7,
    obstacles = [(1,3), (6,3), (6,4)],
    kids = [(1,1), (0,4), (3,3), (3,4), (4,5), (5,5)],
    corrals = [(0,0), (1,0), (2,0), (0,1), (1,1), (2,1)],
    robots = [(6,6)],
    dirts = [(4,2), (5,2), (3,1)]
    }
                    | n == 2 = return Board {b = [
    [EmptyCorral 0 0 "ec",  EmptyCorral 0 1 "ec", Empty 0 2 "em", Empty 0 3 "em",    Empty 0 4 "em",    Empty 0 5 "em",    Empty 0 6 "em"],
    [EmptyCorral 1 0 "ec",  EmptyCorral 1 1 "ec", Empty 1 2 "em", Obstacle 1 3 "ob", Obstacle 1 4 "ob", Obstacle 1 5 "ob", Empty 1 6 "em"],
    [EmptyCorral 2 0 "ec",  EmptyCorral 2 1 "ec", Empty 2 2 "em", Obstacle 2 3 "ob", Kid 2 4 "ki",      Obstacle 2 5 "ob", Empty 2 6 "em"],
    [Empty 3 0 "em",               Dirt 3 1 "di", Empty 3 2 "em", Obstacle 3 3 "ob", Obstacle 3 4 "ob", Obstacle 3 5 "ob", Empty 3 6 "em"],
    [Empty 4 0 "em",              Empty 4 1 "em",  Dirt 4 2 "di", Empty 4 3 "em",    Empty 4 4 "em",    Empty 4 5 "em",    Empty 4 6 "em"],
    [Empty 5 0 "em",              Empty 5 1 "em",  Dirt 5 2 "di", Empty 5 3 "em",    Empty 5 4 "em",    Empty 5 5 "em",    Empty 5 6 "em"],
    [Empty 6 0 "em",              Empty 6 1 "em", Empty 6 2 "em", Obstacle 6 3 "ob", Obstacle 6 4 "ob", Empty 6 5 "em",    Robot 6 6 "ro"]
    ], w =7, h = 7,
    obstacles = [(1,3), (1,4), (1,5), (2,3), (2,5), (3,3), (3,4), (3,5), (6,3), (6,4)],
    kids = [(2,4)],
    corrals = [(0,0), (1,0), (2,0), (0,1), (1,1), (2,1)],
    robots = [(6,6)],
    dirts = [(4,2), (5,2), (3,1)]
    }

                    | n == 3 = return Board {b = [
    [EmptyCorral 0 0 "ec",  EmptyCorral 0 1 "ec", Empty 0 2 "em", Empty 0 3 "em",    Empty 0 4 "em",    Empty 0 5 "em",      Kid 0 6 "ki"],
    [EmptyCorral 1 0 "ec",  EmptyCorral 1 1 "ec", Empty 1 2 "em", Obstacle 1 3 "ob", Obstacle 1 4 "ob", Obstacle 1 5 "ob", Empty 1 6 "em"],
    [EmptyCorral 2 0 "ec",  EmptyCorral 2 1 "ec", Empty 2 2 "em", Empty 2 3 "em",    Kid 2 4 "ki",      Empty 2 5 "em",    Empty 2 6 "em"],
    [Empty 3 0 "em",               Dirt 3 1 "di", Empty 3 2 "em", Empty 3 3 "em",    Empty 3 4 "em",    Empty 3 5 "em",    Empty 3 6 "em"],
    [Empty 4 0 "em",                Kid 4 1 "ki",  Dirt 4 2 "di", Empty 4 3 "em",    Empty 4 4 "em",    Empty 4 5 "em",    Empty 4 6 "em"],
    [Empty 5 0 "em",              Empty 5 1 "em",  Dirt 5 2 "di", Empty 5 3 "em",      Kid 5 4 "ki",    Empty 5 5 "em",    Empty 5 6 "em"],
    [Kid 6 0 "ki",                  Kid 6 1 "ki", Empty 6 2 "em", Obstacle 6 3 "ob", Empty 6 4 "em",    Empty 6 5 "em",    Robot 6 6 "ro"]
    ], w =7, h = 7,
    obstacles = [(1,3), (1,4), (1,5), (6,3)],
    kids = [(2,4), (4,1), (6,0), (6,1), (5,4), (6,0)],
    corrals = [(0,0), (1,0), (2,0), (0,1), (1,1), (2,1)],
    robots = [(6,6)],
    dirts = [(4,2), (5,2), (3,1)]
    }
                    | n == 4 = return Board {b = [
    [EmptyCorral 0 0 "ec",  EmptyCorral 0 1 "ec", Empty 0 2 "em", Empty 0 3 "em",    Empty 0 4 "em",    Empty 0 5 "em",      Kid 0 6 "ki"],
    [EmptyCorral 1 0 "ec",  EmptyCorral 1 1 "ec", Empty 1 2 "em", Obstacle 1 3 "ob", Obstacle 1 4 "ob", Obstacle 1 5 "ob", Empty 1 6 "em"],
    [EmptyCorral 2 0 "ec",  EmptyCorral 2 1 "ec", Empty 2 2 "em", Empty 2 3 "em",    Kid 2 4 "ki",      Empty 2 5 "em",    Empty 2 6 "em"],
    [Empty 3 0 "em",               Dirt 3 1 "di", Empty 3 2 "em", Empty 3 3 "em",    Empty 3 4 "em",    Empty 3 5 "em",    Empty 3 6 "em"],
    [Empty 4 0 "em",                Kid 4 1 "ki",  Dirt 4 2 "di", Empty 4 3 "em",    Empty 4 4 "em",    Empty 4 5 "em",    Empty 4 6 "em"],
    [Empty 5 0 "em",              Empty 5 1 "em",  Dirt 5 2 "di", Empty 5 3 "em",      Kid 5 4 "ki",    Empty 5 5 "em",    Empty 5 6 "em"],
    [Kid 6 0 "ki",                  Kid 6 1 "ki", Robot 6 2 "ro", Obstacle 6 3 "ob", Empty 6 4 "em",    Empty 6 5 "em",    Robot 6 6 "ro"]
    ], w =7, h = 7,
    obstacles = [(1,3), (1,4), (1,5), (6,3)],
    kids = [(2,4), (4,1), (6,0), (6,1), (5,4), (6,0)],
    corrals = [(0,0), (1,0), (2,0), (0,1), (1,1), (2,1)],
    robots = [(6,6), (6,2)],
    dirts = [(4,2), (5,2), (3,1)]
    }

                    | otherwise = -- generates an empty board
                        return Board {b = [
    [Empty 0 0 "em", Empty 0 1 "em", Empty 0 2 "em", Empty 0 3 "em", Empty 0 4 "em", Empty 0 5 "em", Empty 0 6 "em"],
    [Empty 1 0 "em", Empty 1 1 "em", Empty 1 2 "em", Empty 1 3 "em", Empty 1 4 "em", Empty 1 5 "em", Empty 1 6 "em"],
    [Empty 2 0 "em", Empty 2 1 "em", Empty 2 2 "em", Empty 2 3 "em", Empty 2 4 "em", Empty 2 5 "em", Empty 2 6 "em"],
    [Empty 3 0 "em", Empty 3 1 "em", Empty 3 2 "em", Empty 3 3 "em", Empty 3 4 "em", Empty 3 5 "em", Empty 3 6 "em"],
    [Empty 4 0 "em", Empty 4 1 "em", Empty 4 2 "em", Empty 4 3 "em", Empty 4 4 "em", Empty 4 5 "em", Empty 4 6 "em"],
    [Empty 5 0 "em", Empty 5 1 "em", Empty 5 2 "em", Empty 5 3 "em", Empty 5 4 "em", Empty 5 5 "em", Empty 5 6 "em"],
    [Empty 6 0 "em", Empty 6 1 "em", Empty 6 2 "em", Empty 6 3 "em", Empty 6 4 "em", Empty 6 5 "em", Empty 6 6 "em"]
    ], w =7, h = 7,
    obstacles = [],
    kids = [],
    corrals = [],
    robots = [],
    dirts = []
    }


-- These functions are used for generating the random board 

fillEmpty :: Int -> Int -> [[BoardObject]] -> [[BoardObject]]
fillEmpty _ (-1) board = board
fillEmpty w h board = fillEmpty w (h-1) (fillEmptyRow w h [] : board)

fillEmptyRow :: Int -> Int -> [BoardObject] -> [BoardObject]
fillEmptyRow (-1) _ board = board
fillEmptyRow w h board = fillEmptyRow (w-1) h (Empty h w "em": board)

fillKids :: Int -> Int -> Int -> [[BoardObject]] -> [(Int, Int)]-> IO ([[BoardObject]], [(Int, Int)])
fillKids 0 _ _ board kids = return (board, kids)
fillKids n w h board kids = do
    x <- randomGen 0 w
    y <- randomGen 0 h
    let board_object = (board !! x) !! y
    let ty = typ board_object
    if ty /= "em"
        then fillKids n w h board kids
        else do
            let new_kids = (x, y):kids

            let new_board = replaceAtPosition x y (Kid x y "ki") board
            fillKids (n-1) w h new_board new_kids
                

fillRobots :: Int -> Int -> Int -> [[BoardObject]] -> [(Int, Int)]-> IO ([[BoardObject]], [(Int, Int)])
fillRobots 0 _ _ board robots = return (board, robots)
fillRobots n w h board robots = do
    x <- randomGen 0 w
    y <- randomGen 0 h
    let board_object = (board !! x) !! y
    let ty = typ board_object
    if ty /= "em" 
        then fillRobots n w h board robots
        else do
            let new_robots = (x, y):robots           
            let new_board = replaceAtPosition x y (Robot x y "ro") board
            fillRobots (n-1) w h new_board new_robots

fillObstacles :: Int -> Int -> Int -> [[BoardObject]] -> [(Int, Int)]-> IO ([[BoardObject]], [(Int, Int)])
fillObstacles 0 _ _ board obstacles = return (board, obstacles)
fillObstacles n w h board obstacles = do
    x <- randomGen 0 w
    y <- randomGen 0 h
    let board_object = (board !! x) !! y
    let ty = typ board_object
    if ty /= "em" 
        then fillObstacles n w h board obstacles
        else do
            let new_obstacles = (x, y):obstacles           
            let new_board = replaceAtPosition x y (Obstacle x y "ob") board
            fillObstacles (n-1) w h new_board new_obstacles

fillDirts :: Int -> Int -> Int -> [[BoardObject]] -> [(Int, Int)]-> IO ([[BoardObject]], [(Int, Int)])
fillDirts 0 _ _ board dirts = return (board, dirts)
fillDirts n w h board dirts = do
    x <- randomGen 0 w
    y <- randomGen 0 h
    let board_object = (board !! x) !! y
    let ty = typ board_object
    if ty /= "em" 
        then fillDirts n w h board dirts
        else do
            let new_dirts = (x, y):dirts            
            let new_board = replaceAtPosition x y (Dirt x y "di") board
            fillDirts (n-1) w h new_board new_dirts

fillCorrals :: Int -> Int -> Int -> [[BoardObject]] -> IO [[BoardObject]]
fillCorrals 0 _ _ board = return board
fillCorrals n w h board = do
    x <- randomGen 0 w
    y <- randomGen 0 h
    let new_board = replaceAtPosition x y (EmptyCorral x y "ec") board
    let (result, _) = fillCorralsAround (n-1) x y w h new_board
    return result        

fillCorralsAround :: Int -> Int -> Int -> Int -> Int -> [[BoardObject]] -> ([[BoardObject]], Int)
fillCorralsAround 0 _ _ _ _ board = (board, 0)
fillCorralsAround n x y w h board = let    
    up_board_object = if y - 1 >= 0 then (board !! x) !! (y - 1) else Empty 0 0 "null"
    up_ty = if y - 1 >= 0 then typ up_board_object else "null"

    ri_board_object = if x + 1 <= w then (board !! (x + 1)) !! y else Empty 0 0 "null"
    ri_ty = if x+ 1 <= w then typ ri_board_object else "null"

    da_board_object = if y + 1 <= h then (board !! x) !! (y + 1) else Empty 0 0 "null"
    da_ty = if y + 1 <= h then typ da_board_object else "null"

    le_board_object = if x - 1 >= 0 then (board !! (x - 1)) !! y else Empty 0 0 "null"
    le_ty = if x - 1 >= 0 then typ le_board_object else "null"

    new_board_1 = if up_ty == "em" && n > 0 then replaceAtPosition x (y-1) (EmptyCorral x (y-1) "ec") board else board
    new_n_1 = if up_ty == "em" && n > 0 then n-1 else n

    new_board_2 = if ri_ty == "em" && new_n_1 > 0 then replaceAtPosition (x+1) y (EmptyCorral (x+1) y "ec") new_board_1 else new_board_1
    new_n_2 = if ri_ty == "em" && new_n_1 > 0 then new_n_1-1 else new_n_1

    new_board_3 = if da_ty == "em" && new_n_2 > 0 then replaceAtPosition x (y+1) (EmptyCorral x (y+1) "ec") new_board_2 else new_board_2
    new_n_3 = if da_ty == "em" && new_n_2 > 0 then new_n_2-1 else new_n_2

    new_board_4 = if le_ty == "em" && new_n_3 > 0 then replaceAtPosition (x-1) y (EmptyCorral (x-1) y "ec") new_board_3 else new_board_3
    new_n_4 = if le_ty == "em" && new_n_3 > 0 then new_n_3-1 else new_n_3

    (new_board_5, new_n_5) = if new_n_4 > 0 && up_ty == "em" then fillCorralsAround new_n_4 x (y-1) w h new_board_4 else (new_board_4, new_n_4)

    (new_board_6, new_n_6) = if new_n_5 > 0 && ri_ty == "em" then fillCorralsAround new_n_5 (x+1) y w h new_board_5 else (new_board_5, new_n_5)

    (new_board_7, new_n_7) = if new_n_6 > 0 && da_ty == "em" then fillCorralsAround new_n_6 x (y+1) w h new_board_6 else (new_board_6, new_n_6)

    (new_board_8, new_n_8) = if new_n_7 > 0 && le_ty == "em" then fillCorralsAround new_n_7 (x-1) y w h new_board_7 else (new_board_7, new_n_7)

    in (new_board_8, new_n_8)

    -- =====================================================================================================================