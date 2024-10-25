main = do
  putStrLn "Welcome to Task Manager 3000"
  taskManager []

menu = do
  putStrLn "\nBelow are the options:"
  putStrLn "\tadd"
  putStrLn "\tprint"
  putStrLn "\tsearch"

printTasks (x : xs) = do
  putStrLn $ "\t" ++ x
  printTasks xs
printTasks [] = do
  putStr ""

searchTasks [] term = putStrLn $ "\tCould not find " ++ term
searchTasks (x : xs) term = do
  if x == term
    then putStrLn $ "\tFound: " ++ x
    else searchTasks xs term

taskManager tasks = do
  menu
  putStrLn "Enter option:"
  choice <- getLine
  case choice of
    "add" -> do
      putStrLn "Enter Task to Add:"
      newTask <- getLine
      let updatedTasks = newTask : tasks
      taskManager updatedTasks
    "print" -> do
      putStrLn "Here are your tasks:"
      printTasks tasks
      taskManager tasks
    "search" -> do
      putStrLn "Enter Task to Search:"
      searchTerm <- getLine
      searchTasks tasks searchTerm
      taskManager tasks
    _ -> do
      putStrLn "Error"
