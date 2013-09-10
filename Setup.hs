module Main where
import System.Directory
import System.FilePath
import System.Cmd

initializeSandboxing = do
	currentDir <- getCurrentDirectory
	let executableDirectories = ["sites/", "workers/"]
	let projectDirectories = "lib/" : executableDirectories
	system "cabal sandbox init"
	projectsByDirectory <- mapM getDirectoryContents executableDirectories
	libraries <- fmap (drop 2) $ getDirectoryContents "lib/"
	let projectList = concat $ zipWith (\dir -> map (dir ++)) projectDirectories (libraries : map (drop 2) projectsByDirectory)
	let initializeSandboxForPath p = do
		setCurrentDirectory (currentDir </> p)
		system ("cabal sandbox init --sandbox " ++ currentDir ++ "/.cabal-sandbox")
		setCurrentDirectory currentDir
	let addLibrarySource l = do
		putStrLn ("Adding source lib/" ++ l)
		system ("cabal sandbox add-source lib/" ++ l)
	mapM_ initializeSandboxForPath projectList
	mapM_ addLibrarySource libraries