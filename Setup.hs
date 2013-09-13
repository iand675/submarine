module Main where
import Control.Applicative
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.Cmd

main = do
	args <- getArgs
	--case args of
		--["init"] -> initializeSandboxing
		--["install-dependencies"] -> installDependencies
		--_ -> putStrLn "Doing nothing"
	installDependencies

getDirectoryListing dir = do
	dirs <- drop 2 <$> getDirectoryContents dir
	return $! ((dir </>) <$> dirs)

installDependencies = do
	allProjects <- concat <$> mapM getDirectoryListing ["lib", "sites", "workers"]
	let installProjectDependenciesCmd p = "cabal install -j --only-dependencies " ++ p
	let installProjectDependencies p = putStrLn (installProjectDependenciesCmd p) >> system (installProjectDependenciesCmd p)
	mapM_ installProjectDependencies allProjects

initializeSandboxing = do
	system "cabal sandbox init"
	currentDir <- getCurrentDirectory
	let projectDirectories = ["lib", "sites", "workers"]
	projects <- concat <$> mapM getDirectoryListing projectDirectories
	let initializeSandboxForPath p = do
		setCurrentDirectory (currentDir </> p)
		system ("cabal sandbox init --sandbox " ++ currentDir ++ "/.cabal-sandbox")
		setCurrentDirectory currentDir
	mapM_ initializeSandboxForPath projects
	

addLibrarySources = do
	libraries <- getDirectoryListing "lib"
	mapM_ addLibrarySource libraries

addLibrarySource l = do
	putStrLn ("Adding source " ++ l)
	system ("cabal sandbox add-source " ++ l)

