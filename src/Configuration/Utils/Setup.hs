-- ------------------------------------------------------ --
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- | This module contains a @Setup.hs@ script that hooks into the cabal build
-- process at the end of the configuration phase and generates a module with
-- package information for each component of the cabal package.
--
-- The modules are created in the /autogen/ build directory where also the
-- @Path_@ module is created by cabal's simple build setup. This is usually the
-- directory @.\/dist\/build\/autogen@.
--
-- For a library component the module is named just @PkgInfo@. For all other
-- components the module is named @PkgInfo_COMPONENT_NAME@ where
-- @COMPONENT_NAME@ is the name of the component with @-@ characters replaced by
-- @_@.
--
-- For instance, if a cabal package contains a library and an executable that is
-- called /my-app/, the following modules are created: @PkgInfo@ and
-- @PkgInfo_my_app@.
--
--
-- = Usage as Setup Script
--
-- There are two ways how this module can be used:
--
-- 1. Copy the code of this module into a file called @Setup.hs@ in the root
--    directory of your package.
--
-- 2. If the /configuration-tools/ package is already installed in the system
--    where the build is done, following code can be used as @Setup.hs@ script:
--
--    > module Main (main) where
--    >
--    > import Configuration.Utils.Setup
--
-- With both methods the field @Build-Type@ in the package description (cabal) file
-- must be set to @Custom@:
--
-- > Build-Type: Custom
--
--
-- = Integration With "Configuration.Utils"
--
-- You can integrate the information provided by the @PkgInfo@ modules with the
-- command line interface of an application by importing the respective module
-- for the component and using the
-- 'Configuration.Utils.runWithPkgInfoConfiguration' function from the module
-- "Configuration.Utils" as show in the following example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE FlexibleInstances #-}
-- >
-- > module Main
-- > ( main
-- > ) where
-- >
-- > import Configuration.Utils
-- > import PkgInfo
-- >
-- > instance FromJSON (() -> ()) where parseJSON _ = pure id
-- >
-- > mainInfo :: ProgramInfo ()
-- > mainInfo = programInfo "Hello World" (pure id) ()
-- >
-- > main :: IO ()
-- > main = runWithPkgInfoConfiguration mainInfo pkgInfo . const $ putStrLn "hello world"
--
-- With that the resulting application supports the following additional command
-- line options:
--
-- [@--version@, @-v@]
--     prints the version of the application and exits.
--
-- [@--info@, @-i@]
--     prints a short info message for the application and exits.
--
-- [@--long-info@]
--     print a detailed info message for the application and exits.
--     Beside component name, package name, version, revision, and copyright
--     the message also contain information about the compiler that
--     was used for the build, the build architecture, build flags,
--     the author, the license type, and a list of all direct and
--     indirect dependencies along with their licenses and copyrights.
--
-- [@--license@]
--     prints the text of the lincense of the application and exits.
--
module Configuration.Utils.Setup (main) where

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup
import qualified Distribution.InstalledPackageInfo as I
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.PackageIndex
import Distribution.Text
import System.Process

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Monoid

import Prelude hiding (readFile, writeFile)

import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.Exit (ExitCode(ExitSuccess))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { postConf = mkPkgInfoModules
    }
  where
    mkPkgInfoModules _ _ pkgDesc buildInfo = mapM_ f . map (\(a,_,_) -> a) $ componentsConfigs buildInfo
      where
        f cname = case cname of
            CLibName -> updatePkgInfoModule Nothing pkgDesc buildInfo
            CExeName s -> updatePkgInfoModule (Just s) pkgDesc buildInfo
            CTestName s -> updatePkgInfoModule (Just s) pkgDesc buildInfo
            CBenchName s -> updatePkgInfoModule (Just s) pkgDesc buildInfo

pkgInfoModuleName :: Maybe String -> String
pkgInfoModuleName Nothing = "PkgInfo"
pkgInfoModuleName (Just cn) = "PkgInfo_" ++ map tr cn
  where
    tr '-' = '_'
    tr c = c

pkgInfoFileName :: Maybe String -> LocalBuildInfo -> FilePath
pkgInfoFileName cn buildInfo = autogenModulesDir buildInfo ++ "/" ++ pkgInfoModuleName cn ++ ".hs"

trim = f . f
  where f = reverse . dropWhile isSpace

getVCS :: IO (Maybe RepoType)
getVCS = do
    doesDirectoryExist ".hg" >>= \x0 -> if x0
    then return (Just Mercurial)
    else doesDirectoryExist ".git" >>= \x1 -> if x1
    then return (Just Git)
    else return Nothing

pkgInfoModule :: Maybe String -> PackageDescription -> LocalBuildInfo -> IO B.ByteString
pkgInfoModule componentName pkgDesc buildInfo = do
    (tag, revision, branch) <- getVCS >>= \x -> case x of
        Just Mercurial -> hgInfo
        Just Git -> gitInfo
        _ -> noVcsInfo

    let vcsBranch = if branch == "default" || branch == "master" then "" else branch
        vcsVersion = intercalate "-" . filter (/= "") $ [tag, revision, vcsBranch]
        flags = map fst . filter snd . configConfigurationsFlags . configFlags $ buildInfo

    licenseString <- licenseFilesText pkgDesc

    return $ B.intercalate "\n" $
            [ "{-# LANGUAGE OverloadedStrings #-}"
            , "{-# LANGUAGE RankNTypes #-}"
            , ""
            , "module " <> (pack . pkgInfoModuleName) componentName <> " where"
            , ""
            , "    import Data.String (IsString)"
            , "    import Data.Monoid"
            , ""
            , "    name :: IsString a => Maybe a"
            , "    name = " <> maybe "Nothing" (\x -> "Just \"" <> pack x <> "\"") componentName
            , ""
            , "    tag :: IsString a => a"
            , "    tag = \"" <> pack tag <> "\""
            , ""
            , "    revision :: IsString a => a"
            , "    revision = \"" <> pack revision <> "\""
            , ""
            , "    branch :: IsString a => a"
            , "    branch = \"" <> pack branch <> "\""
            , ""
            , "    branch' :: IsString a => a"
            , "    branch' = \"" <> pack vcsBranch <> "\""
            , ""
            , "    vcsVersion :: IsString a => a"
            , "    vcsVersion = \"" <> pack vcsVersion <> "\""
            , ""
            , "    compiler :: IsString a => a"
            , "    compiler = \"" <> (pack . display . compilerId . compiler) buildInfo <> "\""
            , ""
            , "    flags :: IsString a => [a]"
            , "    flags = " <> (pack . show) flags
            , ""
            , "    arch :: IsString a => a"
            , "    arch = \"" <> (pack . display . hostPlatform) buildInfo <> "\""
            , ""
            , "    license :: IsString a => a"
            , "    license = \"" <> (pack . display . license) pkgDesc <> "\""
            , ""
            , "    licenseText :: IsString a => a"
            , "    licenseText = " <> (pack . show) licenseString
            , ""
            , "    copyright :: IsString a => a"
            , "    copyright = \"" <> (pack . copyright) pkgDesc <> "\""
            , ""
            , "    author :: IsString a => a"
            , "    author = \"" <> (pack . author) pkgDesc <> "\""
            , ""
            , "    homepage :: IsString a => a"
            , "    homepage = \"" <> (pack . homepage) pkgDesc <> "\""
            , ""
            , "    package :: IsString a => a"
            , "    package = \"" <> (pack . display . package) pkgDesc <> "\""
            , ""
            , "    packageName :: IsString a => a"
            , "    packageName = \"" <> (pack . display . packageName) pkgDesc <> "\""
            , ""
            , "    packageVersion :: IsString a => a"
            , "    packageVersion = \"" <> (pack . display . packageVersion) pkgDesc <> "\""
            , ""
            , "    dependencies :: IsString a => [a]"
            , "    dependencies = " <> (pack . show . map (display . packageId) . allPackages . installedPkgs) buildInfo
            , ""
            , "    dependenciesWithLicenses :: IsString a => [a]"
            , "    dependenciesWithLicenses = " <> (pack . show . map pkgIdWithLicense . allPackages . installedPkgs) buildInfo
            , ""
            , "    versionString :: (Monoid a, IsString a) => a"
            , "    versionString = case name of"
            , "        Nothing -> package <> \" (revision \" <> vcsVersion <> \")\""
            , "        Just n -> n <> \"-\" <> packageVersion <> \" (package \" <> package <> \" revision \" <> vcsVersion <> \")\""
            , ""
            , "    info :: (Monoid a, IsString a) => a"
            , "    info = versionString <> \"\\n\" <> copyright"
            , ""
            , "    longInfo :: (Monoid a, IsString a) => a"
            , "    longInfo = info <> \"\\n\\n\""
            , "        <> \"Author: \" <> author <> \"\\n\""
            , "        <> \"License: \" <> license <> \"\\n\""
            , "        <> \"Homepage: \" <> homepage <> \"\\n\""
            , "        <> \"Build with: \" <> compiler <> \" (\" <> arch <> \")\" <> \"\\n\""
            , "        <> \"Build flags: \" <> mconcat (map (\\x -> \" \" <> x) flags) <> \"\\n\\n\""
            , "        <> \"Dependencies:\\n\" <> mconcat (map (\\x -> \"    \" <> x <> \"\\n\") dependenciesWithLicenses)"
            , ""
            , "    pkgInfo :: (Monoid a, IsString a) => (a, a, a, a)"
            , "    pkgInfo ="
            , "        ( info"
            , "        , longInfo"
            , "        , versionString"
            , "        , licenseText"
            , "        )"
            , ""
            ]

updatePkgInfoModule :: Maybe String -> PackageDescription -> LocalBuildInfo -> IO ()
updatePkgInfoModule componentName pkgDesc buildInfo = do
    createDirectoryIfMissing True $ autogenModulesDir buildInfo
    newFile <- pkgInfoModule componentName pkgDesc buildInfo
    let update = B.writeFile fileName newFile
    doesFileExist fileName >>= \x -> if x
    then do
        oldRevisionFile <- B.readFile fileName
        when (oldRevisionFile /= newFile) update
    else
        update
  where
    fileName = pkgInfoFileName componentName buildInfo

licenseFilesText :: PackageDescription -> IO B.ByteString
licenseFilesText PackageDescription{ licenseFiles = fileNames } =
    B.intercalate "\n------------------------------------------------------------\n" <$> mapM fileText fileNames
  where
    fileText file = doesFileExist file >>= \x -> if x
        then B.readFile file
        else return ""

hgInfo :: IO (String, String, String)
hgInfo = do
    tag <- fmap trim $ readProcess "hg" ["id", "-r", "max(ancestors(\".\") and tag())", "-t"] ""
    rev <- fmap trim $ readProcess "hg" ["id", "-i"] ""
    branch <- fmap trim $ readProcess "hg" ["id", "-b"] ""
    return (tag, rev, branch)

gitInfo :: IO (String, String, String)
gitInfo = do
    tag <- do
        (exitCode, out, err) <- readProcessWithExitCode "git" ["describe", "--exact-match", "--abbrev=0"] ""
        case exitCode of
            ExitSuccess -> return $ trim out
            _ -> return ""
    rev <- fmap trim $ readProcess "git" ["rev-parse", "--short", "HEAD"] ""
    branch <- fmap trim $ readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
    return (tag, rev, branch)

noVcsInfo :: IO (String, String, String)
noVcsInfo = return ("", "", "")

pkgIdWithLicense :: I.InstalledPackageInfo -> String
pkgIdWithLicense a = (display . packageId) a
    ++ " ["
    ++ (display . I.license) a
    ++ (if cr /= "" then ", " ++ cr else "")
    ++ "]"
  where
    cr = (unwords . words . I.copyright) a

