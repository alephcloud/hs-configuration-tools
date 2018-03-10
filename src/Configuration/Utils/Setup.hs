-- ------------------------------------------------------ --
-- Copyright © 2015-2016 Lars Kuhtz <lakuhtz@gmail.com>
-- Copyright © 2014 AlephCloud Systems, Inc.
-- ------------------------------------------------------ --

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- There are three ways how this module can be used:
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
-- 3. For usage within a more complex @Setup.hs@ script you shall import this
--    module qualified and use the 'mkPkgInfoModules' function. For example:
--
--    > module Main (main) where
--    >
--    > import qualified Configuration.Utils.Setup as ConfTools
--    >
--    > main :: IO ()
--    > main = defaultMainWithHooks (ConfTools.mkPkgInfoModules simpleUserHooks)
--    >
--
-- With all methods the field @Build-Type@ in the package description (cabal) file
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
module Configuration.Utils.Setup
( main
, mkPkgInfoModules
) where

#ifndef MIN_VERSION_Cabal
#define NO_CABAL_MACROS 1
#define MIN_VERSION_Cabal(a,b,c) 0
#endif

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup
import qualified Distribution.InstalledPackageInfo as I
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import Distribution.Simple.PackageIndex
import Distribution.Text

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.LocalBuildInfo
import Distribution.Types.UnqualComponentName
#endif

#if MIN_VERSION_Cabal(2,2,0)
import Distribution.Pretty
#endif

import System.Process

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Monoid

import Prelude hiding (readFile, writeFile)

import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, getCurrentDirectory, canonicalizePath)
import System.FilePath (isDrive, (</>), takeDirectory)
import System.Exit (ExitCode(ExitSuccess))

#ifdef NO_CABAL_MACROS
import Data.Maybe
import Data.Typeable
#endif

-- | Include this function when your setup doesn't contain any
-- extra functionality.
--
main :: IO ()
main = defaultMainWithHooks (mkPkgInfoModules simpleUserHooks)

-- | Modifies the given record of hooks by adding functionality that
-- creates a package info module for each component of the cabal package.
--
-- This function is intended for usage in more complex @Setup.hs@ scripts.
-- If your setup doesn't contain any other function you can just import
-- the 'main' function from this module.
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
mkPkgInfoModules
    :: UserHooks
    -> UserHooks
mkPkgInfoModules hooks = hooks
    { postConf = mkPkgInfoModulesPostConf (postConf hooks)
    }

mkPkgInfoModulesPostConf
    :: (Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ())
    -> Args
    -> ConfigFlags
    -> PackageDescription
    -> LocalBuildInfo
    -> IO ()
mkPkgInfoModulesPostConf hook args flags pkgDesc bInfo = do
    mkModules
    hook args flags pkgDesc bInfo
  where
    mkModules = mapM_ (f . \(a,_,_) -> a) $ componentsConfigs bInfo
    f cname = case cname of
        CLibName -> updatePkgInfoModule Nothing pkgDesc bInfo
        CExeName s -> updatePkgInfoModule (Just $ unUnqualComponentName s) pkgDesc bInfo
        CTestName s -> updatePkgInfoModule (Just $ unUnqualComponentName s) pkgDesc bInfo
        CBenchName s -> updatePkgInfoModule (Just $ unUnqualComponentName s) pkgDesc bInfo

#if !MIN_VERSION_Cabal(2,0,0)
    unUnqualComponentName = id
#endif

pkgInfoModuleName :: Maybe String -> String
pkgInfoModuleName Nothing = "PkgInfo"
pkgInfoModuleName (Just cn) = "PkgInfo_" ++ map tr cn
  where
    tr '-' = '_'
    tr c = c

-- FIXME: autoModulesDir is deprecated and should be replaced by
-- autogenComponentModulesDir.
pkgInfoFileName :: Maybe String -> LocalBuildInfo -> FilePath
pkgInfoFileName cn bInfo = autogenModulesDir bInfo ++ "/" ++ pkgInfoModuleName cn ++ ".hs"

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

getVCS :: IO (Maybe RepoType)
getVCS = getCurrentDirectory >>= getVcsOfDir

getVcsOfDir :: FilePath -> IO (Maybe RepoType)
getVcsOfDir d = do
    canonicDir <- canonicalizePath d
    doesDirectoryExist (canonicDir </> ".hg") >>= \x0 -> if x0
    then return (Just Mercurial)
    else doesDirectoryExist (canonicDir </> ".git") >>= \x1 -> if x1
        then return $ Just Git
        else if isDrive canonicDir
            then return Nothing
            else getVcsOfDir (takeDirectory canonicDir)

#if !MIN_VERSION_Cabal(2,0,0)
unFlagName :: FlagName -> String
unFlagName (FlagName s) = s
#endif

#if !MIN_VERSION_Cabal(2,2,0)
unFlagAssignment :: FlagAssignment -> [(FlagName, Bool)]
unFlagAssignment = id
#endif

#if !MIN_VERSION_Cabal(2,2,0)
prettyShow :: Text a => a -> String
prettyShow = display
#endif

pkgInfoModule :: Maybe String -> PackageDescription -> LocalBuildInfo -> IO B.ByteString
pkgInfoModule cName pkgDesc bInfo = do
    (tag, revision, branch) <- getVCS >>= \x -> case x of
        Just Mercurial -> hgInfo
        Just Git -> gitInfo
        _ -> noVcsInfo

    let vcsBranch = if branch == "default" || branch == "master" then "" else branch
        vcsVersion = intercalate "-" . filter (/= "") $ [tag, revision, vcsBranch]
        flags = map (unFlagName . fst) . filter snd . unFlagAssignment . configConfigurationsFlags . configFlags $ bInfo

    licenseString <- licenseFilesText pkgDesc

    return $ B.intercalate "\n"
            [ "{-# LANGUAGE OverloadedStrings #-}"
            , "{-# LANGUAGE RankNTypes #-}"
            , ""
            , "module " <> (pack . pkgInfoModuleName) cName <> " where"
            , ""
            , "    import Data.String (IsString)"
            , "    import Data.Monoid"
            , ""
            , "    name :: IsString a => Maybe a"
            , "    name = " <> maybe "Nothing" (\x -> "Just \"" <> pack x <> "\"") cName
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
            , "    compiler = \"" <> (pack . display . compilerId . compiler) bInfo <> "\""
            , ""
            , "    flags :: IsString a => [a]"
            , "    flags = " <> (pack . show) flags
            , ""
            , "    optimisation :: IsString a => a"
            , "    optimisation = \"" <> (displayOptimisationLevel . withOptimization) bInfo <> "\""
            , ""
            , "    arch :: IsString a => a"
            , "    arch = \"" <> (pack . display . hostPlatform) bInfo <> "\""
            , ""
            , "    license :: IsString a => a"
            , "    license = \"" <> (pack . prettyShow . license) pkgDesc <> "\""
            , ""
            , "    licenseText :: IsString a => a"
            , "    licenseText = " <> (pack . show) licenseString
            , ""
            , "    copyright :: IsString a => a"
            , "    copyright = " <> (pack . show . copyright) pkgDesc
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
            , "    dependencies = " <> (pack . show . map (display . packageId) . allPackages . installedPkgs) bInfo
            , ""
            , "    dependenciesWithLicenses :: IsString a => [a]"
            , "    dependenciesWithLicenses = " <> (pack . show . map pkgIdWithLicense . allPackages . installedPkgs) bInfo
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
            , "        <> \"Build flags: \" <> mconcat (map (\\x -> \" \" <> x) flags) <> \"\\n\""
            , "        <> \"Optimisation: \" <> optimisation <> \"\\n\\n\""
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
  where
    displayOptimisationLevel NoOptimisation = "none"
    displayOptimisationLevel NormalOptimisation = "normal"
    displayOptimisationLevel MaximumOptimisation = "maximum"

updatePkgInfoModule :: Maybe String -> PackageDescription -> LocalBuildInfo -> IO ()
updatePkgInfoModule cName pkgDesc bInfo = do
    createDirectoryIfMissing True $ autogenModulesDir bInfo
    newFile <- pkgInfoModule cName pkgDesc bInfo
    let update = B.writeFile fileName newFile
    doesFileExist fileName >>= \x -> if x
    then do
        oldRevisionFile <- B.readFile fileName
        when (oldRevisionFile /= newFile) update
    else
        update
  where
    fileName = pkgInfoFileName cName bInfo

licenseFilesText :: PackageDescription -> IO B.ByteString
licenseFilesText pkgDesc =
    B.intercalate "\n------------------------------------------------------------\n" <$> mapM fileText
#ifdef NO_CABAL_MACROS
        (getLicenseFiles pkgDesc)
#elif MIN_VERSION_Cabal(1,20,0)
        (licenseFiles pkgDesc)
#else
        [licenseFile pkgDesc]
#endif
  where
    fileText file = doesFileExist file >>= \x -> if x
        then B.readFile file
        else return ""

#ifdef NO_CABAL_MACROS
-- The name and the type of the @licenseFile :: String@ field of
-- 'PackageDescription' changed in Cabal version 1.20 to @licenseFiles ::
-- [String]@. Both versions are used with GHC-7.8. In Cabal version 1.24 the
-- number of constructor fields changed for PackageDescription. When compiling
-- @Setup.hs@ the @MIN_VERSION_...@ macros are not available. This function is
-- an ugly hack to do conditional compilation without CPP. It depends on the
-- @RecordWildCards@ and @Typeable@ extensions and abuses shaddowing of
-- existing symbols.
--
-- Alternative methods would include:
--
-- *   Use typeable to pattern match on the number of constructor arguments for
--     'PackageDescription',
-- *   use TH hacks ala @$( if versionBranch cabalVersion < [...] ... )@, and/or
-- *   make guesses about the cabal version based on the @__GLASGOW_HASKELL__@
--     macro.

-- Return license files of from the @PackageDescription@.
--
getLicenseFiles :: PackageDescription -> [FilePath]
getLicenseFiles pkgDesc = f pkgDesc
  where
    -- Recent versions of @PackageDescription@ define a @licenseFiles@ field.
    -- For older versions the function @licenseFiles@ that is defined below is
    -- used.
    --
    f PackageDescription{..} = licenseFiles

    -- If @PackageDescription@ doesn't define @licenseFiles@ this definition
    -- will be used. In that case @PackageDescription@ is assumed to define
    -- @licenseFile@.
    --
    licenseFiles :: [FilePath]
    licenseFiles = fromMaybe (error "unsupported Cabal library version") $
        return <$> cast (g pkgDesc)

    g PackageDescription{..} = licenseFile

    -- The only purpose of this function is to ensure that licenseFile is
    -- defined in case PackageDescription doesn't have a @licenseFile@ field.
    -- This function won't be called with any supported version of the Cabal
    -- library.
    --
    licenseFile :: [FilePath]
    licenseFile = error "unexpected Cabal library version"
#endif

hgInfo :: IO (String, String, String)
hgInfo = do
    tag <- trim <$> readProcess "hg" ["id", "-r", "max(ancestors(\".\") and tag())", "-t"] ""
    rev <- trim <$> readProcess "hg" ["id", "-i"] ""
    branch <- trim <$> readProcess "hg" ["id", "-b"] ""
    return (tag, rev, branch)

gitInfo :: IO (String, String, String)
gitInfo = do
    tag <- do
        (exitCode, out, _err) <- readProcessWithExitCode "git" ["describe", "--exact-match", "--tags", "--abbrev=0"] ""
        case exitCode of
            ExitSuccess -> return $ trim out
            _ -> return ""
    rev <- trim <$> readProcess "git" ["rev-parse", "--short", "HEAD"] ""
    branch <- trim <$> readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
    return (tag, rev, branch)

noVcsInfo :: IO (String, String, String)
noVcsInfo = return ("", "", "")

pkgIdWithLicense :: I.InstalledPackageInfo -> String
pkgIdWithLicense a = (display . packageId) a
    ++ " ["
#if MIN_VERSION_Cabal(2,2,0)
    ++ (either prettyShow prettyShow . I.license) a
#else
    ++ (prettyShow . I.license) a
#endif
    ++ (if cr /= "" then ", " ++ cr else "")
    ++ "]"
  where
    cr = (unwords . words . I.copyright) a

