-- | GHC Dump Tool

-- You may need to do: $ ghc -package ghc GHCD.h

import BasicTypes
import Coercion
import CorePrep
import CoreSyn
import CoreToStg
import CostCentre
import DataCon
import FastString
import ForeignCall
import GHC
import GHC.Paths
import GhcMonad
import HscTypes
import Literal
import Module
import Name
import Outputable
import Pair
import PrimOp
import SrcLoc
import StgSyn
import TyCon
-- import TyCoRep  -- GHC 8.0.2
import TypeRep  -- GHC 7.10.3
import Type
import UniqSet
import Unique
import Var

import System.IO
import System.Environment

import Data.List
import Data.Maybe

-- | Make IO String from Outputable
--   Simple print dump.
mkIOStr :: (Outputable a) => a -> IO String
mkIOStr obj = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let ppr_str = showPpr dflags obj
    return ppr_str

-- | Make Module [Guts]
--   Used for Core Program.
mkModuleGutss :: FilePath -> FilePath -> IO [ModGuts]
mkModuleGutss proj src = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags (dflags {importPaths = [proj]})
    target <- guessTarget src Nothing
    setTargets [target]
    load LoadAllTargets

    mod_graph <- getModuleGraph
    let locs = map ms_location mod_graph
    pmods <- (sequence . map parseModule) mod_graph
    tmods <- (sequence . map typecheckModule) pmods
    dmods <- (sequence . map desugarModule) tmods
    let mod_gutss = map coreModule dmods
    return mod_gutss

-- | Make Multiple Core Programs
--   Concat them, because `type CoreProgram = [CoreBind]`.
mkMultiCoreProgram :: FilePath -> FilePath -> IO CoreProgram
mkMultiCoreProgram proj src = do
    mod_gutss <- mkModuleGutss proj src
    let acc_prog = concatMap mg_binds mod_gutss
    return acc_prog

-- | Make Multiple [StgBinding]
mkMultiStgBindings :: FilePath -> FilePath -> IO [StgBinding]
mkMultiStgBindings proj src = runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags (dflags {importPaths = [proj]})
    env <- getSession
    target <- guessTarget src Nothing
    setTargets [target]
    load LoadAllTargets

    mod_graph <- getModuleGraph
    let locs = map ms_location mod_graph
    pmods <- (sequence . map parseModule) mod_graph
    tmods <- (sequence . map typecheckModule) pmods
    dmods <- (sequence . map desugarModule) tmods
    let mod_gutss = map coreModule dmods
    let locs   = map ms_location mod_graph
    let bindss = map mg_binds mod_gutss
    let tcss   = map mg_tcs mod_gutss

    -- let z1 = zip4 (map ms_module mod_graph) locs bindss tcss  -- GHC 8.0.2
    let z1 = zip3 locs bindss tcss
    -- preps <- liftIO $ sequence $ map (\(m,l,b,t)->corePrepPgm env m lb t) z1
    preps <- liftIO $ sequence $ map (\(l,b,t) -> corePrepPgm env l b t) z1

    let z2 = zip (map mg_module mod_gutss) preps
    bindss <- liftIO $ sequence $ map (\(m, p) -> coreToStg dflags m p) z2
    let acc_binds = concat bindss
    return acc_binds

err_msg = "GHCD [--core | --stg] <proj-dir> <src-file>"

main = do
    args <- getArgs
    case args of
        []         -> error err_msg
        (a:[])     -> error err_msg
        (a:b:[])   -> error err_msg
        (opt:proj:src:[]) -> case opt of
            "--core" -> do
                core_prog <- mkMultiCoreProgram proj src
                putStrLn =<< mkIOStr core_prog

            "--stg" -> do
                stg_binds <- mkMultiStgBindings proj src
                putStrLn =<< mkIOStr stg_binds

            otherwise -> error err_msg
        otherwise -> error err_msg

