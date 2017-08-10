module Camfort.Specification.Hoare where

import           Control.Monad.Except
import           Data.List (intersperse)

import qualified Language.Fortran.Analysis                 as FA
import qualified Language.Fortran.Analysis.BBlocks         as FAB
import qualified Language.Fortran.Analysis.Renaming        as FAR
import qualified Language.Fortran.AST                      as F
import qualified Language.Fortran.Util.Position            as F

import           Camfort.Analysis
import           Camfort.Analysis.Annotations
import           Camfort.Analysis.ModFile
import           Camfort.Helpers
import           Camfort.Input
import           Camfort.Specification.Hoare.Annotation
import           Camfort.Specification.Hoare.CheckFrontend
import           Camfort.Specification.Hoare.Parser

getBlocks = FAB.analyseBBlocks . FAR.analyseRenames . FA.initAnalysis . fmap hoareAnn0

check :: SimpleAnalysis (F.ProgramFile Annotation) Report
check = do
  pf <- analysisInput
  res <- branchAnalysis prettyInvariantChecking (getBlocks pf)
  -- Append filename to any outputs
  let output   = mconcat . intersperse (mkReport "\n\n") . analysisResult $ res
      dbg      = show (analysisDebug res)
      filename = F.pfGetFilename pf

  pure $ (mkReport $ "\n" ++ filename ++ "\n" ++ dbg ++ "\n") `mappend` output

testOn :: FilePath -> IO ()
testOn fp = doAnalysisReport check simpleCompiler () fp "." []

testHoare = testOn "samples/invariants/invariants.f90"