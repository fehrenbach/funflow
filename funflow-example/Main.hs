{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Arrow
import Control.Exception (Exception (..))
import Control.Funflow
import Control.Funflow.ContentHashable
import Control.Funflow.ContentStore (Content (..))
import qualified Control.Funflow.ContentStore as CS
import qualified Control.Funflow.External.Docker as Docker
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Path
import Path.IO

main :: IO ()
main = do
    cwd <- getCurrentDir
    r <- withSimpleLocalRunner (cwd </> [reldir|funflow-example/store|]) $ \run -> do
      run flow 3
    case r of
      Left err ->
        putStrLn $ "FAILED: " ++ displayException err
      Right out ->
        putStrLn $ "SUCCESS: " ++ out

flow :: SimpleFlow Int String
flow = proc n -> do
    moduleDouble <- compileModule -<
      "int times2(int n) { return 2*n; }\n"
    moduleSquare <- compileModule -<
      "int square(int n) { return n*n; }\n"
    moduleMain <- compileModule -<
      "#include <stdio.h>\n\
      \#include <stdlib.h>\n\
      \int times2(int n);\n\
      \int square(int n);\n\
      \int main(int argc, char **argv) {\n\
      \  int n = atoi(argv[1]);\n\
      \  int r = times2(n) + square(n);\n\
      \  printf(\"%d\\n\", r);\n\
      \}"
    exec <- compileExec -< [moduleDouble, moduleSquare, moduleMain]
    out <- runExec -< (exec, [show n])
    readString_ -< out

compileModule :: SimpleFlow String (Content File)
compileModule = proc csrc -> do
    cInput <- writeString -< (csrc, [relfile|out.c|])
    scriptInput <- writeExecutableString -< (compileScript, [relfile|compile.sh|])
    compiled <- compileDocker -< (cInput, scriptInput)
    returnA -< compiled :</> [relfile|out.o|]
  where
    compileScript =
      "#!/usr/bin/env bash\n\
      \gcc -c -o $2 $1\n"
    compileDocker = docker $ \(cInput, scriptInput) -> Docker.Config
      { Docker.image = "gcc"
      , Docker.optImageID = Just "7.3.0"
      , Docker.input = Docker.MultiInput
        $ Map.fromList [ ("script", IPItem $ CS.contentItem scriptInput)
                       , ("data", IPItem $ CS.contentItem cInput)
                       ]
      , Docker.command = "/input/script/compile.sh"
      , Docker.args = ["/input/data/out.c", "/output/out.o"]
      }

compileExec :: SimpleFlow [Content File] (Content File)
compileExec = proc mods -> do
    scriptInput <- writeExecutableString -< (compileScript, [relfile|compile.sh|])
    compiled <- compileDocker -< (mods, scriptInput)
    returnA -< compiled :</> [relfile|out|]
  where
    compileScript =
      "#!/usr/bin/env bash\n\
      \out=\"$1\"\n\
      \shift\n\
      \gcc -o \"$out\" $@\n"
    compileDocker = docker $ \(cModules, scriptInput) -> Docker.Config
      { Docker.image = "gcc"
      , Docker.optImageID = Just "7.3.0"
      , Docker.input = Docker.MultiInput
        $ Map.fromList $ ("script", IPItem $ CS.contentItem scriptInput) :
                       [ ("module"++show n, IPItem $ CS.contentItem cMod)
                       | (n, cMod) <- zip [1..] cModules
                       ]
      , Docker.command = "/input/script/compile.sh"
      , Docker.args = "/output/out" :
                      [ T.pack $ "/input/module"++show n++"/out.o"
                      | (n, cMod) <- zip [1..] cModules
                      ]
      }

runExec :: SimpleFlow (Content File, [String]) CS.Item
runExec = proc (exec, args) -> do
    scriptInput <- writeExecutableString -< (runScript, [relfile|run.sh|])
    runDocker -< (scriptInput, exec, args)
  where
    runScript =
      "#!/bin/sh\n\
      \/input/exec/out $@ > /output/out\n"
    runDocker = docker $ \(script, exec, args) -> Docker.Config
      { Docker.image = "gcc"
      , Docker.optImageID = Just "7.3.0"
      , Docker.input = Docker.MultiInput
        $ Map.fromList [ ("script", IPItem $ CS.contentItem script)
                       , ("exec", IPItem $ CS.contentItem exec)
                       ]
      , Docker.command = "/input/script/run.sh"
      , Docker.args = map T.pack args
      }
