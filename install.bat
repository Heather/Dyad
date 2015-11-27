@echo off
cabal install --only-dependencies
cabal configure
cabal install
pause
