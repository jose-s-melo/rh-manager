module Util.Utilitarios where

import Model.TiposDados
import Data.Time
import qualified Data.Map as Map
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

pause :: IO ()
pause = do
  putStrLn "\nPressione ENTER para voltar..."
  _ <- getLine
  return ()

lerLinha :: String -> IO String
lerLinha msg = do
  putStrLn msg
  putStr "> "
  hFlush stdout
  getLine

lerCPF :: IO CPF
lerCPF = lerLinha "CPF"

lerData :: String -> IO Day
lerData msg = do
  entrada <- lerLinha (msg ++ " (AAAA-MM-DD)")
  case readMaybe entrada of
    Just d  -> return d
    Nothing -> msgErro "Data inválida." >> lerData msg

lerInt :: String -> IO Int
lerInt msg = do
  entrada <- lerLinha msg
  case readMaybe entrada of
    Just n  -> return n
    Nothing -> msgErro "Número inválido." >> lerInt msg

lerDouble :: String -> IO Double
lerDouble msg = do
  entrada <- lerLinha msg
  case readMaybe entrada of
    Just v  -> return v
    Nothing -> msgErro "Valor inválido. Digite um número." >> lerDouble msg

lerSN :: String -> IO Bool
lerSN msg = do
  r <- lerLinha (msg ++ " (S/N)")
  case r of
    "S" -> return True
    "s" -> return True
    "N" -> return False
    "n" -> return False
    _   -> msgErro "Digite S ou N." >> lerSN msg

cabecalho :: String -> IO ()
cabecalho titulo = do
  clearScreen
  putStrLn "========================================"
  putStrLn "             RH - MANAGER"
  putStrLn "========================================"
  putStrLn (" " ++ titulo)
  putStrLn "----------------------------------------"
  putStrLn ""

msgErro, msgSucesso, msgInfo :: String -> IO ()
msgErro s    = putStrLn ("\n[ERRO] " ++ s)
msgSucesso s = putStrLn ("\n[OK] " ++ s)
msgInfo s    = putStrLn ("\n[INFO] " ++ s)

confirmar :: String -> IO Bool
confirmar msg = do
  r <- lerLinha (msg ++ " (S/N)")
  return (r == "S" || r == "s")