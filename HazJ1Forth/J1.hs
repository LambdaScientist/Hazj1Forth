{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char
import System.IO
import Data.Vector.Bit
import Debug.Trace
import Data.Maybe
import System.Directory

import qualified Data.Finite.Internal as Fin

import qualified Data.Binary as B

import qualified Data.ByteString as BS
import Data.Bits
import Numeric (showHex,showIntAtBase)

import Text.PrettyPrint

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans.Except

import qualified Data.Vector.Sized as V 

import GHC.TypeLits
import GHC.Prim
-
import Control.Applicative

instance Num a => Num (Maybe a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger = pure . fromInteger
instance Bits a => Bits (Maybe a) where
  (.&.) = liftA2 (.&.)
  (.|.) = liftA2 (.|.)
  xor = liftA2 xor
  complement = fmap complement
  shift Nothing _ = Nothing
  shift m i = liftA2 shift m (return i)
  rotate Nothing _ = Nothing
  rotate m i = liftA2 rotate m (return i)
  bitSize Nothing = 0
  bitSize (Just x) = bitSize x
  bitSizeMaybe Nothing = Nothing
  bitSizeMaybe (Just x) = bitSizeMaybe x
  isSigned Nothing = False
  isSigned (Just x) = isSigned x
  testBit m i = case liftA2 testBit m (return i) of
                      Nothing -> False
                      Just p -> p
  bit = Just . bit 
  popCount x = case fmap popCount x of 
                    Nothing -> 0 
                    Just p -> p

type Vec = V.Vector 

newtype Literal = Literal MShort
instance Show Literal where
  show (Literal v) = "Literal" ++ " = " ++ show v ++ " = " ++ prettyBinM v
  -- where
  -- litValue = prettyLitValue
prettyLiteral :: Literal -> Doc
prettyLiteral (Literal v) = text "Literal: " 
  $+$ nest 2 (text "Tail = " <> (text.tail) val)
  where 
  val = show v

newtype Jump = Jump MShort
instance Show Jump where
  show (Jump v) = "Jump" ++ " = " ++ show v ++ " = " ++ prettyBinM v
prettyJump :: Jump -> Doc
prettyJump fv@(Jump v) = text "Jump: " 
  $+$ nest 2 (text "Target = " <> (text.show) val
  $+$text "Full command : " <> (text.show) fv)
  where 
  val = prettyTarget <$> v

newtype CondJump = CondJump MShort 
instance Show CondJump where
  show (CondJump v) = "CondJump" ++ " = " ++ show v ++ " = " ++ prettyBinM v
prettyCondJump :: CondJump -> Doc
prettyCondJump fv@(CondJump v) = text "CondJump: " 
  $+$ nest 2 (text "Target = " <> (text.show) val
  $+$text "Full command : " <> (text.show) fv)
  
  where 
  val = prettyTarget <$> v

newtype Call = Call MShort
instance Show Call where
  show (Call v) = "Call" ++ " = " ++ show v ++ " = " ++ prettyBinM v
prettyCall :: Call -> Doc
prettyCall fv@(Call v) = text "Call: " 
  $+$ nest 2 (text "Target = " <> (text.show) val
  $+$text "Full command : " <> (text.show) fv)
  where 
  val = prettyTarget <$> v

prettyTarget :: B.Word16 -> String
prettyTarget bs = (flip sub2End 2) $ binStrs
  where
  binStrs = prettyBin bs

newtype Alu = Alu MShort
instance Show Alu where
  show (Alu v) = "Alu" ++ " = " ++ show v ++ " = " ++ prettyBinM v

prettyAlu :: Alu -> Doc
prettyAlu fv@(Alu bs) = text "ALU: " 
  $+$ nest 2 
  (
  text "r2pc = " <> text r2pc
  $+$text "t' = " <> text t' 
  $+$text "t2n = " <> text t2n
  $+$text "t2r = " <> text t2r
  $+$text "n2tList = " <> text n2tList
  $+$text "rstack = " <> text rstack
  $+$text "dstack = " <> text dstack
  $+$text "Full command : " <> (text.show) fv
  )
  where
  r2pc = [binStrs !! 3]
  t' = subList binStrs 4 7
  t2n = [binStrs !! 8]
  t2r = [binStrs !! 9]
  n2tList = [binStrs !! 10 ]
  rstack = subList binStrs 12 13
  dstack = subList binStrs 14 15
  binStrs = prettyBinM bs

sub2End :: [a] -> Int -> [a]
sub2End xs start = subList xs start $ (length xs) - 1 
subList :: [a] -> Int -> Int -> [a]
subList [] start end = []
subList xs start end | start > end = []
  | otherwise = [xs !! i | i <- [start .. realEnd]] 
  where
  lenXs = (length xs) - 1
  realEnd = if lenXs < end then lenXs else end
  
data Instruction = L Literal
  | J Jump
  | CJ CondJump
  | C Call
  | A Alu
  | UndefinedInstruction MShort
prettyInstruction :: Instruction -> Doc
prettyInstruction (L v) = prettyLiteral v
prettyInstruction (J v) = prettyJump v
prettyInstruction (CJ v) = prettyCondJump v
prettyInstruction (C v) = prettyCall v
prettyInstruction (A v) = prettyAlu v
prettyInstruction (UndefinedInstruction v) 
  = text ("UndefinedInstruction" ++ " = " ++ show v ++ " = " ++ prettyBinM v)

instance Show Instruction where
  show (L v) = show v
  show (J v) = show v
  show (CJ v) = show v
  show (C v) = show v
  show (A v) = show v
  show (UndefinedInstruction v) = "UndefinedInstruction" ++ " = " ++ show v ++ " = " ++ prettyBinM v


bs2instruction :: B.Word16 -> Instruction
bs2instruction bs | bs .&. 0x8000 /= 0 = L (Literal bs')
  | shiftedBS == 0 = J (Jump bs')
  | shiftedBS == 1 = CJ (CondJump bs')
  | shiftedBS == 2 = C (Call bs')
  | shiftedBS == 3 = A (Alu bs')
  | otherwise = UndefinedInstruction bs'
  where
  shiftedBS = shift bs (-13)
  bs' = Just bs

instruction2bs :: Instruction -> MShort
instruction2bs (L (Literal bs)) = bs
instruction2bs (J (Jump bs)) = bs
instruction2bs (CJ (CondJump bs)) = bs
instruction2bs (C (Call bs)) = bs
instruction2bs (A (Alu bs)) = bs
instruction2bs _ = error "Instrcuting does not exist"

word2to16 :: B.Word8 -> B.Word8 -> B.Word16
word2to16 hibyte lobyte = ((high .|. 0x0000) `shift` 8) .|. low
  where
  high = fromIntegral hibyte
  low = fromIntegral lobyte

prettyBinM :: MShort -> String
prettyBinM Nothing = "No ValueTo display"
prettyBinM (Just v) = prettyBin v

prettyBin :: B.Word16 -> String
prettyBin bs = val
  where
  val' = showIntAtBase 2 intToDigit bs ""
  val = replicate (16 - length val') '0' ++ val'

splitByteString :: BS.ByteString -> [B.Word16]
splitByteString bs | bs == BS.empty = []
  | otherwise 
  = word2to16 sh h : splitByteString (BS.tail removeHead)
  -- BS.pack [sh,h] : splitByteString (BS.tail removeHead)
  where
  h = BS.head bs
  removeHead = BS.tail bs
  sh = BS.head $ removeHead
  
-- sx :: [MShort]
sx = [ 0, 1, (-2), (-1) ]-- /* 2-bit sign extension */

type MShort = Maybe B.Word16
type MByte = Maybe B.Word8

type Forth a = ExceptT String (StateT VirtualMachine IO) a


writeListOverVec :: forall n a . (KnownNat n) => [a] -> Vec n a -> Vec n a
writeListOverVec = writeListOverVec' 0

writeListOverVec' :: forall n a . (KnownNat n) => Int -> [a] -> Vec n a -> Vec n a
writeListOverVec' offset xs v = v V.// (zip [offset..targetLen-1] xs)
  where
  targetLen = V.length v

data VirtualMachine = VirtualMachine {
  t :: MShort
  , s :: MShort
  , d :: Vec 0x2000 MShort -- /* data stack */
  , r :: Vec 0x2000 MShort -- /* return stack */
  , pc :: MShort -- /* program counter, counts cells */
  , dsp, rsp :: MByte -- /* point to top entry */
  , memory :: Vec 0x4000 MShort -- /* ram */
  , curInsn :: Instruction
} deriving (Show)

emptyVM =
  VirtualMachine Nothing --t
  Nothing -- s
  (V.replicate Nothing) --d
  (V.replicate Nothing) --r
  Nothing --pc
  Nothing --dsp
  Nothing --rsp
  (V.replicate Nothing) --RAM 
  (UndefinedInstruction Nothing)


insert2RamBlock :: VirtualMachine -> [MShort] -> VirtualMachine
insert2RamBlock vm@VirtualMachine{..} xs = vm{memory = writeListOverVec xs memory }

push :: MShort -> Forth ()
push v = modify
  $ \vm -> let dReplaceAt Nothing = error "Invalid d insert location" 
               dReplaceAt (Just x) = writeListOverVec' x [t vm] (d vm) 
               oldDsp = dsp vm 
               newDsp = (\x -> 0x1f .&. (x + 1)) <$> oldDsp
            in vm { dsp = newDsp
                  , d = dReplaceAt (fromIntegral <$> newDsp)
                  , t = v
                  }

pop :: Forth MShort
pop = get 
  >>= \vm -> let oldDsp = dsp vm 
                 newDsp = (\x -> 0x1f .&. (x - 1)) <$> oldDsp
                 v = t vm
              in put vm { dsp = newDsp
                        , t = V.index (d vm) (convert oldDsp)
                        } >> return v
  
  
convert :: (Num a, KnownNat n, Integral a) => (Maybe a) -> Fin.Finite n
convert Nothing = error "Index does not exist"
convert (Just v) = fromIntegral v 

startVM :: IO VirtualMachine
startVM = do 
  initDir True reportLogLocation
  input <- BS.readFile "j1.bin"
  let binList = splitByteString input
  let instructionList = bs2instruction <$> binList
  writeFile "./reportLoc/Program.txt" $ (render.vcat.map prettyInstruction) instructionList 
  let initVM = (insert2RamBlock emptyVM (map Just binList)){ pc = Just 0x0
                                                           , curInsn = bs2instruction (0x4000 .|. entryPoint)
                                                           }
  writeVMReports reportLogLocation initVM
  return initVM
  where 
    reportLogLocation = "./reportLoc/Start"
    entryPoint = 0
  
  
run :: VirtualMachine -> IO ()
run vm = do
  initDir True reportLogLocation
  (result, newState) <- runStateT (runExceptT exeVM) vm
  case result of
       Left err -> putStrLn $ "Error: " ++ err
       _ -> return () 
  writeVMReports reportLogLocation newState
  run newState
  where 
    reportLogLocation = "./reportLoc/DuringRun"


main = do vm <- startVM
          run vm
          print "Execution Done"

execute = undefined 

exeVM :: Forth ()
exeVM = get 
  >>= \vm -> performInstruction (curInsn vm) 


  -- Not needed : -- let _pc = Nothing
  -- Not needed : -- let _t = Nothing
  -- Moved into the State of VM -- let insn = 0x4000 .|. entryPoint
  -- TODO pcapdev_init();
  -- let _pc = pc + 1
  -- startVM 
  -- return () -- writeVMReports TODO
  -- where reportLogLocation = "./reportLoc/TempRun"

performInstruction :: Instruction -> Forth()
performInstruction (UndefinedInstruction v) = error "Invalid Comand" 
performInstruction (L (Literal v)) = get >>= \vm -> push (v .&. 0x7fff)
performInstruction i = get >>= pInsn 
  where
   pInsn vm = performInstruction' i 
      where
        target = (instruction2bs.curInsn) vm .&. 0x1fff 
        performInstruction' (J v) = put $ vm{ pc = target
                                            , curInsn = removeMaybe (UndefinedInstruction Nothing) 
                                                                    (bs2instruction <$> V.index (memory vm) (convert target))
                                            }
        performInstruction' (CJ v) = do pValue <- pop 
                                        when (pValue == 0)
                                             (put $ vm{ pc = target
                                                      , curInsn = removeMaybe (UndefinedInstruction Nothing) 
                                                                              (bs2instruction <$> V.index (memory vm) (convert target))
                                                      })
        performInstruction' (C v) = 
          (put $ vm{ rsp = newRSP
                   , r = writeListOverVec' (fromIntegral $ removeMaybe 0 newRSP) [shift (pc vm +1) 1] (r vm)
                   , curInsn = removeMaybe (UndefinedInstruction Nothing) (bs2instruction <$> V.index (memory vm) (convert target))
                   })
          where 
            newRSP = (31 .&. (rsp vm + 1) )
        performInstruction' (A (Alu (Just v))) = 
          do when ((v .&. 0x1000) /= 0) 
                  (put $ vm{ pc = shift (V.index (r vm ) rspAsInt ) (-1) })
             bs2AluOp v
             aVM <- get 
             setDR
            where 
              rspAsInt = (fromIntegral $ removeMaybe 0 (rsp vm))          
              setDR = modify 
                    $ \tvm 
                    -> tvm { dsp = 31 .&. ((fromIntegral.removeMaybe 0 $ dsp tvm) 
                                        + (sx !! (fromIntegral (v .&. 3)))) 
                           , rsp = 31 .&. ((fromIntegral.removeMaybe 0 $ rsp tvm) 
                                        + (sx !! (fromIntegral (shift v (-2) .&. 3))) )
                           }
              checkTS :: Forth()
              checkTS = do tvm <- get 
                           when (v .&. 0x80 /= 0) $ put tvm{d = writeListOverVec' (fromIntegral $ removeMaybe 0 (dsp tvm)) [t tvm] (d tvm) }
              checkTR :: Forth()
              checkTR = do tvm <- get 
                           when (v .&. 0x40 /= 0) $ put tvm{r = writeListOverVec' (fromIntegral $ removeMaybe 0 (rsp tvm)) [t tvm] (r tvm) }
              -- if (insn & 0x20) /* s->[t] */
              --           (t==0xf008)?eth_transmit(): (t==0xf002)?(rsp=0):(t==0xf000)?putch(s):(memory[t>>1]=s); /* ! */
              -- t = _t;
        performInstruction' (A _) = error "ALU operation can not be empty"
        performInstruction _ = error "Invalid State"

removeMaybe :: a -> Maybe a -> a
removeMaybe def Nothing = def
removeMaybe _ (Just cur) = cur

bs2AluOp :: B.Word16 -> Forth()
bs2AluOp bs 
  | shiftedBS == 0   = pure () -- = t; break; /* noop */
  | shiftedBS == 1   = get 
                       >>= \vm -> put vm { t = s vm } -- = s; break; /* copy */
  | shiftedBS == 2   = get 
                       >>= \vm -> put vm { t = s vm + t vm } -- = t+s; break; /* + */
  | shiftedBS == 3   = get 
                       >>= \vm -> put vm { t = s vm .&. t vm } -- = t&s; break; /* and */
  | shiftedBS == 4   = get 
                       >>= \vm -> put vm { t = s vm .|. t vm }  -- = t|s; break; /* or */
  | shiftedBS == 5   = get 
                       >>= \vm -> put vm { t = s vm `xor` t vm } -- = t^s; break; /* xor */
  | shiftedBS == 6   = get 
                       >>= \vm -> put vm { t = complement $ t vm } -- = ~t; break; /* invert */
  | shiftedBS == 7   = get 
                       >>= \vm -> put vm { t = if s vm == t vm then -1 else 0 } -- = -(t==s); break; /* = */
  | shiftedBS == 8   = get 
                       >>= \vm -> put vm { t = if s vm < t vm then -1 else 0 } -- = -((signed short)s < (signed short)t); break; /* < */
  | shiftedBS == 9   = get 
                       >>= \vm -> put vm { t = shift (s vm) (0 - (fromIntegral $ removeMaybe 0 (t vm)))  } -- = s>>t; break; /* rshift */
  | shiftedBS == 0xa = get 
                       >>= \vm -> put vm { t = t vm - 1 } -- = t-1; break; /* 1- */
  | shiftedBS == 0xb = get 
                       >>= \vm -> put vm { t = V.index (r vm) (fromIntegral $ removeMaybe 0 (rsp vm)) } -- = r[rsp];  break; /* r@ */
  | shiftedBS == 0xc = undefined -- = (t==0xf008)?eth_poll():(t==0xf001)?1:(t==0xf000)?getch():memory[t>>1]; break; /* @ */
  | shiftedBS == 0xd = get 
                       >>= \vm -> put vm { t = shift (s vm) (fromIntegral $ removeMaybe 0 (t vm))  } -- = s<<t; break; /* lshift */
  | shiftedBS == 0xe = get 
                       >>= \vm -> put vm { t = shift (s vm) 8 + (fromIntegral.removeMaybe 0 $ dsp vm)} -- = (rsp<<8) + dsp; break; /* dsp */
  | shiftedBS == 0xf = get 
                       >>= \vm -> put vm { t = if s vm < t vm then -1 else 0 } -- = -(s<t); break; /* u< */
  | otherwise = error "Not possile alu op"
  where
    shiftedBS = shift bs (-8) .&. 0xf
    bs' = Just bs

-- fileAsInput = do fh <- fileHandle
-- prepareBuffer fh
-- codeLoop fhs
-- where
-- fileHandle = openFile "j1.bin" ReadMode
-- prepareBuffer fh = do hSetBuffering fh NoBuffering
-- hSetEcho fh False
-- codeLoop fh = codeLoop'
-- where codeLoop' = do detectEOF <- hIsEOF fh
-- if not detectEOF
-- then do c <- hGetChar fh
-- putChar c 
-- codeLoop'
-- else return ()

---------------------------------------------------------------------
-- Handy report stuff
---------------------------------------------------------------------

vmReport :: String -> (VirtualMachine -> Doc ) -> VirtualMachine -> IO()
vmReport saveAs toDoc vm = appendToOrCreate saveAs $ (render.toDoc) vm

prettyVec :: (Show a) => Vec n a -> Doc
prettyVec = vcat . map (text.show) . V.toList
prettyVal :: (Show a) => a -> Doc
prettyVal = text.show

vmTreport :: String -> VirtualMachine -> IO()
vmTreport loc = vmReport loc (prettyVal.t)

vmSreport :: String -> VirtualMachine -> IO()
vmSreport loc = vmReport loc (prettyVal.s)

vmPCreport :: String ->VirtualMachine -> IO()
vmPCreport loc = vmReport loc (prettyVal.pc) 

vmDSPreport :: String -> VirtualMachine -> IO()
vmDSPreport loc = vmReport loc (prettyVal.dsp) 

vmRSPreport :: String -> VirtualMachine -> IO()
vmRSPreport loc = vmReport loc (prettyVal.rsp) 

vmDreport :: String -> VirtualMachine -> IO()
vmDreport loc = vmReport loc (prettyVec.d)

vmRreport :: String -> VirtualMachine -> IO()
vmRreport loc = vmReport loc (prettyVec.r)

vmMEMreport :: String -> VirtualMachine -> IO()
vmMEMreport loc = vmReport loc (prettyVec.memory)

vmCurInsnreport :: String -> VirtualMachine -> IO()
vmCurInsnreport loc = vmReport loc (prettyInstruction.curInsn)

appendToOrCreate :: String -> String-> IO()
appendToOrCreate file str = doesFileExist file >>=
  \exist -> if exist then appendFile file (str ++ "\n")
  else writeFile file str
clearDir :: String -> IO()
clearDir file = doesDirectoryExist file >>=
  \exist -> if exist then removeDirectoryRecursive file
  else return ()
initDir :: Bool -> String -> IO()
initDir flag dir = if flag then clearDir dir
  >> createDirectoryIfMissing True dir
  else return ()

writeVMReports :: String -> VirtualMachine -> IO()
writeVMReports dir vm = sequence_ (allReports <*> [vm])
  where
  allReports = [ vmTreport (dir ++ "/tValue.txt")
               , vmSreport (dir ++ "/sValue.txt")
               , vmPCreport (dir ++ "/pcValue.txt")
               , vmDSPreport (dir ++ "/dspValue.txt")
               , vmPCreport (dir ++ "/rspValue.txt")
               , vmDreport (dir ++ "/dValue.txt")
               , vmRreport (dir ++ "/rValue.txt")
               , vmMEMreport (dir ++ "/vmMem.txt")
               , vmCurInsnreport (dir ++ "/vmInsnLog.txt")
               ]