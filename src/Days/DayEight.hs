module Days.DayEight
  ( parta
  , partb )
  where

import Data.List

import Types

instructionPassed :: Int -> [Int] -> Bool
instructionPassed next past = (find (== next) past) /= Nothing

loopTerminates :: Int -> [HandheldOps] -> Int -> [Int] -> (Bool, Int)
loopTerminates oldAcc instructions instructionPointer past =
  let currentInstruction = instructions !! instructionPointer
  in case currentInstruction of
       Nop _ -> getNewInstruction 1 oldAcc
       Acc val -> getNewInstruction 1 (oldAcc + val)
       Jmp val -> getNewInstruction val oldAcc
  where getNewInstruction offset accumulator = do
          let newInstruction = instructionPointer + offset
          if instructionPassed newInstruction past || newInstruction == length instructions
            then (newInstruction == length instructions, accumulator)
            else loopTerminates accumulator instructions newInstruction (newInstruction:past)

countChangableOp :: Int -> HandheldOps -> Int
countChangableOp a b = case b of
                         Nop _ -> a + 1
                         Jmp _ -> a + 1
                         _ -> a

changeOp :: HandheldOps -> HandheldOps
changeOp a = case a of
               Nop val -> Jmp val
               Jmp val -> Nop val
               _ -> a

newInstructionList :: Int -> [HandheldOps] -> [HandheldOps]
newInstructionList 0 (h:rest) = case h of
                                   Acc _ -> (h:newInstructionList 0 rest)
                                   _ -> (changeOp h:rest)
newInstructionList n (h:rest) = case h of
                                   Acc _ -> (h:newInstructionList n rest)
                                   _ -> (h:newInstructionList (n - 1) rest)
newInstructionList _ a = a

tillFirst :: Int -> [HandheldOps] -> Int -> [Int] -> Int
tillFirst oldAcc instructions instructionPointer past =
  let currentInstruction = instructions !! instructionPointer
  in case currentInstruction of
       Nop _ -> getNewInstruction 1 oldAcc
       Acc val -> getNewInstruction 1 (oldAcc + val)
       Jmp val -> getNewInstruction val oldAcc
  where getNewInstruction offset accumulator = do
          let newInstruction = instructionPointer + offset
          if instructionPassed newInstruction past
            then accumulator
            else tillFirst accumulator instructions newInstruction (newInstruction:past)

parta :: [HandheldOps] -> Int
parta ops = tillFirst 0 ops 0 []

partb :: [HandheldOps] -> Int
partb ops =
  let count = foldl countChangableOp 0 ops
      listOfNewInstructions = map (\n -> newInstructionList n ops) [0..(count - 1)]
      instructionLoops = map (\x -> loopTerminates 0 x 0 []) listOfNewInstructions
      correctInstructionSet = head $ filter fst instructionLoops
  in snd correctInstructionSet
