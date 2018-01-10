--11/Programmers.hs
module Programmers where

data OperatingSystem = GnuPlusLinux | OpenBSDPlus | Mac | Windows  
                        deriving (Eq, Show)
data ProgrammingLanguage = Haskell | Agda | Idris | PureScript
                           deriving (Eq, Show)

data Programmer = Programmer { os   :: OperatingSystem
                             , lang :: ProgrammingLanguage }
                  deriving (Eq, Show)

-- exercise

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlus
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [(Programmer os lang) | os   <- allOperatingSystems
                                       , lang <- allLanguages ]



