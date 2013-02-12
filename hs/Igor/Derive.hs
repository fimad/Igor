{-# LANGUAGE TemplateHaskell #-}
module Igor.Derive
where

import              Control.DeepSeq
import              Data.Binary
import              Data.ByteString
import              Data.DeriveTH
import              Hdis86.Types

instance NFData ByteString where
    rnf s = Data.ByteString.length s `seq` ()

$( derive makeNFData ''XMMRegister)
$( derive makeNFData ''X87Register)
$( derive makeNFData ''MMXRegister)
$( derive makeNFData ''DebugRegister)
$( derive makeNFData ''ControlRegister)
$( derive makeNFData ''Segment)
$( derive makeNFData ''Half)
$( derive makeNFData ''GPR)
$( derive makeNFData ''WordSize)
$( derive makeNFData ''Prefix)
$( derive makeNFData ''Immediate )
$( derive makeNFData ''Pointer )
$( derive makeNFData ''Register )
$( derive makeNFData ''Memory )
$( derive makeNFData ''Opcode )
$( derive makeNFData ''Operand )
$( derive makeNFData ''Instruction )
$( derive makeNFData ''Metadata )
