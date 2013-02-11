{-# LANGUAGE TemplateHaskell #-}
module Igor.Binary
where

import              Control.DeepSeq
import              Data.Binary
import              Data.ByteString
import              Data.DeriveTH
import              Hdis86.Types

$( derive makeBinary ''XMMRegister)
$( derive makeBinary ''X87Register)
$( derive makeBinary ''MMXRegister)
$( derive makeBinary ''DebugRegister)
$( derive makeBinary ''ControlRegister)
$( derive makeBinary ''Segment)
$( derive makeBinary ''Half)
$( derive makeBinary ''GPR)
$( derive makeBinary ''WordSize)
$( derive makeBinary ''Prefix)
$( derive makeBinary ''Immediate )
$( derive makeBinary ''Pointer )
$( derive makeBinary ''Register )
$( derive makeBinary ''Memory )
$( derive makeBinary ''Opcode )
$( derive makeBinary ''Operand )
$( derive makeBinary ''Instruction )
$( derive makeBinary ''Metadata )

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
