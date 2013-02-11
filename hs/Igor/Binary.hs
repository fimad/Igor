{-# LANGUAGE TemplateHaskell #-}
module Igor.Binary
where

import              Hdis86.Types
import              Data.Binary
import              Data.DeriveTH

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
