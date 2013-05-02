import Igor

main = defineMethod "xorEnc" $ do
    [array, length, key]        <- makeInputs 3
    [tmp, current, arrayEnd]    <- makeLocals 3
    [loop]                      <- makeLabels 1

    mul     arrayEnd            length              (4 :: Integer)
    add     arrayEnd            arrayEnd            array
    move    current             array

    label   loop
    xor     (W,current,0)       key                 (R,current,0)
    add     current             current             (4 :: Integer)
    jump    loop                ( current -!=- arrayEnd )

