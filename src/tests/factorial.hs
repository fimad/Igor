import Igor

main = defineMethod "factorial" $ do
    [n]                     <- makeInputs 1
    [tmp, total]            <- makeLocals 2
    [begin]                 <- makeLabels 1

    move    total   tmp

    move    total   (1 :: Integer)
    move    tmp     n

    label   begin
    mul     total   total   tmp
    sub     tmp     tmp     (1 :: Integer)
    jump    begin   ((1 :: Integer) -<- tmp)

    ret     total
