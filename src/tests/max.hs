import Igor

main = defineMethod "max" $ do
    [a, b] <- makeInputs 2
    [aLessB] <- makeLabels 1

    jump    aLessB  (a -<- b)
    ret     a
    label   aLessB
    ret     b
