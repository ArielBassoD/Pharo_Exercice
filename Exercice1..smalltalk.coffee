|createSparseMatrix matrixSparse matrixTraditional createTraditionalMatrix |
createSparseMatrix := [:matrix |
    | numRows numCols sparseMatrix size k |
    numRows := matrix size.
    numCols := matrix first size.    
    size := 0.
    1 to: numRows do: [:i |
        1 to: numCols do: [:j |
            | element |
            element := (matrix at: i) at: j.
            (element = 0) ifFalse: [
                size := size + 1.
            ].
        ].
    ].

    sparseMatrix := (3 to: 1 by: -1) collect: [:index |
        Array new: size.
    ].

    k := 1.
    1 to: numRows do: [:i |
        1 to: numCols do: [:j |
            | element |
            element := (matrix at: i) at: j.
            (element = 0) ifFalse: [
                (sparseMatrix at: 1) at: k put: i.
                (sparseMatrix at: 2) at: k put: j.
                (sparseMatrix at: 3) at: k put: element.
                k := k + 1.
            ].
        ].
    ].

    ^ sparseMatrix.
].

createTraditionalMatrix := [:sparseMatrix |
    | numRows numCols traditionalMatrix k |
    
    numRows := sparseMatrix at: 1 size.
    numCols := sparseMatrix first size.
    
    traditionalMatrix := (Array new: numRows) collect: [:index |
        Array new: numCols withAll: 0.
    ].
    k := 1.
    1 to: sparseMatrix at: 3 size do: [:index |
        | i j value |
        i := (sparseMatrix at: 1) at: k.
        j := (sparseMatrix at: 2) at: k.
        value := (sparseMatrix at: 3) at: k.
        traditionalMatrix at: i at: j put: value.
        k := k + 1.
    ].

    ^ traditionalMatrix.
].

matrixSparse := { {0. 0. 3. 0. 4.}. {0. 0. 5. 7. 0.}. {0. 0. 0. 0. 0.}. {0. 2. 6. 0. 0.} }.
matrixTraditional := (createTraditionalMatrix value: (createSparseMatrix value: matrixSparse)).
matrixTraditional do: [:row |
    row do: [:element |
        Transcript show: element printString, ' '.
    ].
    Transcript show: ''; nl.
].
