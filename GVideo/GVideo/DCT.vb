Public Class DCT
    Private Shared Quantization_Matrix(,) As Double = _
        {{15, 11, 12, 15, 21, 32, 50, 66}, _
        {11, 11, 13, 18, 24, 46, 62, 73}, _
        {12, 13, 15, 23, 38, 56, 73, 75}, _
        {15, 18, 23, 28, 53, 75, 83, 83}, _
        {21, 24, 38, 53, 67, 95, 103, 103}, _
        {32, 46, 56, 75, 95, 103, 117, 117}, _
        {50, 62, 73, 83, 103, 117, 119, 119}, _
        {66, 73, 75, 83, 103, 117, 119, 119}}
    Public Shared Function Generate_DCT_Matrix(Order As Integer) As Double(,)
        Dim DCT_Matrix(Order - 1, Order - 1) As Double
        Dim Alpha As Double = Math.Sqrt(2 / Order)
        Dim Denominator As Double = 2 * Order
        For Y As Integer = 0 To Order - 1
            DCT_Matrix(0, Y) = Math.Sqrt(1 / Order)
            For X As Integer = 1 To Order - 1
                DCT_Matrix(X, Y) = Alpha * Math.Cos(((2 * Y + 1) * X * Math.PI) / Denominator)
            Next
        Next

        Return DCT_Matrix
    End Function
    Public Shared Function Transform(Matrix(,) As Double, DCT_Matrix(,) As Double, Order As Integer) As Double(,)
        Matrix = Matrix_Multiply(DCT_Matrix, Matrix, Order)
        Matrix = Matrix_Multiply(Matrix, Matrix_Transpose(DCT_Matrix, Order), Order)
        Matrix = Matrix_Quantize(Matrix, Order)
        Return Matrix
    End Function
    Public Shared Function Inverse_Transform(Matrix(,) As Double, DCT_Matrix(,) As Double, Order As Integer) As Double(,)
        Matrix = Matrix_Dequantize(Matrix, Order)
        Matrix = Matrix_Multiply(Matrix_Transpose(DCT_Matrix, Order), Matrix, Order)
        Matrix = Matrix_Multiply(Matrix, DCT_Matrix, Order)
        Return Matrix
    End Function
    Private Shared Function Matrix_Multiply(Matrix_A(,) As Double, Matrix_B(,) As Double, Order As Integer) As Double(,)
        Dim Matrix_C(Order - 1, Order - 1) As Double
        For i As Integer = 0 To Order - 1
            For j As Integer = 0 To Order - 1
                Dim Sum As Double = 0
                For k As Integer = 0 To Order - 1
                    Sum += Matrix_A(i, k) * Matrix_B(k, j)
                Next
                Matrix_C(i, j) = Sum
            Next
        Next
        Return Matrix_C
    End Function
    Private Shared Function Matrix_Transpose(Matrix(,) As Double, Order As Integer) As Double(,)
        Dim Transposed_Matrix(Order - 1, Order - 1) As Double
        For Y As Integer = 0 To Order - 1
            For X As Integer = 0 To Order - 1
                Transposed_Matrix(X, Y) = Matrix(Y, X)
            Next
        Next
        Return Transposed_Matrix
    End Function
    Private Shared Function Matrix_Quantize(Matrix(,) As Double, Order As Integer) As Double(,)
        For Y As Integer = 0 To Order - 1
            For X As Integer = 0 To Order - 1
                Matrix(X, Y) /= Quantization_Matrix(X, Y)
            Next
        Next
        Return Matrix
    End Function
    Private Shared Function Matrix_Dequantize(Matrix(,) As Double, Order As Integer) As Double(,)
        For Y As Integer = 0 To Order - 1
            For X As Integer = 0 To Order - 1
                Matrix(X, Y) *= Quantization_Matrix(X, Y)
            Next
        Next
        Return Matrix
    End Function
End Class
