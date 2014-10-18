Imports System.IO
Imports System.IO.Compression
Imports System.Threading
Imports NAudio
Imports NAudio.Wave
Public Class GVideoCodec
    Public Enum ImageFormat
        PNG
        Bitmap
        JPEG
    End Enum

    Const Block_Size As Integer = 8
    Const Half_Block_Size As Integer = Block_Size / 2

    Const Block_Pixels As Integer = Block_Size ^ 2
    Const Normal_Block_Length As Integer = Block_Pixels * 3
    Const Encoded_Block_Length As Integer = (Block_Pixels + ((Block_Pixels / 4) * 2)) * 2
    Const Encoded_Luma_Block_Length As Integer = Block_Pixels * 2
    Const Encoded_Chroma_Block_Length As Integer = (Block_Pixels / 4) * 2

    Private Shared Move_Prediction_Pattern_X() As Integer = {0, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8}
    Private Shared Move_Prediction_Pattern_Y() As Integer = {0, -3, -2, -1, 1, 2, 3, 4}

    Private Shared DCT_Matrix_Luma(,) As Double
    Private Shared DCT_Matrix_Chroma(,) As Double

    Private Shared Encode_Thread, Play_Thread As Thread

    Public Shared Encoder_Move_Prediction As Boolean = False
    Public Shared Sub Initialize()
        DCT_Matrix_Luma = DCT.Generate_DCT_Matrix(8)
        DCT_Matrix_Chroma = DCT.Generate_DCT_Matrix(4)
    End Sub

#Region "Encode"
    Public Shared Sub Encode(Frame_Path As String, Prefix As String, Format As ImageFormat, Out_File As String, Audio As String, Optional Preview As Image = Nothing)
        Encode_Thread = New Thread(Sub() Encode_Job(Frame_Path, Prefix, Format, Out_File, Audio, Preview))
        Encode_Thread.Start()
    End Sub
    Private Shared Sub Encode_Job(Frame_Path As String, Prefix As String, Format As ImageFormat, Out_File As String, Audio As String, Optional Preview As Image = Nothing)
        Dim Image_Count As Integer
        Dim Extension As String = Nothing
        Select Case Format
            Case ImageFormat.PNG
                Image_Count += Directory.GetFiles(Frame_Path, "*.png", SearchOption.TopDirectoryOnly).Length
                Extension = ".png"
            Case ImageFormat.Bitmap
                Image_Count += Directory.GetFiles(Frame_Path, "*.bmp", SearchOption.TopDirectoryOnly).Length
                Extension = ".bmp"
            Case ImageFormat.JPEG
                Image_Count += Directory.GetFiles(Frame_Path, "*.jpg", SearchOption.TopDirectoryOnly).Length
                Extension = ".jpg"
        End Select

        Dim Output_File As New BinaryWriter(New FileStream(Out_File, FileMode.Create))
        Frame_Path = Frame_Path.Trim()
        If Left(Frame_Path, 1) <> "\" Then Frame_Path &= "\"
        Dim Base_Image As New BitmapImage(New Uri(Frame_Path & Prefix & "0" & Extension))
        Dim Width As Integer = Base_Image.Width
        Dim Height As Integer = Base_Image.Height
        Dim Tile_Width As Integer = Width \ Block_Size
        Dim Tile_Height As Integer = Height \ Block_Size
        Dim Stride As Integer = ((Width * 32 + (32 - 1)) And (Not (32 - 1))) / 8

        Output_File.Write(Convert.ToInt16(Width))
        Output_File.Write(Convert.ToInt16(Height))

        Output_File.Seek(16, SeekOrigin.Begin)
        Dim Audio_Data() As Byte = File.ReadAllBytes(Audio)
        Output_File.Write(Audio_Data)
        Output_File.Seek(12, SeekOrigin.Begin)
        Output_File.Write(Audio_Data.Length)

        Dim Pointers_Offset As Int32 = 16 + Audio_Data.Length
        Dim Base_Offset As Int32 = Pointers_Offset + (Image_Count * 4)
        Output_File.Seek(Base_Offset, SeekOrigin.Begin)
        Dim Previous_Frame((Stride * Height) - 1) As Byte
        For Frame As Integer = 0 To Image_Count - 1
            Dim Image_Name As String = Frame_Path & Prefix & Frame & Extension
            Dim Image As New BitmapImage(New Uri(Image_Name))
            Dim Current_Frame((Stride * Height) - 1) As Byte
            Image.CopyPixels(Current_Frame, Stride, 0)
            Current_Frame = RGBA_To_RGB(Current_Frame)
            Dim Preview_Frame(Current_Frame.Length - 1) As Byte

            Dim Offset As Integer = 0
            Dim Compressed_Size As Integer
            If Frame = 0 Then
                Output_File.Write(Current_Frame)
            Else
                Dim Encoded_Frame(Current_Frame.Length + Tile_Width * Tile_Height) As Byte
                For Tile_Y As Integer = 0 To Tile_Height - 1
                    For Tile_X As Integer = 0 To Tile_Width - 1
                        Dim Temp_X As Integer = Tile_X * Block_Size
                        Dim Temp_Y As Integer = Tile_Y * Block_Size
                        Dim Current_Frame_Block() As Byte = Get_Tile_Block(Current_Frame, Temp_X, Temp_Y, Width)

                        Dim Recycle_Frame As Boolean = False
                        Dim Move_X As Integer = 0, Move_Y As Integer = 0
                        Do
                            Dim Value_X As Integer = Temp_X + Move_Prediction_Pattern_X(Move_X)
                            Dim Value_Y As Integer = Temp_Y + Move_Prediction_Pattern_Y(Move_Y)

                            If (Value_X >= 0 And Value_X + Block_Size <= Width) And (Value_Y >= 0 And Value_Y + Block_Size <= Height) Then
                                Dim Old_Frame_Block() As Byte = Get_Tile_Block(Previous_Frame, Value_X, Value_Y, Width)
                                Recycle_Frame = Current_Frame_Block.SequenceEqual(Old_Frame_Block)
                                If Recycle_Frame Then Exit Do
                            End If
                            If Move_X < 15 Then Move_X += 1 Else Move_X = 0 : Move_Y += 1
                        Loop While Move_Y < 8 And Encoder_Move_Prediction

                        If Recycle_Frame Then
                            Encoded_Frame(Offset) = (Move_X And &HF) Or ((Move_Y And 7) * &H10) Or &H80
                        Else
                            Encoded_Frame(Offset) = 0
                            Dim Encoded_Block() As Byte = Encode_Tile_Block(Current_Frame_Block)
                            Buffer.BlockCopy(Encoded_Block, 0, Encoded_Frame, Offset + 1, Encoded_Block.Length)
                            Offset += Encoded_Block.Length

                            For Y As Integer = 0 To Block_Size - 1
                                Buffer.BlockCopy(Current_Frame_Block, (Y * Block_Size) * 3, Preview_Frame, (Temp_X + ((Temp_Y + Y) * Width)) * 3, Block_Size * 3)
                            Next
                        End If
                        Offset += 1
                    Next Tile_X
                Next Tile_Y

                ReDim Preserve Encoded_Frame(Offset - 1)
                Output_File.Write(Convert.ToInt32(Offset))
                Dim Compressed_Frame As Byte() = Compress(Encoded_Frame)
                Compressed_Size = Compressed_Frame.Length
                Output_File.Write(Compressed_Frame)
            End If

            Previous_Frame = Current_Frame 'Copia o Frame atual para o Frame antigo

            Output_File.Seek(Pointers_Offset, SeekOrigin.Begin)
            Output_File.Write(Base_Offset)
            Pointers_Offset += 4
            If Frame = 0 Then Base_Offset += Current_Frame.Length Else Base_Offset += Compressed_Size + 4
            Output_File.Seek(Base_Offset, SeekOrigin.Begin)

            If Preview IsNot Nothing Then
                Application.Current.Dispatcher.BeginInvoke(Sub() Show_Preview(Preview, Preview_Frame, Width, Height))
            End If
        Next

        Output_File.Close()
    End Sub
    Public Shared Sub Abort()
        Encode_Thread.Abort()
    End Sub
    Private Shared Sub Show_Preview(Preview As Image, Img() As Byte, Width As Integer, Height As Integer)
        Dim Stride As Integer = ((Width * 24 + (24 - 1)) And (Not (24 - 1))) / 8
        Preview.Source = BitmapSource.Create(Width, Height, 96.0F, 96.0F, PixelFormats.Bgr24, Nothing, Img, Stride)
    End Sub
#End Region

#Region "Decode/Play"
    Public Shared Sub Play(Video_File As String, Surface As Image, Window As Window)
        Play_Thread = New Thread(Sub() Play_Job(Video_File, Surface, Window))
        Play_Thread.Start()
    End Sub
    Private Shared Sub Play_Job(Video_File As String, Surface As Image, Window As Window)
        Dim Input_File As New FileStream(Video_File, FileMode.Open)
        Dim Width As Integer = Read16(Input_File, 0)
        Dim Height As Integer = Read16(Input_File, 2)
        Application.Current.Dispatcher.BeginInvoke(Sub() Resize_Window(Window, Width, Height))

        Dim Audio_Length As Integer = Read32(Input_File, 12)
        Dim Audio_Data(Audio_Length - 1) As Byte
        Input_File.Read(Audio_Data, 0, Audio_Length)
        Dim WaveOut As New WaveOut()
        Dim WaveStream As New Mp3FileReader(New MemoryStream(Audio_Data))
        WaveOut.Init(WaveStream)
        WaveOut.Play()

        Dim Offset As Integer = 16 + Audio_Length

        Dim Current_Frame((Width * Height * 3) - 1) As Byte
        Dim Previous_Frame((Width * Height * 3) - 1) As Byte
        Dim Tile_Width As Integer = Width \ Block_Size
        Dim Tile_Height As Integer = Height \ Block_Size

        Dim Frame_Rate As Integer = 24

        Dim Frame As Integer
        Do
            Dim Pointer As Integer = Read32(Input_File, Offset)
            Dim Next_Pointer As Integer = Read32(Input_File, Offset + 4)
            Dim Size As Integer = Next_Pointer - Pointer
            Dim Data(Size - 1) As Byte
            Input_File.Seek(Pointer, SeekOrigin.Begin)
            Input_File.Read(Data, 0, Size)

            If Frame = 0 Then
                Buffer.BlockCopy(Data, 0, Current_Frame, 0, Width * Height * 3)
            Else
                Dim Decompressed_Size As Integer = Read32(Data, 0)
                Dim Temp_Data(Data.Length - 5) As Byte
                Buffer.BlockCopy(Data, 4, Temp_Data, 0, Data.Length - 4)
                Data = Decompress(Temp_Data, Decompressed_Size)
                Dim Data_Offset As Integer = 0
                For Tile_Y As Integer = 0 To Tile_Height - 1
                    For Tile_X As Integer = 0 To Tile_Width - 1
                        Dim Temp_X As Integer = Tile_X * Block_Size
                        Dim Temp_Y As Integer = Tile_Y * Block_Size

                        Dim Recycle_Block As Boolean = Data(Data_Offset) And &H80
                        If Recycle_Block Then
                            Dim Move_X As Integer = Data(Data_Offset) And &HF
                            Dim Move_Y As Integer = (Data(Data_Offset) And &H70) / &H10

                            If Move_X <> 0 Or Move_Y <> 0 Then
                                Dim Block() As Byte = Get_Tile_Block(Previous_Frame, _
                                                                     Temp_X + Move_Prediction_Pattern_X(Move_X), _
                                                                     Temp_Y + Move_Prediction_Pattern_Y(Move_Y), _
                                                                     Width)
                                For Y As Integer = 0 To Block_Size - 1
                                    Buffer.BlockCopy(Block, (Y * Block_Size) * 3, Current_Frame, (Temp_X + ((Temp_Y + Y) * Width)) * 3, Block_Size * 3)
                                Next
                            End If

                            Data_Offset += 1
                        Else
                            Dim Block(Encoded_Block_Length - 1) As Byte
                            Buffer.BlockCopy(Data, Data_Offset + 1, Block, 0, Encoded_Block_Length)
                            Block = Decode_Tile_Block(Block)

                            For Y As Integer = 0 To Block_Size - 1
                                Buffer.BlockCopy(Block, (Y * Block_Size) * 3, Current_Frame, (Temp_X + ((Temp_Y + Y) * Width)) * 3, Block_Size * 3)
                            Next
                            Data_Offset += Encoded_Block_Length + 1
                        End If
                    Next Tile_X
                Next Tile_Y
            End If

            Previous_Frame = Current_Frame
            Application.Current.Dispatcher.BeginInvoke(Sub() Display_Frame(Surface, Current_Frame, Width, Height))

            Frame += 1
            Offset += 4

            Dim FPS As Integer = Get_FPS()
            If FPS <> Frame_Rate Then
                'Força o áudio a entrar em sincronia com o vídeo
                WaveStream.Position = (Frame / Frame_Rate) * WaveStream.WaveFormat.AverageBytesPerSecond
            End If

            Lock_Framerate(Frame_Rate)
            Debug.Print(FPS & " fps")
        Loop
    End Sub
    Public Shared Sub Stop_Playback()
        Play_Thread.Abort()
    End Sub
    Private Shared Sub Display_Frame(Surface As Image, Img() As Byte, Width As Integer, Height As Integer)
        Dim Stride As Integer = ((Width * 24 + (24 - 1)) And (Not (24 - 1))) / 8
        Surface.Source = BitmapSource.Create(Width, Height, 96.0F, 96.0F, PixelFormats.Bgr24, Nothing, Img, Stride)
    End Sub
    Private Shared Sub Resize_Window(Window As Window, Width As Integer, Height As Integer)
        Window.Width = Width + 16
        Window.Height = Height + 38
        Window.Left = (SystemParameters.PrimaryScreenWidth / 2) - (Window.Width / 2)
        Window.Top = (SystemParameters.PrimaryScreenHeight / 2) - (Window.Height / 2)
    End Sub
#End Region

    Private Shared Function Get_Tile_Block(Frame() As Byte, X As Integer, Y As Integer, Width As Integer) As Byte()
        Dim Block(Normal_Block_Length - 1) As Byte
        For TY As Integer = 0 To Block_Size - 1
            Dim Offset As Integer = (X + ((Y + TY) * Width)) * 3
            Dim Block_Offset As Integer = (TY * Block_Size) * 3
            Buffer.BlockCopy(Frame, Offset, Block, Block_Offset, Block_Size * 3)
        Next TY
        Return Block
    End Function
    Private Shared Function Encode_Tile_Block(Block() As Byte) As Byte()
        Dim Encoded_Block(Encoded_Block_Length - 1) As Byte

        Dim Luma_Image(Block_Size - 1, Block_Size - 1) As Double
        Dim Chroma_Blue_Image(Half_Block_Size - 1, Half_Block_Size - 1) As Double
        Dim Chroma_Red_Image(Half_Block_Size - 1, Half_Block_Size - 1) As Double

        Dim Offset As Integer
        For Y As Integer = 0 To Block_Size - 1
            For X As Integer = 0 To Block_Size - 1
                Dim R As Byte = Block(Offset)
                Dim G As Byte = Block(Offset + 1)
                Dim B As Byte = Block(Offset + 2)

                Dim Luma As Byte = 0.257F * R + 0.504F * G + 0.098F * B + 16
                Dim Chroma_Blue As Byte = -0.148F * R - 0.291F * G + 0.439F * B + 128
                Dim Chroma_Red As Byte = 0.439F * R - 0.368F * G - 0.071F * B + 128

                Luma_Image(X, Y) = Luma
                Dim Chroma_X As Integer = X \ 2
                Dim Chroma_Y As Integer = Y \ 2
                Chroma_Blue_Image(Chroma_X, Chroma_Y) = Chroma_Blue
                Chroma_Red_Image(Chroma_X, Chroma_Y) = Chroma_Red

                Offset += 3
            Next
        Next

        Dim Data_Luma As Byte() = Matrix_To_Array(DCT.Transform(Luma_Image, DCT_Matrix_Luma, Block_Size), Block_Size)
        Dim Data_Chroma_Blue As Byte() = Matrix_To_Array(DCT.Transform(Chroma_Blue_Image, DCT_Matrix_Chroma, Half_Block_Size), Half_Block_Size)
        Dim Data_Chroma_Red As Byte() = Matrix_To_Array(DCT.Transform(Chroma_Red_Image, DCT_Matrix_Chroma, Half_Block_Size), Half_Block_Size)

        Buffer.BlockCopy(Data_Luma, 0, Encoded_Block, 0, Data_Luma.Length)
        Buffer.BlockCopy(Data_Chroma_Blue, 0, Encoded_Block, Data_Luma.Length, Data_Chroma_Blue.Length)
        Buffer.BlockCopy(Data_Chroma_Red, 0, Encoded_Block, Data_Luma.Length + Data_Chroma_Blue.Length, Data_Chroma_Red.Length)

        Return Encoded_Block
    End Function
    Private Shared Function Decode_Tile_Block(Encoded_Block() As Byte) As Byte()
        Dim Block(Normal_Block_Length - 1) As Byte

        Dim Luma_Image(,) As Double = DCT.Inverse_Transform(Array_To_Matrix(Encoded_Block, Block_Size), _
                                                            DCT_Matrix_Luma, Block_Size)
        Dim Chroma_Blue_Image(,) As Double = DCT.Inverse_Transform(Array_To_Matrix(Encoded_Block, Half_Block_Size, Encoded_Luma_Block_Length), _
                                                                   DCT_Matrix_Chroma, Half_Block_Size)
        Dim Chroma_Red_Image(,) As Double = DCT.Inverse_Transform(Array_To_Matrix(Encoded_Block, Half_Block_Size, Encoded_Luma_Block_Length + Encoded_Chroma_Block_Length), _
                                                                  DCT_Matrix_Chroma, Half_Block_Size)

        Dim Offset As Integer
        For Y As Integer = 0 To Block_Size - 1
            For X As Integer = 0 To Block_Size - 1
                Dim a0 As Integer = 1192 * (Luma_Image(X, Y) - 16)
                Dim Chroma_X As Integer = X \ 2
                Dim Chroma_Y As Integer = Y \ 2
                Dim a1 As Integer = 1634 * (Chroma_Red_Image(Chroma_X, Chroma_Y) - 128)
                Dim a2 As Integer = 832 * (Chroma_Red_Image(Chroma_X, Chroma_Y) - 128)
                Dim a3 As Integer = 400 * (Chroma_Blue_Image(Chroma_X, Chroma_Y) - 128)
                Dim a4 As Integer = 2066 * (Chroma_Blue_Image(Chroma_X, Chroma_Y) - 128)

                Dim R As Byte = Clip((a0 + a1) >> 10)
                Dim G As Byte = Clip((a0 - a2 - a3) >> 10)
                Dim B As Byte = Clip((a0 + a4) >> 10)

                Block(Offset) = R
                Block(Offset + 1) = G
                Block(Offset + 2) = B
                Offset += 3
            Next
        Next

        Return Block
    End Function
    Private Shared Function Clip(Value As Integer) As Byte
        If Value > &HFF Then
            Return &HFF
        ElseIf Value < 0 Then
            Return 0
        Else
            Return Value And &HFF
        End If
    End Function

    'Conversão de Matriz/Array
    Private Shared Function Matrix_To_Array(Matrix(,) As Double, Order As Integer) As Byte()
        Dim Array((Matrix.Length * 2) - 1) As Byte
        Dim Offset As Integer
        For Y As Integer = 0 To Order - 1
            For X As Integer = 0 To Order - 1
                Dim Value As Integer = Matrix(X, Y) + &H8000
                Array(Offset) = Value And &HFF
                Array(Offset + 1) = (Value And &HFF00) / &H100
                Offset += 2
            Next
        Next
        Return Array
    End Function
    Private Shared Function Array_To_Matrix(Array() As Byte, Order As Integer, Optional Start_Index As Integer = 0) As Double(,)
        Dim Matrix(Order - 1, Order - 1) As Double
        Dim Offset As Integer = Start_Index
        For Y As Integer = 0 To Order - 1
            For X As Integer = 0 To Order - 1
                Matrix(X, Y) = (Array(Offset) Or (Array(Offset + 1) * &H100)) - &H8000
                Offset += 2
            Next
        Next
        Return Matrix
    End Function

    'Compressão
    Private Shared Function Compress(Data() As Byte) As Byte()
        Dim Stream As New MemoryStream
        Dim Compressor As New DeflateStream(Stream, CompressionLevel.Optimal)
        Compressor.Write(Data, 0, Data.Length)
        Compressor.Close()
        Return Stream.ToArray
    End Function
    Private Shared Function Decompress(Data() As Byte, Decompressed_Size As Integer) As Byte()
        Dim Stream As New MemoryStream(Data)
        Dim Compressor As New DeflateStream(Stream, CompressionMode.Decompress)
        Dim Decompressed_Data(Decompressed_Size - 1) As Byte
        Compressor.Read(Decompressed_Data, 0, Decompressed_Size)
        Compressor.Close()
        Return Decompressed_Data
    End Function

    'Conversão de Bgr32 para Bgr24
    Private Shared Function RGBA_To_RGB(RGBA_Data() As Byte) As Byte()
        Dim Pixels(RGBA_Data.Length - (RGBA_Data.Length / 4) - 1) As Byte
        Dim k As Integer
        For j As Integer = 0 To RGBA_Data.Length - 1 Step 4
            Pixels(k) = RGBA_Data(j)
            Pixels(k + 1) = RGBA_Data(j + 1)
            Pixels(k + 2) = RGBA_Data(j + 2)
            k += 3
        Next
        Return Pixels
    End Function

    'Funções para leitura
    Private Shared Function Read32(Data As FileStream, Address As Integer) As Integer
        Data.Seek(Address, SeekOrigin.Begin)
        Return (Data.ReadByte And &HFF) + _
            ((Data.ReadByte And &HFF) << 8) + _
            ((Data.ReadByte And &HFF) << 16) + _
            ((Data.ReadByte And &HFF) << 24)
    End Function
    Private Shared Function Read16(Data As FileStream, Address As Integer) As Integer
        Data.Seek(Address, SeekOrigin.Begin)
        Return (Data.ReadByte And &HFF) + _
            ((Data.ReadByte And &HFF) * &H100)
    End Function
    Private Shared Function Read32(Data() As Byte, Address As Integer) As Integer
        Return (Data(Address) And &HFF) + _
            ((Data(Address + 1) And &HFF) << 8) + _
            ((Data(Address + 2) And &HFF) << 16) + _
            ((Data(Address + 3) And &HFF) << 24)
    End Function
    Private Shared Function Read16(Data() As Byte, Address As Integer) As Integer
        Return (Data(Address) And &HFF) + _
            ((Data(Address + 1) And &HFF) * &H100)
    End Function
End Class
