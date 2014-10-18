Imports Microsoft.VisualBasic.Devices
Class MainWindow
    Const Test_Video As String = "D:\Gabriel\gvcteste.vid"
    Private Sub Form_Load()
        GVideoCodec.Initialize()
        Hi_Res_Timer_Initialize()
    End Sub
    Private Sub BtnEncode_Click(sender As Object, e As RoutedEventArgs)
        GVideoCodec.Encode("D:\Gabriel\fracture", "frac_", GVideoCodec.ImageFormat.PNG, Test_Video, "D:\FFOutput\frac.mp3", Screen)
    End Sub
    Private Sub BtnPlay_Click(sender As Object, e As RoutedEventArgs)
        Dim OpenDlg As New Microsoft.Win32.OpenFileDialog
        OpenDlg.Filter = "*.vid|*.vid"
        OpenDlg.ShowDialog()
        If OpenDlg.FileName = Nothing Then Exit Sub
        GVideoCodec.Play(OpenDlg.FileName, Screen, Me)
    End Sub
End Class