' this form is the startform and collects the data for teams, actual game, tables, settings etc.

Imports System.Xml
Imports VB = Microsoft.VisualBasic

Public Class Datacenter

    'variables for caspar ip and channels
    Public ip As String = "localhost"
    Public Channel1 As String = "1"
    Public Channel2 As String = "2"

    Public channel(10) As String

    Public teamlong(50) As String
    Public teamshort(50) As String

    'next linesactual game, Schweiz;Deutschland;CH;DEU
    Public actual_game_homelong As String
    Public actual_game_awaylong As String
    Public actual_game_homeshort As String
    Public actual_game_awayshort As String

    Public refname(50) As String
    Public reffunction(50) As String

    Public freenamefirstrow(50) As String
    Public freenamesecondrow(50) As String

    Public settings(50) As String

    'next lines, colors for teams
    Public colorhome(8) As String
    Public coloraway(8) As String
    Public title(50) As String

    Public textfield(100) As String
    Public colors(100) As String
    Public numfield(100) As Single
    Public checked(100) As Boolean

    Dim updated As Boolean
    Dim filename As String
    Dim directory As String

    Dim TeamsArrayLong(50) As String
    Dim TeamsArrayShort(50) As String
    Dim hometeam As String
    Dim awayteam As String
    Dim hometeamk As String
    Dim awayteamk As String

    Dim h1rgb As String = "0xffffff"
    Dim h1r As Int32
    Dim h1g As Int32
    Dim h1b As Int32
    Dim h2rgb As String = "0xffffff"
    Dim h2r As Int32
    Dim h2g As Int32
    Dim h2b As Int32

    Dim a1rgb As String = "0x000000"
    Dim a1r As Int32
    Dim a1g As Int32
    Dim a1b As Int32
    Dim a2rgb As String = "0x000000"
    Dim a2r As Int32
    Dim a2g As Int32
    Dim a2b As Int32

    Dim text_Titel As String
    Dim text_template As String
    Dim feld(9) As String
    Dim matrix(30, 9) As String
    Dim Selecteditems As String
    Dim showtemplate As Boolean = False

    Dim teamname
    Dim filenameteam As String
    Dim teams(0 To 30) As String
    Dim startlineup(30) As String
    Dim howmuchplayers As Integer = 0
    Dim substitutes(30) As Boolean
    Dim Players(30, 9) As String
    Dim land(16) As String
    Dim t(6) As String
    Dim c(6) As String
    Dim cc(6) As String
    Dim r(6) As String
    Dim arraycopy(31) As String

    Dim whichtable As String
    Dim tablenumber As String

    Dim showcountrycodes As Boolean = False

    Public Sub Wait(ByVal seconds As Single)
        Static start As Single
        start = VB.Timer()
        Do While VB.Timer() < start + seconds
            System.Windows.Forms.Application.DoEvents()
        Loop
    End Sub

    Private Sub Startform_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        TabControl1.DrawMode = TabDrawMode.OwnerDrawFixed
        Read_Init()

        ComboBox1.Text = actual_game_homelong
        ComboBox2.Text = actual_game_awaylong
        Check_for_table_xmlfile()
        Me.Text = My.Application.Info.Title & " " & My.Application.Info.Version.ToString '+ " - " & My.Application.Info.Description

        PictureBox6.Size = New Size(1920, 940)
        PictureBox6.Location = New Point(0, 40)
        PictureBox6.Visible = True
    End Sub

    Private Sub TabControl1_DrawItem(ByVal sender As Object, ByVal e As System.Windows.Forms.DrawItemEventArgs) Handles TabControl1.DrawItem
        'this is to color the tabs 
        Dim g As Graphics = e.Graphics
        Dim tp As TabPage = TabControl1.TabPages(e.Index)
        Dim br As Brush
        Dim sf As New StringFormat

        Dim r As New RectangleF(e.Bounds.X, e.Bounds.Y + 2, e.Bounds.Width, e.Bounds.Height - 2)

        sf.Alignment = StringAlignment.Center

        Dim strTitle As String = tp.Text

        'If the current index is the Selected Index, change the color
        If TabControl1.SelectedIndex = e.Index Then

            'this is the background color of the tabpage header
            br = New SolidBrush(Color.Red) ' chnge to your choice
            g.FillRectangle(br, e.Bounds)

            'this is the foreground color of the text in the tab header
            br = New SolidBrush(Color.Black) ' change to your choice
            g.DrawString(strTitle, TabControl1.Font, br, r, sf)

        Else

            'these are the colors for the unselected tab pages
            br = New SolidBrush(Color.White) ' Change this to your preference
            g.FillRectangle(br, e.Bounds)
            br = New SolidBrush(Color.Black)
            g.DrawString(strTitle, TabControl1.Font, br, r, sf)
        End If
    End Sub

    Sub Check_for_table_xmlfile()
        filename = "C:\CG_Sports\Fussball\Tabellen.xml"
        directory = "C:\CG_Sports\Fussball\"
        ' checks directory
        If (Not System.IO.Directory.Exists(directory)) Then
            System.IO.Directory.CreateDirectory(directory)
        End If

        'if file not exits, create empty file
        If (Not System.IO.File.Exists(filename)) Then
            My.Computer.FileSystem.WriteAllText(filename, " ", True)

            System.Windows.Forms.MessageBox.Show("Tabellen Datafile not found, creating empty file")
            Fill_xlm_variables()
            Write_Init()
            Clear_all_Tabelle() ' Tab_Tables
        End If
    End Sub

    Sub Fill_xlm_variables()
        'could not find another way as filling all variables with empty values, to avoid errors, replacing data with empty fields
        '' example data variables are filled for first time creating new xml file

        For i = 1 To 10
            channel(i) = "not used"
        Next

        For i = 1 To 50
            teamlong(i) = "not used"
            teamshort(i) = "not used"
            title(i) = "not used"
        Next

        teamlong(1) = "Schweiz"
        teamshort(1) = "CH"
        teamlong(2) = "Deutschland"
        teamshort(2) = "DE"

        For i = 1 To 50
            settings(i) = "not used"
        Next

        actual_game_homelong = "Schweiz"
        actual_game_awaylong = "Deutschland"
        actual_game_homeshort = "CH"
        actual_game_awayshort = "DE"

        'next lines are the referees
        For i = 1 To 50
            refname(i) = "not used"
            reffunction(i) = "not used"
        Next

        'next  lines are the free names
        For i = 1 To 50
            freenamefirstrow(i) = "not used"
            freenamesecondrow(i) = "not used"
        Next

        'next line is colors, 0xff0000;255;0;0;0xffff00;255;255;0;      0x000000;0;0;0;0xffffff;255;255;255
        colorhome(1) = "0x000000"
        colorhome(2) = "0"
        colorhome(3) = "0"
        colorhome(4) = "0"
        colorhome(5) = "0x000000"
        colorhome(6) = "0"
        colorhome(7) = "0"
        colorhome(8) = "0"

        coloraway(1) = "0x000000"
        coloraway(2) = "0"
        coloraway(3) = "0"
        coloraway(4) = "0"
        coloraway(5) = "0x000000"
        coloraway(6) = "0"
        coloraway(7) = "0"
        coloraway(8) = "0"

        For i = 1 To 100
            textfield(i) = "not used"
            numfield(i) = 0
            checked(i) = False
        Next
    End Sub

    Sub Write_Init()
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        'actually only needed to write a new database file, if it not exist
        Dim xmlsettings As New XmlWriterSettings With {.Indent = True}
        ' Initialize the XmlWriter.
        Dim XmlWrt As XmlWriter = XmlWriter.Create(filename, xmlsettings)
        With XmlWrt
            ' Write the Xml declaration.
            .WriteStartDocument()
            ' Write a comment.
            .WriteComment("XML Database.")
            .WriteComment("Datacenter Data file, structure of this database made for Caspar Fussball CLient program")
            ' Write the root element.
            .WriteStartElement("Data")
            ' Start Caspar Dataset
            .WriteStartElement("Fussball")

            .WriteComment("IP for Caspar Server")
            .WriteStartElement("IP")
            .WriteString(ip.ToString())
            .WriteEndElement()

            .WriteComment("Channel Numbers for 10 Channels")
            For i = 1 To 10
                .WriteStartElement("channel" + Trim(Str(i)))
                .WriteString(channel(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("Teams long Name / short Name")
            For i = 1 To 50
                If Trim(teamlong(i)) = vbNullString Then teamlong(i) = "not used"
                .WriteStartElement("teamlong" + Trim(Str(i)))
                .WriteString(teamlong(i).ToString())
                .WriteEndElement()
                If Trim(teamshort(i)) = vbNullString Then teamshort(i) = "not used"
                .WriteStartElement("teamshort" + Trim(Str(i)))
                .WriteString(teamshort(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("actual Game Home Team Long")
            .WriteStartElement("actual_game_homelong")
            .WriteString(actual_game_homelong.ToString())
            .WriteEndElement()

            .WriteComment("actual Game Home Team short")
            .WriteStartElement("actual_game_homeshort")
            .WriteString(actual_game_homeshort.ToString())
            .WriteEndElement()

            .WriteComment("actual Game Away Team Long")
            .WriteStartElement("actual_game_awaylong")
            .WriteString(actual_game_awaylong.ToString())
            .WriteEndElement()

            .WriteComment("actual Game Away Team Short")
            .WriteStartElement("actual_game_awayshort")
            .WriteString(actual_game_awayshort.ToString())
            .WriteEndElement()

            For i = 1 To 50
                If Trim(title(i)) = vbNullString Then title(i) = "not used"
                .WriteStartElement("title" + Trim(Str(i)))
                .WriteString(title(i).ToString())
                .WriteEndElement()
            Next i

            .WriteComment("REF first row, second row")
            For i = 1 To 50
                If Trim(refname(i)) = vbNullString Then refname(i) = "not used"
                .WriteStartElement("refname" + Trim(Str(i)))
                .WriteString(refname(i).ToString())
                .WriteEndElement()
                If Trim(reffunction(i)) = vbNullString Then reffunction(i) = "not used"
                .WriteStartElement("reffunction" + Trim(Str(i)))
                .WriteString(reffunction(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("Free Names")
            For i = 1 To 50
                If Trim(freenamefirstrow(i)) = vbNullString Then freenamefirstrow(i) = "not used"
                .WriteStartElement("freenamefirstrow" + Trim(Str(i)))
                .WriteString(freenamefirstrow(i).ToString())
                .WriteEndElement()
                If Trim(freenamesecondrow(i)) = vbNullString Then freenamesecondrow(i) = "not used"
                .WriteStartElement("freenamesecondrow" + Trim(Str(i)))
                .WriteString(freenamesecondrow(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("settings")
            For i = 1 To 50
                If Trim(settings(i)) = vbNullString Then settings(i) = "not used"
                .WriteStartElement("settings" + Trim(Str(i)))
                .WriteString(settings(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("Colors for Home and Away Team")
            For i = 1 To 8
                .WriteStartElement("colorhome" + Trim(Str(i)))
                .WriteString(colorhome(i).ToString())
                .WriteEndElement()
            Next
            For i = 1 To 8
                .WriteStartElement("coloraway" + Trim(Str(i)))
                .WriteString(coloraway(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("spare for text) ")
            For i = 1 To 100
                If Trim(textfield(i)) = vbNullString Then textfield(i) = "not used"
                .WriteStartElement("textfield" + Trim(Str(i)))
                .WriteString(textfield(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("spare for numbers) ")
            For i = 1 To 100
                .WriteStartElement("numfield" + Trim(Str(i)))
                .WriteString(numfield(i).ToString())
                .WriteEndElement()
            Next

            .WriteComment("Boolean (for example checkbox=true etc.) ")
            For i = 1 To 100
                .WriteStartElement("checked" + Trim(Str(i)))
                .WriteString(checked(i).ToString())
                .WriteEndElement()
            Next
            ' Close the XmlTextWriter.
            .WriteEndDocument()
            .Close()
        End With
    End Sub

    Sub Write_Init_Tabelle() 'xml writer
        filename = "C:\CG_Sports\Fussball\Tabellen.xml"
        directory = "C:\CG_Sports\Fussball\"
        'actually only needed to write a new database file, if it not exist
        Dim xmlsettings As New XmlWriterSettings With {.Indent = True}
        ' Initialize the XmlWriter.
        Dim XmlWrt As XmlWriter = XmlWriter.Create(filename, xmlsettings)
        With XmlWrt
            ' Write the Xml declaration.
            .WriteStartDocument()
            ' Write a comment.
            .WriteComment("XML Database.")
            .WriteComment("Datacenter Data file, structure of this database made for Caspar Fussball CLient program")
            ' Write the root element.
            .WriteStartElement("Data")
            ' Start Tabelle1 Dataset
            For ii = 1 To 10


                .WriteStartElement("Tabelle" + Trim(ii))

                .WriteComment("template name")
                .WriteStartElement("templatename")
                .WriteString(Tab_Tables.Controls.Item("template_text").Text)
                .WriteEndElement()

                .WriteComment("Titel für Tabelle")
                .WriteStartElement("titel")
                .WriteString(Tab_Tables.Controls.Item("titel_text").Text)
                .WriteEndElement()

                .WriteComment("Tabelle")
                For i = 1 To 30

                    .WriteStartElement("col" + Trim(Str(i) + "1"))
                    .WriteString(Tab_Tables.Controls.Item("r" & i).Text)
                    .WriteEndElement()

                    .WriteStartElement("col" + Trim(Str(i) + "2"))
                    .WriteString(Tab_Tables.Controls.Item("nr" & i).Text)
                    .WriteEndElement()

                    .WriteStartElement("col" + Trim(Str(i) + "3"))
                    .WriteString(Tab_Tables.Controls.Item("name" & i).Text)
                    .WriteEndElement()

                    .WriteStartElement("col" + Trim(Str(i) + "4"))
                    .WriteString(Tab_Tables.Controls.Item("vorname" & i).Text)
                    .WriteEndElement()

                    .WriteStartElement("col" + Trim(Str(i) + "5"))
                    .WriteString(Tab_Tables.Controls.Item("pos" & i).Text)
                    .WriteEndElement()

                    .WriteStartElement("col" + Trim(Str(i) + "6"))
                    .WriteString(Tab_Tables.Controls.Item("d" & i).Text)
                    .WriteEndElement()

                    .WriteStartElement("col" + Trim(Str(i) + "7"))
                    .WriteString(Tab_Tables.Controls.Item("dd" & i).Text)
                    .WriteEndElement()

                    .WriteStartElement("col" + Trim(Str(i) + "8"))
                    .WriteString(Tab_Tables.Controls.Item("ddd" & i).Text)
                    .WriteEndElement()
                Next

                .WriteComment("Fields")
                For i = 1 To 8
                    .WriteStartElement("feld" + Trim(Str(i)))
                    .WriteString(Tab_Tables.Controls.Item("feld" & i).Text)
                    .WriteEndElement()
                Next

                ' Close the XmlTextWriter.
                .WriteEndElement()

                .WriteComment("")
                .WriteComment("-----------------------------")
                .WriteComment("")
            Next
            .WriteEndDocument()
            .Close()
        End With
    End Sub

    Sub Read_Init()
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        ' checks directory
        If (Not System.IO.Directory.Exists(directory)) Then
            System.IO.Directory.CreateDirectory(directory)
        End If

        'if file not exits, create empty file
        If (Not System.IO.File.Exists(filename)) Then
            My.Computer.FileSystem.WriteAllText(filename, " ", True)
            ip = "localhost"
            channel(1) = "1"
            'System.Windows.Forms.MessageBox.Show("No Record has been Found")
            Fill_xlm_variables()
            Write_Init()
        End If

        'fills all variables textfield out of Datafile
        Try
            Dim Contactlist As XDocument = XDocument.Load(filename)

            For Each contact As XElement In Contactlist...<Fussball>
                ip = contact.Element("IP")
                For i = 1 To 10
                    channel(i) = contact.Element("channel" + Trim(i))
                Next
                For i = 1 To 50
                    teamlong(i) = contact.Element("teamlong" + Trim(i))
                    If teamlong(i) = "not used" Then teamlong(i) = ""
                    teamshort(i) = contact.Element("teamshort" + Trim(i))
                    'If teamshort(i) = "not used" Then teamshort(i) = ""
                    refname(i) = contact.Element("refname" + Trim(i))
                    reffunction(i) = contact.Element("reffunction" + Trim(i))
                    freenamefirstrow(i) = contact.Element("freenamefirstrow" + Trim(i))
                    freenamesecondrow(i) = contact.Element("freenamesecondrow" + Trim(i))
                    settings(i) = contact.Element("settings" + Trim(i))
                    title(i) = contact.Element("title" + Trim(i))
                Next

                actual_game_homelong = contact.Element("actual_game_homelong")
                actual_game_homeshort = contact.Element("actual_game_homeshort")
                actual_game_awaylong = contact.Element("actual_game_awaylong")
                actual_game_awayshort = contact.Element("actual_game_awayshort")

                For i = 1 To 8
                    colorhome(i) = contact.Element("colorhome" + Trim(i))
                    coloraway(i) = contact.Element("coloraway" + Trim(i))
                Next
                For i = 1 To 5
                    refname(i) = contact.Element("refname" + Trim(i))
                    reffunction(i) = contact.Element("reffunction" + Trim(i))
                Next

                For i = 1 To 15
                    freenamefirstrow(i) = contact.Element("freenamefirstrow" + Trim(i))
                    freenamesecondrow(i) = contact.Element("freenamesecondrow" + Trim(i))
                Next

                For i = 1 To 100
                    textfield(i) = (contact.Element("textfield" + Trim(i)))
                    numfield(i) = contact.Element("numfield" + Trim(i))
                    checked(i) = contact.Element("checked" + Trim(i))
                Next
            Next
        Catch ex As System.IO.IOException
            MessageBox.Show("File nicht vorhanden")
        Catch ex As NullReferenceException
            MessageBox.Show("NullReferenceException: " & ex.Message)
            MessageBox.Show("Stack Trace: " & vbCrLf & ex.StackTrace)
        Catch ex As Exception
        End Try

        For i = 1 To 100
            If textfield(i) = "not used" Then textfield(i) = ""
        Next

        Set_variables()
        Set_labels()
    End Sub

    Sub Set_variables()
        hometeam = actual_game_homelong
        awayteam = actual_game_awaylong
        hometeamk = actual_game_homeshort
        awayteamk = actual_game_awayshort

        h1rgb = colorhome(1)
        h1r = colorhome(2)
        h1g = colorhome(3)
        h1b = colorhome(4)
        h2rgb = colorhome(5)
        h2r = colorhome(6)
        h2g = colorhome(7)
        h2b = colorhome(8)
        a1rgb = coloraway(1)
        a1r = coloraway(2)
        a1g = coloraway(3)
        a1b = coloraway(4)
        a2rgb = coloraway(5)
        a2r = coloraway(6)
        a2g = coloraway(7)
        a2b = coloraway(8)
    End Sub

    Sub Set_labels()
        For i = 1 To 50
            TeamsArrayLong(i) = teamlong(i)
        Next
        Array.Sort(TeamsArrayLong)

        ComboBox1.Items.Clear()
        ComboBox2.Items.Clear()

        For i = 1 To 50
            If Trim(TeamsArrayLong(i)) <> "" Then ComboBox1.Items.Add(TeamsArrayLong(i))
            If Trim(TeamsArrayLong(i)) <> "" Then ComboBox2.Items.Add(TeamsArrayLong(i))
        Next
        ComboBox1.Text = hometeam
        ComboBox2.Text = awayteam

        For i = 1 To 40
            If teamlong(i) = "not used" Then teamlong(i) = ""
            If teamshort(i) = "not used" Then teamshort(i) = ""
            TAB_TEAMS.Controls.Item("teamname" & i).Text = teamlong(i)
            TAB_TEAMS.Controls.Item("tk" & i).Text = teamshort(i)
        Next
        For i = 1 To 40
            If settings(i) = "not used" Then settings(i) = "0"
            Tab_settings.Controls.Item("s" & i).Text = settings(i)
        Next

        For i = 1 To 40
            If title(i) = "not used" Then title(i) = ""
            Tab_Titles.Controls.Item("titel" & i).Text = title(i)
        Next

        homecolor1.BackColor = Color.FromArgb(h1r, h1g, h1b)
        awaycolor1.BackColor = Color.FromArgb(a1r, a1g, a1b)
        homecolor2.BackColor = Color.FromArgb(h2r, h2g, h2b)
        awaycolor2.BackColor = Color.FromArgb(a2r, a2g, a2b)

        actual_home.Text = hometeam
        actual_away.Text = awayteam

        For i = 1 To 5
            If refname(i) = "not used" Then refname(i) = ""
            If reffunction(i) = "not used" Then reffunction(i) = ""
            Tab_REF.Controls.Item("ref_name" & i).Text = refname(i)
            Tab_REF.Controls.Item("ref_vorname" & i).Text = reffunction(i)
        Next
        For i = 1 To 20
            If freenamefirstrow(i) = "not used" Then freenamefirstrow(i) = ""
            If freenamesecondrow(i) = "not used" Then freenamesecondrow(i) = ""
            Tab_REF.Controls.Item("free_name" & i).Text = freenamefirstrow(i)
            Tab_REF.Controls.Item("free_vorname" & i).Text = freenamesecondrow(i)
        Next
    End Sub

    Private Sub Save_team_Click(sender As Object, e As EventArgs) Handles save_team.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        For i = 1 To 40
            TeamsArrayLong(i - 1) = TAB_TEAMS.Controls.Item("teamname" & i).Text
        Next
        For i = 1 To 40
            TeamsArrayShort(i - 1) = TAB_TEAMS.Controls.Item("tk" & i).Text
        Next

        'update combobox with teams
        Me.ComboBox1.Items.Clear()
        Me.ComboBox2.Items.Clear()
        Me.ComboBox1.Items.AddRange(TeamsArrayLong)
        Me.ComboBox2.Items.AddRange(TeamsArrayLong)

        For i = 1 To 40
            Update_data(filename, "Data/Fussball/" + ("teamlong" + Trim(Str(i))), TAB_TEAMS.Controls.Item("teamname" & i).Text)
            Update_data(filename, "Data/Fussball/" + ("teamshort" + Trim(Str(i))), TAB_TEAMS.Controls.Item("tk" & i).Text)
        Next i

        countrycodes_label.Hide()
        showcountrycodes = False

        save_team.BackColor = Color.Green
        Wait(1)
        save_team.BackColor = Color.LightCoral
    End Sub

    Private Sub Save_titles_Click(sender As Object, e As EventArgs) Handles save_titles.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        For i = 1 To 40
            Update_data(filename, "Data/Fussball/" + ("title" + Trim(Str(i))), Tab_Titles.Controls.Item("titel" & i).Text)
        Next i

        save_titles.BackColor = Color.Green
        Wait(1)
        save_titles.BackColor = Color.LightCoral
    End Sub

    Private Sub Button_save_schiri_Click(sender As Object, e As EventArgs) Handles Button_save_schiri.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        For i = 1 To 5
            Update_data(filename, "Data/Fussball/" + ("refname" + Trim(Str(i))), Tab_REF.Controls.Item("ref_name" & i).Text)
            Update_data(filename, "Data/Fussball/" + ("reffunction" + Trim(Str(i))), Tab_REF.Controls.Item("ref_vorname" & i).Text)
        Next i
        For i = 1 To 20
            Update_data(filename, "Data/Fussball/" + ("freenamefirstrow" + Trim(Str(i))), Tab_REF.Controls.Item("free_name" & i).Text)
            Update_data(filename, "Data/Fussball/" + ("freenamesecondrow" + Trim(Str(i))), Tab_REF.Controls.Item("free_vorname" & i).Text)
        Next i
        Button_save_schiri.BackColor = Color.Green
        Wait(1)
        Button_save_schiri.BackColor = Color.LightCoral
    End Sub

    Private Sub Save_settings_Click(sender As Object, e As EventArgs) Handles save_settings.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        For i = 1 To 40
            Update_data(filename, "Data/Fussball/" + ("settings" + Trim(Str(i))), Tab_settings.Controls.Item("s" & i).Text)
        Next i
        save_settings.BackColor = Color.Green
        Wait(1)
        save_settings.BackColor = Color.LightCoral
    End Sub

    Private Sub Save_actualgame_Click(sender As Object, e As EventArgs) Handles save_actualgame.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        ' saves the new selected pair of teams, when changing home team 
        hometeam = ComboBox1.Text
        awayteam = ComboBox2.Text
        actual_game_homelong = hometeam
        actual_game_awaylong = awayteam

        For i = 1 To 50
            If Trim(ComboBox1.Text) = teamlong(i) Then
                hometeamk = teamshort(i)
            End If
        Next

        For i = 1 To 50
            If Trim(ComboBox2.Text) = teamlong(i) Then
                awayteamk = teamshort(i)
            End If
        Next

        actual_game_homelong = hometeam
        actual_game_homeshort = hometeamk
        actual_game_awaylong = awayteam
        actual_game_awayshort = awayteamk

        homecolor1.BackColor = Color.FromArgb(h1r, h1g, h1b)
        awaycolor1.BackColor = Color.FromArgb(a1r, a1g, a1b)

        homecolor2.BackColor = Color.FromArgb(h2r, h2g, h2b)
        awaycolor2.BackColor = Color.FromArgb(a2r, a2g, a2b)

        Update_data(filename, "Data/Fussball/actual_game_homelong", hometeam)
        Update_data(filename, "Data/Fussball/actual_game_awaylong", awayteam)
        Update_data(filename, "Data/Fussball/actual_game_homeshort", hometeamk)
        Update_data(filename, "Data/Fussball/actual_game_awayshort", awayteamk)

        Update_data(filename, "Data/Fussball/colorhome1", h1rgb)
        Update_data(filename, "Data/Fussball/colorhome2", h1r)
        Update_data(filename, "Data/Fussball/colorhome3", h1g)
        Update_data(filename, "Data/Fussball/colorhome4", h1b)
        Update_data(filename, "Data/Fussball/colorhome5", h2rgb)
        Update_data(filename, "Data/Fussball/colorhome6", h2r)
        Update_data(filename, "Data/Fussball/colorhome7", h2g)
        Update_data(filename, "Data/Fussball/colorhome8", h2b)

        Update_data(filename, "Data/Fussball/coloraway1", a1rgb)
        Update_data(filename, "Data/Fussball/coloraway2", a1r)
        Update_data(filename, "Data/Fussball/coloraway3", a1g)
        Update_data(filename, "Data/Fussball/coloraway4", a1b)
        Update_data(filename, "Data/Fussball/coloraway5", a2rgb)
        Update_data(filename, "Data/Fussball/coloraway6", a2r)
        Update_data(filename, "Data/Fussball/coloraway7", a2g)
        Update_data(filename, "Data/Fussball/coloraway8", a2b)
        Set_labels()

        save_actualgame.BackColor = Color.Green
        Wait(1)
        save_actualgame.BackColor = Color.LightCoral
    End Sub

    Private Sub SaveData_Tables_Click(sender As Object, e As EventArgs) Handles SaveData_Tables.Click
        If template_text.Text <> "template name of this table in template folder" Then

            filename = "C:\CG_Sports\Fussball\Tabellen.xml"
            directory = "C:\CG_Sports\Fussball\"

            'saves actual tabelle data
            Update_data(filename, "Data/" + whichtable + "/templatename", Tab_Tables.Controls.Item(titel_text.Text))
            Update_data(filename, "Data/" + whichtable + "/titel", Tab_Tables.Controls.Item(template_text.Text))

            Update_data(filename, "Data/" + whichtable + "/templatename", template_text.Text)
            Update_data(filename, "Data/" + whichtable + "/titel", titel_text.Text)

            For i = 1 To 30
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "1", (Tab_Tables.Controls.Item("r" & Trim(Str(i))).Text))
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "2", (Tab_Tables.Controls.Item("nr" & Trim(Str(i))).Text))
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "3", (Tab_Tables.Controls.Item("name" & Trim(Str(i))).Text))
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "4", (Tab_Tables.Controls.Item("vorname" & Trim(Str(i))).Text))
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "5", (Tab_Tables.Controls.Item("pos" & Trim(Str(i))).Text))
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "6", (Tab_Tables.Controls.Item("d" & Trim(Str(i))).Text))
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "7", (Tab_Tables.Controls.Item("dd" & Trim(Str(i))).Text))
                Update_data(filename, "Data/" + whichtable + "/col" + Trim(Str(i)) + "8", (Tab_Tables.Controls.Item("ddd" & Trim(Str(i))).Text))
            Next
            For i = 1 To 8
                Update_data(filename, "Data/" + whichtable + "/feld" + Trim(Str(i)), (Tab_Tables.Controls.Item("feld" & Trim(Str(i))).Text))
            Next

            SaveData_Tables.BackColor = Color.Green
            Wait(1)
            SaveData_Tables.BackColor = Color.LightCoral
        Else
            SaveData_Tables.BackColor = Color.Red
            SaveData_Tables.Text = "NOT SAVED"
            Wait(2)
            SaveData_Tables.Text = "SAVE Tables"
            SaveData_Tables.BackColor = Color.LightCoral
        End If
    End Sub

    Private Sub TabControl1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabControl1.SelectedIndexChanged
        'HERE ACTION IF TAB CHANGES
        countrycodes_label.Hide()
        labelcountrycodes_data.Hide()

        If TabControl1.SelectedTab Is Tab_Main Then

        ElseIf TabControl1.SelectedTab Is TAB_TEAMS Then

        ElseIf TabControl1.SelectedTab Is Tab_Data_Teams Then
            'Daten.Show()
            PictureBox6.Size = New Size(1920, 940)
            PictureBox6.Location = New Point(0, 40)
            PictureBox6.Visible = True
            Read_teams()


        ElseIf TabControl1.SelectedTab Is Tab_Titles Then

        ElseIf TabControl1.SelectedTab Is Tab_REF Then

        ElseIf TabControl1.SelectedTab Is Tab_settings Then

        ElseIf TabControl1.SelectedTab Is Tab_Tables Then
            Button_tabelle1.PerformClick()

        End If
    End Sub

    Sub Update_data(filename_update, node, updated_data)
        'sub to update direct a specific xml node
        If Trim(updated_data) = "" Then updated_data = "not used"
        Dim MyXML As New XmlDocument()
        Dim MyXMLNode As XmlNode
        MyXML.Load(filename_update)
        MyXMLNode = MyXML.SelectSingleNode(node)
        If MyXMLNode IsNot Nothing Then
            MyXMLNode.ChildNodes(0).InnerText = updated_data
            MyXML.Save(Trim(filename_update))
        Else
            MessageBox.Show("geht nicht")
        End If ' Save the Xml.
    End Sub

    Private Sub Homecolor1_Click(sender As Object, e As EventArgs) Handles homecolor1.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        ' choose teamcolor1 home
        If ColorDialog1.ShowDialog <> Windows.Forms.DialogResult.Cancel Then
            homecolor1.BackColor = ColorDialog1.Color
        End If
        h1r = ColorDialog1.Color.R
        h1g = ColorDialog1.Color.G
        h1b = ColorDialog1.Color.B
        Dim ss1 As String
        Dim ss2 As String
        Dim ss3 As String

        ss1 = CInt(h1r).ToString("x2")
        ss2 = CInt(h1g).ToString("x2")
        ss3 = CInt(h1b).ToString("x2")
        h1rgb = "0x" + ss1 + ss2 + ss3
        homecolor1.BackColor = Color.FromArgb(h1r, h1g, h1b)
        Update_data(filename, "Data/Fussball/colorhome1", h1rgb)
        Update_data(filename, "Data/Fussball/colorhome2", h1r)
        Update_data(filename, "Data/Fussball/colorhome3", h1g)
        Update_data(filename, "Data/Fussball/colorhome4", h1b)
    End Sub

    Private Sub Awaycolor1_Click(sender As Object, e As EventArgs) Handles awaycolor1.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        ' choose teamcolor1 away, needed for recognizing the teams and for scorebug/small result
        If ColorDialog1.ShowDialog <> Windows.Forms.DialogResult.Cancel Then
            awaycolor1.BackColor = ColorDialog1.Color
        End If
        a1r = ColorDialog1.Color.R
        a1g = ColorDialog1.Color.G
        a1b = ColorDialog1.Color.B
        Dim ss1 As String
        Dim ss2 As String
        Dim ss3 As String

        ss1 = CInt(a1r).ToString("x2")
        ss2 = CInt(a1g).ToString("x2")
        ss3 = CInt(a1b).ToString("x2")
        a1rgb = "0x" + ss1 + ss2 + ss3
        awaycolor1.BackColor = Color.FromArgb(a1r, a1g, a1b)
        Update_data(filename, "Data/Fussball/coloraway1", a1rgb)
        Update_data(filename, "Data/Fussball/coloraway2", a1r)
        Update_data(filename, "Data/Fussball/coloraway3", a1g)
        Update_data(filename, "Data/Fussball/coloraway4", a1b)
    End Sub

    Private Sub Homecolor2_Click(sender As Object, e As EventArgs) Handles homecolor2.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        ' choose teamcolor2 home
        If ColorDialog1.ShowDialog <> Windows.Forms.DialogResult.Cancel Then
            homecolor2.BackColor = ColorDialog1.Color
        End If
        h2r = ColorDialog1.Color.R
        h2g = ColorDialog1.Color.G
        h2b = ColorDialog1.Color.B
        Dim ss1 As String
        Dim ss2 As String
        Dim ss3 As String

        ss1 = CInt(h2r).ToString("x2")
        ss2 = CInt(h2g).ToString("x2")
        ss3 = CInt(h2b).ToString("x2")
        h2rgb = "0x" + ss1 + ss2 + ss3
        'homecolor2.BackColor = Color.FromArgb(h2r, h2g, h2b)
        Update_data(filename, "Data/Fussball/colorhome5", h2rgb)
        Update_data(filename, "Data/Fussball/colorhome6", h2r)
        Update_data(filename, "Data/Fussball/colorhome7", h2g)
        Update_data(filename, "Data/Fussball/colorhome8", h2b)
    End Sub

    Private Sub Awaycolor2_Click(sender As Object, e As EventArgs) Handles awaycolor2.Click
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        ' choose teamcolor2 away
        If ColorDialog1.ShowDialog <> Windows.Forms.DialogResult.Cancel Then
            awaycolor2.BackColor = ColorDialog1.Color
        End If
        a2r = ColorDialog1.Color.R
        a2g = ColorDialog1.Color.G
        a2b = ColorDialog1.Color.B
        Dim ss1 As String
        Dim ss2 As String
        Dim ss3 As String

        ss1 = CInt(a2r).ToString("x2")
        ss2 = CInt(a2g).ToString("x2")
        ss3 = CInt(a2b).ToString("x2")
        a2rgb = "0x" + ss1 + ss2 + ss3
        ' awaycolor2.BackColor = Color.FromArgb(a2r, a2g, a2b)
        Update_data(filename, "Data/Fussball/coloraway5", a2rgb)
        Update_data(filename, "Data/Fussball/coloraway6", a2r)
        Update_data(filename, "Data/Fussball/coloraway7", a2g)
        Update_data(filename, "Data/Fussball/coloraway8", a2b)
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged_1(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        ' saves the new selected pair of teams, when changing home team 
        hometeam = ComboBox1.Text
        actual_game_homelong = hometeam

        For i = 0 To 29
            If Trim(ComboBox1.Text) = TeamsArrayLong(i) Then
                hometeamk = TeamsArrayShort(i)
            End If
        Next
        actual_game_homelong = hometeam
        actual_game_homeshort = hometeamk
        actual_home.Text = hometeam
        'Try
        '    Picturehome.Load("C:/CG_Sports/flags/" + LSet(hometeamk, 2) + ".png")
        'Catch ex As Exception
        '    MsgBox("Flagge nicht vorhanden")
        'End Try
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged_1(sender As Object, e As EventArgs) Handles ComboBox2.SelectedIndexChanged
        ' saves the new selected pair of teams, when changing home team 
        awayteam = ComboBox2.Text
        actual_game_awaylong = awayteam

        For i = 0 To 29
            If Trim(ComboBox2.Text) = TeamsArrayLong(i) Then
                awayteamk = TeamsArrayShort(i)
            End If
        Next
        actual_game_awaylong = awayteam
        actual_game_awayshort = awayteamk
        actual_away.Text = awayteam
        'Try
        '    Pictureaway.Load("C:/CG_Sports/flags/" + LSet(awayteamk, 2) + ".png")
        'Catch ex As Exception
        '    MsgBox("Flagge nicht vorhanden")
        'End Try
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'clock in master tab
        Label31.Text = TimeOfDay.ToString("HH:mm:ss")
    End Sub

    Private Sub _countrycodes_Click(sender As Object, e As EventArgs) Handles _countrycodes.Click
        If showcountrycodes = False Then
            countrycodes_label.Show()
            countrycodes_label.BringToFront()
            showcountrycodes = True
        Else
            countrycodes_label.Hide()
            showcountrycodes = False
        End If
    End Sub

    Private Sub Button_default_example_Click(sender As Object, e As EventArgs) Handles Button_default_example.Click
        ' settings default example
        Tab_settings.Controls.Item("s1").Text = "45"  'Halftime minutes
        Tab_settings.Controls.Item("s2").Text = "15"  'overtime  minutes
        Tab_settings.Controls.Item("s3").Text = "overtime"  'overtime text
        Tab_settings.Controls.Item("s4").Text = "1st half"  '1st half text 
        Tab_settings.Controls.Item("s5").Text = "2nd half"  '2nd half text
        Tab_settings.Controls.Item("s6").Text = "1st overtime"  '1st half overtime
        Tab_settings.Controls.Item("s7").Text = "2nd overtime"  '1st half overtime
        Tab_settings.Controls.Item("s8").Text = "kicking "  'foul1
        Tab_settings.Controls.Item("s9").Text = "tripping "  'foul2
        Tab_settings.Controls.Item("s10").Text = "charging "  'foul3
        Tab_settings.Controls.Item("s11").Text = "striking "  'foul4
        Tab_settings.Controls.Item("s12").Text = "pushing "  'foul5
        Tab_settings.Controls.Item("s13").Text = "contact before touching"  'foul6
        Tab_settings.Controls.Item("s14").Text = "blatant holding"  'foul7
        Tab_settings.Controls.Item("s15").Text = "C:\CG_Sports\advertising\batman.png"  'sponsor 1
        Tab_settings.Controls.Item("s16").Text = "C:\CG_Sports\advertising\fanta.png"  'sponsor 2
        Tab_settings.Controls.Item("s17").Text = "C:\CG_Sports\advertising\cola.png"  'sponsor 3
        Tab_settings.Controls.Item("s18").Text = "C:\CG_Sports\advertising\sprite.png"  'sponsor 4
        Tab_settings.Controls.Item("s19").Text = "C:\CG_Sports\cards\yellow.png"  'yellow card logo
        Tab_settings.Controls.Item("s20").Text = "C:\CG_Sports\cards\red.png"  'red card logo
        Tab_settings.Controls.Item("s21").Text = "C:\CG_Sports\cards\yellowred.png"  'yellowred logo
        Tab_settings.Controls.Item("s22").Text = "C:\CG_Sports\cards\empty.png"  'transparent logo to clear yellow card
        Tab_settings.Controls.Item("s23").Text = "C:\CG_Sports\cards\ref.png"  'ref logo
        Tab_settings.Controls.Item("s24").Text = "TOR PLUS"  'text + 1 point home
        Tab_settings.Controls.Item("s25").Text = "TOR PLUS"  'text + 1 point away
        Tab_settings.Controls.Item("s26").Text = "Tor minus"  'text - 1 point home
        Tab_settings.Controls.Item("s27").Text = "Tor minus"  'text -1 point away
        Tab_settings.Controls.Item("s28").Text = "5"  'comport
        Tab_settings.Controls.Item("s29").Text = "29"  '-
        Tab_settings.Controls.Item("s30").Text = "30"  '-
        Tab_settings.Controls.Item("s31").Text = "31" '-
        Tab_settings.Controls.Item("s32").Text = "32" '-
        Tab_settings.Controls.Item("s33").Text = "33" '-
        Tab_settings.Controls.Item("s34").Text = "34" '-
        Tab_settings.Controls.Item("s35").Text = "35" '-
        Tab_settings.Controls.Item("s36").Text = "36" '-
        Tab_settings.Controls.Item("s37").Text = "37" '-
        Tab_settings.Controls.Item("s38").Text = "38" '-
        Tab_settings.Controls.Item("s39").Text = "39"  '-
        Tab_settings.Controls.Item("s40").Text = "40"  '-
    End Sub

    Private Sub Countrycodes_label_Click(sender As Object, e As EventArgs)
        countrycodes_label.Hide()
        showcountrycodes = False
    End Sub

    Private Sub Button_tabelle1_Click(sender As Object, e As EventArgs) Handles Button_tabelle1.Click, Button_tabelle2.Click, Button_tabelle3.Click, Button_tabelle4.Click, Button_tabelle5.Click, Button_tabelle6.Click, Button_tabelle7.Click, Button_tabelle8.Click, Button_tabelle9.Click, Button_tabelle10.Click


        'reads tabellen 1-10 data
        Dim b As Button = DirectCast(sender, Button)
        Dim filename = "C:\CG_Sports\Fussball\tabellen.xml"

        ' gets button number as integer
        tablenumber = Trim(b.Text.Replace("Tabelle", ""))

        ' creates a string form "Tabelle" and the tablenumber for the xml datanode
        whichtable = "Tabelle" + tablenumber

        Label136.Text = "Tables " + tablenumber

        Try
            Dim Contactlist As XDocument = XDocument.Load(filename)
            For Each contact As XElement In Contactlist.Descendants(whichtable)
                'For Each contact As XElement In Contactlist...<Tabelle1>
                titel_text.Text = contact.Element("titel")
                template_text.Text = contact.Element("templatename")

                For i = 1 To 30
                    Tab_Tables.Controls.Item("r" & i).Text = contact.Element("col" + Trim(i) + "1")
                    Tab_Tables.Controls.Item("nr" & i).Text = contact.Element("col" + Trim(i) + "2")
                    Tab_Tables.Controls.Item("name" & i).Text = contact.Element("col" + Trim(i) + "3")
                    Tab_Tables.Controls.Item("vorname" & i).Text = contact.Element("col" + Trim(i) + "4")
                    Tab_Tables.Controls.Item("pos" & i).Text = contact.Element("col" + Trim(i) + "5")
                    Tab_Tables.Controls.Item("d" & i).Text = contact.Element("col" + Trim(i) + "6")
                    Tab_Tables.Controls.Item("dd" & i).Text = contact.Element("col" + Trim(i) + "7")
                    Tab_Tables.Controls.Item("ddd" & i).Text = contact.Element("col" + Trim(i) + "8")
                Next
                For i = 1 To 8
                    Tab_Tables.Controls.Item("feld" & i).Text = contact.Element("feld" + Trim(i))
                Next
            Next
        Catch ex As System.IO.IOException
            MessageBox.Show("File nicht vorhanden")
        Catch ex As NullReferenceException
            MessageBox.Show("NullReferenceException: " & ex.Message)
            MessageBox.Show("Stack Trace: " & vbCrLf & ex.StackTrace)
        Catch ex As Exception
        End Try

        'erases all text fields with "not used" to ""
        Dim a As Control
        For Each a In Tab_Tables.Controls
            If TypeOf a Is TextBox Then
                If a.Text = "not used" Then a.Text = ""
            End If
        Next
        'erster versuch den node per variable zu wechseln
        'For Each contact As XElement In If(variable = 1, Contactlist...<Tabelle1>, Contactlist...<Tabelle2>)
    End Sub

    Private Sub Button_testdata_Click(sender As Object, e As EventArgs) Handles button_testdata.Click
        titel_text.Text = "Testdata " + tablenumber
        template_text.Text = "Test-Template " + tablenumber
        Dim first(31) As String
        Dim num(31) As String
        Dim secondnum(31) As String

        num(1) = "1"
        num(2) = "2"
        num(3) = "3"
        num(4) = "4"
        num(5) = "5"
        num(6) = "6"
        num(7) = "7"
        num(8) = "8"
        num(9) = "9"
        num(10) = "10"
        num(11) = "11"
        num(12) = "12"
        num(13) = "13"
        num(14) = "14"
        num(15) = "15"
        num(16) = "16"
        num(17) = "17"
        num(18) = "18"
        num(19) = "19"
        num(20) = "20"
        num(21) = "21"
        num(22) = "22"
        num(23) = "23"
        num(24) = "24"
        num(25) = "25"
        num(26) = "26"
        num(27) = "27"
        num(28) = "28"
        num(29) = "29"
        num(30) = "30"
        num(31) = "31"

        secondnum(1) = "1"
        secondnum(2) = "2"
        secondnum(3) = "3"
        secondnum(4) = "4"
        secondnum(5) = "5"
        secondnum(6) = "6"
        secondnum(7) = "7"
        secondnum(8) = "8"
        secondnum(9) = "9"
        secondnum(10) = "10"
        secondnum(11) = "11"
        secondnum(12) = "12"
        secondnum(13) = "13"
        secondnum(14) = "14"
        secondnum(15) = "15"
        secondnum(16) = "16"
        secondnum(17) = "17"
        secondnum(18) = "18"
        secondnum(19) = "19"
        secondnum(20) = "20"
        secondnum(21) = "21"
        secondnum(22) = "22"
        secondnum(23) = "23"
        secondnum(24) = "24"
        secondnum(25) = "25"
        secondnum(26) = "26"
        secondnum(27) = "27"
        secondnum(28) = "28"
        secondnum(29) = "29"
        secondnum(30) = "30"
        secondnum(31) = "31"

        first(1) = "Real Madrid"
        first(2) = "FC Barcelona"
        first(3) = "Atlético Madrid"
        first(4) = "Bayern München"
        first(5) = "Juventus Turin"
        first(6) = "FC Sevilla"
        first(7) = "Paris St. Germain"
        first(8) = "Manchester City"
        first(9) = "FC Arsenal"
        first(10) = "Borussia Dortmund"
        first(11) = "FC Porto"
        first(12) = "SSC Neapel"
        first(13) = "AS Roma"
        first(14) = "FC Chelsea"
        first(15) = "FC Villareal"
        first(16) = "FC Liverpool"
        first(17) = "AS Monaco"
        first(18) = "Besitkas"
        first(19) = "Tottenham"
        first(20) = "Bayern Leverkusen"
        first(21) = "Dynamo Kiew"
        first(22) = "Benfica"
        first(23) = "FC Schalke 04"
        first(24) = "FC Basel"
        first(25) = "Olympique Lyon"
        first(26) = "RSC Anderlecht"
        first(27) = "Ajax Amsterdam"
        first(28) = "Sparta Prag"
        first(29) = "FC Brügge"
        first(30) = "FC Valencia"
        first(31) = "Inter Mailand"
        Dim II = 30
        For I = 1 To 30
            Tab_Tables.Controls.Item("name" & I).Text = Str(II)
            II = II - 1
        Next

        Shuffle(first)
        Shuffle(num)
        Shuffle(secondnum)

        For I = 1 To 30
            Tab_Tables.Controls.Item("r" & I).Text = Str(I)
            Tab_Tables.Controls.Item("nr" & I).Text = first(I)
            Tab_Tables.Controls.Item("vorname" & I).Text = num(I) + " : " + secondnum(I)
            Tab_Tables.Controls.Item("pos" & I).Text = "Data1 " + Str(I)
            Tab_Tables.Controls.Item("d" & I).Text = "Data2  " + Str(I)
            Tab_Tables.Controls.Item("dd" & I).Text = "Data3  " + Str(I)
            Tab_Tables.Controls.Item("ddd" & I).Text = "Data4  " + Str(I)
        Next

        For i = 1 To 8
            Tab_Tables.Controls.Item("feld" & i).Text = "column " + Str(i)
        Next
    End Sub

    Private rnd As New Random()

    Public Sub Shuffle(items As String())
        Dim j As Int32
        Dim temp As String

        For n As Int32 = items.Length - 1 To 1 Step -1
            j = rnd.Next(1, n + 1)
            ' Swap them.
            temp = items(n)
            items(n) = items(j)
            items(j) = temp
        Next
    End Sub

    Private Sub Button_templatefields_Click(sender As Object, e As EventArgs) Handles Button_templatefields.Click
        titel_text.Text = "_titeltext"
        template_text.Text = "template name of this table in template folder"
        For I = 1 To 30
            Tab_Tables.Controls.Item("r" & I).Text = "_r" + Trim(Str(I))
            Tab_Tables.Controls.Item("nr" & I).Text = "_team" + Trim(Str(I))
            Tab_Tables.Controls.Item("name" & I).Text = "_1" + Trim(Str(I))
            Tab_Tables.Controls.Item("vorname" & I).Text = "_2" + Trim(Str(I))
            Tab_Tables.Controls.Item("pos" & I).Text = "_3" + Trim(Str(I))
            Tab_Tables.Controls.Item("d" & I).Text = "_4" + Trim(Str(I))
            Tab_Tables.Controls.Item("dd" & I).Text = "_5" + Trim(Str(I))
            Tab_Tables.Controls.Item("ddd" & I).Text = "_6" + Trim(Str(I))
        Next

        For i = 1 To 8
            Tab_Tables.Controls.Item("feld" & i).Text = "_column" + Trim(Str(i))
        Next

        Label136.Text = "Tables shows Caspar Template variables"
    End Sub

    Private Sub Button_show_example_Click(sender As Object, e As EventArgs) Handles Button_show_example.Click
        If showtemplate = False Then
            PictureBox1.Visible = True
            PictureBox1.BringToFront()
            showtemplate = True
        Else
            PictureBox1.Visible = False
            showtemplate = False
        End If
    End Sub

    Private Sub Countrycodes_label_DoubleClick(sender As Object, e As EventArgs) Handles countrycodes_label.DoubleClick
        If showcountrycodes = False Then
            countrycodes_label.Show()
            countrycodes_label.BringToFront()
            showcountrycodes = True
        Else
            countrycodes_label.Hide()
            showcountrycodes = False
        End If
    End Sub

    Sub Clear_Startlineup()
        For i = 1 To 30
            Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i).Visible = True
            Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = True
        Next
    End Sub

    Sub Reset_Startlineup()
        For i = 1 To 30
            Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i).Visible = True
            Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = True
            startlineup(i) = "False"
            substitutes(i) = "False"
            howmuchplayers = 0
        Next
    End Sub

    Sub Set_Startlinup()
        Clear_Startlineup()
        howmuchplayers = 0
        Label166.Text = howmuchplayers
        For i = 1 To 30
            Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i).Visible = True
            Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = True

            If startlineup(i) = "True" Then
                Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Yellow
                Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = False
                If howmuchplayers < 11 Then howmuchplayers = howmuchplayers + 1
                Label166.Text = howmuchplayers
                startlineup(i) = "True"
                substitutes(i) = "False"
            End If
            If substitutes(i) = "True" Then
                Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Red
                Tab_Data_Teams.Controls.Item("db" & i).Visible = False
                startlineup(i) = "False"
                substitutes(i) = "True"
            End If
        Next
    End Sub

    Sub Read_teams()
        filename = "C:\CG_Sports\Fussball\Fussball_Data.xml"
        directory = "C:\CG_Sports\Fussball\"
        Try
            Dim Contactlist As XDocument = XDocument.Load(filename)
            For Each contact As XElement In Contactlist...<Fussball>

                For i = 0 To 30
                    teams(i) = contact.Element("teamlong" + Trim(i))
                    If teams(i) = "not used" Then teams(i) = ""
                Next
            Next

        Catch ex As System.IO.IOException
            MessageBox.Show("File nicht vorhanden")
        Catch ex As NullReferenceException
            MessageBox.Show("NullReferenceException: " & ex.Message)
            MessageBox.Show("Stack Trace: " & vbCrLf & ex.StackTrace)
        Catch ex As Exception
        End Try

        'sort Teams- countrynames for ComboBox1
        Array.Sort(teams)
        'clear combobox1
        ComboBox3.Items.Clear()

        'Add teams in array to combobox
        For i = 0 To 30
            If Trim(teams(i)) > "" Then ComboBox3.Items.Add(teams(i))
        Next
    End Sub

    Private Sub ComboBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox3.SelectedIndexChanged
        'read team-data textfile and makes the datafields visible 
        filenameteam = "C:\CG_Sports\Fussball\" + Trim(ComboBox3.Text) + ".xml"
        landauswahl.Text = Trim(ComboBox3.Text)

        Me.Text = "Daten " + landauswahl.Text
        If (Not System.IO.Directory.Exists("C:\CG_Sports\Fussball")) Then
            System.IO.Directory.CreateDirectory("C:\CG_Sports\Fussball")
        End If
        If (Not System.IO.File.Exists(filenameteam)) Then
            My.Computer.FileSystem.WriteAllText(filenameteam, " ", True)
            Clear_all_Data()
            Write_Data()
        End If
        Read_club()
        If (Not System.IO.File.Exists("C:\CG_Sports\flags\" + Trim(flag.Text) + ".png")) Then
            Label168.BackColor = Color.Red
            Label168.Text = (("C:\CG_Sports\flags\" + Trim(flag.Text) + ".png") + " existiert nicht")
        Else
            Label168.BackColor = Color.Green
            Label168.Text = (("C:\CG_Sports\flags\" + Trim(flag.Text) + ".png") + " OK")
        End If
    End Sub

    Sub Clear_all_Data()
        'SUB, erases all text fields
        Dim a As Control
        For Each a In Tab_Data_Teams.Controls
            If TypeOf a Is TextBox Then
                a.Text = Nothing
            End If
        Next
        Reset_Startlineup()
    End Sub

    Sub Clear_all_Teams()
        'SUB, erases all text fields
        Dim a As Control
        For Each a In TAB_TEAMS.Controls
            If TypeOf a Is TextBox Then
                a.Text = Nothing
            End If
        Next
    End Sub

    Sub Clear_all_Tabelle()
        'erases all text fields
        Dim a As Control
        For Each a In Tab_Tables.Controls
            If TypeOf a Is TextBox Then
                a.Text = Nothing
            End If
        Next
    End Sub

    Sub Clear_all_Schiri()
        'erases all text fields
        Dim a As Control
        For Each a In Tab_REF.Controls
            If TypeOf a Is TextBox Then
                a.Text = Nothing
            End If
        Next
    End Sub

    Sub Clear_all_Titles()
        'erases all text fields
        Dim a As Control
        For Each a In Tab_Titles.Controls
            If TypeOf a Is TextBox Then
                a.Text = Nothing
            End If
        Next
    End Sub

    Sub Write_Data() 'xml writer
        filenameteam = "C:\CG_Sports\Fussball\" + Trim(landauswahl.Text) + ".xml"
        directory = "C:\CG_Sports\Fussball\"
        'actually only needed to write a new database file, if it not exist
        Dim xmlsettings As New XmlWriterSettings With {.Indent = True}
        ' Initialize the XmlWriter.
        Dim XmlWrt As XmlWriter = XmlWriter.Create(filenameteam, xmlsettings)
        With XmlWrt
            ' Write the Xml declaration.
            .WriteStartDocument()
            ' Write a comment.
            .WriteComment("XML Database.")
            .WriteComment("Datacenter Data file, structure of this database made for Caspar Fussball CLient program")
            ' Write the root element.
            .WriteStartElement("Data")
            ' Start Caspar Dataset
            .WriteStartElement("Fussballteam")

            .WriteComment("teamlogo")
            .WriteStartElement("Flag")
            .WriteString(flag.Text)
            .WriteEndElement()

            .WriteComment("Players")
            For i = 1 To 30
                .WriteComment("Player " + Trim(Str(i)))
                'nr
                .WriteStartElement("Player" + Trim(Str(i) + "1"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i).Text)
                End If
                .WriteEndElement()
                'name
                .WriteStartElement("Player" + Trim(Str(i) + "2"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 30).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 30).Text)
                End If
                .WriteEndElement()
                'vorname
                .WriteStartElement("Player" + Trim(Str(i) + "3"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 60).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 60).Text)
                End If
                .WriteEndElement()
                'position
                .WriteStartElement("Player" + Trim(Str(i) + "4"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 90).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 90).Text)
                End If
                .WriteEndElement()
                'data1
                .WriteStartElement("Player" + Trim(Str(i) + "5"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 120).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 120).Text)
                End If
                .WriteEndElement()
                'data2
                .WriteStartElement("Player" + Trim(Str(i) + "6"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 150).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 150).Text)
                End If
                .WriteEndElement()
                'data3
                .WriteStartElement("Player" + Trim(Str(i) + "7"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 180).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 180).Text)
                End If
                .WriteEndElement()
                'data4
                .WriteStartElement("Player" + Trim(Str(i) + "8"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 210).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 210).Text)
                End If
                .WriteEndElement()
                'data5
                .WriteStartElement("Player" + Trim(Str(i) + "9"))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 240).Text)) = "" Then
                    .WriteString("not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 240).Text)
                End If
                .WriteEndElement()

                .WriteStartElement("Startlineup" + Trim(Str(i)))
                .WriteString(startlineup(i))
                .WriteEndElement()

                .WriteStartElement("Substitutes" + Trim(Str(i)))
                .WriteString(substitutes(i))
                .WriteEndElement()
            Next

            .WriteComment("coaches")

            .WriteComment("Coach")
            For i = 1 To 6
                .WriteStartElement("coach" + Trim(Str(i)))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 270).Text)) = "" Then
                    .WriteString("Not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 270).Text)
                End If
                .WriteEndElement()
            Next

            .WriteComment("Assistant Coach 1")
            For i = 1 To 6
                .WriteStartElement("first_assistant" + Trim(Str(i)))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 276).Text)) = "" Then
                    .WriteString("Not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 276).Text)
                End If
                .WriteEndElement()
            Next

            .WriteComment("Assistant Coach 2")
            For i = 1 To 6
                .WriteStartElement("second_assistant" + Trim(Str(i)))
                If Trim((Tab_Data_Teams.Controls.Item("Textbox" & i + 282).Text)) = "" Then
                    .WriteString("Not used")
                Else
                    .WriteString(Tab_Data_Teams.Controls.Item("Textbox" & i + 282).Text)
                End If
                .WriteEndElement()
            Next

            ' Close the XmlTextWriter.
            .WriteEndDocument()
            .Close()
        End With
    End Sub

    Sub Read_club()
        'reads players names of selected club in DATA TAB
        PictureBox6.Visible = False
        filenameteam = "C:\CG_Sports\Fussball\" + Trim(landauswahl.Text) + ".xml"
        Try

            Dim Contactlist As XDocument = XDocument.Load(filenameteam)
            For Each contact As XElement In Contactlist...<Fussballteam>
                flag.Text = contact.Element("Flag")
                For i = 1 To 30
                    If Trim(contact.Element("Player" + Trim(i) + "1")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i).Text = contact.Element("Player" + Trim(i) + "1")
                    End If
                    If Trim(contact.Element("Player" + Trim(i) + "2")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 30).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 30).Text = contact.Element("Player" + Trim(i) + "2")
                    End If

                    If Trim(contact.Element("Player" + Trim(i) + "3")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 60).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 60).Text = contact.Element("Player" + Trim(i) + "3")
                    End If

                    If Trim(contact.Element("Player" + Trim(i) + "4")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 90).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 90).Text = contact.Element("Player" + Trim(i) + "4")
                    End If

                    If Trim(contact.Element("Player" + Trim(i) + "5")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 120).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 120).Text = contact.Element("Player" + Trim(i) + "5")
                    End If

                    If Trim(contact.Element("Player" + Trim(i) + "6")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 150).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 150).Text = contact.Element("Player" + Trim(i) + "6")
                    End If

                    If Trim(contact.Element("Player" + Trim(i) + "7")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 180).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 180).Text = contact.Element("Player" + Trim(i) + "7")
                    End If

                    If Trim(contact.Element("Player" + Trim(i) + "8")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 210).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 210).Text = contact.Element("Player" + Trim(i) + "8")
                    End If

                    If Trim(contact.Element("Player" + Trim(i) + "9")) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 240).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 240).Text = contact.Element("Player" + Trim(i) + "9")
                    End If

                    Tab_Data_Teams.Controls.Item("db" & i).Text = ""   'contact.Element("Startlineup" + Trim(i))
                    startlineup(i) = contact.Element("Startlineup" + Trim(i))
                    Tab_Data_Teams.Controls.Item("db" & i + 100).Text = ""   'contact.Element("Substitutes" + Trim(i))
                    substitutes(i) = contact.Element("Substitutes" + Trim(i))
                Next

                For i = 1 To 6
                    If Trim(contact.Element("coach" + Trim(i))) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 270).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 270).Text = contact.Element("coach" + Trim(i))
                    End If

                    If Trim(contact.Element("first_assistant" + Trim(i))) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 276).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 276).Text = contact.Element("first_assistant" + Trim(i))
                    End If

                    If Trim(contact.Element("second_assistant" + Trim(i))) = "not used" Then
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 282).Text = ""
                    Else
                        Tab_Data_Teams.Controls.Item("Textbox" & i + 282).Text = contact.Element("second_assistant" + Trim(i))
                    End If
                Next
            Next
            Set_Startlinup()
        Catch ex As System.IO.IOException
            MessageBox.Show("File nicht vorhanden")
        Catch ex As NullReferenceException
            MessageBox.Show("NullReferenceException: " & ex.Message)
            MessageBox.Show("Stack Trace: " & vbCrLf & ex.StackTrace)
        Catch ex As Exception
        End Try
    End Sub

    Private Sub Savedata_Click(sender As Object, e As EventArgs) Handles savedata.Click
        'calls SUB writedata to save all changes
        If (Not System.IO.File.Exists("C:\CG_Sports\flags\" + Trim(flag.Text) + ".png")) Then
            MsgBox(("C:\CG_Sports\flags\" + Trim(flag.Text) + ".png") + " existiert nicht")
        End If

        If Trim(flag.Text) = "" Then
            MsgBox("no flag chosen")
        Else
            Write_Data()
            PictureBox6.BringToFront()
            MsgBox("Team " + Trim(landauswahl.Text) + " updated" + Chr(13) + "C:\CG_Sports\Fussball\" + Trim(landauswahl.Text) + ".xml")
            'Datacenter.updated = True
            ComboBox3.Text = ""
            PictureBox6.Size = New Size(1920, 940)
            PictureBox6.Location = New Point(0, 40)
            PictureBox6.Visible = True
            landauswahl.Text = ""
        End If
    End Sub

    Private Sub Button_Testdata_Team_Click(sender As Object, e As EventArgs) Handles Button_Testdata_Team.Click
        Dim last(31) As String
        Dim first(31) As String
        Dim num(31) As String
        Dim tfirst(10) As String
        Dim tlast(10) As String
        Dim tdata(10)

        last(1) = "Stanton"
        last(2) = "Ausman"
        last(3) = "Nedry"
        last(4) = "Candric"
        last(5) = "Longman"
        last(6) = "Chang"
        last(7) = "Vickson"
        last(8) = "Derpston"
        last(9) = "Smith"
        last(10) = "Cooper"
        last(11) = "Peterson"
        last(12) = "Vanguard"
        last(13) = "LaPierre"
        last(14) = "Chase"
        last(15) = "Cromwell"
        last(16) = "Livingston"
        last(17) = "Ericson"
        last(18) = "Greyson"
        last(19) = "Walker"
        last(20) = "Walker"
        last(21) = "Huber"
        last(22) = "Meier"
        last(23) = "Bänziger"
        last(24) = "Weber"
        last(25) = "Bühler"
        last(26) = "Zihlmann"
        last(27) = "Schwaller"
        last(28) = "Kaiser"
        last(29) = "Kehl"
        last(30) = "Randolfi"
        last(31) = "Sprignsteen"

        num(1) = "1"
        num(2) = "2"
        num(3) = "3"
        num(4) = "4"
        num(5) = "5"
        num(6) = "6"
        num(7) = "7"
        num(8) = "8"
        num(9) = "9"
        num(10) = "10"
        num(11) = "11"
        num(12) = "12"
        num(13) = "13"
        num(14) = "14"
        num(15) = "15"
        num(16) = "16"
        num(17) = "17"
        num(18) = "18"
        num(19) = "19"
        num(20) = "20"
        num(21) = "21"
        num(22) = "22"
        num(23) = "23"
        num(24) = "24"
        num(25) = "25"
        num(26) = "26"
        num(27) = "27"
        num(28) = "28"
        num(29) = "29"
        num(30) = "30"
        num(31) = "31"

        first(1) = "Wallace"
        first(2) = "William"
        first(3) = "Scott"
        first(4) = "Harold"
        first(5) = "Steven"
        first(6) = "John"
        first(7) = "Adam"
        first(8) = "Bartholomew"
        first(9) = "Sam"
        first(10) = "Alan"
        first(11) = "Sean"
        first(12) = "Wayne"
        first(13) = "Joseph"
        first(14) = "Carl"
        first(15) = "Andrew"
        first(16) = "Michael"
        first(17) = "Victor"
        first(18) = "Mark"
        first(19) = "Jeff"
        first(20) = "George"
        first(21) = "Paul"
        first(22) = "Joe"
        first(23) = "Günter"
        first(24) = "Kurt"
        first(25) = "Andi"
        first(26) = "Mario"
        first(27) = "Michael"
        first(28) = "Alfio"
        first(29) = "Bert"
        first(30) = "Christian"
        first(31) = "Bruce"

        tfirst(1) = "Giovanni"
        tfirst(2) = "Johann"
        tfirst(3) = "José"
        tfirst(4) = "Carlo"
        tfirst(5) = "Zinédine"
        tfirst(6) = "Pep"
        tfirst(7) = "Ottmar"
        tfirst(8) = "Luis"
        tfirst(9) = "Huub"
        tfirst(10) = "Dino"

        tlast(1) = "Trapattoni"
        tlast(2) = "Cruyff"
        tlast(3) = "Mourinho"
        tlast(4) = "Ancelotti"
        tlast(5) = "Zidane"
        tlast(6) = "Guardiola"
        tlast(7) = "Hitzfeld"
        tlast(8) = "Enrique"
        tlast(9) = "Stevens"
        tlast(10) = "Zoff"

        For i = 1 To 30
            Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Gainsboro
            Tab_Data_Teams.Controls.Item("db" & i).Visible = True
            Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = True
        Next

        For i = 1 To 11
            Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Yellow
            Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = False
            Label166.Text = 11
            startlineup(i) = "True"
            substitutes(i) = "False"
        Next

        For i = 12 To 15
            Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Red
            Tab_Data_Teams.Controls.Item("db" & i).Visible = False
            startlineup(i) = "False"
            substitutes(i) = "True"
        Next

        Shuffle(last)
        Shuffle(first)
        Shuffle(tlast)
        Shuffle(tfirst)
        Shuffle(num)

        For I = 1 To 30
            'Controls.Item("nr" & I).Text = Str(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I).Text = num(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 30).Text = last(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 60).Text = first(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 90).Text = "Pos. " + Str(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 120).Text = Trim(landauswahl.Text) + " Data1  " + Str(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 150).Text = Trim(landauswahl.Text) + " Data2  " + Str(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 180).Text = Trim(landauswahl.Text) + " Data3  " + Str(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 210).Text = Trim(landauswahl.Text) + " Data4  " + Str(I)
            Tab_Data_Teams.Controls.Item("Textbox" & I + 240).Text = Trim(landauswahl.Text) + " Data5  " + Str(I)
        Next

        Tab_Data_Teams.Controls.Item("Textbox271").Text = tlast(1)
        Tab_Data_Teams.Controls.Item("Textbox272").Text = tfirst(1)
        Tab_Data_Teams.Controls.Item("Textbox273").Text = "Trainer"
        Tab_Data_Teams.Controls.Item("Textbox274").Text = "1.1.1950"
        Tab_Data_Teams.Controls.Item("Textbox275").Text = "four Liga titles in row "
        Tab_Data_Teams.Controls.Item("Textbox276").Text = "two times Trainer of the Year"

        Tab_Data_Teams.Controls.Item("Textbox277").Text = tlast(2)
        Tab_Data_Teams.Controls.Item("Textbox278").Text = tfirst(2)
        Tab_Data_Teams.Controls.Item("Textbox279").Text = "Co-Trainer"
        Tab_Data_Teams.Controls.Item("Textbox280").Text = "2.2.1960"
        Tab_Data_Teams.Controls.Item("Textbox281").Text = "six Liga titles in row "
        Tab_Data_Teams.Controls.Item("Textbox282").Text = "one times Trainer of the Year"

        Tab_Data_Teams.Controls.Item("Textbox283").Text = tlast(3)
        Tab_Data_Teams.Controls.Item("Textbox284").Text = tfirst(3)
        Tab_Data_Teams.Controls.Item("Textbox285").Text = "Co-Co-Trainer"
        Tab_Data_Teams.Controls.Item("Textbox286").Text = "3.3.1970"
        Tab_Data_Teams.Controls.Item("Textbox287").Text = "one Liga titles in row "
        Tab_Data_Teams.Controls.Item("Textbox288").Text = "nine times Trainer of the Year"
    End Sub

    ' Private Sub db1_Click(sender As Object, e As EventArgs) Handles db1.Click
    Private Sub DB1_Click(sender As Object, e As EventArgs) Handles db1.Click, db2.Click, db3.Click, db4.Click, db5.Click, db6.Click, db7.Click, db8.Click, db9.Click, db10.Click,
         db11.Click, db12.Click, db13.Click, db14.Click, db15.Click, db16.Click, db17.Click, db18.Click, db19.Click, db20.Click,
        db21.Click, db22.Click, db23.Click, db24.Click, db25.Click, db26.Click, db27.Click, db28.Click, db29.Click, db30.Click
        Dim b As Button = DirectCast(sender, Button)

        For i = 1 To 30
            If b.Name = Tab_Data_Teams.Controls.Item("db" & i).Name Then
                If startlineup(i) = "True" Then
                    Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Gainsboro
                    Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = True
                    If howmuchplayers > 0 Then howmuchplayers = howmuchplayers - 1
                    Label166.Text = howmuchplayers
                    startlineup(i) = "False"
                    substitutes(i) = "False"
                    Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Gainsboro
                Else
                    If howmuchplayers = 11 Then MsgBox("schon 11 Spieler gewählt")
                    If howmuchplayers < 11 Then
                        Tab_Data_Teams.Controls.Item("db" & i).BackColor = Color.Yellow
                        Tab_Data_Teams.Controls.Item("db" & i + 100).Visible = False
                        If howmuchplayers < 11 Then howmuchplayers = howmuchplayers + 1
                        Label166.Text = howmuchplayers
                        startlineup(i) = "True"
                        substitutes(i) = "False"
                    End If
                End If
            End If
        Next
    End Sub

    Private Sub DB101_Click(sender As Object, e As EventArgs) Handles db101.Click, db102.Click, DB103.Click, DB104.Click, DB105.Click, DB106.Click, DB107.Click, DB108.Click, DB109.Click, DB110.Click,
         DB111.Click, DB112.Click, DB113.Click, DB114.Click, DB115.Click, DB116.Click, DB117.Click, DB118.Click, DB119.Click, DB120.Click,
        DB121.Click, DB122.Click, DB123.Click, DB124.Click, DB125.Click, DB126.Click, DB127.Click, DB128.Click, DB129.Click, DB130.Click
        Dim b As Button = DirectCast(sender, Button)

        For i = 1 To 30
            If b.Name = Tab_Data_Teams.Controls.Item("db" & i + 100).Name Then
                If substitutes(i) = "True" Then

                    Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Gainsboro
                    Tab_Data_Teams.Controls.Item("db" & i).Visible = True
                    Label59.Text = howmuchplayers
                    startlineup(i) = "False"
                    substitutes(i) = "False"
                Else

                    Tab_Data_Teams.Controls.Item("db" & i + 100).BackColor = Color.Red
                    Tab_Data_Teams.Controls.Item("db" & i).Visible = False
                    startlineup(i) = "false"
                    substitutes(i) = "True"
                End If
            End If
        Next
    End Sub



    Private Sub Label177_Click(sender As Object, e As EventArgs) Handles Label177.Click
        'shows or hides countrycodes label
        If showcountrycodes = False Then
            labelcountrycodes_data.Show()
            labelcountrycodes_data.BringToFront()
            showcountrycodes = True
        Else
            labelcountrycodes_data.Hide()
            showcountrycodes = False
        End If
    End Sub

    Private Sub Button_Clear_All_Data_Click(sender As Object, e As EventArgs) Handles Button_Clear_All_Data.Click
        'calls sub to erase all textfields with a message control box YES-NO
        labelcountrycodes_data.Hide()
        If MessageBox.Show("alle Datenfelder löschen ?", "EXIT", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then Clear_all_Data()
    End Sub

    Private Sub Clearall_schiri_button_Click(sender As Object, e As EventArgs) Handles clearall_schiri_button.Click
        If MessageBox.Show("alle Datenfelder löschen ?", "EXIT", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then Clear_all_Schiri()
    End Sub

    Private Sub Clearall_teams_button_Click(sender As Object, e As EventArgs) Handles clearall_teams_button.Click
        If MessageBox.Show("alle Datenfelder löschen ?", "EXIT", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then Clear_all_Teams()
    End Sub

    Private Sub Button_Clear_All_Click(sender As Object, e As EventArgs) Handles Button_Clear_All.Click
        If MessageBox.Show("alle Datenfelder löschen ?", "EXIT", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then Clear_all_Tabelle()
    End Sub

    Private Sub Button_Clear_All_Titles_Click(sender As Object, e As EventArgs) Handles Button_Clear_All_Titles.Click
        If MessageBox.Show("alle Datenfelder löschen ?", "EXIT", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then Clear_all_Titles()
    End Sub

    Private Sub Buttonexit_Click_1(sender As Object, e As EventArgs) Handles Buttonexit.Click
        End
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) 
        Clear_all_Data()
        MsgBox("please please don't save your team now.....")
        'player
        TextBox1.Text = "CG_nr"
        TextBox31.Text = "CG_name"
        TextBox61.Text = "CG_firstname"
        TextBox91.Text = "CG_pos"
        TextBox121.Text = "CG_Data1"
        TextBox151.Text = "CG_Data2"
        TextBox181.Text = "CG_Data3"
        TextBox211.Text = "CG_Data4"
        TextBox241.Text = "CG_Data5"

        'coach
        TextBox271.Text = "CG_name"
        TextBox272.Text = "CG_firstname"
        TextBox273.Text = "CG_Data1"
        TextBox274.Text = "CG_Data2"
        TextBox275.Text = "CG_Data3"
        TextBox276.Text = "CG_Data4"
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        ' settings default example
        Clear_all_Teams()
        TAB_TEAMS.Controls.Item("teamname1").Text = "Schweiz"
        TAB_TEAMS.Controls.Item("teamname2").Text = "Deutschland"
        TAB_TEAMS.Controls.Item("teamname3").Text = "Italien"
        TAB_TEAMS.Controls.Item("teamname4").Text = "Spanien"
        TAB_TEAMS.Controls.Item("teamname5").Text = "Portugal"
        TAB_TEAMS.Controls.Item("teamname6").Text = "Frankreich"
        TAB_TEAMS.Controls.Item("teamname7").Text = "Dänemark"
        TAB_TEAMS.Controls.Item("teamname8").Text = "Niederlande"

        TAB_TEAMS.Controls.Item("tk1").Text = "CH"
        TAB_TEAMS.Controls.Item("tk2").Text = "DE"
        TAB_TEAMS.Controls.Item("tk3").Text = "IT"
        TAB_TEAMS.Controls.Item("tk4").Text = "ES"
        TAB_TEAMS.Controls.Item("tk5").Text = "PT"
        TAB_TEAMS.Controls.Item("tk6").Text = "FR"
        TAB_TEAMS.Controls.Item("tk7").Text = "DK"
        TAB_TEAMS.Controls.Item("tk8").Text = "NL"

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        ' settings default example
        Clear_all_Titles()
        Tab_Titles.Controls.Item("titel1").Text = "Schweiz - Deutschland"
        Tab_Titles.Controls.Item("titel2").Text = "EM Qualifikation"
        Tab_Titles.Controls.Item("titel3").Text = "Stadion Letzigrund Zürich"
        Tab_Titles.Controls.Item("titel4").Text = "Samstag, 28.1.2025"
        Tab_Titles.Controls.Item("titel5").Text = "Viertelfinal"
        Tab_Titles.Controls.Item("titel6").Text = "Halbfinal"
        Tab_Titles.Controls.Item("titel7").Text = "Final"
        Tab_Titles.Controls.Item("titel8").Text = "Bratwurst 4.50"
    End Sub


End Class