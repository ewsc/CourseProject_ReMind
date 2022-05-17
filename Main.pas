Unit Main;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Grids, Vcl.StdCtrls, System.IOUtils,
    System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, Vcl.ToolWin, Vcl.ComCtrls,
    Vcl.ExtCtrls, Vcl.AppEvnts;

Type
    TMainForm = class(TForm)
        MainGrid: TStringGrid;
    MainToolBar: TToolBar;
    AppIconsList: TImageList;
    MainActionList: TActionList;
    aAddNew: TAction;
    tbAddNew: TToolButton;
    aAddRecord: TAction;
    aEditRecord: TAction;
    aDeleteRecord: TAction;
    aClearAll: TAction;
    tbDeleteAll: TToolButton;
    aNotesEditor: TAction;
    tbNotes: TToolButton;
    MainTrayIcon: TTrayIcon;
    MainApplicationEvents: TApplicationEvents;
    aDrawNote: TAction;
    tbDrawNote: TToolButton;
    aSetNotification: TAction;
    tbSetNotification: TToolButton;
    aChangeTheme: TAction;
    tbChangeTheme: TToolButton;
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure Settings1Click(Sender: TObject);
    Procedure AddNew1Click(Sender: TObject);
    Procedure aAddNewExecute(Sender: TObject);
    Procedure aAddRecordExecute(Sender: TObject);
    Procedure MainGridDblClick(Sender: TObject);
    Procedure aEditRecordExecute(Sender: TObject);
    Procedure MainGridFixedCellClick(Sender: TObject; ACol, ARow: Integer);
    Procedure aDeleteRecordExecute(Sender: TObject);
    Procedure aClearAllExecute(Sender: TObject);
    Procedure tbDeleteAllClick(Sender: TObject);
    Procedure aNotesEditorExecute(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    Procedure MainGridKeyPress(Sender: TObject; var Key: Char);
    Procedure MainApplicationEventsMinimize(Sender: TObject);
    Procedure MainApplicationEventsException(Sender: TObject; E: Exception);
    Procedure MainTrayIconDblClick(Sender: TObject);
    Procedure tbDrawClick(Sender: TObject);
    Procedure aDrawNoteExecute(Sender: TObject);
    Procedure aSetNotificationExecute(Sender: TObject);
    procedure aChangeThemeExecute(Sender: TObject);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
    MainForm: TMainForm;

Implementation

{$R *.dfm}

Uses Settings, AddNew, Edit, Notes, Draw, Notification;

Const
    ProgramVersion = '1.3.1';
    PriorityArray: Array[1..3] of String = ('Low', 'Normal', 'High');
    FixedRowCaptionsArray: Array[0..3] of String = ('№', 'Information', 'Due To', 'Priority');
    FILE_LOGS: String = '\ReMind\logs.dat';
    FILE_REMINDERS: String = '\ReMind\reminders.dat';
    

Type
    TLog = Record
        Number: Integer;
        About: String[255];
        Priority: Byte;
        DueTo: TDateTime;
    End;
    TListElem = ^TElement;
    TElement = Record
        Data: TLog;
        PNext: TListElem;
        PPrev: TListElem;
    End;
    TList = Record
        PFirst: TListElem;
        PLast: TListElem;
    End;

Var
    MainList, SortedList: TList;
    OrderAsc: Boolean = True;
    EditingRowIndicator: Integer;
    IsLightTheme: Boolean = True;

Procedure DeleteList(var GivenList : TList);
Var
    PNext, PDel: TListElem;
    IsCorrect: Boolean;
Begin
    IsCorrect := GivenList.PFirst = nil;
    If not IsCorrect then
    Begin
        PNext := GivenList.PFirst;
        While PNext <> nil do begin
            PDel := PNext;
            PNext := PNext^.PNext;
            Dispose(PDel);
        End;

        GivenList.PFirst := nil;
        GivenList.PLast := nil;
    End;
End;

Procedure AddNextElement(const NewElement : TListElem; var GivenList: TList);
Var
    IsNilElement: Boolean;
Begin
    IsNilElement := NewElement = nil;

    If not IsNilElement then
    Begin
        NewElement^.PNext := nil;
        NewElement^.PPrev := nil;
        If (GivenList.PFirst = nil) then
        Begin
            GivenList.PFirst := NewElement;
            GivenList.PLast := NewElement;
        End
        Else
        Begin
            GivenList.PLast^.PNext := NewElement;
            NewElement^.PPrev := GivenList.PLast;
            GivenList.PLast := NewElement;
        End;
    End;
End;

Procedure NameFixedRows(MainGrid: TStringGrid);
Var
    I: Integer;
Begin
    For I := 0 to 3 do
    Begin
        With MainGrid do
        Begin
            Cells[I, 0] := FixedRowCaptionsArray[I];
        End;    
    End;
End;

Function GetListSize(const GivenList: TList) : Integer;
Var
    Size: Integer;
    CurrentElem: TListElem;
Begin
    CurrentElem := GivenList.PFirst;
    Size := 0;
    While CurrentElem <> nil do
    Begin
        Inc(Size);
        CurrentElem := CurrentElem.PNext;
    End;
    GetListSize := Size;
End;

Procedure RewriteGeneralFiles();
Var
    GeneralFile: TextFile;
    LaunchTime: TDateTime;
Begin
    AssignFile(GeneralFile, TPath.GetDocumentsPath + FILE_LOGS);
    Rewrite(GeneralFile);
    WriteLn(GeneralFile, 'Re:Mind version ' + ProgramVersion);
    LaunchTime := Now;
    WriteLn(GeneralFile, 'Last launched at ' + TimeToStr(LaunchTime) + ', ' + DateToStr(LaunchTime));
    Close(GeneralFile);
End;

Procedure AddGeneralErrorReport(ErrorMessage: String);
Var
    GeneralFile: TextFile;
    ErrorTime: TDateTime;
Begin
    AssignFile(GeneralFile, TPath.GetDocumentsPath + FILE_LOGS);
    Append(GeneralFile);
    ErrorTime := Now;
    Writeln(GeneralFile, '[' + TimeToStr(ErrorTime) + '] ' + ErrorMessage);
    Close(GeneralFile);    
End;

Function ElementIsUnique(NewElement: TListElem) : Boolean;
Var
    CurrentElem: TListElem;
    IsUnique: Boolean;
Begin
    IsUnique := True;
    CurrentElem := MainList.PFirst;
    While (CurrentElem <> nil) and IsUnique do
    Begin
        If (CurrentElem.Data.About = NewElement.Data.About) then
            IsUnique := False;
        CurrentElem := CurrentElem.PNext;
    End;
    ElementIsUnique := IsUnique;
End;

Procedure ReadFileData();
Var
    FReminders: File of TLog;
    NewElement: TListElem;
Begin
    AssignFile(FReminders, TPath.GetDocumentsPath + FILE_REMINDERS);
    If not FileExists(TPath.GetDocumentsPath + FILE_REMINDERS) then
    Begin
        Rewrite(FReminders);
    End
    Else
        Reset(FReminders);
    While not Eof(FReminders) do
    Begin
        New(NewElement);
        Read(FReminders, NewElement.Data);
        If ElementIsUnique(NewElement) then
        Begin
            NewElement.Data.Number := GetListSize(MainList) + 1;
            AddNextElement(NewElement, MainList)
        End
        Else
            ShowMessage('Reminder you are trying to add is already exists!')
    End;

    Close(FReminders);
End;

Procedure ClearGrid(MainGrid: TStringGrid);
Var
    I: Integer;
Begin
    MainGrid.RowCount := 2;
    For I := 0 to MainGrid.ColCount - 1 do
    Begin
        MainGrid.Cells[I, 1] := '';
    End;
End;

Procedure PrintList(MainGrid: TStringGrid; const GList: TList);
Var
    CurrentElem: TListElem;
    I: Integer;
Begin
    ClearGrid(MainGrid);
    CurrentElem := GList.PFirst;
    I := 1;
    While CurrentElem <> nil do
    Begin
        With CurrentElem.Data do
        Begin
            With MainGrid do
            Begin
                Cells[0, I] := IntToStr(Number);
                Cells[1, I] := About;
                Cells[2, I] := DateToStr(DueTo);
                Cells[3, I] := PriorityArray[Priority];
            End;
        End;
        Inc(I);
        MainGrid.RowCount := I;
        CurrentElem := CurrentElem.PNext;
    End;
End;

Procedure PrintListBackwards(MainGrid: TStringGrid; const GList: TList; ListSize: Integer);
Var
    CurrentElem: TListElem;
    I: Integer;
Begin
    ClearGrid(MainGrid);
    CurrentElem := GList.PLast;
    I := 1;
    While CurrentElem <> nil do
    Begin
        With CurrentElem.Data do
        Begin
            With MainGrid do
            Begin
                Cells[0, I] := IntToStr(Number);
                Cells[1, I] := About;
                Cells[2, I] := DateToStr(DueTo);
                Cells[3, I] := PriorityArray[Priority];
            End;
        End;
        Inc(I);
        Dec(ListSize);
        MainGrid.RowCount := I;
        CurrentElem := CurrentElem.PPrev;
    End;
End;

Procedure SetColsWidth(MainGrid: TStringGrid);
Begin
    MainGrid.Width := MainForm.ClientWidth;
    With MainGrid do
    Begin
        ColWidths[0] := Round(ClientWidth * 0.1);
        ColWidths[1] := Round(ClientWidth * 0.6);
        ColWidths[2] := Round(ClientWidth * 0.15);
        ColWidths[3] := Round(ClientWidth * 0.15);
    End;
End;

Procedure TMainForm.aAddNewExecute(Sender: TObject);
Begin
    AddNewForm.Show;
End;

Procedure SaveList();
Var
    FToDoLog: File of TLog;
    CurrentElem: TListElem;
Begin
    AssignFile(FToDoLog, TPath.GetDocumentsPath + FILE_REMINDERS);
    Rewrite(FTodoLog);
    CurrentElem := MainList.PFirst;
    While CurrentElem <> nil do
    Begin
        With CurrentElem.Data do
        Begin
            Write(FToDoLog, CurrentElem.Data);
            CurrentElem := CurrentElem.PNext;
        End;
    End;
    Close(FToDoLog);
End;

Function IsAlreadyAdded(Key: Integer) : Boolean;
Var
    CurrentElem: TListElem;
    IsFound: Boolean;
Begin
    IsFound := False;
    CurrentElem := SortedList.PLast;
    While (CurrentElem <> nil) and not IsFound do
    Begin
        If CurrentElem.Data.Number = Key then  
            IsFound := True;
        CurrentElem := CurrentElem.PPrev;
    End;
    IsAlreadyAdded := IsFound;
End;

Function CompareDate(IsAsc: Boolean; A: TDateTime; B: TDateTime) : Integer;
Begin
    If IsAsc then
    Begin
        If A > B then
            Result := 1
        Else If A < B then
            Result := -1
        Else If A = B then
            Result := 0;
    End
    Else
    Begin
        If A > B then
            Result := -1
        Else If A < B then
            Result := 1
        Else If A = B then
            Result := 0;
    End;
End;

Function CompareDescription(IsAsc: Boolean; A: AnsiChar; B: AnsiChar) : Integer;
Begin
    If IsAsc then
    Begin
        If A > B then
            Result := 1
        Else If A < B then
            Result := -1
        Else If A = B then
            Result := 0;
    End
    Else
    Begin
        If A > B then
            Result := -1
        Else If A < B then
            Result := 1
        Else If A = B then
            Result := 0;
    End;
End;

Function ComparePriority(IsAsc: Boolean; A: Byte; B: Byte) : Integer;
Begin
    If IsAsc then
    Begin
        If A > B then
            Result := 1
        Else If A < B then
            Result := -1
        Else If A = B then
            Result := 0;
    End
    Else
    Begin
        If A > B then
            Result := -1
        Else If A < B then
            Result := 1
        Else If A = B then
            Result := 0;
    End;
End;

Function GeneralComporator(ComparatorID: Byte; GivenElemA, GivenElemB: TListElem; IsAsc: Boolean) : Integer;
Begin
    Case ComparatorID of
        1: Result := CompareDescription(IsAsc, GivenElemA.Data.About[1], GivenElemB.Data.About[1]);
        2: Result := CompareDate(IsAsc, GivenElemA.Data.DueTo, GivenElemB.Data.DueTo);
        3: Result := ComparePriority(IsAsc, GivenElemA.Data.Priority, GivenElemB.Data.Priority);
    End;
End;

Procedure AddAsFirstElement(var GivenList: TList; const NewElem: TListElem);
Begin
    NewElem^.PNext := nil;
    NewElem^.PPrev := nil;
    If GivenList.PFirst = nil then
    Begin
        GivenList.PFirst := NewElem;
        GivenList.PLast := NewElem;
    End
    Else
    Begin
        NewElem^.PNext := GivenList.PFirst;
        GivenList.PFirst^.PPrev := NewElem;
        GivenList.PFirst := NewElem;
    End;
End;

Procedure InsB(var GivenList: TList; const BeforeThisElem, NewElem: TListElem);
Begin
    If (GivenList.PFirst = nil) or (BeforeThisElem = nil) or (BeforeThisElem = GivenList.PFirst) then
    Begin
        AddAsFirstElement(GivenList, NewElem);
    End
    Else
    Begin
        NewElem^.PPrev := BeforeThisElem^.PPrev;
        NewElem^.PNext := BeforeThisElem;
        BeforeThisElem^.PPrev := NewElem;
        NewElem^.PPrev^.PNext := NewElem;
    End;
End;

Procedure SortList(IsAsc: Boolean; SortBy: Byte);
Var
    CurrentElement, NewElement, ComparingElement: TListElem;
    IsSortedElem: Boolean;
Begin
    CurrentElement := MainList.PFirst;
    New(NewElement);
    NewElement.Data := CurrentElement.Data;
    AddNextElement(NewElement, SortedList);
    CurrentElement := CurrentElement.PNext;
    While CurrentElement <> nil do
    Begin
        ComparingElement := SortedList.PFirst;
        IsSortedElem := False;
        While (ComparingElement <> nil) and not IsSortedElem do
        Begin
            If GeneralComporator(SortBy, CurrentElement, ComparingElement, IsAsc) = 1 then
            Begin
                New(NewElement);
                NewElement.Data := CurrentElement.Data;
                InsB(SortedList, ComparingElement, NewElement);
                IsSortedElem := True;    
            End
            Else If ComparingElement.PNext = nil then
            Begin
                New(NewElement);
                NewElement.Data := CurrentElement.Data;
                AddNextElement(NewElement, SortedList);
                IsSortedElem := True;
            End;
            ComparingElement := ComparingElement.PNext;
        End;
        CurrentElement := CurrentElement.PNext;
    End;
End;

Function GetCurrentlySortedCol(MainGrid: TStringGrid) : Byte;
Var
    FoundIn, I: Byte;
Begin
    FoundIn := 100;
    For I := 0 to MainGrid.ColCount - 1 do
    Begin
        If (MainGrid.Cells[I, 0] = FixedRowCaptionsArray[I] + ' ▲') or (MainGrid.Cells[I, 0] = FixedRowCaptionsArray[I] + ' ▼') then
            FoundIn := I;
    End;
    GetCurrentlySortedCol := FoundIn;
End;

Procedure TMainForm.aAddRecordExecute(Sender: TObject);
Var
    Description: String;
    DueTo: TDateTime;
    Priority: Byte;
    NewListElement: TListElem;
Begin
    New(NewListElement);
    With NewListElement.Data do
    Begin
        With AddNewForm do
        Begin
            DueTo := DatePick1.Date;
            Priority := PriorityBox1.ItemIndex + 1;
            About := InformationMemo1.Text;
        End;
    End;
    If ElementIsUnique(NewListElement) then
    Begin
        NewListElement.Data.Number := GetListSize(MainList) + 1;
        AddNextElement(NewListElement, MainList);
    End
    Else
        ShowMessage('Reminder you are trying to add is already exists!');

    If GetCurrentlySortedCol(MainGrid) = 100 then
        PrintList(MainGrid, MainList)
    Else
    Begin
        OrderAsc := not OrderAsc;
        MainGrid.OnFixedCellClick(MainGrid, GetCurrentlySortedCol(MainGrid), 0);
    End;
    //PrintList(MainGrid, List);
    SaveList;
    AddNewForm.Close;
End;

Procedure TMainForm.aChangeThemeExecute(Sender: TObject);
Begin
    IsLightTheme := not IsLightTheme;
    If IsLightTheme then
    Begin
        // GRID_LIGHT
        MainGrid.Color := clWhite;
        MainGrid.Font.Color := clBlack;
        MainGrid.GradientStartColor := clSkyBlue;
        MainGrid.GradientEndColor := clSkyBlue;
        // TOOLBAR_LIGHT
        MainToolBar.GradientStartColor := clSkyBlue;
        MainToolBar.GradientEndColor := clSkyBlue;
    End
    Else
    Begin
        // GRID_DARK
        MainGrid.Color := $323232;
        MainGrid.Font.Color := clWhite;
        MainGrid.GradientStartColor := $515151;
        MainGrid.GradientEndColor := $515151;
        // TOOLBAR_DARK
        MainToolBar.GradientStartColor := $515151;
        MainToolBar.GradientEndColor := $515151;
    End;
End;

Procedure TMainForm.aClearAllExecute(Sender: TObject);
Var
    IsConfirmed: Byte;
Begin
    IsConfirmed := MessageDlg('Are you sure that you want to clear all records? This action can''t be undone!', mtConfirmation, mbYesNo, 0);

    If IsConfirmed = mrYes then
    Begin
        DeleteList(MainList);
        SaveList;
        PrintList(MainGrid, MainList);
    End;
End;

Procedure TMainForm.AddNew1Click(Sender: TObject);
Begin
    AddNewForm.ShowModal;
End;

Procedure DeleteListItem(var GivenList: TList; var DeletingElem : TListElem);
Begin
    If DeletingElem = GivenList.PFirst then
    Begin
      GivenList.PFirst := DeletingElem^.PNext;
      If GivenList.PFirst = nil then
            GivenList.PLast := nil
      Else
            GivenList.PFirst^.PPrev := nil;
    End
    Else If DeletingElem = GivenList.PLast then
    Begin
      GivenList.PLast := DeletingElem^.PPrev;
      If GivenList.PLast = nil then
            GivenList.PFirst := nil
      Else
            GivenList.PLast^.PNext := nil;
    End
    Else
    Begin
        DeletingElem^.PPrev^.PNext := DeletingElem^.PNext;
        DeletingElem^.PNext^.PPrev := DeletingElem^.PPrev;
    End;

    Dispose(DeletingElem);
    DeletingElem := nil;
End;

Function GetByNum(const GivenList: TList; const Key: Integer) : TListElem;
Var
    I : Integer;
    PNext : TListElem;
Begin
    Result := nil;
    I := 1;
    PNext := GivenList.PFirst;
    While (I <= Key) and (PNext <> nil) do
    Begin
        If I = Key then begin
            Result := PNext;
            Break;
        End;
        Inc(i);
        PNext := PNext^.PNext;
    End;
End;

Procedure OrderListAgain();
Var
    CurrentElem: TListELem;
    I: Integer;
Begin
    CurrentElem := MainList.PFirst;
    I := 1;
    While CurrentElem <> nil do
    Begin
        CurrentElem.Data.Number := I;
        CurrentElem := CurrentElem.PNext;
        Inc(I);
    End;
End;

Procedure TMainForm.aDeleteRecordExecute(Sender: TObject);
Var
    DeletingElement: TListElem;
Begin
    DeletingElement := GetByNum(MainList, EditingRowIndicator);
    DeleteListItem(MainList, DeletingElement);
    SaveList;
    OrderListAgain;
    If GetCurrentlySortedCol(MainGrid) = 100 then
        PrintList(MainGrid, MainList)
    Else
    Begin
        OrderAsc := not OrderAsc;
        MainGrid.OnFixedCellClick(MainGrid, GetCurrentlySortedCol(MainGrid), 0);
    End;
End;

Procedure TMainForm.aDrawNoteExecute(Sender: TObject);
Begin
    DrawForm.ShowModal;
End;

Procedure TMainForm.aEditRecordExecute(Sender: TObject);
Var
    CurrentElem: TListElem;
    ElementNumber: Integer;
    IsAlreadyEdited: Boolean;
Begin
    CurrentElem := MainList.PFirst;
    ElementNumber := 1;
    IsAlreadyEdited := False;
    While (CurrentElem <> nil) and not (IsAlreadyEdited) do
    Begin
        If ElementNumber = EditingRowIndicator then
        Begin
            With CurrentElem.Data do
            Begin
                With EditForm do
                Begin
                    DueTo := DatePick1.Date;
                    Priority := PriorityBox1.ItemIndex + 1;
                    About := InformationMemo1.Text;
                End
            End;
            IsAlreadyEdited := True;
        End;
        Inc(ElementNumber);    
        CurrentElem := CurrentElem.PNext;
    End;
    SaveList;
    If GetCurrentlySortedCol(MainGrid) = 100 then
        PrintList(MainGrid, MainList)
    Else
    Begin
        OrderAsc := not OrderAsc;
        MainGrid.OnFixedCellClick(MainGrid, GetCurrentlySortedCol(MainGrid), 0);
    End;
End;

Procedure TMainForm.aNotesEditorExecute(Sender: TObject);
Begin
    NotesForm.ShowModal;
    NotesForm.aStartController.Execute;
End;

Procedure TMainForm.aSetNotificationExecute(Sender: TObject);
Begin
    NotificationForm.aSetComboBox.Execute;
    NotificationForm.ShowModal;
End;

Procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
Var
    IsConfirmed: Byte;
Begin
    IsConfirmed := MessageDlg('You sure that you want to exit?', mtConfirmation, mbYesNo, 0);
    If IsConfirmed = mrYes then
        CanClose := True
    Else
    Begin
        CanClose := False;
    End;
End;

Procedure CreateNewDirectory;
Begin
    If not TDirectory.Exists(TPath.GetDocumentsPath + '\ReMind\') then
        TDirectory.CreateDirectory(TPath.GetDocumentsPath + '\ReMind\');
End;

Procedure TMainForm.FormCreate(Sender: TObject);
Begin
    DeleteList(MainList);
    SetColsWidth(MainGrid);
    NameFixedRows(MainGrid);
    CreateNewDirectory;
    RewriteGeneralFiles;
    ReadFileData;
    PrintList(MainGrid, MainList);
    Self.Caption := Self.Caption + ' v' + ProgramVersion;
End;

Procedure TMainForm.FormResize(Sender: TObject);
Begin
    SetColsWidth(MainGrid);
End;

Procedure SetEditFormProperties(MainGrid: TStringGrid; Row: Integer);
Begin
    With EditForm do
    Begin
        With MainGrid do
        Begin
            InformationMemo1.Text := Cells[1, Row];
            DatePick1.Date := StrToDate(Cells[2, Row]);
            aSetPriorities.Execute;
            If Cells[3, Row] = 'High' then
                PriorityBox1.ItemIndex := 2
            Else If Cells[3, Row] = 'Normal' then
                PriorityBox1.ItemIndex := 1
            Else If Cells[3, Row] = 'Low' then
                PriorityBox1.ItemIndex := 0;
            EditingRowIndicator := StrToInt(Cells[0, Row]);
        End;
        ShowModal;
    End;
End;

Procedure TMainForm.MainApplicationEventsException(Sender: TObject; E: Exception);
Begin
    AddGeneralErrorReport(E.ToString);
End;

Procedure TMainForm.MainApplicationEventsMinimize(Sender: TObject);
Begin
    MainForm.Visible := False;
    MainTrayIcon.Visible := True; 
End;

Procedure TMainForm.MainGridDblClick(Sender: TObject);
Var
    SelectedRow: Integer;
Begin
    If not (MainGrid.Row = 0) then
    Begin
        SelectedRow := MainGrid.Row;
        SetEditFormProperties(MainGrid, SelectedRow);
    End;
End;

Procedure TMainForm.MainGridFixedCellClick(Sender: TObject; ACol, ARow: Integer);
Begin
    If not (ACol = 0) then
    Begin
        DeleteList(SortedList);
        SortList(OrderAsc, ACol);
        NameFixedRows(MainGrid);
        If OrderAsc then
            MainGrid.Cells[ACol, ARow] := FixedRowCaptionsArray[ACol] + ' ▲'
        Else
            MainGrid.Cells[ACol, ARow] := FixedRowCaptionsArray[ACol] + ' ▼';
        PrintList(MainGrid, SortedList);
    End;
    If ACol = 0 then
    Begin
        If OrderAsc then
        Begin
            PrintList(MainGrid, MainList);
            NameFixedRows(MainGrid)
        End
        Else
        Begin
            NameFixedRows(MainGrid);
            MainGrid.Cells[ACol, ARow] := FixedRowCaptionsArray[ACol] + ' ▲';
            PrintListBackwards(MainGrid, MainList, GetListSize(MainList));
        End;
    End;
    OrderAsc := not OrderAsc;
End;

Procedure TMainForm.MainGridKeyPress(Sender: TObject; var Key: Char);
Begin
    If (Key = #13) and (MainGrid.Row <> 0) then
    Begin
        SetEditFormProperties(MainGrid, MainGrid.Row);
    End;
End;

Procedure TMainForm.MainTrayIconDblClick(Sender: TObject);
Begin
    MainTrayIcon.Visible := False;
    MainForm.Visible := True;
    MainForm.WindowState := wsNormal;
    MainForm.FormStyle := fsStayOnTop;
    MainForm.FormStyle := fsNormal;
End;

Procedure TMainForm.Settings1Click(Sender: TObject);
Begin
    SettingsForm.ShowModal;
End;

Procedure TMainForm.tbDeleteAllClick(Sender: TObject);
Begin
    aClearAll.Execute;
End;

Procedure TMainForm.tbDrawClick(Sender: TObject);
Begin
    DrawForm.ShowModal;
End;

End.
