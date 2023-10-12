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
    aSafeNotes: TAction;
    tbSafe: TToolButton;
    MainSaveDialog: TSaveDialog;
    aSaveRecords: TAction;
    tbSaveRec: TToolButton;
    aHelp: TAction;
    tbHelp: TToolButton;
    MainTimer: TTimer;
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
    Procedure aChangeThemeExecute(Sender: TObject);
    Procedure aSafeNotesExecute(Sender: TObject);
    Procedure aSaveRecordsExecute(Sender: TObject);
    Procedure aHelpExecute(Sender: TObject);
    procedure MainTimerTimer(Sender: TObject);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
    MainForm: TMainForm;

Implementation

{$R *.dfm}

Uses Settings, AddNew, Edit, Notes, Draw, Notification, Login, Secrets;

Const
    ProgramVersion = '1.4';
    PriorityArray: Array[1..3] of String = ('Low', 'Normal', 'High');
    FixedRowCaptionsArray: Array[0..3] of String = ('№', 'Information', 'Due To', 'Priority');
    FILE_LOGS: String = '\ReMind\logs.dat';
    FILE_REMINDERS: String = '\ReMind\reminders.dat';
    FILE_SECRETS: String = '\ReMind\secrets.dat';
    

Type
    TReminder = Record
        Number: Integer;
        About: String[255];
        Priority: Byte;
        DueTo: TDateTime;
    End;
    TListElem = ^TElement;
    TElement = Record
        Data: TReminder;
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
    FReminders: File of TReminder;
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
    AddNewForm.ShowModal;
End;

Procedure SaveList();
Var
    FToDoLog: File of TReminder;
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

Procedure InsertBefore(var GivenList: TList; const BeforeThisElem, NewElem: TListElem);
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
                InsertBefore(SortedList, ComparingElement, NewElement);
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

Procedure MakeChangesSorted(MainGrid: TStringGrid);
Begin
    If GetCurrentlySortedCol(MainGrid) = 100 then
        PrintList(MainGrid, MainList)
    Else
    Begin
        OrderAsc := not OrderAsc;
        MainGrid.OnFixedCellClick(MainGrid, GetCurrentlySortedCol(MainGrid), 0);
    End;
End;

Procedure TMainForm.aAddRecordExecute(Sender: TObject);
Var
    NewListElement: TListElem;
Begin
    New(NewListElement);
    With NewListElement.Data do
    Begin
        With AddNewForm do
        Begin
            DueTo := DatePick1.Date;
            Priority := PriorityBox1.ItemIndex + 1;
            About := String(InformationMemo1.Text);
        End;
    End;
    If ElementIsUnique(NewListElement) then
    Begin
        NewListElement.Data.Number := GetListSize(MainList) + 1;
        AddNextElement(NewListElement, MainList);
    End
    Else
        ShowMessage('Reminder you are trying to add is already exists!');


    MakeChangesSorted(MainGrid);
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
        // ADDNEW_LIGHT
        AddNewForm.Color := clWhite;
        AddNewForm.Font.Color := clBlack;
        // NOTES_LIGHT
        NotesForm.NotesMemo.Color := clWhite;
        NotesForm.NotesMemo.Font.Color := clBlack;
        // NOTIF_LIGHT
        NotificationForm.Color := clWhite;
        NotificationForm.Font.Color := clBlack;
        // LOGIN_LIGHT
        LoginForm.Color := clWhite;
        LoginForm.Font.Color := clBlack;
        // SECRET_LIGHT
        SecretForm.Color := clWhite;
        SecretForm.Font.Color := clBlack;
        // EDIT_LIGHT
        EditForm.Color := clWhite;
        EditForm.Font.Color := clBlack;
        aChangeTheme.ImageIndex := 5;
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
        // ADDNEW_DARK
        AddNewForm.Color := $323232;
        AddNewForm.Font.Color := clWhite;
        // NOTES_DARK
        NotesForm.NotesMemo.Color := $323232;
        NotesForm.NotesMemo.Font.Color := clWhite;// NOTIF_LIGHT
        // NOTIF_DARK
        NotificationForm.Color := $323232;
        NotificationForm.Font.Color := clWhite;
        // LOGIN_DARK
        LoginForm.Color := $323232;
        LoginForm.Font.Color := clWhite;
        // SECRETS_DARK
        SecretForm.Color := $323232;
        SecretForm.Font.Color := clWhite;
        // EDIT_DARK
        EditForm.Color := $323232;
        EditForm.Font.Color := clWhite;
        aChangeTheme.ImageIndex := 7;
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
    MakeChangesSorted(MainGrid);
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
    MakeChangesSorted(MainGrid);
End;

Procedure TMainForm.aHelpExecute(Sender: TObject);
Begin
    ShowMessage('Welcome to Re:Mind Aplication! This is short user guide.' + #13#10 + #13#10 +
                '[Adding new record] Press first Button in toolbar (Plus icon), fill in the fields, and press add.' + #13#10 + #13#10 +
                '[Sorting] Pressing on fixed cells, will sort your records, according on field to sort you clicked.' + #13#10 + #13#10 +
                '[Editing] Double click on record, to show edit form. After editing, click "Edit" button to save your changes.' + #13#10 + #13#10 +
                '[Deleting] Double click on record you want to delete, then in right low corner press "Delete" button to delete record.' + #13#10 + #13#10 +
                '[Note Editor] Press second button in toolbar (Note icon). Now you can write down anything, then just close this form. Thing that you wrote, would be saved and available next time.' + #13#10 + #13#10 +
                '[Drawing] Press third button in toolbar (Palette icon). Select color and draw, drawings would be saved. Furthermore, drawn image saved in your Documents path.' + #13#10 + #13#10 +
                '[Notifications] Press fourth button in toolbar (Bell icon). Fill the fields and you will be notificated when time runs out.' + #13#10 + #13#10 +
                '[Secret space] Press fifth button in toolbar (Lock icon). First time, set your password, next times re-enter it. All your secret data would be encrypted and saved.' + #13#10 + #13#10 +
                '[Export Record] Press sixth button in toolbar (Down arrow icon). Select location and file, then click save. In set path, will created .txt file with your records' + #13#10 + #13#10 +
                '[Change Theme] Press eighth button in toolbar (Display icon) to change theme. Next click will revert theme.' + #13#10 + #13#10 +
                '[Delete All Records] Press ninth button in toolbar (Cross icon) to delete all records.' + #13#10 + #13#10 +
                '[Minimizing application] By minimizing your main window, application will stay in tray. You can maximize it back by double clicking tray icon.' + #13#10 + #13#10 +
                'Created by Azam "gthanksg" Alamov, 2022. Made by the power of love, coffee and deadlines <3');
End;

Procedure TMainForm.aNotesEditorExecute(Sender: TObject);
Begin
    NotesForm.ShowModal;
    NotesForm.aStartController.Execute;
End;

Function PasswordIsCreated() : Boolean;
Var
    FSecrets: TextFile;
    LineWithPassword: String[20];
Begin
    Result := False;
    AssignFile(FSecrets, TPath.GetDocumentsPath + FILE_SECRETS);
    If FileExists(TPath.GetDocumentsPath + FILE_SECRETS) then
    Begin
        Reset(FSecrets);
        ReadLn(FSecrets, LineWithPassword);
        If (LineWithPassword <> '') and (LineWithPassword[1] = '!') then
            Result := True;
    End                  
    Else
    Begin
        Rewrite(FSecrets);
    End;
    Close(FSecrets);
End;

Procedure TMainForm.aSafeNotesExecute(Sender: TObject);
Var
    PasswordExists: Boolean;
Begin
    PasswordExists := PasswordIsCreated();
    If not PasswordExists then
        LoginForm.InformationLabel.Caption := 'Set up password:'
    Else
        LoginForm.InformationLabel.Caption := 'Input your password:';
    LoginForm.ShowModal;
End;

Procedure TMainForm.aSaveRecordsExecute(Sender: TObject);
Var
    FRes: TextFile;
    I, J: Integer;
Begin
    MainSaveDialog.InitialDir := TPath.GetDownloadsPath;
    If MainSaveDialog.Execute() then
    Begin
        AssignFile(FRes, MainSaveDialog.FileName);
        Rewrite(FRes);
        For I := 1 to MainGrid.RowCount do
        Begin
            For J := 0 to MainGrid.ColCount - 1 do
            Begin
                With MainGrid do
                Begin
                    Write(FRes, Cells[J, I] + ' ');
                End;
            End;
            WriteLn(FRes);
        End;
        CloseFile(FRes);
        ShowMessage('Records saved to: [' + MainSaveDialog.FileName + ']');
    End;
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

Procedure ShowHints();
Begin
    ShowMessage('Welcome!' + #13#10 + #13#10 + 'To feel yourself comfortable here, don''t forget to read short manual. Just press Ctrl + H or Hint icon on Taskbar.');
End;

Procedure CreateNewDirectory;
Begin
    If not TDirectory.Exists(TPath.GetDocumentsPath + '\ReMind\') then
    Begin
        TDirectory.CreateDirectory(TPath.GetDocumentsPath + '\ReMind\');
        ShowHints;
    End;
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
    NameFixedRows(MainGrid);
    If not (ACol = 0) then
    Begin
        DeleteList(SortedList);
        SortList(OrderAsc, ACol);
        If OrderAsc then
            MainGrid.Cells[ACol, ARow] := FixedRowCaptionsArray[ACol] + ' ▼'
        Else
            MainGrid.Cells[ACol, ARow] := FixedRowCaptionsArray[ACol] + ' ▲';
        PrintList(MainGrid, SortedList);
    End
    Else If ACol = 0 then
    Begin
        If OrderAsc then
            PrintList(MainGrid, MainList)
        Else
        Begin
            MainGrid.Cells[ACol, ARow] := FixedRowCaptionsArray[ACol] + ' ▼';
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

Procedure TMainForm.MainTimerTimer(Sender: TObject);
Begin
    MainTimer.Enabled := False;
    NotificationForm.aSetComboBox.Execute;
    //NotificationForm.ShowModal;
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
