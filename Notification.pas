Unit Notification;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.WinXCalendars, System.IOUtils,
    Vcl.WinXPickers, System.Actions, Vcl.ActnList, Vcl.Samples.Spin, DateUtils, Vcl.ExtCtrls,
    System.Notification;

Type
    TNotificationForm = class(TForm)
        DescriptionComboBox: TComboBox;
        CustomDesc: TEdit;
        SelectDesc: TLabel;
        SelectTime: TLabel;
    TimePicker: TTimePicker;
        DatePicker: TCalendarPicker;
        InformationMemo: TMemo;
        AddButton: TButton;
    NotificationsActionList: TActionList;
    aSetComboBox: TAction;
    DeleteSpinEdit: TSpinEdit;
    DeleteButton: TButton;
    NotificationTime: TTimer;
    NotificationCenter: TNotificationCenter;
    aLoadAndClose: TAction;
        procedure FormActivate(Sender: TObject);
    Procedure aSetComboBoxExecute(Sender: TObject);
    Procedure DescriptionComboBoxChange(Sender: TObject);
    Procedure AddButtonClick(Sender: TObject);
    Procedure DeleteButtonClick(Sender: TObject);
    Procedure NotificationTimeTimer(Sender: TObject);
    Procedure aLoadAndCloseExecute(Sender: TObject);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
    NotificationForm: TNotificationForm;

Implementation

{$R *.dfm}

Uses Main;

Type
    TNotificationInfo = Record
        Description: String[128];
        NotificationTime: TDateTime;
    End;

Const
    FILE_NOTIFICATIONS: String = '\ReMind\notification.dat';

Var
    ANotifications: Array of TNotificationInfo;

Procedure QuickSort(var A: Array of TNotificationInfo; Min, Max: Integer);
Var
    I, J: Integer;
    Supp: TDateTime;
    Temp: TNotificationInfo;
Begin
    Supp := A[Max-((max-min) div 2)].NotificationTime;
    I := Min;
    J := Max;
    While I<j do
    Begin
        While A[I].NotificationTime < Supp do
            I := I + 1;
        While A[J].NotificationTime > Supp do
            J := J - 1;
        If i<=j then
        Begin
            Temp := A[I];
            A[I] := A[J];
            A[J] := Temp;
            I := I + 1;
            J := J - 1;
        End;
    End;
    If Min < j then
        QuickSort(A, min, J);
    If I < Max then
        QuickSort(A, I, Max);
End; 

Procedure AddNewNotificationToFile();
Var
    FNotification: File of TNotificationInfo;
    I: Integer;
Begin
    AssignFile(FNotification, TPath.GetDocumentsPath + FILE_NOTIFICATIONS);
    Rewrite(FNotification);
    For I := 0 to Length(ANotifications) - 1 do
    Begin
        Write(FNotification, ANotifications[I]);
    End;
    Close(FNotification);    
End;

Procedure AddNewNotificationToArray(Notification: TNotificationInfo);
Begin
    SetLength(ANotifications, Length(ANotifications) + 1);
    ANotifications[Length(ANotifications) - 1] := Notification;   
End;

Procedure ReadFileData();
Var
    FNotification: File of TNotificationInfo;
    Notification: TNotificationInfo;
Begin
    AssignFile(FNotification, TPath.GetDocumentsPath + FILE_NOTIFICATIONS);
    If not FileExists(TPath.GetDocumentsPath + FILE_NOTIFICATIONS) then
    Begin
        Rewrite(FNotification);
    End
    Else
        Reset(FNotification);
        
    While not Eof(FNotification) do
    Begin
        Read(FNotification, Notification);
        AddNewNotificationToArray(Notification);
    End;
    Close(FNotification); 
End;

Procedure PrintNotifications(InformationMemo: TMemo);
Var
    I: Integer;
Begin
    InformationMemo.Clear;
    For I := 0 to Length(ANotifications) - 1 do
    Begin
        With ANotifications[I] do
        Begin
            InformationMemo.Lines.Add(Description + ' ' + TimeToStr(NotificationTime) + ' • ' + DateToStr(NotificationTime));
        End;
    End;
End;

Procedure SetSpinEditValues(SE: TSpinEdit; DeleteButton: TButton);
Begin
    If Length(ANotifications) = 0 then
    Begin
        SE.Text := '0';
        SE.MinValue := 0;
    End
    Else
        SE.MinValue := 1;
    SE.MaxValue := Length(ANotifications);    
    DeleteButton.Enabled := Length(ANotifications) > 0;
End;

Procedure TNotificationForm.AddButtonClick(Sender: TObject);
Var
    NewNotification: TNotificationInfo;
    NotificationTime: TDateTime;
Begin
    If DescriptionComboBox.ItemIndex = DescriptionComboBox.Items.Count - 1 then
        NewNotification.Description := CustomDesc.Text
    Else
        NewNotification.Description := DescriptionComboBox.Text;

    ReplaceDate(NewNotification.NotificationTime, DatePicker.Date);
    ReplaceTime(NewNotification.NotificationTime, TimePicker.Time);  
    AddNewNotificationToArray(NewNotification);
    AddNewNotificationToFile();
    
    QuickSort(ANotifications, 0, Length(ANotifications) - 1);
    PrintNotifications(InformationMemo);
    
    aSetComboBox.Execute;
    CustomDesc.Text := '';
    CustomDesc.Visible := False;
    SetSpinEditValues(DeleteSpinEdit, DeleteButton);
End;

Procedure TNotificationForm.aLoadAndCloseExecute(Sender: TObject);
Begin
    Self.Close;
End;

Procedure TNotificationForm.aSetComboBoxExecute(Sender: TObject);
Var
    I: Integer;
Begin
    DescriptionComboBox.Clear;
    For I := 1 to MainForm.MainGrid.RowCount - 1 do
    Begin
        DescriptionComboBox.Items.Add(MainForm.MainGrid.Cells[1, I]);        
    End;
    DescriptionComboBox.Items.Add('Custom...');
    DatePicker.Date := Now;
    TimePicker.Time := Now;
End;

Procedure DeleteArrayElement(DIndex: Integer);
Var
    I: Integer;
Begin
    For I := DIndex to Length(ANotifications) - 2 do
    Begin
        ANotifications[I] := ANotifications[I + 1];
    End;
    SetLength(ANotifications, Length(ANotifications) - 1);    
End;

Procedure TNotificationForm.DeleteButtonClick(Sender: TObject);
Var
    DeletingItem: Integer;
Begin
    DeletingItem := StrToInt(DeleteSpinEdit.Text) - 1;
    DeleteArrayElement(DeletingItem);
    PrintNotifications(InformationMemo);
    SetSpinEditValues(DeleteSpinEdit, DeleteButton);
    AddNewNotificationToFile();
End;

Procedure TNotificationForm.DescriptionComboBoxChange(Sender: TObject);
Begin
    CustomDesc.Visible := DescriptionComboBox.ItemIndex = DescriptionComboBox.Items.Count - 1;
End;

Procedure TNotificationForm.FormActivate(Sender: TObject);
Begin
    InformationMemo.Clear;
    SetLength(ANotifications, 0);
    ReadFileData();
    If Length(ANotifications) <> 0 then
        QuickSort(ANotifications, 0, Length(ANotifications) - 1);
    PrintNotifications(InformationMemo);
    SetSpinEditValues(DeleteSpinEdit, DeleteButton);
End;

Procedure TNotificationForm.NotificationTimeTimer(Sender: TObject);
Var
    Hour, Min, Sec, MSec: Word;
    FHour, FMin, FSec, FMSec: Word;
    NewTime, ExistingTime: TDateTime;
    NewNotification: TNotification;
Begin
    If Length(ANotifications) > 0 then
    Begin
        If CompareDate(Date, ANotifications[0].NotificationTime) = 0 then
        Begin
            DecodeTime(Now, Hour, Min, Sec, MSec);
            NewTime := EncodeTime(Hour, Min, 0, 0);
            ExistingTime := ANotifications[0].NotificationTime;
            DecodeTime(ExistingTime, FHour, FMin, FSec, FMSec);
            ExistingTime := EncodeTime(FHour, FMin, 0, 0);
            If CompareTime(NewTime, ExistingTime) = 0 then
            Begin
                NewNotification := NotificationCenter.CreateNotification;
                Try
                    NewNotification.Name := 'Name';
                    NewNotification.Title := 'Re:Minder Notification';
                    With ANotifications[0] do
                        NewNotification.AlertBody := Description + #13#10 + TimeToStr(NotificationTime) + ' • ' + DateToStr(NotificationTime);
                    NotificationCenter.PresentNotification(NewNotification);
                Finally
                    NewNotification.Free;
                End;
                DeleteArrayElement(0);
                PrintNotifications(InformationMemo);
                SetSpinEditValues(DeleteSpinEdit, DeleteButton);
                AddNewNotificationToFile();
            End;
        End;
    End;
End;

End.
