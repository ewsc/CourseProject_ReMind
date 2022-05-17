Unit Edit;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.WinXCalendars, System.DateUtils,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.WinXPickers;

Type
    TEditForm = class(TForm)
        InformationLabel1: TLabel;
        PriorityLabel2: TLabel;
        DeadlineLabel3: TLabel;
        InformationMemo1: TMemo;
        PriorityBox1: TComboBox;
        DatePick1: TCalendarPicker;
    EditButton1: TButton;
        CloseButton1: TButton;
    EditActionList: TActionList;
    aSetPriorities: TAction;
    DeleteButton1: TButton;
    aCloseForm: TAction;
    Procedure SetPrioritiesClick(Sender: TObject);
    Procedure InformationMemo1Change(Sender: TObject);
    Procedure InformationMemo1KeyPress(Sender: TObject; var Key: Char);
    Procedure PriorityBox1Change(Sender: TObject);
    Procedure DatePick1Change(Sender: TObject);
    Procedure EditButton1Click(Sender: TObject);
    Procedure aSetPrioritiesExecute(Sender: TObject);
    Procedure DeleteButton1Click(Sender: TObject);
    procedure aCloseFormExecute(Sender: TObject);
    procedure CloseButton1Click(Sender: TObject);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
    EditForm: TEditForm;

Implementation

{$R *.dfm}

Uses Main;

Procedure SetPriorityBoxLines(PriorityBox1: TComboBox);
Begin
    PriorityBox1.Items.Add('Low');
    PriorityBox1.Items.Add('Normal');
    PriorityBox1.Items.Add('High');
End;

Procedure TEditForm.aCloseFormExecute(Sender: TObject);
Begin
    Self.Close;
End;

Procedure TEditForm.aSetPrioritiesExecute(Sender: TObject);
Begin
    PriorityBox1.Items.Clear;
    SetPriorityBoxLines(PriorityBox1);
End;

Procedure TEditForm.CloseButton1Click(Sender: TObject);
Begin
    Self.Close;
End;

Procedure CheckIfButtonCouldBeEnabled(InformationMemo1: TMemo; PriorityBox1: TComboBox; DatePick1: TCalendarPicker; EditButton1: TButton);
Begin
    EditButton1.Enabled := (Length(InformationMemo1.Text) > 2) and (PriorityBox1.Text <> '') and (CompareDate(DatePick1.Date, Now) >= 0);
End;

Procedure TEditForm.DatePick1Change(Sender: TObject);
Begin
    CheckIfButtonCouldBeEnabled(InformationMemo1, PriorityBox1, DatePick1, EditButton1);
End;

Procedure TEditForm.DeleteButton1Click(Sender: TObject);
Begin
    MainForm.aDeleteRecord.Execute;
    Self.Close;
End;

Procedure TEditForm.EditButton1Click(Sender: TObject);
Begin
    MainForm.aEditRecord.Execute;
    Self.Close;
    PriorityBox1.Items.Clear;
End;

Procedure TEditForm.InformationMemo1Change(Sender: TObject);
Begin
    CheckIfButtonCouldBeEnabled(InformationMemo1, PriorityBox1, DatePick1, EditButton1);
End;

Procedure TEditForm.InformationMemo1KeyPress(Sender: TObject; var Key: Char);
Begin
    If Key = #13 then
        Key := #0;
End;

Procedure TEditForm.PriorityBox1Change(Sender: TObject);
Begin
    CheckIfButtonCouldBeEnabled(InformationMemo1, PriorityBox1, DatePick1, EditButton1);
End;

Procedure TEditForm.SetPrioritiesClick(Sender: TObject);
Begin
    PriorityBox1.Items.Clear;
    SetPriorityBoxLines(PriorityBox1);
End;

End.
