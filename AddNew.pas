Unit AddNew;

Interface

Uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.WinXCalendars, Vcl.StdCtrls, System.DateUtils;

Type
    TAddNewForm = class(TForm)
        InformationLabel1: TLabel;
        InformationMemo1: TMemo;
        PriorityLabel2: TLabel;
        PriorityBox1: TComboBox;
        DeadlineLabel3: TLabel;
        DatePick1: TCalendarPicker;
        AddButton1: TButton;
        CloseButton1: TButton;
        Procedure FormCreate(Sender: TObject);
    procedure InformationMemo1KeyPress(Sender: TObject; var Key: Char);
    procedure CloseButton1Click(Sender: TObject);
    procedure InformationMemo1Change(Sender: TObject);
    procedure PriorityBox1Change(Sender: TObject);
    procedure DatePick1Change(Sender: TObject);
    procedure AddButton1Click(Sender: TObject);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
    AddNewForm: TAddNewForm;

Implementation

{$R *.dfm}

Uses Main;

Procedure SetPriorityBoxLines(PriorityBox1: TComboBox);
Begin
    PriorityBox1.Items.Add('Low');
    PriorityBox1.Items.Add('Normal');
    PriorityBox1.Items.Add('High');
End;

Procedure SetDefaultValues(InformationMemo1: TMemo; DatePick1: TCalendarPicker; PriorityBox1: TComboBox);
Begin
    InformationMemo1.Clear;
    PriorityBox1.Items.Clear;
    DatePick1.Date := Now;
End;

Procedure TAddNewForm.AddButton1Click(Sender: TObject);
Begin
    MainForm.aAddRecord.Execute;
    SetDefaultValues(InformationMemo1, DatePick1, PriorityBox1);
    SetPriorityBoxLines(PriorityBox1);
End;

Procedure TAddNewForm.CloseButton1Click(Sender: TObject);
Begin
    Self.Close;
End;


Procedure TAddNewForm.FormCreate(Sender: TObject);
Var
    TodayDate: TDateTime;
Begin
    SetDefaultValues(InformationMemo1, DatePick1, PriorityBox1);
    SetPriorityBoxLines(PriorityBox1);
End;

Procedure CheckIfButtonCouldBeEnabled(InformationMemo1: TMemo; PriorityBox1: TComboBox; DatePick1: TCalendarPicker; AddButton1: TButton);
Begin
    AddButton1.Enabled := (Length(InformationMemo1.Text) > 2) and (PriorityBox1.Text <> '') and (CompareDate(DatePick1.Date, Now) >= 0);
End;

Procedure TAddNewForm.InformationMemo1Change(Sender: TObject);
Begin
    CheckIfButtonCouldBeEnabled(InformationMemo1, PriorityBox1, DatePick1, AddButton1);
End;

Procedure TAddNewForm.InformationMemo1KeyPress(Sender: TObject; var Key: Char);
Begin
    If Key = #13 then
        Key := #0;
End;

Procedure TAddNewForm.DatePick1Change(Sender: TObject);
Begin
    CheckIfButtonCouldBeEnabled(InformationMemo1, PriorityBox1, DatePick1, AddButton1);
End;

Procedure TAddNewForm.PriorityBox1Change(Sender: TObject);
Begin
    CheckIfButtonCouldBeEnabled(InformationMemo1, PriorityBox1, DatePick1, AddButton1);
End;

End.
