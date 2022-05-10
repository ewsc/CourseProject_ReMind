Unit Settings;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

Type
    TSettingsForm = class(TForm)
        AutoSaveChB1: TCheckBox;
        WeeklyViewChB2: TCheckBox;
        SaveButton1: TButton;
        CancelButton2: TButton;
        Procedure CancelButton2Click(Sender: TObject);
    Private
    { Private declarations }
    Public
    { Public declarations }
    End;

Var
    SettingsForm: TSettingsForm;

Implementation

{$R *.dfm}

Uses Main;

Procedure TSettingsForm.CancelButton2Click(Sender: TObject);
Begin
    Self.Close;
End;

End.
