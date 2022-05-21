Unit Login;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Actions,
    Vcl.ActnList, System.IOUtils;

Type
    TLoginForm = class(TForm)
        InformationLabel: TLabel;
        PasswordInput: TEdit;
        SubmitButton: TButton;
        LoginActionList: TActionList;
        procedure PasswordInputChange(Sender: TObject);
    Procedure SubmitButtonClick(Sender: TObject);
    procedure PasswordInputKeyPress(Sender: TObject; var Key: Char);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
    LoginForm: TLoginForm;

Implementation

Uses Secrets;

Const
    FILE_SECRETS: String = '\ReMind\secrets.dat';

Type
    TPasswordString = String[9];    

{$R *.dfm}

Procedure TLoginForm.PasswordInputChange(Sender: TObject);
Begin
    SubmitButton.Enabled := Length(PasswordInput.Text) = 8;
End;

Procedure TLoginForm.PasswordInputKeyPress(Sender: TObject; var Key: Char);
Begin
    If (Key = #13) and (Length(PasswordInput.Text) = 8) then
        SubmitButton.Click;
End;

Procedure EncodeNewPassword(Password: TPasswordString);
Var
    FSecrets: TextFile;
    I, Secret: Integer;
Begin
    AssignFile(FSecrets, TPath.GetDocumentsPath + FILE_SECRETS);
    Rewrite(FSecrets);
    Write(FSecrets, '!');
    For I := 1 to Length(Password) do
    Begin
        Write(FSecrets, ' ');
        Secret := Ord(Password[I]) - 2;
        Secret := Secret Xor 7;
        Write(FSecrets, IntToStr(Secret));
    End;
    Close(FSecrets);    
End;

Function DecodedPassword(Password: TPasswordString) : String;
Var
    I, Secret: Integer;
Begin
    Result := '!';
    For I := 1 to Length(Password) do
    Begin
        Secret := Ord(Password[I]) - 2;
        Secret := Secret Xor 7;
        Result := Result + ' ' + IntToStr(Secret);
    End;    
End;

Procedure TLoginForm.SubmitButtonClick(Sender: TObject);
Var
    FSecrets: TextFile;
    PasswordLine: String;
    CanOpen: Boolean;
Begin
    AssignFile(FSecrets, TPath.GetDocumentsPath + FILE_SECRETS);
    Reset(FSecrets);
    ReadLn(FSecrets, PasswordLine);
    System.Close(FSecrets);
    CanOpen := False;
    If (PasswordLine <> '') and (PasswordLine[1] = '!') then
    Begin
        If PasswordLine = DecodedPassword(PasswordInput.Text) then
        Begin
            CanOpen := True;
        End
        Else
            ShowMessage('Wrong password!');   
    End
    Else
    Begin  
        EncodeNewPassword(PasswordInput.Text);
        CanOpen := True;
    End;
    
    If CanOpen then
    Begin
        SecretForm.aStartupConfiguration.Execute;
        SecretForm.ShowModal;
        Self.Close;
    End;
    PasswordInput.Text := '';
End;

End.
