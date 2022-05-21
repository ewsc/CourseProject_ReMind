Unit Secrets;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, System.IOUtils,
    Vcl.StdCtrls;

Type
    TSecretForm = class(TForm)
        SecretMemo: TMemo;
        SecretActionList: TActionList;
        aStartupConfiguration: TAction;
        aCloseActions: TAction;
        Procedure aStartupConfigurationExecute(Sender: TObject);
        Procedure FormActivate(Sender: TObject);
        Procedure FormResize(Sender: TObject);
        Procedure FormClose(Sender: TObject; var Action: TCloseAction);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
    SecretForm: TSecretForm;

Implementation

Const
    FILE_SECRETS: String = '\ReMind\secrets.dat';

{$R *.dfm}

Function DecodeLine(Line: String) : String;
Var
    I, Secret: Integer;
    TempNumStr: String;
Begin
    I := 1;
    Result := '';
    TempNumStr := '';
    While I <= Length(Line) do
    Begin
        If Line[I] = ' ' then
        Begin
            Secret := StrToInt(TempNumStr);
            Secret := Secret xor 7; 
            Secret := Secret + 2; 
            Result := Result + Chr(Secret);
            Inc(I);
            TempNumStr := '';
        End
        Else If (Line[I] in ['0'..'9']) then
        Begin
            TempNumStr := TempNumStr + Line[I];
            Inc(I);
        End;
    End;
End;

Procedure AddFileDataToMemo(NotesMemo: TMemo);
Var
    FSecrets: TextFile;
    Temp: String;
Begin
    AssignFile(FSecrets, TPath.GetDocumentsPath + FILE_SECRETS);
    Reset(FSecrets);
    ReadLn(FSecrets, Temp);
    While not EoF(FSecrets) do
    Begin
        ReadLn(FSecrets, Temp);
        Temp := DecodeLine(Temp);
        NotesMemo.Lines.Add(Temp);
    End;
    System.Close(FSecrets);
End;

Procedure TSecretForm.aStartupConfigurationExecute(Sender: TObject);
Begin
    SecretMemo.Clear;
    AddFileDataToMemo(SecretMemo);
End;

Procedure TSecretForm.FormActivate(Sender: TObject);
Begin
    SecretMemo.Width := Self.ClientWidth;
    SecretMemo.Height := Self.ClientHeight;
End;

Function EncryptedLine(Line: String) : String;
Var
    I, Secret: Integer;
Begin
    Result := '';
    For I := 1 to Length(Line) do
    Begin
        Secret := Ord(Line[I]) - 2;
        Secret := Secret Xor 7;
        Result := Result + IntToStr(Secret) + ' ';
    End;
End;

Procedure SaveMemoData(SecretMemo: TMemo);
Var
    FSecrets: TextFile;
    I: Integer;
    PasswordLine: String;
Begin
    AssignFile(FSecrets, TPath.GetDocumentsPath + FILE_SECRETS);
    Reset(FSecrets);
    ReadLn(FSecrets, PasswordLine);
    Close(FSecrets);
    AssignFile(FSecrets, TPath.GetDocumentsPath + FILE_SECRETS);
    Rewrite(FSecrets);
    Writeln(FSecrets, PasswordLine);
    For I := 0 to SecretMemo.Lines.Count - 1 do
    Begin
        WriteLn(FSecrets, EncryptedLine(SecretMemo.Lines[I]));
    End;
    Close(FSecrets);
End;

Procedure TSecretForm.FormClose(Sender: TObject; var Action: TCloseAction);
Begin
    SaveMemoData(SecretMemo);
    SecretMemo.Clear;
    Self.Close;
End;

Procedure TSecretForm.FormResize(Sender: TObject);
Begin
    SecretMemo.Width := Self.ClientWidth;
    SecretMemo.Height := Self.ClientHeight;
End;

end.
