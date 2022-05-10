Unit Notes;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, System.IOUtils;

Type
    TNotesForm = class(TForm)
        NotesMemo: TMemo;
        NotesActionList: TActionList;
        aStartController: TAction;
    aCloseForm: TAction;
        Procedure aStartControllerExecute(Sender: TObject);
        Procedure FormClose(Sender: TObject; var Action: TCloseAction);
        Procedure FormCreate(Sender: TObject);
        Procedure FormResize(Sender: TObject);
    procedure aCloseFormExecute(Sender: TObject);
    Private
        { Private declarations }
    Public
        { Public declarations }
    End;

Var
  NotesForm: TNotesForm;

Implementation

{$R *.dfm}

Const
    FILE_NOTES: String = '\ReMind\notes.dat';

Procedure CreateNewFiles();
Var
    NewFile: TextFile;
Begin
    AssignFile(NewFile, TPath.GetDocumentsPath + FILE_NOTES);
    Rewrite(NewFile);
    WriteLn(NewFile, 'This is your notepad.');
    Close(NewFile);
End;

Procedure AddFileDataToMemo(NotesMemo: TMemo);
Var
    FNotes: TextFile;
    Temp: String;
Begin
    AssignFile(FNotes, TPath.GetDocumentsPath + FILE_NOTES);
    Reset(FNotes);
    While not EoF(FNotes) do
    Begin
        ReadLn(FNotes, Temp);
        NotesMemo.Lines.Add(Temp);
    End;
    Close(FNotes);
End;

Procedure TNotesForm.aCloseFormExecute(Sender: TObject);
Begin
    Self.Close;
End;

Procedure TNotesForm.aStartControllerExecute(Sender: TObject);
Var
    IsFirstLaunch: Boolean;
    FileData: String;
Begin
    IsFirstLaunch := not FileExists(TPath.GetDocumentsPath + FILE_NOTES);
    If IsFirstLaunch then
    Begin
        CreateNewFiles();
    End;
    AddFileDataToMemo(NotesMemo);
End;

Procedure TNotesForm.FormClose(Sender: TObject; var Action: TCloseAction);
Var
    FNotes: TextFile;
Begin
    NotesMemo.Lines.SaveToFile(TPath.GetDocumentsPath + FILE_NOTES);
    NotesMemo.Clear;
End;

Procedure TNotesForm.FormCreate(Sender: TObject);
Begin
    NotesMemo.Clear;
    NotesMemo.Width := Self.ClientWidth;
    NotesMemo.Height := Self.ClientHeight;
End;

Procedure TNotesForm.FormResize(Sender: TObject);
Begin
    NotesMemo.Width := Self.ClientWidth;
    NotesMemo.Height := Self.ClientHeight;
End;

End.
