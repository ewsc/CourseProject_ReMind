program ReMind;

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  Settings in 'Settings.pas' {SettingsForm},
  AddNew in 'AddNew.pas' {AddNewForm},
  Edit in 'Edit.pas' {EditForm},
  Notes in 'Notes.pas' {NotesForm},
  Draw in 'Draw.pas' {DrawForm},
  Notification in 'Notification.pas' {NotificationForm},
  Login in 'Login.pas' {LoginForm},
  Secrets in 'Secrets.pas' {SecretForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TAddNewForm, AddNewForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TNotesForm, NotesForm);
  Application.CreateForm(TDrawForm, DrawForm);
  Application.CreateForm(TNotificationForm, NotificationForm);
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TSecretForm, SecretForm);
  Application.Run;
end.
