Unit Draw;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.Pngimage, System.IOUtils,
    Vcl.StdCtrls, Vcl.ActnMan, Vcl.ActnColorMaps;

Type
    TDrawForm = class(TForm)
        NoteDrawImage: TImage;
    ClearCanvas: TButton;
    PenColorBox: TColorBox;
        Procedure FormCreate(Sender: TObject);
    procedure NoteDrawImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClearCanvasClick(Sender: TObject);
    procedure PenColorBoxChange(Sender: TObject);
    Private
         { Private declarations }
    Public
         { Public declarations }
    End;

Var
    DrawForm: TDrawForm;

Implementation

{$R *.dfm}

Const
    FILE_IMAGE: String = '\ReMind\image.png';

Var
    PenColor: TColor = clBlack;

Procedure MakeImageSameSizeAsFormIs(NoteDrawImage: TImage);
Begin
    NoteDrawImage.Width := DrawForm.Width;
    NoteDrawImage.Height := DrawForm.Height;
End;

Procedure TDrawForm.ClearCanvasClick(Sender: TObject);
Begin
    With NoteDrawImage.Canvas do
    Begin
        Pen.Color := clWhite;
        Brush.Color := clWhite;
        Rectangle(0, 0, Width, Height); 
    End;
End;

Procedure SetPenColor(NoteDrawImage: TImage; ColorPicker: TComboBox);
Begin
    PenColor := StringToColor(ColorPicker.Text);
End;

Procedure TDrawForm.PenColorBoxChange(Sender: TObject);
Begin
    PenColor := PenColorBox.Selected;
End;

Procedure TDrawForm.FormClose(Sender: TObject; var Action: TCloseAction);
Var
    Bmp: TBitmap;
    Png: TPngImage;
Begin
    Bmp := TBitmap.Create;
    Try
        Bmp.SetSize(NoteDrawImage.Canvas.ClipRect.Right, NoteDrawImage.Canvas.ClipRect.Bottom);
        BitBlt(Bmp.Canvas.Handle, 0, 0, Width, Height, NoteDrawImage.Canvas.Handle, 0, 0, SRCCOPY);
        Png := TPngImage.Create;
        Try
            Png.Assign(Bmp);
            Png.SaveToFile(TPath.GetDocumentsPath + FILE_IMAGE);
        Finally
            Png.Free;
        End;
    Finally
        Bmp.Free;
    End;
End;

Procedure SetColorPicker(ColorPicker: TComboBox);
Begin
    With ColorPicker.Items do
    Begin    
        Add('clBlack');
        Add('clRed');
        Add('clYellow');
        Add('clGreen');
        Add('clBlue');
        Add('clWhite');
    End;
End;

Procedure TDrawForm.FormCreate(Sender: TObject);
Var
    PNG: TPNGObject;
Begin
    MakeImageSameSizeAsFormIs(NoteDrawImage);
    PNG := TPNGObject.Create;
    PNG.LoadFromFile(TPath.GetDocumentsPath + FILE_IMAGE);
    NoteDrawImage.Canvas.Draw(0, 0, PNG);
    //SetColorPicker(ColorPicker);
End;

Procedure TDrawForm.NoteDrawImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
    With NoteDrawImage.Canvas do
    Begin
        If ssLeft in Shift then
        Begin
            Pixels[X, Y] := PenColor;
            Pixels[X + 1, Y] := PenColor;
            Pixels[X, Y + 1] := PenColor;
            Pixels[X + 1, Y + 1] := PenColor;
        End;
    End;
End;

End.
