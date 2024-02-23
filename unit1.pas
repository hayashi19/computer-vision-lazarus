unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, ExtDlgs, ImageProcessing;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonLoad: TButton;
    ButtonProcess: TButton;
    ImageOriginal: TImage;
    ImageEdited: TImage;
    LabelProcess: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ImageProcessor: TImageProcessor;
    Filter: TImageProcessor.TFilter;

    procedure ApplyFilterMatrix();
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImageProcessor := TImageProcessor.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ImageProcessor.Free;
end;

// [-1, -1] M1  [0, -1] M2  [1, -1] M3
// [-1,  0] M4  [0,  0] M5  [1,  0] M6
// [-1,  1] M7  [0,  1] M8  [1,  1] M9
procedure TForm1.ApplyFilterMatrix();
begin
  Filter[-1, -1] := 1;
  Filter[0, -1] := 1;
  Filter[1, -1] := 1;
  Filter[-1, 0] := 1;
  Filter[0, 0] := -8;
  Filter[1, 0] := 1;
  Filter[-1, 1] := 1;
  Filter[0, 1] := 1;
  Filter[1, 1] := 1;
end;

procedure TForm1.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageOriginal.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  end;
end;

procedure TForm1.ButtonProcessClick(Sender: TObject);
begin
  ApplyFilterMatrix();

  //ImageEdited.Picture:=ImageProcessor.ProcessBrightness(ImageOriginal, 100);
  //ImageEdited.Picture:=ImageProcessor.ProcessContrast(ImageOriginal, 128, 2);
  //ImageEdited.Picture:=ImageProcessor.ProcessGreyscale(ImageOriginal);
  ImageEdited.Picture:=ImageProcessor.ProcessTreshold(ImageOriginal, 128);
  //ImageEdited.Picture:=ImageProcessor.ProcessInverse(ImageOriginal); 
  ImageEdited.Picture:=ImageProcessor.ProcessFilter(ImageEdited, Filter);

  LabelProcess.Caption:='Process ' + IntToStr(ImageProcessor.ProcessTime);
end;

end.

