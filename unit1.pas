unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, ImageProcessing;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonMaskIrisHasil: TButton;
    ButtonMaskPupil: TButton;
    ButtonMaskBijiMata: TButton;
    ButtonLoad: TButton;
    ButtonBijiMataTexture: TButton;
    ButtonBijiMataContour: TButton;
    ButtonMaskIris: TButton;
    ButtonPupilTexture: TButton;
    ButtonPupilContour: TButton;
    ImageBijiMataContour: TImage;
    ImageMaskIrisHasil: TImage;
    ImageMaskPupil: TImage;
    ImageMaskIris: TImage;
    ImageMaskBijiMata: TImage;
    ImagePupilTexture: TImage;
    ImagePupilContour: TImage;
    ImageOriginal: TImage;
    ImageBijiMataTexture: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure ButtonBijiMataContourClick(Sender: TObject);
    procedure ButtonBijiMataTextureClick(Sender: TObject);
    procedure ButtonMaskIrisClick(Sender: TObject);
    procedure ButtonMaskIrisHasilClick(Sender: TObject);
    procedure ButtonMaskPupilClick(Sender: TObject);
    procedure ButtonMaskBijiMataClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonPupilContourClick(Sender: TObject);
    procedure ButtonPupilTextureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private      
    ImageProcessor: TImageProcessor;
    Filter: TImageProcessor.TFilter;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImageProcessor := TImageProcessor.Create;
  ImageOriginal.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ImageProcessor.Free;
end;

procedure TForm1.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ImageOriginal.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  end;
end;

procedure TForm1.ButtonBijiMataTextureClick(Sender: TObject);
begin
  ImageBijiMataTexture.Picture:=ImageOriginal.Picture;
  ImageBijiMataTexture.Picture:=ImageProcessor.ProcessGreyscale(ImageBijiMataTexture);

  Filter[-1, -1] := 1;
  Filter[0, -1] := 1;
  Filter[1, -1] := 1;
  Filter[-1, 0] := 1;
  Filter[0, 0] := -8;
  Filter[1, 0] := 1;
  Filter[-1, 1] := 1;
  Filter[0, 1] := 1;
  Filter[1, 1] := 1;
  ImageBijiMataContour.Picture:=ImageProcessor.ProcessFilter(ImageBijiMataContour, Filter);

  ImageBijiMataTexture.Picture:=ImageProcessor.ProcessTreshold(ImageBijiMataTexture, 200);
end;

procedure TForm1.ButtonBijiMataContourClick(Sender: TObject);
begin
  ImageBijiMataContour.Picture:=ImageOriginal.Picture;
  ImageBijiMataContour.Picture:=ImageProcessor.ProcessGreyscale(ImageBijiMataContour);
  ImageBijiMataContour.Picture:=ImageProcessor.ProcessTreshold(ImageBijiMataContour, 240);
  ImageBijiMataContour.Picture:=ImageProcessor.ProcessInverse(ImageBijiMataContour);

  Filter[-1, -1] := 1;
  Filter[0, -1] := 1;
  Filter[1, -1] := 1;
  Filter[-1, 0] := 1;
  Filter[0, 0] := -8;
  Filter[1, 0] := 1;
  Filter[-1, 1] := 1;
  Filter[0, 1] := 1;
  Filter[1, 1] := 1;
  ImageBijiMataContour.Picture:=ImageProcessor.ProcessFilter(ImageBijiMataContour, Filter);
end;

procedure TForm1.ButtonPupilTextureClick(Sender: TObject);
begin
  ImagePupilTexture.Picture:=ImageOriginal.Picture;
  ImagePupilTexture.Picture:=ImageProcessor.ProcessBrightness(ImagePupilTexture, 100);
  ImagePupilTexture.Picture:=ImageProcessor.ProcessGreyscale(ImagePupilTexture);

  Filter[-1, -1] := 1;
  Filter[0, -1] := 1;
  Filter[1, -1] := 1;
  Filter[-1, 0] := 1;
  Filter[0, 0] := -8;
  Filter[1, 0] := 1;
  Filter[-1, 1] := 1;
  Filter[0, 1] := 1;
  Filter[1, 1] := 1;
  ImagePupilTexture.Picture:=ImageProcessor.ProcessFilter(ImagePupilTexture, Filter);

  ImagePupilTexture.Picture:=ImageProcessor.ProcessTreshold(ImagePupilTexture, 50);
end;

procedure TForm1.ButtonPupilContourClick(Sender: TObject);
begin
  ImagePupilContour.Picture:=ImageOriginal.Picture;
  ImagePupilContour.Picture:=ImageProcessor.ProcessGreyscale(ImagePupilContour);
  ImagePupilContour.Picture:=ImageProcessor.ProcessTreshold(ImagePupilContour, 50);
  ImagePupilContour.Picture:=ImageProcessor.ProcessInverse(ImagePupilContour);

  Filter[-1, -1] := 1;
  Filter[0, -1] := 1;
  Filter[1, -1] := 1;
  Filter[-1, 0] := 1;
  Filter[0, 0] := -8;
  Filter[1, 0] := 1;
  Filter[-1, 1] := 1;
  Filter[0, 1] := 1;
  Filter[1, 1] := 1;
  ImagePupilContour.Picture:=ImageProcessor.ProcessFilter(ImagePupilContour, Filter);
end;

procedure TForm1.ButtonMaskBijiMataClick(Sender: TObject);
begin
  ImageMaskBijiMata.Picture:=ImageOriginal.Picture;
  ImageMaskBijiMata.Picture:=ImageProcessor.ProcessContrast(ImageMaskBijiMata, 200, 1.5);
  ImageMaskBijiMata.Picture:=ImageProcessor.ProcessGreyscale(ImageMaskBijiMata);
  ImageMaskBijiMata.Picture:=ImageProcessor.ProcessTreshold(ImageMaskBijiMata, 240);
  ImageMaskBijiMata.Picture:=ImageProcessor.ProcessInverse(ImageMaskBijiMata);
end;

procedure TForm1.ButtonMaskPupilClick(Sender: TObject);
begin
  ImageMaskPupil.Picture:=ImageOriginal.Picture;
  ImageMaskPupil.Picture:=ImageProcessor.ProcessContrast(ImageMaskPupil, 200, 1.5);
  ImageMaskPupil.Picture:=ImageProcessor.ProcessGreyscale(ImageMaskPupil);
  ImageMaskPupil.Picture:=ImageProcessor.ProcessTreshold(ImageMaskPupil, 20);
end;

procedure TForm1.ButtonMaskIrisClick(Sender: TObject);

begin
   ImageMaskIris.Picture:=ImageOriginal.Picture;
   ImageMaskIris.Picture:=ImageProcessor.ProcessMult(ImageMaskBijiMata, ImageMaskPupil);
end;

procedure TForm1.ButtonMaskIrisHasilClick(Sender: TObject);
begin
   ImageMaskIrisHasil.Picture:=ImageOriginal.Picture;
   ImageMaskIrisHasil.Picture:=ImageProcessor.ProcessMult(ImageOriginal, ImageMaskIris);
end;

end.

