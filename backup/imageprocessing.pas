unit ImageProcessing;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, Windows;

type
  TImageProcessor = class
  public
    ProcessTime: Integer;

    type TFilter = array [-1..1, -1..1] of Integer;

    function ProcessBrightness(Image: TImage; BrightnessValue: Integer): TPicture;
    function ProcessContrast(Image: TImage; PValue: Integer; GValue: Double): TPicture;
    function ProcessGreyscale(Image: TImage): TPicture;
    function ProcessTreshold(Image: TImage; TresholdValue: Integer): TPicture;
    function ProcessInverse(Image: TImage): TPicture;
    function ProcessFilter(Image: TImage; Filter: TFilter): TPicture;
    function ProcessMasking(Image, Mask: TImage): TPicture;
    function ProcessMult(Image, Image2: TImage): TPicture;
  end;


implementation

function TImageProcessor.ProcessBrightness(Image: TImage; BrightnessValue: Integer): TPicture;
var
  X, Y, R, G, B: Integer;
begin
  ProcessTime:=0;

  Result := TPicture.Create;
  try
    Result.Assign(Image.Picture);

    for X := 0 to Image.Width - 1 do
    begin
      for Y := 0 to Image.Height - 1 do
      begin
        R:=GetRValue(Image.Canvas.Pixels[X, Y]) + BrightnessValue;
        G:=GetGValue(Image.Canvas.Pixels[X, Y]) + BrightnessValue;
        B:=GetBValue(Image.Canvas.Pixels[X, Y]) + BrightnessValue;

        R:=Min(255, Max(0, R));
        G:=Min(255, Max(0, G));
        B:=Min(255, Max(0, B));

        Result.Bitmap.Canvas.Pixels[X, Y]:=RGB(R, G, B);

        ProcessTime:=ProcessTime + 1;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TImageProcessor.ProcessContrast(Image: TImage; PValue: Integer; GValue: Double): TPicture;
var
  X, Y, R, G, B: Integer;
begin
  ProcessTime:=0;

  Result := TPicture.Create;
  try
    Result.Assign(Image.Picture);

    for X := 0 to Image.Width - 1 do
    begin
      for Y := 0 to Image.Height - 1 do
      begin
        R:=Round((GetRValue(Image.Canvas.Pixels[X, Y]) - PValue) * GValue) + PValue;
        G:=Round((GetGValue(Image.Canvas.Pixels[X, Y]) - PValue) * GValue) + PValue;
        B:=Round((GetBValue(Image.Canvas.Pixels[X, Y]) - PValue) * GValue) + PValue;

        R:=Min(255, Max(0, R));
        G:=Min(255, Max(0, G));
        B:=Min(255, Max(0, B));

        Result.Bitmap.Canvas.Pixels[X, Y]:=RGB(R, G, B);

        ProcessTime:=ProcessTime + 1;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TImageProcessor.ProcessGreyscale(Image: TImage): TPicture;
var
  X, Y, R, G, B, Grey: Integer;
begin
  ProcessTime:=0;

  Result := TPicture.Create;
  try
    Result.Assign(Image.Picture);

    for X := 0 to Image.Width - 1 do
    begin
      for Y := 0 to Image.Height - 1 do
      begin
        R:=GetRValue(Image.Canvas.Pixels[X, Y]);
        G:=GetGValue(Image.Canvas.Pixels[X, Y]);
        B:=GetBValue(Image.Canvas.Pixels[X, Y]);

        Grey:=(r + g + b) div 3;
        Grey:=Min(255, Max(0, Grey));

        Result.Bitmap.Canvas.Pixels[X, Y]:=RGB(Grey, Grey, Grey);

        ProcessTime:=ProcessTime + 1;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TImageProcessor.ProcessTreshold(Image: TImage; TresholdValue: Integer): TPicture;
var
  X, Y, R, G, B, Grey: Integer;
begin
  ProcessTime:=0;

  Result := TPicture.Create;
  try
    Result.Assign(Image.Picture);

    for X := 0 to Image.Width - 1 do
    begin
      for Y := 0 to Image.Height - 1 do
      begin
        R:=GetRValue(Image.Canvas.Pixels[X, Y]);
        G:=GetGValue(Image.Canvas.Pixels[X, Y]);
        B:=GetBValue(Image.Canvas.Pixels[X, Y]);

        Grey:=(r + g + b) div 3;
        Grey:=Min(255, Max(0, Grey));

        if Grey <= TresholdValue then
        begin
          Result.Bitmap.Canvas.Pixels[X, Y]:=RGB(0,0,0);
        end
        else begin
          Result.Bitmap.Canvas.Pixels[X, Y]:=RGB(255,255,255);
        end;

        ProcessTime:=ProcessTime + 1;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TImageProcessor.ProcessInverse(Image: TImage): TPicture;
var
  X, Y, R, G, B: Integer;
begin
  ProcessTime:=0;

  Result := TPicture.Create;
  try
    Result.Assign(Image.Picture);

    for X := 0 to Image.Width - 1 do
    begin
      for Y := 0 to Image.Height - 1 do
      begin
        R:=255 - GetRValue(Image.Canvas.Pixels[X, Y]);
        G:=255 - GetGValue(Image.Canvas.Pixels[X, Y]);
        B:=255 - GetBValue(Image.Canvas.Pixels[X, Y]);

        R:=Min(255, Max(0, r));
        G:=Min(255, Max(0, g));
        B:=Min(255, Max(0, b));

        Result.Bitmap.Canvas.Pixels[X, Y]:=RGB(R,G,B);

        ProcessTime:=ProcessTime + 1;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TImageProcessor.ProcessFilter(Image: TImage; Filter: TFilter): TPicture;
var
  ImgWidth, ImgHeight, X, Y, M1, M2, M3, M4, M5, M6, M7, M8, M9, Sum: Integer;
  ImageTemp: array [0..258, 0..258] of Integer;
begin
  ProcessTime:=0;

  Result := TPicture.Create;
  try
    ImgWidth:=Image.Width - 1;
    ImgHeight:=Image.Height - 1;

    Result.Assign(Image.Picture);

    // copying edge
    for X := 0 to ImgWidth do
    begin
      ImageTemp[X + 1, 0]:=Image.Canvas.Pixels[X, 0];
      ImageTemp[X + 1, ImgHeight + 2]:=Image.Canvas.Pixels[X, ImgHeight];
    end;

    for Y := 0 to ImgHeight do
    begin
      ImageTemp[0, Y + 1]:=Image.Canvas.Pixels[0, Y];
      ImageTemp[ImgWidth + 2, Y + 1]:=Image.Canvas.Pixels[ImgHeight, Y];
    end;

    // copying each 4 corner
    ImageTemp[0, 0]:=Image.Canvas.Pixels[0, 0];
    ImageTemp[ImgWidth + 2, 0]:=Image.Canvas.Pixels[ImgWidth, 0];
    ImageTemp[ImgWidth + 2, ImgHeight + 2]:=Image.Canvas.Pixels[ImgWidth, ImgHeight];
    ImageTemp[0, ImgHeight + 2]:=Image.Canvas.Pixels[1, ImgHeight];

    // copying all image
    for X := 0 to ImgWidth do
    begin
      for Y := 0 to ImgHeight do
      begin
        ImageTemp[X + 1, Y + 1]:=Image.Canvas.Pixels[X, Y];
      end;
    end;

    for X:=1 to ImgWidth + 1 do
    begin
      for Y:=1 to ImgHeight + 1 do
      begin
      // cari nilai pixel
      // [-1, -1] M1  [0, -1] M2  [1, -1] M3
      // [-1,  0] M4  [0,  0] M5  [1,  0] M6
      // [-1,  1] M7  [0,  1] M8  [1,  1] M9
      M1:=ImageTemp[X - 1, Y - 1];
      M2:=ImageTemp[X, Y - 1];
      M3:=ImageTemp[X + 1, Y - 1];
      M4:=ImageTemp[X - 1, Y];
      M5:=ImageTemp[X, Y];
      M6:=ImageTemp[X + 1, Y];
      M7:=ImageTemp[X - 1, Y + 1];
      M8:=ImageTemp[X, Y + 1];
      M9:=ImageTemp[X + 1, Y + 1];

      // multiply it by filter matrix
      M1:=M1 * Filter[-1, -1];
      M2:=M2 * Filter[0, -1];
      M3:=M3 * Filter[1, -1];
      M4:=M4 * Filter[-1, 0];
      M5:=M5 * Filter[0, 0];
      M6:=M6 * Filter[1, 0];
      M7:=M7 * Filter[-1, 1];
      M8:=M8 * Filter[0, 1];
      M9:=M9 * Filter[1, 1];

      Sum:=M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9;

      Sum:=Min(255, Max(0, Sum));

      Result.Bitmap.Canvas.Pixels[X - 1, Y - 1]:=RGB(Sum, Sum, Sum);
      end;
    end;

  except
    Result.Free;
    raise;
  end;
end;

function TImageProcessor.ProcessMult(Image, Image2: TImage): TPicture;
var
  X, Y, R, G, B, R2, G2, B2: Integer;
begin
  ProcessTime:=0;

  Result := TPicture.Create;
  try
    Result.Assign(Image.Picture);

    for X := 0 to Image.Width - 1 do
    begin
      for Y := 0 to Image.Height - 1 do
      begin
        R:=GetRValue(Image.Canvas.Pixels[X, Y]);
        G:=GetGValue(Image.Canvas.Pixels[X, Y]);
        B:=GetBValue(Image.Canvas.Pixels[X, Y]);

        R2:=GetRValue(Image2.Canvas.Pixels[X, Y]);
        G2:=GetGValue(Image2.Canvas.Pixels[X, Y]);
        B2:=GetBValue(Image2.Canvas.Pixels[X, Y]);

        R:=R * R2 div 255;
        G:=G * G2 div 255;
        B:=B * B2 div 255;

        Result.Bitmap.Canvas.Pixels[X, Y]:=RGB(R, G, B);

        ProcessTime:=ProcessTime + 1;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
