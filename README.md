# Desc

Main library file in `imageprocessing.pas`. The other files is the Lazarus application project file.

# Installation

Just download the `imageprocessing.pas` only and put it into your lazarus project folder.

Add the `ImageProcessing` to `uses` package in your `project1.lpr` and your unit file.

# Class Public Member

### Atribute
```pascal
// Use to check the on going process execution time
ProcessTime: Integer;
```

```pascal
// Use to store the filter matrix 3x3 for `ProcessFilter` function
type TFilter = array [-1..1, -1..1] of Integer;
```

```pascal
function ProcessBrightness(Image: TImage; BrightnessValue: Integer): TPicture;
function ProcessContrast(Image: TImage; PValue: Integer; GValue: Double): TPicture;
function ProcessGreyscale(Image: TImage): TPicture;
function ProcessTreshold(Image: TImage; TresholdValue: Integer): TPicture;
function ProcessInverse(Image: TImage): TPicture;
function ProcessFilter(Image: TImage; Filter: TFilter): TPicture; 
```

# Usage

Init the class
```pascal
ImageProcessor: TImageProcessor;
ImageProcessor:=TImageProcessor.Create;
```

Use the processing. All image processing functions always need a @param Image type of `TImage` and always return Result type of `TPicture`
```pascal
ImageEdited.Picture:=ImageProcessor.ProcessBrightness(ImageOriginal, 100);
ImageEdited.Picture:=ImageProcessor.ProcessContrast(ImageOriginal, 128, 2);
ImageEdited.Picture:=ImageProcessor.ProcessGreyscale(ImageOriginal);
ImageEdited.Picture:=ImageProcessor.ProcessTreshold(ImageOriginal, 128);
ImageEdited.Picture:=ImageProcessor.ProcessInverse(ImageOriginal);
ImageEdited.Picture:=ImageProcessor.ProcessFilter(ImageEdited, Filter); 
```
