unit untMain;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ExtDlgs, GraphType;

type
  TKernelSize = (ks3, ks5, ks7, ks9, ks11, ks13);
  TPostProcessDField = function (const S: Single): Byte of object;

type
  TChamferItem = record
    offsetX, offsetY: Integer;
    distance: Single;
  end;
  TChamfers = array of TChamferItem;

  { TfrmMain }

  TfrmMain = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    imgKernels: TImageList;
    OpenPictureDialog: TOpenPictureDialog;
    pbPostProcess: TPaintBox;
    rbKernel3: TRadioButton;
    rbKernel9: TRadioButton;
    rbKernel5: TRadioButton;
    rbKernel11: TRadioButton;
    rbKernel7: TRadioButton;
    rbClamp: TRadioButton;
    rbKernel13: TRadioButton;
    rbMod: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbPostProcessPaint(Sender: TObject);
    procedure rbClampChange(Sender: TObject);
    procedure rbKernelChange(Sender: TObject);
    procedure rbModChange(Sender: TObject);
  private
    FBmp : TBitmap;
    FDist: TBitmap;
    FPostProcess: TBitmap;
    FChamfers: array [TKernelSize] of TChamfers;
    procedure LoadImage(const FileName: string);
    procedure BuildDistanceField(ch: TChamfers);
    procedure PostProcesImage(proc: TPostProcessDField);

    function GetKernel: TChamfers;
    function GetPostProcessFunction: TPostProcessDField;
    function PPClamp(const S: Single): Byte;
    function PPMod(const S: Single): Byte;

    procedure InitChamfers;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses Math;

{$R *.lfm}

type
  TRGB = record
    b,g,r: Byte;
  end;
  PRGB = ^TRGB;

function GetPixelRGB(const baseP: PRGB; bmpWidth, bmpHeight: Integer; x,y: Integer): PRGB; inline;
begin
  Result := baseP;
  Inc(Result, bmpWidth*y+x);
end;

function GetPixelSingle(const baseP: PSingle; bmpWidth, bmpHeight: Integer; x,y: Integer): PSingle; inline;
begin
  Result := baseP;
  Inc(Result, bmpWidth*y+x);
end;

function GetPixelSingleRC(const baseP: PSingle; bmpWidth, bmpHeight: Integer; x,y: Integer): PSingle; inline;
begin
  if (x < 0) or (y < 0) then Exit(Nil);
  if (x >= bmpWidth) or (y >= bmpHeight) then Exit(Nil);
  Result := baseP;
  Inc(Result, bmpWidth*y+x);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  InitChamfers;

  FBmp := TBitmap.Create;
  FDist := TBitmap.Create;
  FDist.PixelFormat:=pf32bit;
  FPostProcess := TBitmap.Create;
  FPostProcess.PixelFormat:=pf24bit;

  LoadImage('test.png');
  BuildDistanceField(GetKernel);
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin;
    LoadImage(OpenPictureDialog.FileName);
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBmp);
  FreeAndNil(FDist);
  FreeAndNil(FPostProcess);
end;

procedure TfrmMain.pbPostProcessPaint(Sender: TObject);
begin
  pbPostProcess.Canvas.Draw(0, 0, FPostProcess);
end;

procedure TfrmMain.rbClampChange(Sender: TObject);
begin
  PostProcesImage(GetPostProcessFunction);
end;

procedure TfrmMain.rbKernelChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    BuildDistanceField(GetKernel);
end;

procedure TfrmMain.rbModChange(Sender: TObject);
begin
  PostProcesImage(GetPostProcessFunction);
end;

procedure TfrmMain.BuildDistanceField(ch: TChamfers);
var pDist, pNeir: PSingle;
    i, j, k: Integer;
    StartTime, EndTime, Freq: Int64;

    baseBmp: PRGB;
    baseField: PSingle;
    BmpWidth, BmpHeight: Integer;
begin
  StartTime := 0;
  EndTime := 0;
  Freq := 0;

  BmpWidth := FBmp.Width;
  BmpHeight := FBmp.Height;

  FDist.Width:=BmpWidth;
  FDist.Height:=BmpHeight;

  FBmp.BeginUpdate;
  FDist.BeginUpdate;
  try
    baseBmp := PRGB(FBmp.RawImage.Data);
    baseField := PSingle(FDist.RawImage.Data);

    QueryPerformanceCounter(StartTime);
    pDist := PSingle(FDist.RawImage.Data);
    for j := 0 to BmpHeight - 1 do
      for i := 0 to BmpWidth - 1 do
      begin
        pDist := GetPixelSingle(baseField, BmpWidth, BmpHeight, i, j);
        if GetPixelRGB(baseBmp, BmpWidth, BmpHeight, i, j)^.b < 128 then
           pDist^ := Infinity
        else
           pDist^ := 0;
      end;

    for j := 0 to BmpHeight - 1 do
      for i := 0 to BmpWidth - 1 do
      begin
        pDist := GetPixelSingle(baseField, BmpWidth, BmpHeight, i, j);
        if pDist^=0 then Continue;
        for k := 0 to High(ch) do
        begin
          pNeir := GetPixelSingleRC(baseField, BmpWidth, BmpHeight, i+ch[k].offsetX, j+ch[k].offsetY);
          if pNeir = nil then Continue;
          pDist^ := Min(pDist^, pNeir^+ch[k].distance);
        end;
      end;

    for j := BmpHeight - 1 downto 0 do
      for i := BmpWidth - 1 downto 0 do
      begin
        pDist := GetPixelSingle(baseField, BmpWidth, BmpHeight, i, j);
        if pDist^=0 then Continue;
        for k := 0 to High(ch) do
        begin
          pNeir := GetPixelSingleRC(baseField, BmpWidth, BmpHeight, i-ch[k].offsetX, j-ch[k].offsetY);
          if pNeir = nil then Continue;
          pDist^ := Min(pDist^, pNeir^+ch[k].distance);
        end;
      end;

    QueryPerformanceCounter(EndTime);
    QueryPerformanceFrequency(Freq);
    Caption := Format('Build time %F msec', [(EndTime - StartTime)/Freq*1000]);
  finally
    FBmp.EndUpdate;
    FDist.EndUpdate;
  end;
  PostProcesImage(GetPostProcessFunction);
end;

procedure TfrmMain.PostProcesImage(proc: TPostProcessDField);
var pCol: PRGB;
    pDist: PSingle;
    i, j: Integer;
    basePost: PRGB;
    baseField: PSingle;
    BmpWidth, BmpHeight: Integer;
begin
  BmpWidth := FDist.Width;
  BmpHeight := FDist.Height;
  FPostProcess.Width:=BmpWidth;
  FPostProcess.Height:=BmpHeight;

  FDist.BeginUpdate;
  FPostProcess.BeginUpdate;
  try
    basePost := PRGB(FPostProcess.RawImage.Data);
    baseField := PSingle(FDist.RawImage.Data);

    for j := 0 to BmpHeight - 1 do
      for i := 0 to BmpWidth - 1 do
      begin
        pCol := GetPixelRGB(basePost, BmpWidth, BmpHeight, i, j);
        pCol^.r := proc(GetPixelSingle(baseField, BmpWidth, BmpHeight, i, j)^);
        pCol^.g := pCol^.r;
        pCol^.b := pCol^.r;
      end;
  finally
    FPostProcess.EndUpdate;
    FDist.EndUpdate;
  end;
  Invalidate;
end;

function TfrmMain.GetKernel: TChamfers;
begin
  Result := FChamfers[ks3]; //defaul value
  if rbKernel3.Checked then Exit(FChamfers[ks3]);
  if rbKernel5.Checked then Exit(FChamfers[ks5]);
  if rbKernel7.Checked then Exit(FChamfers[ks7]);
  if rbKernel9.Checked then Exit(FChamfers[ks9]);
  if rbKernel11.Checked then Exit(FChamfers[ks11]);
  if rbKernel13.Checked then Exit(FChamfers[ks13]);
end;

function TfrmMain.GetPostProcessFunction: TPostProcessDField;
begin
  Result := @PPClamp; //default value
  if rbClamp.Checked then Exit(@PPClamp);
  if rbMod.Checked then Exit(@PPMod);
end;

procedure TfrmMain.LoadImage(const FileName: string);
var pic: TPicture;
begin
  pic := TPicture.Create;
  try
    pic.LoadFromFile(FileName);
    FBmp.PixelFormat:=pf24bit;
    FBmp.Width:=pic.Width;
    FBmp.Height:=pic.Height;
    FBmp.Canvas.Draw(0,0,pic.Graphic);
    pbPostProcess.Width:=FBmp.Width;
    pbPostProcess.Height:=FBmp.Height;
  finally
    FreeAndNil(pic);
  end;
  BuildDistanceField(GetKernel);
end;

function TfrmMain.PPClamp(const S: Single): Byte;
begin
  if IsInfinite(s) then Exit(255);
  Result := Min(Trunc(S), 255);
end;

function TfrmMain.PPMod(const S: Single): Byte;
begin
  if IsInfinite(s) then Exit(255);
  Result := (Trunc(S) mod 16)*(255 div 16);
end;

procedure TfrmMain.InitChamfers;
  function InitFromBMP(const raw: TRawImage; ks: TKernelSize): TChamfers;
  const BmpWidth : array [TKernelSize] of Integer = (3, 5, 7, 9, 11, 13);
        BmpHeight : array [TKernelSize] of Integer = (2, 3, 4, 5, 6, 7);
  var pb: PBYTE;
      i, j, n: Integer;
      cx, cy: Integer;
  begin
    pb := raw.Data;
    n := 0;
    for j := 0 to BmpHeight[ks] - 1 do
    begin
      for i := 0 to BmpWidth[ks] - 1 do
      begin
        if pb^=0 then Inc(n);
        Inc(pb, raw.Description.BitsPerPixel div 8);
      end;
      Inc(pb, (raw.Description.Width-BmpWidth[ks]) * raw.Description.BitsPerPixel div 8);
    end;

    cx := BmpWidth[ks] div 2;
    cy := BmpHeight[ks]-1;
    SetLength(Result, n);
    n := 0;
    pb := raw.Data;
    for j := 0 to BmpHeight[ks] - 1 do
    begin
      for i := 0 to BmpWidth[ks] - 1 do
      begin
        if pb^=0 then
        begin
           Result[n].offsetY:=j - cy;
           Result[n].offsetX:=i - cx;
           Result[n].distance:=sqrt(sqr(Result[n].offsetX)+
                                    sqr(Result[n].offsetY));
           Inc(n);
        end;
        Inc(pb, raw.Description.BitsPerPixel div 8);
      end;
      Inc(pb, (raw.Description.Width-BmpWidth[ks]) * raw.Description.BitsPerPixel div 8);
    end;
  end;
var i: Integer;
    ks: TKernelSize;
    raw: TRawImage;
begin
  for ks := Low(TKernelSize) to High(TKernelSize) do
  begin
    i := Ord(ks);
    imgKernels.GetRawImage(i, raw);
    FChamfers[ks] := InitFromBMP(raw, ks);
  end;
end;

end.

