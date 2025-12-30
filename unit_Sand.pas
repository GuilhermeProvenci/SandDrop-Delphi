unit unit_Sand;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Direct2D, Winapi.D2D1, System.Threading, System.Math, System.Diagnostics;

type
  // Tipo para acesso ultra-rápido aos pixels do bitmap
  TRGBQuad = record
    B, G, R, A: Byte;
  end;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..32767] of TRGBQuad;

  TForm1 = class(TForm)
    tbrRadius: TTrackBar;
    btnReset: TButton;
    btnRain: TButton;
    Timer1: TTimer;
    RainTimer: TTimer;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    lblFPS: TLabel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnRainClick(Sender: TObject);
    procedure RainTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FSandBuffer1, FSandBuffer2: TArray<Byte>;
    FColorBuffer1, FColorBuffer2: TArray<TColor>;
    FCurrentBuffer: Integer; 
    
    FHueValue: Integer;
    FCanMove: Boolean;
    
    // FPS e Performance
    FStopwatch: TStopwatch;
    FFrameCount: Integer;
    FLastFPSUpdate: Int64;
    
    // Bitmap de Renderização ultra-rápida
    FRenderBitmap: TBitmap;
    
    function GetIndex(X, Y: Integer): Integer; inline;
    procedure DropSand(X, Y: Integer);
    function D2DColor(Color: TColor; Alpha: Single = 1.0): D2D1_COLOR_F;
    function HSVToColor(H, S, V: Single): TColor;
    procedure StartRain;
    
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    
    procedure UpdateSimulation;
    function GetCurrentSand: TArray<Byte>;
    function GetCurrentColors: TArray<TColor>;
    function GetNextSand: TArray<Byte>;
    function GetNextColors: TArray<TColor>;
    procedure SwapBuffers;
    procedure UpdateFPS;
  public
    { Public declarations }
  end;

const
  MatrixSize = 400; 
  CellSize = 2;     
  StepsPerFrame = 4;
  
var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.GetIndex(X, Y: Integer): Integer;
begin
  Result := Y * MatrixSize + X;
end;

procedure TForm1.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

function TForm1.GetCurrentSand: TArray<Byte>;
begin
  if FCurrentBuffer = 1 then Result := FSandBuffer1 else Result := FSandBuffer2;
end;

function TForm1.GetCurrentColors: TArray<TColor>;
begin
  if FCurrentBuffer = 1 then Result := FColorBuffer1 else Result := FColorBuffer2;
end;

function TForm1.GetNextSand: TArray<Byte>;
begin
  if FCurrentBuffer = 1 then Result := FSandBuffer2 else Result := FSandBuffer1;
end;

function TForm1.GetNextColors: TArray<TColor>;
begin
  if FCurrentBuffer = 1 then Result := FColorBuffer2 else Result := FColorBuffer1;
end;

procedure TForm1.SwapBuffers;
begin
  if FCurrentBuffer = 1 then FCurrentBuffer := 2 else FCurrentBuffer := 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLength(FSandBuffer1, MatrixSize * MatrixSize);
  SetLength(FSandBuffer2, MatrixSize * MatrixSize);
  SetLength(FColorBuffer1, MatrixSize * MatrixSize);
  SetLength(FColorBuffer2, MatrixSize * MatrixSize);
  
  FCurrentBuffer := 1;
  Randomize;
  FHueValue := 0;
  
  FRenderBitmap := TBitmap.Create;
  FRenderBitmap.SetSize(MatrixSize, MatrixSize);
  FRenderBitmap.PixelFormat := pf32bit; 

  FStopwatch := TStopwatch.StartNew;
  FFrameCount := 0;
  FLastFPSUpdate := 0;
  
  DoubleBuffered := True;
  Panel1.DoubleBuffered := True;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  LCanvas: TDirect2DCanvas;
  X, Y: Integer;
  LSand: TArray<Byte>;
  LColors: TArray<TColor>;
  LinePtr: PRGBQuadArray;
  LColor: TColor;
begin
  LSand := GetCurrentSand;
  LColors := GetCurrentColors;

  for Y := 0 to MatrixSize - 1 do
  begin
    LinePtr := FRenderBitmap.ScanLine[Y];
    for X := 0 to MatrixSize - 1 do
    begin
      if LSand[Y * MatrixSize + X] = 1 then
      begin
        LColor := LColors[Y * MatrixSize + X];
        LinePtr[X].R := GetRValue(LColor);
        LinePtr[X].G := GetGValue(LColor);
        LinePtr[X].B := GetBValue(LColor);
        LinePtr[X].A := 255;
      end
      else
      begin
        LinePtr[X].R := 255;
        LinePtr[X].G := 255;
        LinePtr[X].B := 255;
        LinePtr[X].A := 255;
      end;
    end;
  end;

  LCanvas := TDirect2DCanvas.Create(PaintBox1.Canvas, PaintBox1.ClientRect);
  LCanvas.BeginDraw;
  try
    LCanvas.RenderTarget.Clear(D2DColor(clWhite));
    // Desenha o bitmap inteiro de uma vez via GPU usando StretchDraw para escala
    LCanvas.StretchDraw(Rect(0, 0, FRenderBitmap.Width * CellSize, FRenderBitmap.Height * CellSize), FRenderBitmap);
  finally
    LCanvas.EndDraw;
    LCanvas.Free;
  end;
  
  Inc(FFrameCount);
  UpdateFPS;
end;

procedure TForm1.UpdateFPS;
var
  Elapsed: Int64;
begin
  Elapsed := FStopwatch.ElapsedMilliseconds;
  if Elapsed - FLastFPSUpdate >= 500 then
  begin
    lblFPS.Caption := Format('FPS: %.1f | Mode: Multi-Core + D2D', [FFrameCount / ((Elapsed - FLastFPSUpdate) / 1000)]);
    FFrameCount := 0;
    FLastFPSUpdate := Elapsed;
  end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    DropSand(X div CellSize, Y div CellSize);
    Timer1.Enabled := True;
  end;
end;

procedure TForm1.DropSand(X, Y: Integer);
var
  i, j, LRadius, LX, LY, LIdx: Integer;
  LSand: TArray<Byte>;
  LColors: TArray<TColor>;
begin
  LRadius := tbrRadius.Position;
  LSand := GetCurrentSand;
  LColors := GetCurrentColors;
  
  for i := -LRadius to LRadius do
    for j := -LRadius to LRadius do
    begin
      LX := X + i;
      LY := Y + j;
      if (Sqr(i) + Sqr(j) <= Sqr(LRadius)) and (LX >= 0) and (LX < MatrixSize) and (LY >= 0) and (LY < MatrixSize) then
      begin
        LIdx := GetIndex(LX, LY);
        if LSand[LIdx] = 0 then
        begin
          LSand[LIdx] := 1;
          LColors[LIdx] := HSVToColor(FHueValue, 0.8, 0.9);
        end;
      end;
    end;
end;

procedure TForm1.UpdateSimulation;
var
  LSand, LNextSand: TArray<Byte>;
  LColors, LNextColors: TArray<TColor>;
begin
  LSand := GetCurrentSand;
  LColors := GetCurrentColors;
  LNextSand := GetNextSand;
  LNextColors := GetNextColors;
  
  FillChar(LNextSand[0], Length(LNextSand), 0);
  FCanMove := False;

  // Processo em paralelo com cast explícito para evitar erro de sobrecarga
  TParallel.For(0, MatrixSize - 1, TProc<Integer>(procedure(X: Integer)
  var
    Y, CurrentIdx, BottomIdx, LocalMatrixSize: Integer;
  begin
    LocalMatrixSize := MatrixSize;
    for Y := LocalMatrixSize - 1 downto 0 do
    begin
      CurrentIdx := Y * LocalMatrixSize + X;
      if LSand[CurrentIdx] = 1 then
      begin
        if Y < MatrixSize - 1 then
        begin
          BottomIdx := (Y + 1) * MatrixSize + X;
          
          if (LSand[BottomIdx] = 0) and (LNextSand[BottomIdx] = 0) then
          begin
            LNextSand[BottomIdx] := 1;
            LNextColors[BottomIdx] := LColors[CurrentIdx];
            FCanMove := True;
          end
          else if (X > 0) and (LSand[BottomIdx - 1] = 0) and (LNextSand[BottomIdx - 1] = 0) then
          begin
            LNextSand[BottomIdx - 1] := 1;
            LNextColors[BottomIdx - 1] := LColors[CurrentIdx];
            FCanMove := True;
          end
          else if (X < MatrixSize - 1) and (LSand[BottomIdx + 1] = 0) and (LNextSand[BottomIdx + 1] = 0) then
          begin
            LNextSand[BottomIdx + 1] := 1;
            LNextColors[BottomIdx + 1] := LColors[CurrentIdx];
            FCanMove := True;
          end
          else
          begin
            LNextSand[CurrentIdx] := 1;
            LNextColors[CurrentIdx] := LColors[CurrentIdx];
          end;
        end
        else
        begin
          LNextSand[CurrentIdx] := 1;
          LNextColors[CurrentIdx] := LColors[CurrentIdx];
        end;
      end;
    end;
  end);

  SwapBuffers;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Step: Integer;
begin
  FHueValue := (FHueValue + 1) mod 360;
  for Step := 1 to StepsPerFrame do UpdateSimulation;
  PaintBox1.Invalidate;
  if not FCanMove then Timer1.Enabled := False;
end;

procedure TForm1.btnResetClick(Sender: TObject);
var
  LSand: TArray<Byte>;
begin
  LSand := GetCurrentSand;
  FillChar(LSand[0], Length(LSand), 0);
  PaintBox1.Invalidate;
end;

procedure TForm1.StartRain;
var
  i: Integer;
begin
  Timer1.Enabled := True;
  for i := 0 to 20 do DropSand(Random(MatrixSize), 0);
end;

procedure TForm1.RainTimerTimer(Sender: TObject);
begin
  StartRain;
end;

procedure TForm1.btnRainClick(Sender: TObject);
begin
  RainTimer.Enabled := not RainTimer.Enabled;
end;

function TForm1.HSVToColor(H, S, V: Single): TColor;
var
  R, G, B: Single;
  i: Integer;
  f, p, q, t: Single;
begin
  H := H / 60;
  i := Floor(H);
  f := H - i;
  p := V * (1 - S);
  q := V * (1 - S * f);
  t := V * (1 - S * (1 - f));
  case i mod 6 of
    0: begin R := V; G := t; B := p; end;
    1: begin R := q; G := V; B := p; end;
    2: begin R := p; G := V; B := t; end;
    3: begin R := p; G := q; B := V; end;
    4: begin R := t; G := p; B := V; end;
    else begin R := V; G := p; B := q; end;
  end;
  Result := RGB(Trunc(R * 255), Trunc(G * 255), Trunc(B * 255));
end;

function TForm1.D2DColor(Color: TColor; Alpha: Single): D2D1_COLOR_F;
begin
  Result.r := GetRValue(Color) / 255;
  Result.g := GetGValue(Color) / 255;
  Result.b := GetBValue(Color) / 255;
  Result.a := Alpha;
end;

end.
