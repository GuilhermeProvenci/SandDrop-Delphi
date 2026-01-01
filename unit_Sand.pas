unit unit_Sand;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  System.Diagnostics, Winapi.MMSystem, System.Math,
  USand.Constants, USand.Engine, USand.Renderer;

type
  TForm1 = class(TForm)
    tbrRadius: TTrackBar;
    btnReset: TButton;
    btnRain: TButton;
    Timer1: TTimer;
    RainTimer: TTimer;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    lblFPS: TLabel;
    chkEraser: TCheckBox;
    pnlSidebar: TPanel;
    pnlHeader: TPanel;
    lblTitle: TLabel;
    pnlBrushGroup: TPanel;
    lblBrushTitle: TLabel;
    pnlActionGroup: TPanel;
    lblActionTitle: TLabel;
    rbSand: TRadioButton;
    rbWater: TRadioButton;
    rbStone: TRadioButton;
    btnSpawnObstacle: TButton;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnRainClick(Sender: TObject);
    procedure RainTimerTimer(Sender: TObject);
    procedure btnSpawnObstacleClick(Sender: TObject);
  private
    { Private declarations }
    FEngine: TSandEngine;
    FRenderer: TSandRenderer;
    
    FHueValue: Integer;
    
    // FPS e Performance
    FStopwatch: TStopwatch;
    FFrameCount: Integer;
    FLastFPSUpdate: Int64;
    
    procedure UpdateFPS;
    function GetSelectedMaterial: Integer;
    function GetSelectedColor: TColor;
    function HSVToColor(H, S, V: Single): TColor;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEngine := TSandEngine.Create;
  FRenderer := TSandRenderer.Create;
  
  FHueValue := 0;
  Randomize;

  FStopwatch := TStopwatch.StartNew;
  FFrameCount := 0;
  FLastFPSUpdate := 0;
  
  DoubleBuffered := True;
  Panel1.DoubleBuffered := True;
  
  timeBeginPeriod(1);
  Timer1.Interval := 1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  timeEndPeriod(1);
  FEngine.Free;
  FRenderer.Free;
end;

function TForm1.GetSelectedMaterial: Integer;
begin
  if chkEraser.Checked then Exit(MAT_EMPTY);
  if rbSand.Checked then Exit(MAT_SAND);
  if rbWater.Checked then Exit(MAT_WATER);
  if rbStone.Checked then Exit(MAT_STONE);
  Result := MAT_SAND;
end;

function TForm1.GetSelectedColor: TColor;
begin
  Result := HSVToColor(FHueValue, 0.8, 0.9);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  FRenderer.RenderTo(PaintBox1.Canvas, PaintBox1.ClientRect, FEngine.GetCurrentSand, FEngine.GetCurrentColors);
  
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
    lblFPS.Caption := Format('FPS: %.1f | Modular Engine', [FFrameCount / ((Elapsed - FLastFPSUpdate) / 1000)]);
    FFrameCount := 0;
    FLastFPSUpdate := Elapsed;
  end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  SimX, SimY: Integer;
begin
  if (ssLeft in Shift) and (PaintBox1.Width > 0) and (PaintBox1.Height > 0) then
  begin
    SimX := (X * MatrixSize) div PaintBox1.Width;
    SimY := (Y * MatrixSize) div PaintBox1.Height;
    
    if SimX < 0 then SimX := 0;
    if SimX >= MatrixSize then SimX := MatrixSize - 1;
    if SimY < 0 then SimY := 0;
    if SimY >= MatrixSize then SimY := MatrixSize - 1;
    
    FEngine.DropMaterial(SimX, SimY, tbrRadius.Position, GetSelectedMaterial, GetSelectedColor);
    Timer1.Enabled := True;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Step: Integer;
begin
  FHueValue := (FHueValue + 1) mod 360;
  for Step := 1 to StepsPerFrame do 
    FEngine.Update;
    
  PaintBox1.Invalidate;
  if not FEngine.GetCanMove then Timer1.Enabled := False;
end;

procedure TForm1.btnResetClick(Sender: TObject);
begin
  FEngine.Clear;
  PaintBox1.Invalidate;
end;

procedure TForm1.RainTimerTimer(Sender: TObject);
var
  i: Integer;
begin
  Timer1.Enabled := True;
  for i := 0 to 20 do 
    FEngine.DropMaterial(Random(MatrixSize), 0, tbrRadius.Position, MAT_SAND, GetSelectedColor);
end;

procedure TForm1.btnRainClick(Sender: TObject);
begin
  RainTimer.Enabled := not RainTimer.Enabled;
end;

procedure TForm1.btnSpawnObstacleClick(Sender: TObject);
var
  i, j, CenterX, CenterY: Integer;
begin
  CenterX := MatrixSize div 2;
  CenterY := MatrixSize div 2;
  
  for i := -50 to 50 do
    for j := -5 to 5 do
    begin
      FEngine.DropMaterial(CenterX + i, CenterY + j, 0, MAT_STONE, clDkGray);
      FEngine.DropMaterial(CenterX + j, CenterY + i, 0, MAT_STONE, clDkGray);
    end;
    
  PaintBox1.Invalidate;
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

end.
