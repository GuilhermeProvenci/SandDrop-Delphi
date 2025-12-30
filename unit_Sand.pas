unit unit_Sand;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Math, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    tbrRadius: TTrackBar;
    btnReset: TButton;
    btnRain: TButton;
    RainTimer: TTimer;
    Panel1: TPanel;
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnRainClick(Sender: TObject);
    procedure RainTimerTimer(Sender: TObject);
  private
    SandMatrix: TArray<TArray<Boolean>>;
    ColorMatrix: TArray<TArray<TColor>>;
    Bitmap: TBitmap;
    HueValue: Integer;
    ColorChangeCounter: Integer;
    procedure DropSand(X, Y: Integer);
    procedure DrawCell(X, Y: Integer; Color: TColor);
    function HSVToColor(H, S, V: Single): TColor;
    function InterpolateColor(Color1, Color2: TColor; Factor: Single): TColor;
    procedure StartRain;
  public
  end;

const
  MatrixSize = 200;
  CellSize = 5;
  ColorChangeInterval = 1;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 30;
  Timer1.Enabled := False;
  HueValue := 0;
  ColorChangeCounter := 0;

  // Inicializa matrizes de areia e cores
  SetLength(SandMatrix, MatrixSize, MatrixSize);
  SetLength(ColorMatrix, MatrixSize, MatrixSize);
  Bitmap := TBitmap.Create;
  Bitmap.Width := MatrixSize * CellSize;
  Bitmap.Height := MatrixSize * CellSize;
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height)); // fundo branco
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, Bitmap);
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    if (X >= 0) and (Y >= 0) and (X < MatrixSize * CellSize) and (Y < MatrixSize * CellSize) then
    begin
      DropSand(X div CellSize, Y div CellSize);
      Timer1.Enabled := True;
    end;
  end;
end;

procedure TForm1.DropSand(X, Y: Integer);
var
  i, j, Radius: Integer;
begin
  Radius := tbrRadius.Position;

  for i := -Radius to Radius do
    for j := -Radius to Radius do
      if (Sqr(i) + Sqr(j) <= Sqr(Radius)) and (X + i >= 0) and (X + i < MatrixSize) and
         (Y + j >= 0) and (Y + j < MatrixSize) then
      begin
        SandMatrix[X + i, Y + j] := True;
        ColorMatrix[X + i, Y + j] := HSVToColor(HueValue, 1.0, 1.0); // cor, mas ta esquisito quando tem muito pixel
        DrawCell(X + i, Y + j, ColorMatrix[X + i, Y + j]); // Desenha a célula, ver pra colocar thread pra ver se fica menos travado
      end;
end;


procedure TForm1.DrawCell(X, Y: Integer; Color: TColor);
begin
  with Bitmap.Canvas do
  begin
    Brush.Color := Color;
    Pen.Style := psClear; //tira a borda preta do pixel
    Rectangle(
      X * CellSize,              // X inicial
      Y * CellSize,              // Y inicial
      (X + 1) * CellSize,        // X final
      (Y + 1) * CellSize         // Y final
    );
  end;
end;

procedure TForm1.btnResetClick(Sender: TObject);
var
  i: Integer;
begin
  Timer1.Enabled := False;

  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height)); // Fundo branco

  for i := 0 to High(SandMatrix) do
  begin
    FillChar(SandMatrix[i, 0], Length(SandMatrix[i]) * SizeOf(Boolean), 0);
    FillChar(ColorMatrix[i, 0], Length(ColorMatrix[i]) * SizeOf(TColor), Byte(clWhite));
  end;

  PaintBox1.Invalidate;
end;

function TForm1.HSVToColor(H, S, V: Single): TColor;
var
  R, G, B: Single;
  i: Integer;
  f, p, q, t: Single;
begin
  if S = 0 then
  begin
    R := V;
    G := V;
    B := V;
  end
  else
  begin
    H := H / 60;
    i := Floor(H);
    f := H - i;
    p := V * (1 - S);
    q := V * (1 - S * f);
    t := V * (1 - S * (1 - f));
    case i of
      0: begin R := V; G := t; B := p; end;
      1: begin R := q; G := V; B := p; end;
      2: begin R := p; G := V; B := t; end;
      3: begin R := p; G := q; B := V; end;
      4: begin R := t; G := p; B := V; end;
      else begin R := V; G := p; B := q; end;
    end;
  end;
  Result := RGB(Trunc(R * 255), Trunc(G * 255), Trunc(B * 255));
end;

function TForm1.InterpolateColor(Color1, Color2: TColor; Factor: Single): TColor;
var
  R1, G1, B1, R2, G2, B2: Byte;
  R, G, B: Byte;
begin
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);

  R := Round(R1 + (R2 - R1) * Factor);
  G := Round(G1 + (G2 - G1) * Factor);
  B := Round(B1 + (B2 - B1) * Factor);

  Result := RGB(R, G, B);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i, j: Integer;
  CanMove: Boolean;
  TempMatrix: TArray<TArray<Boolean>>;
  TempColorMatrix: TArray<TArray<TColor>>;
begin
  CanMove := False;
  ColorChangeCounter := (ColorChangeCounter + 1) mod ColorChangeInterval;

  if ColorChangeCounter = 0 then
    HueValue := (HueValue + 1) mod 360;

  // Inicializa as matrizes temporárias com o mesmo tamanho
  SetLength(TempMatrix, MatrixSize, MatrixSize);
  SetLength(TempColorMatrix, MatrixSize, MatrixSize);

  // Copia o conteúdo das matrizes
  for i := 0 to High(SandMatrix) do
  begin
    for j := 0 to High(SandMatrix[i]) do
    begin
      TempMatrix[i][j] := SandMatrix[i][j];
      TempColorMatrix[i][j] := ColorMatrix[i][j];
    end;
  end;

  for i := 0 to High(SandMatrix) do
    for j := High(SandMatrix[i]) downto 0 do
    begin
      if SandMatrix[i, j] then
      begin
        if (j < High(SandMatrix[i])) and not SandMatrix[i, j + 1] then
        begin
          TempMatrix[i, j + 1] := True;
          TempColorMatrix[i, j + 1] := InterpolateColor(ColorMatrix[i, j], HSVToColor(HueValue, 1.0, 1.0), 0.5);
          TempMatrix[i, j] := False;
          CanMove := True;
          DrawCell(i, j, clWhite);
          DrawCell(i, j + 1, TempColorMatrix[i, j + 1]);
        end
        else
        begin
          if (i > 0) and (j < High(SandMatrix[i])) and not SandMatrix[i - 1, j + 1] then
          begin
            TempMatrix[i - 1, j + 1] := True;
            TempColorMatrix[i - 1, j + 1] := InterpolateColor(ColorMatrix[i, j], HSVToColor(HueValue, 1.0, 1.0), 0.5);
            TempMatrix[i, j] := False;
            CanMove := True;
            DrawCell(i, j, clWhite);
            DrawCell(i - 1, j + 1, TempColorMatrix[i - 1, j + 1]);
          end
          else if (i < High(SandMatrix)) and (j < High(SandMatrix[i])) and not SandMatrix[i + 1, j + 1] then
          begin
            TempMatrix[i + 1, j + 1] := True;
            TempColorMatrix[i + 1, j + 1] := InterpolateColor(ColorMatrix[i, j], HSVToColor(HueValue, 1.0, 1.0), 0.5);
            TempMatrix[i, j] := False;
            CanMove := True;
            DrawCell(i, j, clWhite);
            DrawCell(i + 1, j + 1, TempColorMatrix[i + 1, j + 1]);
          end;
        end;
      end;
    end;

  // Atualiza as matrizes principais com as temporárias
  SandMatrix := TempMatrix;
  ColorMatrix := TempColorMatrix;

  if CanMove then
    PaintBox1.Invalidate
  else
    Timer1.Enabled := False;
end;


procedure TForm1.StartRain;
var
  i, x: Integer;
begin
  Timer1.Enabled := true;

  for i := 0 to 48 do
  begin
    x := Random(MatrixSize); // Gera uma posição aleatória na linha
    DropSand(x, 0);
  end;
end;

procedure TForm1.RainTimerTimer(Sender: TObject);
begin
StartRain;
end;

procedure TForm1.btnRainClick(Sender: TObject);
begin
RainTimer.Enabled := not(RainTimer.Enabled);
end;

end.

