unit SandSimulation;

interface

uses
  Vcl.Graphics, Math, System.SysUtils, System.Classes, Windows, Vcl.ExtCtrls;

type
  TSandSimulation = class(tpaintBox)
  private
    FSandMatrix: TArray<TArray<Boolean>>;
    FColorMatrix: TArray<TArray<TColor>>;
    FMatrixSize: Integer;
    FCellSize: Integer;
    FBitmap: TBitmap;
    FHueValue: Integer;
    FColorChangeCounter: Integer;
{    procedure DrawCell(X, Y: Integer; Color: TColor);
  public
    constructor Create(MatrixSize, CellSize: Integer);
    destructor Destroy; override;
    procedure DropSand(X, Y, Radius: Integer);
    procedure UpdateSimulation;
    procedure ResetSimulation;
    procedure DrawToCanvas(Canvas: TCanvas);
    function HSVToColor(H, S, V: Single): TColor;
    function InterpolateColor(Color1, Color2: TColor; Factor: Single): TColor;
    }
  end;

implementation

{constructor TSandSimulation.Create(MatrixSize, CellSize: Integer);
begin
  FMatrixSize := MatrixSize;
  FCellSize := CellSize;
  FHueValue := 0;
  FColorChangeCounter := 0;

  SetLength(FSandMatrix, FMatrixSize, FMatrixSize);
  SetLength(FColorMatrix, FMatrixSize, FMatrixSize);

  FBitmap := TBitmap.Create;
  FBitmap.Width := FMatrixSize * FCellSize;
  FBitmap.Height := FMatrixSize * FCellSize;
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height)); // fundo branco
end;

destructor TSandSimulation.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TSandSimulation.DrawCell(X, Y: Integer; Color: TColor);
begin
  with FBitmap.Canvas do
  begin
    Brush.Color := Color;
    Pen.Style := psClear;
    Rectangle(X * FCellSize, Y * FCellSize, (X + 1) * FCellSize, (Y + 1) * FCellSize);
  end;
end;

procedure TSandSimulation.DropSand(X, Y, Radius: Integer);
var
  i, j: Integer;
begin
  for i := -Radius to Radius do
    for j := -Radius to Radius do
      if (Sqr(i) + Sqr(j) <= Sqr(Radius)) and (X + i >= 0) and (X + i < FMatrixSize) and
         (Y + j >= 0) and (Y + j < FMatrixSize) then
      begin
        FSandMatrix[X + i, Y + j] := True;
        FColorMatrix[X + i, Y + j] := HSVToColor(FHueValue, 1.0, 1.0);
        DrawCell(X + i, Y + j, FColorMatrix[X + i, Y + j]);
      end;
end;

procedure TSandSimulation.UpdateSimulation;
var
  i, j: Integer;
  TempMatrix: TArray<TArray<Boolean>>;
  TempColorMatrix: TArray<TArray<TColor>>;
  CanMove: Boolean;
begin
  CanMove := False;
  FColorChangeCounter := (FColorChangeCounter + 1) mod 1;

  if FColorChangeCounter = 0 then
    FHueValue := (FHueValue + 1) mod 360;

  SetLength(TempMatrix, FMatrixSize, FMatrixSize);
  SetLength(TempColorMatrix, FMatrixSize, FMatrixSize);

  for i := 0 to High(FSandMatrix) do
  begin
    for j := 0 to High(FSandMatrix[i]) do
    begin
      TempMatrix[i][j] := FSandMatrix[i][j];
      TempColorMatrix[i][j] := FColorMatrix[i][j];
    end;
  end;

  for i := 0 to High(FSandMatrix) do
    for j := High(FSandMatrix[i]) downto 0 do
    begin
      if FSandMatrix[i, j] then
      begin
        if (j < High(FSandMatrix[i])) and not FSandMatrix[i, j + 1] then
        begin
          TempMatrix[i, j + 1] := True;
          TempColorMatrix[i, j + 1] := InterpolateColor(FColorMatrix[i, j], HSVToColor(FHueValue, 1.0, 1.0), 0.5);
          TempMatrix[i, j] := False;
          CanMove := True;
          DrawCell(i, j, clWhite);
          DrawCell(i, j + 1, TempColorMatrix[i, j + 1]);
        end
        else
        begin
          if (i > 0) and (j < High(FSandMatrix[i])) and not FSandMatrix[i - 1, j + 1] then
          begin
            TempMatrix[i - 1, j + 1] := True;
            TempColorMatrix[i - 1, j + 1] := InterpolateColor(FColorMatrix[i, j], HSVToColor(FHueValue, 1.0, 1.0), 0.5);
            TempMatrix[i, j] := False;
            CanMove := True;
            DrawCell(i, j, clWhite);
            DrawCell(i - 1, j + 1, TempColorMatrix[i - 1, j + 1]);
          end
          else if (i < High(FSandMatrix)) and (j < High(FSandMatrix[i])) and not FSandMatrix[i + 1, j + 1] then
          begin
            TempMatrix[i + 1, j + 1] := True;
            TempColorMatrix[i + 1, j + 1] := InterpolateColor(FColorMatrix[i, j], HSVToColor(FHueValue, 1.0, 1.0), 0.5);
            TempMatrix[i, j] := False;
            CanMove := True;
            DrawCell(i, j, clWhite);
            DrawCell(i + 1, j + 1, TempColorMatrix[i + 1, j + 1]);
          end;
        end;
      end;
    end;

  FSandMatrix := TempMatrix;
  FColorMatrix := TempColorMatrix;
end;

procedure TSandSimulation.ResetSimulation;
var
  i: Integer;
begin
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));

  for i := 0 to High(FSandMatrix) do
  begin
    FillChar(FSandMatrix[i, 0], Length(FSandMatrix[i]) * SizeOf(Boolean), 0);
    FillChar(FColorMatrix[i, 0], Length(FColorMatrix[i]) * SizeOf(TColor), Byte(clWhite));
  end;
end;

procedure TSandSimulation.DrawToCanvas(Canvas: TCanvas);
begin
  Canvas.Draw(0, 0, FBitmap);
end;

function TSandSimulation.HSVToColor(H, S, V: Single): TColor;
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

function TSandSimulation.InterpolateColor(Color1, Color2: TColor; Factor: Single): TColor;
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
}
end.

