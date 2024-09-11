unit FluidForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Math;

type
  TForm2 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FluidMatrix: TArray<TArray<Single>>;
    VelocityX: TArray<TArray<Single>>;
    VelocityY: TArray<TArray<Single>>;
    Bitmap: TBitmap;
    procedure InitializeMatrices;
    procedure UpdateFluid;
    procedure DrawFluid;
    function Clamp(Value, Min, Max: Single): Single; // Função Clamp
  public
  end;

const
  MatrixSize = 100;
  CellSize = 5;
  Diffusion: Single = 0.1; // Difusão do fluido
  Viscosity: Single = 0.1; // Viscosidade do fluido

var
  Form2: TForm2;

implementation

{$R *.dfm}

function TForm2.Clamp(Value, Min, Max: Single): Single;
begin
  if Value < Min then
    Result := Min
  else if Value > Max then
    Result := Max
  else
    Result := Value;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 30;
  Timer1.Enabled := True;
  Bitmap := TBitmap.Create;
  Bitmap.Width := MatrixSize * CellSize;
  Bitmap.Height := MatrixSize * CellSize;
  PaintBox1.Canvas.Draw(0, 0, Bitmap);
  InitializeMatrices;
end;

procedure TForm2.InitializeMatrices;
var
  i, j: Integer;
begin
  SetLength(FluidMatrix, MatrixSize, MatrixSize);
  SetLength(VelocityX, MatrixSize, MatrixSize);
  SetLength(VelocityY, MatrixSize, MatrixSize);

  // Inicializa todas as células com zero
  for i := 0 to High(FluidMatrix) do
    for j := 0 to High(FluidMatrix[i]) do
    begin
      FluidMatrix[i][j] := 0;
      VelocityX[i][j] := 0;
      VelocityY[i][j] := 0;
    end;

  // Adiciona fluido inicial em algumas células
  FluidMatrix[MatrixSize div 2][MatrixSize div 2] := 1.0; // Adiciona fluido no centro
  FluidMatrix[MatrixSize div 2 + 10][MatrixSize div 2] := 0.5; // Adiciona fluido próximo ao centro
  FluidMatrix[MatrixSize div 2][MatrixSize div 2 + 10] := 0.5; // Adiciona fluido próximo ao centro
  FluidMatrix[MatrixSize div 2 - 10][MatrixSize div 2] := 0.5; // Adiciona fluido próximo ao centro
  FluidMatrix[MatrixSize div 2][MatrixSize div 2 - 10] := 0.5; // Adiciona fluido próximo ao centro
end;

procedure TForm2.UpdateFluid;
var
  i, j: Integer;
  TempFluid, TempVelX, TempVelY: TArray<TArray<Single>>;
begin
  SetLength(TempFluid, MatrixSize, MatrixSize);
  SetLength(TempVelX, MatrixSize, MatrixSize);
  SetLength(TempVelY, MatrixSize, MatrixSize);

  // Cópia da matriz atual
  for i := 0 to High(FluidMatrix) do
    for j := 0 to High(FluidMatrix[i]) do
    begin
      TempFluid[i][j] := FluidMatrix[i][j];
      TempVelX[i][j] := VelocityX[i][j];
      TempVelY[i][j] := VelocityY[i][j];
    end;

  // Atualização das matrizes
  for i := 1 to High(FluidMatrix) - 1 do
    for j := 1 to High(FluidMatrix[i]) - 1 do
    begin
      // Simulação de advecção e difusão
      TempFluid[i][j] := FluidMatrix[i][j] +
        (VelocityX[i][j] + VelocityY[i][j]) * 0.1 +
        Diffusion * ((FluidMatrix[i - 1][j] + FluidMatrix[i + 1][j] +
                      FluidMatrix[i][j - 1] + FluidMatrix[i][j + 1] - 4 * FluidMatrix[i][j]) / 4);

      TempVelX[i][j] := VelocityX[i][j] * (1 - Viscosity);
      TempVelY[i][j] := VelocityY[i][j] * (1 - Viscosity);
    end;

  // Atualiza a matriz principal
  FluidMatrix := TempFluid;
  VelocityX := TempVelX;
  VelocityY := TempVelY;
end;

procedure TForm2.DrawFluid;
var
  i, j: Integer;
  Color: TColor;
begin
  Bitmap.Canvas.Lock;
  try
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    for i := 0 to High(FluidMatrix) do
      for j := 0 to High(FluidMatrix[i]) do
      begin
        // Garante que a cor seja visível
        Color := RGB(Round(Clamp(FluidMatrix[i][j] * 255, 0, 255)), 0, 0); // Cor baseada na densidade
        Bitmap.Canvas.Brush.Color := Color;
        Bitmap.Canvas.Pen.Style := psClear;
        Bitmap.Canvas.Rectangle(
          i * CellSize,
          j * CellSize,
          (i + 1) * CellSize,
          (j + 1) * CellSize
        );
      end;
  finally
    Bitmap.Canvas.Unlock;
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  UpdateFluid;
  DrawFluid;
  PaintBox1.Canvas.Draw(0, 0, Bitmap);
end;

end.

