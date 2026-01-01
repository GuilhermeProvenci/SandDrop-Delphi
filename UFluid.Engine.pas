unit UFluid.Engine;

interface

uses
  System.SysUtils, System.Math, UFluid.Constants;

type
  TFluidEngine = class
  private
    // 1D Buffers for maximum performance
    FDensity: TArray<Single>;
    FVelocityX: TArray<Single>;
    FVelocityY: TArray<Single>;
    
    // Scratchpad buffer to avoid re-allocations
    FScratch: TArray<Single>;
    
    FDiffusion: Single;
    FViscosity: Single;
    
    procedure Diffuse(var Matrix: TArray<Single>; Diff, dt: Single);
  public
    constructor Create;
    
    procedure Update(dt: Single);
    procedure AddDensity(X, Y: Integer; Amount: Single);
    procedure AddVelocity(X, Y: Integer; VX, VY: Single);
    procedure Reset;
    
    property Density: TArray<Single> read FDensity;
    property Diffusion: Single read FDiffusion write FDiffusion;
    property Viscosity: Single read FViscosity write FViscosity;
  end;

implementation

{$POINTERMATH ON}

constructor TFluidEngine.Create;
var
  Size: Integer;
begin
  Size := FluidMatrixSize * FluidMatrixSize;
  SetLength(FDensity, Size);
  SetLength(FVelocityX, Size);
  SetLength(FVelocityY, Size);
  SetLength(FScratch, Size);
  
  FDiffusion := 0.1;
  FViscosity := 0.1;
end;

procedure TFluidEngine.Reset;
var
  i: Integer;
begin
  for i := 0 to Length(FDensity) - 1 do
  begin
    FDensity[i] := 0;
    FVelocityX[i] := 0;
    FVelocityY[i] := 0;
  end;
end;

procedure TFluidEngine.AddDensity(X, Y: Integer; Amount: Single);
begin
  if (X >= 0) and (X < FluidMatrixSize) and (Y >= 0) and (Y < FluidMatrixSize) then
    FDensity[Y * FluidMatrixSize + X] := FDensity[Y * FluidMatrixSize + X] + Amount;
end;

procedure TFluidEngine.AddVelocity(X, Y: Integer; VX, VY: Single);
var
  Idx: Integer;
begin
  if (X >= 0) and (X < FluidMatrixSize) and (Y >= 0) and (Y < FluidMatrixSize) then
  begin
    Idx := Y * FluidMatrixSize + X;
    FVelocityX[Idx] := FVelocityX[Idx] + VX;
    FVelocityY[Idx] := FVelocityY[Idx] + VY;
  end;
end;

procedure TFluidEngine.Diffuse(var Matrix: TArray<Single>; Diff, dt: Single);
var
  i, j, k, n: Integer;
  a: Single;
  POrig, PNew: PSingle;
begin
  n := FluidMatrixSize;
  a := dt * Diff * n * n;
  
  // Use Scratch buffer instead of allocating NewMatrix every time
  Move(Matrix[0], FScratch[0], Length(Matrix) * SizeOf(Single));
  
  POrig := @Matrix[0];
  PNew := @FScratch[0];

  // Gauss-Seidel relaxation with pointer loops
  for k := 0 to 19 do
  begin
    for i := 1 to n - 2 do
    begin
      for j := 1 to n - 2 do
      begin
        // 1D Index calculation: i*n + j
        PNew[i * n + j] := (POrig[i * n + j] + a * (PNew[(i-1) * n + j] + PNew[(i+1) * n + j] + 
                           PNew[i * n + (j-1)] + PNew[i * n + (j+1)])) / (1 + 4 * a);
      end;
    end;
  end;
  
  Move(FScratch[0], Matrix[0], Length(Matrix) * SizeOf(Single));
end;

procedure TFluidEngine.Update(dt: Single);
begin
  // Optimized passes
  Diffuse(FDensity, FDiffusion, dt);
  Diffuse(FVelocityX, FViscosity, dt);
  Diffuse(FVelocityY, FViscosity, dt);
end;

end.
