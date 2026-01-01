unit USand.Engine;

interface

uses
  System.SysUtils, System.Threading, System.SyncObjs, System.Math, Vcl.Graphics,
  USand.Constants, USand.Physics;

type
  TSandEngine = class
  private
    FSandBuffers: array[0..1] of TMatrixBuffer;
    FColorBuffers: array[0..1] of TColorBuffer;
    FCurrentBufferIdx: Integer;
    FCanMoveInt: Integer;
    
    procedure SwapBuffers;
  public
    constructor Create;
    
    procedure Update;
    procedure Clear;
    procedure DropMaterial(X, Y, Radius, MatType: Integer; Color: TColor);
    
    function GetCurrentSand: TMatrixBuffer;
    function GetCurrentColors: TColorBuffer;
    function GetCanMove: Boolean;
  end;

implementation

constructor TSandEngine.Create;
begin
  SetLength(FSandBuffers[0], MatrixSize * MatrixSize);
  SetLength(FSandBuffers[1], MatrixSize * MatrixSize);
  SetLength(FColorBuffers[0], MatrixSize * MatrixSize);
  SetLength(FColorBuffers[1], MatrixSize * MatrixSize);
  FCurrentBufferIdx := 0;
end;

procedure TSandEngine.SwapBuffers;
begin
  FCurrentBufferIdx := 1 - FCurrentBufferIdx;
end;

function TSandEngine.GetCurrentSand: TMatrixBuffer;
begin
  Result := FSandBuffers[FCurrentBufferIdx];
end;

function TSandEngine.GetCurrentColors: TColorBuffer;
begin
  Result := FColorBuffers[FCurrentBufferIdx];
end;

function TSandEngine.GetCanMove: Boolean;
begin
  Result := FCanMoveInt <> 0;
end;

procedure TSandEngine.Clear;
begin
  FillChar(FSandBuffers[0][0], Length(FSandBuffers[0]) * SizeOf(Integer), 0);
  FillChar(FSandBuffers[1][0], Length(FSandBuffers[1]) * SizeOf(Integer), 0);
end;

procedure TSandEngine.DropMaterial(X, Y, Radius, MatType: Integer; Color: TColor);
var
  i, j, LX, LY, LIdx: Integer;
  LSand: TMatrixBuffer;
  LColors: TColorBuffer;
begin
  LSand := GetCurrentSand;
  LColors := GetCurrentColors;
  
  for i := -Radius to Radius do
    for j := -Radius to Radius do
    begin
      LX := X + i;
      LY := Y + j;
      if (Sqr(i) + Sqr(j) <= Sqr(Radius)) and (LX >= 0) and (LX < MatrixSize) and (LY >= 0) and (LY < MatrixSize) then
      begin
        LIdx := LY * MatrixSize + LX;
        if MatType = MAT_EMPTY then
        begin
          LSand[LIdx] := MAT_EMPTY;
        end
        else if LSand[LIdx] = MAT_EMPTY then
        begin
          LSand[LIdx] := MatType;
          LColors[LIdx] := Color;
        end;
      end;
    end;
  FCanMoveInt := 1;
end;

procedure TSandEngine.Update;
var
  LSand, LNextSand: TMatrixBuffer;
  LColors, LNextColors: TColorBuffer;
  NextIdx: Integer;
begin
  LSand := FSandBuffers[FCurrentBufferIdx];
  LColors := FColorBuffers[FCurrentBufferIdx];
  
  NextIdx := 1 - FCurrentBufferIdx;
  LNextSand := FSandBuffers[NextIdx];
  LNextColors := FColorBuffers[NextIdx];
  
  FillChar(LNextSand[0], Length(LNextSand) * SizeOf(Integer), 0);
  FCanMoveInt := 0;

  TParallel.For(0, MatrixSize - 1, TProc<Integer>(procedure(X: Integer)
  var
    LCanMove: Integer;
  begin
    LCanMove := 0;
    TSandPhysics.ProcessColumn(X, @LSand[0], @LNextSand[0], @LColors[0], @LNextColors[0], LCanMove);
    if LCanMove <> 0 then TInterlocked.Exchange(FCanMoveInt, 1);
  end));

  SwapBuffers;
end;

end.
