unit USand.Physics.Water;

interface

uses
  System.SyncObjs, System.Math, Vcl.Graphics, 
  USand.Constants, USand.Physics.Types, USand.Physics.Utils;

type
  TWaterBehavior = class
  public
    class function Process(
      X, Y, CurrentIdx: Integer;
      LSand, LNextSand: PIntArray;
      LColors, LNextColors: PColorArray
    ): Boolean; static;
  end;

implementation

class function TWaterBehavior.Process(
  X, Y, CurrentIdx: Integer;
  LSand, LNextSand: PIntArray;
  LColors, LNextColors: PColorArray
): Boolean;
var
  BottomIdx, targetMat, Offset, NewX, i, Dir: Integer;
begin
  Result := False;

  // 1. VERTICAL
  if Y < MatrixSize - 1 then
  begin
    BottomIdx := (Y + 1) * MatrixSize + X;
    if LSand[BottomIdx] = MAT_EMPTY then
    begin
      if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
      begin
        LNextColors[BottomIdx] := LColors[CurrentIdx];
        Exit(True);
      end;
    end;

    // 2. DIAGONAL
    Dir := IfThen(Random(2) = 0, -1, 1);
    for i := 0 to 1 do
    begin
      Offset := IfThen(i = 0, Dir, -Dir);
      NewX := X + Offset;
      if (NewX >= 0) and (NewX < MatrixSize) then
      begin
        BottomIdx := (Y + 1) * MatrixSize + NewX;
        if LSand[BottomIdx] = MAT_EMPTY then
        begin
          if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
          begin
            LNextColors[BottomIdx] := LColors[CurrentIdx];
            Exit(True);
          end;
        end;
      end;
    end;
  end;

  // 3. HORIZONTAL FLOW
  Dir := IfThen(Random(2) = 0, 1, -1);
  for i := 0 to 1 do
  begin
    Offset := IfThen(i = 0, Dir, -Dir);
    NewX := X + Offset;
    while (Abs(NewX - X) <= 15) and (NewX >= 0) and (NewX < MatrixSize) do
    begin
      if LSand[Y * MatrixSize + NewX] <> MAT_EMPTY then
      begin
        if (LSand[Y * MatrixSize + NewX] = MAT_STONE) or (LSand[Y * MatrixSize + NewX] = MAT_SAND) then Break;
      end
      else
      begin
        if TInterlocked.CompareExchange(LNextSand[Y * MatrixSize + NewX], MAT_WATER, MAT_EMPTY) = MAT_EMPTY then
        begin
          LNextColors[Y * MatrixSize + NewX] := LColors[CurrentIdx];
          Exit(True);
        end;
      end;
      NewX := NewX + Offset;
    end;
  end;
end;

end.
