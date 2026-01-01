unit USand.Physics.Sand;

interface

uses
  System.SyncObjs, System.Math, Vcl.Graphics, 
  USand.Constants, USand.Physics.Types, USand.Physics.Utils;

type
  TSandBehavior = class
  public
    class function Process(
      X, Y, CurrentIdx: Integer;
      LSand, LNextSand: PIntArray;
      LColors, LNextColors: PColorArray
    ): Boolean; static;
  end;

implementation

class function TSandBehavior.Process(
  X, Y, CurrentIdx: Integer;
  LSand, LNextSand: PIntArray;
  LColors, LNextColors: PColorArray
): Boolean;
var
  BottomIdx, targetMat, Offset, NewX, i, Dir, OldVal: Integer;
begin
  Result := False;
  if Y >= MatrixSize - 1 then Exit;

  // 1. VERTICAL
  BottomIdx := (Y + 1) * MatrixSize + X;
  targetMat := LSand[BottomIdx];
  
  if (targetMat = MAT_EMPTY) or (targetMat = MAT_WATER) then
  begin
    OldVal := TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_EMPTY);
    if OldVal = MAT_EMPTY then
    begin
      LNextColors[BottomIdx] := LColors[CurrentIdx];
      Result := True;
    end
    else if OldVal = MAT_WATER then
    begin
      if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER then
      begin
        TPhysicsUtils.PlacePreservedWater(X, Y, CurrentIdx, clSkyBlue, LNextSand, LNextColors);
        LNextColors[BottomIdx] := LColors[CurrentIdx];
        Result := True;
      end;
    end;
  end;

  // 2. DIAGONAL
  if (not Result) and ((targetMat <> MAT_WATER) or (Random(20) = 0)) then
  begin
    Dir := IfThen(Random(2) = 0, -1, 1);
    for i := 0 to 1 do
    begin
      Offset := IfThen(i = 0, Dir, -Dir);
      NewX := X + Offset;
      if (NewX >= 0) and (NewX < MatrixSize) then
      begin
        BottomIdx := (Y + 1) * MatrixSize + NewX;
        targetMat := LSand[BottomIdx];
        if (targetMat = MAT_EMPTY) or (targetMat = MAT_WATER) then
        begin
          OldVal := TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_EMPTY);
          if OldVal = MAT_EMPTY then
          begin
            LNextColors[BottomIdx] := LColors[CurrentIdx];
            Result := True;
            Break;
          end
          else if OldVal = MAT_WATER then
          begin
            if TInterlocked.CompareExchange(LNextSand[BottomIdx], MAT_SAND, MAT_WATER) = MAT_WATER then
            begin
              TPhysicsUtils.PlacePreservedWater(X, Y, CurrentIdx, clSkyBlue, LNextSand, LNextColors);
              LNextColors[BottomIdx] := LColors[CurrentIdx];
              Result := True;
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
