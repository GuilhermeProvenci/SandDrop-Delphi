unit USand.Physics;

interface

uses
  System.SysUtils, System.SyncObjs, Vcl.Graphics,
  USand.Constants, 
  USand.Physics.Types, 
  USand.Physics.Utils,
  USand.Physics.Sand,
  USand.Physics.Water,
  USand.Physics.Stone;

type
  TSandPhysics = class
  public
    class procedure ProcessColumn(
      X: Integer;
      PSand, PNextSand: PMatrixBuffer;
      PColors, PNextColors: PColorBuffer;
      var CanMove: Integer
    );
  end;

implementation

class procedure TSandPhysics.ProcessColumn(
  X: Integer;
  PSand, PNextSand: PMatrixBuffer;
  PColors, PNextColors: PColorBuffer;
  var CanMove: Integer
);
var
  Y, CurrentIdx, mat: Integer;
  FoundSpot: Boolean;
  LSand, LNextSand: PIntArray;
  LColors, LNextColors: PColorArray;
begin
  LSand := PIntArray(PSand);
  LNextSand := PIntArray(PNextSand);
  LColors := PColorArray(PColors);
  LNextColors := PColorArray(PNextColors);

  // PASS 1: Statics
  for Y := 0 to MatrixSize - 1 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    if LSand[CurrentIdx] = MAT_STONE then
      TStoneBehavior.Process(CurrentIdx, LColors, LNextColors, LNextSand);
  end;

  // PASS 2: Dynamics
  for Y := MatrixSize - 1 downto 0 do
  begin
    CurrentIdx := Y * MatrixSize + X;
    mat := LSand[CurrentIdx];
    
    if (mat = MAT_EMPTY) or (mat = MAT_STONE) then Continue;

    FoundSpot := False;
    case mat of
      MAT_SAND:  FoundSpot := TSandBehavior.Process(X, Y, CurrentIdx, LSand, LNextSand, LColors, LNextColors);
      MAT_WATER: FoundSpot := TWaterBehavior.Process(X, Y, CurrentIdx, LSand, LNextSand, LColors, LNextColors);
    end;

    if FoundSpot then
    begin
      CanMove := 1;
    end
    else
    begin
      // Stay put with Rescue
      if TInterlocked.CompareExchange(LNextSand[CurrentIdx], mat, MAT_EMPTY) = MAT_EMPTY then
        LNextColors[CurrentIdx] := LColors[CurrentIdx]
      else
        TPhysicsUtils.RescueParticle(X, Y, mat, LColors[CurrentIdx], LNextSand, LNextColors);
    end;
  end;
end;

end.
